000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4039800.                                                
000400 AUTHOR.         CAP GEMINI AB/EP.                                        
000500 DATE-WRITTEN.   AUG 86.                                                  
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET BEHANDLAR ENDAST ETT VISST ANTAL                      
001100*        ORAPPORTERADE RADER OCH UPPDATERA                                
001200*        ORDER-, ARTIKEL-, LÅSNINGS-, AVVIKELSE- OCH                      
001300*        AUTOMATFAKTURERINGSREGISTREN.                                    
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T398X                                             
001800*                                                                         
001900*    UTDATA.                                                              
002000*        TRANS:       W4O31801                                            
002100*                                                                         
002200*    CHANGE LOG                                                           
002300*                                                                         
002400*    DIGAMBAR/021011                                                      
002500*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
002600*    THE RESPONSE TIME OF THE SCREEN 4312.                                
002700*                                                                         
002800* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
002900*                                                                         
003000* LINDA NILSSON DEC-2004                                                  
003100* ETRACKER: 1570221                                                       
003200* ETRACKER: 7450328  2008-HÖST  VOHF                                      
003300* ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                        
003400* ETRACKER: 10130993 2015-04-22                                           
003500*           REDUCE NUMBER OF DELIVERY SCHEDULES                           
003600* ETRACKER: 10254592 2015-HÖST  DECOMISSION VOHF                          
003700* ETRACKER: 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2          
003800*                                                                         
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP3                                                                
004100 DATA DIVISION.                                                           
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400*    -- CHECKED BY WY2000                                                 
004500     SKIP3                                                                
004600 77    IDPGM                     PIC X(8)    VALUE 'W4039800'.            
004700 77    FILLER                    PIC X(08)   VALUE 'ERRORTEX'.            
004800 77    ERRORTEX                  PIC X(64)   VALUE SPACE.                 
004900 77    FELTEXT                   PIC X(80)   VALUE SPACE.                 
005000 77    CURRENT-SECTION           PIC X(30)   VALUE SPACE.                 
005100 77    WS-MID-FLSVAR             PIC X       VALUE SPACE.                 
005200 77    JA                        PIC X       VALUE 'J'.                   
005300 77    YES                       PIC X       VALUE 'Y'.                   
005400 77    NEJ                       PIC X       VALUE 'N'.                   
005500 77    DEF-IDROLL                PIC X(5)    VALUE 'VOR99'.               
005600 77    WS-IDDC                   PIC X(2)    VALUE SPACE.                 
005700                                                                          
005800*01    -COPY WWDCKONS                                                     
005900                                                                          
006000 77    RAETT                     PIC X       VALUE 'R'.                   
006100 77    FEL                       PIC X       VALUE 'F'.                   
006200 77    KOLLI-99XXX-FINNS         PIC X       VALUE 'N'.                   
006300 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
006310 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006320 77    IX-DCCLEAR-MAX            PIC S9(3)   COMP SYNC VALUE +99.         
006400 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
006500 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
006600 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77    0605-LAENGD               PIC S9(4)   VALUE +58  COMP SYNC.        
006800 77    4342-LAENGD               PIC S9(4)   VALUE +31  COMP SYNC.        
006900 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
007000 77    SW-TIRODAT-LIKA-MED-ZERO  PIC  X(1)   VALUE 'N'.                   
007100 77    WS-RADER-PER-START        PIC S9(3)   VALUE +26   COMP-3.          
007200 77    WS-RADER-PER-START-PLUS-1 PIC S9(3)   VALUE +27   COMP-3.          
007300 77    WS-ANT-RADER-INT          PIC S9(3)   VALUE +0    COMP-3.          
007400 77    WS-ANT-RADER-INT-PLUS-1   PIC S9(3)   VALUE +1    COMP-3.          
007500 77    WS-ANT-RADER-FYSAVVIK     PIC S9(3)   VALUE +0    COMP-3.          
007600 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
007700 77    WS-KVORDRAD-PACK          PIC S9(5)   VALUE +0    COMP-3.          
007800 77    WS-KVORAPP-TOTAL          PIC S9(6)   VALUE +0.                    
007900 77    WS-KVORAPP-PACK           PIC S9(6)   VALUE +0.                    
008000 77    FILLER                    PIC X(8)    VALUE  'DDDDDDDD'.           
008100 77    WS-KVPRERO                PIC S9(7)  COMP-3 VALUE +0.              
008200 77    WS-AVVIKELSE-UTSKR        PIC S9(7)   VALUE +0.                    
008300 77    WS-SUMMA                  PIC S9(7)   VALUE +0.                    
008400 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
008500 77    WS-IDANSTNR               PIC 9(5)   VALUE ZERO.                   
008600 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
008700 77    WS-IDDISTR-X4             PIC 9(4)   VALUE ZERO.                   
008800 77    WS-IDKUNDNR               PIC 9(6)   VALUE ZERO.                   
008900 77    WS-IDPRODNR               PIC 9(7)   VALUE ZERO.                   
009000 77    WS-IDPRODNR-RED           PIC Z(6)9  VALUE ZERO.                   
009100 77    WS-MODNAMN                PIC X(8)   VALUE SPACE.                  
009200 77    WS-LISTA                  PIC X(3)   VALUE SPACE.                  
009300 77    WS-KDFRAKT                PIC  9(2)  VALUE ZERO.                   
009400 77    WS-KDKOLSTA               PIC 9(1)   VALUE ZERO.                   
009500 77    FILLER                    PIC X(8)    VALUE  'EEEEEEEE'.           
009600 77    WS-KDORDKL                PIC S9(1)  VALUE ZERO.                   
009700 77    WS-FLLSBOK                PIC X(1)   VALUE SPACE.                  
009800 77    WS-FLORDSPE               PIC X(1)   VALUE SPACE.                  
009900 77    WS-FLOVRLEV               PIC X(1)   VALUE SPACE.                  
010000 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
010100 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
010200 77    WS-KVSLATTAT              PIC S9(7)  COMP-3 VALUE ZERO.            
010300 77    WS-SAMMANSLAGNING-RAD     PIC X.                                   
010400 77    W-SPAR-IDDC               PIC X(2).                                
010500 77    W-SPAR-IDPRODNR           PIC S9(7)  COMP-3.                       
010600 77    W-SPAR-KDORDSTA           PIC S9     COMP-3 VALUE ZERO.            
010700 77    SPAR-KART-KVRESS-ART      PIC S9(7)  COMP-3.                       
010800 77    WS-KDRAPRIO               PIC S9(03) COMP-3.                       
010900 77    WS-KDTPOTYP               PIC S9     COMP-3  VALUE ZERO.           
011000 77    WS-TIDISPIN               PIC S9(7)  COMP-3.                       
011100 77    FILLER                    PIC X(8)    VALUE  'FFFFFFFF'.           
011200 77    WS-IDORDER                PIC S9(7)  COMP-3.                       
011300 77    WS-KDORDBEK               PIC S9(3)  COMP-3.                       
011400 77    WS-KVPTID-MIN             PIC S9(7)  COMP-3.                       
011500 77    WS-KVPTID-TIM             PIC S9(3)  COMP-3.                       
011600 77    W-KVKOLLI                 PIC S9(7)  VALUE ZERO  COMP-3.           
011700 77    W-KVKOLLI-FAKT            PIC S9(7)  VALUE ZERO  COMP-3.           
011800 77    W-KVKOLLI-LAST            PIC S9(7)  VALUE ZERO  COMP-3.           
011900 77    W-KVKOLLI-FL              PIC S9(7)  VALUE ZERO  COMP-3.           
012000 77    PTYP-AVVIK                PIC X(3)   VALUE '002'.                  
012100 77    WS-OHUV-BEKUNDRF          PIC X(15)  VALUE SPACE.                  
012200 77    WS-OHUV-BEVARREF          PIC X(10)  VALUE SPACE.                  
012300 77    FILLER                    PIC X(8)    VALUE  'GGGGGGGG'.           
012400 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
012500 77    KDRC-DISP                 PIC 9(4)    VALUE ZERO.                  
012600 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
012700 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
012800 77    WS-IDPRQUES               PIC S9(7)   VALUE ZERO.                  
012900 77    WS-VOR-TID-BRIST          PIC 9(9)    VALUE ZERO.                  
013000 77    S28-IDARTNR               PIC 9(9)    VALUE ZERO COMP-3.           
013100 77    S28-IDDC                  PIC X(2)    VALUE SPACE.                 
013200 77    S28-KVVORKO               PIC S9(7)   VALUE ZERO COMP-3.           
013300 77    S28-IDANSK                PIC 9(3)    VALUE ZERO COMP-3.           
013400 77    S28-IDLEVNR               PIC X(5)    VALUE SPACE.                 
013500 77    WS-PARTNER                PIC X(1)    VALUE SPACE.                 
013510 77    W-GMT-KDKUNDKAT           PIC 9(2)    VALUE ZERO.                  
013520 77    WS-IDSYSTEM               PIC X(4)    VALUE SPACE.                 
013600                                                                          
013610 01  W-GMT-IDDC-CLEAR-GRP.                                                
013620*                                 GRUPP AV IDDC-CLEAR                     
013630     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
013640                                 PIC X(2)    VALUE SPACE.                 
013700 77    WS-IDANSK                 PIC 9(3)    VALUE ZERO.                  
013800                                                                          
013900*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
014000 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
014100 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
014200                                                                          
014300*    --- ARBETSFÄLT FÖR BERÄKNING AV SALDOFÖRÄNDRADE ART.                 
014400 77  WS-KVEFRS-DIFF              PIC S9(7)  COMP-3.                       
014500 77  WS-KVLS-DIFF                PIC S9(7)  COMP-3.                       
014600*                                                                         
014700 77  S27-IDDC                    PIC X(2)    VALUE SPACE.                 
014800 77  S27-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
014900*                                                                         
015000 01  S27-IDANSK-X.                                                        
015100     03 S27-IDANSK               PIC 9(3).                                
015200                                                                          
015300 01  S27-IDARTNR-X.                                                       
015400     03 S27-IDARTNR              PIC 9(9).                                
015500                                                                          
015600 01  S27-IDDISTR-X.                                                       
015700     03 S27-IDDISTR              PIC 9(4).                                
015800                                                                          
015900 01  S27-IDKUNDNR-X.                                                      
016000     03 S27-IDKUNDNR             PIC 9(6).                                
016100                                                                          
016200 01    FILLER                    PIC X(8) VALUE 'HHHHHHHH'.               
016300                                                                          
016400 01  ARBETSFALT.                                                          
016500                                                                          
016600     03 WS-DAORDREG              PIC 9(8) VALUE ZERO.                     
016700     03 WS-DAORDREG-DELAR        REDEFINES WS-DAORDREG.                   
016800        05 WS-DAORDREG-TISS      PIC 9(2).                                
016900        05 WS-DAORDREG-TIAAMMDD  PIC 9(6).                                
017000                                                                          
017100     03 WS-DARODAT               PIC 9(8) VALUE ZERO.                     
017200     03 WS-DARODAT-DELAR         REDEFINES WS-DARODAT.                    
017300        05 WS-DARODAT-TISS       PIC 9(2).                                
017400        05 WS-DARODAT-TIAAMMDD   PIC 9(6).                                
017500                                                                          
017600     03 WS-9KOMPL-GRUND          PIC 9(9) VALUE 999999999.                
017700                                                                          
018400 77    RKOD-ABEND                PIC S9(4)  VALUE +33   COMP SYNC.        
018500 77    IX                        PIC S9(9)  VALUE ZERO  COMP SYNC.        
018600 77    IX-CD-OMR                 PIC S9(9)  VALUE ZERO  COMP-3.           
018700                                                                          
018800 01    WS-FATTAS                 PIC S9(9)  VALUE +0.                     
018900     EJECT                                                                
019000 77    WS-KDMFSFOR               PIC X(1)   VALUE SPACE.                  
019100   88  SWEDISH-TEXT                         VALUE '1'.                    
019200   88  ENGLISH-TEXT                         VALUE '2'.                    
019300     SKIP2                                                                
019400 77    WS-IDTRANS                PIC X(04).                               
019500     SKIP2                                                                
019600 77    WS-INDATA-TEST            PIC X(01).                               
019700   88  WS-INDATA-FEL                        VALUE 'F'.                    
019800   88  WS-INDATA-RATT                       VALUE 'R'.                    
019900     SKIP2                                                                
020000 77    WS-BEHANDLING-TEST        PIC X(01).                               
020100   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
020200   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
020300                                                                          
020400 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
020500                                                                          
020600*                                                                         
020700 01  IDDC-USER.                                                           
020800     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
020900     03  IDDC-XX                 PIC X(2).                                
021000     03  FILLER                  PIC X(1) VALUE SPACE.                    
021100*                                                                         
021200 01     FILLER                  PIC X(9)   VALUE 'LOGG-AREA'.             
021300 01     LOGG-AREA               PIC X(75).                                
021400     SKIP2                                                                
021500 01     WS-TID-W.                                                         
021600   03   WS-TTMMSS                PIC 9(6).                                
021700   03   WS-HH                    PIC 9(2).                                
021800                                                                          
021900 01     WS-IDKUNDRF.                                                      
022000   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
022100   03   FILLER                   PIC X(5)   VALUE SPACE.                  
022200                                                                          
022300 01     WS-IDKUNDRF-OLD.                                                  
022400   03   WS-IDORDNR5-OLD          PIC 9(5).                                
022500   03   FILLER                   PIC X(5)   VALUE SPACE.                  
022600                                                                          
022700 01     WS-IDKUNDRF-NEW.                                                  
022800   03   WS-IDORDNR7-NEW          PIC 9(7).                                
022900   03   FILLER                   PIC X(3)   VALUE SPACE.                  
023000                                                                          
023100 01     WS-SUPTID-PRAPP           PIC 9(3)V99.                            
023200 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
023300   03   WS-SUPTID-TIM             PIC 9(3).                               
023400   03   WS-SUPTID-MIN             PIC 9(2).                               
023500                                                                          
023600 01 DB2-LASNING.                                                          
023700     03 FILLER                   PIC X(16)   VALUE                        
023800                                             'WS-DB2-SEKTION'.            
023900     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
024000                                                                          
024100     EJECT                                                                
024200 01 NYCKLAR-TP4TRAN.                                                      
024300     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
024400                                                                          
024500                                                                          
024600 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
024700 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
024800 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
024900   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
025000   03   FILLER                  PIC  X(4).                                
025100     SKIP2                                                                
025200 01     HJALP-4472-TIRFS        PIC 9(11).                                
025300 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
025400   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
025500   03   FILLER                  PIC  X(4).                                
025600 01    KDORDSTA-SW               PIC X(01).                               
025700   88  KDORDSTA-KLAR                        VALUE 'J'.                    
025800   88  KDORDSTA-EJ-KLAR                     VALUE 'N'.                    
025900                                                                          
026000*    PARAMETRAR FÖR EVENT                                                 
026100 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
026200     88 EVENT-OK                             VALUE 'LYNB'                 
026300                                                   'LYND'                 
026400                                                   'LYNV'                 
026500                                                   'LYNK'                 
026600                                                   'TADB'                 
026700                                                   'TADD'                 
026800                                                   'TADV'                 
026900                                                   'TAD '                 
027000                                                   'POLE'                 
027100                                                   'ACC '                 
027200                                                   'APA '                 
027300                                                   'APB '                 
027400                                                   'APC '                 
027500                                                   'APD '                 
027600                                                   'APE '                 
027700                                                   'APF '                 
027800                                                   'APG '                 
027900                                                   'APH '                 
028000                                                   'API '                 
028100                                                   'APJ '                 
028200                                                   'ECOM'.                
028300                                                                          
028400 77  CREATE-EVENT-SW              PIC X(1)   VALUE 'N'.                   
028500   88 CREATE-EVENT                           VALUE 'J'.                   
028600                                                                          
028610 77  SW-LYNK-NON-API              PIC X(1)   VALUE 'N'.                   
028620   88 LYNK-NON-API                           VALUE 'J'.                   
028630                                                                          
028640 77  SW-VOR                       PIC X(1)   VALUE 'N'.                   
028650   88 VOR                                    VALUE 'J'.                   
028660                                                                          
028700 01  WS-IDEVENTORDREF.                                                    
028800     03 WS-IDDISTR-EVENT         PIC 9(4).                                
028900     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
029000     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
029100     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
029200                                                                          
029300     SKIP2                                                                
029400 01  ABENDKODER.                                                          
029500     03  FILLER                  PIC X(16) VALUE 'ABENDKODER'.            
029600     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE   +16.         
029700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE   +33.         
029800     03  RKOD-FELTEXT            PIC X(32) VALUE SPACE.                   
029900     EJECT                                                                
030000 01    SPAR-AREA-RAD             PIC X(20)  VALUE 'SPAR-AREA-RAD'.        
030100 01               -COPY WDE411   -PRE SPAR-.                              
030200     EJECT                                                                
030300 01    FILLER                    PIC X(08)  VALUE 'TESTDIST'.             
030400 01    TEST-IDDISTR              PIC 9(5)         COMP-3.                 
030500     SKIP3                                                                
030600*01    FILLER  -COPY WWDIST03      -RED TEST-IDDISTR.                     
030700     EJECT                                                                
030800*01    FILLER  -COPY WWDIST07      -RED TEST-IDDISTR.                     
030900     EJECT                                                                
031000*01    FILLER  -COPY WWDIST18      -RED TEST-IDDISTR.                     
031100     SKIP3                                                                
031200*01    FILLER  -COPY WWDIST19      -RED TEST-IDDISTR.                     
031300     SKIP3                                                                
031400*01    FILLER  -COPY WWDIST20      -RED TEST-IDDISTR.                     
031500     EJECT                                                                
031600*01    FILLER  -COPY WWDIST35      -RED TEST-IDDISTR.                     
031700     EJECT                                                                
031800*01    FILLER  -COPY WWDIS128      -RED TEST-IDDISTR.                     
031900     EJECT                                                                
032000*    ----DISTR-DEALER-PRICE-----                                          
032100*01    FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                     
032200     EJECT                                                                
032300*01    FILLER  -COPY WWDIS103      -RED TEST-IDDISTR.                     
032400     EJECT                                                                
032500 01    FILLER                    PIC X(08)  VALUE 'TESTKUND'.             
032600 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
032700*   -COPY WWDIST57                                                        
032800     EJECT                                                                
032900 01  FILLER                      PIC X(08)  VALUE 'FRAKT1  '.             
033000*   -COPY WWFRAKT1                                                        
033100     EJECT                                                                
033200 01  FILLER                      PIC X(08)  VALUE 'DAGDAT  '.             
033300 01     DAGDAT.                                                           
033400   03   DAGDAT-AAMMD             PIC 9(6).                                
033500   03   DAGDAT-AAMMDD-X      REDEFINES DAGDAT-AAMMD.                      
033600     05 DAGDAT-AAMMDD-AA         PIC 9(2).                                
033700     05 DAGDAT-AAMMDD-MM         PIC 9(2).                                
033800     05 DAGDAT-AAMMDD-DD         PIC 9(2).                                
033900*                                                                         
034000   03   DAGDAT-AAVVD             PIC 9(5).                                
034100   03   DAGDAT-AAVVD-X       REDEFINES DAGDAT-AAVVD.                      
034200     05 DAGDAT-AAVVD-AA          PIC 9(2).                                
034300     05 DAGDAT-AAVVD-VV          PIC 9(2).                                
034400     05 DAGDAT-AAVVD-D           PIC 9(1).                                
034500     EJECT                                                                
034600*01  WDATAREA      -COPY WDATAREA.                                        
034700     EJECT                                                                
034800 01     FILLER                  PIC X(10)   VALUE 'FLAGGOR   '.           
034900*                                                                         
035000 01    FL-RADER-FINNS            PIC X(01).                               
035100   88  RADER-SLUT                           VALUE 'N'.                    
035200   88  RADER-FINNS                          VALUE 'J'.                    
035300     SKIP2                                                                
035400 01    WS-FLAUTFAK               PIC X(01).                               
035500   88  AUT-FAK-SKRIVS-EJ-UT                 VALUE 'N'.                    
035600   88  AUT-FAKTURA-SKRIVS-UT                VALUE 'J'.                    
035700     SKIP2                                                                
035800 01    FL-LASNINGSTRANS          PIC X(01).                               
035900   88  LASNINGSTRANS-TAS-EJ-BORT            VALUE 'N'.                    
036000   88  LASNINGSTRANS-TAS-BORT               VALUE 'J'.                    
036100     SKIP2                                                                
036200 01    FL-PACKADEORDERL          PIC X(01).                               
036300   88  SKRIV-EJ-PACKADEORDERL               VALUE 'N'.                    
036400   88  SKRIV-PACKADEORDERL                  VALUE 'J'.                    
036500*                                                                         
036600 01  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
036700   88 TRAFF-VORKO                            VALUE 'J'.                   
036800     SKIP2                                                                
036900 01    DYNAMISKA-SUBPROGRAM.                                              
037000   03   WDATKONV                PIC X(8)    VALUE 'WDATKONV'.             
037100   03   FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
037200   03   ABEND                   PIC X(8)    VALUE 'ABEND   '.             
037300   03   CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
037400   03   W005INIT                PIC X(8)    VALUE 'W005INIT'.             
037500   03   W403NILP                PIC X(8)    VALUE 'W403NILP'.             
037600   03   W006KOM                 PIC X(8)    VALUE 'W006KOM '.             
037700     SKIP3                                                                
037800 01  GEMENSAMMA-SUBPROGRAM.                                               
037900     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
038000     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
038100*        PRISFRÅGA                                                        
038200*                                                                         
038300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
038400 01    FILLER                   PIC X(16)   VALUE 'WMSGINIT '.            
038500*01 -COPY WMSGINIT                                                        
038600     EJECT                                                                
038700 01  FILLER                     PIC X(8)    VALUE 'W335PRQU'.             
038800*   -COPY W335PRQU                                                        
038900     EJECT                                                                
039000 01  FILLER                     PIC X(8)    VALUE 'W335PRNO'.             
039100*   -COPY W335PRNO                                                        
039200     EJECT                                                                
039300*                                                                         
039400 01  FILLER                      PIC X(16)   VALUE 'W403NILP'.            
039500*    --- PARAMETRAR TILL SUBPROGRAM W403NILP                              
039600*01 -COPY W403NILP                                                        
039700     SKIP3                                                                
039800*                                                                         
039900 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
040000 01     ARBETSAREOR.                                                      
040100   03   ARB-AREA-RAD.                                                     
040200     05 ARB-IDRADNR             PIC  9(4)         VALUE ZERO.             
040300     05 ARB-KVLEVART            PIC  9(6)         VALUE ZERO.             
040400     SKIP2                                                                
040500 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREA '.           
040600 01     SPAR-AREA.                                                        
040700*                                                                         
040800   03   SPAR-AREA.                                                        
040900     05 SPAR-KVORDRAD           PIC  S9(5)        VALUE ZERO.             
041000     05 SPAR-KVORDRAD-PACK      PIC  S9(5)        VALUE ZERO.             
041100     05 SPAR-IDDISTR            PIC  S9(5)        VALUE ZERO.             
041200     05 SPAR-IDKUNDNR           PIC  S9(7)        VALUE ZERO.             
041300     05 SPAR-KDFRAKT            PIC  S9(3)        VALUE ZERO.             
041400     05 SPAR-KDFAKTYP           PIC  X(1)         VALUE SPACE.            
041500     05 SPAR-VKORDNTO           PIC  9(6)V9(1)    VALUE ZERO.             
041600     05 SPAR-VKORDNTO-TOT       PIC  9(6)V9(1)    VALUE ZERO.             
041700     05 SPAR-VLORDNTO           PIC  9(4)V9(3)    VALUE ZERO.             
041800     05 SPAR-VLORDNTO-TOT       PIC  9(4)V9(3)    VALUE ZERO.             
041900     05 SPAR-KVKOLLI            PIC  S9(5)        VALUE ZERO.             
042000     05 SPAR-KDORDKL            PIC  S9(1)        VALUE ZERO.             
042100     05 SPAR-SUORDV             PIC  S9(9)V9(2)   VALUE ZERO.             
042200     05 SPAR-SUORDV-LOC         PIC  S9(9)V9(2)   VALUE ZERO.             
042300     05 SPAR-SUORDV-LOCPREL     PIC  S9(9)V9(2)   VALUE ZERO.             
042400     05 SPAR-SUORDV-TOT         PIC  S9(9)V9(2)   VALUE ZERO.             
042500     05 SPAR-SUORDV-TOT-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
042600     05 SPAR-SUORDV-TOT-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
042700     05 SPAR-IDKUNDRF           PIC X(10)         VALUE SPACE.            
042800     05 SPAR-BEKUNDRF           PIC X(15)         VALUE SPACE.            
042900     EJECT                                                                
043000 01    FILLER        PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                
043100*                                                                         
043200 01    NYCKLAR-TILL-DLI.                                                  
043300*                                                                         
043400   03  W-WDQ2CSEQ-X.                                                      
043500       05  W-WDQ2CSEQ-IDGMTREF.                                           
043600           07  W-WDQ2CSEQ-IDDISTR                                         
043700                               PIC S9(5) COMP-3 VALUE +0.                 
043800           07  W-WDQ2CSEQ-IDKUNDNR                                        
043900                               PIC S9(7) COMP-3 VALUE +0.                 
044000           07  W-WDQ2CSEQ-IDKUNDRF                                        
044100                               PIC X(10)     VALUE '0000000   '.          
044200*                                                                         
044300   03    W-WDE401-KUNDORDER-X.                                            
044400     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
044500     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
044600     05    W-401-IDKUNDRF.                                                
044700       07  W-401-IDORDNR         PIC X(5)    VALUE SPACE.                 
044800       07  FILLER                PIC X(5)    VALUE SPACE.                 
044900     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045000     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
045100*                                                                         
045200   03    W-WDE4ASEQ-X.                                                    
045300     05    W-4A1-IDGMTREF.                                                
045400       07    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.        
045500       07    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.        
045600       07    W-4A1-IDKUNDRF.                                              
045700             09    W-4A1-IDORDNR   PIC X(5)    VALUE SPACE.               
045800             09    FILLER          PIC X(5)    VALUE SPACE.               
045900*                                                                         
046000   03    W-WDE4B-KEYSEQ-X.                                                
046100     05    W-411-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
046200     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
046300*                                                                         
046400   03    W-WDE4B-MIN-X.                                                   
046500     05    W-IDPRODNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
046600     05    W-IDPURAD-MIN         PIC S9(5)   VALUE ZERO  COMP-3.          
046700*                                                                         
046800   03    W-WDE4B-MAX-X.                                                   
046900     05    W-IDPRODNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
047000     05    FILLER                PIC X(3)    VALUE HIGH-VALUE.            
047100*                                                                         
047200   03    W-WDE411-IDPRODNR2-MIN-X.                                        
047300     05    W-411-IDPRODNR2-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
047400     05    FILLER                PIC  X(3)   VALUE LOW-VALUE.             
047500*                                                                         
047600   03    W-WDE411-IDPRODNR2-MAX-X.                                        
047700     05    W-411-IDPRODNR2-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
047800     05    FILLER                PIC  X(3)   VALUE HIGH-VALUE.            
047900*                                                                         
048000   03    W-WDE601-IDPRODNR-X.                                             
048100     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
048200*                                                                         
048300   03    W-WDE611-IDKOLLI-X.                                              
048400     05    W-611-IDKOLLI         PIC S9(5)   VALUE +99000 COMP-3.         
048500     SKIP2                                                                
048600   03    W-WDA601KY-MIN-X.                                                
048700     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
048800     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
048900     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
049000     05    FILLER REDEFINES W-A601KY-MIN-IDKUNDRF.                        
049100        07  W-A601KY-MIN-IDORDNR     PIC 9(07).                           
049200        07  FILLER                   PIC X(03).                           
049300     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
049400     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
049500     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
049600     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
049700     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
049800     SKIP2                                                                
049900   03    W-WDA601KY-MAX-X.                                                
050000     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
050100     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
050200     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
050300     05    FILLER REDEFINES W-A601KY-MAX-IDKUNDRF.                        
050400        07  W-A601KY-MAX-IDORDNR     PIC 9(07).                           
050500        07  FILLER                   PIC X(03).                           
050600     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
050700     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
050800     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
050900     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
051000     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
051100*                                                                         
051200   03    W-WDA6BSEQ-MIN-X.                                                
051300     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
051400     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
051500     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
051600     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
051700     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
051800     05    FILLER                    PIC X(14) VALUE SPACE.               
051900     SKIP2                                                                
052000   03    W-WDA6BSEQ-MAX-X.                                                
052100     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
052200     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
052300     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
052400     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
052500     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
052600     05    FILLER                    PIC X(14) VALUE SPACE.               
052700*                                                                         
052800   03    Q301-WDQ301KY-X.                                                 
052900     05    Q301-IDORDER          PIC S9(7)   VALUE ZERO  COMP-3.          
053000     05    Q301-IDDC             PIC X(2)    VALUE SPACE.                 
053100     05    Q301-IDPRODNR         PIC S9(7)   VALUE ZERO  COMP-3.          
053200     05    Q301-IDPLKLST         PIC S9(3)   VALUE ZERO  COMP-3.          
053300                                                                          
053400   03  W-WDQ301KY-MIN.                                                    
053500     05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                       
053600     05  W-Q301KY-MIN-IDDC       PIC X(2)   VALUE SPACE.                  
053700     05  FILLER                  PIC X(6)   VALUE LOW-VALUE.              
053800                                                                          
053900   03  W-WDQ301KY-MAX.                                                    
054000     05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                       
054100     05  W-Q301KY-MAX-IDDC       PIC X(2)   VALUE SPACE.                  
054200     05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.             
054300                                                                          
054400   03  W-KDODELST                PIC X.                                   
054500                                                                          
054600*                                                                         
054700     03  W-WDQ101KY-MIN-X.                                                
054800         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
054900         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
055000         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
055100         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
055200         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
055300                                                                          
055400     03  W-WDQ101KY-MAX-X.                                                
055500         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
055600         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
055700         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
055800         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
055900         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
056000*                                                                         
056100     03  W-IDORDER-X.                                                     
056200         05  W-IDORDER-Q2        PIC S9(7)   COMP-3.                      
056300*                                                                         
056400     03  W-IDDC-Q2-X.                                                     
056500         05  W-IDDC-Q2           PIC X(2).                                
056600*                                                                         
056700   03    W-4305-X.                                                        
056800     05    FILLER                PIC X(4) VALUE '4305'.                   
056900     05    W-XXDJ-IDDC           PIC X(2)  VALUE SPACE.                   
057000     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
057100*                                                                         
057200   03    W-4306-X.                                                        
057300     05    W-XXDJ-IDPRODNR       PIC S9(7) COMP-3.                        
057400     05    FILLER                PIC X(6)  VALUE LOW-VALUE.               
057500*                                                                         
057600   03    W-4726-X.                                                        
057700     05    FILLER                PIC X(4) VALUE '4726'.                   
057800     05    W-XXDV-FLBATCH        PIC X(1) VALUE SPACE.                    
057900     05    FILLER                PIC X(25) VALUE LOW-VALUE.               
058000     EJECT                                                                
058100*                                                                         
058200   03    W-4319-X.                                                        
058300     05    FILLER                PIC X(4)  VALUE '4319'.                  
058400     05    W-XXJD-TIREGDAT       PIC S9(7) COMP-3.                        
058500     05    FILLER                PIC X(22) VALUE LOW-VALUE.               
058600*                                                                         
058700   03    W-4320-X.                                                        
058800     05    W-XXJD-IDDISTR        PIC S9(5) COMP-3.                        
058900     05    W-XXJD-IDKUNDNR       PIC S9(7) COMP-3.                        
059000     05    W-XXJD-IDORDNR        PIC S9(5) COMP-3.                        
059100     05    W-XXJD-IDPRODNR       PIC S9(7) COMP-3.                        
059200     05    W-XXJD-IDARTNR        PIC S9(9) COMP-3.                        
059300     05    W-XXJD-REKSIFFR       PIC S9    COMP-3.                        
059400*                                                                         
059500   03    W-IDARTNR-X.                                                     
059600     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
059700*                                                                         
059800   03    W-KDSEGKEY-1-X.                                                  
059900     05    W-KDSEGKEY            PIC X       VALUE '1'.                   
060000*                                                                         
060100   03    W-WDK711-IDDC-X.                                                 
060200     05    W-711-IDDC            PIC X(2).                                
060300*                                                                         
060400   03    W-IDLAND-X.                                                      
060500     05    W-IDLAND              PIC X(2).                                
060600*                                                                         
060700   03    W-WDK901-IDARTNR-X.                                              
060800     05    W-901-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.          
060900*                                                                         
061000   03    W-WDG6KEY-X.                                                     
061100     05    W-RDG-TIAAMMDD        PIC 9(6)    VALUE ZERO.                  
061200     05    W-RDG-TIKLOCK         PIC 9(8)    VALUE ZERO.                  
061300     05    W-RDG-IDLOGLOP        PIC 9(1)    VALUE ZERO.                  
061400     05    W-RDG-IDPTYP          PIC X(3)    VALUE 'RY1'.                 
061500*                                                                         
061600   03    W-WDGX11-WDGXKEY-X.                                              
061700     05    W-RDG-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
061800     05    W-RDG-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
061900     05    W-RDG-IDDC            PIC X(2)    VALUE SPACE.                 
062000     05    W-RDG-KDFAKTYP        PIC X(1)    VALUE SPACE.                 
062100*                                                                         
062200   03    W-WDGX01.                                                        
062300     05    W-IDHTYP-N5           PIC X(04)   VALUE '4511'.                
062400     05    W-VALFRI-N5           PIC X(26)   VALUE SPACE.                 
062500*                                                                         
062600   03    W-WDGXKEY-N5-MIN.                                                
062700     05    FILLER                PIC X(10)   VALUE SPACE.                 
062800*                                                                         
062900   03    W-WDGXKEY-N5-MAX.                                                
063000     05    FILLER                PIC X(10)   VALUE SPACE.                 
063100*                                                                         
063200   03    W-KDRAPRIO-N5-MIN-X.                                             
063300     05    W-KDRAPRIO-N5-MIN     PIC S9(3)   COMP-3 VALUE ZERO.           
063400*                                                                         
063500   03    W-KDRAPRIO-N5-MAX-X.                                             
063600     05    W-KDRAPRIO-N5-MAX     PIC S9(3)   COMP-3 VALUE ZERO.           
063700*                                                                         
063800   03    W-KDTPOTYP-N5-X.                                                 
063900     05    W-KDTPOTYP-N5         PIC S9(1)   COMP-3 VALUE ZERO.           
064000*                                                                         
064100   03    W-KDORDKL-N5-X.                                                  
064200     05    W-KDORDKL-N5          PIC S9(1)   COMP-3 VALUE ZERO.           
064300*                                                                         
064400   03    W-IDDISTR-FOM-N5-X.                                              
064500     05    W-IDDISTR-FOM-N5      PIC S9(5)   COMP-3 VALUE ZERO.           
064600*                                                                         
064700   03    W-IDDISTR-TOM-N5-X.                                              
064800     05    W-IDDISTR-TOM-N5      PIC S9(5)   COMP-3 VALUE ZERO.           
064900*                                                                         
065000   03    W1-WDA501KY-X.                                                   
065100     05    W1-IDDISTR            PIC S9(5)   COMP-3.                      
065200     05    W1-IDKUNDNR           PIC S9(7)   COMP-3.                      
065300     05    W1-IDKUNDRF           PIC X(10).                               
065400     05    W1-IDARTNR            PIC S9(9)   COMP-3.                      
065500     05    W1-IDLOPNR            PIC S9(3)   COMP-3.                      
065600*                                                                         
065700   03    W2-WDA501KY-X.                                                   
065800     05    W2-IDDISTR            PIC S9(5)   COMP-3.                      
065900     05    W2-IDKUNDNR           PIC S9(7)   COMP-3.                      
066000     05    W2-IDKUNDRF           PIC X(10).                               
066100     05    W2-IDARTNR            PIC S9(9)   COMP-3.                      
066200     05    W2-IDLOPNR            PIC S9(3)   COMP-3.                      
066300*                                                                         
066400   03    W-KDSTARAD-X.                                                    
066500     05    W-KDSTARAD            PIC X.                                   
066600*                                                                         
066700   03  W-WDA501KY-A5-MIN-X.                                               
066800       05  W-IDDISTR-A5-MIN          PIC S9(5) VALUE ZERO COMP-3.         
066900       05  W-IDKUNDNR-A5-MIN         PIC S9(7) VALUE ZERO COMP-3.         
067000       05  FILLER                    PIC X(17) VALUE LOW-VALUE.           
067100                                                                          
067200   03  W-WDA501KY-A5-MAX-X.                                               
067300       05  W-IDDISTR-A5-MAX          PIC S9(5) VALUE ZERO COMP-3.         
067400       05  W-IDKUNDNR-A5-MAX         PIC S9(7) VALUE ZERO COMP-3.         
067500       05  FILLER                    PIC X(17) VALUE HIGH-VALUE.          
067600*                                                                         
067700   03  W-KDORDKL-X.                                                       
067800       05  W-KDORDKL                 PIC S9    VALUE ZERO COMP-3.         
067900                                                                          
068000   03    W-IDKUNDRF-LEV-X.                                                
068100     05  W-IDKUNDRF-LEV              PIC X(10)   VALUE SPACE.             
068200*                                                                         
068300   03  W-WDGXKEY-4541-X.                                                  
068400     05 W-WDGXKEY-4541           PIC X(4)    VALUE '4541'.                
068500     05 FILLER                   PIC X(26)   VALUE LOW-VALUE.             
068600*                                                                         
068700   03    W-WDGXKEY-4471-X.                                                
068800     05    W-4471-IDHTYP          PIC X(4)  VALUE '4471'.                 
068900     05    W-4471-IDDC            PIC X(2)  VALUE SPACE.                  
069000     05    W-4471-IDPRC.                                                  
069100        07 W-4471-IDPRCBAS        PIC X(3).                               
069200        07 W-4471-IDPRCVAR        PIC X(1).                               
069300     05    FILLER                 PIC X(20) VALUE LOW-VALUE.              
069400*                                                                         
069500   03    W-KDSEGKEY-4472-X.                                               
069600     05    W-4472-KDSEGKEY        PIC X(1)  VALUE '1'.                    
069700*                                                                         
069800   03    W-WDGXKEY-4477-X.                                                
069900     05    W-4477-IDHTYP          PIC X(4)  VALUE '4477'.                 
070000     05    W-4477-IDDC            PIC X(2)  VALUE SPACE.                  
070100     05    FILLER                 PIC X(24) VALUE LOW-VALUE.              
070200*                                                                         
070300   03    W-WDGXKEY-4478-X.                                                
070400     05    W-4478-IDSHIFT         PIC X(1).                               
070500     05    W-4478-IDUSER          PIC X(8).                               
070600     05    FILLER                 PIC X(1)  VALUE LOW-VALUE.              
070700*                                                                         
070800   03    W-WDM201-X.                                                      
070900     05    W-KAMP-IDKAMPRF      PIC S9(07)   VALUE ZERO COMP-3.           
071000     05    W-KAMP-IDDC          PIC X(02)    VALUE SPACE.                 
071100                                                                          
071200   03    W-WDM211-X.                                                      
071300     05    W-KART-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.           
071400                                                                          
071500   03    W-WDM221-X.                                                      
071600     05    W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.             
071700     05    W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.             
071800     05    W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.             
071900     05    W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.             
072000     EJECT                                                                
072100   03    W-2203-X.                                                        
072200     05    FILLER                PIC X(4)  VALUE '2203'.                  
072300     05    W-2203-IDDC           PIC X(2)  VALUE SPACE.                   
072400     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
072500*                                                                         
072600   03    W-4447-X.                                                        
072700     05    FILLER                PIC X(4)  VALUE '4447'.                  
072800     05    W-4447-IDDC           PIC X(2)  VALUE SPACE.                   
072900     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
073000*                                                                         
073100   03    W-4448-X.                                                        
073200     05    W-4448-IDPRC          PIC X(4).                                
073300     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
073400*                                                                         
073500   03    W-4487-X.                                                        
073600     05    FILLER                PIC X(4)  VALUE '4487'.                  
073700     05    W-4487-IDDC           PIC X(2)  VALUE SPACE.                   
073800     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
073900*                                                                         
074000   03    W-4488-X.                                                        
074100     05    W-4488-KDPRCGRP       PIC X(5).                                
074200*                                                                         
074300   03    W-4490-X.                                                        
074400     05    W-4490-DARFS          PIC 9(12).                               
074500     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
074600     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
074700*  03    -COPY WDGX01                                                     
074800                                                                          
074900     03  W-IDDC-B6-X.                                                     
075000         05 W-IDDC-B6                  PIC X(2).                          
075100     03  W-IDGMT-X.                                                       
075200         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
075300         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
075400                                                                          
075500   03  W-IDDISTR-P4-X.                                                    
075600       05 W-IDDISTR-P4             PIC S9(5)   VALUE ZERO  COMP-3.        
075700*                                                                         
075800******************************************************************        
075900*                                                                *        
076000*                                                                *        
076100*                                                                *        
076200******************************************************************        
076300*    --- AREOR FÖR HANTERING AV API                                       
076400*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSG'.         
076500 01  KOM-IO-AREA.                                                         
076600     03  -COPY WMSGKOM                                                    
076700                                                                          
076800*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-A '.        
076900*01  -COPY WZ0430I1  -PRE Z430-                                           
077000*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
077100     SKIP3                                                                
077200 01    FILLER                 PIC X(16) VALUE 'MID W4I39801 MID'.         
077300     SKIP3                                                                
077400*01    MID -COPY W4I39801.                                                
077500     EJECT                                                                
077600 01  FILLER                   PIC X(16) VALUE 'MOD W40636I1 MOD'.         
077700*01  -COPY W40636I1   -PRE MOD4636-                                       
077800     EJECT                                                                
077900*01    -COPY WMSGAREA                                                     
078000     EJECT                                                                
078100 01  FILLER                    PIC X(16) VALUE 'ALT2191-IO-AREA'.         
078200 01  ALT2191-IO-AREA.                                                     
078300  03     ALT2191-LL               PIC S9(4) COMP SYNC.                    
078400  03     ALT2191-Z1               PIC X(1)  VALUE LOW-VALUE.              
078500  03     ALT2191-Z2               PIC X(1)  VALUE LOW-VALUE.              
078600  03     ALT2191-TRANSKOD         PIC X(8)  VALUE 'W2T191X '.             
078700  03     ALT2191-IDTRANS          PIC X(4)  VALUE '4398'.                 
078800  03     ALT2191-SPRAK            PIC X(1).                               
078900* 03     MID -COPY W2I19101   -PRE ALT2191-                               
079000     EJECT                                                                
079100******************************************************************        
079200*                                                                         
079300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
079400*                                                                         
079500 01    IMS-WS.                                                            
079600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
079700     SKIP3                                                                
079800*                        **** STATUS-KOD FRÅN IMS                         
079900   03    STATUS-WS               PIC XX.                                  
080000     88    SEGMENT-FINNS                     VALUE '  '.                  
080100     88    ISRT-OK                           VALUE '  '.                  
080200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
080300     88    SEGMENT-SLUT                      VALUE 'GB'.                  
080400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
080500     SKIP3                                                                
080600   03    GODK-STATUSKODER.                                                
080700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
080800     SKIP3                                                                
080900 01    SSA1                      PIC X(160).                              
081000 01    SSA2                      PIC X(160).                              
081100 01    SSA3                      PIC X(128).                              
081200 01    SSA4                      PIC X(128).                              
081300     EJECT                                                                
081400*                            IMS FUNKTIONSKODER                           
081500     EJECT                                                                
081600*                            DB2 FUNKTIONSKODER                           
081700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
081800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
081900                                                                          
082000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
082100 01  DB2-WS.                                                              
082200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
082300         88  CURSOR-OK                       VALUE 000.                   
082400         88  DB2-RADER-FINNS                 VALUE 000.                   
082500         88  RADER-SAKNAS                    VALUE 100.                   
082600         88  ATKOMST-FEL                     VALUE 904.                   
082700     03  GODK-SQLCODEKODER.                                               
082800         05  GODK-SQLCODE OCCURS 5                                        
082900             INDEXED BY SQLCODE-IX PIC 9(3).                              
083000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
083100     EJECT                                                                
083200*    --- IMS FUNKTIONSKODER                                               
083300*01    -COPY W0003                                                        
083400   03    ROLB                PIC X(4)    VALUE 'ROLB'.                    
083500     EJECT                                                                
083600*                            DLI INPUT-OUTPUT AREA                        
083700 01  FILLER                      PIC X(16) VALUE 'IO-E411  '.             
083800 01  DLI-IO-E411.                                                         
083900*  03             -COPY WDE411                                            
084000     EJECT                                                                
084100 01  FILLER                      PIC X(16) VALUE 'IO-E601  '.             
084200 01  DLI-IO-E601.                                                         
084300*  03             -COPY WDE601                                            
084400     EJECT                                                                
084500 01  FILLER                      PIC X(16) VALUE 'IO-E611  '.             
084600 01  DLI-IO-E611.                                                         
084700*  03             -COPY WDE611                                            
084800     EJECT                                                                
084900 01  FILLER                      PIC X(16) VALUE 'IO-DJ01  '.             
085000 01  DLI-IO-DJ01.                                                         
085100*  03    WLXXDJ01 -COPY WDGX4305                                          
085200     EJECT                                                                
085300 01  FILLER                      PIC X(16) VALUE 'IO-DJ11  '.             
085400 01  DLI-IO-DJ11.                                                         
085500*  03    WLXXDJ11 -COPY WDGX4306                                          
085600     EJECT                                                                
085700 01  FILLER                      PIC X(16) VALUE 'IO-AREA-3'.             
085800 01    DLI-IO-AREA3.                                                      
085900   03    IO-AREA3                PIC X(200)  VALUE SPACE.                 
086000     SKIP3                                                                
086100*  03    WDGX2204 -COPY WDGX2204             -RED IO-AREA3.               
086200     EJECT                                                                
086300*  03    WDGX4726 -COPY WDGX4726             -RED IO-AREA3.               
086400     SKIP2                                                                
086500*  03    WDGX4727 -COPY WDGX4727             -RED IO-AREA3.               
086600     EJECT                                                                
086700 01  FILLER                      PIC X(16) VALUE 'IO-AREA-4'.             
086800 01    DLI-IO-AREA4.                                                      
086900   03    IO-AREA4                PIC X(200)  VALUE SPACE.                 
087000     SKIP3                                                                
087100*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA4.               
087200     EJECT                                                                
087300*01      WDGZRY1  -COPY WDGZRY1.                                          
087400     EJECT                                                                
087500*01      WDGZRY1S -COPY WDGZRY1S.                                         
087600     EJECT                                                                
087700*01      WDGZRY6  -COPY WDGZRY6.                                          
087800     EJECT                                                                
087900*01      WDGZRYK  -COPY WDGZRYK.                                          
088000     EJECT                                                                
088100 01  FILLER                      PIC X(16) VALUE 'IO-AREA-8'.             
088200 01    DLI-IO-AREA8.                                                      
088300   03    IO-AREA8                PIC X(150)  VALUE SPACE.                 
088400     SKIP3                                                                
088500*  03  WDGX4512   -COPY WDGX4512 -PRE STYR-   -RED IO-AREA8.              
088600     EJECT                                                                
088700 01  FILLER                      PIC X(16) VALUE 'IO-AREA-9'.             
088800 01    DLI-IO-AREA9.                                                      
088900   03    IO-AREA9                PIC X(396)  VALUE SPACE.                 
089000     SKIP3                                                                
089100*  03  WDA501     -COPY WDA501                -RED IO-AREA9.              
089200     EJECT                                                                
089300 01  FILLER                      PIC X(16) VALUE 'IO-AREA-10'.            
089400 01    DLI-IO-AREA10.                                                     
089500   03    IO-AREA10               PIC X(396)  VALUE SPACE.                 
089600     SKIP3                                                                
089700*  03  WDA501     -COPY WDA501   -PRE OLD-    -RED IO-AREA10.             
089800     EJECT                                                                
089900 01  FILLER                      PIC X(16) VALUE 'IO-AREA-11'.            
090000 01    DLI-IO-AREA11.                                                     
090100   03    IO-AREA11               PIC X(900)  VALUE SPACE.                 
090200     SKIP3                                                                
090300*  03  WLARTC01   -COPY WDK601                -RED IO-AREA11.             
090400     SKIP3                                                                
090500*  03  WLARTC11   -COPY WDK611                -RED IO-AREA11.             
090600     EJECT                                                                
090700 01  FILLER                      PIC X(16) VALUE 'IO-WDK7-11'.            
090800 01    DLI-IO-WDK711.                                                     
090900   03    IO-WDK711               PIC X(512)  VALUE SPACE.                 
091000     SKIP3                                                                
091100*  03  WDK711   -COPY WDK711                -RED IO-WDK711.               
091200     SKIP3                                                                
091300 01  FILLER                      PIC X(16) VALUE 'IO-WDK7-22'.            
091400 01    DLI-IO-WDK722.                                                     
091500*  03  WDK722   -COPY WDK722                                              
091600     EJECT                                                                
091700 01  FILLER                      PIC X(16) VALUE 'IO-WDK7-12'.            
091800 01    DLI-IO-WDK712.                                                     
091900*  03  WDK712   -COPY WDK712                                              
092000     EJECT                                                                
092100 01  FILLER                      PIC X(16) VALUE 'IO-AREA-12'.            
092200 01    DLI-IO-AREA12.                                                     
092300   03    IO-AREA12             PIC X(90)     VALUE SPACE.                 
092400     SKIP3                                                                
092500*  03  WDGX4487   -COPY WDGX4487 -RED IO-AREA12.                          
092600     EJECT                                                                
092700*  03  WDGX4490   -COPY WDGX4490 -RED IO-AREA12.                          
092800     EJECT                                                                
092900 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
093000 01    DLI-IO-AREA17.                                                     
093100*  03    WDA601   -COPY WDA601                                            
093200     EJECT                                                                
093300 01  FILLER                      PIC X(16)  VALUE 'IO-AREA13'.            
093400 01  DLI-IO-AREA13.                                                       
093500     03  IO-AREA13               PIC X(1200) VALUE SPACE.                 
093600     SKIP2                                                                
093700     03  WLXXLB11 -COPY WDGX4478  -RED IO-AREA13.                         
093800     EJECT                                                                
093900     03  WLXXKW11 -COPY WDGX4472  -RED IO-AREA13.                         
094000     EJECT                                                                
094100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
094200 01  DLI-IO-WDM211.                                                       
094300*    03 -COPY WDM211                                                      
094400     EJECT                                                                
094500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
094600 01  DLI-IO-WDM221.                                                       
094700*    03 -COPY WDM221                                                      
094800     EJECT                                                                
094900 01  FILLER                      PIC X(16)  VALUE 'Q301-AREA'.            
095000*01    WLORQA01 -COPY WDQ301.                                             
095100     EJECT                                                                
095200 01  FILLER                      PIC X(16)  VALUE 'E401-AREA'.            
095300*01             -COPY WDE401.                                             
095400     EJECT                                                                
095500 01  FILLER                      PIC X(16)  VALUE 'Q101-AREA'.            
095600*01    WLORQM01 -COPY WDQ101.                                             
095700     EJECT                                                                
095800 01  FILLER                      PIC X(16)  VALUE 'K901-AREA'.            
095900*01    WLARTM01 -COPY WDK901.                                             
096000     EJECT                                                                
096100 01  FILLER                      PIC X(16)  VALUE '4542-AREA'.            
096200*01  WDGX4542 -COPY WDGX4542.                                             
096300     EJECT                                                                
096400 01  FILLER                      PIC X(16)  VALUE 'Q201-AREA'.            
096500*01  -COPY WDQ201.                                                        
096600     EJECT                                                                
096700 01  FILLER                      PIC X(16)  VALUE 'Q212-AREA'.            
096800*01  -COPY WDQ212.                                                        
096900     EJECT                                                                
097000 01  FILLER                      PIC X(16)  VALUE '4448-AREA'.            
097100*01  WLXXKH11 -COPY WDGX4448.                                             
097200     EJECT                                                                
097300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-XXJD'.         
097400*01    WLXXJD01 -COPY WDGX4319.                                           
097500     EJECT                                                                
097600*01    WLXXJD11 -COPY WDGX4320.                                           
097700     EJECT                                                                
097800 01    FILLER                 PIC X(16) VALUE 'MID W4I34201 MID'.         
097900     SKIP3                                                                
098000*01    MID -COPY W4I34201  -PRE 4342-.                                    
098100     EJECT                                                                
098200 01    FILLER                 PIC X(16) VALUE 'MID W0I60502 MID'.         
098300     SKIP3                                                                
098400*01    MID -COPY W0I60502  -PRE 0605-.                                    
098500     EJECT                                                                
098600 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
098700 01  DLI-IO-WLLOGA01.                                                     
098800*    03  WLLOGA01  -COPY WDL901                                           
098900                                                                          
099000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
099100 01   DLI-IO-AREA-B601.                                                   
099200*     03  -COPY WDB601                                                    
099300     EJECT                                                                
099400 01  FILLER                PIC X(16)   VALUE 'WDB201-AREA'.               
099500 01  DLI-IO-AREA-WDB201.                                                  
099600*     03  -COPY WDB201                                                    
099700                                                                          
099800 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
099900 01   DLI-IO-AREA-WDP4A1.                                                 
100000*     03  -COPY WDP4A1                                                    
100100     EJECT                                                                
100200 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
100300                                                                          
100400*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
100500     EJECT                                                                
100600     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
100700     EJECT                                                                
100800     EJECT                                                                
100900 LINKAGE SECTION.                                                         
101000 01  0693-PCB     PIC X.                                                  
101100*01    -COPY W0009     -PRE MSG-                                          
101200                                                                          
101300*01    -COPY W0009     -PRE ALT0605-                                      
101400     EJECT                                                                
101500*01    -COPY W0009     -PRE ALT2191-                                      
101600                                                                          
101700*01    -COPY W0009     -PRE ALT4342-                                      
101800     EJECT                                                                
101900*01    -COPY W0009     -PRE MAIL-                                         
102000     EJECT                                                                
102100*01    -COPY W0009     -PRE DISTRDOC-                                     
102200     EJECT                                                                
102300*01    -COPY W0008     -PRE USEA-                                         
102400     05  FILLER                  PIC X.                                   
102500                                                                          
102600*01    -COPY W0008     -PRE WDE41-                                        
102700     05  FILLER                  PIC X.                                   
102800     EJECT                                                                
102900*01    -COPY W0008     -PRE WDE42-                                        
103000     05  FILLER                  PIC X.                                   
103100                                                                          
103200*01    -COPY W0008     -PRE WDE43-                                        
103300     05  FILLER                  PIC X.                                   
103400     EJECT                                                                
103500*01    -COPY W0008     -PRE WDE6-                                         
103600     05  FILLER                  PIC X.                                   
103700                                                                          
103800*01    -COPY W0008     -PRE ZZAC-                                         
103900     05  FILLER                  PIC X.                                   
104000     EJECT                                                                
104100*01    -COPY W0008     -PRE ARTC-                                         
104200     05  FILLER                  PIC X.                                   
104300     EJECT                                                                
104400*01    -COPY W0008     -PRE XXDJ-                                         
104500     05  FILLER                  PIC X.                                   
104600                                                                          
104700*01    -COPY W0008     -PRE AUTF-                                         
104800     05  FILLER                  PIC X.                                   
104900     EJECT                                                                
105000*01  -COPY W0008     -PRE XXJD-                                           
105100     05  FILLER                  PIC X.                                   
105200     EJECT                                                                
105300*01  -COPY W0008     -PRE ORDP1-                                          
105400     05  FILLER                  PIC X.                                   
105500                                                                          
105600*01  -COPY W0008     -PRE ORDP2-                                          
105700     05  FILLER                  PIC X.                                   
105800     EJECT                                                                
105900*01  -COPY W0008     -PRE XXJN-                                           
106000     05  FILLER                  PIC X.                                   
106100                                                                          
106200*01  -COPY W0008     -PRE ORQA-                                           
106300     05  FILLER                  PIC X.                                   
106400     EJECT                                                                
106500*01  -COPY W0008     -PRE ORQM-                                           
106600     05  FILLER                  PIC X.                                   
106700                                                                          
106800*01  -COPY W0008     -PRE ARTM-                                           
106900     05  FILLER                  PIC X.                                   
107000     EJECT                                                                
107100*01  -COPY W0008     -PRE 4541-                                           
107200     05  FILLER                  PIC X.                                   
107300                                                                          
107400*01  -COPY W0008     -PRE 4487-                                           
107500     05  FILLER                  PIC X.                                   
107600     EJECT                                                                
107700*01  -COPY W0008     -PRE XXKW-                                           
107800     05  FILLER                  PIC X.                                   
107900                                                                          
108000*01  -COPY W0008     -PRE XXLB-                                           
108100     05  FILLER                  PIC X.                                   
108200     EJECT                                                                
108300*01  -COPY W0008     -PRE WDM2-                                           
108400     05  FILLER                  PIC X.                                   
108500                                                                          
108600*01    -COPY W0008     -PRE ORQI-                                         
108700     05  FILLER                  PIC X.                                   
108800                                                                          
108900*01    -COPY W0008     -PRE ORQICSQ-                                      
109000     05  FILLER                  PIC X.                                   
109100     EJECT                                                                
109200*01    -COPY W0008     -PRE XXKH-                                         
109300     05  FILLER                  PIC X.                                   
109400                                                                          
109500*01    -COPY W0008     -PRE WDK7-                                         
109600     05  FILLER                  PIC X.                                   
109700     EJECT                                                                
109800*01  -COPY W0008  -PRE WLLOGA-                                            
109900     05  FILLER                  PIC X.                                   
110000     EJECT                                                                
110100*01  -COPY W0008  -PRE WDK6-                                              
110200     05  FILLER                  PIC X.                                   
110300     EJECT                                                                
110400*01  -COPY W0008  -PRE WDA6B-                                             
110500     05  FILLER                  PIC X.                                   
110600     EJECT                                                                
110700*01  -COPY W0008  -PRE WDA6-                                              
110800     05  FILLER                  PIC X.                                   
110900     EJECT                                                                
111000*01  -COPY W0008  -PRE WDB6-                                              
111100     05  FILLER                  PIC X.                                   
111200     EJECT                                                                
111300*01  -COPY W0008  -PRE WDB2-                                              
111400     05  FILLER                  PIC X.                                   
111500     EJECT                                                                
111600*01  -COPY W0008  -PRE WDP4A-                                             
111700     05  FILLER                  PIC X.                                   
111800*                                                                         
111900 01  KOM-WDP8-PCB                PIC X.                                   
112000*                                                                         
112100 01  PRQU-WDG2-PCB               PIC X.                                   
112200 01  PRQU-WDC7-PCB               PIC X.                                   
112300 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
112400                                                                          
112500 01  PRNO-3107-PCB               PIC X.                                   
112600     EJECT                                                                
112700 PROCEDURE DIVISION USING  MSG-PCB 0693-PCB                               
112800         ALT0605-PCB ALT2191-PCB                                          
112900         ALT4342-PCB MAIL-PCB DISTRDOC-PCB USEA-PCB                       
113000         WDE41-PCB WDE42-PCB WDE43-PCB                                    
113100         WDE6-PCB ZZAC-PCB ARTC-PCB  XXDJ-PCB AUTF-PCB                    
113200         XXJD-PCB ORDP1-PCB ORDP2-PCB XXJN-PCB ORQA-PCB                   
113300         ORQM-PCB ARTM-PCB 4541-PCB  4487-PCB  XXKW-PCB XXLB-PCB          
113400         WDM2-PCB ORQI-PCB  ORQICSQ-PCB XXKH-PCB                          
113500         WDK7-PCB WLLOGA-PCB                                              
113600         WDK6-PCB  WDA6B-PCB WDA6-PCB WDB6-PCB WDB2-PCB                   
113700         WDP4A-PCB                                                        
113800         KOM-WDP8-PCB                                                     
113900         PRQU-WDG2-PCB                                                    
114000         PRQU-WDC7-PCB                                                    
114100         PRQU-SJKO-WDK6-PCB                                               
114200         PRNO-3107-PCB.                                                   
114300                                                                          
114400 MAIN SECTION.                                                            
114500     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB                               
114600         ALT0605-PCB ALT2191-PCB                                          
114700         ALT4342-PCB MAIL-PCB DISTRDOC-PCB USEA-PCB                       
114800         WDE41-PCB WDE42-PCB WDE43-PCB                                    
114900         WDE6-PCB ZZAC-PCB ARTC-PCB  XXDJ-PCB AUTF-PCB                    
115000         XXJD-PCB ORDP1-PCB ORDP2-PCB XXJN-PCB ORQA-PCB                   
115100         ORQM-PCB ARTM-PCB 4541-PCB  4487-PCB  XXKW-PCB XXLB-PCB          
115200         WDM2-PCB ORQI-PCB  ORQICSQ-PCB XXKH-PCB                          
115300         WDK7-PCB WLLOGA-PCB                                              
115400         WDK6-PCB  WDA6B-PCB WDA6-PCB WDB6-PCB WDB2-PCB                   
115500         WDP4A-PCB                                                        
115600         KOM-WDP8-PCB                                                     
115700         PRQU-WDG2-PCB                                                    
115800         PRQU-WDC7-PCB                                                    
115900         PRQU-SJKO-WDK6-PCB                                               
116000         PRNO-3107-PCB.                                                   
116100                                                                          
116200     PERFORM IMS-GET-MSG                                                  
116300     IF SEGMENT-FINNS                                                     
116310                                                                          
116400         PERFORM A-INIT                                                   
116500         IF WS-INDATA-RATT                                                
116600             MOVE +1                           TO RAD-INX                 
116700             IF MID-IDRADNR (RAD-INX)   =  ALL '+'                        
116800                 MOVE NEJ               TO  FL-RADER-FINNS                
116900             ELSE                                                         
117000                 MOVE MID-IDRADNR (RAD-INX) TO ARB-IDRADNR                
117100                 MOVE MID-KVORAPP (RAD-INX) TO ARB-KVLEVART               
117200                                               WS-KVORAPP-PACK            
117300                 MOVE JA                TO  FL-RADER-FINNS                
117400             END-IF                                                       
117500             MOVE WS-IDPRODNR           TO  W-411-IDPRODNR                
117600                                            W-601-IDPRODNR                
117700             PERFORM IMS-GHU-KOLLIREG                                     
117800             MOVE VORD-IDDC             TO WS-IDDC                        
117900                                           W-IDDC-B6                      
118000             PERFORM IMS-GU-WDB601                                        
118100             MOVE ZERO                  TO  INX-TOT-ANT-RADER             
118200                                                                          
118300             PERFORM UNTIL RADER-SLUT                                     
118400                OR  RAD-INX >= WS-RADER-PER-START-PLUS-1                  
118500                 MOVE ARB-IDRADNR       TO  W-411-IDPURAD                 
118600                 PERFORM IMS-GHU-RAD                                      
118700                 PERFORM B-UPPDATERA-FYS-AVVIKELSE                        
118800                 PERFORM D-BEHANDLA-RAD                                   
118900                 ADD +1                 TO  RAD-INX                       
119000                 IF MID-IDRADNR (RAD-INX) = ALL '+'                       
119100                     MOVE NEJ           TO  FL-RADER-FINNS                
119200                 ELSE                                                     
119300                     MOVE MID-IDRADNR (RAD-INX) TO ARB-IDRADNR            
119400                     MOVE MID-KVORAPP (RAD-INX) TO ARB-KVLEVART           
119500                                                  WS-KVORAPP-PACK         
119600                     MOVE JA            TO  FL-RADER-FINNS                
119700                 END-IF                                                   
119800             END-PERFORM                                                  
119900                                                                          
120000             PERFORM G-UPPDATERA-KOLLIREG                                 
120100             PERFORM I-UPPDATERA-WDE401-OCH-WDQ301                        
120200             PERFORM H-AVSLUT                                             
120300             PERFORM IMS-INSERT-ALT0605MSG                                
120400         END-IF                                                           
120500     END-IF                                                               
120600     MOVE ZERO TO RETURN-CODE                                             
120700     GOBACK.                                                              
120800     EJECT                                                                
120900 A-INIT             SECTION.                                              
121000                                                                          
121100     IF MSG-DUBBLA-TRANSKODER                                             
121200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I39801               
121300       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
121400       MOVE MSG-KDMFSFOR-2                TO   WS-KDMFSFOR                
121500     ELSE                                                                 
121600       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I39801               
121700       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
121800       MOVE MSG-KDMFSFOR-1                TO   WS-KDMFSFOR                
121900     END-IF                                                               
122000*                                                                         
122100     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
122200     ACCEPT WS-TID-W                      FROM TIME                       
122300     ACCEPT WS-VOR-TID-BRIST              FROM TIME                       
122400     PERFORM S10-HAMTA-MASKINDATUM                                        
122500     MOVE DAT-TIAAMMDD                    TO   DAGDAT-AAMMD               
122600     MOVE DAT-TIAAVVD                     TO   DAGDAT-AAVVD               
122700                                                                          
122800     MOVE LOW-VALUE                       TO   MSG-AREA                   
122900     MOVE '1'                             TO   ALT2191-SPRAK              
123000     MOVE ZERO                            TO   WS-TIDISPIN                
123100                                               W-IDORDER-Q2               
123200                                                                          
123500     MOVE NEJ                             TO WS-FLAUTFAK                  
123600                                             FL-LASNINGSTRANS             
123700                                             FL-RADER-FINNS               
123800                                             FL-PACKADEORDERL             
123810                                             SW-LYNK-NON-API              
123811                                             SW-VOR                       
123820                                                                          
123900     MOVE RAETT                           TO WS-INDATA-TEST               
124000                                             WS-BEHANDLING-TEST           
124100     SKIP2                                                                
124200     MOVE MID-IDPRODNR-UT               TO   WS-IDPRODNR                  
124300     IF WS-IDPRODNR NOT NUMERIC                                           
124400         MOVE FEL                       TO   WS-INDATA-TEST               
124500     END-IF                                                               
124600     MOVE MID-IDDISTR-UT                TO   WS-IDDISTR                   
124700                                             WS-IDDISTR-X4                
124800     IF WS-IDDISTR  NOT NUMERIC                                           
124900         MOVE FEL                       TO   WS-INDATA-TEST               
125000     END-IF                                                               
125100                                                                          
125200     MOVE MID-IDKUNDNR-UT               TO   WS-IDKUNDNR                  
125300     IF WS-IDKUNDNR NOT NUMERIC                                           
125400         MOVE FEL                       TO   WS-INDATA-TEST               
125500     END-IF                                                               
125600                                                                          
125700     MOVE MID-IDORDNR-UT                TO   WS-IDORDNR                   
125800     IF WS-IDORDNR  NOT NUMERIC                                           
125900         MOVE FEL                       TO   WS-INDATA-TEST               
126000     END-IF                                                               
126100     MOVE ZERO                          TO   LOGG-IDLOGLOP                
126200                                                                          
126210     MOVE SPACE                           TO EVENT-SW                     
126220*                                                                         
126230*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
126240*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
126250     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
126260     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
126270     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
126280     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
126290     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
126291     MOVE 'W4039800'                 TO MSG-KOM-IDSNDJOB                  
126292     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
126293*    -- THIS IS THE START VALUE                                           
126294     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
126300     .                                                                    
126400     EJECT                                                                
126500 B-UPPDATERA-FYS-AVVIKELSE       SECTION.                                 
126600                                                                          
126700******************************************************************        
126800*                                                                         
126900*  KOLLA OM TRANSFER (GER SVARET DB2-RADER-FINNS)                         
127000*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
127100*                                                                         
127200******************************************************************        
127300                                                                          
127400     MOVE WS-IDDISTR         TO W-TP4TRAN-IDDISTR                         
127500                                                                          
127600     PERFORM DB2-SELECT-TP4TRAN                                           
127700                                                                          
127800     MOVE WS-IDDISTR   TO TEST-IDDISTR                                    
127900                                                                          
128000     IF  NOT DIST18-SKROT                                                 
128100     AND NOT DIST18-SCRAP-NDC                                             
128200     AND NOT DIST19-SATS                                                  
128300     AND NOT DIST20-EMBALLAGE                                             
128400     AND NOT DIST20-EMBALLAGE-NDC                                         
128500     AND NOT DIST35-REFILL                                                
128600     AND NOT DIST35-REFILL-INOM-NDC                                       
128700     AND NOT DIST35-REFILL-NA-JAP                                         
128800     AND NOT DIST35-NONVCC-CDC-REFILL                                     
128900     AND NOT DIST35-NA-TRANSFER                                           
129000     AND NOT DIST35-NA-NDC-RETURNS                                        
129100     AND NOT DIST35-PACIFIC-TRANSFER                                      
129200     AND NOT DIST35-REFILL-INOM-JP                                        
129300     AND NOT DIST35-CN-TRANSFER                                           
129400     AND NOT DB2-RADER-FINNS                                              
129500     AND ORAD-KDRADSTA < 4                                                
129600                                                                          
129700       IF (ORAD-KDORDKL = 0 OR 1 OR 2 OR 3)    AND                        
129800          ORAD-KVLEVART NOT =  ORAD-KVAVBART                              
129900         PERFORM BD-GET-REGDAT                                            
130000         PERFORM BB-GEN-4320-BARN                                         
130100                                                                          
130200         IF     SEGMENT-SAKNAS                                            
130300           PERFORM BA-GEN-4319-ROT                                        
130400           PERFORM BB-GEN-4320-BARN                                       
130500                                                                          
130600         ELSE                                                             
130700           IF SEGMENT-FINNS-REDAN                                         
130800             PERFORM BC-REPL-4320-BARN                                    
130900           END-IF                                                         
131000         END-IF                                                           
131100         PERFORM BE-NILPICK-MAIL                                          
131200       END-IF                                                             
131300     END-IF                                                               
131400     .                                                                    
131500     EJECT                                                                
131600 BA-GEN-4319-ROT       SECTION.                                           
131700                                                                          
131800     MOVE '4319'              TO 4319-IDHTYP                              
131900                                 MSGI-IDTRANS                             
132000                                                                          
132100     MOVE LOW-VALUE           TO 4319-LOWVALUE                            
132200     IF DCS-NDC OR                                                        
132300       (DCS-SDC AND NOT DCS-SWEDEN)                                       
132400       MOVE MSGI-TILOKDAT     TO 4319-TIREGDAT                            
132500     ELSE                                                                 
132600       MOVE DAGDAT-AAMMD      TO 4319-TIREGDAT                            
132700     END-IF                                                               
132800                                                                          
132900     PERFORM IMS-4319-ISRT-ROT                                            
133000     .                                                                    
133100     SKIP3                                                                
133200 BB-GEN-4320-BARN      SECTION.                                           
133300                                                                          
133400     MOVE WS-IDDISTR                    TO 4320-IDDISTR                   
133500     MOVE WS-IDKUNDNR                   TO 4320-IDKUNDNR                  
133600     MOVE WS-IDORDNR                    TO 4320-IDORDNR                   
133700     MOVE WS-IDPRODNR                   TO 4320-IDPRODNR                  
133800     MOVE ORAD-IDARTNR                  TO 4320-IDARTNR                   
133900     MOVE ORAD-REKSIFFR                 TO 4320-REKSIFFR                  
134000     MOVE ORAD-BEART                    TO 4320-BEART                     
134100     MOVE ORAD-KVAVBART                 TO 4320-KVAVBART                  
134200     MOVE ORAD-KVLEVART                 TO 4320-KVLEVART                  
134300     MOVE ORAD-KDORDKL                  TO 4320-KDORDKL                   
134400                                                                          
134500     MOVE WS-OHUV-BEVARREF              TO 4320-BEVARREF                  
134600                                                                          
134700     IF DCS-NDC OR                                                        
134800       (DCS-SDC AND NOT DCS-SWEDEN)                                       
134900       MOVE MSGI-TILOKDAT               TO W-XXJD-TIREGDAT                
135000     ELSE                                                                 
135100       MOVE DAGDAT-AAMMD                TO W-XXJD-TIREGDAT                
135200     END-IF                                                               
135300     PERFORM IMS-4320-ISRT-BARN                                           
135400     .                                                                    
135500     EJECT                                                                
135600                                                                          
135700 BC-REPL-4320-BARN     SECTION.                                           
135800                                                                          
135900     MOVE DAGDAT-AAMMD                  TO W-XXJD-TIREGDAT                
136000                                                                          
136100     MOVE WS-IDDISTR                    TO W-XXJD-IDDISTR                 
136200     MOVE WS-IDKUNDNR                   TO W-XXJD-IDKUNDNR                
136300     MOVE WS-IDORDNR                    TO W-XXJD-IDORDNR                 
136400     MOVE WS-IDPRODNR                   TO W-XXJD-IDPRODNR                
136500     MOVE ORAD-IDARTNR                  TO W-XXJD-IDARTNR                 
136600     MOVE ORAD-REKSIFFR                 TO W-XXJD-REKSIFFR                
136700     PERFORM IMS-GHU-XXJD11                                               
136800                                                                          
136900     ADD ORAD-KVLEVART                  TO 4320-KVLEVART                  
137000     ADD ORAD-KVAVBART                  TO 4320-KVAVBART                  
137100     PERFORM IMS-REPL-XXJD11                                              
137200     .                                                                    
137300     EJECT                                                                
137400 BD-GET-REGDAT  SECTION.                                                  
137500                                                                          
137600     IF DCS-NDC OR                                                        
137700       (DCS-SDC AND NOT DCS-SWEDEN)                                       
137800       MOVE ALL '+'           TO MSGI-WMSGINIT                            
137900       MOVE '013'             TO MSGI-KDCALL                              
138000       MOVE WS-IDDC           TO IDDC-XX                                  
138100       MOVE IDDC-USER         TO MSGI-IDUSER                              
138200       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
138300                                                                          
138400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
138500       MOVE MSGI-TILOKDAT     TO 4319-TIREGDAT                            
138600     ELSE                                                                 
138700       MOVE DAGDAT-AAMMD      TO 4319-TIREGDAT                            
138800     END-IF                                                               
138900     .                                                                    
139000     SKIP2                                                                
139100 BE-NILPICK-MAIL   SECTION.                                               
139200                                                                          
139300     MOVE WS-IDDISTR         TO W-IDDISTR-WDB2                            
139400     MOVE WS-IDKUNDNR        TO W-IDKUNDNR-WDB2                           
139500                                                                          
139600     PERFORM IMS-GU-GMTA-WDB201                                           
139700     IF GMT-FLLDCKND = JA                                                 
139800       MOVE +1 TO INDX                                                    
139900       PERFORM UNTIL INDX > IX-DCCLEAR-MAX                                
140000         IF VORD-KDORDKL = 0                                              
140100           MOVE GMT-IDDC-VOR(INDX) TO W-GMT-IDDC-CLEAR(INDX)              
140200         END-IF                                                           
140300         IF VORD-KDORDKL = 1                                              
140400           MOVE GMT-IDDC-DAY(INDX) TO W-GMT-IDDC-CLEAR(INDX)              
140500         END-IF                                                           
140600         IF VORD-KDORDKL > 1                                              
140700           MOVE GMT-IDDC-BULK(INDX) TO W-GMT-IDDC-CLEAR(INDX)             
140800         END-IF                                                           
140900         ADD +1 TO INDX                                                   
141000       END-PERFORM                                                        
141100                                                                          
141200       MOVE 1 TO INDX                                                     
141210       PERFORM UNTIL INDX > 4 OR                                          
141220               W-GMT-IDDC-CLEAR (INDX) = WS-IDDC                          
141230         ADD 1 TO INDX                                                    
141240       END-PERFORM                                                        
141250                                                                          
141260       IF INDX < 4                                                        
141270         IF W-GMT-IDDC-CLEAR(INDX) NOT = WC-CDC-SE                        
141280           ADD 1 TO INDX                                                  
141290           IF W-GMT-IDDC-CLEAR(INDX) NOT = WC-CDC-SE                      
141291             MOVE WS-IDDISTR    TO NILP-IDDISTR                           
141292             MOVE WS-IDKUNDNR   TO NILP-IDKUNDNR                          
141293             MOVE WS-IDKUNDRF   TO NILP-IDKUNDRF                          
141294             MOVE WS-IDDC       TO NILP-IDDC                              
141295             MOVE WS-IDANSTNR   TO NILP-IDANSTNR                          
141296             MOVE ORAD-IDARTNR  TO NILP-IDARTNR                           
141297             MOVE ORAD-KVBEART  TO NILP-KVBEART                           
141298             MOVE ORAD-KVAVBART TO NILP-KVAVBART                          
141299             MOVE ORAD-KVANNANT TO NILP-KVANNANT                          
141300             MOVE ORAD-KVLEVART TO NILP-KVLEVART                          
141301                                                                          
141302             CALL W403NILP USING NILP-W403NILP DISTRDOC-PCB               
141303           END-IF                                                         
141304         END-IF                                                           
141305       END-IF                                                             
141306     END-IF                                                               
141310     .                                                                    
141400     SKIP2                                                                
141500 D-BEHANDLA-RAD        SECTION.                                           
141600                                                                          
141700     MOVE ORAD-WDE411                   TO SPAR-ORAD-WDE411               
141800                                                                          
141900     MOVE ZERO                          TO WS-KVSLATTAT                   
142000                                           SPAR-KART-KVRESS-ART           
142100                                                                          
142200     IF SPAR-ORAD-KDRADSTA = 4   AND                                      
142300        SPAR-ORAD-KVAVBART = 0   AND                                      
142400        SPAR-ORAD-KVANNANT > 0                                            
142500        MOVE 0                          TO WS-KVPRERO                     
142600     ELSE                                                                 
142700       IF DCS-CDC                                                         
142800         COMPUTE WS-KVPRERO = SPAR-ORAD-KVBEART  -                        
142900                              SPAR-ORAD-KVANNANT -                        
143000                              SPAR-ORAD-KVAVBART                          
143100         END-COMPUTE                                                      
143200       ELSE                                                               
143300         MOVE 0                          TO WS-KVPRERO                    
143400       END-IF                                                             
143500     END-IF                                                               
143600                                                                          
143700     ADD +1                             TO   INX-TOT-ANT-RADER            
143800     COMPUTE WS-KVORAPP-TOTAL = ORAD-KVBEART -                            
143900                                ORAD-KVANNANT -                           
144000                                ORAD-KVLEVART                             
144100     END-COMPUTE                                                          
144200     COMPUTE ORAD-KVAVBART = ORAD-KVAVBART - WS-KVORAPP-PACK              
144300     END-COMPUTE                                                          
144400                                                                          
144500     IF ORAD-KDRADSTA < 4                                                 
144600       MOVE JA                          TO  ORAD-FLFYSAVV                 
144700                                            SPAR-ORAD-FLFYSAVV            
144800       ADD  1                           TO  WS-ANT-RADER-FYSAVVIK         
144900     END-IF                                                               
145000                                                                          
145100     MOVE +4                            TO  ORAD-KDRADSTA                 
145200     PERFORM S01-UPPD-SPAR-UPPGIFTER                                      
145300     PERFORM IMS-REPL-RAD                                                 
145400                                                                          
145500     PERFORM DB-LAS-WDE4-OCH-ARTREG                                       
145600     IF KORD-KDORDKL = +0                                                 
145700        PERFORM DD-UPPDATERA-VOR-TIKLAR                                   
145800     END-IF                                                               
145900     PERFORM DI-BESTAM-ORDERBEKR-KOD                                      
146000     PERFORM DJ-UPPDATERA-EV-KAMP-REG                                     
146100     PERFORM DC-UPPDATERA-ORDERREG                                        
146200     PERFORM DF-UPDATE-ROREG                                              
146300     PERFORM DA-UPDATE-ARTREG                                             
146400     PERFORM S12-GENERERA-AVVIKELSE-TRANS                                 
146500     SKIP2                                                                
146600     .                                                                    
146700     EJECT                                                                
146800 DA-UPDATE-ARTREG  SECTION.                                               
146900                                                                          
147000     IF SPAR-ORAD-FLDIRLEV       =   NEJ     OR                           
147100        SPAR-ORAD-FLRESTN        =   JA                                   
147200       IF DCS-CDC OR DCS-CDC-TR                                           
147300         PERFORM DAA-UPDATE-PRERO-WDK9                                    
147400         PERFORM DAB-UPDATE-SALDO-CDC                                     
147500       ELSE                                                               
147600         IF DCS-SDC                                                       
147700           PERFORM DAC-UPPDAT-SALDO-SDC                                   
147800         ELSE                                                             
147900           IF DCS-NDC                                                     
148000             PERFORM DAF-UPDATE-SALDO-NDC                                 
148100           END-IF                                                         
148200         END-IF                                                           
148300       END-IF                                                             
148400                                                                          
148500     END-IF                                                               
148600     PERFORM DAD-EV-UPDATE-REFILL-SDC                                     
148700     .                                                                    
148800     EJECT                                                                
148900 DAA-UPDATE-PRERO-WDK9             SECTION.                               
149000                                                                          
149100     IF WS-FLLSBOK = JA                                                   
149200       IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                         
149300          MOVE SPAR-ORAD-IDARTNR              TO W-901-IDARTNR            
149400          PERFORM IMS-GHU-WDK901                                          
149500                                                                          
149600          PERFORM DAAB-UPDATE-PRERO                                       
149700                                                                          
149800          PERFORM IMS-REPL-WDK901                                         
149900       END-IF                                                             
150000     END-IF                                                               
150100     .                                                                    
150200     EJECT                                                                
150300 DAAB-UPDATE-PRERO                       SECTION.                         
150400                                                                          
150500     EVALUATE TRUE                                                        
150600       WHEN SPAR-ORAD-KDORDKL = 1                                         
150700         COMPUTE ART-KVPRERO-DAG =                                        
150800                 ART-KVPRERO-DAG -                                        
150900                 WS-KVPRERO                                               
151000         END-COMPUTE                                                      
151100                                                                          
151200       WHEN SPAR-ORAD-KDORDKL = 2 OR 3 OR 4                               
151300         COMPUTE ART-KVPRERO-BULK =                                       
151400                 ART-KVPRERO-BULK -                                       
151500                 WS-KVPRERO                                               
151600         END-COMPUTE                                                      
151700     END-EVALUATE                                                         
151800     .                                                                    
151900     EJECT                                                                
152000 DAB-UPDATE-SALDO-CDC                 SECTION.                            
152100                                                                          
152200     IF WS-FLLSBOK = JA                                                   
152300        MOVE SPAR-ORAD-IDARTNR        TO  W-IDARTNR                       
152400        PERFORM IMS-GHU-ARTC                                              
152500                                                                          
152600        IF SPAR-ORAD-FLDIRLEV    =   NEJ                                  
152700*SVS FROG                                                                 
152800           MOVE WS-IDDISTR     TO DIST20-IDDISTR                          
152900           IF DIST20-EMBALLAGE-SVS                                        
153000           OR SPAR-ORAD-ADLAGOMR = +016                                   
153100             COMPUTE CLAG-KVLS-SVS =                                      
153200                     CLAG-KVLS-SVS + WS-KVORAPP-PACK                      
153300             END-COMPUTE                                                  
153400           END-IF                                                         
153500                                                                          
153600           COMPUTE CLAG-KVLS     =   CLAG-KVLS + WS-KVORAPP-PACK          
153700           END-COMPUTE                                                    
153800                                                                          
153900           COMPUTE CLAG-KVEFRS   =   CLAG-KVEFRS - WS-KVORAPP-PACK        
154000           END-COMPUTE                                                    
154100                                                                          
154200***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
154300           PERFORM DABA-SKAPA-SALDOLOGG                                   
154400                                                                          
154500                                                                          
154600           IF SPAR-ORAD-IDKAMPRF   > 0          AND                       
154700              SPAR-KART-KVRESS-ART >= 0                                   
154800               COMPUTE CLAG-KVRESS = CLAG-KVRESS +                        
154900                                      WS-KVSLATTAT                        
155000           END-IF                                                         
155100        END-IF                                                            
155200                                                                          
155300        IF SPAR-ORAD-FLRESTN      =   JA           AND                    
155400           ( SPAR-ORAD-KVLEVART     < SPAR-ORAD-KVBEART                   
155500                                    - SPAR-ORAD-KVANNANT                  
155600                                    - SPAR-ORAD-KVSLATT )                 
155700           IF SPAR-ORAD-KDORDKL > 0                                       
155800              PERFORM S14-EV-LARM-2191-MID                                
155900                                                                          
156000              IF DCS-CDC OR DCS-CDC-TR                                    
156100                COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-TOTAL        
156200                END-COMPUTE                                               
156300              ELSE                                                        
156400                COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK         
156500                END-COMPUTE                                               
156600              END-IF                                                      
156700           END-IF                                                         
156800           IF CLAG-KVROS          =   WS-KVORAPP-TOTAL                    
156900              MOVE DAGDAT-AAVVD TO  CLAG-TIRODAT                          
157000           END-IF                                                         
157100        END-IF                                                            
157200        PERFORM IMS-REPL-ARTC                                             
157300     END-IF                                                               
157400     .                                                                    
157500     EJECT                                                                
157600 DABA-SKAPA-SALDOLOGG SECTION.                                            
157700                                                                          
157800     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
157900                                                                          
158000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
158100     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
158200                                   - WS-AAAAMMDD                          
158300     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
158400     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
158500                                   - WS-TTMMSSTH                          
158600     MOVE 9                        TO LOGG-IDSEKVNR                       
158700     MOVE WC-CDC-SE                TO LOGG-IDDC                           
158800     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
158900     MOVE 'PAC'                    TO LOGG-IDSUBTYP                       
159000     MOVE 'W4039800'               TO LOGG-IDPGM                          
159100     MOVE WS-IDTRANS                TO LOGG-IDTRANS                       
159200     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
159300     MOVE SPACE                    TO LOGG-REF                            
159400     MOVE WS-IDDISTR               TO LOGG-IDDISTR                        
159500     MOVE WS-IDKUNDNR              TO LOGG-IDKUNDNR                       
159600     MOVE WS-IDKUNDRF              TO LOGG-IDKUNDRF                       
159700     MOVE WS-IDPRODNR              TO LOGG-IDPRODNR                       
159800                                                                          
159900     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
160000     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
160100     MOVE '-'                      TO LOGG-IDTECKEN-KVEFRS                
160200     MOVE '+'                      TO LOGG-IDTECKEN-KVLS                  
160300     MOVE WS-KVORAPP-PACK          TO LOGG-KVART-SALDO                    
160400                                                                          
160500     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
160600                        + CLAG-KVAKS-T                                    
160700                                                                          
160800     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
160900     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
161000     MOVE CLAG-KVLS                TO LOGG-KVLS                           
161100     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
161200                                                                          
161300     PERFORM IMS-ISRT-WDL901                                              
161400     IF SEGMENT-FINNS-REDAN                                               
161500       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
161600         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
161700         PERFORM IMS-ISRT-WDL901                                          
161800       END-PERFORM                                                        
161900     END-IF                                                               
162000     .                                                                    
162100     EJECT                                                                
162200 DAC-UPPDAT-SALDO-SDC      SECTION.                                       
162300                                                                          
162400     IF WS-FLLSBOK = JA                                                   
162500        IF SPAR-ORAD-FLDIRLEV = NEJ                                       
162600                                                                          
162700           MOVE WS-IDDC                  TO W-711-IDDC                    
162800           MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                     
162900*          PERFORM IMS-GU-WDK722                                          
163000           PERFORM IMS-GHU-WDK711                                         
163100                                                                          
163200           COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK          
163300           END-COMPUTE                                                    
163400           COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK          
163500           END-COMPUTE                                                    
163600                                                                          
163700           IF NOT DCS-CHINA                                               
163800              PERFORM IMS-REPL-WDK7                                       
163900***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
164000              PERFORM S15-SKAPA-SALDOLOGG                                 
164100              PERFORM DACA-UPDATE-CLAG-KVROS                              
164200           ELSE                                                           
164300              PERFORM DACB-UPDATE-SLAG-KVROS                              
164400              PERFORM IMS-REPL-WDK7                                       
164500              PERFORM S15-SKAPA-SALDOLOGG                                 
164600           END-IF                                                         
164700        END-IF                                                            
164800     END-IF                                                               
164900     .                                                                    
165000     EJECT                                                                
165100 DAF-UPDATE-SALDO-NDC      SECTION.                                       
165200                                                                          
165300     IF  WS-FLLSBOK = JA                                                  
165400     AND SPAR-ORAD-FLDIRLEV = NEJ                                         
165500        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
165600        MOVE WS-IDDC                  TO W-711-IDDC                       
165700        PERFORM IMS-GHU-WDK711                                            
165800                                                                          
165900        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
166000        END-COMPUTE                                                       
166100        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
166200        END-COMPUTE                                                       
166300                                                                          
166400**** SKALL LOGGA SALDOFÖRÄNDRING I DATABAS WDL9. ********                 
166500        PERFORM S15-SKAPA-SALDOLOGG                                       
166600                                                                          
166700        IF  SPAR-ORAD-FLRESTN = JA                                        
166800        AND SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                        
166900                               - SPAR-ORAD-KVANNANT                       
167000                               - SPAR-ORAD-KVSLATT                        
167100        AND SPAR-ORAD-KDORDKL > 0                                         
167200          IF  SPAR-ORAD-IDDC-RO = WS-IDDC                                 
167300            IF  DCS-NDC-CN                                                
167400            OR (DCS-NDC-NA AND DCS-USA)                                   
167500              IF SLAG-IDDC-REF = SPACE                                    
167600                PERFORM S14-EV-LARM-2191-MID-CN-US                        
167700              END-IF                                                      
167800            END-IF                                                        
167900            IF SPAR-ORAD-KDORDKL > 1                                      
168000              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
168100                                      + WS-KVORAPP-TOTAL                  
168200              END-COMPUTE                                                 
168300            ELSE                                                          
168400              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
168500                                      + WS-KVORAPP-TOTAL                  
168600              END-COMPUTE                                                 
168700            END-IF                                                        
168800            PERFORM IMS-REPL-WDK7                                         
168900          ELSE                                                            
169000            PERFORM IMS-REPL-WDK7                                         
169100            MOVE SPAR-ORAD-IDDC-RO        TO W-711-IDDC                   
169200            PERFORM IMS-GHU-WDK711                                        
169300            IF  DCS-NDC-CN                                                
169400            OR (DCS-NDC-NA AND DCS-USA)                                   
169500              IF SLAG-IDDC-REF = SPACE                                    
169600                PERFORM S14-EV-LARM-2191-MID-CN-US                        
169700              END-IF                                                      
169800            END-IF                                                        
169900            IF SPAR-ORAD-KDORDKL > 1                                      
170000              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
170100                                      + WS-KVORAPP-TOTAL                  
170200              END-COMPUTE                                                 
170300            ELSE                                                          
170400              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
170500                                      + WS-KVORAPP-TOTAL                  
170600              END-COMPUTE                                                 
170700            END-IF                                                        
170800            PERFORM IMS-REPL-WDK7                                         
170900            MOVE WS-IDDC                  TO W-711-IDDC                   
171000          END-IF                                                          
171100        ELSE                                                              
171200          PERFORM IMS-REPL-WDK7                                           
171300        END-IF                                                            
171400     END-IF                                                               
171500     .                                                                    
171600     EJECT                                                                
171700 DACA-UPDATE-CLAG-KVROS    SECTION.                                       
171800                                                                          
171900     IF SPAR-ORAD-KDORDKL > 0  AND                                        
172000        WS-KVORAPP-TOTAL > ZERO                                           
172100                                                                          
172200       IF SPAR-ORAD-FLRESTN  = JA           AND                           
172300         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
172400                             - SPAR-ORAD-KVANNANT                         
172500                             - SPAR-ORAD-KVSLATT)                         
172600          PERFORM IMS-GHU-ARTC                                            
172700          IF SPAR-ORAD-FLFYSAVV = JA                                      
172800                                                                          
172900             PERFORM S14-EV-LARM-2191-MID                                 
173000             COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK            
173100             END-COMPUTE                                                  
173200          END-IF                                                          
173300                                                                          
173400          IF CLAG-KVROS = WS-KVORAPP-TOTAL                                
173500             MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                            
173600          END-IF                                                          
173700          PERFORM IMS-REPL-ARTC                                           
173800       END-IF                                                             
173900     END-IF                                                               
174000     .                                                                    
174100     EJECT                                                                
174200 DACB-UPDATE-SLAG-KVROS    SECTION.                                       
174300                                                                          
174400     IF SPAR-ORAD-KDORDKL > 0  AND                                        
174500        WS-KVORAPP-TOTAL > ZERO                                           
174600                                                                          
174700       IF SPAR-ORAD-FLRESTN  = JA           AND                           
174800         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
174900                             - SPAR-ORAD-KVANNANT                         
175000                             - SPAR-ORAD-KVSLATT)                         
175100                                                                          
175200          IF  SPAR-ORAD-IDDC-RO = WS-IDDC                                 
175300            IF  DCS-NDC AND DCS-CHINA                                     
175400            AND SLAG-IDDC-REF = SPACE                                     
175500               PERFORM S14-EV-LARM-2191-MID-CN-US                         
175600            END-IF                                                        
175700            IF SPAR-ORAD-KDORDKL > 1                                      
175800              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
175900                                      + WS-KVORAPP-PACK                   
176000            ELSE                                                          
176100              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
176200                                      + WS-KVORAPP-PACK                   
176300            END-IF                                                        
176400          ELSE                                                            
176500            PERFORM IMS-REPL-WDK7                                         
176600            MOVE SPAR-ORAD-IDDC-RO   TO W-711-IDDC                        
176700            PERFORM IMS-GHU-WDK711                                        
176800            IF  DCS-NDC AND DCS-CHINA                                     
176900            AND SLAG-IDDC-REF = SPACE                                     
177000               PERFORM S14-EV-LARM-2191-MID-CN-US                         
177100            END-IF                                                        
177200            IF SPAR-ORAD-KDORDKL > 1                                      
177300              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
177400                                      + WS-KVORAPP-PACK                   
177500            ELSE                                                          
177600              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
177700                                      + WS-KVORAPP-PACK                   
177800              END-COMPUTE                                                 
177900            END-IF                                                        
178000            MOVE WS-IDDC             TO W-711-IDDC                        
178100          END-IF                                                          
178200                                                                          
178300       END-IF                                                             
178400     END-IF                                                               
178500     .                                                                    
178600     EJECT                                                                
178700 DAD-EV-UPDATE-REFILL-SDC  SECTION.                                       
178800*REFILLORDER                                                              
178900                                                                          
179000     IF WS-FLLSBOK = JA                                                   
179100                                                                          
179200******************************************************************        
179300*                                                                         
179400*  KOLLA OM TRANSFER (GER SVARET DB2-RADER-FINNS)                         
179500*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
179600*                                                                         
179700******************************************************************        
179800                                                                          
179900       MOVE KORD-IDDISTR       TO W-TP4TRAN-IDDISTR                       
180000                                                                          
180100       PERFORM DB2-SELECT-TP4TRAN                                         
180200                                                                          
180300       MOVE KORD-IDDISTR            TO TEST-IDDISTR                       
180400                                                                          
180500*NDC&SDC OCH INTE RESTNOTERING.                                           
180600       IF (DIST35-REFILL                                                  
180700        OR DIST35-REFILL-INOM-NDC                                         
180800        OR DIST35-REFILL-NA-JAP                                           
180900        OR DIST35-NA-TRANSFER                                             
181000        OR DIST35-NA-NDC-RETURNS                                          
181100        OR DIST35-PACIFIC-TRANSFER                                        
181200        OR DIST35-REFILL-INOM-JP                                          
181300        OR DIST35-CN-TRANSFER                                             
181400        OR DB2-RADER-FINNS)                                               
181500       AND SPAR-ORAD-FLRESTN = NEJ                                        
181600       AND WS-KVORAPP-TOTAL > ZERO                                        
181700                                                                          
181800         IF DB2-RADER-FINNS                                               
181900           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
182000         ELSE                                                             
182100           PERFORM DADA-GET-SDC-IDDC-VALUE                                
182200         END-IF                                                           
182300                                                                          
182400         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
182500         PERFORM IMS-GHU-WDK711                                           
182600         SUBTRACT WS-KVORAPP-TOTAL  FROM SLAG-KVBEART                     
182700         PERFORM IMS-REPL-WDK7                                            
182800       ELSE                                                               
182900         IF  DIST35-NONVCC-CDC-REFILL                                     
183000         AND SPAR-ORAD-FLRESTN = NEJ                                      
183100         AND WS-KVORAPP-TOTAL > ZERO                                      
183200                                                                          
183300           PERFORM DADA-GET-SDC-IDDC-VALUE                                
183400                                                                          
183500           MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                        
183600           PERFORM IMS-GHU-WDK611                                         
183700           SUBTRACT WS-KVORAPP-TOTAL  FROM CLAG-KVBEART                   
183800           PERFORM IMS-REPL-WDK611                                        
183900         END-IF                                                           
184000       END-IF                                                             
184100                                                                          
184200*NDC&SDC OCH RESTNOTERING (SLATTGRÄNSEN ÖVERSKRIDEN).                     
184300       IF (DIST35-REFILL                                                  
184400        OR DIST35-REFILL-INOM-NDC                                         
184500        OR DIST35-REFILL-NA-JAP                                           
184600        OR DIST35-NA-TRANSFER                                             
184700        OR DIST35-NA-NDC-RETURNS                                          
184800        OR DIST35-PACIFIC-TRANSFER                                        
184900        OR DIST35-REFILL-INOM-JP                                          
185000        OR DIST35-CN-TRANSFER                                             
185100        OR DB2-RADER-FINNS)                                               
185200       AND SPAR-ORAD-FLRESTN = JA                                         
185300       AND WS-KVORAPP-TOTAL > ZERO                                        
185400       AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                      
185500                                 SPAR-ORAD-KVANNANT -                     
185600                                 SPAR-ORAD-KVSLATT                        
185700                                                                          
185800         IF DB2-RADER-FINNS                                               
185900           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
186000         ELSE                                                             
186100           PERFORM DADA-GET-SDC-IDDC-VALUE                                
186200         END-IF                                                           
186300                                                                          
186400         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
186500         PERFORM IMS-GHU-WDK711                                           
186600         SUBTRACT WS-KVORAPP-TOTAL FROM SLAG-KVBEART                      
186700         PERFORM IMS-REPL-WDK7                                            
186800       ELSE                                                               
186900         IF  DIST35-NONVCC-CDC-REFILL                                     
187000         AND SPAR-ORAD-FLRESTN = JA                                       
187100         AND WS-KVORAPP-TOTAL > ZERO                                      
187200         AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                    
187300                                   SPAR-ORAD-KVANNANT -                   
187400                                   SPAR-ORAD-KVSLATT                      
187500                                                                          
187600           PERFORM DADA-GET-SDC-IDDC-VALUE                                
187700                                                                          
187800           MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                        
187900           PERFORM IMS-GHU-WDK611                                         
188000           SUBTRACT WS-KVORAPP-TOTAL FROM CLAG-KVBEART                    
188100           PERFORM IMS-REPL-WDK611                                        
188200         END-IF                                                           
188300       END-IF                                                             
188400     END-IF                                                               
188500     SKIP2                                                                
188600     .                                                                    
188700 DADA-GET-SDC-IDDC-VALUE            SECTION.                              
188800                                                                          
188900     SEARCH ALL DIST57-REFILL-DC                                          
189000        AT END                                                            
189100           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
189200                            TO FELTEXT                                    
189300           CALL FELLOG                                                    
189400        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                 
189500           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-711-IDDC              
189600     END-SEARCH                                                           
189700     .                                                                    
189800     EJECT                                                                
189900 DB-LAS-WDE4-OCH-ARTREG                  SECTION.                         
190000                                                                          
190100     PERFORM IMS-GHNP-WDE401-MED-PCB-WDE42                                
190200                                                                          
190300     MOVE KORD-IDDISTR                   TO W-401-IDDISTR                 
190400     MOVE KORD-IDKUNDNR                  TO W-401-IDKUNDNR                
190500     MOVE KORD-IDORDNR5                  TO W-401-IDORDNR                 
190600     MOVE KORD-IDPRODNR                  TO W-401-IDPRODNR                
190700     MOVE KORD-IDPLKLST                  TO W-401-IDPLKLST                
190800                                                                          
190900     MOVE KORD-IDORDER                   TO W-IDORDER-Q2                  
191000     PERFORM IMS-GU-WDQ201                                                
191100     MOVE    WS-IDDC                     TO W-IDDC-Q2                     
191200     PERFORM IMS-GHNP-WDQ212                                              
191300                                                                          
191400     MOVE OHUV-BEKUNDRF                  TO WS-OHUV-BEKUNDRF              
191500     MOVE OHUV-BEVARREF                  TO WS-OHUV-BEVARREF              
191600     MOVE OHUV-BEKUNDRF                  TO SPAR-BEKUNDRF                 
191601     MOVE OHUV-IDSYSTEM                  TO WS-IDSYSTEM                   
191700*                                                                         
191800     MOVE SPAR-ORAD-IDARTNR TO W-IDARTNR                                  
191900     PERFORM IMS-GU-ARTC11                                                
192000     MOVE DCS-IDDC          TO W-711-IDDC                                 
192100*                                                                         
192200     MOVE CLAG-IDANSK       TO WS-IDANSK                                  
192300     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                             
192400       PERFORM IMS-GU-WDK722                                              
192500       IF SEGMENT-FINNS AND XLAG-IDANSK > 0                               
192600          MOVE XLAG-IDANSK  TO WS-IDANSK                                  
192700       END-IF                                                             
192800     END-IF                                                               
192900     .                                                                    
193000     EJECT                                                                
193230 DC-UPPDATERA-ORDERREG     SECTION.                                       
193240                                                                          
193300     MOVE KORD-KDFAKTYP             TO  WS-KDFAKTYP                       
193400     MOVE KORD-KDORDKL              TO  WS-KDORDKL                        
193500     MOVE KORD-FLLSBOK              TO  WS-FLLSBOK                        
193600     MOVE KORD-FLORDSPE             TO  WS-FLORDSPE                       
193700     MOVE KORD-FLOVRLEV             TO  WS-FLOVRLEV                       
193800     COMPUTE KORD-VKORDNTO = KORD-VKORDNTO - SPAR-VKORDNTO                
193900     END-COMPUTE                                                          
194000     COMPUTE KORD-VLORDNTO = KORD-VLORDNTO - SPAR-VLORDNTO                
194100     END-COMPUTE                                                          
194200                                                                          
194300     IF SPAR-ORAD-FLDIRLEV = JA                                           
194400        COMPUTE KORD-SUORDV-LEVPL-LOC = KORD-SUORDV-LEVPL-LOC             
194500                                         - SPAR-SUORDV-LOC                
194600        COMPUTE KORD-SUORDV-LEVPL-LOCPREL =                               
194700               KORD-SUORDV-LEVPL-LOCPREL - SPAR-SUORDV-LOCPREL            
194800        COMPUTE KORD-SUORDV-LEVPL = KORD-SUORDV-LEVPL                     
194900                                         - SPAR-SUORDV                    
195000     ELSE                                                                 
195100*      MOVE KORD-KDFRAKT    TO WS-KDFRAKT                                 
195200       COMPUTE KORD-SUORDV-LOC = KORD-SUORDV-LOC                          
195300                                   - SPAR-SUORDV-LOC                      
195400       COMPUTE KORD-SUORDV-LOCPREL = KORD-SUORDV-LOCPREL                  
195500                                   - SPAR-SUORDV-LOCPREL                  
195600       COMPUTE KORD-SUORDV = KORD-SUORDV                                  
195700                                   - SPAR-SUORDV                          
195800     END-IF                                                               
195900                                                                          
196000     PERFORM IMS-REPL-WDE401-MED-PCB-WDE42                                
196100     .                                                                    
196200     EJECT                                                                
196300 DD-UPPDATERA-VOR-TIKLAR SECTION.                                         
196400                                                                          
196500     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
196600     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
196700     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
196800                                    W-A601KY-MAX-IDDISTR                  
196900     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
197000                                    W-A601KY-MAX-IDKUNDNR                 
197100     MOVE SPACE                  TO W-A601KY-MIN-IDKUNDRF                 
197200                                    W-A601KY-MAX-IDKUNDRF                 
197300     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
197400                                    W-A601KY-MAX-IDORDNR                  
197500                                                                          
197600     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
197700                                                                          
197800     PERFORM IMS-GHN-SEQB-WDA601                                          
197900     PERFORM UNTIL SEGMENT-SAKNAS                                         
198000                OR SEGMENT-SLUT                                           
198100                OR SW-TIKLAR-UPPDATERAD = JA                              
198200                                                                          
198300         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
198400         AND VOR-TIKLAR = +0                                              
198500                                                                          
198600             MOVE WS-DAGENS-DATUM   TO VOR-TIKLAR                         
198700             MOVE WS-TTMMSS         TO VOR-TIKLATID                       
198800             PERFORM IMS-REPL-SEQB-WDA601                                 
198900             MOVE JA                TO SW-TIKLAR-UPPDATERAD               
199000         END-IF                                                           
199100                                                                          
199200         PERFORM IMS-GHN-SEQB-WDA601                                      
199300     END-PERFORM                                                          
199400     .                                                                    
199500 DF-UPDATE-ROREG        SECTION.                                          
199600     SKIP3                                                                
199700     PERFORM DFE-HAMTA-TPOTYP-FRAN-ROREG                                  
199800                                                                          
199900     IF SPAR-ORAD-FLRESTN   = JA AND                                      
200000        ( SPAR-ORAD-KVLEVART     < SPAR-ORAD-KVBEART                      
200100                                 - SPAR-ORAD-KVANNANT                     
200200                                 - SPAR-ORAD-KVSLATT )                    
200300                                                                          
200400        IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                           
200500           PERFORM DFD-SAMMANSL-EJ-BIPACKAD-RAD                           
200600           IF WS-SAMMANSLAGNING-RAD = JA                                  
200700              IF DCS-CDC OR DCS-NDC                                       
200800                ADD WS-KVORAPP-TOTAL  TO OLD-RAD-KVART                    
200900              ELSE                                                        
201000                ADD WS-KVORAPP-PACK   TO OLD-RAD-KVART                    
201100              END-IF                                                      
201200              PERFORM IMS-REPL-ORDP01-OLD                                 
201300           ELSE                                                           
201400              PERFORM DFA-KATEGORI                                        
201500              PERFORM DFB-FLYTTA-WDA5-POSTER                              
201600              IF RAD-KVART > ZERO                                         
201700                PERFORM IMS-ISRT-ORDP01                                   
201800                PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                     
201900                   ADD +1               TO RAD-IDLOPNR                    
202000                   PERFORM IMS-ISRT-ORDP01                                
202100                END-PERFORM                                               
202200              END-IF                                                      
202300           END-IF                                                         
202400        ELSE                                                              
202500           MOVE KORD-IDDISTR          TO W1-IDDISTR                       
202600           MOVE KORD-IDKUNDNR         TO W1-IDKUNDNR                      
202700           MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF                      
202800           MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                       
202900           MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                       
203000                                                                          
203100           PERFORM IMS-GHU-ORDP01-GE                                      
203200           IF SEGMENT-SAKNAS                                              
203300             PERFORM DFA-KATEGORI                                         
203400*FIX   START* FÖR PROBLEM MED ENHETSLASTER**********************          
203500***********   NÄR PGM ÅKER PÅ GE MOT WDA5 (ORDP01) *************          
203600                                                                          
203700             MOVE W1-IDDISTR         TO RAD-IDDISTR                       
203800             MOVE W1-IDKUNDNR        TO RAD-IDKUNDNR                      
203900             MOVE W1-IDKUNDRF        TO RAD-IDKUNDRF                      
204000             MOVE W1-IDARTNR         TO RAD-IDARTNR                       
204100             MOVE W1-IDLOPNR         TO RAD-IDLOPNR                       
204200                                                                          
204300             MOVE SPAR-ORAD-BERADREF TO RAD-BERADREF                      
204400             MOVE 'N'                TO RAD-FLERS                         
204500             MOVE WS-IDANSK          TO RAD-IDANSK                        
204600             MOVE SPAR-ORAD-IDANALYS TO RAD-IDANALYS                      
204700             MOVE SPAR-ORAD-IDKONTO  TO RAD-IDKONTO                       
204800             MOVE SPAR-ORAD-IDKST    TO RAD-IDKST                         
204900             MOVE '00000     '       TO RAD-IDKUNDRF-LEV                  
205000             MOVE WS-IDDC            TO RAD-IDDC                          
205100             MOVE SPAR-ORAD-IDDC-RO  TO RAD-IDDC-RO                       
205200             MOVE SPAR-ORAD-KDDSP    TO RAD-KDDSP                         
205300             MOVE KORD-KDFAKTYP      TO RAD-KDFAKTYP                      
205400             MOVE SPAR-ORAD-KDFRAKT  TO RAD-KDFRAKT                       
205500             MOVE SPAR-ORAD-KDKVBRYT TO RAD-KDKVBRYT                      
205600             MOVE SPAR-ORAD-KDOI     TO RAD-KDOI                          
205700             MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP                  
205800             MOVE SPAR-ORAD-KDORDING TO RAD-KDORDING                      
205900             MOVE KORD-KDORDKL       TO RAD-KDORDKL                       
206000             MOVE SPAR-ORAD-KDPRODSL TO RAD-KDPRODSL                      
206100             MOVE WS-KDRAPRIO        TO RAD-KDRAPRIO                      
206200             MOVE +4                 TO RAD-KDROO                         
206300             MOVE '4'                TO RAD-KDSTARAD                      
206400             MOVE WS-KDTPOTYP        TO RAD-KDTPOTYP                      
206500             MOVE SPAR-ORAD-KDVRINFO TO RAD-KDVRINFO                      
206600             IF DCS-CDC OR DCS-NDC                                        
206700               MOVE WS-KVORAPP-TOTAL TO RAD-KVART                         
206800                                              RAD-KVRO                    
206900             ELSE                                                         
207000               MOVE WS-KVORAPP-PACK  TO RAD-KVART                         
207100                                              RAD-KVRO                    
207200             END-IF                                                       
207300             MOVE SPAR-ORAD-PRARTNTO TO RAD-PRARTNTO                      
207400             MOVE SPAR-ORAD-PRARTNTO-LOC                                  
207500                                        TO RAD-PRARTNTO-LOC               
207600             MOVE SPAR-ORAD-PRARTNTO-LOCPREL                              
207700                                        TO RAD-PRARTNTO-LOCPREL           
207800             MOVE SPAR-ORAD-REKSIFFR TO RAD-REKSIFFR                      
207900             MOVE ZERO               TO RAD-TIAVBOKN                      
208000             MOVE DAT-TIAAMMDD       TO RAD-DARODAT                       
208100                                              RAD-TIREGDAT                
208200             MOVE DAT-TISEKEL        TO RAD-DARODAT (1:2)                 
208300             MOVE ZERO               TO RAD-TIRES                         
208400             MOVE +0                 TO RAD-TITPO                         
208500             MOVE SPAR-ORAD-KDPRTYP  TO RAD-KDPRTYP                       
208600             MOVE SPAR-ORAD-BEVOLREF TO RAD-BEVOLREF                      
208700             MOVE SPAR-ORAD-FLINVEST TO RAD-FLINVEST                      
208800             MOVE SPAR-ORAD-FLPRTILL TO RAD-FLPRTILL                      
208900             MOVE JA                 TO RAD-FLTPOBEK                      
209000             MOVE SPAR-BEKUNDRF      TO RAD-BEKUNDRF                      
209100             MOVE SPAR-ORAD-IDKAMPRF TO RAD-IDKAMPRF                      
209200             MOVE SPAR-ORAD-IDLEVNR  TO RAD-IDLEVNR                       
209300             MOVE SPAR-ORAD-IDSYSTEM TO RAD-IDSYSTEM                      
209400             MOVE SPAR-ORAD-KVBEART  TO RAD-KVBEART-Q                     
209500             MOVE WS-TTMMSS          TO RAD-TIREGTID                      
209600             MOVE 0                  TO RAD-DASENDAT                      
209700             MOVE 0                  TO RAD-TISENBEK-KL                   
209800                                                                          
209900             MOVE OHUV-KDORDTYP-LDC  TO RAD-KDORDTYP-LDC                  
210000             MOVE OHUV-TIREPDAT      TO RAD-TIREPDAT                      
210100             MOVE SPAR-ORAD-IDKUNDRF-WIP                                  
210200                                     TO RAD-IDKUNDRF-WIP                  
210300             MOVE SPAR-ORAD-PRAVCOST TO RAD-PRAVCOST                      
210400             MOVE ARB-KDROPACK       TO RAD-KDROPACK                      
210500             MOVE SPAR-ORAD-IDARBREF TO RAD-IDARBREF                      
210600             PERFORM S36-ANDRA-WDC711                                     
210700                                                                          
210800             PERFORM IMS-ISRT-ORDP01                                      
210900             IF RAD-KVART > ZERO                                          
211000               PERFORM IMS-ISRT-ORDP01                                    
211100               PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                      
211200                  ADD +1                TO RAD-IDLOPNR                    
211300                  PERFORM IMS-ISRT-ORDP01                                 
211400               END-PERFORM                                                
211500             END-IF                                                       
211600           END-IF                                                         
211700                                                                          
211800           PERFORM IMS-GHU-ORDP01                                         
211900                                                                          
212000           IF RAD-DARODAT = ZERO                                          
212100             MOVE JA TO SW-TIRODAT-LIKA-MED-ZERO                          
212200           END-IF                                                         
212300                                                                          
212400           PERFORM DFC-SAMMANSL-BIPACKAD-RAD                              
212500                                                                          
212600           IF WS-SAMMANSLAGNING-RAD = JA                                  
212700              IF DCS-CDC OR DCS-NDC                                       
212800                IF WS-KVORAPP-TOTAL = RAD-KVART                           
212900                  ADD WS-KVORAPP-TOTAL     TO OLD-RAD-KVART               
213000                  PERFORM IMS-REPL-ORDP01-OLD                             
213100                                                                          
213200                  MOVE KORD-IDDISTR          TO W1-IDDISTR                
213300                  MOVE KORD-IDKUNDNR         TO W1-IDKUNDNR               
213400                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
213500                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
213600                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
213700                  PERFORM IMS-GHU-ORDP01                                  
213800                  PERFORM IMS-DLET-ORDP01                                 
213900                ELSE                                                      
214000                  SUBTRACT WS-KVORAPP-TOTAL  FROM RAD-KVART               
214100                  PERFORM IMS-REPL-ORDP01                                 
214200                                                                          
214300                  ADD WS-KVORAPP-TOTAL       TO OLD-RAD-KVART             
214400                  PERFORM IMS-REPL-ORDP01-OLD                             
214500                END-IF                                                    
214600              ELSE                                                        
214700                IF WS-KVORAPP-PACK = RAD-KVART                            
214800                  ADD WS-KVORAPP-PACK    TO OLD-RAD-KVART                 
214900                  PERFORM IMS-REPL-ORDP01-OLD                             
215000                                                                          
215100                  MOVE KORD-IDDISTR          TO W1-IDDISTR                
215200                  MOVE KORD-IDKUNDNR         TO W1-IDKUNDNR               
215300                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
215400                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
215500                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
215600                  PERFORM IMS-GHU-ORDP01                                  
215700                  PERFORM IMS-DLET-ORDP01                                 
215800                ELSE                                                      
215900                  SUBTRACT WS-KVORAPP-PACK   FROM RAD-KVART               
216000                  PERFORM IMS-REPL-ORDP01                                 
216100                                                                          
216200                  ADD WS-KVORAPP-PACK        TO OLD-RAD-KVART             
216300                  PERFORM IMS-REPL-ORDP01-OLD                             
216400                END-IF                                                    
216500              END-IF                                                      
216600           ELSE                                                           
216700             IF DCS-CDC OR DCS-NDC                                        
216800              IF WS-KVORAPP-TOTAL = RAD-KVART                             
216900                 IF RAD-DARODAT = 0                                       
217000                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
217100                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
217200                 END-IF                                                   
217300                                                                          
217400                 MOVE +0                TO RAD-TIRES                      
217500                 MOVE +0                TO RAD-TIAVBOKN                   
217600                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
217700                 MOVE SPAR-ORAD-CLEARGROUP                                
217800                                        TO RAD-CLEARGROUP                 
217900                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
218000                                           RAD-IDDC-RO                    
218100                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
218200                 MOVE '2'               TO RAD-KDSTARAD                   
218300                 PERFORM S36-ANDRA-WDC711                                 
218400                 PERFORM IMS-REPL-ORDP01                                  
218500              ELSE                                                        
218600                 SUBTRACT WS-KVORAPP-TOTAL FROM RAD-KVART                 
218700                 PERFORM IMS-REPL-ORDP01                                  
218800                                                                          
218900                 IF RAD-DARODAT = 0                                       
219000                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
219100                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
219200                 END-IF                                                   
219300                                                                          
219400                 MOVE +0                TO RAD-TIRES                      
219500                 MOVE +0                TO RAD-TIAVBOKN                   
219600                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
219700                 MOVE SPAR-ORAD-CLEARGROUP                                
219800                                        TO RAD-CLEARGROUP                 
219900                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
220000                                           RAD-IDDC-RO                    
220100                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
220200                 MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                      
220300                 MOVE '2'               TO RAD-KDSTARAD                   
220400                 ADD +1                 TO RAD-IDLOPNR                    
220500                 PERFORM S36-ANDRA-WDC711                                 
220600                 PERFORM IMS-ISRT-ORDP01                                  
220700                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
220800                   ADD +1             TO RAD-IDLOPNR                      
220900                   PERFORM IMS-ISRT-ORDP01                                
221000                 END-PERFORM                                              
221100              END-IF                                                      
221200             ELSE                                                         
221300              IF WS-KVORAPP-PACK = RAD-KVART                              
221400                 IF RAD-DARODAT = 0                                       
221500                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
221600                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
221700                 END-IF                                                   
221800                                                                          
221900                 MOVE +0                TO RAD-TIRES                      
222000                 MOVE +0                TO RAD-TIAVBOKN                   
222100                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
222200                 MOVE SPAR-ORAD-CLEARGROUP                                
222300                                        TO RAD-CLEARGROUP                 
222400                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
222500                                           RAD-IDDC-RO                    
222600                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
222700                 MOVE '2'               TO RAD-KDSTARAD                   
222800                 PERFORM S36-ANDRA-WDC711                                 
222900                 PERFORM IMS-REPL-ORDP01                                  
223000              ELSE                                                        
223100                 SUBTRACT WS-KVORAPP-PACK FROM RAD-KVART                  
223200                 PERFORM IMS-REPL-ORDP01                                  
223300                                                                          
223400                 IF RAD-DARODAT = 0                                       
223500                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
223600                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
223700                 END-IF                                                   
223800                                                                          
223900                 MOVE +0                TO RAD-TIRES                      
224000                 MOVE +0                TO RAD-TIAVBOKN                   
224100                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
224200                 MOVE SPAR-ORAD-CLEARGROUP                                
224300                                        TO RAD-CLEARGROUP                 
224400                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
224500                                           RAD-IDDC-RO                    
224600                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
224700                 MOVE WS-KVORAPP-PACK   TO RAD-KVART                      
224800                 MOVE '2'               TO RAD-KDSTARAD                   
224900                 ADD +1                 TO RAD-IDLOPNR                    
225000                 PERFORM S36-ANDRA-WDC711                                 
225100                 PERFORM IMS-ISRT-ORDP01                                  
225200                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
225300                   ADD +1             TO RAD-IDLOPNR                      
225400                   PERFORM IMS-ISRT-ORDP01                                
225500                 END-PERFORM                                              
225600              END-IF                                                      
225700             END-IF                                                       
225800           END-IF                                                         
225900        END-IF                                                            
226000     END-IF                                                               
226100     .                                                                    
226200     EJECT                                                                
226300 DFA-KATEGORI              SECTION.                                       
226400     SKIP3                                                                
226500*    I DENNA SEKTION LÄSES STYRREG FÖR ATT BESTÄMMA PRIO FÖR              
226600*    DEN NYA ORDERKLASSEN                                                 
226700                                                                          
226800     MOVE LOW-VALUE                  TO W-WDGXKEY-N5-MIN                  
226900     MOVE HIGH-VALUE                 TO W-WDGXKEY-N5-MAX                  
227000     MOVE LOW-VALUE                  TO W-KDRAPRIO-N5-MIN-X               
227100     MOVE HIGH-VALUE                 TO W-KDRAPRIO-N5-MAX-X               
227200     MOVE WS-KDTPOTYP                TO W-KDTPOTYP-N5                     
227300     MOVE KORD-IDDISTR               TO W-IDDISTR-FOM-N5                  
227400     MOVE KORD-IDDISTR               TO W-IDDISTR-TOM-N5                  
227500     MOVE SPAR-ORAD-KDORDKL          TO W-KDORDKL-N5                      
227600     MOVE '4511'                     TO W-IDHTYP-N5                       
227700     MOVE LOW-VALUE                  TO W-VALFRI-N5                       
227800                                                                          
227900     PERFORM IMS-GU-XXJN                                                  
228000                                                                          
228100     MOVE STYR-4512-KDRAPRIO         TO WS-KDRAPRIO                       
228200     PERFORM DFAA-OVERRIDA-EV-KDRAPRIO                                    
228300     .                                                                    
228400     EJECT                                                                
228500 DFAA-OVERRIDA-EV-KDRAPRIO SECTION.                                       
228600                                                                          
228700     MOVE KORD-IDDISTR TO TEST-IDDISTR                                    
228800                                                                          
228900     IF DCS-CDC AND                                                       
229000        DIST07-USA-RETAILER       OR                                      
229100        DIST07-USA-SUPPL-FROM-CDC OR                                      
229200        DIST35-REFILL-NA                                                  
229300       IF KORD-KDFRAKT = 17 AND KORD-KDORDKL = 4                          
229400         MOVE 40                     TO WS-KDRAPRIO                       
229500       END-IF                                                             
229600     END-IF                                                               
229700     .                                                                    
229800     EJECT                                                                
229900 DFB-FLYTTA-WDA5-POSTER    SECTION.                                       
230000     SKIP3                                                                
230100     MOVE KORD-IDDISTR               TO RAD-IDDISTR                       
230200     MOVE KORD-IDKUNDNR              TO RAD-IDKUNDNR                      
230300                                                                          
230400     MOVE KORD-IDKUNDRF              TO RAD-IDKUNDRF                      
230500     MOVE SPAR-ORAD-IDARTNR          TO RAD-IDARTNR                       
230600     MOVE +1                         TO RAD-IDLOPNR                       
230700     MOVE SPAR-ORAD-BERADREF         TO RAD-BERADREF                      
230800     MOVE 'N'                        TO RAD-FLERS                         
230900     MOVE SPAR-ORAD-IDARTNR          TO W-IDARTNR                         
231000     MOVE WS-IDANSK                  TO RAD-IDANSK                        
231100     MOVE SPAR-ORAD-IDKONTO          TO RAD-IDKONTO                       
231200     MOVE SPAR-ORAD-IDKST            TO RAD-IDKST                         
231300     MOVE SPAR-ORAD-IDANALYS         TO RAD-IDANALYS                      
231400     MOVE '00000     '               TO RAD-IDKUNDRF-LEV                  
231500     MOVE SPAR-ORAD-KDOI             TO RAD-KDOI                          
231600     MOVE SPAR-ORAD-CLEARGROUP       TO RAD-CLEARGROUP                    
231700     MOVE SPAR-ORAD-IDDC-RO          TO RAD-IDDC                          
231800                                        RAD-IDDC-RO                       
231900     MOVE SPAR-ORAD-KDDSP            TO RAD-KDDSP                         
232000     MOVE KORD-KDFAKTYP              TO RAD-KDFAKTYP                      
232100     MOVE SPAR-ORAD-KDFRAKT          TO RAD-KDFRAKT                       
232200     MOVE SPAR-ORAD-KDKVBRYT         TO RAD-KDKVBRYT                      
232300     MOVE SPAR-ORAD-KDORDING         TO RAD-KDORDING                      
232400     MOVE KORD-KDORDKL               TO RAD-KDORDKL                       
232500     MOVE SPAR-ORAD-KDPRODSL         TO RAD-KDPRODSL                      
232600     MOVE WS-KDRAPRIO                TO RAD-KDRAPRIO                      
232700     MOVE +4                         TO RAD-KDROO                         
232800     MOVE '2'                        TO RAD-KDSTARAD                      
232900     MOVE WS-KDTPOTYP                TO RAD-KDTPOTYP                      
233000     MOVE SPAR-ORAD-KDVRINFO         TO RAD-KDVRINFO                      
233100     IF DCS-CDC OR DCS-NDC                                                
233200       MOVE WS-KVORAPP-TOTAL         TO RAD-KVART                         
233300                                        RAD-KVRO                          
233400     ELSE                                                                 
233500       MOVE WS-KVORAPP-PACK          TO RAD-KVART                         
233600                                        RAD-KVRO                          
233700     END-IF                                                               
233800     MOVE SPAR-ORAD-PRARTNTO         TO RAD-PRARTNTO                      
233900     MOVE SPAR-ORAD-PRARTNTO-LOC     TO RAD-PRARTNTO-LOC                  
234000     MOVE SPAR-ORAD-PRARTNTO-LOCPREL TO RAD-PRARTNTO-LOCPREL              
234100     MOVE SPAR-ORAD-REKSIFFR         TO RAD-REKSIFFR                      
234200     MOVE ZERO                       TO RAD-TIAVBOKN                      
234300     MOVE DAT-TIAAMMDD               TO RAD-DARODAT                       
234400                                        RAD-TIREGDAT                      
234500     MOVE DAT-TISEKEL                TO RAD-DARODAT (1:2)                 
234600     MOVE ZERO                       TO RAD-TIRES                         
234700     MOVE +0                         TO RAD-TITPO                         
234800     MOVE JA                         TO RAD-FLTPOBEK                      
234900     MOVE WS-OHUV-BEKUNDRF           TO RAD-BEKUNDRF                      
235000     MOVE SPAR-ORAD-BEVOLREF         TO RAD-BEVOLREF                      
235100     MOVE SPAR-ORAD-IDKAMPRF         TO RAD-IDKAMPRF                      
235200     MOVE SPAR-ORAD-IDLEVNR          TO RAD-IDLEVNR                       
235300     MOVE SPAR-ORAD-IDSYSTEM         TO RAD-IDSYSTEM                      
235400     MOVE SPAR-ORAD-KVBEART          TO RAD-KVBEART-Q                     
235500     MOVE WS-TTMMSS                  TO RAD-TIREGTID                      
235600     MOVE 0                          TO RAD-DASENDAT                      
235700     MOVE 0                          TO RAD-TISENBEK-KL                   
235800     MOVE SPAR-ORAD-KDPRTYP          TO RAD-KDPRTYP                       
235900     MOVE SPAR-ORAD-FLINVEST         TO RAD-FLINVEST                      
236000     MOVE SPAR-ORAD-FLPRTILL         TO RAD-FLPRTILL                      
236100     MOVE SPAR-ORAD-IDPRQUES         TO RAD-IDPRQUES                      
236200     MOVE SPAR-ORAD-PRARTBTO-LOC     TO RAD-PRARTBTO-LOC                  
236300     IF SPAR-ORAD-PRAVCOST > ZERO                                         
236400       MOVE SPAR-ORAD-KDVALISO-EXP   TO RAD-KDVALISO                      
236500     ELSE                                                                 
236600       MOVE SPAR-ORAD-KDVALISO       TO RAD-KDVALISO                      
236700     END-IF                                                               
236800     MOVE SPAR-ORAD-KDVAT            TO RAD-KDVAT                         
236900     MOVE SPAR-ORAD-RERAB            TO RAD-RERAB                         
237000     MOVE SPAR-ORAD-KDRAB            TO RAD-KDRAB                         
237100     MOVE SPAR-ORAD-BEART-VIPS       TO RAD-BEART-VIPS                    
237200                                                                          
237300     MOVE OHUV-KDORDTYP-LDC          TO RAD-KDORDTYP-LDC                  
237400     MOVE OHUV-TIREPDAT              TO RAD-TIREPDAT                      
237500     MOVE SPAR-ORAD-IDKUNDRF-WIP     TO RAD-IDKUNDRF-WIP                  
237600     MOVE SPAR-ORAD-PRAVCOST         TO RAD-PRAVCOST                      
237700     MOVE ARB-KDROPACK               TO RAD-KDROPACK                      
237800     MOVE SPAR-ORAD-IDARBREF         TO RAD-IDARBREF                      
237900                                                                          
238000     PERFORM DFBA-KOLLA-CROSS-DOCKING                                     
238100     .                                                                    
238200     EJECT                                                                
238300 DFBA-KOLLA-CROSS-DOCKING   SECTION.                                      
238400                                                                          
238500     MOVE SPAR-ORAD-IDARTNR                                               
238600                             TO W-IDARTNR                                 
238700     PERFORM IMS-GHU-ARTC                                                 
238800     MOVE 1                  TO IX-CD-OMR                                 
238900     PERFORM UNTIL IX-CD-OMR > 4                                          
239000     OR SPAR-ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                 
239100       ADD 1                 TO IX-CD-OMR                                 
239200     END-PERFORM                                                          
239300     IF IX-CD-OMR <= 4                                                    
239400*                                                                         
239500*BOKA UPP CROSS DOCKING SALDO                                             
239600*                                                                         
239700        ADD SPAR-ORAD-KVBEART                                             
239800                         TO CLAG-KVLS-CD (IX-CD-OMR)                      
239900     END-IF                                                               
240000     PERFORM IMS-REPL-ARTC                                                
240100     SKIP2                                                                
240200     .                                                                    
240300     EJECT                                                                
240400 DFC-SAMMANSL-BIPACKAD-RAD SECTION.                                       
240500     SKIP3                                                                
240600     MOVE RAD-IDDISTR                TO W1-IDDISTR W2-IDDISTR             
240700     MOVE RAD-IDKUNDNR               TO W1-IDKUNDNR W2-IDKUNDNR           
240800     MOVE RAD-IDKUNDRF               TO W1-IDKUNDRF W2-IDKUNDRF           
240900     MOVE RAD-IDARTNR                TO W1-IDARTNR  W2-IDARTNR            
241000     MOVE +0                         TO W1-IDLOPNR                        
241100     MOVE +999                       TO W2-IDLOPNR                        
241200     MOVE '2'                        TO W-KDSTARAD                        
241300                                                                          
241400     PERFORM IMS-GHU-ORDP01-OLD                                           
241500                                                                          
241600     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
241700                                                                          
241800     PERFORM UNTIL SEGMENT-SAKNAS           OR                            
241900             WS-SAMMANSLAGNING-RAD = JA                                   
242000                                                                          
242100        IF  OLD-RAD-KDFRAKT  = RAD-KDFRAKT                                
242200          AND OLD-RAD-KDFRAKT  = RAD-KDFRAKT                              
242300          AND OLD-RAD-KDORDKL  = RAD-KDORDKL                              
242400          AND OLD-RAD-PRARTNTO = RAD-PRARTNTO                             
242500          AND OLD-RAD-DEAL-PR-LINE = RAD-DEAL-PR-LINE                     
242600          AND OLD-RAD-KDTPOTYP = RAD-KDTPOTYP                             
242700          AND OLD-RAD-IDKONTO  = RAD-IDKONTO                              
242800          AND OLD-RAD-IDKST    = RAD-IDKST                                
242900           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
243000        ELSE                                                              
243100           PERFORM IMS-GHN-ORDP01-OLD                                     
243200        END-IF                                                            
243300     END-PERFORM                                                          
243400     .                                                                    
243500     EJECT                                                                
243600 DFD-SAMMANSL-EJ-BIPACKAD-RAD SECTION.                                    
243700     SKIP3                                                                
243800     MOVE KORD-IDDISTR               TO W1-IDDISTR  W2-IDDISTR            
243900     MOVE KORD-IDKUNDNR              TO W1-IDKUNDNR W2-IDKUNDNR           
244000     MOVE KORD-IDKUNDRF              TO W1-IDKUNDRF W2-IDKUNDRF           
244100     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
244200     MOVE +0                         TO W1-IDLOPNR                        
244300     MOVE +999                       TO W2-IDLOPNR                        
244400     MOVE '2'                        TO W-KDSTARAD                        
244500                                                                          
244600     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
244700                                                                          
244800     PERFORM IMS-GHU-ORDP01-OLD                                           
244900                                                                          
245000     PERFORM UNTIL SEGMENT-SAKNAS      OR                                 
245100                   WS-SAMMANSLAGNING-RAD = JA                             
245200                                                                          
245300        IF  OLD-RAD-KDFRAKT  = KORD-KDFRAKT                               
245400          AND OLD-RAD-KDORDKL  = KORD-KDORDKL                             
245500          AND OLD-RAD-PRARTNTO = SPAR-ORAD-PRARTNTO                       
245600          AND OLD-RAD-PRARTNTO-LOC = SPAR-ORAD-PRARTNTO-LOC               
245700          AND OLD-RAD-PRARTNTO-LOCPREL =                                  
245800                                        SPAR-ORAD-PRARTNTO-LOCPREL        
245900          AND OLD-RAD-KDTPOTYP = WS-KDTPOTYP                              
246000          AND OLD-RAD-IDKONTO  = SPAR-ORAD-IDKONTO                        
246100          AND OLD-RAD-IDKST    = SPAR-ORAD-IDKST                          
246200          MOVE  JA                   TO WS-SAMMANSLAGNING-RAD             
246300        ELSE                                                              
246400           PERFORM IMS-GHN-ORDP01-OLD                                     
246500        END-IF                                                            
246600     END-PERFORM                                                          
246700     .                                                                    
246800     EJECT                                                                
246900 DFE-HAMTA-TPOTYP-FRAN-ROREG             SECTION.                         
247000                                                                          
247100     MOVE KORD-IDDISTR               TO W1-IDDISTR  W2-IDDISTR            
247200     MOVE KORD-IDKUNDNR              TO W1-IDKUNDNR W2-IDKUNDNR           
247300                                                                          
247400     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
247500       MOVE KORD-IDKUNDRF            TO W1-IDKUNDRF W2-IDKUNDRF           
247600     ELSE                                                                 
247700       MOVE SPAR-ORAD-IDKUNDRF-RO    TO W1-IDKUNDRF W2-IDKUNDRF           
247800     END-IF                                                               
247900                                                                          
248000     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
248100     MOVE SPAR-ORAD-IDLOPNR-RO       TO W1-IDLOPNR  W2-IDLOPNR            
248200     MOVE '4'                        TO W-KDSTARAD                        
248300                                                                          
248400     PERFORM IMS-GHU-ORDP01-OLD                                           
248500                                                                          
248600     IF SEGMENT-FINNS                                                     
248700       MOVE OLD-RAD-KDTPOTYP         TO WS-KDTPOTYP                       
248800     ELSE                                                                 
248900       MOVE 0                        TO WS-KDTPOTYP                       
249000     END-IF                                                               
249100     .                                                                    
249200     EJECT                                                                
249300 DI-BESTAM-ORDERBEKR-KOD                 SECTION.                         
249400                                                                          
249500     MOVE KORD-IDDISTR            TO TEST-IDDISTR                         
249600     IF  SPAR-ORAD-FLSDCLEV = JA                                          
249700        MOVE +080                         TO WS-KDORDBEK                  
249800*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
249900     ELSE                                                                 
250000       IF SPAR-ORAD-KDORDKL > 0                                           
250100         IF SPAR-ORAD-KVSLATT > 0                                         
250200           IF SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                      
250300                                 - SPAR-ORAD-KVANNANT                     
250400                                 - SPAR-ORAD-KVSLATT                      
250500             PERFORM DIA-SATT-KOD-80-90-ELLER-91                          
250600           ELSE                                                           
250700             MOVE 81                       TO WS-KDORDBEK                 
250800*           81 = INTE RESTNOTERING, GÖR NY BESTÄLLNING, SLATT.            
250900           END-IF                                                         
251000         ELSE                                                             
251100           PERFORM DIA-SATT-KOD-80-90-ELLER-91                            
251200         END-IF                                                           
251300       ELSE                                                               
251400*           93 = VOR, RESTNOTERAD KVANT, FYSISK AVVIKELSE                 
251500         MOVE 93                           TO WS-KDORDBEK                 
251600       END-IF                                                             
251700     END-IF                                                               
251800     .                                                                    
251900     EJECT                                                                
252000 DIA-SATT-KOD-80-90-ELLER-91             SECTION.                         
252100                                                                          
252200     IF SPAR-ORAD-FLRESTN = JA                                            
252300       IF SPAR-ORAD-IDKUNDRF-RO > '00000     '                            
252400         MOVE 91                   TO WS-KDORDBEK                         
252500       ELSE                                                               
252600         MOVE 90                   TO WS-KDORDBEK                         
252700       END-IF                                                             
252800     ELSE                                                                 
252900       MOVE 80                     TO WS-KDORDBEK                         
253000     END-IF                                                               
253100     .                                                                    
253200     EJECT                                                                
253300 DJ-UPPDATERA-EV-KAMP-REG  SECTION.                                       
253400                                                                          
253500     IF SPAR-ORAD-IDKAMPRF     > 0 AND                                    
253600       (WS-KDORDBEK           = 80 OR 81)                                 
253700         COMPUTE WS-KVSLATTAT  = SPAR-ORAD-KVBEART -                      
253800                                 SPAR-ORAD-KVLEVART -                     
253900                                 SPAR-ORAD-KVANNANT                       
254000         PERFORM DJA-UPPDATERA-WDM211                                     
254100         PERFORM DJB-UPPDATERA-WDM221                                     
254200     END-IF                                                               
254300     .                                                                    
254400     EJECT                                                                
254500 DJA-UPPDATERA-WDM211 SECTION.                                            
254600                                                                          
254700     MOVE SPAR-ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                     
254800     MOVE KORD-IDDC                TO W-KAMP-IDDC                         
254900     MOVE SPAR-ORAD-IDARTNR        TO W-KART-IDARTNR                      
255000     PERFORM IMS-GHU-WDM211                                               
255100     IF SEGMENT-FINNS                                                     
255200        MOVE KART-KVRESS-ART       TO SPAR-KART-KVRESS-ART                
255300                                                                          
255400        IF KART-KVBEART-KUND     >= WS-KVSLATTAT                          
255500          SUBTRACT WS-KVSLATTAT     FROM KART-KVBEART-KUND                
255600          COMPUTE KART-KVRESS-ART = KART-KVRESS-ART                       
255700                                       + WS-KVSLATTAT                     
255800        ELSE                                                              
255900          MOVE 'WDM211 KART-KVBEART-KUND BLIR NEGATIV'                    
256000                                   TO FELTEXT                             
256100          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
256200        END-IF                                                            
256300        PERFORM IMS-REPL-WDM211                                           
256400     END-IF                                                               
256500     .                                                                    
256600     EJECT                                                                
256700 DJB-UPPDATERA-WDM221 SECTION.                                            
256800                                                                          
256900     MOVE SPAR-ORAD-IDKAMPRF   TO W-KAMP-IDKAMPRF                         
257000     MOVE KORD-IDDC            TO W-KAMP-IDDC                             
257100     MOVE SPAR-ORAD-IDARTNR    TO W-KART-IDARTNR                          
257200     MOVE WS-IDDISTR           TO W-KMRK-IDDISTR-FOM                      
257300     MOVE WS-IDDISTR           TO W-KMRK-IDDISTR-TOM                      
257400     MOVE WS-IDKUNDNR          TO W-KMRK-IDKUNDNR-FOM                     
257500     MOVE WS-IDKUNDNR          TO W-KMRK-IDKUNDNR-TOM                     
257600                                                                          
257700     PERFORM S20-FINN-INTERVALL                                           
257800     PERFORM IMS-GHU-WDM221                                               
257900                                                                          
258000     IF SEGMENT-FINNS                                                     
258100       IF KMRK-KVBEART-KUND >= WS-KVSLATTAT                               
258200         SUBTRACT WS-KVSLATTAT FROM KMRK-KVBEART-KUND                     
258300       ELSE                                                               
258400           MOVE 'WDM221 KMRK-KVBEART-KUND BLIR NEGATIV'                   
258500                              TO FELTEXT                                  
258600           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
258700       END-IF                                                             
258800       PERFORM IMS-REPL-WDM221                                            
258900     END-IF                                                               
259000     .                                                                    
259100     EJECT                                                                
259200 G-UPPDATERA-KOLLIREG      SECTION.                                       
259300     MOVE WS-IDPRODNR                TO  W-601-IDPRODNR                   
259400     PERFORM IMS-GHU-KOLLIREG                                             
259500     IF SEGMENT-FINNS                                                     
259600         MOVE VORD-IDDC              TO W-SPAR-IDDC                       
259700         MOVE VORD-IDDISTR           TO  TEST-IDDISTR                     
259800         IF   DIST03-SVERIGE                                              
259900         AND  VORD-FLDIRLEV = NEJ                                         
260000         AND  VORD-KDFRAKT  NOT = +17                                     
260100              PERFORM IMS-GNP-KOLLI-99XXX                                 
260200              IF  SEGMENT-FINNS                                           
260300                  MOVE JA  TO KOLLI-99XXX-FINNS                           
260400              ELSE                                                        
260500                  MOVE NEJ TO KOLLI-99XXX-FINNS                           
260600              END-IF                                                      
260700         END-IF                                                           
260800     END-IF                                                               
260900                                                                          
261000     PERFORM IMS-GHU-KOLLIREG                                             
261100                                                                          
261200     IF SEGMENT-FINNS                                                     
261300         MOVE VORD-IDDISTR           TO  TEST-IDDISTR                     
261400         PERFORM S11-UPPD-VORD-FRAN-SPAR                                  
261500                                                                          
261600         IF RADER-SLUT                                                    
261700             MOVE VORD-KVORDRAD      TO  VORD-KVORDRAD-PACK               
261800             MOVE VORD-IDDC          TO  WS-IDDC                          
261900             MOVE VORD-KDFAKTYP      TO  WS-KDFAKTYP                      
262000             IF VORD-KVORDRAD-PACK   =   VORD-KVORDRAD                    
262100*****************************************************************         
262200*** KOMMENTAR AV SVANTE B. 920219                             ***         
262300*****************************************************************         
262400*** EFTER DENNA KOMMENTAR GJÖRS FYRA UPPDATERINGAR:           ***         
262500***                                                           ***         
262600*** A) VORD-TIPACKN-SK SÄTTS                                  ***         
262700*** B) "S13-GENERERA-KLAR-SV4" SKAPAR EN "RY6"-TRANS.         ***         
262800*** C) VORD-KDORDSTA SÄTTS                                    ***         
262900*** D) "S04-PACK-ORDER-LISTA" STARTAR 4342                    ***         
263000***                                                           ***         
263100*** OM MAN TAR BORT EN ORDERDEL I ORDER-ENTRY FÖR EN ORDER    ***         
263200*** DÄR RESTERANDE ORDERDELAR REDAN ÄR PACKADE FÅR MAN ETT    ***         
263300*** LÄGE DÄR ORDERN GÅR FRÅN OPACKAD TILL PACKAD. MAN MÅSTE   ***         
263400*** DÅ I W413AVSO UTFÖRA PUNKT A-D.                           ***         
263500***                                                           ***         
263600*** OM MAN I 4397 ELLER 4398 ÄNDRAR DESSA UPPDATERINGAR ELLER ***         
263700*** LÄGGER TILL NYA MÅSTE MAN DÄRFÖR ÄVEN GÖRA DETTA I        ***         
263800*** W413AVSO.                                                 ***         
263900*****************************************************************         
264000                 IF  VORD-KVKOLPAC = 0                                    
264100                     MOVE DAT-TIAAMMDD  TO VORD-TIPACKN-SK                
264200                 END-IF                                                   
264300                                                                          
264400                 IF VORD-KVKOLLI     =   VORD-KVKOLPAC                    
264500                                                                          
264600                     IF  VORD-KDORDSTA < +3                               
264700                     AND VORD-KVKOLLI  > +0                               
264800                     AND (DIST03-SVERIGE OR DIS103-EMB-INFO)              
264900                         PERFORM S13-GENERERA-KLAR-SV                     
265000                         MOVE VORD-KDFRAKT   TO WS-KDFRAKT                
265100                         MOVE WS-KDFRAKT     TO FRAK01-KDFRAKT            
265200                         IF  FRAK01-SVERIGE2                              
265300                         OR  FRAK01-NORDEN                                
265400                         OR  FRAK01-KDFRAKT21                             
265500                         OR  FRAK01-KDFRAKT62                             
265600                            IF   VORD-FLDIRLEV = NEJ                      
265700                            AND  VORD-KDFRAKT  NOT = +17                  
265800                            AND  KOLLI-99XXX-FINNS = JA                   
265900                               IF NOT DIS128-FRAKTS                       
266000                                 MOVE JA TO VORD-FLFRAKTS                 
266100                               END-IF                                     
266200                            END-IF                                        
266300                         END-IF                                           
266400                     END-IF                                               
266500                                                                          
266600                     MOVE +3         TO  VORD-KDORDSTA                    
266700                     IF VORD-FLAUTFAK =  JA                               
266800                     AND VORD-KVKOLLI > 0                                 
266900                         MOVE JA     TO  WS-FLAUTFAK                      
267000                     END-IF                                               
267100                 END-IF                                                   
267200                 IF  VORD-KVKOLLI    =   VORD-KVKOLLI-FL                  
267300                 AND VORD-KVKOLLI    =   VORD-KVKOLPAC                    
267400                     MOVE +4         TO  VORD-KDORDSTA                    
267500                 END-IF                                                   
267600                 IF  VORD-KVKOLLI    =   VORD-KVKOLLI-FAKT                
267700                 AND VORD-KVKOLLI    =   VORD-KVKOLLI-LAST                
267800                 AND VORD-KVKOLLI    =   VORD-KVKOLPAC                    
267900                     MOVE +5         TO  VORD-KDORDSTA                    
268000                                         W-SPAR-KDORDSTA                  
268100                 END-IF                                                   
268200             END-IF                                                       
268300                                                                          
268400             IF VORD-KDORDSTA > 2                                         
268500                 MOVE JA             TO  FL-LASNINGSTRANS                 
268600                                                                          
268700                 IF DCS-KDPORDL = JA                                      
268800                    MOVE JA             TO  FL-PACKADEORDERL              
268900                 END-IF                                                   
269000                                                                          
269100             END-IF                                                       
269200         END-IF                                                           
269300     SKIP2                                                                
269400         PERFORM IMS-REPL-KOLLIREG                                        
269500     SKIP2                                                                
269600     ELSE                                                                 
269700         MOVE FEL                       TO  WS-BEHANDLING-TEST            
269800     END-IF                                                               
269900     .                                                                    
270000     EJECT                                                                
270100 H-AVSLUT             SECTION.                                            
270200     SKIP3                                                                
270300     IF WS-BEHANDLING-RATT                                                
270400         IF SKRIV-PACKADEORDERL                                           
270500             PERFORM S04-PACK-ORDER-LISTA                                 
270600         END-IF                                                           
270700         IF LASNINGSTRANS-TAS-BORT                                        
270800             PERFORM S03-BORTTAG-LASPOST                                  
270900         END-IF                                                           
271000         IF AUT-FAKTURA-SKRIVS-UT                                         
271100           PERFORM S05-AUTOMATFAKTURERING                                 
271200           IF SEGMENT-SAKNAS                                              
271300               MOVE SPACE          TO  IO-AREA3                           
271400               MOVE WS-IDDISTR     TO  AUTFAKT-IDDISTR                    
271500               MOVE WS-IDKUNDNR    TO  AUTFAKT-IDKUNDNR                   
271600               MOVE WS-IDDC        TO  AUTFAKT-IDDC                       
271700               MOVE WS-KDFAKTYP    TO  AUTFAKT-KDFAKTYP                   
271800               PERFORM IMS-ISRT-AUTFAKTURA-ROT                            
271900               MOVE SPACE          TO  IO-AREA3                           
272000               PERFORM S05-AUTOMATFAKTURERING                             
272100             END-IF                                                       
272200         END-IF                                                           
272300         MOVE 'W0T605U '                 TO MSG-KDTRANS-1                 
272400         MOVE '4398'                     TO MSG-IDTRANS-1                 
272500         MOVE WS-KDMFSFOR                TO MSG-KDMFSFOR-1                
272600         MOVE 0605-LAENGD                TO MSG-KVLL                      
272700         MOVE SPACE                      TO 0605-MID-TEMFSFEL             
272800         MOVE JA                         TO 0605-MID-FLSVAR               
272900         MOVE 0605-MID TO MSG-INDATA-MINUS-1-TRANSKOD                     
273000     END-IF                                                               
273100     .                                                                    
273200     EJECT                                                                
273300 I-UPPDATERA-WDE401-OCH-WDQ301           SECTION.                         
273400                                                                          
273500     IF RADER-SLUT                                                        
273600       IF MID-IDRADNR (1) = ALL '+'                                       
273700         PERFORM IA-LAS-WDE401                                            
273800       END-IF                                                             
273900                                                                          
274000       MOVE KORD-IDDISTR                 TO W-4A1-IDDISTR                 
274100       MOVE KORD-IDKUNDNR                TO W-4A1-IDKUNDNR                
274200       MOVE KORD-IDORDNR5                TO W-4A1-IDORDNR                 
274300       PERFORM IMS-GHU-WDE401-SEQA                                        
274400                                                                          
274500       PERFORM UNTIL SEGMENT-SAKNAS                                       
274600         IF WS-IDPRODNR = KORD-IDPRODNR                                   
274700                                                                          
274800            COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD    +               
274900                                         KORD-KVORDRAD-LEVPL              
275000            END-COMPUTE                                                   
275100                                                                          
275200            PERFORM IMS-REPL-WDE401-MED-PCB-WDE41                         
275300            MOVE KORD-IDDISTR               TO TEST-IDDISTR               
275400                                                                          
275500            IF NOT DIST19-SATS                                            
275600               PERFORM IB-UPPDATERA-ORQA                                  
275700               PERFORM IC-UPPDATERA-PRODTAB                               
275800            END-IF                                                        
275900         END-IF                                                           
276000                                                                          
276100         PERFORM IMS-GHN-WDE401-SEQA                                      
276200       END-PERFORM                                                        
276300       PERFORM ID-UPPDATERA-ORQI                                          
276400     END-IF                                                               
276500     .                                                                    
276600     EJECT                                                                
276700 IA-LAS-WDE401                           SECTION.                         
276800     MOVE WS-IDPRODNR TO W-411-IDPRODNR2-MIN                              
276900                         W-411-IDPRODNR2-MAX                              
277000     PERFORM IMS-GU-WDE401-MED-PCB-WDE42                                  
277100     .                                                                    
277200     EJECT                                                                
277300 IB-UPPDATERA-ORQA                       SECTION.                         
277400                                                                          
277500     MOVE KORD-IDORDER               TO Q301-IDORDER                      
277600                                        WS-IDORDER                        
277700     MOVE KORD-IDDC                  TO Q301-IDDC                         
277800     MOVE KORD-IDPRODNR              TO Q301-IDPRODNR                     
277900     MOVE KORD-IDPLKLST              TO Q301-IDPLKLST                     
278000     PERFORM IMS-GHU-ORQA01                                               
278100                                                                          
278200     MOVE KORD-KVORDRAD-PACK         TO ODEL-KVPACKRAD-OD                 
278300     MOVE 'P'                        TO ODEL-KDODELSTA                    
278400     MOVE DAT-TIAAMMDD               TO ODEL-TIPACKN                      
278500     MOVE WS-TTMMSS                  TO ODEL-TIPACTID                     
278600     PERFORM IMS-REPL-ORQA                                                
278700                                                                          
278800     PERFORM S06-BORTTAG-PRODTAB                                          
278900     .                                                                    
279000 IC-UPPDATERA-PRODTAB                    SECTION.                         
279100                                                                          
279200     IF WS-ANT-RADER-FYSAVVIK > 0                                         
279300        PERFORM ICA-LAES-SHIFTTAB                                         
279400        IF ODEL-KDPRODKL = 'B' OR ODEL-KDPRODKL = 'C'                     
279500*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
279600                                                                          
279700           MOVE KORD-IDDC          TO W-4471-IDDC                         
279800           MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                     
279900           MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                     
280000           PERFORM IMS-GHU-XXKW11                                         
280100                                                                          
280200           IF SEGMENT-FINNS                                               
280300*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
280400                                                                          
280500              MOVE 1                 TO IND1                              
280600              MOVE W-4478-IDSHIFT    TO IND2                              
280700              MOVE ODEL-DARFS (3:10) TO HJALP-ODEL-TIRFS                  
280800              MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                  
280900                                                                          
281000              PERFORM UNTIL IND1 = 30 OR                                  
281100                         4472-TIRFS (IND1) = ZERO OR                      
281200                         HJALP-ODEL-TIRFS-7 = HJALP-4472-TIRFS-7          
281300                ADD 1                  TO IND1                            
281400                MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                
281500              END-PERFORM                                                 
281600                                                                          
281700              MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                 
281800              MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)           
281900              ADD WS-ANT-RADER-FYSAVVIK                                   
282000                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
282100              PERFORM ICB-ADDERA-TOTAL-PRODTID                            
282200              PERFORM IMS-REPL-XXKW11                                     
282300           END-IF                                                         
282400        END-IF                                                            
282500     END-IF                                                               
282600     .                                                                    
282700     EJECT                                                                
282800 ICA-LAES-SHIFTTAB         SECTION.                                       
282900                                                                          
283000     MOVE KORD-IDDC         TO W-4477-IDDC                                
283100     MOVE '1'               TO W-4478-IDSHIFT                             
283200     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
283300     PERFORM IMS-GU-XXLB                                                  
283400                                                                          
283500     IF SEGMENT-SAKNAS                                                    
283600        MOVE '2'            TO W-4478-IDSHIFT                             
283700        PERFORM IMS-GU-XXLB                                               
283800                                                                          
283900        IF SEGMENT-SAKNAS                                                 
284000           MOVE '3'         TO W-4478-IDSHIFT                             
284100           PERFORM IMS-GU-XXLB                                            
284200                                                                          
284300           IF SEGMENT-SAKNAS                                              
284400              MOVE '1'      TO W-4478-IDSHIFT                             
284500           END-IF                                                         
284600        END-IF                                                            
284700     END-IF                                                               
284800     .                                                                    
284900     EJECT                                                                
285000 ICB-ADDERA-TOTAL-PRODTID             SECTION.                            
285100                                                                          
285200     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
285300                                                                          
285400     COMPUTE WS-KVPTID-MIN ROUNDED = WS-ANT-RADER-FYSAVVIK *              
285500                                     ODEL-KVPTID                          
285600     END-COMPUTE                                                          
285700                                                                          
285800     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
285900     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
286000     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
286100                            (WS-KVPTID-TIM * 60)                          
286200                                                                          
286300     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
286400     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
286500     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
286600     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
286700                            (WS-KVPTID-TIM * 60)                          
286800     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
286900                                                                          
287000     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
287100     .                                                                    
287200     EJECT                                                                
287300 ID-UPPDATERA-ORQI                       SECTION.                         
287400                                                                          
287500     MOVE NEJ                       TO KDORDSTA-SW                        
287600                                                                          
287700     IF DIST19-SATS                                                       
287800       CONTINUE                                                           
287900     ELSE                                                                 
288000       MOVE KORD-IDORDER            TO W-Q301KY-MIN-IDORDER               
288100                                       W-Q301KY-MAX-IDORDER               
288200       MOVE WS-IDDC                 TO W-Q301KY-MIN-IDDC                  
288300                                       W-Q301KY-MAX-IDDC                  
288400                                                                          
288500       MOVE 'R'                     TO W-KDODELST                         
288600       PERFORM IMS-GU-ORQA-STATUS                                         
288700       IF SEGMENT-FINNS                                                   
288800          MOVE 'R*'                 TO WS-KDORDSTA                        
288900       ELSE                                                               
289000                                                                          
289100          MOVE 'U'                  TO W-KDODELST                         
289200          PERFORM IMS-GU-ORQA-STATUS                                      
289300          IF SEGMENT-FINNS                                                
289400             MOVE 'U*'              TO WS-KDORDSTA                        
289500          ELSE                                                            
289600             PERFORM IDA-KOLLA-KVKOLLI                                    
289700             IF KDORDSTA-KLAR                                             
289800                 CONTINUE                                                 
289900              ELSE                                                        
290000                 PERFORM IDB-TA-FRAM-KDORDSTA                             
290100             END-IF                                                       
290200          END-IF                                                          
290300       END-IF                                                             
290400                                                                          
290500** WDQ2 IN DB- SECTION IS NOT READ FOR NO-DEVIATION ORDERS!!              
290510       IF ARB-IDDC = LOW-VALUE                                            
290511         MOVE KORD-IDORDER               TO W-IDORDER-Q2                  
290512         PERFORM IMS-GU-WDQ201                                            
290518         MOVE WS-IDDC                    TO W-IDDC-Q2                     
290520         PERFORM IMS-GHNP-WDQ212                                          
290530       END-IF                                                             
290600       MOVE WS-KDORDSTA   TO ARB-KDORDSTA                                 
290700       PERFORM IMS-REPL-WDQ212                                            
290800     END-IF                                                               
290900     .                                                                    
291000     EJECT                                                                
291100 IDA-KOLLA-KVKOLLI SECTION.                                               
291200                                                                          
291300     MOVE ZERO                        TO W-SPAR-IDPRODNR                  
291400     PERFORM IMS-GHU-WDE401-SEQA                                          
291500                                                                          
291600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
291700                   KDORDSTA-KLAR                                          
291800        IF KORD-IDPRODNR              = W-SPAR-IDPRODNR OR                
291900           KORD-IDDC              NOT = W-SPAR-IDDC                       
292000            CONTINUE                                                      
292100         ELSE                                                             
292200            MOVE KORD-IDPRODNR        TO W-601-IDPRODNR                   
292300            PERFORM IMS-GHU-KOLLIREG                                      
292400            IF (VORD-KVKOLLI-LAST      >  ZERO    OR                      
292500                VORD-KVKOLLI-FAKT      >  ZERO    OR                      
292600                VORD-KVKOLLI-FL        >  ZERO)   OR                      
292700                W-SPAR-KDORDSTA        =  5                               
292800                                                                          
292900                COMPUTE W-KVKOLLI  =  W-KVKOLLI + VORD-KVKOLLI            
293000                COMPUTE W-KVKOLLI-FAKT =                                  
293100                               W-KVKOLLI-FAKT + VORD-KVKOLLI-FAKT         
293200                COMPUTE W-KVKOLLI-LAST =                                  
293300                               W-KVKOLLI-LAST + VORD-KVKOLLI-LAST         
293400                COMPUTE W-KVKOLLI-FL   =                                  
293500                               W-KVKOLLI-FL   + VORD-KVKOLLI-FL           
293600            ELSE                                                          
293700                MOVE 'P'              TO WS-KDORDSTA                      
293800                MOVE JA               TO KDORDSTA-SW                      
293900            END-IF                                                        
294000            MOVE KORD-IDPRODNR        TO W-SPAR-IDPRODNR                  
294100        END-IF                                                            
294200        PERFORM IMS-GHN-WDE401-SEQA                                       
294300     END-PERFORM                                                          
294400     .                                                                    
294500     EJECT                                                                
294600 IDB-TA-FRAM-KDORDSTA SECTION.                                            
294700                                                                          
294800     IF VORD-KDORDSTA          <  4                                       
294900        MOVE 'P*'              TO WS-KDORDSTA                             
295000     ELSE                                                                 
295100       IF W-KVKOLLI-FL            = W-KVKOLLI                             
295200         IF W-KVKOLLI-FAKT = ZERO AND                                     
295300            W-KVKOLLI-LAST = ZERO                                         
295400           MOVE 'S'          TO WS-KDORDSTA                               
295500         ELSE                                                             
295600           IF W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                     
295700              W-KVKOLLI-LAST     NOT = W-KVKOLLI                          
295800             MOVE 'S*'       TO WS-KDORDSTA                               
295900           ELSE                                                           
296000             MOVE 'SF'       TO WS-KDORDSTA                               
296100           END-IF                                                         
296200         END-IF                                                           
296300       END-IF                                                             
296400     END-IF                                                               
296500     .                                                                    
296600     EJECT                                                                
296700 S01-UPPD-SPAR-UPPGIFTER SECTION.                                         
296800                                                                          
296900     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
297000                         ORAD-VKARTNTO * ARB-KVLEVART                     
297100     END-COMPUTE                                                          
297200*                                                                         
297300     COMPUTE SPAR-VLORDNTO = SPAR-VLORDNTO +                              
297400                         ORAD-VLARTNTO * ARB-KVLEVART                     
297500     END-COMPUTE                                                          
297600*                                                                         
297700       COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +                
297800                        ORAD-PRARTNTO-LOC * ARB-KVLEVART                  
297900       COMPUTE SPAR-SUORDV-LOCPREL ROUNDED =                              
298000             SPAR-SUORDV-LOCPREL + ORAD-PRARTNTO-LOCPREL                  
298100                                          * ARB-KVLEVART                  
298200       COMPUTE SPAR-SUORDV ROUNDED = SPAR-SUORDV +                        
298300                      ORAD-PRARTNTO * ARB-KVLEVART                        
298400*                                                                         
298500       COMPUTE SPAR-SUORDV-TOT-LOC = SPAR-SUORDV-TOT-LOC                  
298600                                         + SPAR-SUORDV-LOC                
298700       COMPUTE SPAR-SUORDV-TOT-LOCPREL =                                  
298800             SPAR-SUORDV-TOT-LOCPREL + SPAR-SUORDV-LOCPREL                
298900       COMPUTE SPAR-SUORDV-TOT = SPAR-SUORDV-TOT + SPAR-SUORDV            
299000     COMPUTE SPAR-VKORDNTO-TOT = SPAR-VKORDNTO-TOT + SPAR-VKORDNTO        
299100     END-COMPUTE                                                          
299200     COMPUTE SPAR-VLORDNTO-TOT = SPAR-VLORDNTO-TOT + SPAR-VLORDNTO        
299300     END-COMPUTE                                                          
299400     .                                                                    
299500     EJECT                                                                
299730 S03-BORTTAG-LASPOST    SECTION.                                          
299740                                                                          
299800     MOVE WS-IDPRODNR       TO  W-XXDJ-IDPRODNR                           
299900                                4306-IDPRODNR                             
300000     MOVE WS-IDDC           TO  W-XXDJ-IDDC                               
300100     MOVE LOW-VALUE         TO  4306-LOWVALUE                             
300200     PERFORM IMS-GU-XXDJ-ROT                                              
300300     IF SEGMENT-FINNS                                                     
300400       PERFORM IMS-GHNP-XXDJ-LASNING                                      
300500       IF SEGMENT-FINNS                                                   
300600           PERFORM IMS-DLET-XXDJ-LASNING                                  
300700       END-IF                                                             
300800     END-IF                                                               
300900     .                                                                    
301000     EJECT                                                                
301100 S04-PACK-ORDER-LISTA   SECTION.                                          
301200                                                                          
301300     MOVE LOW-VALUE                TO  4342-MID                           
301400     MOVE '+++++++'                TO  4342-MID-IDPRODNR-IN               
301500     MOVE WS-IDPRODNR              TO  WS-IDPRODNR-RED                    
301600     MOVE WS-IDPRODNR-RED          TO  4342-MID-IDPRODNR-UT               
301700     MOVE 'W4T342U '               TO  MSG-KDTRANS-1                      
301800     MOVE '439H'                   TO  MSG-IDTRANS-1                      
301900     MOVE WS-KDMFSFOR              TO  MSG-KDMFSFOR-1                     
302000     MOVE 4342-LAENGD              TO  MSG-KVLL                           
302100     MOVE 4342-MID                 TO  MSG-INDATA-MINUS-1-TRANSKOD        
302200     PERFORM IMS-INSERT-ALT4342MSG                                        
302300     .                                                                    
302400     EJECT                                                                
302500 S05-AUTOMATFAKTURERING      SECTION.                                     
302600                                                                          
302700     MOVE WS-IDDISTR                      TO  W-RDG-IDDISTR               
302800     MOVE WS-IDKUNDNR                     TO  W-RDG-IDKUNDNR              
302900     MOVE WS-IDDC                         TO  W-RDG-IDDC                  
303000     MOVE WS-KDFAKTYP                     TO  W-RDG-KDFAKTYP              
303100                                                                          
303200     MOVE WS-IDPRODNR                     TO  AUTFAKT-IDPRODNR            
303300     MOVE ZERO                            TO  AUTFAKT-PRFRAKT             
303400                                              AUTFAKT-IDSKEPPN            
303500     MOVE WS-IDDISTR                      TO  TEST-IDDISTR                
303600     IF DIST19-SATS                                                       
303700        MOVE NEJ                          TO  W-XXDV-FLBATCH              
303800     ELSE                                                                 
303900*       IF DIST03-SVERIGE                                                 
304000*          IF DCS-CDC                                                     
304100*          OR (DCS-SDC AND DCS-SWEDEN)                                    
304200*          OR DIST18-SKROT                                                
304300*             MOVE JA                     TO  W-XXDV-FLBATCH              
304400*          ELSE                                                           
304500*             MOVE NEJ                    TO  W-XXDV-FLBATCH              
304600*          END-IF                                                         
304700*       ELSE                                                              
304800           MOVE NEJ                       TO  W-XXDV-FLBATCH              
304900*       END-IF                                                            
305000     END-IF                                                               
305100                                                                          
305200     IF DIST03-SVERIGE                                                    
305300         MOVE NEJ                         TO  AUTFAKT-FLLASTA             
305400     ELSE                                                                 
305500         MOVE JA                          TO  AUTFAKT-FLLASTA             
305600     END-IF                                                               
305700     PERFORM IMS-ISRT-AUTFAKTURA                                          
305800     .                                                                    
305900     EJECT                                                                
306000 S06-BORTTAG-PRODTAB         SECTION.                                     
306100                                                                          
306200     MOVE ODEL-IDDC     TO W-4447-IDDC                                    
306300     MOVE ODEL-IDPRC    TO W-4448-IDPRC                                   
306400     PERFORM IMS-GU-XXKH11                                                
306500                                                                          
306600     MOVE ODEL-IDDC     TO W-4487-IDDC                                    
306700     MOVE 4448-KDPRCGRP TO W-4488-KDPRCGRP                                
306800     MOVE ODEL-DARFS    TO W-4490-DARFS                                   
306900                                                                          
307000     MOVE ODEL-IDPRODNR TO W-4490-IDPRODNR                                
307100     MOVE ODEL-IDPLKLST TO W-4490-IDPLKLST                                
307200     PERFORM IMS-GHU-WDGX4490                                             
307300     IF SEGMENT-FINNS                                                     
307400        PERFORM IMS-DLET-WDGX4490                                         
307500     END-IF                                                               
307600     .                                                                    
307700     EJECT                                                                
307710 S07-GET-IDSYS-KDORDKL     SECTION.                                       
307720                                                                          
307760     MOVE KORD-IDDISTR         TO W-IDDISTR-WDB2                          
307770     MOVE KORD-IDKUNDNR        TO W-IDKUNDNR-WDB2                         
307780     MOVE ZERO                 TO W-GMT-KDKUNDKAT                         
307781     MOVE NEJ                  TO SW-LYNK-NON-API                         
307782                                  SW-VOR                                  
307783                                                                          
307784     PERFORM IMS-GU-GMTA-WDB201                                           
307790     IF SEGMENT-FINNS                                                     
307792        MOVE GMT-KDKUNDKAT     TO W-GMT-KDKUNDKAT                         
307793        IF W-GMT-KDKUNDKAT = 03                                           
307794          IF WS-IDSYSTEM(1:3) NOT = 'LYN'                                 
307795             MOVE JA           TO SW-LYNK-NON-API                         
307796          END-IF                                                          
307797          IF OHUV-KDORDKL = 0                                             
307798             MOVE JA           TO SW-VOR                                  
307799          END-IF                                                          
307800        END-IF                                                            
307900     END-IF                                                               
307910     .                                                                    
307920     EJECT                                                                
307930                                                                          
307956 S10-HAMTA-MASKINDATUM     SECTION.                                       
307960                                                                          
308000     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
308100     CALL WDATKONV USING DAT-KDDATFORM                                    
308200                         DAT-I-TIDATUM                                    
308300                         DAT-O-TIDATUM                                    
308400                         DAT-KDSVAR                                       
308500     .                                                                    
308600     EJECT                                                                
308700 S11-UPPD-VORD-FRAN-SPAR   SECTION.                                       
308800                                                                          
308900     SUBTRACT SPAR-VKORDNTO FROM VORD-VKORDNTO                            
309000     COMPUTE VORD-VLORDNTO = VORD-VLORDNTO - SPAR-VLORDNTO                
309100                               / 1000000                                  
309200     END-COMPUTE                                                          
309300     SUBTRACT SPAR-SUORDV-LOC       FROM VORD-SUORDV-LOC                  
309400     SUBTRACT SPAR-SUORDV-LOCPREL FROM VORD-SUORDV-LOCPREL                
309500     SUBTRACT SPAR-SUORDV             FROM VORD-SUORDV                    
309600*                                                                         
309700     .                                                                    
309800     EJECT                                                                
309900 S12-GENERERA-AVVIKELSE-TRANS SECTION.                                    
310000                                                                          
310100     PERFORM S07-GET-IDSYS-KDORDKL                                        
310110     PERFORM S12A-UPPDATERA-RY1-POST                                      
310200     MOVE KORD-IDDISTR               TO TEST-IDDISTR                      
310300                                                                          
310400     IF NOT DIST19-SATS                                                   
310500        IF DCS-CDC                                                        
310600        OR DCS-NDC OR (DCS-SDC AND DCS-CHINA)                             
310700        OR SPAR-ORAD-FLFYSAVV = JA                                        
310800           PERFORM S12B-UPPDATERA-ORDBEK-WDQ1                             
310810           IF LYNK-NON-API                                                
310820             PERFORM S12G-EVENTS-NON-API                                  
310830           ELSE                                                           
310900*NEW                                                                      
311000*'LYND'   = DÖSKALLE WDQ2C                                                
311100             IF (OHUV-IDSYSTEM(1:4) = 'LYND' OR 'TADD')                   
311200               MOVE KORD-IDDISTR       TO   W-WDQ2CSEQ-IDDISTR            
311300               MOVE KORD-IDKUNDNR      TO   W-WDQ2CSEQ-IDKUNDNR           
311400               MOVE SPAR-ORAD-IDKUNDRF-RO(1:5)                            
311500                                       TO W-WDQ2CSEQ-IDKUNDRF(3:7)        
311600               PERFORM IMS-GU-ORQI01-CSEQ                                 
311700               IF SEGMENT-FINNS                                           
311800                 MOVE OHUV-IDORDER     TO  RYK-IDORDER                    
311900                                                                          
312000                 MOVE OHUV-IDDISTR     TO WS-IDDISTR-EVENT                
312100                 MOVE OHUV-IDKUNDNR    TO WS-IDKUNDNR-EVENT               
312200                 MOVE OHUV-IDORDNR7    TO WS-IDORDNR7-EVENT               
312300                 MOVE OHUV-TIREGDAT    TO WS-TIREGDAT-EVENT               
312400****             MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT               
312500                 MOVE JA               TO CREATE-EVENT-SW                 
312600               END-IF                                                     
312700             ELSE                                                         
312800*'LYNV'   = VOR KUNDRF-LEV -A6                                            
312900               IF (OHUV-IDSYSTEM(1:4) = 'LYNV' OR 'TADV')                 
313000                 MOVE LOW-VALUE        TO W-WDA6BSEQ-MIN-X                
313100                 MOVE HIGH-VALUE       TO W-WDA6BSEQ-MAX-X                
313200                                                                          
313300                 MOVE OHUV-IDDISTR     TO W-A6BSEQ-MIN-IDDISTR            
313400                                          W-A6BSEQ-MAX-IDDISTR            
313500                 MOVE OHUV-IDKUNDNR    TO W-A6BSEQ-MIN-IDKUNDNR           
313600                                          W-A6BSEQ-MAX-IDKUNDNR           
313700                 MOVE OHUV-IDKUNDRF   TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
313800                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
313900                 PERFORM IMS-GU-SEQB-WDA601                               
314000                 IF SEGMENT-FINNS                                         
314100                   MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT                
314200                   MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT               
314300                   MOVE VOR-IDORDNR7   TO WS-IDORDNR7-EVENT               
314400                   MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT            
314500                   MOVE JA             TO CREATE-EVENT-SW                 
314600                 END-IF                                                   
314700               ELSE                                                       
314800*                                                                         
314900*'LYNB'   = VERKSTADS/REPARATIONS-ORDER -A5                               
315000                 IF (OHUV-IDSYSTEM(1:4) = 'LYNB' OR 'TADB')               
315200                   MOVE OHUV-IDDISTR     TO W-IDDISTR-A5-MIN              
315300                                            W-IDDISTR-A5-MAX              
315400                   MOVE OHUV-IDKUNDNR    TO W-IDKUNDNR-A5-MIN             
315500                                            W-IDKUNDNR-A5-MAX             
315600                   MOVE OHUV-KDORDKL     TO W-KDORDKL                     
315700                   MOVE OHUV-IDORDNR7(3:5) TO W-IDKUNDRF-LEV              
315800                                                                          
315900                   PERFORM IMS-GU-WDA501                                  
316000                   IF SEGMENT-FINNS                                       
316100                     MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT              
316200                     MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT             
316300                     MOVE RAD-IDORDNR5   TO WS-IDORDNR7-EVENT             
316400                     MOVE RAD-TIREGDAT   TO WS-TIREGDAT-EVENT             
316500                     MOVE JA             TO CREATE-EVENT-SW               
316600                   END-IF                                                 
316700                 ELSE                                                     
316800                   MOVE KORD-IDDISTR     TO WS-IDDISTR-EVENT              
316900                   MOVE KORD-IDKUNDNR    TO WS-IDKUNDNR-EVENT             
317000                   MOVE KORD-IDORDNR7(1:5)                                
317010                                         TO WS-IDORDNR7-EVENT(3:5)        
317100                   MOVE ZERO             TO WS-IDORDNR7-EVENT(1:2)        
317200                   MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT             
317300                   MOVE JA               TO CREATE-EVENT-SW               
317400                 END-IF                                                   
317500               END-IF                                                     
317600             END-IF                                                       
317610           END-IF                                                         
317700                                                                          
317800           IF CREATE-EVENT                                                
317900             PERFORM S12F-CREATE-EVENT-HANDLING-152                       
318000             MOVE NEJ             TO CREATE-EVENT-SW                      
318100           END-IF                                                         
318200                                                                          
318300           IF  SPAR-ORAD-KDORDKL = 0                                      
318400           AND SPAR-ORAD-FLSDCLEV NOT = JA                                
318500              IF DCS-NDC OR (DCS-SDC AND DCS-CHINA)                       
318600                  PERFORM S12C-UPPDATERA-VOR-KOEN                         
318700              ELSE                                                        
318800                  PERFORM S12D-UPDATE-VORKONY                             
318900              END-IF                                                      
319000              PERFORM S25-DELETE-PRICE-Q-LINE                             
319100           END-IF                                                         
319200        END-IF                                                            
319300     END-IF                                                               
319400                                                                          
319500     IF WS-KDORDBEK              = 90 OR                                  
319600       (WS-KDORDBEK              = 91 AND                                 
319700        SW-TIRODAT-LIKA-MED-ZERO = JA)                                    
319800       PERFORM S12E-SKAPA-RYK-TRANS                                       
319900     END-IF                                                               
320000     .                                                                    
320100     EJECT                                                                
320200 S12A-UPPDATERA-RY1-POST                 SECTION.                         
320300                                                                          
320400     IF LOGG-IDLOGLOP = 9                                                 
320500       MOVE ZERO                          TO   LOGG-IDLOGLOP              
320600     END-IF                                                               
320700     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
320800     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
320900     ADD +1                               TO   LOGG-IDLOGLOP              
321000     MOVE 'RY1'                           TO   RY1-IDPTYP                 
321100                                               LOGG-IDPTYP                
321200     MOVE SPAR-ORAD-BERADREF              TO   RY1-BERADREF               
321300     MOVE SPAR-ORAD-BEVOLREF              TO   RY1-BEVOLREF               
321400     MOVE WS-IDKUNDRF                     TO   RY1-IDKUNDRF               
321500     MOVE SPAR-ORAD-IDARTNR               TO   RY1-IDARTNR                
321600     MOVE SPAR-ORAD-FLRESTN               TO   RY1-FLRESTN                
321700     MOVE SPAR-ORAD-FLDIRLEV              TO   RY1-FLDIRLEV               
321800     MOVE SPAR-ORAD-IDKUNDRF-RO           TO   RY1-IDKUNDRF-RO            
321900     MOVE SPAR-ORAD-FLTILLK               TO   RY1-FLTILLK                
322000                                                                          
322100     IF SPAR-ORAD-KDDSP = 0                                               
322200        MOVE 1                            TO  RY1-KDDSP                   
322300     ELSE                                                                 
322400        MOVE SPAR-ORAD-KDDSP              TO  RY1-KDDSP                   
322500     END-IF                                                               
322600     MOVE WS-KDFAKTYP                     TO  RY1-KDFAKTYP                
322700                                                                          
322800     IF   WS-KDORDBEK = 90 AND WS-KDTPOTYP =  6                           
322900       MOVE 91                            TO  RY1-KDORDBEK                
323000     ELSE                                                                 
323100       MOVE WS-KDORDBEK                   TO  RY1-KDORDBEK                
323200     END-IF                                                               
323300                                                                          
323400     MOVE SPAR-ORAD-KDORDING              TO  RY1-KDORDING                
323500     MOVE SPAR-ORAD-KDORDTYP              TO  RY1-KDORDTYP                
323600     MOVE SPAR-ORAD-KDKVBRYT              TO  RY1-KDKVBRYT                
323700     MOVE WS-KDTPOTYP                     TO  RY1-KDTPOTYP                
323800     MOVE SPAR-ORAD-KDVRINFO              TO  RY1-KDVRINFO                
323900     MOVE SPAR-ORAD-KVBEART               TO  RY1-KVBEART                 
324000     MOVE SPAR-ORAD-KVAVBART              TO  RY1-KVAVBART                
324100     IF DCS-CDC                                                           
324200       MOVE WS-KVORAPP-TOTAL              TO  RY1-KVAVART                 
324300     ELSE                                                                 
324400       MOVE WS-KVORAPP-PACK               TO  RY1-KVAVART                 
324500     END-IF                                                               
324600     MOVE WS-KVORAPP-TOTAL                TO  RY1-KVAVART                 
324700     MOVE SPAR-ORAD-KVLEVART              TO  RY1-KVLEVART                
324800     MOVE SPAR-ORAD-REKSIFFR              TO  RY1-REKSIFFR                
324900                                                                          
325000     MOVE SPAR-ORAD-IDARTNR               TO  W-IDARTNR                   
325100                                                                          
325200     PERFORM IMS-GU-ARTC11                                                
325300                                                                          
325400     MOVE DCS-IDDC TO W-711-IDDC                                          
325500*                                                                         
325600     MOVE CLAG-IDANSK         TO WS-IDANSK                                
325700     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                             
325800       PERFORM IMS-GU-WDK722                                              
325900       IF SEGMENT-FINNS AND XLAG-IDANSK > 0                               
326000          MOVE XLAG-IDANSK TO WS-IDANSK                                   
326100       END-IF                                                             
326200     END-IF                                                               
326300*                                                                         
326400     MOVE CLAG-TIDISPIN                   TO  RY1-TIDISPIN                
326500                                                                          
326600     MOVE SPAR-ORAD-TIUTSKR               TO  RY1-TIORDREG                
326700     MOVE SPAR-ORAD-TIRODAT               TO  RY1-TIRODAT                 
326800     MOVE WS-IDDISTR                      TO  RY1S-IDDISTR                
326900     MOVE WS-IDDC                         TO  RY1S-IDDC                   
327000     MOVE WS-IDKUNDNR                     TO  RY1S-IDKUNDNR               
327100     IF  OHUV-FLVORKO = JA                                                
327200     OR  OHUV-FLVORKO = YES                                               
327300         MOVE JA                          TO  RY1S-FLVORKO                
327400     ELSE                                                                 
327500         MOVE OHUV-FLVORKO                TO  RY1S-FLVORKO                
327600     END-IF                                                               
327700     MOVE OHUV-FLFORBI                    TO  RY1S-FLFORBI                
327800     MOVE OHUV-FLOVRLEV                   TO  RY1S-FLOVRLEV               
327900     MOVE SPAR-ORAD-KDPRODSL              TO  RY1S-KDPRODSL               
328000     MOVE SPAR-ORAD-IDSYSTEM              TO  RY1S-IDSYSTEM               
328100     MOVE WS-FLLSBOK                      TO  RY1S-FLLSBOK                
328200     MOVE WS-FLORDSPE                     TO  RY1S-FLORDSPE               
328300     MOVE SPAR-ORAD-KDFRAKT               TO  RY1S-KDFRAKT                
328400     MOVE SPAR-ORAD-KDORDKL               TO  RY1S-KDORDKL                
328500     MOVE SPAR-ORAD-KVANNANT              TO  RY1S-KVANNANT               
328600     MOVE SPAR-ORAD-KVSLATT               TO  RY1S-KVSLATT                
328700                                                                          
328800     MOVE RY1S-WDGZRY1S                   TO  LOGG-SORTPOST               
328900     MOVE RY1-WDGZRY1                     TO  LOGG-LOGGPOST               
329000*                                                                         
329100     PERFORM IMS-ISRT-AVVIKELSE                                           
329200*                                                                         
329300     PERFORM UNTIL SEGMENT-FINNS                                          
329400       IF LOGG-IDLOGLOP = 9                                               
329500         MOVE ZERO              TO LOGG-IDLOGLOP                          
329600         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
329700       END-IF                                                             
329800       ADD +1                   TO LOGG-IDLOGLOP                          
329900       PERFORM IMS-ISRT-AVVIKELSE                                         
330000     END-PERFORM                                                          
330100     .                                                                    
330200     EJECT                                                                
330300 S12B-UPPDATERA-ORDBEK-WDQ1              SECTION.                         
330400                                                                          
330500     MOVE KORD-IDORDER                   TO OBKR-IDORDER                  
330600     MOVE SPAR-ORAD-IDARTNR              TO OBKR-IDARTNR                  
330700*--- LÄS FRAM TILL FÖRSTA LEDIGA LÖPNR                                    
330800     MOVE OBKR-IDORDER                   TO W-IDORDER-Q1-MIN              
330900                                            W-IDORDER-Q1-MAX              
331000     MOVE OBKR-IDARTNR                   TO W-IDARTNR-Q1-MIN              
331100                                            W-IDARTNR-Q1-MAX              
331200     MOVE +1                             TO W-IDLOPNR-Q1-MIN              
331300                                            W-IDLOPNR-Q1-MAX              
331400                                            W-IDSEKVNR-Q1-MIN             
331500                                            W-IDSEKVNR-Q1-MAX             
331600     PERFORM IMS-GU-ORQM01                                                
331700     PERFORM UNTIL SEGMENT-SAKNAS                                         
331800        ADD +1                           TO W-IDLOPNR-Q1-MIN              
331900                                            W-IDLOPNR-Q1-MAX              
332000        PERFORM IMS-GU-ORQM01                                             
332100     END-PERFORM                                                          
332200     MOVE W-IDLOPNR-Q1-MIN               TO OBKR-IDLOPNR                  
332300     MOVE 1                              TO OBKR-IDSEKVNR                 
332400     MOVE WS-IDDC                        TO OBKR-IDDC                     
332500     MOVE SPAR-ORAD-IDDC-RO              TO OBKR-IDDC-RO                  
332600     MOVE SPAR-ORAD-KDOI                 TO OBKR-KDOI                     
332700     MOVE SPAR-ORAD-CLEARGROUP           TO OBKR-CLEARGROUP               
332800     MOVE WS-KDORDBEK                    TO OBKR-KDORDBEK                 
332900     MOVE IDPGM                          TO OBKR-IDPGM                    
333000     MOVE SPACE                          TO OBKR-BEERS                    
333100     MOVE WS-OHUV-BEKUNDRF               TO OBKR-BEKUNDRF                 
333200     MOVE SPAR-ORAD-BERADREF             TO OBKR-BERADREF                 
333300     MOVE SPAR-ORAD-BEVOLREF             TO OBKR-BEVOLREF                 
333400     MOVE SPAR-ORAD-IDKAMPRF             TO OBKR-IDKAMPRF                 
333500     MOVE 0                              TO OBKR-DIERS-KVOT               
333600     MOVE NEJ                            TO OBKR-FLAKPLOC                 
333700     MOVE NEJ                            TO OBKR-FLSLATT                  
333800     MOVE SPAR-ORAD-FLINVEST             TO OBKR-FLINVEST                 
333900     MOVE JA                             TO OBKR-FLOBOK                   
334000     MOVE NEJ                            TO OBKR-FLOBTRAN                 
334100     MOVE NEJ                            TO OBKR-FLOBPRT                  
334200     MOVE SPAR-ORAD-FLPRTILL             TO OBKR-FLPRTILL                 
334300     MOVE SPAR-ORAD-FLRESTN              TO OBKR-FLRESTN                  
334400     MOVE NEJ                            TO OBKR-FLTILLK                  
334500     MOVE 0                              TO OBKR-IDARTNR-TILLK            
334600     MOVE KORD-IDDISTR                   TO OBKR-IDDISTR                  
334700     MOVE KORD-IDKUNDNR                  TO OBKR-IDKUNDNR                 
334800                                                                          
334900     MOVE KORD-IDKUNDRF                  TO WS-IDKUNDRF-OLD               
335000     MOVE WS-IDORDNR5-OLD                TO WS-IDORDNR7-NEW               
335100     MOVE WS-IDKUNDRF-NEW                TO OBKR-IDKUNDRF                 
335200                                                                          
335300     MOVE SPAR-ORAD-IDKUNDRF-RO          TO WS-IDKUNDRF-OLD               
335400     MOVE WS-IDORDNR5-OLD                TO WS-IDORDNR7-NEW               
335500     MOVE WS-IDKUNDRF-NEW                TO OBKR-IDKUNDRF-RO              
335600                                                                          
335700     MOVE SPAR-ORAD-IDLEVNR              TO OBKR-IDLEVNR                  
335800     MOVE SPAR-ORAD-IDLOPNR-RO           TO OBKR-IDLOPNR-RO               
335900     MOVE SPAR-ORAD-IDSYSTEM             TO OBKR-IDSYSTEM                 
336000     MOVE SPAR-ORAD-KDDSP                TO OBKR-KDDSP                    
336100                                                                          
336200     MOVE 0                              TO OBKR-KDERS                    
336300     MOVE SPAR-ORAD-KDKVBRYT             TO OBKR-KDKVBRYT                 
336400     MOVE SPAR-ORAD-KDPRTYP              TO OBKR-KDPRTYP                  
336500     MOVE 0                              TO OBKR-KDTPOTYP                 
336600     MOVE SPAR-ORAD-KDVRINFO             TO OBKR-KDVRINFO                 
336700                                                                          
336800     EVALUATE TRUE                                                        
336900       WHEN WS-KDORDBEK = 90 OR 91                                        
337000         MOVE 0                          TO OBKR-KVANNANT                 
337100       WHEN WS-KDORDBEK = 80 OR 81 OR 93                                  
337200         IF DCS-CDC OR DCS-NDC                                            
337300           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVBEART                      
337400                                 - SPAR-ORAD-KVLEVART                     
337500           END-COMPUTE                                                    
337600         ELSE                                                             
337700           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVAVBART                     
337800                                 - SPAR-ORAD-KVLEVART                     
337900           END-COMPUTE                                                    
338000         END-IF                                                           
338100       WHEN OTHER                                                         
338200         MOVE 'FELAKTIG ORDERBEKR-KOD'   TO RKOD-FELTEXT                  
338300         CALL ABEND  USING RKOD-ABEND-MED-DUMP                            
338400     END-EVALUATE                                                         
338500                                                                          
338600     MOVE SPAR-ORAD-KVAVBART             TO OBKR-KVAVBART                 
338700     MOVE SPAR-ORAD-KVBEART              TO OBKR-KVBEART                  
338800     MOVE SPAR-ORAD-KVBEART              TO OBKR-KVBEART-Q                
338900     MOVE 0                              TO OBKR-KVBEART-TILLK            
339000                                                                          
339100     MOVE 0                              TO OBKR-KVPREAVB                 
339200     MOVE 0                              TO OBKR-KVPRERO                  
339300     MOVE CLAG-KVQPACK-1                 TO OBKR-KVQPACK                  
339400                                                                          
339500     IF WS-KDORDBEK = 90 OR 91                                            
339600       IF DCS-CDC OR DCS-NDC                                              
339700         MOVE WS-KVORAPP-TOTAL             TO OBKR-KVRO                   
339800       ELSE                                                               
339900         MOVE WS-KVORAPP-PACK              TO OBKR-KVRO                   
340000       END-IF                                                             
340100       MOVE WS-DAGENS-DATUM              TO OBKR-TIRODAT                  
340200     ELSE                                                                 
340300       MOVE 0                            TO OBKR-KVRO                     
340400       MOVE 000000                       TO OBKR-TIRODAT                  
340500     END-IF                                                               
340600                                                                          
340700     MOVE SPAR-ORAD-KVSLATT              TO OBKR-KVSLATT                  
340800                                                                          
340900     MOVE SPAR-ORAD-PRARTNTO             TO OBKR-PRARTNTO                 
341000     MOVE SPAR-ORAD-PRARTNTO-LOC         TO OBKR-PRARTNTO-LOC             
341100     MOVE SPAR-ORAD-PRARTNTO-LOCPREL     TO OBKR-PRARTNTO-LOCPREL         
341200     MOVE 0                              TO OBKR-PRBPRIS                  
341300     MOVE SPAR-ORAD-REKSIFFR             TO OBKR-REKSIFFR                 
341400     MOVE 0                              TO OBKR-REKSIFFR-TILLK           
341500     MOVE 0                              TO OBKR-RERF-RAD                 
341600                                                                          
341700     MOVE WS-TIDISPIN                    TO OBKR-TIDISPIN                 
341800     MOVE KORD-TIORDREG                  TO OBKR-TIORDREG                 
341900                                                                          
342000     MOVE ZERO                            TO WS-DAORDREG                  
342100     MOVE FUNCTION CURRENT-DATE(1:2)      TO WS-DAORDREG-TISS             
342200     MOVE OBKR-TIORDREG                   TO WS-DAORDREG-TIAAMMDD         
342300     COMPUTE OBKR-TITIORDD-9KOMPL =                                       
342400             WS-9KOMPL-GRUND - WS-DAORDREG                                
342500                                                                          
342600     MOVE SPAR-ORAD-TIPRIS               TO OBKR-TIPRIS                   
342700     MOVE WS-DAGENS-DATUM                TO OBKR-TIREGDAT                 
342800     MOVE   WS-TTMMSS                    TO OBKR-TIREGTID                 
342900                                                                          
343000     IF WS-KDORDBEK = 90 OR 91                                            
343100        MOVE ZERO                         TO WS-DARODAT                   
343200        MOVE FUNCTION CURRENT-DATE(1:2)   TO WS-DARODAT-TISS              
343300        MOVE OBKR-TIRODAT                 TO WS-DARODAT-TIAAMMDD          
343400        COMPUTE OBKR-TITIREGD-9KOMPL =                                    
343500             WS-9KOMPL-GRUND - WS-DARODAT                                 
343600     ELSE                                                                 
343700        MOVE 0                           TO OBKR-TITIREGD-9KOMPL          
343800     END-IF                                                               
343900                                                                          
344000     MOVE 0                              TO OBKR-TITPO                    
344100     MOVE SPAR-ORAD-KDFRAKT              TO OBKR-KDFRAKT                  
344200     MOVE SPAR-ORAD-KDORDKL              TO OBKR-KDORDKL                  
344300     MOVE SPACE                          TO OBKR-IDBIL                    
344400     MOVE SPACE                         TO OBKR-KDORDTYP-LDC              
344500     MOVE SPAR-ORAD-IDKUNDRF-WIP        TO OBKR-IDKUNDRF-WIP              
344600     MOVE ZERO                          TO OBKR-TIREPDAT                  
344700     MOVE SPAR-ORAD-IDPRQUES             TO OBKR-IDPRQUES                 
344800     MOVE SPAR-ORAD-PRARTBTO-LOC         TO OBKR-PRARTBTO-LOC             
344900     IF SPAR-ORAD-PRAVCOST > ZERO                                         
345000       MOVE SPAR-ORAD-KDVALISO-EXP       TO OBKR-KDVALISO                 
345100     ELSE                                                                 
345200       MOVE SPAR-ORAD-KDVALISO           TO OBKR-KDVALISO                 
345300     END-IF                                                               
345400     MOVE SPAR-ORAD-KDVAT                TO OBKR-KDVAT                    
345500     MOVE SPAR-ORAD-RERAB                TO OBKR-RERAB                    
345600     MOVE SPAR-ORAD-KDRAB                TO OBKR-KDRAB                    
345700     MOVE SPAR-ORAD-BEART-VIPS           TO OBKR-BEART-VIPS               
345800     MOVE ZERO                           TO OBKR-TIDLEVDAT                
345900     MOVE SPAR-ORAD-PRAVCOST             TO OBKR-PRAVCOST                 
346000                                                                          
346100     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
346200        MOVE OBKR-IDARTNR         TO W-IDARTNR                            
346300        IF OBKR-IDDC NOT = DCS-IDDC                                       
346400           MOVE OBKR-IDDC             TO W-IDDC-B6                        
346500           PERFORM IMS-GU-WDB601                                          
346600        END-IF                                                            
346700        MOVE DCS-IDLANDX2         TO W-IDLAND                             
346800        PERFORM IMS-GU-WDK712                                             
346900        IF SEGMENT-FINNS AND LART-FLREFERAL = JA                          
347000           MOVE 98                TO OBKR-KDORDBEK                        
347100        END-IF                                                            
347200     END-IF                                                               
347300                                                                          
347400     PERFORM IMS-ISRT-ORQM01                                              
347500     .                                                                    
347600     EJECT                                                                
347700 S12C-UPPDATERA-VOR-KOEN                 SECTION.                         
347800                                                                          
347900     IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                           
348000        MOVE SPAR-ORAD-BERADREF       TO 4542-BERADREF                    
348100        MOVE KORD-IDDISTR             TO 4542-IDDISTR                     
348200        MOVE WS-IDANSK                TO 4542-IDANSK                      
348300        MOVE SPAR-ORAD-IDARTNR        TO 4542-IDARTNR                     
348400        MOVE KORD-IDKUNDNR            TO 4542-IDKUNDNR                    
348500        MOVE OBKR-IDDC                TO 4542-IDDC                        
348600                                                                          
348700        MOVE KORD-IDKUNDRF            TO WS-IDKUNDRF-OLD                  
348800        MOVE WS-IDORDNR5-OLD          TO WS-IDORDNR7-NEW                  
348900        MOVE WS-IDKUNDRF-NEW          TO 4542-IDKUNDRF                    
349000        MOVE KORD-IDORDER             TO 4542-IDORDER                     
349100        MOVE SPACE                    TO 4542-IDUSER                      
349200        MOVE 93                       TO 4542-KDORDBEK                    
349300        MOVE SPAR-ORAD-KDPRTYP        TO 4542-KDPRTYP                     
349400        MOVE 0                        TO 4542-KDVORATG                    
349500        MOVE SPAR-ORAD-KVBEART        TO 4542-KVBEART                     
349600        MOVE SPAR-ORAD-KVBEART        TO 4542-KVBEART-Q                   
349700                                                                          
349800        IF SPAR-ORAD-FLFYSAVV = JA                                        
349900          IF DCS-SDC AND DCS-CHINA                                        
350000            MOVE ZERO                 TO WS-AVVIKELSE-UTSKR               
350100            COMPUTE WS-AVVIKELSE-UTSKR =                                  
350200                    SPAR-ORAD-KVBEART - SPAR-ORAD-KVAVBART                
350300            END-COMPUTE                                                   
350400                                                                          
350500            COMPUTE WS-SUMMA =                                            
350600                    SPAR-ORAD-KVLEVART + WS-AVVIKELSE-UTSKR               
350700            END-COMPUTE                                                   
350800            MOVE WS-SUMMA                  TO 4542-KVPREAVB               
350900          ELSE                                                            
351000            MOVE SPAR-ORAD-KVLEVART        TO 4542-KVPREAVB               
351100          END-IF                                                          
351200*         WS-SUMMA MINUS ORAD-KVBEART ÄR AVVIKELSE I PACKNINGEN           
351300*         VILKET ÄR DET SOM VISAS (RÄKNAS FRAM) PÅ 4224-BILDEN            
351400        ELSE                                                              
351500          MOVE SPAR-ORAD-KVLEVART          TO 4542-KVPREAVB               
351600        END-IF                                                            
351700                                                                          
351800        MOVE SPAR-ORAD-PRARTNTO       TO 4542-PRARTNTO                    
351900        MOVE SPAR-ORAD-PRARTNTO-LOC   TO 4542-PRARTNTO-LOC                
352000        MOVE SPAR-ORAD-PRARTNTO-LOCPREL  TO 4542-PRARTNTO-LOCPREL         
352100        MOVE SPACE                    TO 4542-TEVORMRK                    
352200        MOVE SPAR-ORAD-IDLEVNR        TO 4542-IDLEVNR                     
352300        MOVE WS-DAGENS-DATUM          TO 4542-TIREGDAT                    
352400        MOVE WS-TTMMSS                TO 4542-TIREGTID                    
352500        MOVE 0                        TO 4542-TIUPPDAT                    
352600        MOVE 0                        TO 4542-TIUPPTID                    
352700        MOVE 1                        TO 4542-IDLOPNR                     
352800        MOVE SPAR-ORAD-IDPRQUES       TO 4542-IDPRQUES                    
352900        MOVE SPAR-ORAD-PRARTBTO-LOC   TO 4542-PRARTBTO-LOC                
353000        IF SPAR-ORAD-PRAVCOST > ZERO                                      
353100          MOVE SPAR-ORAD-KDVALISO-EXP TO 4542-KDVALISO                    
353200        ELSE                                                              
353300          MOVE SPAR-ORAD-KDVALISO     TO 4542-KDVALISO                    
353400        END-IF                                                            
353500        MOVE SPAR-ORAD-KDVAT          TO 4542-KDVAT                       
353600        MOVE SPAR-ORAD-RERAB          TO 4542-RERAB                       
353700        MOVE SPAR-ORAD-KDRAB          TO 4542-KDRAB                       
353800        MOVE SPAR-ORAD-BEART-VIPS     TO 4542-BEART-VIPS                  
353900                                                                          
354000        PERFORM IMS-ISRT-4542                                             
354100        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
354200          ADD +1                      TO 4542-IDLOPNR                     
354300          PERFORM IMS-ISRT-4542                                           
354400        END-PERFORM                                                       
354500                                                                          
354600        IF  DCS-NDC-CN                                                    
354700        OR (DCS-NDC-NA AND DCS-USA)                                       
354800          IF SLAG-IDDC-REF = SPACE                                        
354900            MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                       
355000            MOVE WS-IDDC               TO W-711-IDDC                      
355100            MOVE WS-IDDISTR            TO S27-IDDISTR                     
355200            MOVE WS-IDKUNDNR           TO S27-IDKUNDNR                    
355300            MOVE SPAR-ORAD-IDARTNR     TO S27-IDARTNR                     
355400*                                                                         
355500            MOVE CLAG-IDANSK           TO S27-IDANSK                      
355600            PERFORM IMS-GU-WDK722                                         
355700            IF SEGMENT-FINNS AND XLAG-IDANSK > 0                          
355800              MOVE XLAG-IDANSK         TO S27-IDANSK                      
355900            END-IF                                                        
356000*                                                                         
356100            MOVE WS-IDDC               TO S27-IDDC                        
356200            MOVE SLAG-IDLEVNR          TO S27-IDLEVNR                     
356300            PERFORM S27-STARTA-W2T191X                                    
356400          END-IF                                                          
356500        END-IF                                                            
356600                                                                          
356700        IF SPAR-ORAD-IDLEVNR = SPACE AND                                  
356800           WS-FLORDSPE = NEJ         AND                                  
356900           WS-FLOVRLEV = NEJ                                              
357000           IF  DCS-NDC                                                    
357100           OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                       
357200             MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                      
357300             MOVE WS-IDDC               TO W-711-IDDC                     
357400             PERFORM IMS-GHU-WDK711                                       
357500                                                                          
357600             IF  DCS-NDC                                                  
357700               COMPUTE SLAG-KVOKS-DAG =                                   
357800                       SLAG-KVOKS-DAG +                                   
357900                       (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)           
358000               END-COMPUTE                                                
358100             ELSE                                                         
358200               COMPUTE SLAG-KVOKS-DAG =                                   
358300                       SLAG-KVOKS-DAG +                                   
358400                       (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)          
358500               END-COMPUTE                                                
358600             END-IF                                                       
358700                                                                          
358800             PERFORM IMS-REPL-WDK7                                        
358900           END-IF                                                         
359000        END-IF                                                            
359100     END-IF                                                               
359200     .                                                                    
359300     EJECT                                                                
359400 S12D-UPDATE-VORKONY          SECTION.                                    
359500                                                                          
359600     MOVE SPAR-ORAD-IDARTNR TO S28-IDARTNR                                
359700     MOVE DCS-IDDC          TO S28-IDDC                                   
359800     PERFORM S28-BESTAM-LENVR-ANSK                                        
359900                                                                          
360000*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
360100                                                                          
360200     PERFORM S26-SOK-RAD-VORKO                                            
360300                                                                          
360400     IF  TRAFF-VORKO                                                      
360500         IF DCS-CDC OR DCS-CDC-TR                                         
360600             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
360700                                   - SPAR-ORAD-KVBEART                    
360800                                   + SPAR-ORAD-KVLEVART                   
360900             END-COMPUTE                                                  
361000             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
361100                                   - SPAR-ORAD-KVBEART                    
361200                                   + SPAR-ORAD-KVLEVART                   
361300             END-COMPUTE                                                  
361400         ELSE                                                             
361500             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
361600                                   - SPAR-ORAD-KVAVBART                   
361700                                   + SPAR-ORAD-KVLEVART                   
361800             END-COMPUTE                                                  
361900             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
362000                                   - SPAR-ORAD-KVAVBART                   
362100                                   + SPAR-ORAD-KVLEVART                   
362200             END-COMPUTE                                                  
362300         END-IF                                                           
362400         IF  VOR-KVPREAVB = 0                                             
362500             MOVE '7'              TO VOR-KDVORATG                        
362600             MOVE 93               TO VOR-KDORDBEK                        
362700             IF VOR-TIKLAR = ZERO                                         
362800                MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                       
362900                COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                  
363000                                      / 100                               
363100                END-COMPUTE                                               
363200             END-IF                                                       
363300         END-IF                                                           
363400         PERFORM IMS-REPL-SEQB-WDA601                                     
363500                                                                          
363600         MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                       
363700         ADD +1                 TO WS-VOR-TID-BRIST                       
363800         MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                       
363900         SUBTRACT WS-DAGENS-DATUM FROM 9999999                            
364000                                  GIVING VOR-TIREGDAT-AVV9                
364100         SUBTRACT WS-VOR-TID-BRIST FROM 999999999                         
364200                                   GIVING VOR-TIREGTID-AVV9               
364300         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
364400         MOVE 0                 TO VOR-TIREGDAT-LEV                       
364500         MOVE 0                 TO VOR-TIREGTID-LEV                       
364600         IF DCS-CDC                                                       
364700             SUBTRACT SPAR-ORAD-KVLEVART                                  
364800                                  FROM SPAR-ORAD-KVBEART                  
364900                                  GIVING VOR-KVBEART                      
365000                                         VOR-KVBEART-Q                    
365100         ELSE                                                             
365200             SUBTRACT SPAR-ORAD-KVLEVART                                  
365300                                  FROM SPAR-ORAD-KVAVBART                 
365400                                  GIVING VOR-KVBEART                      
365500                                         VOR-KVBEART-Q                    
365600         END-IF                                                           
365700         MOVE 0                 TO VOR-KVPREAVB                           
365800         MOVE WS-IDDC           TO VOR-IDDC                               
365900         MOVE SPACE             TO VOR-IDUSER                             
366000         MOVE 93                TO VOR-KDORDBEK                           
366100         MOVE '0'               TO VOR-KDVORATG                           
366200         MOVE 0                 TO VOR-TIKLAR                             
366300         MOVE 0                 TO VOR-TIKLATID                           
366400                                                                          
366500         PERFORM IMS-ISRT-WDA601                                          
366600         PERFORM UNTIL ISRT-OK                                            
366700            ADD +1                TO WS-VOR-TID-BRIST                     
366800                                     VOR-TIREGTID-URSP                    
366900            MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                     
367000            SUBTRACT WS-VOR-TID-BRIST FROM 999999999                      
367100                                  GIVING VOR-TIREGTID-AVV9                
367200            PERFORM IMS-ISRT-WDA601                                       
367300         END-PERFORM                                                      
367400     ELSE                                                                 
367500*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
367600*                                        BORDE NOG INTE FÖREKOMMA         
367700       MOVE WS-IDDISTR          TO VOR-IDDISTR                            
367800       MOVE WS-IDKUNDNR         TO VOR-IDKUNDNR                           
367900       MOVE WS-IDKUNDRF         TO WS-IDKUNDRF-OLD                        
368000       MOVE WS-IDORDNR5-OLD     TO WS-IDORDNR7-NEW                        
368100       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF                           
368200       MOVE KORD-TIORDREG       TO VOR-TIREGDAT-URSP                      
368300       MOVE SPAR-ORAD-IDARTNR   TO VOR-IDARTNR                            
368400       ADD +1                   TO WS-VOR-TID-BRIST                       
368500       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
368600       MOVE 0                   TO VOR-TIREGDAT-AVV                       
368700       MOVE 0                   TO VOR-TIREGTID-AVV                       
368800       SUBTRACT 0            FROM 9999999                                 
368900                              GIVING VOR-TIREGDAT-AVV9                    
369000       SUBTRACT 0            FROM 999999999                               
369100                              GIVING VOR-TIREGTID-AVV9                    
369200       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF-LEV                       
369300       MOVE KORD-TIORDREG       TO VOR-TIREGDAT-LEV                       
369400       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
369500       MOVE S28-IDANSK          TO VOR-IDANSK                             
369600                                                                          
369700       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
369800       PERFORM IMS-GU-WDP4A1                                              
369900       IF SEGMENT-SAKNAS                                                  
370000          MOVE DEF-IDROLL  TO SEQA-IDROLL                                 
370100       END-IF                                                             
370200       MOVE SEQA-IDROLL         TO VOR-IDROLL                             
370300                                                                          
370400       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
370500       MOVE SPAR-ORAD-BERADREF  TO VOR-BERADREF                           
370600       IF  DCS-CDC OR DCS-CDC-TR                                          
370700           SUBTRACT SPAR-ORAD-KVLEVART                                    
370800                                FROM SPAR-ORAD-KVBEART                    
370900                                GIVING VOR-KVBEART-URSP                   
371000                                       VOR-KVBEART                        
371100       ELSE                                                               
371200           SUBTRACT SPAR-ORAD-KVLEVART                                    
371300                                FROM SPAR-ORAD-KVAVBART                   
371400                                GIVING VOR-KVBEART-URSP                   
371500                                       VOR-KVBEART                        
371600       END-IF                                                             
371700       MOVE 0                   TO VOR-KVPREAVB                           
371800                                   VOR-KVBEART-Q                          
371900       MOVE WS-IDDC             TO VOR-IDDC                               
372000       MOVE SPACE               TO VOR-IDUSER                             
372100       MOVE 93                  TO VOR-KDORDBEK                           
372200       MOVE SPAR-ORAD-KDPRTYP   TO VOR-KDPRTYP                            
372300       MOVE '7'                  TO VOR-KDVORATG                          
372400       MOVE SPAR-ORAD-PRARTNTO  TO VOR-PRARTNTO                           
372500       MOVE '  '                TO VOR-TEVORMRK                           
372600*      MOVE '  '                TO VOR-TEVORMRK-SC                        
372700       MOVE 0                   TO VOR-TIUPPDAT                           
372800       MOVE 0                   TO VOR-TIUPPTID                           
372900       MOVE WS-DAGENS-DATUM     TO VOR-TIKLAR                             
373000       COMPUTE VOR-TIKLATID     = WS-VOR-TID-BRIST                        
373100                                / 100                                     
373200       END-COMPUTE                                                        
373300       MOVE SPAR-ORAD-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                    
373400       MOVE NEJ                 TO VOR-FLVORFK                            
373500       PERFORM IMS-ISRT-WDA601                                            
373600       PERFORM UNTIL ISRT-OK                                              
373700          ADD +1              TO VOR-TIREGTID-URSP                        
373800          ADD +1              TO VOR-TIREGTID-LEV                         
373900          PERFORM IMS-ISRT-WDA601                                         
374000       END-PERFORM                                                        
374100                                                                          
374200*--------------------------------------- EJ TRÄFF, AVVIK RAD              
374300       MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                         
374400       ADD +1                 TO WS-VOR-TID-BRIST                         
374500       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
374600       SUBTRACT WS-DAGENS-DATUM  FROM 9999999                             
374700                                 GIVING VOR-TIREGDAT-AVV9                 
374800       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
374900                                 GIVING VOR-TIREGTID-AVV9                 
375000       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
375100       MOVE 0                 TO VOR-TIREGDAT-LEV                         
375200       MOVE 0                 TO VOR-TIREGTID-LEV                         
375300       IF  DCS-CDC OR DCS-CDC-TR                                          
375400           SUBTRACT SPAR-ORAD-KVLEVART                                    
375500                                FROM SPAR-ORAD-KVBEART                    
375600                                GIVING VOR-KVBEART-Q                      
375700       ELSE                                                               
375800           SUBTRACT SPAR-ORAD-KVLEVART                                    
375900                                FROM SPAR-ORAD-KVAVBART                   
376000                                GIVING VOR-KVBEART-Q                      
376100       END-IF                                                             
376200       MOVE 0                 TO VOR-KVPREAVB                             
376300       MOVE 93                TO VOR-KDORDBEK                             
376400       MOVE '0'               TO VOR-KDVORATG                             
376500       MOVE 0                 TO VOR-TIKLAR                               
376600       MOVE 0                 TO VOR-TIKLATID                             
376700                                                                          
376800       PERFORM IMS-ISRT-WDA601                                            
376900       PERFORM UNTIL ISRT-OK                                              
377000          ADD +1                TO WS-VOR-TID-BRIST                       
377100          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
377200          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
377300                                GIVING VOR-TIREGTID-AVV9                  
377400          PERFORM IMS-ISRT-WDA601                                         
377500       END-PERFORM                                                        
377600     END-IF                                                               
377700                                                                          
377800     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
377900                                + VOR-KVBEART-Q                           
378000                                - VOR-KVPREAVB                            
378100     END-COMPUTE                                                          
378200                                                                          
378300     PERFORM IMS-REPL-WDK611                                              
378400                                                                          
378500     MOVE WS-IDDISTR            TO S27-IDDISTR                            
378600     MOVE WS-IDKUNDNR           TO S27-IDKUNDNR                           
378700     MOVE SPAR-ORAD-IDARTNR     TO S27-IDARTNR                            
378800     MOVE S28-IDANSK            TO S27-IDANSK                             
378900     MOVE WS-IDDC               TO S27-IDDC                               
379000     MOVE SPACE                 TO S27-IDLEVNR                            
379100     PERFORM S27-STARTA-W2T191X                                           
379200                                                                          
379300*------------------------------SAMMA EFTERHANTERING SOM I S12C            
379400     IF SPAR-ORAD-IDLEVNR = SPACE AND                                     
379500        WS-FLORDSPE = NEJ        AND                                      
379600        WS-FLOVRLEV = NEJ                                                 
379700        IF  DCS-NDC                                                       
379800        OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                          
379900          MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                         
380000          MOVE WS-IDDC               TO W-711-IDDC                        
380100          PERFORM IMS-GHU-WDK711                                          
380200                                                                          
380300          IF  DCS-NDC                                                     
380400            COMPUTE SLAG-KVOKS-DAG =                                      
380500                    SLAG-KVOKS-DAG +                                      
380600                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
380700            END-COMPUTE                                                   
380800          ELSE                                                            
380900            COMPUTE SLAG-KVOKS-DAG =                                      
381000                    SLAG-KVOKS-DAG +                                      
381100                   (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)              
381200            END-COMPUTE                                                   
381300          END-IF                                                          
381400                                                                          
381500          PERFORM IMS-REPL-WDK7                                           
381600        ELSE                                                              
381700          IF  DCS-CDC OR DCS-CDC-TR                                       
381800            MOVE SPAR-ORAD-IDARTNR        TO W-901-IDARTNR                
381900            PERFORM IMS-GHU-WDK901                                        
382000                                                                          
382100            COMPUTE ART-KVOKS-VOR =                                       
382200                    ART-KVOKS-VOR +                                       
382300                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
382400            END-COMPUTE                                                   
382500                                                                          
382600            PERFORM IMS-REPL-WDK901                                       
382700          END-IF                                                          
382800        END-IF                                                            
382900     END-IF                                                               
383000     .                                                                    
383100     EJECT                                                                
383200 S20-FINN-INTERVALL  SECTION.                                             
383300                                                                          
383400     PERFORM IMS-GU-WDM211                                                
383500     IF SEGMENT-FINNS                                                     
383600       PERFORM IMS-GNP-WDM221                                             
383700       PERFORM UNTIL SEGMENT-SAKNAS                                       
383800         IF  WS-IDDISTR > KMRK-IDDISTR-TOM                                
383900         OR  WS-IDDISTR < KMRK-IDDISTR-FOM                                
384000           CONTINUE                                                       
384100         ELSE                                                             
384200           IF  WS-IDDISTR  = KMRK-IDDISTR-TOM                             
384300           AND WS-IDKUNDNR > KMRK-IDKUNDNR-TOM                            
384400             CONTINUE                                                     
384500           ELSE                                                           
384600             IF  WS-IDDISTR  = KMRK-IDDISTR-FOM                           
384700             AND WS-IDKUNDNR < KMRK-IDKUNDNR-FOM                          
384800               CONTINUE                                                   
384900             ELSE                                                         
385000               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
385100               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
385200               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
385300               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
385400             END-IF                                                       
385500           END-IF                                                         
385600         END-IF                                                           
385700         PERFORM IMS-GNP-WDM221                                           
385800       END-PERFORM                                                        
385900     END-IF                                                               
386000     .                                                                    
386100     EJECT                                                                
386200 S26-SOK-RAD-VORKO SECTION.                                               
386300                                                                          
386400     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
386500     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
386600                                                                          
386700     MOVE WS-IDDISTR        TO W-A601KY-MIN-IDDISTR                       
386800                               W-A601KY-MAX-IDDISTR                       
386900     MOVE WS-IDKUNDNR       TO W-A601KY-MIN-IDKUNDNR                      
387000                               W-A601KY-MAX-IDKUNDNR                      
387100     MOVE WS-IDKUNDRF       TO WS-IDKUNDRF-OLD                            
387200     MOVE WS-IDORDNR5-OLD   TO WS-IDORDNR7-NEW                            
387300     MOVE WS-IDKUNDRF-NEW   TO W-A601KY-MIN-IDKUNDRF                      
387400                               W-A601KY-MAX-IDKUNDRF                      
387500     MOVE KORD-TIORDREG     TO W-A601KY-MIN-TIREGDAT                      
387600                               W-A601KY-MAX-TIREGDAT                      
387700     MOVE SPAR-ORAD-IDARTNR TO W-A601KY-MIN-IDARTNR                       
387800                               W-A601KY-MAX-IDARTNR                       
387900     MOVE NEJ               TO TRAFF-VORKO-SW                             
388000                                                                          
388100     PERFORM IMS-GHU-SEQB-WDA601                                          
388200     PERFORM UNTIL SEGMENT-SAKNAS                                         
388300                OR SEGMENT-SLUT                                           
388400                OR TRAFF-VORKO                                            
388500       IF  DCS-CDC                                                        
388600       AND SPAR-ORAD-KVBEART   = VOR-KVPREAVB                             
388700           MOVE JA       TO TRAFF-VORKO-SW                                
388800       ELSE                                                               
388900         IF  SPAR-ORAD-KVAVBART = VOR-KVPREAVB                            
389000             MOVE JA       TO TRAFF-VORKO-SW                              
389100         ELSE                                                             
389200            PERFORM IMS-GHN-SEQB-WDA601                                   
389300         END-IF                                                           
389400       END-IF                                                             
389500     END-PERFORM                                                          
389600     .                                                                    
389700     EJECT                                                                
389800 S27-STARTA-W2T191X  SECTION.                                             
389900                                                                          
390000     COMPUTE ALT2191-LL = LENGTH OF ALT2191-MID-W2I19101 + 17             
390100     MOVE +1                    TO ALT2191-MID-KDCLAGER                   
390200     MOVE S27-IDARTNR-X         TO ALT2191-MID-IDARTNR                    
390300     MOVE ZERO                  TO ALT2191-MID-TISENBEK-DAG               
390400                                   ALT2191-MID-TISENBEK-KL                
390500     MOVE SPACE                 TO ALT2191-MID-IDKR                       
390600     MOVE S27-IDANSK-X          TO ALT2191-MID-IDANSK                     
390700     MOVE '500'                 TO ALT2191-MID-KDLARM                     
390800     MOVE S27-IDDISTR-X         TO ALT2191-MID-IDDISTR                    
390900     MOVE S27-IDKUNDNR-X        TO ALT2191-MID-IDKUNDNR                   
391000     MOVE WS-IDKUNDRF           TO WS-IDKUNDRF-OLD                        
391100     MOVE WS-IDORDNR5-OLD       TO WS-IDORDNR7-NEW                        
391200     MOVE WS-IDKUNDRF-NEW       TO ALT2191-MID-IDKUNDRF                   
391300     MOVE 'J'                   TO ALT2191-MID-FLNYLARM                   
391400     MOVE S27-IDDC              TO ALT2191-MID-IDDC                       
391500     MOVE S27-IDLEVNR           TO ALT2191-MID-IDLEVNR                    
391600                                                                          
391700     PERFORM IMS-PURGE-ALT2191-MSG                                        
391800                                                                          
391900     MOVE SPACE                 TO ALT2191-MID-W2I19101                   
392000     .                                                                    
392100     EJECT                                                                
392200 S28-BESTAM-LENVR-ANSK SECTION.                                           
392300                                                                          
392400     MOVE S28-IDARTNR    TO W-IDARTNR                                     
392500     MOVE S28-IDDC       TO W-711-IDDC                                    
392600     PERFORM IMS-GU-WDK601                                                
392700     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
392800                                                                          
392900     PERFORM IMS-GHNP-WDK611                                              
393000     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
393100     .                                                                    
393200     SKIP2                                                                
393300 S12E-SKAPA-RYK-TRANS SECTION.                                            
393400                                                                          
393500     IF LOGG-IDLOGLOP = 9                                                 
393600       MOVE ZERO                TO   LOGG-IDLOGLOP                        
393700     END-IF                                                               
393800     ACCEPT  LOGG-TIAAMMDD      FROM DATE                                 
393900     ACCEPT  LOGG-TIKLOCK       FROM TIME                                 
394000     ADD     +1                 TO   LOGG-IDLOGLOP                        
394100                                                                          
394200     MOVE    'RYK'              TO   RYK-IDPTYP                           
394300                                     LOGG-IDPTYP                          
394400                                                                          
394500     MOVE    KORD-IDDISTR       TO   RYK-IDDISTR                          
394600     MOVE    KORD-IDKUNDNR      TO   RYK-IDKUNDNR                         
394700                                                                          
394800     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
394900       MOVE  KORD-IDORDER       TO   RYK-IDORDER                          
395000     ELSE                                                                 
395100       MOVE  KORD-IDDISTR       TO   W-WDQ2CSEQ-IDDISTR                   
395200       MOVE  KORD-IDKUNDNR      TO   W-WDQ2CSEQ-IDKUNDNR                  
395300       MOVE  SPAR-ORAD-IDKUNDRF-RO(1:5)                                   
395400                                TO   W-WDQ2CSEQ-IDKUNDRF(3:7)             
395500       PERFORM IMS-GU-ORQI01-CSEQ                                         
395600       IF SEGMENT-FINNS                                                   
395700         MOVE OHUV-IDORDER      TO   RYK-IDORDER                          
395800       ELSE                                                               
395900         MOVE ZERO              TO   RYK-IDORDER                          
396000       END-IF                                                             
396100                                                                          
396200     END-IF                                                               
396300                                                                          
396400     MOVE    SPAR-ORAD-IDARTNR  TO   RYK-IDARTNR                          
396500     MOVE    DAT-TIAAMMDD       TO   RYK-TIRODAT                          
396600     MOVE    SPAR-ORAD-KVLEVART TO   RYK-KVLEVART                         
396700     MOVE    SPAR-ORAD-KVBEART  TO   RYK-KVBEART-Q                        
396800     MOVE    SPAR-ORAD-KDORDKL  TO   RYK-KDORDKL                          
396900     MOVE    SPAR-ORAD-KDPRODSL TO   RYK-KDPRODSL                         
397000                                                                          
397100     MOVE    WS-KDORDBEK        TO   RYK-KDORDBEK                         
397200                                                                          
397300     MOVE    SPACE              TO   LOGG-SORTPOST                        
397400     MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                        
397500                                                                          
397600     PERFORM IMS-ISRT-AVVIKELSE                                           
397700                                                                          
397800     PERFORM UNTIL SEGMENT-FINNS                                          
397900       IF LOGG-IDLOGLOP = 9                                               
398000         MOVE ZERO              TO LOGG-IDLOGLOP                          
398100         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
398200       END-IF                                                             
398300       ADD +1                   TO LOGG-IDLOGLOP                          
398400       PERFORM IMS-ISRT-AVVIKELSE                                         
398500     END-PERFORM                                                          
398600     .                                                                    
398700     SKIP2                                                                
398800 S12F-CREATE-EVENT-HANDLING-152    SECTION.                               
398900     MOVE 'S12F-CREATE-EVENT-HANDLING'  TO  CURRENT-SECTION               
399000                                                                          
399100*    EVENT HANTERING                                                      
399200     MOVE OHUV-IDSYSTEM(1:4)   TO EVENT-SW                                
399300     IF EVENT-OK OR LYNK-NON-API                                          
399400                                                                          
399500       IF (OHUV-IDSYSTEM(1:3) = 'LYN') OR LYNK-NON-API                    
399600         MOVE 'L'              TO WS-PARTNER                              
399700       END-IF                                                             
399800       IF OHUV-IDSYSTEM(1:3) = 'POL'                                      
399900         MOVE 'P'              TO WS-PARTNER                              
400000       END-IF                                                             
400100       IF OHUV-IDSYSTEM(1:3) = 'ECO'                                      
400200         MOVE 'E'              TO WS-PARTNER                              
400300       END-IF                                                             
400400       IF OHUV-IDSYSTEM(1:3) = 'TAD'                                      
400500         MOVE 'T'              TO WS-PARTNER                              
400600       END-IF                                                             
400700       IF OHUV-IDSYSTEM(1:3) = 'ACC'                                      
400800         MOVE 'A'              TO WS-PARTNER                              
400900       END-IF                                                             
401000       IF OHUV-IDSYSTEM(1:3) = 'APA'                                      
401100         MOVE 'K'              TO WS-PARTNER                              
401200       END-IF                                                             
401300       IF OHUV-IDSYSTEM(1:3) = 'APB'                                      
401400         MOVE 'B'              TO WS-PARTNER                              
401500       END-IF                                                             
401600       IF OHUV-IDSYSTEM(1:3) = 'APC'                                      
401700         MOVE 'C'              TO WS-PARTNER                              
401800       END-IF                                                             
401900       IF OHUV-IDSYSTEM(1:3) = 'APD'                                      
402000         MOVE 'D'              TO WS-PARTNER                              
402100       END-IF                                                             
402200       IF OHUV-IDSYSTEM(1:3) = 'APE'                                      
402300         MOVE 'M'              TO WS-PARTNER                              
402400       END-IF                                                             
402500       IF OHUV-IDSYSTEM(1:3) = 'APF'                                      
402600         MOVE 'F'              TO WS-PARTNER                              
402700       END-IF                                                             
402800       IF OHUV-IDSYSTEM(1:3) = 'APG'                                      
402900         MOVE 'G'              TO WS-PARTNER                              
403000       END-IF                                                             
403100       IF OHUV-IDSYSTEM(1:3) = 'APH'                                      
403200         MOVE 'H'              TO WS-PARTNER                              
403300       END-IF                                                             
403400       IF OHUV-IDSYSTEM(1:3) = 'API'                                      
403500         MOVE 'I'              TO WS-PARTNER                              
403600       END-IF                                                             
403700       IF OHUV-IDSYSTEM(1:3) = 'APJ'                                      
403800         MOVE 'J'              TO WS-PARTNER                              
403900       END-IF                                                             
404000                                                                          
404100       MOVE SPACE               TO Z430-REQU-TIMESTAMP                    
404200       PERFORM S12FA-CREATE-EVENT-152                                     
404300                                                                          
404400     END-IF                                                               
404500     .                                                                    
404600     EJECT                                                                
404700 S12FA-CREATE-EVENT-152     SECTION.                                      
404800     MOVE 'S12FA-CREATE-EVENT-152'   TO CURRENT-SECTION                   
404900                                                                          
405000     MOVE '001'                 TO Z430-REQU-IDMSGVER                     
405110     IF LYNK-NON-API                                                      
405120        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
405130     ELSE                                                                 
405140        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
405150     END-IF                                                               
405200     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
405300     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
405500     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
405600     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
405700     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
405800     MOVE '152'                 TO Z430-IDMSG                             
405900     MOVE 'ORDER LINE IS BACKORDERED AT PACKREPORTING'                    
406000                                TO Z430-TEMFSINF                          
406100                                                                          
406200*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
406300*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
406400     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
406500     MOVE 'Z430'               TO MSG-IDTRANS-1                           
406600     MOVE '1'                  TO MSG-KDMFSFOR-1                          
406700     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
406800     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
406900          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
407000                                                                          
407100     ADD  1                    TO MSG-KOM-TIKLOCK                         
407200     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
407300     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
407400                                                                          
407500     CALL W006KOM USING MSG-PCB                                           
407600                        0693-PCB                                          
407700                        KOM-WDP8-PCB                                      
407800                        MSG-KOM-WMSGKOM                                   
407900                        MSG-IO-AREA                                       
408000                                                                          
408100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
408200        MOVE                                                              
408300        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
408400                                     TO ERRORTEX                          
408500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
408600     END-IF                                                               
408700     .                                                                    
408800     EJECT                                                                
408900                                                                          
408910 S12G-EVENTS-NON-API        SECTION.                                      
408920                                                                          
408921     MOVE 'S12G-EVENTS-NON-API'      TO CURRENT-SECTION                   
408922                                                                          
408930     IF VOR                                                               
408940       MOVE LOW-VALUE        TO W-WDA6BSEQ-MIN-X                          
408950       MOVE HIGH-VALUE       TO W-WDA6BSEQ-MAX-X                          
408960                                                                          
408970       MOVE OHUV-IDDISTR     TO W-A6BSEQ-MIN-IDDISTR                      
408980                                W-A6BSEQ-MAX-IDDISTR                      
408990       MOVE OHUV-IDKUNDNR    TO W-A6BSEQ-MIN-IDKUNDNR                     
408991                                W-A6BSEQ-MAX-IDKUNDNR                     
408992       MOVE OHUV-IDKUNDRF   TO W-A6BSEQ-MIN-IDKUNDRF-LEV                  
408993                               W-A6BSEQ-MAX-IDKUNDRF-LEV                  
408994       PERFORM IMS-GU-SEQB-WDA601                                         
408995       IF SEGMENT-FINNS                                                   
408996         MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT                          
408997         MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT                         
408998         MOVE VOR-IDORDNR7   TO WS-IDORDNR7-EVENT                         
408999         MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT                      
409000         MOVE JA             TO CREATE-EVENT-SW                           
409001       END-IF                                                             
409002     ELSE                                                                 
409003       IF (ORAD-IDKUNDRF-RO NOT = '0000000   ') AND                       
409004          (ORAD-IDKUNDRF-RO NOT = '00000     ')                           
409005          MOVE KORD-IDDISTR       TO   W-WDQ2CSEQ-IDDISTR                 
409006          MOVE KORD-IDKUNDNR      TO   W-WDQ2CSEQ-IDKUNDNR                
409007          MOVE SPAR-ORAD-IDKUNDRF-RO(1:5)                                 
409008                                   TO W-WDQ2CSEQ-IDKUNDRF(3:7)            
409009          PERFORM IMS-GU-ORQI01-CSEQ                                      
409010          IF SEGMENT-FINNS                                                
409011            MOVE OHUV-IDORDER     TO  RYK-IDORDER                         
409012                                                                          
409013            MOVE OHUV-IDDISTR     TO WS-IDDISTR-EVENT                     
409014            MOVE OHUV-IDKUNDNR    TO WS-IDKUNDNR-EVENT                    
409015            MOVE OHUV-IDORDNR7    TO WS-IDORDNR7-EVENT                    
409016            MOVE OHUV-TIREGDAT    TO WS-TIREGDAT-EVENT                    
409017****        MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT                    
409018            MOVE JA               TO CREATE-EVENT-SW                      
409019          END-IF                                                          
409020       ELSE                                                               
409021          MOVE OHUV-IDDISTR     TO W-IDDISTR-A5-MIN                       
409022                                   W-IDDISTR-A5-MAX                       
409023          MOVE OHUV-IDKUNDNR    TO W-IDKUNDNR-A5-MIN                      
409024                                   W-IDKUNDNR-A5-MAX                      
409025          MOVE OHUV-KDORDKL     TO W-KDORDKL                              
409026          MOVE OHUV-IDORDNR7(3:5) TO W-IDKUNDRF-LEV                       
409027                                                                          
409028          PERFORM IMS-GU-WDA501                                           
409029          IF SEGMENT-FINNS                                                
409030            MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT                       
409031            MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT                      
409032            MOVE RAD-IDORDNR5   TO WS-IDORDNR7-EVENT                      
409033            MOVE RAD-TIREGDAT   TO WS-TIREGDAT-EVENT                      
409034            MOVE JA             TO CREATE-EVENT-SW                        
409035          END-IF                                                          
409036       END-IF                                                             
409037     END-IF                                                               
409038     IF CREATE-EVENT-SW = NEJ                                             
409039       MOVE KORD-IDDISTR     TO WS-IDDISTR-EVENT                          
409040       MOVE KORD-IDKUNDNR    TO WS-IDKUNDNR-EVENT                         
409041       MOVE KORD-IDORDNR7(1:5)                                            
409042                             TO WS-IDORDNR7-EVENT(3:5)                    
409043       MOVE ZERO             TO WS-IDORDNR7-EVENT(1:2)                    
409044       MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT                         
409045       MOVE JA               TO CREATE-EVENT-SW                           
409046     END-IF                                                               
409047     .                                                                    
409048     EJECT                                                                
409050 S13-GENERERA-KLAR-SV SECTION.                                            
409100     SKIP2                                                                
409200     IF LOGG-IDLOGLOP = 9                                                 
409300       MOVE ZERO                          TO LOGG-IDLOGLOP                
409400     END-IF                                                               
409500     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
409600     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
409700     ADD +1                               TO LOGG-IDLOGLOP                
409800     MOVE 'RY6'                           TO RY6-IDPTYP                   
409900                                             LOGG-IDPTYP                  
410000     MOVE VORD-IDDISTR                    TO RY6-IDDISTR                  
410100     MOVE VORD-IDKUNDNR                   TO RY6-IDKUNDNR                 
410200     MOVE WS-IDKUNDRF                     TO RY6-IDKUNDRF                 
410300     MOVE VORD-IDDC                       TO RY6-IDDC                     
410400     MOVE VORD-IDPRODNR                   TO RY6-IDPRODNR                 
410500     MOVE SPACE                           TO LOGG-SORTPOST                
410600     MOVE RY6-WDGZRY6                     TO LOGG-LOGGPOST                
410700                                                                          
410800     PERFORM IMS-ISRT-KLAR-SV                                             
410900                                                                          
411000     PERFORM UNTIL SEGMENT-FINNS                                          
411100       IF LOGG-IDLOGLOP = 9                                               
411200         MOVE ZERO              TO LOGG-IDLOGLOP                          
411300         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
411400       END-IF                                                             
411500       ADD +1                   TO LOGG-IDLOGLOP                          
411600       PERFORM IMS-ISRT-KLAR-SV                                           
411700     END-PERFORM                                                          
411800     .                                                                    
411900     SKIP2                                                                
412000 S14-EV-LARM-2191-MID  SECTION.                                           
412100                                                                          
412200** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
412300     IF CLAG-KVROS = 0                                                    
412400       IF CLAG-KVAKS-CDC = 0                                              
412500         COMPUTE ALT2191-LL = LENGTH OF ALT2191-MID-W2I19101 + 17         
412600         MOVE +1              TO ALT2191-MID-KDCLAGER                     
412700         MOVE ORAD-IDARTNR    TO ALT2191-MID-IDARTNR                      
412800         MOVE ZERO            TO ALT2191-MID-TISENBEK-DAG                 
412900                                 ALT2191-MID-TISENBEK-KL                  
413000         MOVE SPACE           TO ALT2191-MID-IDKR                         
413100         MOVE WS-IDANSK       TO ALT2191-MID-IDANSK                       
413200         MOVE 210             TO ALT2191-MID-KDLARM                       
413300         MOVE WS-IDDISTR-X4   TO ALT2191-MID-IDDISTR                      
413400         MOVE WS-IDKUNDNR     TO ALT2191-MID-IDKUNDNR                     
413500         MOVE WS-IDKUNDRF     TO ALT2191-MID-IDKUNDRF                     
413600         MOVE 'J'             TO ALT2191-MID-FLNYLARM                     
413700         MOVE WC-CDC-SE       TO ALT2191-MID-IDDC                         
413800         MOVE SPACE           TO ALT2191-MID-IDLEVNR                      
413900                                                                          
414000         PERFORM IMS-PURGE-ALT2191-MSG                                    
414100       END-IF                                                             
414200     END-IF                                                               
414300     .                                                                    
414400     EJECT                                                                
414500 S14-EV-LARM-2191-MID-CN-US SECTION.                                      
414600     MOVE 'S14-EV-LARM-2191-MID-CN-US' TO CURRENT-SECTION                 
414700                                                                          
414800** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
414900     IF SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                        
415000       IF SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                       
415100         COMPUTE ALT2191-LL = LENGTH OF ALT2191-MID-W2I19101 + 17         
415200         MOVE +1              TO ALT2191-MID-KDCLAGER                     
415300         MOVE ORAD-IDARTNR    TO ALT2191-MID-IDARTNR                      
415400         MOVE ZERO            TO ALT2191-MID-TISENBEK-DAG                 
415500                                 ALT2191-MID-TISENBEK-KL                  
415600         MOVE SPACE           TO ALT2191-MID-IDKR                         
415700*                                                                         
415800         MOVE CLAG-IDANSK     TO ALT2191-MID-IDANSK                       
415900         PERFORM IMS-GU-WDK722                                            
416000         IF SEGMENT-FINNS AND XLAG-IDANSK > 0                             
416100           MOVE XLAG-IDANSK   TO ALT2191-MID-IDANSK                       
416200         END-IF                                                           
416300*                                                                         
416400         MOVE 210             TO ALT2191-MID-KDLARM                       
416500         MOVE WS-IDDISTR-X4   TO ALT2191-MID-IDDISTR                      
416600         MOVE WS-IDKUNDNR     TO ALT2191-MID-IDKUNDNR                     
416700         MOVE WS-IDKUNDRF     TO ALT2191-MID-IDKUNDRF                     
416800         MOVE 'J'             TO ALT2191-MID-FLNYLARM                     
416900         MOVE SLAG-IDDC       TO ALT2191-MID-IDDC                         
417000         MOVE SLAG-IDLEVNR    TO ALT2191-MID-IDLEVNR                      
417100                                                                          
417200         PERFORM IMS-PURGE-ALT2191-MSG                                    
417300                                                                          
417400       END-IF                                                             
417500     END-IF                                                               
417600     .                                                                    
417700     EJECT                                                                
417800 S15-SKAPA-SALDOLOGG SECTION.                                             
417900                                                                          
418000     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
418100                                                                          
418200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
418300     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
418400                                   - WS-AAAAMMDD                          
418500     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
418600     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
418700                                   - WS-TTMMSSTH                          
418800     MOVE 9                        TO LOGG-IDSEKVNR                       
418900     MOVE WS-IDDC                  TO LOGG-IDDC                           
419000     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
419100     MOVE 'PAC'                    TO LOGG-IDSUBTYP                       
419200     MOVE 'W4039800'               TO LOGG-IDPGM                          
419300     MOVE WS-IDTRANS                TO LOGG-IDTRANS                       
419400     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
419500     MOVE SPACE                    TO LOGG-REF                            
419600     MOVE WS-IDDISTR               TO LOGG-IDDISTR                        
419700     MOVE WS-IDKUNDNR              TO LOGG-IDKUNDNR                       
419800     MOVE WS-IDKUNDRF              TO LOGG-IDKUNDRF                       
419900     MOVE WS-IDPRODNR              TO LOGG-IDPRODNR                       
420000                                                                          
420100     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
420200     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
420300     MOVE '-'                      TO LOGG-IDTECKEN-KVEFRS                
420400     MOVE '+'                      TO LOGG-IDTECKEN-KVLS                  
420500     MOVE WS-KVORAPP-PACK          TO LOGG-KVART-SALDO                    
420600     MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                          
420700     MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
420800     MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                         
420900     MOVE SLAG-KVLS                TO LOGG-KVLS                           
421000     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
421100                                                                          
421200     PERFORM IMS-ISRT-WDL901                                              
421300     IF SEGMENT-FINNS-REDAN                                               
421400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
421500         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
421600         PERFORM IMS-ISRT-WDL901                                          
421700       END-PERFORM                                                        
421800     END-IF                                                               
421900     .                                                                    
422000     EJECT                                                                
422100 S25-DELETE-PRICE-Q-LINE SECTION.                                         
422200                                                                          
422300     IF DIST79-DEALER-PRICE                                               
422400       IF SPAR-ORAD-IDPRQUES > ZERO                                       
422500         INITIALIZE PRQU-W335PRQU                                         
422600         MOVE KORD-IDDISTR            TO PRQU-IDDISTR                     
422700         MOVE KORD-IDKUNDNR           TO PRQU-IDKUNDNR                    
422800         MOVE WS-IDKUNDRF-NEW         TO PRQU-IDKUNDRF                    
422900         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
423000         MOVE 4                       TO PRQU-KDCALL                      
423100         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
423200                                            PRQU-WDC7-PCB                 
423300                                            PRQU-SJKO-WDK6-PCB            
423400       END-IF                                                             
423500     END-IF                                                               
423600     .                                                                    
423700     EJECT                                                                
423800 S36-ANDRA-WDC711 SECTION.                                                
423900                                                                          
424000                                                                          
424100     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
424200       IF RAD-IDPRQUES > ZERO                                             
424300         INITIALIZE PRQU-W335PRQU                                         
424400         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
424500         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
424600         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
424700         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
424800         MOVE RAD-IDPRQUES            TO PRQU-IDPRQUES                    
424900         MOVE 6                       TO PRQU-KDCALL                      
425000         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
425100                                            PRQU-WDC7-PCB                 
425200                                            PRQU-SJKO-WDK6-PCB            
425300         MOVE 'N'                     TO RAD-FLPRTILL                     
425400         IF RAD-PRARTNTO-LOC > +0                                         
425500           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
425600           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
425700         END-IF                                                           
425800         IF PRQU-KDCALL = -1                                              
425900           PERFORM S36-NY-FRAGA                                           
426000         END-IF                                                           
426100       ELSE                                                               
426200*SKAPA NY PRISFRÅGA                                                       
426300         PERFORM S36-NY-FRAGA                                             
426400       END-IF                                                             
426500     END-IF                                                               
426600     .                                                                    
426700     EJECT                                                                
426800 S36-NY-FRAGA  SECTION.                                                   
426900                                                                          
427000     MOVE ZERO                     TO WS-IDPRQUES                         
427100     IF WS-IDPRQUES                = +0                                   
427200        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
427300        MOVE +1                    TO PRNO-KDCALL                         
427400                                                                          
427500        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
427600                                                                          
427700        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
427800                                      WS-IDPRQUES                         
427900        MOVE +1                    TO PRQU-KDCALL                         
428000     END-IF                                                               
428100     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
428200     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
428300     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
428400     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
428500     MOVE ZERO                     TO PRQU-IDORDER                        
428600     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
428700     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
428800       AND RAD-DARODAT > 0                                                
428900       MOVE 4                      TO PRQU-KDORDKL                        
429000     END-IF                                                               
429100     MOVE 'Q'                      TO PRQU-KDPRSTA                        
429200     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
429300     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
429400     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
429500     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
429600     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
429700     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
429800*        PERFORM IMS-GU-GMTA-WDB201                                       
429900                                                                          
430000     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
430100                                         PRQU-WDC7-PCB                    
430200                                         PRQU-SJKO-WDK6-PCB               
430300     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
430400                                       WS-IDPRQUES                        
430500     MOVE 'N'                      TO  RAD-FLPRTILL                       
430600                                                                          
430700     IF RAD-PRARTNTO-LOC = +0                                             
430800        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
430900                       RAD-PRARTNTO-LOCPREL                               
431000     ELSE                                                                 
431100        MOVE RAD-PRARTNTO-LOC  TO RAD-PRARTNTO-LOCPREL                    
431200        MOVE ZERO              TO RAD-PRARTNTO-LOC                        
431300     END-IF                                                               
431400                                                                          
431500     MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                     
431600     MOVE +3                      TO PRNO-KDCALL                          
431700                                                                          
431800     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
431900     .                                                                    
432000     EJECT                                                                
432100* IMS SEKTIONER                                                           
432200*                                                                         
432300 IMS-GET-MSG SECTION.                                                     
432400                                                                          
432500     MOVE '  QC' TO GODK-STATUSKODER                                      
432600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
432700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
432800     PERFORM IMS-STATUSKONTROLL                                           
432900     .                                                                    
433000     SKIP2                                                                
433100 IMS-INSERT-ALT0605MSG SECTION.                                           
433200                                                                          
433300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
433400     MOVE SPACE TO GODK-STATUSKODER                                       
433500     CALL CBLTDLI USING ISRT ALT0605-PCB MSG-IO-AREA                      
433600     MOVE ALT0605-STATUS-CODE TO STATUS-WS                                
433700     PERFORM IMS-STATUSKONTROLL                                           
433800     .                                                                    
433900     EJECT                                                                
434000 IMS-PURGE-ALT2191-MSG SECTION.                                           
434100     MOVE LOW-VALUE TO ALT2191-Z1 ALT2191-Z2                              
434200     MOVE '  '  TO GODK-STATUSKODER                                       
434300     CALL CBLTDLI USING PURG ALT2191-PCB ALT2191-IO-AREA                  
434400     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
434500     PERFORM IMS-STATUSKONTROLL                                           
434600     .                                                                    
434700     SKIP2                                                                
434800 IMS-INSERT-ALT4342MSG SECTION.                                           
434900                                                                          
435000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
435100     MOVE SPACE TO GODK-STATUSKODER                                       
435200     CALL CBLTDLI USING ISRT ALT4342-PCB MSG-IO-AREA                      
435300     MOVE ALT4342-STATUS-CODE TO STATUS-WS                                
435400     PERFORM IMS-STATUSKONTROLL                                           
435500     .                                                                    
435600     SKIP2                                                                
435700 IMS-GHNP-WDE401-MED-PCB-WDE42           SECTION.                         
435800     MOVE '  ' TO GODK-STATUSKODER                                        
435900     CALL CBLTDLI USING GHNP WDE42-PCB KORD-WDE401                        
436000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
436100     PERFORM IMS-STATUSKONTROLL                                           
436200     .                                                                    
436300     SKIP2                                                                
436400 IMS-GU-WDE401-MED-PCB-WDE42             SECTION.                         
436500                                                                          
436600     STRING 'WDE411  (WDE4BSEQ>=' W-WDE411-IDPRODNR2-MIN-X                
436700                    '&WDE4BSEQ<=' W-WDE411-IDPRODNR2-MAX-X ')'            
436800            DELIMITED BY SIZE INTO SSA1                                   
436900     MOVE 'WDE401   ' TO SSA2                                             
437000     MOVE '  ' TO GODK-STATUSKODER                                        
437100     CALL CBLTDLI USING GU   WDE42-PCB KORD-WDE401 SSA1 SSA2              
437200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
437300     PERFORM IMS-STATUSKONTROLL                                           
437400     .                                                                    
437500     EJECT                                                                
437600 IMS-REPL-WDE401-MED-PCB-WDE42           SECTION.                         
437700     MOVE '  '   TO GODK-STATUSKODER                                      
437800     CALL CBLTDLI USING REPL WDE42-PCB KORD-WDE401                        
437900     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
438000     PERFORM IMS-STATUSKONTROLL                                           
438100     .                                                                    
438200     EJECT                                                                
438300 IMS-GHU-RAD        SECTION.                                              
438400     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
438500            DELIMITED BY SIZE INTO SSA1                                   
438600     MOVE '  ' TO GODK-STATUSKODER                                        
438700     CALL CBLTDLI USING GHU  WDE42-PCB DLI-IO-E411 SSA1                   
438800     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
438900     PERFORM IMS-STATUSKONTROLL                                           
439000     .                                                                    
439100     SKIP2                                                                
439200 IMS-REPL-RAD           SECTION.                                          
439300     MOVE '    ' TO GODK-STATUSKODER                                      
439400     CALL CBLTDLI USING REPL WDE42-PCB DLI-IO-E411                        
439500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
439600     PERFORM IMS-STATUSKONTROLL                                           
439700     .                                                                    
439800     EJECT                                                                
439900 IMS-GHU-WDE401-SEQA                     SECTION.                         
440000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
440100            DELIMITED BY SIZE INTO SSA1                                   
440200     MOVE '  ' TO GODK-STATUSKODER                                        
440300     CALL CBLTDLI USING GHU  WDE41-PCB KORD-WDE401 SSA1                   
440400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
440500     PERFORM IMS-STATUSKONTROLL                                           
440600     .                                                                    
440700     SKIP2                                                                
440800 IMS-GHN-WDE401-SEQA                     SECTION.                         
440900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
441000            DELIMITED BY SIZE INTO SSA1                                   
441100     MOVE '  GE' TO GODK-STATUSKODER                                      
441200     CALL CBLTDLI USING GHN  WDE41-PCB KORD-WDE401 SSA1                   
441300     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
441400     PERFORM IMS-STATUSKONTROLL                                           
441500     .                                                                    
441600     SKIP2                                                                
441700 IMS-REPL-WDE401-MED-PCB-WDE41           SECTION.                         
441800     MOVE '  '   TO GODK-STATUSKODER                                      
441900     CALL CBLTDLI USING REPL WDE41-PCB KORD-WDE401                        
442000     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
442100     PERFORM IMS-STATUSKONTROLL                                           
442200     .                                                                    
442300     EJECT                                                                
442400 IMS-GHU-ORQA01                          SECTION.                         
442500     STRING 'WLORQA01(WDQ301KY =' Q301-WDQ301KY-X ')'                     
442600            DELIMITED BY SIZE INTO SSA1                                   
442700     MOVE '    ' TO GODK-STATUSKODER                                      
442800     CALL CBLTDLI USING GHU  ORQA-PCB ODEL-WDQ301 SSA1                    
442900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
443000     PERFORM IMS-STATUSKONTROLL                                           
443100     .                                                                    
443200                                                                          
443300 IMS-GU-ORQA-STATUS    SECTION.                                           
443400                                                                          
443500     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
443600                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
443700                    '&KDODELST =' W-KDODELST     ')'                      
443800          DELIMITED BY SIZE INTO SSA1                                     
443900     MOVE '  GE' TO GODK-STATUSKODER                                      
444000     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
444100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
444200     PERFORM IMS-STATUSKONTROLL                                           
444300     .                                                                    
444400                                                                          
444500 IMS-REPL-ORQA           SECTION.                                         
444600     MOVE '  '   TO GODK-STATUSKODER                                      
444700     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
444800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
444900     PERFORM IMS-STATUSKONTROLL                                           
445000     .                                                                    
445100     SKIP3                                                                
445200 IMS-GU-ORQM01 SECTION.                                                   
445300                                                                          
445400     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
445500                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
445600          DELIMITED BY SIZE INTO SSA1                                     
445700     MOVE '  GE'               TO GODK-STATUSKODER                        
445800     CALL CBLTDLI USING GU ORQM-PCB OBKR-WDQ101 SSA1                      
445900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
446000     PERFORM IMS-STATUSKONTROLL                                           
446100     .                                                                    
446200     SKIP2                                                                
446300 IMS-ISRT-ORQM01                         SECTION.                         
446400     MOVE 'WLORQM01'  TO SSA1                                             
446500     MOVE '  '   TO GODK-STATUSKODER                                      
446600     CALL CBLTDLI USING ISRT ORQM-PCB OBKR-WDQ101 SSA1                    
446700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
446800     PERFORM IMS-STATUSKONTROLL                                           
446900     .                                                                    
447000     EJECT                                                                
447100 IMS-GHU-KOLLIREG SECTION.                                                
447200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
447300            DELIMITED BY SIZE INTO SSA1                                   
447400     MOVE '  GE' TO GODK-STATUSKODER                                      
447500     CALL CBLTDLI USING GHU  WDE6-PCB DLI-IO-E601 SSA1                    
447600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
447700     PERFORM IMS-STATUSKONTROLL                                           
447800     .                                                                    
447900     SKIP2                                                                
448000 IMS-GNP-KOLLI-99XXX SECTION.                                             
448100     STRING 'WDE611  (IDKOLLI  >' W-WDE611-IDKOLLI-X ')'                  
448200            DELIMITED BY SIZE INTO SSA1                                   
448300     MOVE '  GE' TO GODK-STATUSKODER                                      
448400     CALL CBLTDLI USING GNP  WDE6-PCB DLI-IO-E611 SSA1                    
448500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
448600     PERFORM IMS-STATUSKONTROLL                                           
448700     .                                                                    
448800     EJECT                                                                
448900 IMS-REPL-KOLLIREG SECTION.                                               
449000     MOVE '    ' TO GODK-STATUSKODER                                      
449100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
449200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
449300     PERFORM IMS-STATUSKONTROLL                                           
449400     .                                                                    
449500     SKIP2                                                                
449600 IMS-GU-XXDJ-ROT  SECTION.                                                
449700                                                                          
449800     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
449900            DELIMITED BY SIZE INTO SSA1                                   
450000     MOVE '  GE' TO GODK-STATUSKODER                                      
450100     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-DJ01 SSA1                      
450200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
450300     PERFORM IMS-STATUSKONTROLL                                           
450400     .                                                                    
450500     SKIP2                                                                
450600 IMS-GHNP-XXDJ-LASNING SECTION.                                           
450700                                                                          
450800     STRING 'WLXXDJ11(WDGXKEY  =' W-4306-X ')'                            
450900            DELIMITED BY SIZE INTO SSA1                                   
451000     MOVE '  GE' TO GODK-STATUSKODER                                      
451100     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-DJ11 SSA1                    
451200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
451300     PERFORM IMS-STATUSKONTROLL                                           
451400     .                                                                    
451500     SKIP2                                                                
451600 IMS-DLET-XXDJ-LASNING SECTION.                                           
451700                                                                          
451800     MOVE '  ' TO GODK-STATUSKODER                                        
451900     CALL CBLTDLI USING DLET XXDJ-PCB DLI-IO-DJ11                         
452000     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
452100     PERFORM IMS-STATUSKONTROLL                                           
452200     .                                                                    
452300     EJECT                                                                
452400 IMS-GU-ARTC11                           SECTION.                         
452500                                                                          
452600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
452700            DELIMITED BY SIZE INTO SSA1                                   
452800     MOVE 'WLARTC11 '           TO SSA2                                   
452900     MOVE '  '     TO GODK-STATUSKODER                                    
453000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA11 SSA1 SSA2               
453100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
453200     PERFORM IMS-STATUSKONTROLL                                           
453300     .                                                                    
453400     SKIP3                                                                
453500 IMS-GHU-ARTC       SECTION.                                              
453600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
453700            DELIMITED BY SIZE INTO SSA1                                   
453800     STRING 'WLARTC11(KDSEGKEY =1)'                                       
453900            DELIMITED BY SIZE INTO SSA2                                   
454000     MOVE '  GE' TO GODK-STATUSKODER                                      
454100     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-AREA11                       
454200                                        SSA1 SSA2                         
454300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
454400     PERFORM IMS-STATUSKONTROLL                                           
454500     .                                                                    
454600     SKIP2                                                                
454700 IMS-REPL-ARTC       SECTION.                                             
454800     MOVE '  '   TO GODK-STATUSKODER                                      
454900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA11                       
455000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
455100     PERFORM IMS-STATUSKONTROLL                                           
455200     .                                                                    
455300     EJECT                                                                
455400 IMS-GHU-WDK711     SECTION.                                              
455500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
455600            DELIMITED BY SIZE INTO SSA1                                   
455700     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
455800            DELIMITED BY SIZE INTO SSA2                                   
455900     MOVE '  GE'   TO GODK-STATUSKODER                                    
456000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
456100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
456200     PERFORM IMS-STATUSKONTROLL                                           
456300     SKIP2                                                                
456400     .                                                                    
456500     EJECT                                                                
456600 IMS-GU-WDK722     SECTION.                                               
456700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
456800            DELIMITED BY SIZE INTO SSA1                                   
456900     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
457000            DELIMITED BY SIZE INTO SSA2                                   
457100     MOVE 'WDK722 '             TO SSA3                                   
457200     MOVE '  GE'   TO GODK-STATUSKODER                                    
457300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
457400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
457500     PERFORM IMS-STATUSKONTROLL                                           
457600     SKIP2                                                                
457700     .                                                                    
457800 IMS-GU-WDK712 SECTION.                                                   
457900                                                                          
458000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
458100          DELIMITED BY SIZE INTO SSA1                                     
458200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
458300          DELIMITED BY SIZE INTO SSA2                                     
458400     MOVE '  GE' TO GODK-STATUSKODER                                      
458500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
458600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
458700     PERFORM IMS-STATUSKONTROLL                                           
458800     .                                                                    
458900                                                                          
459000 IMS-REPL-WDK7       SECTION.                                             
459100     MOVE '  '   TO GODK-STATUSKODER                                      
459200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
459300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
459400     PERFORM IMS-STATUSKONTROLL                                           
459500     .                                                                    
459600     EJECT                                                                
459700 IMS-ISRT-AVVIKELSE  SECTION.                                             
459800     MOVE 'WLZZAC01'  TO SSA1                                             
459900     MOVE '  II' TO GODK-STATUSKODER                                      
460000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA4 SSA1                   
460100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
460200     PERFORM IMS-STATUSKONTROLL                                           
460300     .                                                                    
460400     SKIP2                                                                
460500 IMS-ISRT-KLAR-SV  SECTION.                                               
460600     MOVE 'WLZZAC01'  TO SSA1                                             
460700     MOVE '  II' TO GODK-STATUSKODER                                      
460800     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA4 SSA1                   
460900     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
461000     PERFORM IMS-STATUSKONTROLL                                           
461100     .                                                                    
461200     SKIP2                                                                
461300 IMS-ISRT-AUTFAKTURA-ROT SECTION.                                         
461400     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
461500            DELIMITED BY SIZE INTO SSA1                                   
461600     MOVE   'WLXXDV11 '         TO SSA2                                   
461700     MOVE '  '     TO GODK-STATUSKODER                                    
461800     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3 SSA1 SSA2              
461900     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
462000     PERFORM IMS-STATUSKONTROLL                                           
462100     .                                                                    
462200     SKIP2                                                                
462300 IMS-ISRT-AUTFAKTURA     SECTION.                                         
462400     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
462500            DELIMITED BY SIZE INTO SSA1                                   
462600     STRING 'WLXXDV11(WDGXKEY  =' W-WDGX11-WDGXKEY-X ')'                  
462700            DELIMITED BY SIZE INTO SSA2                                   
462800     MOVE 'WLXXDV21 ' TO SSA3                                             
462900     MOVE '  IIGE' TO GODK-STATUSKODER                                    
463000     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3                        
463100                                        SSA1 SSA2 SSA3                    
463200     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
463300     PERFORM IMS-STATUSKONTROLL                                           
463400     .                                                                    
463500     EJECT                                                                
463600 IMS-4319-ISRT-ROT SECTION.                                               
463700     MOVE 'WLXXJD01 ' TO SSA1                                             
463800     MOVE '  ' TO GODK-STATUSKODER                                        
463900     CALL CBLTDLI USING ISRT XXJD-PCB 4319-WDGX4319 SSA1                  
464000     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
464100     PERFORM IMS-STATUSKONTROLL                                           
464200     .                                                                    
464300     SKIP3                                                                
464400 IMS-4320-ISRT-BARN SECTION.                                              
464500     STRING 'WLXXJD01(WDGXKEY  =' W-4319-X ')'                            
464600            DELIMITED BY SIZE INTO SSA1                                   
464700     MOVE 'WLXXJD11 ' TO SSA2                                             
464800     MOVE '  GEII' TO GODK-STATUSKODER                                    
464900     CALL CBLTDLI USING ISRT XXJD-PCB 4320-WDGX4320 SSA1 SSA2             
465000     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
465100     PERFORM IMS-STATUSKONTROLL                                           
465200     .                                                                    
465300     EJECT                                                                
465400 IMS-GHU-XXJD11     SECTION.                                              
465500     STRING 'WLXXJD01(WDGXKEY  =' W-4319-X ')'                            
465600            DELIMITED BY SIZE INTO SSA1                                   
465700     STRING 'WLXXJD11(WDGXKEY >=' W-4320-X ')'                            
465800            DELIMITED BY SIZE INTO SSA2                                   
465900     MOVE '  ' TO GODK-STATUSKODER                                        
466000     CALL CBLTDLI USING GHU XXJD-PCB 4320-WDGX4320 SSA1 SSA2              
466100     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
466200     PERFORM IMS-STATUSKONTROLL                                           
466300     .                                                                    
466400     EJECT                                                                
466500 IMS-REPL-XXJD11                SECTION.                                  
466600     MOVE '  '   TO GODK-STATUSKODER                                      
466700     CALL CBLTDLI USING REPL XXJD-PCB 4320-WDGX4320                       
466800     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
466900     PERFORM IMS-STATUSKONTROLL                                           
467000     .                                                                    
467100     SKIP3                                                                
467200 IMS-GU-WDA501 SECTION.                                                   
467300                                                                          
467400     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-A5-MIN-X                     
467500                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
467600                    '&KDORDKL  =' W-KDORDKL-X                             
467700                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
467800          DELIMITED BY SIZE INTO SSA1                                     
467900     MOVE '  GE'              TO GODK-STATUSKODER                         
468000     CALL CBLTDLI USING GU ORDP1-PCB DLI-IO-AREA9   SSA1                  
468100     MOVE ORDP1-STATUS-CODE   TO STATUS-WS                                
468200     PERFORM IMS-STATUSKONTROLL                                           
468300     .                                                                    
468400     EJECT                                                                
468500 IMS-GHU-ORDP01   SECTION.                                                
468600                                                                          
468700     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
468800            DELIMITED BY SIZE INTO SSA1                                   
468900     MOVE '  '     TO GODK-STATUSKODER                                    
469000     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA9 SSA1                   
469100     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
469200     PERFORM IMS-STATUSKONTROLL                                           
469300     .                                                                    
469400     SKIP3                                                                
469500 IMS-GHU-ORDP01-GE      SECTION.                                          
469600                                                                          
469700     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
469800            DELIMITED BY SIZE INTO SSA1                                   
469900     MOVE '  GE'   TO GODK-STATUSKODER                                    
470000     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA9 SSA1                   
470100     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
470200     PERFORM IMS-STATUSKONTROLL                                           
470300     .                                                                    
470400     SKIP3                                                                
470500 IMS-REPL-ORDP01  SECTION.                                                
470600                                                                          
470700     MOVE '  '     TO GODK-STATUSKODER                                    
470800     CALL CBLTDLI USING REPL ORDP1-PCB DLI-IO-AREA9                       
470900     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
471000     PERFORM IMS-STATUSKONTROLL                                           
471100     .                                                                    
471200     SKIP3                                                                
471300 IMS-ISRT-ORDP01  SECTION.                                                
471400                                                                          
471500     MOVE 'WLORDP01 ' TO SSA1                                             
471600     MOVE '  II'   TO GODK-STATUSKODER                                    
471700     CALL CBLTDLI USING ISRT ORDP1-PCB DLI-IO-AREA9 SSA1                  
471800     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
471900     PERFORM IMS-STATUSKONTROLL                                           
472000     .                                                                    
472100     SKIP3                                                                
472200*IMS-ISRT-ORDP01-OLD  SECTION.                                            
472300*                                                                         
472400*    MOVE 'WLORDP01 ' TO SSA1                                             
472500*    MOVE '  II'   TO GODK-STATUSKODER                                    
472600*    CALL CBLTDLI USING ISRT ORDP2-PCB DLI-IO-AREA10 SSA1                 
472700*    MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
472800*    PERFORM IMS-STATUSKONTROLL                                           
472900*    SKIP3                                                                
473000*    .                                                                    
473100 IMS-DLET-ORDP01  SECTION.                                                
473200                                                                          
473300     MOVE '  '     TO GODK-STATUSKODER                                    
473400     CALL CBLTDLI USING DLET ORDP1-PCB DLI-IO-AREA9                       
473500     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
473600     PERFORM IMS-STATUSKONTROLL                                           
473700     .                                                                    
473800     EJECT                                                                
473900 IMS-GHU-ORDP01-OLD SECTION.                                              
474000                                                                          
474100     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
474200                    '&WDA501KY=<' W2-WDA501KY-X                           
474300                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
474400            DELIMITED BY SIZE INTO SSA1                                   
474500     MOVE '  GE'   TO GODK-STATUSKODER                                    
474600     CALL CBLTDLI USING GHU ORDP2-PCB DLI-IO-AREA10 SSA1                  
474700     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
474800     PERFORM IMS-STATUSKONTROLL                                           
474900     .                                                                    
475000     SKIP3                                                                
475100 IMS-GHN-ORDP01-OLD SECTION.                                              
475200                                                                          
475300     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
475400                    '&WDA501KY=<' W2-WDA501KY-X                           
475500                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
475600            DELIMITED BY SIZE INTO SSA1                                   
475700     MOVE '  GE'   TO GODK-STATUSKODER                                    
475800     CALL CBLTDLI USING GHN ORDP2-PCB DLI-IO-AREA10 SSA1                  
475900     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
476000     PERFORM IMS-STATUSKONTROLL                                           
476100     .                                                                    
476200     SKIP3                                                                
476300 IMS-REPL-ORDP01-OLD  SECTION.                                            
476400                                                                          
476500     MOVE '  '     TO GODK-STATUSKODER                                    
476600     CALL CBLTDLI USING REPL ORDP2-PCB DLI-IO-AREA10                      
476700     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
476800     PERFORM IMS-STATUSKONTROLL                                           
476900     .                                                                    
477000     EJECT                                                                
477100 IMS-GU-XXJN     SECTION.                                                 
477200                                                                          
477300     STRING 'WLXXJN01(WDGXKEY  =' W-WDGX01 ')'                            
477400            DELIMITED BY SIZE INTO SSA1                                   
477500     STRING 'WLXXJN11(WDGXKEY >=' W-WDGXKEY-N5-MIN                        
477600                    '&WDGXKEY <=' W-WDGXKEY-N5-MAX                        
477700                    '&KDRAPRIO>=' W-KDRAPRIO-N5-MIN-X                     
477800                    '&KDRAPRIO<=' W-KDRAPRIO-N5-MAX-X                     
477900                    '&KDTPOTYP =' W-KDTPOTYP-N5-X                         
478000                    '&KDORDKL  =' W-KDORDKL-N5-X                          
478100                    '&IDDISTRF<=' W-IDDISTR-FOM-N5-X                      
478200                    '&IDDISTRT>=' W-IDDISTR-TOM-N5-X ')'                  
478300            DELIMITED BY SIZE INTO SSA2                                   
478400     MOVE '  ' TO GODK-STATUSKODER                                        
478500     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA8 SSA1 SSA2                
478600     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
478700     PERFORM IMS-STATUSKONTROLL                                           
478800     .                                                                    
478900     EJECT                                                                
479000 IMS-ISRT-4542                           SECTION.                         
479100                                                                          
479200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
479300            DELIMITED BY SIZE INTO SSA1                                   
479400     MOVE 'WDGX4542 ' TO SSA2                                             
479500     MOVE '  II' TO GODK-STATUSKODER                                      
479600     CALL CBLTDLI USING ISRT 4541-PCB 4542-WDGX4542 SSA1 SSA2             
479700     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
479800     PERFORM IMS-STATUSKONTROLL                                           
479900     .                                                                    
480000     EJECT                                                                
480100 IMS-GHU-WDK901                          SECTION.                         
480200                                                                          
480300     STRING 'WLARTM01(IDARTNR  =' W-WDK901-IDARTNR-X ')'                  
480400            DELIMITED BY SIZE INTO SSA1                                   
480500     MOVE '  ' TO GODK-STATUSKODER                                        
480600     CALL CBLTDLI USING GHU  ARTM-PCB ART-WDK901 SSA1                     
480700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
480800     PERFORM IMS-STATUSKONTROLL                                           
480900     .                                                                    
481000     SKIP2                                                                
481100 IMS-REPL-WDK901                         SECTION.                         
481200                                                                          
481300     MOVE '  '     TO GODK-STATUSKODER                                    
481400     CALL CBLTDLI USING REPL ARTM-PCB ART-WDK901                          
481500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
481600     PERFORM IMS-STATUSKONTROLL                                           
481700     .                                                                    
481800     EJECT                                                                
481900 IMS-GHU-WDGX4490   SECTION.                                              
482000     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
482100         DELIMITED BY SIZE INTO SSA1                                      
482200     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
482300         DELIMITED BY SIZE INTO SSA2                                      
482400     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
482500         DELIMITED BY SIZE INTO SSA3                                      
482600     MOVE '  GE' TO GODK-STATUSKODER                                      
482700     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-AREA12 SSA1 SSA2 SSA3         
482800     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
482900     PERFORM IMS-STATUSKONTROLL                                           
483000     .                                                                    
483100     SKIP2                                                                
483200 IMS-DLET-WDGX4490     SECTION.                                           
483300     MOVE '  ' TO GODK-STATUSKODER                                        
483400     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-AREA12                       
483500     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
483600     PERFORM IMS-STATUSKONTROLL                                           
483700     .                                                                    
483800     EJECT                                                                
483900 IMS-GHU-XXKW11       SECTION.                                            
484000     STRING 'WLXXKW01(WDGXKEY  =' W-WDGXKEY-4471-X ')'                    
484100            DELIMITED BY SIZE INTO SSA1                                   
484200     STRING 'WLXXKW11(KDSEGKEY =' W-KDSEGKEY-4472-X ')'                   
484300            DELIMITED BY SIZE INTO SSA2                                   
484400     MOVE '  GE' TO GODK-STATUSKODER                                      
484500     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA13 SSA1 SSA2           
484600     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
484700     PERFORM IMS-STATUSKONTROLL                                           
484800     .                                                                    
484900 IMS-REPL-XXKW11    SECTION.                                              
485000     MOVE '  '   TO GODK-STATUSKODER                                      
485100     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA13                       
485200     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
485300     PERFORM IMS-STATUSKONTROLL                                           
485400     .                                                                    
485500     EJECT                                                                
485600 IMS-GU-XXLB         SECTION.                                             
485700     STRING 'WLXXLB01(WDGXKEY  =' W-WDGXKEY-4477-X ')'                    
485800            DELIMITED BY SIZE INTO SSA1                                   
485900     STRING 'WLXXLB11(WDGXKEY  =' W-WDGXKEY-4478-X ')'                    
486000            DELIMITED BY SIZE INTO SSA2                                   
486100     MOVE '  GE' TO GODK-STATUSKODER                                      
486200     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA13 SSA1 SSA2            
486300     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
486400     PERFORM IMS-STATUSKONTROLL                                           
486500     .                                                                    
486600     EJECT                                                                
486700 IMS-GU-WDM211 SECTION.                                                   
486800                                                                          
486900     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
487000          DELIMITED BY SIZE INTO SSA1                                     
487100     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
487200          DELIMITED BY SIZE INTO SSA2                                     
487300     MOVE '  GE'              TO GODK-STATUSKODER                         
487400     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
487500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
487600     PERFORM IMS-STATUSKONTROLL                                           
487700     .                                                                    
487800                                                                          
487900 IMS-GNP-WDM221 SECTION.                                                  
488000                                                                          
488100     MOVE 'WDM221 '           TO SSA1                                     
488200     MOVE '    GE'            TO GODK-STATUSKODER                         
488300     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
488400     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
488500     PERFORM IMS-STATUSKONTROLL                                           
488600     .                                                                    
488700                                                                          
488800 IMS-GHU-WDM211 SECTION.                                                  
488900                                                                          
489000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
489100          DELIMITED BY SIZE INTO SSA1                                     
489200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
489300          DELIMITED BY SIZE INTO SSA2                                     
489400     MOVE '  GE'              TO GODK-STATUSKODER                         
489500     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
489600     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
489700     PERFORM IMS-STATUSKONTROLL                                           
489800     .                                                                    
489900                                                                          
490000 IMS-REPL-WDM211 SECTION.                                                 
490100                                                                          
490200     MOVE '  '             TO GODK-STATUSKODER                            
490300     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
490400     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
490500     PERFORM IMS-STATUSKONTROLL                                           
490600     .                                                                    
490700     EJECT                                                                
490800 IMS-GHU-WDM221 SECTION.                                                  
490900                                                                          
491000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
491100          DELIMITED BY SIZE INTO SSA1                                     
491200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
491300          DELIMITED BY SIZE INTO SSA2                                     
491400     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
491500          DELIMITED BY SIZE INTO SSA3                                     
491600     MOVE '  GE' TO GODK-STATUSKODER                                      
491700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
491800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
491900     PERFORM IMS-STATUSKONTROLL                                           
492000     .                                                                    
492100                                                                          
492200 IMS-REPL-WDM221 SECTION.                                                 
492300                                                                          
492400     MOVE '  '             TO GODK-STATUSKODER                            
492500     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
492600     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
492700     PERFORM IMS-STATUSKONTROLL                                           
492800     .                                                                    
492900                                                                          
493000     EJECT                                                                
493100 IMS-GU-ORQI01-CSEQ SECTION.                                              
493200                                                                          
493300     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
493400             DELIMITED BY SIZE INTO SSA1                                  
493500     MOVE    '  GE'              TO GODK-STATUSKODER                      
493600     CALL    CBLTDLI USING       GU ORQICSQ-PCB OHUV-WDQ201               
493700                                    SSA1                                  
493800     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
493900     PERFORM IMS-STATUSKONTROLL                                           
494000     .                                                                    
494100                                                                          
494200 IMS-GU-WDQ201 SECTION.                                                   
494300                                                                          
494400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
494500            DELIMITED BY SIZE INTO SSA1                                   
494600     MOVE '  '   TO GODK-STATUSKODER                                      
494700     CALL CBLTDLI USING GU  ORQI-PCB OHUV-WDQ201 SSA1                     
494800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
494900     PERFORM IMS-STATUSKONTROLL                                           
495000     SKIP2                                                                
495100     .                                                                    
495200 IMS-GHNP-WDQ212 SECTION.                                                 
495300                                                                          
495400     STRING 'WLORQI12(IDDC     =' W-IDDC-Q2-X ')'                         
495500            DELIMITED BY SIZE INTO SSA1                                   
495600     MOVE '  '   TO GODK-STATUSKODER                                      
495700     CALL CBLTDLI USING GHNP    ORQI-PCB ARB-WDQ212 SSA1                  
495800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
495900     PERFORM IMS-STATUSKONTROLL                                           
496000     SKIP2                                                                
496100     .                                                                    
496200 IMS-REPL-WDQ212 SECTION.                                                 
496300                                                                          
496400     MOVE '    ' TO GODK-STATUSKODER                                      
496500     CALL CBLTDLI USING REPL ORQI-PCB ARB-WDQ212                          
496600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
496700     PERFORM IMS-STATUSKONTROLL                                           
496800     .                                                                    
496900     EJECT                                                                
497000 IMS-GU-XXKH11 SECTION.                                                   
497100                                                                          
497200     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X    ')'                         
497300            DELIMITED BY SIZE INTO SSA1                                   
497400     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X    ')'                         
497500            DELIMITED BY SIZE INTO SSA2                                   
497600     MOVE '  '   TO GODK-STATUSKODER                                      
497700     CALL CBLTDLI USING GU   XXKH-PCB 4448-WDGX4448-CTX SSA1 SSA2         
497800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
497900     PERFORM IMS-STATUSKONTROLL                                           
498000                                                                          
498100     .                                                                    
498200     EJECT                                                                
498300 IMS-ISRT-WDL901 SECTION.                                                 
498400                                                                          
498500     MOVE 'WLLOGA01 ' TO SSA1                                             
498600     MOVE '  II' TO GODK-STATUSKODER                                      
498700     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
498800     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
498900     PERFORM IMS-STATUSKONTROLL                                           
499000     .                                                                    
499100     SKIP2                                                                
499200 IMS-GU-WDK601                 SECTION.                                   
499300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
499400            DELIMITED BY SIZE INTO SSA1                                   
499500     MOVE '  '                   TO GODK-STATUSKODER                      
499600     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-AREA11 SSA1                
499700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
499800     PERFORM IMS-STATUSKONTROLL                                           
499900     .                                                                    
500000     SKIP2                                                                
500100 IMS-GHNP-WDK611               SECTION.                                   
500200     MOVE 'WDK611  '           TO SSA1                                    
500300     MOVE '  '                 TO GODK-STATUSKODER                        
500400     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-AREA11 SSA1                
500500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
500600     PERFORM IMS-STATUSKONTROLL                                           
500700     .                                                                    
500800     SKIP2                                                                
500900 IMS-GHU-WDK611                SECTION.                                   
501000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
501100            DELIMITED BY SIZE INTO SSA1                                   
501200     MOVE 'WDK611  '           TO SSA2                                    
501300     MOVE '  '                 TO GODK-STATUSKODER                        
501400     CALL  CBLTDLI  USING GHU  WDK6-PCB DLI-IO-AREA11 SSA1 SSA2           
501500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
501600     PERFORM IMS-STATUSKONTROLL                                           
501700     .                                                                    
501800     SKIP2                                                                
501900 IMS-REPL-WDK611               SECTION.                                   
502000     MOVE 'WDK611  '           TO SSA1                                    
502100     MOVE '    '               TO GODK-STATUSKODER                        
502200     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-AREA11 SSA1                
502300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
502400     PERFORM IMS-STATUSKONTROLL                                           
502500     .                                                                    
502600     SKIP2                                                                
502700 IMS-GU-SEQB-WDA601 SECTION.                                              
502800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
502900                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
503000            DELIMITED BY SIZE INTO SSA1                                   
503100     MOVE '  GE'                 TO GODK-STATUSKODER                      
503200     CALL  CBLTDLI  USING GHU  WDA6B-PCB DLI-IO-AREA17 SSA1               
503300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
503400     PERFORM IMS-STATUSKONTROLL                                           
503500     .                                                                    
503600     SKIP2                                                                
503700 IMS-GHU-SEQB-WDA601            SECTION.                                  
503800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
503900                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
504000            DELIMITED BY SIZE INTO SSA1                                   
504100     MOVE '  GE'                 TO GODK-STATUSKODER                      
504200     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA17 SSA1              
504300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
504400     PERFORM IMS-STATUSKONTROLL                                           
504500     .                                                                    
504600     SKIP2                                                                
504700 IMS-GHN-SEQB-WDA601            SECTION.                                  
504800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
504900                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
505000            DELIMITED BY SIZE INTO SSA1                                   
505100     MOVE '  GEGB'               TO GODK-STATUSKODER                      
505200     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA17 SSA1              
505300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
505400     PERFORM IMS-STATUSKONTROLL                                           
505500     .                                                                    
505600     SKIP2                                                                
505700 IMS-REPL-SEQB-WDA601                 SECTION.                            
505800     MOVE 'WDA601  '           TO SSA1                                    
505900     MOVE '    '               TO GODK-STATUSKODER                        
506000     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA17 SSA1               
506100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
506200     PERFORM IMS-STATUSKONTROLL                                           
506300     .                                                                    
506400     EJECT                                                                
506500 IMS-ISRT-WDA601            SECTION.                                      
506600     MOVE   'WDA601  '         TO SSA1                                    
506700     MOVE '  IINI' TO GODK-STATUSKODER                                    
506800     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA17 SSA1                
506900     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
507000     PERFORM IMS-STATUSKONTROLL                                           
507100     .                                                                    
507200                                                                          
507300 IMS-GU-WDB601    SECTION.                                                
507400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
507500          DELIMITED BY SIZE INTO SSA1                                     
507600     MOVE '  GE' TO GODK-STATUSKODER                                      
507700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
507800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
507900     PERFORM IMS-STATUSKONTROLL                                           
508000     IF SEGMENT-SAKNAS                                                    
508100        MOVE SPACE TO DCS-KDDC                                            
508200     END-IF                                                               
508300     .                                                                    
508400     EJECT                                                                
508500 IMS-GU-GMTA-WDB201       SECTION.                                        
508600                                                                          
508700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
508800                      DELIMITED BY SIZE INTO SSA1                         
508900     MOVE '  ' TO GODK-STATUSKODER                                        
509000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
509100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
509200     PERFORM IMS-STATUSKONTROLL                                           
509300     .                                                                    
509400                                                                          
509500 IMS-GU-WDP4A1 SECTION.                                                   
509600                                                                          
509700     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
509800                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
509900          DELIMITED BY SIZE INTO SSA1                                     
510000     MOVE '  GE' TO GODK-STATUSKODER                                      
510100     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
510200                           SSA1                                           
510300     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
510400     PERFORM IMS-STATUSKONTROLL                                           
510500     .                                                                    
510600                                                                          
510700     EJECT                                                                
510800 DB2-SELECT-TP4TRAN     SECTION.                                          
510900     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
511000                                                                          
511100     MOVE 000100 TO GODK-SQLCODEKODER                                     
511200                                                                          
511300     EXEC SQL                                                             
511400           SELECT  DISTINCT                                               
511500                   IDDC_REC                                               
511600                                                                          
511700           INTO   :TP4TRAN-IDDC-REC                                       
511800                                                                          
511900           FROM    TP4TRAN                                                
512000                                                                          
512100           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
512200     END-EXEC                                                             
512300                                                                          
512400     MOVE SQLCODE TO SQLCODE-WS                                           
512500     PERFORM DB2-STATUSKONTROLL                                           
512600     .                                                                    
512700     EJECT                                                                
512800 IMS-STATUSKONTROLL SECTION.                                              
512900     SET STATUS-IX TO 1                                                   
513000     SEARCH GODK-STATUS                                                   
513100       AT END                                                             
513200         CALL FELLOG                                                      
513300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
513400         CONTINUE                                                         
513500     END-SEARCH                                                           
513600     .                                                                    
513700     EJECT                                                                
513800 DB2-STATUSKONTROLL  SECTION.                                             
513900                                                                          
514000     SET SQLCODE-IX TO 1                                                  
514100     SEARCH GODK-SQLCODE                                                  
514200       AT END                                                             
514300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
514400          DELIMITED BY SIZE INTO FELTEXT                                  
514500          CALL ABEND USING RKOD-ABEND-DB2                                 
514600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
514700     END-SEARCH                                                           
514800     .                                                                    
