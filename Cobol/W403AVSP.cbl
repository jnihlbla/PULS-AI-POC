000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W403AVSP.                                                
000500 AUTHOR.         BERT ANDERSSON.                                          
000600 DATE-WRITTEN.   JANUARI  96.                                             
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION.                                                            
001000*        ORDERDELS PACKNING - PACK-RAPPORTERING AV EN ORDERDEL OCH        
001100*        OM DET ÄR SISTA (ELLER DEN ENDA) ORDERDELEN ÄVEN AVSLUT-         
001200*        NING AV PACK-RAPPORTERINGEN.                                     
001300*                                                                         
001400*        DESSUTOM RAPPORTERAS UPPGIFTER OM KOLLIT.                        
001500*                                                                         
001600*        W403AVSP ÄR KONSTRUERAT MED VALDA DELAR UR PROGRAM               
001700*        W40315 (PACKRAPPORTERING) OCH W40397 (AVSLUT PACKNING).          
001800*                                                                         
001900*        SKAPAR SALDOLOGG PÅ BAS WDL9/WLLOGA                              
002000*                                                                         
002100*    LÄNKAREA :       W403AVSP                                            
002200*                                                                         
002300*    CHANGE LOG                                                           
002400*                                                                         
002500*    DIGAMBAR/021011                                                      
002600*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
002700*    THE RESPONSE TIME OF THE SCREEN 4312.                                
002800*                                                                         
002900* ETRACK 1290414  INTERVALL FOR DISTRICT AND CUSTOMER                     
003000* ETRACK 7450328  2008-HÖST  VOHF                                         
003100* ETRACK 10143273 2012-09  LOCAL SOURCING CHINA                           
003200* ETRACK 10130993 150422   REDUCE NUMBER OF DELIVERY SCHEDULES            
003300* ETRACK 10254592 2015       DECOMISSION VOHF                             
003400* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
003500*                                                                         
003600*                                                                         
003700 DATA DIVISION.                                                           
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77    IDPGM                     PIC X(8)    VALUE 'W403AVSP'.            
004300 77    FELTEXT                   PIC X(64)   VALUE SPACE.                 
004400 77    CURRENT-SECTION           PIC X(30)   VALUE SPACE.                 
004500 77    W-IDDC                    PIC X(2)    VALUE SPACE.                 
004600 77    WS-PGM-POSITION           PIC X(32)   VALUE SPACE.                 
004700 77    JA                        PIC X       VALUE 'J'.                   
004800 77    YES                       PIC X       VALUE 'Y'.                   
004900 77    NEJ                       PIC X       VALUE 'N'.                   
005000 77    DEF-IDROLL                PIC X(5)    VALUE 'VOR99'.               
005100 77    DATUM-SW                  PIC X       VALUE 'N'.                   
005200 77    RAETT                     PIC X       VALUE 'R'.                   
005300 77    FEL                       PIC X       VALUE 'F'.                   
005400 77    SAKNAS                    PIC X       VALUE 'S'.                   
005500 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
005600 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77    REST-INX                  PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    FG-MAX-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
006200 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
006300 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +366 COMP SYNC.        
006400 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
006500 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
006600 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
006700 77    WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.          
006800 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
006900 77    WS-IDORDER                PIC S9(7)  COMP-3.                       
007000 77    WS-KDRAPRIO               PIC S9(3)  COMP-3.                       
007100 77    WS-KDORDBEK               PIC 9(2).                                
007200 77    WS-SUMMA                  PIC S9(7)   VALUE +0.                    
007300 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
007400 77    W-SPAR-IDDC               PIC X(2).                                
007500 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
007600 77    WS-EMBPROF                PIC X(1)   VALUE SPACE.                  
007700 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
007800 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
007900 77    W-LOGG-DATUM              PIC S9(8)  VALUE ZERO.                   
008000 77    W-LOGG-TID                PIC S9(9)  VALUE ZERO.                   
008100 77    W-KVKOLLI                 PIC S9(7)  VALUE ZERO  COMP-3.           
008200 77    W-KVKOLLI-FAKT            PIC S9(7)  VALUE ZERO  COMP-3.           
008300 77    W-KVKOLLI-LAST            PIC S9(7)  VALUE ZERO  COMP-3.           
008400 77    W-KVKOLLI-FL              PIC S9(7)  VALUE ZERO  COMP-3.           
008500 77    S03-IDROLL                PIC X(5)    VALUE SPACE.                 
008600 77    S28-IDARTNR               PIC 9(9)    VALUE ZERO COMP-3.           
008700 77    S28-IDDC                  PIC X(2)    VALUE SPACE.                 
008800 77    S28-KVVORKO               PIC S9(7)   VALUE ZERO COMP-3.           
008900 77    S28-IDANSK                PIC 9(3)    VALUE ZERO COMP-3.           
009000 77    S28-IDLEVNR               PIC X(5)    VALUE SPACE.                 
009100                                                                          
009200*01   -COPY WWDCKONS                                                      
009300                                                                          
009400 77    WS-VOR-TID-BRIST          PIC 9(9)   VALUE ZERO.                   
009500 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
009600 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
009700 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
009800 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
009900 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
010000 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
010100 77    WS-IDDISTR-NUM4           PIC 9(4)   VALUE ZERO.                   
010200 77    WS-SAVE-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
010300 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
010400 77    WS-IDPLKLST-NUM           PIC  9(3)  VALUE ZERO.                   
010500 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
010600 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
010700 77    WS-IDKOLLI-SAMP           PIC 9(5)   VALUE ZERO.                   
010800 77    WS-SAVE-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
010900 77    WS-TIORDREG               PIC S9(7) VALUE ZERO COMP-3.             
011000 77    WS-TIORDREG-NUM6          PIC 9(6)  VALUE ZERO.                    
011100 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
011200 77    WS-FLLSBOK                PIC X(1)   VALUE SPACE.                  
011300 77    WS-ODEL-IDPRC             PIC X(4)   VALUE SPACE.                  
011400 77    WS-FLORDSPE               PIC X(1)   VALUE SPACE.                  
011500 77    WS-FLOVRLEV               PIC X(1)   VALUE SPACE.                  
011600 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
011700 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
011800 77    WS-IDPRODNR-RED           PIC Z(6)9  VALUE ZERO.                   
011900 77    WS-IDPURAD                PIC 9(4)   VALUE ZERO.                   
012000 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
012100 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
012200 77    WS-KDKOLLI                PIC X(8)   VALUE SPACE.                  
012300 77    SOEK-VIA-PRODNR           PIC X      VALUE 'N'.                    
012400 77    FILLER                    PIC X(8)   VALUE 'FFFFFFFF'.             
012500 77    WS-KDTPOTYP               PIC S9     COMP-3.                       
012600 77    WS-KDFRAKT                PIC  9(3)  VALUE ZERO.                   
012700 77    WS-KDFRAKT-NUM2           PIC  9(2)  VALUE ZERO.                   
012800 77    WS-VKORDBTO               PIC 9(6)V9 VALUE ZERO.                   
012900 77    WS-VLORDBTO               PIC 9(4)V9(3)  VALUE ZERO.               
013000 77    WS-KDEMBTYP               PIC 9(2)   VALUE ZERO.                   
013100 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
013200 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
013300 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
013400 77    WS-KDKOLLID               PIC X(1)   VALUE 'L'.                    
013500 77    WS-ADFLGEO                PIC X(3)   VALUE SPACE.                  
013600 77    WS-ADFLOMR                PIC 9(3)   VALUE ZERO.                   
013700 77    WS-ADRUTNIV               PIC 9(3)   VALUE ZERO.                   
013800 77    WS-KVSLATTAT              PIC S9(7)  COMP-3  VALUE ZERO.           
013900 77    SW-TIRODAT-LIKA-MED-ZERO  PIC  X(1)   VALUE 'N'.                   
014000 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
014100 77    SPAR-KART-KVRESS-ART      PIC S9(7)  COMP-3.                       
014200 77    WS-TRAEFF-PACKARE         PIC X(01).                               
014300 77    MAX-RAD-ANTAL-PLUS-1      PIC S9(3)  VALUE +13  COMP-3.            
014400 77    WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.            
014500 77    WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.            
014600 77    FILLER                    PIC X(8)    VALUE 'HHHHHHHH'.            
014700 77    WS-SAMMANSLAGNING-RAD     PIC X.                                   
014800 77    ANTAL-EJ-PACKRAP-ORDDEL   PIC 9(1)   VALUE ZERO.                   
014900 77    WS-DARFS                  PIC 9(12)  VALUE ZERO.                   
015000*                                        ANTAL FÄRDIGPACKADE RADER        
015100*                                        I ETT RAD-INTERVALL.             
015200 77    WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.            
015300 77    FILLER                    PIC X(8)    VALUE 'IIIIIIII'.            
015400 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
015500 77    WS-KVORDRAD-PACK          PIC S9(5)   VALUE +0    COMP-3.          
015600 77    WS-KVORDRAD               PIC S9(5)   VALUE +0    COMP-3.          
015700 77    WS-KVORAPP-TOTAL          PIC S9(6)   VALUE +0.                    
015800 77    WS-KVORAPP-PACK           PIC S9(6)   VALUE +0.                    
015900 77    WS-KVPRERO                PIC S9(7)   VALUE +0.                    
016000 77    WS-AVVIKELSE-UTSKR        PIC S9(7)   VALUE +0.                    
016100 77    WS-TIDISPIN               PIC S9(7)  COMP-3.                       
016200 77    WS-ALLA-ODEL-UTSKRIVNA    PIC X(1).                                
016300 77    WS-FLAUTFAK               PIC X(01).                               
016400 77    KDRC-DISP                 PIC 9(4)    VALUE ZERO.                  
016500 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
016600 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
016700 77    WS-IDPRQUES               PIC 9(7)    VALUE ZERO.                  
016800 77    WS-IDANSK                 PIC 9(3)   VALUE ZERO.                   
016900 77    WS-OHUV-IDSYSTEM          PIC X(4)   VALUE SPACE.                  
017000 77    WS-ORAD-KVLEVART          PIC 9(7)    VALUE ZERO.                  
017001                                                                          
017002 01   WS-IDTRPTNR               PIC S9(3)   VALUE ZERO.                   
017003 01   WS-ADFLGEO                PIC X(3)    VALUE SPACE.                  
017004 01   WS-ADFLOMR                PIC S9(3)   VALUE ZERO.                   
017005 01   WS-ADRUTNIV               PIC S9(3)   VALUE ZERO.                   
017006 01   WS-DIHMODUL               PIC S9(3)   VALUE ZERO.                   
017007 01   WS-DIDMODUL               PIC S9(3)   VALUE ZERO.                   
017008 01   WS-ADVMODUL               PIC S9(3)   VALUE ZERO.                   
017009 01   WS-ADHMODUL               PIC S9(3)   VALUE ZERO.                   
017010 01   WS-FLUTLAST               PIC X       VALUE SPACE.                  
017011 01   WS-IDDC-CROSS             PIC X(2)    VALUE SPACE.                  
017020                                                                          
017030 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
017040 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
017050 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
017060                                                                          
017070 01  WS-DCUSER.                                                           
017080     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
017090     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
017100     03 FILLER                   PIC X(1)   VALUE SPACE.                  
017200                                                                          
017300 01    WS-TID-W.                                                          
017400   03  WS-TTMMSS                 PIC 9(6).                                
017500   03  WS-HH                     PIC 9(2).                                
017600 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
017700                                                                          
017800 77    WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
017900 77    WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
018000 77    WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                   
018100 77    WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                  
018200 77    WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.            
018300 77    WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                  
018400 77    WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                  
018500 77    WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
018600 77    WS-IDLEVNR                PIC X(5)   VALUE SPACES.                 
018700 77    WS-ODEL-IDDC-EXP          PIC X(2)   VALUE SPACES.                 
018800                                                                          
018900 01     WS-IDKUNDRF.                                                      
019000   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
019100   03   FILLER                   PIC X(5)   VALUE SPACE.                  
019200                                                                          
019300 01     WS-IDKUNDRF-OLD.                                                  
019400   03   WS-IDORDNR5-OLD          PIC 9(5).                                
019500   03   FILLER                   PIC X(5)   VALUE SPACE.                  
019600                                                                          
019700 01     WS-IDKUNDRF-NEW.                                                  
019800   03   WS-IDORDNR7-NEW          PIC 9(7).                                
019900   03   FILLER                   PIC X(3)   VALUE SPACE.                  
020000*                                                                         
020100 01   WS-TRP-GRP.                                                         
020200   03 WS-TRP-SEKEL             PIC 9(02)  VALUE ZERO.                     
020300   03 WS-TRP-YEAR              PIC 9(02)  VALUE ZERO.                     
020400   03 WS-TRP-MONTH             PIC 9(02)  VALUE ZERO.                     
020500   03 WS-TRP-DAY               PIC 9(02)  VALUE ZERO.                     
020600   03 WS-TRP-HOUR              PIC 9(02)  VALUE ZERO.                     
020700   03 WS-TRP-MIN               PIC 9(02)  VALUE ZERO.                     
020800*                                                                         
020900 01    WS-DATRP-GRP.                                                      
021000   03  WS-DATRP-SEKEL            PIC 9(02)  VALUE ZERO.                   
021100   03  WS-DATRP-DATE.                                                     
021200     05 WS-DATRP-YEAR            PIC 9(02)  VALUE ZERO.                   
021300     05 FILLER                   PIC X(01)  VALUE '-'.                    
021400     05 WS-DATRP-MONTH           PIC 9(02)  VALUE ZERO.                   
021500     05 FILLER                   PIC X(01)  VALUE '-'.                    
021600     05 WS-DATRP-DAY             PIC 9(02)  VALUE ZERO.                   
021700     05 FILLER                   PIC X(01)  VALUE 'T'.                    
021800   03 WS-DATRP-TIME.                                                      
021900     05 WS-DATRP-HOUR            PIC 9(02)  VALUE ZERO.                   
022000     05 FILLER                   PIC X(01)  VALUE ':'.                    
022100     05 WS-DATRP-MIN             PIC 9(02)  VALUE ZERO.                   
022200     05 FILLER                   PIC X(01)  VALUE ':'.                    
022300     05 WS-DATRP-SEC             PIC 9(02)  VALUE ZERO.                   
022400     05 FILLER                   PIC X(01)  VALUE ':'.                    
022500     05 WS-DATRP-HUN             PIC 9(02)  VALUE ZERO.                   
022600     05 FILLER                   PIC X(01)  VALUE 'Z'.                    
022700*                                                                         
022800                                                                          
022900 01  ARBETSFALT.                                                          
023000                                                                          
023100     03 WS-DAORDREG              PIC 9(8) VALUE ZERO.                     
023200     03 WS-DAORDREG-DELAR        REDEFINES WS-DAORDREG.                   
023300        05 WS-DAORDREG-TISS      PIC 9(2).                                
023400        05 WS-DAORDREG-TIAAMMDD  PIC 9(6).                                
023500                                                                          
023600     03 WS-DARODAT               PIC 9(8) VALUE ZERO.                     
023700     03 WS-DARODAT-DELAR         REDEFINES WS-DARODAT.                    
023800        05 WS-DARODAT-TISS       PIC 9(2).                                
023900        05 WS-DARODAT-TIAAMMDD   PIC 9(6).                                
024000                                                                          
024100     03 WS-9KOMPL-GRUND          PIC 9(9) VALUE 999999999.                
024200*                                                                         
024300 77    FILLER                    PIC X(8)    VALUE 'KKKKKKKK'.            
024400                                                                          
024500 01    FL-420-SEGMENT            PIC X(01)  VALUE 'N'.                    
024600   88  ORAPPORTERADE-RADER-FINNS            VALUE 'J'.                    
024700   88  ORAPPORTERADE-RADER-SAKNAS           VALUE 'N'.                    
024800     SKIP2                                                                
024900 01    SW-FLAUTFAK               PIC X(01).                               
025000   88  AUT-FAK-SKRIVS-EJ-UT                 VALUE 'N'.                    
025100   88  AUT-FAKTURA-SKRIVS-UT                VALUE 'J'.                    
025200     SKIP2                                                                
025300 01    KDORDSTA-SW               PIC X(01).                               
025400   88  KDORDSTA-KLAR                        VALUE 'J'.                    
025500   88  KDORDSTA-EJ-KLAR                     VALUE 'N'.                    
025600     SKIP2                                                                
025700*                                                                         
025800 01  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
025900   88 TRAFF-VORKO                            VALUE 'J'.                   
026000*                                                                         
026100 77    ABEND-MED1                PIC X(80)  VALUE  SPACE.                 
026200     SKIP2                                                                
026300                                                                          
026400 77    WS-IDTRANS                PIC X(04).                               
026500   88  WS-GODKAND-BILD                      VALUE '0121', '4313'.         
026600                                                                          
026700     SKIP2                                                                
026800 77    WS-INDATA-TEST            PIC X(01).                               
026900   88  WS-INDATA-FEL                        VALUE 'F'.                    
027000   88  WS-INDATA-RATT                       VALUE 'R'.                    
027100     SKIP2                                                                
027200 77    WS-BEHANDLING-TEST        PIC X(01).                               
027300   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
027400   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
027500     SKIP2                                                                
027600 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
027700 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
027800   03   WS-SUPTID-TIM           PIC 9(3).                                 
027900   03   WS-SUPTID-MIN           PIC 9(2).                                 
028000*                                                                         
028100 01  S27-IDANSK-X.                                                        
028200     03 S27-IDANSK               PIC 9(3).                                
028300                                                                          
028400 01  S27-IDARTNR-X.                                                       
028500     03 S27-IDARTNR              PIC 9(9).                                
028600                                                                          
028700 01  S27-IDDISTR-X.                                                       
028800     03 S27-IDDISTR              PIC 9(4).                                
028900                                                                          
029000 01  S27-IDKUNDNR-X.                                                      
029100     03 S27-IDKUNDNR             PIC 9(6).                                
029200                                                                          
029300 01 DB2-LASNING.                                                          
029400     03 FILLER                   PIC X(16)   VALUE                        
029500                                             'WS-DB2-SEKTION'.            
029600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
029700                                                                          
029800     EJECT                                                                
029900 01 NYCKLAR-TP4TRAN.                                                      
030000     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
030100                                                                          
030200                                                                          
030300     SKIP2                                                                
030400 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREA '.           
030500 01     SPAR-AREA.                                                        
030600*                                                                         
030700   03   SPAR-AREA-GRP.                                                    
030800     05 SPAR-VKORDNTO           PIC  9(6)V9(3)    VALUE ZERO.             
030900     05 SPAR-VKORDNTO-TOT       PIC  9(6)V9(3)    VALUE ZERO.             
031000     05 SPAR-VKORDNTO-DEL       PIC  9(6)V9(3)    VALUE ZERO.             
031100     05 SPAR-VLORDNTO           PIC  9(4)V9(3)    VALUE ZERO.             
031200     05 SPAR-VLORDNTO-TOT       PIC  9(4)V9(3)    VALUE ZERO.             
031300     05 SPAR-VLORDNTO-DEL       PIC  9(4)V9(3)    VALUE ZERO.             
031400     05 SPAR-KVKOLLI            PIC  S9(5)        VALUE ZERO.             
031500     05 SPAR-KVKOLPAC           PIC  S9(5)        VALUE ZERO.             
031600     05 SPAR-KVKOLLI-FAKT       PIC  S9(5)        VALUE ZERO.             
031700     05 SPAR-SUORDV             PIC  S9(9)V9(2)   VALUE ZERO.             
031800     05 SPAR-SUORDV-EXP         PIC  S9(9)V9(2)   VALUE ZERO.             
031900     05 SPAR-SUORDV-LOC         PIC  S9(9)V9(2)   VALUE ZERO.             
032000     05 SPAR-SUORDV-LOCPREL     PIC  S9(9)V9(2)   VALUE ZERO.             
032100     05 SPAR-SUORDV-TOT         PIC  S9(9)V9(2)   VALUE ZERO.             
032200     05 SPAR-SUORDV-TOT-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
032300     05 SPAR-SUORDV-TOT-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
032400     05 SPAR-SUORDV-DEL         PIC  S9(9)V9(2)   VALUE ZERO.             
032500     05 SPAR-SUORDV-DEL-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
032600     05 SPAR-SUORDV-DEL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
032700     05 SPAR-IDPRODNR           PIC S9(7)         VALUE ZERO.             
032800     05 SPAR-BEKUNDRF           PIC X(15)         VALUE SPACE.            
032900     05 SPAR-KDVALISO           PIC X(3)          VALUE SPACE.            
033000     05 SPAR-KDVALISO-EXP       PIC X(3)          VALUE SPACE.            
033100*                                                                         
033200 01     DYNAMISKA-SUBPROGRAM.                                             
033300     03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
033400     03 FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
033500     03 ABEND                   PIC X(8)    VALUE 'ABEND   '.             
033600     03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.             
033700     03 W403PLAT                PIC X(8)    VALUE 'W403PLAT'.             
033800     03 W005INIT                PIC X(8)    VALUE 'W005INIT'.             
033900     03 W411DNOT                PIC X(8)    VALUE 'W411DNOT'.             
034000     03 W335PRQU                PIC X(8)    VALUE 'W335PRQU'.             
034100     03 W335PRNO                PIC X(8)    VALUE 'W335PRNO'.             
034200     03 W403TMS1                PIC X(8)    VALUE 'W403TMS1'.             
034300*                                                                         
034400     SKIP2                                                                
034500 01    ABENDKODER.                                                        
034600     03 FILLER                  PIC X(16) VALUE 'ABENDKODER'.             
034700     03 RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.            
034800     03 RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +33.            
034900     03 RKOD-FELTEXT            PIC X(32) VALUE SPACE.                    
035000     EJECT                                                                
035100*                                                                         
035200*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
035300*                                                                         
035400 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
035500*01 -COPY W403PLAT                                                        
035600     EJECT                                                                
035700 01  FILLER                     PIC X(16)   VALUE 'WMSGINIT '.            
035800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
035900*01 -COPY WMSGINIT                                                        
036000*                                                                         
036100 01  FILLER                      PIC X(16)  VALUE 'W411DNOT '.            
036200*01 -COPY W411DNOT                                                        
036300     EJECT                                                                
036400*                                                                         
036500 01  FILLER                      PIC X(8)   VALUE  'W335PRQU'.            
036600*    -COPY W335PRQU                                                       
036700*                                                                         
036800 01  FILLER                      PIC X(8)   VALUE  'W335PRNO'.            
036900*    -COPY W335PRNO                                                       
037000*                                                                         
037100*TMS PACKNING INFO                                                        
037200 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
037300*    -COPY W403TMS1                                                       
037400*                                                                         
037500 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
037600*                                                                         
037700 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
037800 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
037900 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
038000   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
038100   03   FILLER                  PIC  X(4).                                
038200     SKIP2                                                                
038300 01     HJALP-4472-TIRFS        PIC 9(11).                                
038400 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
038500   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
038600   03   FILLER                  PIC  X(4).                                
038700     EJECT                                                                
038800 01     FILLER                  PIC X(10)   VALUE 'SPAR-PRAD-'.           
038900 01     SPAR-PRAD.                                                        
039000*                                                                         
039100   03   SPAR-PRAD-UPPG-AREA.                                              
039200     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
039300     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
039400     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
039500     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
039600     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
039700     05 SPAR-PRAD-PRARTNTO-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.          
039800     05 SPAR-PRAD-VKARTNTO      PIC  S9(4)V9(3)   VALUE ZERO.             
039900     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
040000*                                                                         
040100   03   SPAR-FARLIGT-GODS-DATA.                                           
040200     05 SPAR-IDPSN              PIC  9(3)                VALUE 0.         
040300     05 SPAR-VKART-FG           PIC  S9(7)        COMP-3 VALUE 0.         
040400     05 SPAR-VLFG               PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
040500     05 SPAR-SUEQFG             PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
040600     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
040700     EJECT                                                                
040800 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
040900 01     ARBETSAREOR.                                                      
041000   03   ARB-KOLLI-UPPG-AREA.                                              
041100     05 ARB-KOLLI-VKORDNTO      PIC  9(6)V9(1)    VALUE ZERO.             
041200     05 ARB-KOLLI-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
041300     05 ARB-KOLLI-KDFARLIG      PIC  S9           VALUE ZERO.             
041400     05 ARB-KOLLI-KVORDRAD      PIC  S9(5)        VALUE ZERO.             
041500     05 ARB-KOLLI-KVFALRAD      PIC  S9(5)        VALUE ZERO.             
041600     05 ARB-KOLLI-SUORDV        PIC  S9(9)V9(2)   VALUE ZERO.             
041700     05 ARB-KOLLI-SUORDV-EXP    PIC  S9(9)V9(2)   VALUE ZERO.             
041800     05 ARB-KOLLI-SUORDV-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
041900     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
042000     05 ARB-KOLLI-KDVALISO      PIC X(3)          VALUE SPACE.            
042100     05 ARB-KOLLI-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
042200     SKIP2                                                                
042300   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5) COMP-3  VALUE ZERO.             
042400   03   ARB-ANTAL-KOLLI          PIC 9(5) COMP-3  VALUE ZERO.             
042500     SKIP2                                                                
042600   03   ARB-ADRESS.                                                       
042700     05 ARB-ADFLGEO             PIC  X(3)   VALUE SPACE.                  
042800     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
042900     05 ARB-ADFLOMR             PIC  9(3)   VALUE ZERO.                   
043000     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
043100     05 ARB-ADRUTNIV            PIC  9(3)   VALUE ZERO.                   
043200     SKIP2                                                                
043300 01     WS-TIDPUNKT-RED.                                                  
043400   03   WS-HHMMSS               PIC  9(6).                                
043500   03   WS-DD                   PIC  9(2).                                
043600     EJECT                                                                
043700 01    FILLER                   PIC X(20)  VALUE 'SPAR-ORAD-AREA'.        
043800*01      WDE411   -COPY WDE411  -PRE SPAR-.                               
043900*                                                                         
044000     EJECT                                                                
044100 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
044200 01     FILLER REDEFINES TEST-IDDISTR.                                    
044300*  03   -COPY WWDIST03.                                                   
044400     SKIP2                                                                
044500 01     FILLER REDEFINES TEST-IDDISTR.                                    
044600*  03   -COPY WWDIST07.                                                   
044700     SKIP2                                                                
044800 01     FILLER REDEFINES TEST-IDDISTR.                                    
044900*  03   -COPY WWDIST18.                                                   
045000     SKIP2                                                                
045100 01     FILLER REDEFINES TEST-IDDISTR.                                    
045200*  03   -COPY WWDIST19.                                                   
045300     SKIP2                                                                
045400 01     FILLER REDEFINES TEST-IDDISTR.                                    
045500*  03   -COPY WWDIST20.                                                   
045600     SKIP2                                                                
045700 01     FILLER REDEFINES TEST-IDDISTR.                                    
045800*  03   -COPY WWDIST21.                                                   
045900     SKIP2                                                                
046000 01     FILLER REDEFINES TEST-IDDISTR.                                    
046100*  03   -COPY WWDIST35.                                                   
046200     EJECT                                                                
046300 01     FILLER REDEFINES TEST-IDDISTR.                                    
046400*  03   -COPY WWDIST47.                                                   
046500     EJECT                                                                
046600 01     FILLER REDEFINES TEST-IDDISTR.                                    
046700*  03   -COPY WWDIS128.                                                   
046800     EJECT                                                                
046900 01     FILLER REDEFINES TEST-IDDISTR.                                    
047000*  03   -COPY WWDIST79.                                                   
047100     EJECT                                                                
047200 01     FILLER REDEFINES TEST-IDDISTR.                                    
047300*  03   -COPY WWDIST85.                                                   
047400     EJECT                                                                
047500 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
047600*   -COPY WWDIST57                                                        
047700     EJECT                                                                
047800 01  FILLER                      PIC X(08)  VALUE 'TESTKUND'.             
047900 01  FILLER                      PIC X(08)  VALUE 'WWFRAKT1'.             
048000*01     FILLER  -COPY WWFRAKT1                                            
048100     EJECT                                                                
048200 01     DAGDAT.                                                           
048300   03   DAGDAT-AAMMD             PIC 9(6).                                
048400   03   DAGDAT-AAMMDD-X      REDEFINES DAGDAT-AAMMD.                      
048500     05 DAGDAT-AAMMDD-AA         PIC 9(2).                                
048600     05 DAGDAT-AAMMDD-MM         PIC 9(2).                                
048700     05 DAGDAT-AAMMDD-DD         PIC 9(2).                                
048800*                                                                         
048900   03   DAGDAT-AAVVD             PIC 9(5).                                
049000   03   DAGDAT-AAVVD-X       REDEFINES DAGDAT-AAVVD.                      
049100     05 DAGDAT-AAVVD-AA          PIC 9(2).                                
049200     05 DAGDAT-AAVVD-VV          PIC 9(2).                                
049300     05 DAGDAT-AAVVD-D           PIC 9(1).                                
049400*                                                                         
049500*01  WDATAREA      -COPY WDATAREA.                                        
049600     EJECT                                                                
049700***************************************************************           
049800 01    NYCKLAR-TILL-DLI.                                                  
049900*                                                                         
050000*  03    -COPY WDGX01                                                     
050100*                                                                         
050200   03    W-WDE4A1-KUNDORDER-X.                                            
050300     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
050400     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
050500     05    W-4A1-IDKUNDRF.                                                
050600       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
050700       07  FILLER                PIC X(05)   VALUE SPACE.                 
050800*                                                                         
050900   03    W-WDE401-KUNDORDER-X.                                            
051000     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
051100     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
051200     05    W-401-IDKUNDRF.                                                
051300       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
051400       07  FILLER                PIC X(05)   VALUE SPACE.                 
051500     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
051600     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
051700*                                                                         
051800   03    W-WDE4B-KEYSEQ-MIN-X.                                            
051900     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
052000     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
052100*                                                                         
052200   03    W-WDE4B-KEYSEQ-MAX-X.                                            
052300     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
052400     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
052500*                                                                         
052600   03    W-WDE4B-KEYSEQ-X.                                                
052700     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
052800     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
052900*                                                                         
053000   03    W-WDE4ASEQ-X.                                                    
053100    05     W-E4ASEQ-IDGMTREF         PIC X(17)  VALUE SPACE.              
053200                                                                          
053300   03    W-WDE411-IDPURAD-X.                                              
053400     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
053500*                                                                         
053600   03    W-WDE421-IDKOLLI-X.                                              
053700     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
053800     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
053900     SKIP2                                                                
054000   03    W-WDE601-IDPRODNR-X.                                             
054100     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
054200*                                                                         
054300   03    W-WDE611-IDKOLLI-X.                                              
054400     05    W-610-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
054500     SKIP2                                                                
054600   03    W-WDA601KY-MIN-X.                                                
054700     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
054800     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
054900     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
055000     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
055100     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
055200     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
055300     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
055400     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
055500     SKIP2                                                                
055600   03    W-WDA601KY-MAX-X.                                                
055700     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
055800     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
055900     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
056000     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
056100     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
056200     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
056300     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
056400     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
056500*                                                                         
056600     03  W-WDQ101KY-MIN-X.                                                
056700         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
056800         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
056900         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
057000         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
057100         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
057200                                                                          
057300     03  W-WDQ101KY-MAX-X.                                                
057400         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
057500         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
057600         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
057700         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
057800         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
057900*                                                                         
058000   03    W-WDQ201-X.                                                      
058100     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
058200*                                                                         
058300   03  W-WDQ2CSEQ-X.                                                      
058400       05  W-WDQ2CSEQ-IDGMTREF.                                           
058500           07  W-WDQ2CSEQ-IDDISTR                                         
058600                               PIC S9(5) COMP-3 VALUE +0.                 
058700           07  W-WDQ2CSEQ-IDKUNDNR                                        
058800                               PIC S9(7) COMP-3 VALUE +0.                 
058900           07  W-WDQ2CSEQ-IDKUNDRF                                        
059000                               PIC X(10)     VALUE '0000000   '.          
059100*                                                                         
059200   03    W-WDQ2C-X.                                                       
059300     05    W-WDQ2C-IDGMTREF-X.                                            
059400       07    W-WDQ2C-IDDISTR     PIC S9(5) COMP-3 VALUE +0.               
059500       07    W-WDQ2C-IDKUNDNR    PIC S9(7) COMP-3 VALUE +0.               
059600       07    W-WDQ2C-IDKUNDRF.                                            
059700         09    FILLER            PIC  9(2)        VALUE ZERO.             
059800         09    W-WDQ2C-IDORDNR5                                           
059900                                 PIC  9(5)        VALUE ZERO.             
060000         09    FILLER            PIC  X(3)        VALUE SPACE.            
060100*                                                                         
060200   03    W-WDQ301-KEY-X.                                                  
060300     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
060400     05    W-WDQ301-IDDC         PIC X(2).                                
060500     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
060600     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
060700*                                                                         
060800   03  W-Q301-KEY-MIN-X.                                                  
060900         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
061000         05  W-Q301-MIN-IDDC     PIC X(2).                                
061100         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
061200         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
061300*                                                                         
061400   03  W-Q301-KEY-MAX-X.                                                  
061500         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
061600         05  W-Q301-MAX-IDDC     PIC X(2).                                
061700         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
061800         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
061900*                                                                         
062000   03  W-WDQ301KY-MIN.                                                    
062100         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
062200         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
062300         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
062400*                                                                         
062500   03    W-WDQ301-ORDERDEL-X.                                             
062600     05    W-301-IDORDER         PIC S9(7) COMP-3.                        
062700     05    W-301-IDDC            PIC X(2).                                
062800     05    W-301-IDPRODNR        PIC S9(7) COMP-3.                        
062900     05    W-301-IDPLKLST        PIC S9(3) COMP-3.                        
063000*                                                                         
063100   03  W-KDODELST                PIC X.                                   
063200*                                                                         
063300   03    W-IDARTNR-X.                                                     
063400     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
063500*                                                                         
063600   03    W-KDSEGKEY-X.                                                    
063700     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
063800*                                                                         
063900   03    W-WDK711-IDDC-X.                                                 
064000     05    W-711-IDDC            PIC X(2).                                
064100*                                                                         
064200   03  W-IDLAND-X.                                                        
064300      05  W-IDLAND               PIC X(2).                                
064400*                                                                         
064500   03    W-WDK901-IDARTNR-X.                                              
064600     05    W-901-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.          
064700*                                                                         
064800   03    W-WDG6KEY-X.                                                     
064900     05    W-RDG-TIAAMMDD        PIC 9(6)    VALUE ZERO.                  
065000     05    W-RDG-TIKLOCK         PIC 9(8)    VALUE ZERO.                  
065100     05    W-RDG-IDLOGLOP        PIC 9(1)    VALUE ZERO.                  
065200     05    W-RDG-IDPTYP          PIC X(3)    VALUE 'RY1'.                 
065300*                                                                         
065400   03    W-WDGX11-WDGXKEY-X.                                              
065500     05    W-RDG-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
065600     05    W-RDG-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
065700     05    W-RDG-IDDC            PIC X(2).                                
065800     05    W-RDG-KDFAKTYP        PIC X(1)    VALUE SPACE.                 
065900*                                                                         
066000     03  W-WDQ301KY-MAX.                                                  
066100         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
066200         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
066300         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
066400*                                                                         
066500   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
066600*                                                                         
066700     03  W-WDE6E1KY-MIN-X.                                                
066800         05 W-E6E-IDDC-MIN     PIC  X(2)   VALUE SPACE.                   
066900         05 W-E6E-IDKOLLIS-MIN PIC S9(5)   VALUE ZERO  COMP-3.            
067000         05 FILLER             PIC  X(7)   VALUE LOW-VALUE.               
067100     03  W-WDE6E1KY-MAX-X.                                                
067200         05 W-E6E-IDDC-MAX     PIC  X(2)   VALUE SPACE.                   
067300         05 W-E6E-IDKOLLIS-MAX PIC S9(5)   VALUE ZERO  COMP-3.            
067400         05 FILLER             PIC  X(7)   VALUE HIGH-VALUE.              
067500                                                                          
067600*                                                                         
067700   03    W-2203-X.                                                        
067800     05    W-2203-IDHTYP         PIC X(4) VALUE '2203'.                   
067900     05    W-2203-IDDC           PIC X(2).                                
068000     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
068100*                                                                         
068200   03    W-4321-IDHTYP-X.                                                 
068300       05    W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
068400       05    W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
068500*                                                                         
068600   03    W-4447-X.                                                        
068700     05    FILLER                PIC X(4)  VALUE '4447'.                  
068800     05    W-4447-IDDC           PIC X(2).                                
068900     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
069000*                                                                         
069100   03    W-4448-X.                                                        
069200     05    W-4448-IDPRC          PIC X(4).                                
069300     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
069400*                                                                         
069500   03    W-4471-WDGXKEY-X.                                                
069600     05    W-4471-IDHTYP         PIC X(4)  VALUE '4471'.                  
069700     05    W-4471-IDDC           PIC X(2).                                
069800     05    W-4471-IDPRC.                                                  
069900       07    W-4471-IDPRCBAS     PIC X(3).                                
070000       07    W-4471-IDPRCVAR     PIC X(1).                                
070100     05    FILLER                PIC X(20) VALUE LOW-VALUE.               
070200*                                                                         
070300   03    W-4472-KDSEGKEY-X.                                               
070400     05    W-4472-KDSEGKEY       PIC X(1)  VALUE '1'.                     
070500*                                                                         
070600   03    W-4477-WDGXKEY-X.                                                
070700     05    W-4477-IDHTYP         PIC X(4)  VALUE '4477'.                  
070800     05    W-4477-IDDC           PIC X(2).                                
070900     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
071000*                                                                         
071100   03    W-4478-WDGXKEY-X.                                                
071200     05    W-4478-IDSHIFT        PIC X(1).                                
071300     05    W-4478-IDUSER         PIC X(8).                                
071400     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
071500*                                                                         
071600   03    W-4487-X.                                                        
071700     05    FILLER                PIC X(4)  VALUE '4487'.                  
071800     05    W-4487-IDDC           PIC X(2).                                
071900     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
072000*                                                                         
072100   03    W-KDPRCGRP-X.                                                    
072200     05    W-4488-KDPRCGRP       PIC X(5).                                
072300*                                                                         
072400   03    W-4490-X.                                                        
072500     05    W-4490-DARFS          PIC 9(12).                               
072600     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
072700     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
072800     EJECT                                                                
072900   03  W-WDM201-X.                                                        
073000       05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.            
073100       05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                  
073200                                                                          
073300   03  W-WDM211-X.                                                        
073400       05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.            
073500                                                                          
073600   03  W-WDM221-X.                                                        
073700       05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.             
073800       05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.             
073900       05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.             
074000       05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.             
074100*                                                                         
074200   03    W-WDGXKEY-4541-X.                                                
074300     05    W-WDGXKEY4541          PIC X(4)  VALUE '4541'.                 
074400     05    FILLER                 PIC X(26) VALUE LOW-VALUE.              
074500*                                                                         
074600   03    W-4726-WDGXKEY-ROT-X.                                            
074700     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
074800     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
074900     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
075000*                                                                         
075100   03    W-4726-WDGXKEY-UNDSEG-X.                                         
075200     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
075300     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
075400     05    W-4726-IDDC           PIC X(2).                                
075500     05    W-4726-KDFAKTYP       PIC X.                                   
075600*                                                                         
075700   03    W1-WDA501KY-X.                                                   
075800     05    W1-IDDISTR             PIC S9(5)   COMP-3.                     
075900     05    W1-IDKUNDNR            PIC S9(7)   COMP-3.                     
076000     05    W1-IDKUNDRF            PIC X(10).                              
076100     05    W1-IDARTNR             PIC S9(9)   COMP-3.                     
076200     05    W1-IDLOPNR             PIC S9(3)   COMP-3.                     
076300*                                                                         
076400   03    W2-WDA501KY-X.                                                   
076500     05    W2-IDDISTR             PIC S9(5)   COMP-3.                     
076600     05    W2-IDKUNDNR            PIC S9(7)   COMP-3.                     
076700     05    W2-IDKUNDRF            PIC X(10).                              
076800     05    W2-IDARTNR             PIC S9(9)   COMP-3.                     
076900     05    W2-IDLOPNR             PIC S9(3)   COMP-3.                     
077000*                                                                         
077100   03    W-KDSTARAD-X.                                                    
077200     05    W-KDSTARAD             PIC X.                                  
077300*                                                                         
077400   03    W-WDGX01.                                                        
077500     05    W-IDHTYP-N5           PIC X(04)   VALUE '4511'.                
077600     05    W-VALFRI-N5           PIC X(26)   VALUE SPACE.                 
077700*                                                                         
077800   03    W-WDGXKEY-N5-MIN.                                                
077900     05    FILLER                PIC X(10)   VALUE SPACE.                 
078000*                                                                         
078100   03    W-WDGXKEY-N5-MAX.                                                
078200     05    FILLER                PIC X(10)   VALUE SPACE.                 
078300*                                                                         
078400   03    W-KDRAPRIO-N5-MIN-X.                                             
078500     05    W-KDRAPRIO-N5-MIN     PIC S9(3) COMP-3 VALUE ZERO.             
078600*                                                                         
078700   03    W-KDRAPRIO-N5-MAX-X.                                             
078800     05    W-KDRAPRIO-N5-MAX     PIC S9(3) COMP-3 VALUE ZERO.             
078900*                                                                         
079000   03    W-KDTPOTYP-N5-X.                                                 
079100     05    W-KDTPOTYP-N5         PIC S9(1) COMP-3 VALUE ZERO.             
079200*                                                                         
079300   03    W-KDORDKL-N5-X.                                                  
079400     05    W-KDORDKL-N5          PIC S9(1) COMP-3 VALUE ZERO.             
079500*                                                                         
079600   03    W-IDDISTR-FOM-N5-X.                                              
079700     05    W-IDDISTR-FOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
079800*                                                                         
079900   03    W-IDDISTR-TOM-N5-X.                                              
080000     05    W-IDDISTR-TOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
080100                                                                          
080200   03    W-IDDC-B6-X.                                                     
080300     05    W-IDDC-B6             PIC  X(2).                               
080400                                                                          
080500   03    W-IDDC-Q2-X.                                                     
080600     05    W-IDDC-Q2             PIC  X(2).                               
080700                                                                          
080800   03  W-IDDISTR-P4-X.                                                    
080900     05  W-IDDISTR-P4            PIC S9(5)   VALUE ZERO  COMP-3.          
081000*                                                                         
081100*************************************************                         
081200     EJECT                                                                
081300 01  FILLER                     PIC X(16) VALUE '4342-IO-AREA'.           
081400 01  4342-IO-AREA.                                                        
081500                                                                          
081600  03     4342-LL                 PIC S9(4) VALUE +75 COMP SYNC.           
081700  03     4342-Z1                 PIC X(1)  VALUE LOW-VALUE.               
081800  03     4342-Z2                 PIC X(1)  VALUE LOW-VALUE.               
081900  03     4342-TRANSKOD           PIC X(8)  VALUE 'W4T342U '.              
082000  03     4342-IDTRANS            PIC X(4)  VALUE '43AV'.                  
082100  03     4342-SPRAK              PIC X(1).                                
082200* 03     MID -COPY W4I34201   -PRE 4342-                                  
082300     EJECT                                                                
082400 01  FILLER                     PIC X(16) VALUE '2191-IO-AREA'.           
082500 01  2191-IO-AREA.                                                        
082600                                                                          
082700  03     2191-LL                 PIC S9(4) COMP SYNC.                     
082800  03     2191-Z1                 PIC X(1)  VALUE LOW-VALUE.               
082900  03     2191-Z2                 PIC X(1)  VALUE LOW-VALUE.               
083000  03     2191-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
083100  03     2191-IDTRANS            PIC X(4)  VALUE '43AV'.                  
083200  03     2191-SPRAK              PIC X(1).                                
083300* 03     MID -COPY W2I19101   -PRE 2191-                                  
083400     EJECT                                                                
083500                                                                          
083600 01    FILLER                 PIC X(16) VALUE 'MID W4I34901 MID'.         
083700     SKIP3                                                                
083800*01    MID -COPY W4I34901  -PRE 4349-.                                    
083900     SKIP2                                                                
084000     EJECT                                                                
084100*---MSG-AERA FÖR HOPP TILL 4349-UTSKRIFT DELIVERY NOTE NA                 
084200 01  FILLER                PIC X(16)  VALUE '4349-MSG-IO-AREA'.           
084300 01  4349-MSG-IO-AREA.                                                    
084400     03  4349-LL              PIC S9(4)  VALUE +748 COMP SYNC.            
084500     03  4349-Z1              PIC X.                                      
084600     03  4349-Z2              PIC X.                                      
084700     03  4349-TRANSKOD        PIC X(8)   VALUE 'W4T349X '.                
084800     03  4349-IDTRANS         PIC X(4)   VALUE '403A'.                    
084900     03  4349-SPRAK           PIC X.                                      
085000     03  4349-FILLER          PIC X(731).                                 
085100                                                                          
085200 01    FILLER                 PIC X(16) VALUE 'WMSGAREA        '.         
085300*01    -COPY WMSGAREA                                                     
085400     EJECT                                                                
085500 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
085600     SKIP3                                                                
085700 01    EMB-TABELL.                                                        
085800   03    EMB-TAB-X.                                                       
085900     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
086000         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
086100                                         PIC S9(5)  COMP-3.               
086200   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
086300     05  FILLER.                                                          
086400         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
086500     05  FILLER.                                                          
086600         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
086700     05  FILLER.                                                          
086800         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
086900     EJECT                                                                
087000 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
087100                                                                          
087200 01    FG-TABELL.                                                         
087300   03    TAB-POST OCCURS 10.                                              
087400                                                                          
087500     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
087600                                                                          
087700     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
087800                                                                          
087900     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
088000     EJECT                                                                
088100******************************************************************        
088200*                                                                *        
088300*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
088400*                                                                *        
088500******************************************************************        
088600*01  XXJK  -COPY WDGX4322    -PRE XXJK-                                   
088700     EJECT                                                                
088800******************************************************************        
088900*                                                                         
089000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
089100*                                                                         
089200 01    IMS-WS.                                                            
089300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
089400     SKIP3                                                                
089500*                        **** STATUS-KOD FRÅN IMS                         
089600   03    STATUS-RAD-WS           PIC XX.                                  
089700     88    RAD-FINNS                         VALUE '  '.                  
089800     88    RAD-SAKNAS                        VALUE 'GE'.                  
089900   03    STATUS-WS               PIC XX.                                  
090000     88    SEGMENT-FINNS                     VALUE '  '.                  
090100     88    ISRT-OK                           VALUE '  '.                  
090200     88    SEGMENT-SLUT                      VALUE 'GB'.                  
090300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
090400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
090500     SKIP3                                                                
090600   03    GODK-STATUSKODER.                                                
090700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
090800     SKIP3                                                                
090900 01    SSA1                      PIC X(200).                              
091000 01    SSA2                      PIC X(160).                              
091100 01    SSA3                      PIC X(128).                              
091200 01    SSA4                      PIC X(128).                              
091300     EJECT                                                                
091400*                            DB2 FUNKTIONSKODER                           
091500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
091600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
091700                                                                          
091800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
091900 01  DB2-WS.                                                              
092000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
092100         88  CURSOR-OK                       VALUE 000.                   
092200         88  RADER-FINNS                     VALUE 000.                   
092300         88  RADER-SAKNAS                    VALUE 100.                   
092400         88  ATKOMST-FEL                     VALUE 904.                   
092500     03  GODK-SQLCODEKODER.                                               
092600         05  GODK-SQLCODE OCCURS 5                                        
092700             INDEXED BY SQLCODE-IX PIC 9(3).                              
092800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
092900     EJECT                                                                
093000*                            IMS FUNKTIONSKODER                           
093100*01    -COPY W0003                                                        
093200     EJECT                                                                
093300 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
093400 01    DLI-IO-AREA2.                                                      
093500   03    IO-AREA2                PIC X(400)  VALUE SPACE.                 
093600     SKIP3                                                                
093700*  03    WDE601 -COPY WDE601               -RED IO-AREA2.                 
093800     EJECT                                                                
093900*  03    WDE611 -COPY WDE611               -RED IO-AREA2.                 
094200     EJECT                                                                
094300 01  FILLER               PIC X(16)   VALUE 'WDE621  '.                   
094400 01  DLI-IO-WDE621.                                                       
094500*    03  -COPY WDE621                                                     
094600                                                                          
096100 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA3'.               
096200 01    DLI-IO-AREA3.                                                      
096300   03    IO-AREA3                PIC X(32)   VALUE SPACE.                 
096400     SKIP3                                                                
096500*  03    WLXXDV11 -COPY WDGX4726           -RED IO-AREA3.                 
096600*  03    WLXXDV21 -COPY WDGX4727           -RED IO-AREA3.                 
096700     EJECT                                                                
096800 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA4'.               
096900 01  DLI-IO-AREA4.                                                        
097000     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
097100*                                                                         
097200*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4.           
097300*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4.                       
097400     EJECT                                                                
097500*    03  WLXXJN    -COPY WDGX4512    -PRE STYR-  -RED IO-AREA4.           
097600     EJECT                                                                
097700 01    FILLER    PIC X(16)  VALUE 'DLI-IO-Q301'.                          
097800 01  DLI-IO-Q301.                                                         
097900*    03  WLORQA01  -COPY WDQ301                                           
098000*                                                                         
098100 01    FILLER    PIC X(16)  VALUE 'DLI-IO-Q101'.                          
098200 01  DLI-IO-Q101.                                                         
098300*    03  WLORQM01  -COPY WDQ101                                           
098400     EJECT                                                                
098500 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA6'.               
098600 01  DLI-IO-AREA6.                                                        
098700     03  IO-AREA6                PIC X(1000) VALUE SPACE.                 
098800*                                                                         
098900*    03  WLXXKW11  -COPY WDGX4472            -RED IO-AREA6.               
099000     EJECT                                                                
099100*    03  WLXXLB11  -COPY WDGX4478            -RED IO-AREA6.               
099200     EJECT                                                                
099300*  03  WDA501     -COPY WDA501               -RED IO-AREA6.               
099400     EJECT                                                                
099500 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA7'.               
099600 01    DLI-IO-AREA7.                                                      
099700   03    IO-AREA7                PIC X(384)  VALUE SPACE.                 
099800     SKIP3                                                                
099900*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA7.               
100000     EJECT                                                                
100100*  03  WDA501     -COPY WDA501   -PRE OLD-   -RED IO-AREA7.               
100200     EJECT                                                                
100300 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA8'.               
100400 01    DLI-IO-AREA8.                                                      
100500   03    IO-AREA8                PIC X(900)  VALUE SPACE.                 
100600     SKIP3                                                                
100700*  03  WLARTM01   -COPY WDK901               -RED IO-AREA8.               
100800     EJECT                                                                
100900*  03  WDK711     -COPY WDK711               -RED IO-AREA8.               
101000     EJECT                                                                
101100*  03  WLARTC01   -COPY WDK601               -RED IO-AREA8.               
101200     EJECT                                                                
101300*  03  WLARTC11   -COPY WDK611               -RED IO-AREA8.               
101400     EJECT                                                                
101500*  03  WLOGAG01   -COPY WDE601   -PRE OGAG-  -RED IO-AREA8.               
101600     EJECT                                                                
101700*  03  WLOGAG12   -COPY WDE401   -PRE OGAG-  -RED IO-AREA8.               
101800     EJECT                                                                
101900 01  FILLER                      PIC X(16)   VALUE 'K722-AREA'.           
102000 01    DLI-IO-WDK722.                                                     
102100*  03    WDK722   -COPY WDK722                                            
102200     EJECT                                                                
102300 01  FILLER                      PIC X(16)   VALUE 'K712-AREA'.           
102400 01    DLI-IO-WDK712.                                                     
102500*  03    WDK722   -COPY WDK712                                            
102600     EJECT                                                                
102700 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
102800 01    DLI-IO-AREA11.                                                     
102900*  03    WDA601   -COPY WDA601                                            
103000     EJECT                                                                
103100 01  FILLER                      PIC X(16)  VALUE 'WDQ2-AREA'.            
103200     SKIP2                                                                
103300 01  -COPY WDQ201                                                         
103400 01  FILLER                      PIC X(16)  VALUE 'WDQ212-AREA'.          
103500     SKIP2                                                                
103600 01  -COPY WDQ212                                                         
103700     EJECT                                                                
103800 01  FILLER                      PIC X(16)  VALUE '4490-AREA'.            
103900     SKIP2                                                                
104000 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4490'.           
104100 01    DLI-IO-WDGX4490.                                                   
104200*  03    -COPY WDGX4490                                                   
104300     EJECT                                                                
104400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
104500 01  DLI-IO-WDM211.                                                       
104600*    03 -COPY WDM211                                                      
104700     EJECT                                                                
104800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
104900 01  DLI-IO-WDM221.                                                       
105000*    03 -COPY WDM221                                                      
105100     EJECT                                                                
105200                                                                          
105300 01  FILLER                      PIC X(16)  VALUE '4542-AREA'.            
105400 01  -COPY WDGX4542.                                                      
105500     EJECT                                                                
105600 01  FILLER                      PIC X(16)  VALUE '4448-AREA'.            
105700*01  WLXXKH11 -COPY WDGX4448.                                             
105800     EJECT                                                                
105900 01  FILLER                      PIC X(16)  VALUE '2204-AREA'.            
106000     SKIP2                                                                
106100 01  -COPY WDGX2204                                                       
106200     EJECT                                                                
106300 01  FILLER                      PIC X(16)  VALUE 'RY1-AREA'.             
106400 01  -COPY WDGZRY1.                                                       
106500     EJECT                                                                
106600 01  FILLER                      PIC X(16)  VALUE 'RY1S-AREA'.            
106700 01  -COPY WDGZRY1S.                                                      
106800     EJECT                                                                
106900 01  FILLER                      PIC X(16)  VALUE 'RY6-AREA'.             
107000 01  -COPY WDGZRY6.                                                       
107100     EJECT                                                                
107200 01  FILLER                      PIC X(16)  VALUE 'RYK-AREA'.             
107300 01  -COPY WDGZRYK.                                                       
107400     EJECT                                                                
107500 01  FILLER                      PIC X(16)  VALUE 'WDE6E1XX'.             
107600*01  -COPY WDE6E1.                                                        
107700 01  FILLER                      PIC X(16)  VALUE 'WLLOGA01'.             
107800*01  WLLOGA01 -COPY WDL901                                                
107900     EJECT                                                                
108000 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
108100 01  DLI-IO-AREA-B601.                                                    
108200*    03  -COPY WDB601                                                     
108300     EJECT                                                                
108400 01  FILLER                      PIC X(16) VALUE 'WDE401 AREA'.           
108500 01    DLI-IO-WDE401.                                                     
108600*  03    WDE401 -COPY WDE401                                              
108700     EJECT                                                                
108800 01  FILLER                      PIC X(16) VALUE 'WDE411 AREA'.           
108900 01    DLI-IO-WDE40111.                                                   
109000*  03    WDE401 -COPY WDE401 -PRE  E401-                                  
109100*  03    WDE411 -COPY WDE411                                              
109200     EJECT                                                                
109300 01  FILLER                      PIC X(16) VALUE 'WDE421 AREA'.           
109400 01    DLI-IO-WDE421.                                                     
109500*  03    WDE421 -COPY WDE421                                              
109600                                                                          
109700 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
109800 01   DLI-IO-AREA-WDP4A1.                                                 
109900*     03  -COPY WDP4A1                                                    
110000     EJECT                                                                
110100 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
110200                                                                          
110300*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
110400     EJECT                                                                
110500     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
110600     EJECT                                                                
110700 LINKAGE SECTION.                                                         
110800*                                                                         
110900 01  FILLER                      PIC X(16)   VALUE 'AVSP-AREA  '.         
111000*    -COPY W403AVSP                                                       
111100     EJECT                                                                
111200*01    -COPY W0009     -PRE 4342-                                         
111300     EJECT                                                                
111400*01    -COPY W0009     -PRE 2191-                                         
111500     EJECT                                                                
111600*01    -COPY W0009     -PRE 4349-                                         
111700     EJECT                                                                
111800 01  TMS-CRE-PCB                 PIC X.                                   
111900 01  TMS-DEL-PCB                 PIC X.                                   
112000     EJECT                                                                
112100 01  ATAB-PCB                    PIC X.                                   
112200     EJECT                                                                
112300*01    -COPY W0008     -PRE USEA-                                         
112400     05  FILLER                  PIC X.                                   
112500     SKIP2                                                                
112600*01    -COPY W0008     -PRE WDE41-                                        
112700     05  FILLER                  PIC X.                                   
112800     SKIP2                                                                
112900*01    -COPY W0008     -PRE WDE42-                                        
113000     05  FILLER                  PIC X.                                   
113100     EJECT                                                                
113200*01    -COPY W0008     -PRE WDE4-                                         
113300     05  FILLER                  PIC X.                                   
113400     EJECT                                                                
113500*01    -COPY W0008     -PRE WDE4B-                                        
113600     05  FILLER                  PIC X.                                   
113700     EJECT                                                                
113800*01    -COPY W0008     -PRE WDE4E-                                        
113900     05  FILLER                  PIC X.                                   
114000     EJECT                                                                
114100*01    -COPY W0008     -PRE WDE6-                                         
114200     05  FILLER                  PIC X.                                   
114300     EJECT                                                                
114400*01    -COPY W0008     -PRE XXDV-                                         
114500     05  FILLER                  PIC X.                                   
114600     EJECT                                                                
114700*01    -COPY W0008     -PRE ORQA-                                         
114800     05  FILLER                  PIC X.                                   
114900     EJECT                                                                
115000*01    -COPY W0008     -PRE XXKW-                                         
115100     05  FILLER                  PIC X.                                   
115200     EJECT                                                                
115300*01    -COPY W0008     -PRE XXLB-                                         
115400     05  FILLER                  PIC X.                                   
115500     EJECT                                                                
115600*01    -COPY W0008     -PRE XXJK-                                         
115700     05  FILLER                  PIC X.                                   
115800     EJECT                                                                
115900*01    -COPY W0008     -PRE ZZAC-                                         
116000     05  FILLER                  PIC X.                                   
116100     EJECT                                                                
116200*01    -COPY W0008     -PRE ORQI-                                         
116300     05  FILLER                  PIC X.                                   
116400     EJECT                                                                
116500*01  -COPY W0008       -PRE ORQL-                                         
116600     05  FILLER                  PIC X.                                   
116700     EJECT                                                                
116800*01  -COPY W0008       -PRE WDE6E-                                        
116900     05  FILLER                  PIC X.                                   
117000     EJECT                                                                
117100*01  -COPY W0008       -PRE WDE62-                                        
117200     05  FILLER                  PIC X.                                   
117300     EJECT                                                                
117400*01    -COPY W0008     -PRE ORQICSQ-                                      
117500     05  FILLER                  PIC X.                                   
117600     EJECT                                                                
117700*01    -COPY W0008     -PRE ORQM-                                         
117800     05  FILLER                  PIC X.                                   
117900     EJECT                                                                
118000*01    -COPY W0008     -PRE WDE4A-                                        
118100     05  FILLER                  PIC X.                                   
118200     EJECT                                                                
118300*01    -COPY W0008     -PRE WDM2-                                         
118400     05  FILLER                  PIC X.                                   
118500     EJECT                                                                
118600*01    -COPY W0008     -PRE ORQA2-                                        
118700     05  FILLER                  PIC X.                                   
118800     EJECT                                                                
118900*01    -COPY W0008     -PRE ORDP1-                                        
119000     05  FILLER                  PIC X.                                   
119100     EJECT                                                                
119200*01    -COPY W0008     -PRE ORDP2-                                        
119300     05  FILLER                  PIC X.                                   
119400     EJECT                                                                
119500*01    -COPY W0008     -PRE XXJN-                                         
119600     05  FILLER                  PIC X.                                   
119700     EJECT                                                                
119800*01    -COPY W0008     -PRE XXKH-                                         
119900     05  FILLER                  PIC X.                                   
120000     EJECT                                                                
120100*01    -COPY W0008     -PRE ARTC-                                         
120200     05  FILLER                  PIC X.                                   
120300     EJECT                                                                
120400*01    -COPY W0008     -PRE WDK7-                                         
120500     05  FILLER                  PIC X.                                   
120600     EJECT                                                                
120700*01    -COPY W0008     -PRE ARTM-                                         
120800     05  FILLER                  PIC X.                                   
120900     EJECT                                                                
121000*01    -COPY W0008     -PRE AUTF-                                         
121100     05  FILLER                  PIC X.                                   
121200     EJECT                                                                
121300*01    -COPY W0008     -PRE 4487-                                         
121400     05  FILLER                  PIC X.                                   
121500     EJECT                                                                
121600*01    -COPY W0008     -PRE 4541-                                         
121700     05  FILLER                  PIC X.                                   
121800     EJECT                                                                
121900*01    -COPY W0008     -PRE LOGA-                                         
122000     05  FILLER                  PIC X.                                   
122100     EJECT                                                                
122200*01    -COPY W0008     -PRE WDK6-                                         
122300     05  FILLER                  PIC X.                                   
122400     EJECT                                                                
122500*01    -COPY W0008     -PRE WDA6B-                                        
122600     05  FILLER                  PIC X.                                   
122700     EJECT                                                                
122800*01    -COPY W0008     -PRE WDA6-                                         
122900     05  FILLER                  PIC X.                                   
123000     EJECT                                                                
123100*01    -COPY W0008     -PRE WDP4A-                                        
123200     05  FILLER                  PIC X.                                   
123300     EJECT                                                                
123400*01    -COPY W0008     -PRE WDB6-                                         
123500     05  FILLER                  PIC X.                                   
123600     EJECT                                                                
123700*01    -COPY W0008     -PRE PLATS-DM-                                     
123800     05  FILLER                  PIC X.                                   
123900     EJECT                                                                
124000*01    -COPY W0008     -PRE PLATS-DN-                                     
124100     05  FILLER                  PIC X.                                   
124200     EJECT                                                                
124300*01    -COPY W0008     -PRE PLATS-DP-                                     
124400     05  FILLER                  PIC X.                                   
124500     EJECT                                                                
124600*01    -COPY W0008     -PRE PLATS-DO-                                     
124700     05  FILLER                  PIC X.                                   
124800     EJECT                                                                
124900*01    -COPY W0008     -PRE PLATS-SEK-                                    
125000     05  FILLER                  PIC X.                                   
125100     EJECT                                                                
125200*01    -COPY W0008     -PRE PLATS-GMTC-                                   
125300     05  FILLER                  PIC X.                                   
125400     EJECT                                                                
125500*01    -COPY W0008     -PRE PLATS-WDB6-                                   
125600     05  FILLER                  PIC X.                                   
125700     EJECT                                                                
125800                                                                          
125900 01  DNOT-ORQP-PCB               PIC X.                                   
126000 01  DNOT-ORQP2-PCB              PIC X.                                   
126100 01  DNOT-ORQP3-PCB              PIC X.                                   
126200 01  DNOT-4013-PCB               PIC X.                                   
126300 01  DNOT-BENA-PCB               PIC X.                                   
126400                                                                          
126500 01  PRQU-WDG2-PCB               PIC X.                                   
126600 01  PRQU-WDC7-PCB               PIC X.                                   
126700 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
126800 01  PRNO-3107-PCB               PIC X.                                   
126900                                                                          
127000 01  TMS-1165-PCB                PIC X.                                   
127100 01  TMS-4141-PCB                PIC X.                                   
127200 01  TMS-WDB2-PCB                PIC X.                                   
127300 01  TMS-WDB6-PCB                PIC X.                                   
127400 01  TMS-WDD3-PCB                PIC X.                                   
127500 01  TMS-WDB1-PCB                PIC X.                                   
127600 01  TMS-WDE4A-PCB               PIC X.                                   
127700 01  TMS-WDE4F-PCB               PIC X.                                   
127800 01  TMS-WDQ2-PCB                PIC X.                                   
127900 01  TMS-WDQ3-PCB                PIC X.                                   
128000 01  TMS-WDK6-PCB                PIC X.                                   
128100 01  TMS-WDE6-PCB                PIC X.                                   
128200 01  TMS-WDK5-PCB                PIC X.                                   
128300 01  TMS-WDQ2C-PCB               PIC X.                                   
128400     EJECT                                                                
128500                                                                          
128600 PROCEDURE DIVISION USING AVSP-W403AVSP 4342-PCB 2191-PCB                 
128700                          4349-PCB  TMS-CRE-PCB TMS-DEL-PCB               
128800                          ATAB-PCB                                        
128900                          USEA-PCB  WDE41-PCB WDE42-PCB WDE4-PCB          
129000                          WDE4B-PCB WDE4E-PCB WDE6-PCB  XXDV-PCB          
129100                          ORQA-PCB  XXKW-PCB  XXLB-PCB  XXJK-PCB          
129200                          ZZAC-PCB  ORQI-PCB  ORQL-PCB  WDE6E-PCB         
129300                          WDE62-PCB  ORQICSQ-PCB         ORQM-PCB         
129400                          WDE4A-PCB WDM2-PCB  ORQA2-PCB                   
129500                          ORDP1-PCB ORDP2-PCB XXJN-PCB  XXKH-PCB          
129600                          ARTC-PCB  WDK7-PCB  ARTM-PCB  AUTF-PCB          
129700                          4487-PCB  4541-PCB LOGA-PCB                     
129800                          WDK6-PCB  WDA6B-PCB WDA6-PCB WDP4A-PCB          
129900                          WDB6-PCB                                        
130000                          PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB          
130100                          PLATS-DO-PCB PLATS-SEK-PCB                      
130200                          PLATS-GMTC-PCB PLATS-WDB6-PCB                   
130300                          DNOT-ORQP-PCB                                   
130400                          DNOT-ORQP2-PCB                                  
130500                          DNOT-ORQP3-PCB                                  
130600                          DNOT-4013-PCB                                   
130700                          DNOT-BENA-PCB                                   
130800                          PRQU-WDG2-PCB                                   
130900                          PRQU-WDC7-PCB                                   
131000                          PRQU-SJKO-WDK6-PCB                              
131100                          PRNO-3107-PCB                                   
131200                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
131300                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
131400                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
131500                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
131600                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
131700 MAIN SECTION.                                                            
131800                                                                          
131900     PERFORM A-INIT                                                       
132000                                                                          
132100     PERFORM C-LAGG-UPP-KOLLI-SEG                                         
132200     IF WS-INDATA-RATT AND WS-GODKAND-BILD                                
132300        PERFORM D-BEHANDLA-RADER                                          
132400                                                                          
132500        PERFORM E-HAMTA-ADRESS                                            
132600        PERFORM F-UPPDATERA-KOLLIREG                                      
132700                                                                          
132800        PERFORM G-EV-UPDATE-4726-4727                                     
132900                                                                          
133000        PERFORM I-BEHANDLA-INTERVALL                                      
133100                                                                          
133200        IF ORAPPORTERADE-RADER-SAKNAS                                     
133300           PERFORM J-UPDATE-KUNDORDER                                     
133400        END-IF                                                            
133500                                                                          
133600        PERFORM K-UPDATE-KOLLIREG                                         
133700                                                                          
133800        IF ORAPPORTERADE-RADER-SAKNAS                                     
133900           PERFORM L-UPDATE-ORDERKO                                       
134000        END-IF                                                            
134100                                                                          
134200        PERFORM M-AVSLUT                                                  
134300                                                                          
134400     ELSE                                                                 
134500        MOVE FEL               TO AVSP-KDSVAR                             
134600        MOVE 'FEL FR. C-SEC'   TO WS-PGM-POSITION                         
134700     END-IF                                                               
134800                                                                          
134900     MOVE ZERO TO RETURN-CODE                                             
135000     GOBACK                                                               
135100     .                                                                    
135200     EJECT                                                                
135300 A-INIT             SECTION.                                              
135400                                                                          
135500     PERFORM S21-INIT-WS-FIELDS                                           
135600     PERFORM AA-FLYTTA-NYCKLAR                                            
135700                                                                          
135800     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
135900     ACCEPT WS-TIDPUNKT                   FROM TIME                       
136000     ACCEPT WS-TID-W                      FROM TIME                       
136100     ACCEPT WS-VOR-TID-BRIST              FROM TIME                       
136200                                                                          
136300     PERFORM S08-HAMTA-MASKINDATUM                                        
136400     MOVE DAT-TIAAMMDD                    TO   DAGDAT-AAMMD               
136500     MOVE DAT-TIAAVVD                     TO   DAGDAT-AAVVD               
136600                                                                          
136700     INITIALIZE TMS-W403TMS1                                              
136800                                                                          
136900     MOVE W-IDDC TO W-IDDC-B6                                             
137000     PERFORM IMS-GU-WDB601                                                
137100     IF DCS-NDC OR                                                        
137200       (DCS-SDC AND (DCS-ENGLAND OR DCS-CHINA))                           
137300       MOVE ALL '+'           TO MSGI-WMSGINIT                            
137400       MOVE '011'             TO MSGI-KDCALL                              
137500       MOVE WS-DCUSER         TO MSGI-IDUSER                              
137600       MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                            
137700       MOVE WS-TIDPUNKT(1:4)  TO MSGI-TILOKTID                            
137800       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
137900       MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                          
138000                                 DAT-I-TIDATUM                            
138100                                 DAGDAT-AAMMD(1:4)                        
138200       MOVE MSGI-TILOKTID     TO WS-TIDPUNKT (1:4)                        
138300                                 WS-TTMMSS   (1:4)                        
138400                                                                          
138500       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
138600       CALL WDATKONV USING DAT-KDDATFORM                                  
138700                           DAT-I-TIDATUM                                  
138800                           DAT-O-TIDATUM                                  
138900                           DAT-KDSVAR                                     
139000       MOVE DAT-TIAAVVD       TO DAGDAT-AAVVD                             
139100     END-IF                                                               
139200                                                                          
139300     IF MSGI-IDLAND-SPR = 'SE'                                            
139400       MOVE '1'                           TO   2191-SPRAK                 
139500     ELSE                                                                 
139600       MOVE '2'                           TO   2191-SPRAK                 
139700     END-IF                                                               
139800     MOVE '1'                             TO   W-KDSEGKEY                 
139900*                                                                         
140000     MOVE NEJ                             TO SW-FLAUTFAK                  
140100                                             FL-420-SEGMENT               
140200     MOVE RAETT                           TO WS-INDATA-TEST               
140300                                             WS-BEHANDLING-TEST           
140400                                                                          
140500     CONTINUE                                                             
140600     .                                                                    
140700     SKIP2                                                                
140800 AA-FLYTTA-NYCKLAR  SECTION.                                              
140900                                                                          
141000     MOVE AVSP-IDTRANS                 TO WS-IDTRANS                      
141100     MOVE AVSP-IDANSTNR                TO WS-IDANSTNR                     
141200     MOVE AVSP-IDDC                    TO W-IDDC                          
141300                                          WS-DCUSER-IDDC                  
141400     MOVE AVSP-IDPRODNR                TO WS-IDPRODNR                     
141500                                          W-401-IDPRODNR                  
141600                                          WS-IDPRODNR                     
141700                                          WS-JFR-IDPRODNR                 
141800                                          WS-IDPRODNR-RED                 
141900     MOVE AVSP-IDDISTR                 TO WS-IDDISTR-NUM                  
142000                                          W-401-IDDISTR                   
142100                                          WS-IDDISTR                      
142200                                          WS-IDDISTR-NUM                  
142300                                          WS-IDDISTR-NUM4                 
142400                                          WS-SAVE-IDDISTR                 
142500     MOVE AVSP-IDKUNDNR                TO WS-IDKUNDNR-NUM                 
142600                                          W-401-IDKUNDNR                  
142700                                          WS-IDKUNDNR                     
142800                                          WS-IDKUNDNR-NUM                 
142900                                          WS-SAVE-IDKUNDNR                
143000     MOVE AVSP-IDORDNR                 TO WS-IDORDNR                      
143100                                          W-401-IDORDNR                   
143200     MOVE AVSP-IDORDER                 TO WS-IDORDER                      
143300     MOVE AVSP-IDPLKLST                TO WS-IDPLKLST                     
143400                                          W-401-IDPLKLST                  
143500                                          WS-IDPLKLST                     
143600     MOVE AVSP-IDKOLLI                 TO WS-IDKOLLI                      
143700                                          WS-IDKOLLI-NUM                  
143800     MOVE AVSP-KDKOLLI                 TO WS-KDKOLLI                      
143900     MOVE AVSP-VKORDBTO                TO WS-VKORDBTO                     
144000     MOVE AVSP-VKORDNTO                TO PLATS-VKORDNTO-KOLLI            
144100     MOVE AVSP-KDEMBTYP                TO WS-KDEMBTYP                     
144200     MOVE AVSP-DIKOLLIL                TO WS-DIKOLLIL                     
144300     MOVE AVSP-DIKOLLIB                TO WS-DIKOLLIB                     
144400     MOVE AVSP-DIKOLLIH                TO WS-DIKOLLIH                     
144500     MOVE AVSP-IDKOLLI-SAMP            TO WS-IDKOLLI-SAMP                 
144600                                                                          
144700     .                                                                    
144800     SKIP2                                                                
144900 C-LAGG-UPP-KOLLI-SEG   SECTION.                                          
145000                                                                          
145100     MOVE WS-IDPRODNR            TO W-601-IDPRODNR                        
145200     MOVE WS-IDKOLLI             TO W-421-IDKOLLI                         
145300                                    W-610-IDKOLLI                         
145400     PERFORM IMS-GHU-WDE601                                               
145500                                                                          
145600     MOVE VORD-KDORDKL           TO WS-KDORDKL                            
145700     MOVE VORD-FLAUTFAK          TO WS-FLAUTFAK                           
145800     MOVE VORD-KDFRAKT           TO   PLATS-KDFRAKT                       
145900     MOVE VORD-KDORDKL           TO   PLATS-KDORDKLX                      
146000                                      WS-KDORDKL                          
146100     MOVE VORD-IDDC              TO   PLATS-IDDC                          
146200     MOVE VORD-IDDISTR           TO   PLATS-IDDISTR                       
146300     MOVE VORD-IDKUNDNR          TO   PLATS-IDKUNDNR                      
146400     MOVE WS-IDKOLLI             TO WS-IDKOLLI-NUM                        
146500     IF   VORD-KDORDKL = 4                                                
146600       IF  VORD-IDDISTR = +00878                                          
146700       AND VORD-IDKUNDNR > +006000                                        
146800          MOVE 2                         TO   PLATS-KDCALL                
146900       ELSE                                                               
147000          MOVE 1                         TO   PLATS-KDCALL                
147100       END-IF                                                             
147200     ELSE                                                                 
147300         MOVE 2                 TO   PLATS-KDCALL                         
147400     END-IF                                                               
147500                                                                          
147600     PERFORM IMS-GHNP-KOLLI                                               
147700     IF SEGMENT-FINNS                                                     
147800         ADD +1                  TO W-421-IDKOLLI                         
147900                                    W-610-IDKOLLI                         
148000     ELSE                                                                 
148100         MOVE 1                  TO ARB-ANTAL-KOLLI                       
148200     END-IF                                                               
148300                                                                          
148400     PERFORM S09-INIT-KOLLI-VALUE                                         
148500     PERFORM CA-KOLLA-PLATSSATTNING                                       
148600                                                                          
148700     IF WS-INDATA-RATT AND                                                
148800        WS-IDKOLLI-SAMP > ZERO                                            
148900       PERFORM CB-KOLLA-IDKOLLI-SAMP                                      
149000     END-IF                                                               
149100                                                                          
149200     IF WS-INDATA-RATT AND WS-GODKAND-BILD                                
149300       PERFORM IMS-ISRT-KOLLI                                             
149400                                                                          
149500       IF SEGMENT-FINNS-REDAN                                             
149510         PERFORM UNTIL ISRT-OK                                            
149600           ADD +1                TO WS-IDKOLLI-NUM                        
149700                                    KOLLI-IDKOLLI                         
149800           PERFORM IMS-ISRT-KOLLI                                         
149810         END-PERFORM                                                      
149900       END-IF                                                             
150000         IF WS-IDDC-CROSS > SPACES                                        
150100          MOVE W-KDSEGKEY-X     TO  CROSS-KDSEGKEY                        
150200          MOVE KOLLI-IDDC       TO  CROSS-IDDC-SEND                       
150300          MOVE WS-IDDC-CROSS    TO  CROSS-IDDC-CROSS                      
150400          MOVE KOLLI-IDDISTR    TO  CROSS-IDDISTR                         
150500          MOVE KOLLI-IDKUNDNR   TO  CROSS-IDKUNDNR                        
150600          MOVE WS-IDPRODNR      TO  CROSS-IDPRODNR                        
150700          MOVE KOLLI-IDKOLLI    TO  CROSS-IDKOLLI                         
150800          MOVE KOLLI-IDLEVNR    TO  CROSS-IDLEVNR                         
150900          MOVE KOLLI-IDSUPREF   TO  CROSS-IDSUPREF                        
151000          MOVE KOLLI-DARFS(3:6) TO  CROSS-TIRFSDAT                        
151100          MOVE ZERO             TO  CROSS-IDTRPTNR-CROSS                  
151200          MOVE ZERO             TO  CROSS-TIRECXDAT                       
151300          MOVE ZERO             TO  CROSS-TIRECXTID                       
151310          MOVE ZERO             TO  CROSS-TISKEPPN                        
151320          MOVE ZERO             TO  CROSS-IDSHIPM-CROSS                   
151330          MOVE SPACE            TO  CROSS-IDLBBET-CROSS                   
151400          MOVE 1                TO  CROSS-KDKOLSTA-CROSS                  
151500          PERFORM IMS-ISRT-WDE621                                         
151510         END-IF                                                           
151520     END-IF                                                               
151530                                                                          
151540     .                                                                    
151550     EJECT                                                                
151560 CA-KOLLA-PLATSSATTNING SECTION.                                          
151570                                                                          
151580     MOVE ZERO                          TO PLATS-ADVMODUL                 
151590                                           PLATS-ADHMODUL                 
151600                                           PLATS-ADFLOMR                  
151700                                           PLATS-ADRUTNIV                 
151800                                           PLATS-IDTRPTNR                 
151900                                           PLATS-DIHMODUL                 
152000                                           PLATS-DIDMODUL                 
152100                                           PLATS-IDDC-CROSS               
152200     MOVE SPACE                         TO PLATS-ADFLGEO                  
152300                                           PLATS-FLUTLAST                 
152400     MOVE WS-IDORDNR                    TO PLATS-IDORDNR                  
152500     MOVE WS-DIKOLLIL                   TO PLATS-DIKOLLIL                 
152600     MOVE WS-DIKOLLIB                   TO PLATS-DIKOLLIB                 
152700     MOVE WS-DIKOLLIH                   TO PLATS-DIKOLLIH                 
152800     MOVE WS-KDKOLLID                   TO PLATS-KDKOLLID                 
152900                                                                          
153000     CALL W403PLAT USING PLATS-W403PLAT                                   
153100                         PLATS-DM-PCB                                     
153200                         PLATS-DN-PCB                                     
153300                         PLATS-DP-PCB                                     
153400                         PLATS-DO-PCB                                     
153500                         PLATS-SEK-PCB                                    
153600                         PLATS-GMTC-PCB                                   
153700                         PLATS-WDB6-PCB                                   
153800                                                                          
153900                                                                          
154000     IF PLATS-KDSVAR = SPACE                                              
154100         MOVE PLATS-IDTRPTNR       TO WS-IDTRPTNR                         
154200         MOVE PLATS-ADFLGEO        TO WS-ADFLGEO                          
154300                                      ARB-ADFLGEO                         
154400         MOVE PLATS-ADFLOMR        TO WS-ADFLOMR                          
154500                                      ARB-ADFLOMR                         
154600         MOVE PLATS-ADRUTNIV       TO WS-ADRUTNIV                         
154700                                      ARB-ADRUTNIV                        
154800         MOVE PLATS-DIHMODUL       TO WS-DIHMODUL                         
154900         MOVE PLATS-DIDMODUL       TO WS-DIDMODUL                         
155000         MOVE PLATS-ADVMODUL       TO WS-ADVMODUL                         
155100         MOVE PLATS-ADHMODUL       TO WS-ADHMODUL                         
155200         MOVE PLATS-FLUTLAST       TO WS-FLUTLAST                         
155210         MOVE PLATS-IDDC-CROSS     TO WS-IDDC-CROSS                       
155300     ELSE                                                                 
155400         MOVE FEL                  TO WS-INDATA-TEST                      
155500         MOVE '730'                TO AVSP-ERROR-MESSAGE                  
155600     END-IF                                                               
155700     .                                                                    
155800     EJECT                                                                
155900 CB-KOLLA-IDKOLLI-SAMP     SECTION.                                       
156000                                                                          
156100                                                                          
156200     IF WS-IDTRPTNR     > ZERO                                            
156300         MOVE WS-IDKOLLI-SAMP     TO W-E6E-IDKOLLIS-MIN                   
156400                                     W-E6E-IDKOLLIS-MAX                   
156500         MOVE AVSP-IDDC           TO W-E6E-IDDC-MIN                       
156600                                     W-E6E-IDDC-MAX                       
156700        PERFORM IMS-GU-WDE6E                                              
156800        IF SEGMENT-FINNS                                                  
156900           IF WS-IDTRPTNR         =  SEQE-IDTRPTNR                        
157000              CONTINUE                                                    
157100           ELSE                                                           
157200             MOVE FEL             TO WS-INDATA-TEST                       
157300             MOVE '317'           TO AVSP-ERROR-MESSAGE                   
157400           END-IF                                                         
157500        END-IF                                                            
157600     ELSE                                                                 
157700       MOVE FEL                   TO WS-INDATA-TEST                       
157800       MOVE '318'                 TO AVSP-ERROR-MESSAGE                   
157900     END-IF                                                               
158000     .                                                                    
158100     EJECT                                                                
158200 D-BEHANDLA-RADER     SECTION.                                            
158300                                                                          
158400     IF WS-BEHANDLING-RATT                                                
158500         PERFORM DA-BEHANDLA-RAD-INOM-INTERVALL                           
158600         IF WS-BEHANDLING-RATT                                            
158700            MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                           
158800            IF NOT DIST19-SATS                                            
158900               PERFORM DB-UPPDATERA-PRODTAB                               
159000            END-IF                                                        
159100         END-IF                                                           
159200     END-IF                                                               
159300     .                                                                    
159400     EJECT                                                                
159500 DA-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
159600     MOVE 'STA DA-BEHANDLA-RAD-INOM ' TO WS-PGM-POSITION                  
159700                                                                          
159800     MOVE ZERO                   TO  WS-RINT-ANT-FPACK-ORAD               
159900                                                                          
160000     PERFORM IMS-GHU-WDE401                                               
160100     MOVE KORD-IDKUNDRF          TO  WS-IDKUNDRF                          
160200     MOVE KORD-IDORDER           TO  WS-IDORDER                           
160300     MOVE KORD-TIORDREG          TO  WS-TIORDREG                          
160400     MOVE KORD-KDFRAKT           TO  WS-KDFRAKT                           
160500     MOVE KORD-KDORDKL           TO  WS-KDORDKL                           
160600     MOVE KORD-KDFAKTYP          TO  WS-KDFAKTYP                          
160700                                                                          
160800     MOVE KORD-IDORDER           TO WS-SPAR-IDORDER                       
160900     MOVE KORD-IDDC              TO WS-SPAR-IDDC                          
161000                                                                          
161100     PERFORM IMS-GHNP-WDE411                                              
161200     IF SEGMENT-FINNS                                                     
161300       MOVE ORAD-IDPURAD         TO WS-IDPURAD                            
161400     END-IF                                                               
161500                                                                          
161600     MOVE KORD-IDORDER           TO W-201-IDORDER                         
161700     PERFORM IMS-GU-ORQI01                                                
161800                                                                          
161900     PERFORM UNTIL SEGMENT-SAKNAS                                         
162000                                                                          
162100        PERFORM DAB-UPPDATERA-RAD                                         
162200                                                                          
162300        PERFORM DAC-LAGG-UPP-KOLLI-KOPPL                                  
162400                                                                          
162500        PERFORM IMS-GHNP-WDE411                                           
162600                                                                          
162700     END-PERFORM                                                          
162800                                                                          
162900     .                                                                    
163000     EJECT                                                                
163100 DAB-UPPDATERA-RAD         SECTION.                                       
163200     MOVE 'STA DAB-UPPDATERA-RAD ' TO WS-PGM-POSITION                     
163300                                                                          
163400     MOVE ORAD-VKARTNTO               TO  SPAR-PRAD-VKARTNTO              
163500     MOVE ORAD-KVFLAMP                TO  SPAR-PRAD-KVFLAMP               
163600     MOVE ORAD-KDFARLIG               TO  SPAR-PRAD-KDFARLIG              
163700     MOVE ORAD-PRARTNTO               TO  SPAR-PRAD-PRARTNTO              
163800     MOVE ORAD-PRAVCOST               TO  SPAR-PRAD-PRAVCOST              
163900     MOVE ORAD-PRARTNTO-LOC           TO  SPAR-PRAD-PRARTNTO-LOC          
164000     MOVE ORAD-PRARTNTO-LOCPREL     TO  SPAR-PRAD-PRARTNTO-LOCPREL        
164100*                                                                         
164200     MOVE ORAD-KDVALISO               TO SPAR-KDVALISO                    
164300     MOVE ORAD-KDVALISO-EXP           TO SPAR-KDVALISO-EXP                
164400     MOVE ORAD-KDVALISO               TO ARB-KOLLI-KDVALISO               
164500     MOVE ORAD-KDVALISO-EXP           TO ARB-KOLLI-KDVALISO-EXP           
164600*                                                                         
164700     MOVE ORAD-IDPSN                  TO SPAR-IDPSN                       
164800     MOVE ORAD-VKART-FG               TO SPAR-VKART-FG                    
164900     MOVE ORAD-VLFG                   TO SPAR-VLFG                        
165000     MOVE ORAD-SUEQFG                 TO SPAR-SUEQFG                      
165100*                                                                         
165200     MOVE ORAD-KVAVBART               TO SPAR-PRAD-KVLEVART               
165300     PERFORM S10-UPPD-SPAR-KOLLI                                          
165400                                                                          
165500     MOVE ORAD-KVAVBART               TO ORAD-KVLEVART                    
165600     MOVE ORAD-KVAVBART               TO WS-ORAD-KVLEVART                 
165700                                                                          
165800     IF ORAD-KVLEVART = ORAD-KVAVBART                                     
165900        MOVE +4              TO ORAD-KDRADSTA                             
166000        MOVE 'Y'             TO DATUM-SW                                  
166100                                                                          
166200        ADD 1 TO WS-TOT-ANT-RADER                                         
166300        ADD 1 TO WS-RINT-ANT-FPACK-ORAD                                   
166400     END-IF                                                               
166500                                                                          
166600     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
166700     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
166800     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
166900     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
167000     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
167100     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
167200                                                                          
167300     PERFORM IMS-REPL-WDE411                                              
167400     PERFORM DABA-EV-SKAPA-RYK-TRANS                                      
167500     .                                                                    
167600     EJECT                                                                
167700 DABA-EV-SKAPA-RYK-TRANS SECTION.                                         
167800     MOVE 'STA DABA-EV-SKAPA'     TO WS-PGM-POSITION                      
167900                                                                          
168000     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
168100        ORAD-TIRODAT         > ZERO                                       
168200                                                                          
168300       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
168400       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
168500       ADD +1 TO LOGG-IDLOGLOP                                            
168600       IF LOGG-IDLOGLOP = 0                                               
168700         ADD +1 TO LOGG-TIKLOCK                                           
168800         MOVE +1 TO LOGG-IDLOGLOP                                         
168900       END-IF                                                             
169000                                                                          
169100       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
169200                                       W-WDQ2C-IDDISTR                    
169300       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
169400                                       W-WDQ2C-IDKUNDNR                   
169500       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
169600                                  TO   W-WDQ2C-IDORDNR5                   
169700                                                                          
169800       PERFORM IMS-GU-ORQI01-CSEQ-ORQL                                    
169900       IF SEGMENT-FINNS                                                   
170000         MOVE OHUV-IDORDER      TO   RYK-IDORDER                          
170100       ELSE                                                               
170200         MOVE ZERO              TO   RYK-IDORDER                          
170300       END-IF                                                             
170400                                                                          
170500       MOVE    'RYK'              TO   RYK-IDPTYP                         
170600                                       LOGG-IDPTYP                        
170700                                                                          
170800       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
170900       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
171000       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
171100       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
171200       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
171300       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
171400       MOVE    ZERO               TO   RYK-KDORDBEK                       
171500                                                                          
171600       MOVE    SPACE              TO   LOGG-SORTPOST                      
171700       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
171800                                                                          
171900       PERFORM IMS-ISRT-ZZAC01                                            
172000                                                                          
172100       PERFORM UNTIL SEGMENT-FINNS                                        
172200         ACCEPT LOGG-TIKLOCK FROM TIME                                    
172300         ADD +1 TO LOGG-IDLOGLOP                                          
172400         IF LOGG-IDLOGLOP = 0                                             
172500            ADD +1 TO LOGG-TIKLOCK                                        
172600            MOVE +1 TO LOGG-IDLOGLOP                                      
172700         END-IF                                                           
172800         PERFORM IMS-ISRT-ZZAC01                                          
172900       END-PERFORM                                                        
173000     END-IF                                                               
173100     MOVE 'END DABA-EV-SKAPA'     TO WS-PGM-POSITION                      
173200     .                                                                    
173300     EJECT                                                                
173400 DAC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
173500     SKIP3                                                                
173600     MOVE WS-IDPRODNR        TO KKOLLI-IDPRODNR                           
173700     MOVE WS-IDKOLLI         TO KKOLLI-IDKOLLI                            
173800     MOVE SPAR-PRAD-KVLEVART TO KKOLLI-KVLEVART                           
173900     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
174000     IF SEGMENT-FINNS-REDAN                                               
174100         MOVE WS-IDPRODNR    TO  W-421-IDPRODNR                           
174200         MOVE WS-IDKOLLI     TO  W-421-IDKOLLI                            
174300         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
174400         ADD SPAR-PRAD-KVLEVART  TO KKOLLI-KVLEVART                       
174500         PERFORM IMS-REPL-KOLLI-KOPPL                                     
174600     ELSE                                                                 
174700         ADD +1                  TO ARB-KOLLI-KVORDRAD                    
174800                                                                          
174900         IF SPAR-PRAD-KDFARLIG = +4                                       
175000         OR SPAR-PRAD-KDFARLIG = +7                                       
175100             ADD +1              TO ARB-KOLLI-KVFALRAD                    
175200         END-IF                                                           
175300     END-IF                                                               
175400*                                                                         
175500     IF W-IDDC NOT = W-IDDC-B6                                            
175600        MOVE W-IDDC TO W-IDDC-B6                                          
175700        PERFORM IMS-GU-WDB601                                             
175800     END-IF                                                               
175900     IF DCS-NDC-NA                                                        
176000        PERFORM S24-PACK-UPPG-DEL-NOTE                                    
176100     END-IF                                                               
176200                                                                          
176300     IF SPAR-IDPSN > ZERO                                                 
176400       PERFORM DACA-SPARA-FG-DATA                                         
176500     END-IF                                                               
176600     .                                                                    
176700     EJECT                                                                
176800 DACA-SPARA-FG-DATA SECTION.                                              
176900     SKIP3                                                                
177000     MOVE +1 TO FG-INDX                                                   
177100     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
177200                                                                          
177300       IF TAB-IDPSN(FG-INDX) = ZERO                                       
177400         MOVE SPAR-IDPSN TO TAB-IDPSN(FG-INDX)                            
177500         PERFORM S17-BERAEKNA-FG-FAELT                                    
177600                                                                          
177700       ELSE                                                               
177800         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
177900           PERFORM S17-BERAEKNA-FG-FAELT                                  
178000         END-IF                                                           
178100       END-IF                                                             
178200                                                                          
178300       ADD +1 TO FG-INDX                                                  
178400     END-PERFORM                                                          
178500                                                                          
178600     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
178700                            (SPAR-SUEQFG      *                           
178800                             KKOLLI-KVLEVART)                             
178900     .                                                                    
179000     EJECT                                                                
179100 DB-UPPDATERA-PRODTAB      SECTION.                                       
179200                                                                          
179300     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
179400* RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                                
179500                                                                          
179600                                                                          
179700       PERFORM IMS-GU-WDE401                                              
179800                                                                          
179900       PERFORM DBA-LAES-SHIFTTAB                                          
180000                                                                          
180100       MOVE KORD-IDORDER         TO W-301-IDORDER                         
180200                                    WS-IDORDER                            
180300       MOVE KORD-IDDC            TO W-301-IDDC                            
180400       MOVE KORD-IDPRODNR        TO W-301-IDPRODNR                        
180500       MOVE KORD-IDPLKLST        TO W-301-IDPLKLST                        
180600       PERFORM IMS-GU-ORQA01                                              
180700       MOVE ODEL-IDPRC           TO WS-ODEL-IDPRC                         
180800       MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                            
180900       MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                      
181000                                                                          
181100       IF  ODEL-KDPRODKL = 'B'                                            
181200       OR  ODEL-KDPRODKL = 'C'                                            
181300*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
181400                                                                          
181500         MOVE KORD-IDDC          TO W-4471-IDDC                           
181600         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
181700         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
181800         PERFORM IMS-GHU-XXKW11                                           
181900                                                                          
182000         IF  SEGMENT-FINNS                                                
182100*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
182200                                                                          
182300           MOVE 1                TO IND1                                  
182400           MOVE W-4478-IDSHIFT   TO IND2                                  
182500           MOVE ODEL-DARFS (3:10) TO HJALP-ODEL-TIRFS                     
182600           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
182700                                                                          
182800           PERFORM UNTIL IND1 = 30 OR                                     
182900                         4472-TIRFS (IND1) = ZERO OR                      
183000                         HJALP-ODEL-TIRFS-7 = HJALP-4472-TIRFS-7          
183100             ADD 1                TO IND1                                 
183200             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
183300           END-PERFORM                                                    
183400                                                                          
183500           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
183600           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
183700           ADD WS-RINT-ANT-FPACK-ORAD                                     
183800                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
183900           PERFORM DBB-ADDERA-TOTAL-PRODTID                               
184000           PERFORM IMS-REPL-XXKW11                                        
184100         END-IF                                                           
184200       END-IF                                                             
184300     END-IF                                                               
184400     .                                                                    
184500     EJECT                                                                
184600 DBA-LAES-SHIFTTAB         SECTION.                                       
184700                                                                          
184800     MOVE KORD-IDDC         TO W-4477-IDDC                                
184900     MOVE '1'               TO W-4478-IDSHIFT                             
185000     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
185100     PERFORM IMS-GU-XXLB                                                  
185200*                                                                         
185300     IF SEGMENT-SAKNAS                                                    
185400        MOVE '2'            TO W-4478-IDSHIFT                             
185500        PERFORM IMS-GU-XXLB                                               
185600*                                                                         
185700        IF SEGMENT-SAKNAS                                                 
185800           MOVE '3'         TO W-4478-IDSHIFT                             
185900           PERFORM IMS-GU-XXLB                                            
186000*                                                                         
186100           IF SEGMENT-SAKNAS                                              
186200              MOVE '1'      TO W-4478-IDSHIFT                             
186300           END-IF                                                         
186400        END-IF                                                            
186500     END-IF                                                               
186600     .                                                                    
186700     EJECT                                                                
186800 DBB-ADDERA-TOTAL-PRODTID             SECTION.                            
186900                                                                          
187000     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
187100                                                                          
187200     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
187300                                     ODEL-KVPTID                          
187400     END-COMPUTE                                                          
187500                                                                          
187600     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
187700     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
187800     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
187900                            (WS-KVPTID-TIM * 60)                          
188000                                                                          
188100     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
188200     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
188300     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
188400     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
188500                            (WS-KVPTID-TIM * 60)                          
188600     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
188700                                                                          
188800     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
188900     .                                                                    
189000     EJECT                                                                
189100 E-HAMTA-ADRESS SECTION.                                                  
189200                                                                          
189300     PERFORM IMS-GU-WDE401                                                
189400     MOVE KORD-IDDISTR    TO W-401-IDDISTR                                
189500     MOVE KORD-IDKUNDNR   TO W-401-IDKUNDNR                               
189600     MOVE KORD-IDORDNR5   TO W-401-IDORDNR                                
189700     MOVE KORD-IDPRODNR   TO W-401-IDPRODNR                               
189800     MOVE KORD-IDPLKLST   TO W-401-IDPLKLST                               
189900     MOVE KORD-IDORDER    TO W-201-IDORDER                                
190000     MOVE KORD-IDDISTR    TO WS-SAVE-IDDISTR                              
190100     MOVE KORD-IDKUNDNR   TO WS-SAVE-IDKUNDNR                             
190200     MOVE KORD-IDDC       TO W-IDDC                                       
190300     MOVE KORD-FLLSBOK    TO WS-FLLSBOK                                   
190400     MOVE KORD-FLORDSPE   TO WS-FLORDSPE                                  
190500     MOVE KORD-FLOVRLEV   TO WS-FLOVRLEV                                  
190600     MOVE KORD-KDFRAKT    TO WS-KDFRAKT                                   
190700     MOVE KORD-KDFAKTYP   TO WS-KDFAKTYP                                  
190800     MOVE KORD-KDORDKL    TO WS-KDORDKL                                   
190900     MOVE KORD-KVORDRAD-PACK TO WS-KVORDRAD-PACK                          
191000                                                                          
191100     COMPUTE WS-KVORDRAD = KORD-KVORDRAD +                                
191200                           KORD-KVORDRAD-LEVPL                            
191300                                                                          
191400     MOVE    WS-IDORDER           TO W-201-IDORDER                        
191500     PERFORM IMS-GU-ORQI01                                                
191600                                                                          
191700     MOVE OHUV-BEKUNDRF           TO SPAR-BEKUNDRF                        
191800                                                                          
191900     MOVE KORD-IDDC       TO W-IDDC-Q2                                    
192000     PERFORM IMS-GHNP-ORQI12                                              
192100     .                                                                    
192200     EJECT                                                                
192300 F-UPPDATERA-KOLLIREG      SECTION.                                       
192400                                                                          
192500     PERFORM IMS-GHU-WDE601                                               
192600     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
192700     MOVE VORD-KDORDKL       TO  WS-KDORDKL                               
192800     MOVE VORD-FLAUTFAK      TO  WS-FLAUTFAK                              
192900     MOVE VORD-KDFAKTYP      TO  WS-KDFAKTYP                              
193000     MOVE VORD-DARFS         TO  WS-DARFS                                 
193100     IF VORD-KDORDSTA        =   1                                        
193200         MOVE 2              TO  VORD-KDORDSTA                            
193300     END-IF                                                               
193400     COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + ARB-ANTAL-KOLLI              
193500     COMPUTE VORD-KVORDRAD-PACK                                           
193600                         = VORD-KVORDRAD-PACK + WS-TOT-ANT-RADER          
193700                                                                          
193800     ADD ARB-ANTAL-KOLLI      TO  VORD-KVKOLLI                            
193900     MOVE WS-DAGENS-DATUM     TO  VORD-TIPACKN-SK                         
194000     COMPUTE VORD-VKORDBTO    ROUNDED                                     
194100                     = VORD-VKORDBTO    + WS-VKORDBTO                     
194200                       * ARB-ANTAL-KOLLI                                  
194300*970409                                                                   
194400     IF VORD-VKORDNTO > VORD-VKORDBTO                                     
194500       MOVE VORD-VKORDBTO     TO VORD-VKORDNTO                            
194600     END-IF                                                               
194700*970409                                                                   
194800     COMPUTE VORD-VLORDBTO    ROUNDED                                     
194900                   = VORD-VLORDBTO    + WS-VLORDBTO                       
195000                       * ARB-ANTAL-KOLLI                                  
195100     COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                                 
195200                  = VORD-SUORDV-PACK-LOC + ARB-KOLLI-SUORDV-LOC           
195300                       * ARB-ANTAL-KOLLI                                  
195400     COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                             
195500          = VORD-SUORDV-PACK-LOCPREL + ARB-KOLLI-SUORDV-LOCPREL           
195600                       * ARB-ANTAL-KOLLI                                  
195700     COMPUTE VORD-SUORDV-PACK ROUNDED                                     
195800                     = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV                
195900                       * ARB-ANTAL-KOLLI                                  
196000                                                                          
196100       MOVE ARB-KOLLI-KDVALISO     TO VORD-KDVALISO                       
196200       MOVE ARB-KOLLI-KDVALISO-EXP TO VORD-KDVALISO-EXP                   
196300                                                                          
196400     IF W-IDDC NOT = W-IDDC-B6                                            
196500        MOVE W-IDDC TO W-IDDC-B6                                          
196600        PERFORM IMS-GU-WDB601                                             
196700     END-IF                                                               
196800     IF  VORD-KDORDSTA < +3                                               
196900     AND VORD-KVKOLLI  > +0                                               
197000     AND (DIST03-SVERIGE OR DIST35-CDC-1A-REFILL                          
197100                         OR DIST35-CDC-1B-REFILL)                         
197200     AND (DCS-CDC OR (DCS-SDC AND DCS-SWEDEN))                            
197300                                                                          
197400        MOVE VORD-KDFRAKT       TO WS-KDFRAKT-NUM2                        
197500        MOVE WS-KDFRAKT-NUM2    TO FRAK01-KDFRAKT                         
197600        IF  FRAK01-SVERIGE2                                               
197700        OR  FRAK01-NORDEN                                                 
197800        OR  FRAK01-KDFRAKT21                                              
197900        OR  FRAK01-KDFRAKT62                                              
198000        OR (WS-IDKOLLI-NUM > 149 AND WS-IDKOLLI-NUM < 200)                
198100        OR (WS-IDKOLLI-NUM > 349 AND WS-IDKOLLI-NUM < 400)                
198200          IF   VORD-FLDIRLEV = NEJ                                        
198300          AND  VORD-KDFRAKT  NOT = +17                                    
198400            IF NOT DIS128-FRAKTS                                          
198500              MOVE JA TO VORD-FLFRAKTS                                    
198600            END-IF                                                        
198700          END-IF                                                          
198800        END-IF                                                            
198900     END-IF                                                               
199000                                                                          
199100     PERFORM IMS-REPL-KOLLIREG                                            
199200     SKIP2                                                                
199300     PERFORM FA-BEHANDLA-KOLLI                                            
199400     .                                                                    
199500     EJECT                                                                
199600 FA-BEHANDLA-KOLLI      SECTION.                                          
199700     SKIP3                                                                
199800     MOVE WS-IDKOLLI              TO  W-610-IDKOLLI                       
199900     PERFORM IMS-GHU-KOLLI                                                
200000     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
200100     IF KOLLI-KDKOLSTA            =   ZERO                                
200200*KDKOLSTA                                                                 
200300         MOVE 1                   TO KOLLI-KDKOLSTA                       
200400         MOVE WS-DAGENS-DATUM     TO KOLLI-TIPACKN                        
200500         MOVE WS-TIDPUNKT         TO WS-TIDPUNKT-RED                      
200600         MOVE WS-HHMMSS           TO KOLLI-TIPACTID                       
200700         MOVE WS-DARFS            TO KOLLI-DARFS                          
200800         PERFORM S12-SKAPA-4322                                           
200900                                                                          
201000         IF DIST19-SATS                                                   
201100           CONTINUE                                                       
201200         ELSE                                                             
201300           MOVE 'U*' TO ARB-KDORDSTA                                      
201400           PERFORM IMS-REPL-ORQI12                                        
201500         END-IF                                                           
201600     END-IF                                                               
201700*                                                                         
201800     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
201900*                                                                         
202000     PERFORM IMS-REPL-KOLLI                                               
202100*LK CASE INFO TO TMS                                                      
202200         MOVE KORD-IDDC          TO TMS-IDDC                              
202300         MOVE KORD-IDDISTR       TO TMS-IDDISTR                           
202400         MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                          
202500         MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                          
202600         MOVE KOLLI-IDKOLLI      TO TMS-IDKOLLI(1)                        
202700         CALL W403TMS1 USING TMS-W403TMS1                                 
202800            TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                              
202900            TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                        
203000            TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                        
203100            TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                      
203200            TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                        
203300            TMS-WDK5-PCB TMS-WDQ2C-PCB                                    
203400     .                                                                    
203500     EJECT                                                                
203600 G-EV-UPDATE-4726-4727        SECTION.                                    
203700                                                                          
203800     IF WS-FLAUTFAK     = JA                                              
203900     AND DIST03-SVERIGE-2                                                 
204000                                                                          
204100         PERFORM GA-ISRT-4726-4727                                        
204200                                                                          
204300     END-IF                                                               
204400     .                                                                    
204500     SKIP2                                                                
204600 GA-ISRT-4726-4727       SECTION.                                         
204700                                                                          
204800     MOVE '4726'                  TO W-4726-IDHTYP                        
204900     IF W-IDDC NOT = W-IDDC-B6                                            
205000        MOVE W-IDDC TO W-IDDC-B6                                          
205100        PERFORM IMS-GU-WDB601                                             
205200     END-IF                                                               
205300*    IF  DIST03-SVERIGE                                                   
205400*    OR  DIST18-SKROT                                                     
205500*        IF  DCS-CDC                                                      
205600*        OR (DCS-SDC AND DCS-SWEDEN)                                      
205700*        OR  DIST18-SKROT                                                 
205800*           MOVE JA               TO  W-4726-FLBATCH                      
205900*        ELSE                                                             
206000*           MOVE NEJ              TO  W-4726-FLBATCH                      
206100*        END-IF                                                           
206200*    ELSE                                                                 
206300         MOVE NEJ                 TO W-4726-FLBATCH                       
206400*    END-IF                                                               
206500     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
206600                                                                          
206700     PERFORM IMS-GU-4726-ROT-KVAL                                         
206800                                                                          
206900     MOVE WS-IDDISTR-NUM          TO W-4726-IDDISTR                       
207000     MOVE WS-IDKUNDNR-NUM         TO W-4726-IDKUNDNR                      
207100     MOVE W-IDDC                  TO W-4726-IDDC                          
207200     MOVE WS-KDFAKTYP             TO W-4726-KDFAKTYP                      
207300                                                                          
207400     PERFORM IMS-GNP-4726-UNDERSEG-KVAL                                   
207500                                                                          
207600     IF  SEGMENT-SAKNAS                                                   
207700         MOVE WS-IDDISTR-NUM      TO AUTFAKT-IDDISTR                      
207800         MOVE WS-IDKUNDNR-NUM     TO AUTFAKT-IDKUNDNR                     
207900         MOVE W-IDDC              TO AUTFAKT-IDDC                         
208000         MOVE WS-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
208100                                                                          
208200         PERFORM IMS-INSERT-4726-UNDERSEG                                 
208300     END-IF                                                               
208400     MOVE WS-IDPRODNR             TO AUTFAKT-IDPRODNR                     
208500     MOVE ZERO                    TO AUTFAKT-IDSKEPPN                     
208600                                     AUTFAKT-PRFRAKT                      
208700                                                                          
208800     IF  DIST03-SVERIGE                                                   
208900         MOVE NEJ                 TO AUTFAKT-FLLASTA                      
209000     ELSE                                                                 
209100         MOVE JA                  TO AUTFAKT-FLLASTA                      
209200     END-IF                                                               
209300                                                                          
209400     PERFORM IMS-INSERT-4727                                              
209500                                                                          
209600     .                                                                    
209700     EJECT                                                                
209800 I-BEHANDLA-INTERVALL  SECTION.                                           
209900                                                                          
210000*                                                                         
210100        MOVE WS-IDPURAD TO W-420-IDPURAD                                  
210200        PERFORM IMS-GHU-WDE401                                            
210300        PERFORM IMS-GHNP-WDE411                                           
210400        MOVE ZERO                 TO  INX-TOT-ANT-RADER                   
210500*                                                                         
210600        PERFORM UNTIL SEGMENT-SAKNAS                                      
210700                                                                          
210800           IF (ORAD-KDRADSTA >= 4   AND                                   
210900               ORAD-KVBEART  > ORAD-KVAVBART + ORAD-KVANNANT)             
211000                                                                          
211100*            * AVVIKELSE VID UTSKRIFT                                     
211200              ADD +1               TO  INX-TOT-ANT-RADER                  
211300                                                                          
211400              MOVE ORAD-WDE411     TO SPAR-ORAD-WDE411                    
211500              MOVE ZERO            TO WS-KVSLATTAT                        
211600                                      SPAR-KART-KVRESS-ART                
211700              PERFORM IB-BEHANDLA-RAD-INOM-INTERVALL                      
211800              PERFORM IMS-REPL-WDE411                                     
211900              PERFORM IH-LAS-ARTREG                                       
212000              PERFORM IJ-BESTAM-ORDERBEKR-KOD                             
212100              PERFORM IK-UPDATE-EV-KAMP-REG                               
212200              PERFORM IG-UPDATE-ROREG                                     
212300              PERFORM IE-UPDATE-ARTREG                                    
212400              PERFORM S18-GENERERA-AVVIKELSE-TRANS                        
212500           END-IF                                                         
212600           PERFORM IMS-GHNP-WDE411                                        
212700        END-PERFORM                                                       
212800                                                                          
212900        IF RAD-FINNS                                                      
213000          MOVE ORAD-IDPURAD TO WS-IDPURAD                                 
213100          MOVE JA           TO FL-420-SEGMENT                             
213200        END-IF                                                            
213300                                                                          
213400     PERFORM IF-UPDATE-ORDERREG                                           
213500     .                                                                    
213600     EJECT                                                                
213700 IB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
213800                                                                          
213900     COMPUTE WS-KVORAPP-PACK  = ORAD-KVAVBART -                           
214000                             ORAD-KVLEVART                                
214100     END-COMPUTE                                                          
214200                                                                          
214300     COMPUTE WS-KVORAPP-TOTAL = ORAD-KVBEART -                            
214400                             ORAD-KVANNANT -                              
214500                             ORAD-KVLEVART                                
214600     END-COMPUTE                                                          
214700                                                                          
214800     IF ORAD-KDRADSTA = 4   AND                                           
214900        ORAD-KVAVBART = 0   AND                                           
215000        ORAD-KVANNANT > 0                                                 
215100        MOVE 0               TO WS-KVPRERO                                
215200     ELSE                                                                 
215300        IF W-IDDC NOT = W-IDDC-B6                                         
215400           MOVE W-IDDC TO W-IDDC-B6                                       
215500           PERFORM IMS-GU-WDB601                                          
215600        END-IF                                                            
215700        IF DCS-CDC                                                        
215800           COMPUTE WS-KVPRERO    = ORAD-KVBEART  -                        
215900                                   ORAD-KVANNANT -                        
216000                                   ORAD-KVAVBART                          
216100           END-COMPUTE                                                    
216200        ELSE                                                              
216300           MOVE 0               TO WS-KVPRERO                             
216400        END-IF                                                            
216500     END-IF                                                               
216600                                                                          
216700     MOVE ORAD-KVLEVART                 TO  ORAD-KVAVBART                 
216800     MOVE +4                            TO  ORAD-KDRADSTA                 
216900     MOVE 'Y'                           TO  DATUM-SW                      
217000     PERFORM S01-UPPD-SPAR-UPPGIFTER                                      
217100     EJECT                                                                
217200     .                                                                    
217300 IE-UPDATE-ARTREG  SECTION.                                               
217400                                                                          
217500     IF SPAR-ORAD-FLDIRLEV = NEJ OR                                       
217600        SPAR-ORAD-FLRESTN  = JA                                           
217700                                                                          
217800        IF W-IDDC NOT = W-IDDC-B6                                         
217900           MOVE W-IDDC TO W-IDDC-B6                                       
218000           PERFORM IMS-GU-WDB601                                          
218100        END-IF                                                            
218200        IF DCS-CDC                                                        
218300          PERFORM IEA-UPDATE-PRERO-WDK9                                   
218400          PERFORM IEB-UPDATE-SALDO-CDC                                    
218500        ELSE                                                              
218600          IF DCS-SDC                                                      
218700            PERFORM IEC-UPDATE-SALDO-SDC                                  
218800          ELSE                                                            
218900            IF DCS-NDC                                                    
219000              PERFORM IED-UPDATE-SALDO-NDC                                
219100            END-IF                                                        
219200          END-IF                                                          
219300        END-IF                                                            
219400     END-IF                                                               
219500     PERFORM IED-EV-UPDATE-REFILL-SDC                                     
219600     EJECT                                                                
219700     .                                                                    
219800 IEA-UPDATE-PRERO-WDK9  SECTION.                                          
219900                                                                          
220000     IF WS-FLLSBOK  = JA                                                  
220100       IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                         
220200         MOVE SPAR-ORAD-IDARTNR       TO W-901-IDARTNR                    
220300         PERFORM IMS-GHU-WDK901                                           
220400                                                                          
220500         PERFORM IEAB-UPDATE-PRERO-CDC                                    
220600                                                                          
220700         PERFORM IMS-REPL-WDK901                                          
220800       END-IF                                                             
220900     END-IF                                                               
221000     .                                                                    
221100     EJECT                                                                
221200 IEAB-UPDATE-PRERO-CDC                SECTION.                            
221300                                                                          
221400     IF SPAR-ORAD-KDORDKL = 1                                             
221500       COMPUTE ART-KVPRERO-DAG =                                          
221600               ART-KVPRERO-DAG -                                          
221700               WS-KVPRERO                                                 
221800       END-COMPUTE                                                        
221900     ELSE                                                                 
222000       IF SPAR-ORAD-KDORDKL = 2 OR 3 OR 4                                 
222100         COMPUTE ART-KVPRERO-BULK =                                       
222200                 ART-KVPRERO-BULK -                                       
222300                 WS-KVPRERO                                               
222400         END-COMPUTE                                                      
222500       END-IF                                                             
222600     END-IF                                                               
222700     .                                                                    
222800     EJECT                                                                
222900 IEB-UPDATE-SALDO-CDC SECTION.                                            
223000                                                                          
223100     IF WS-FLLSBOK = JA                                                   
223200        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
223300        PERFORM IMS-GHU-ARTC11                                            
223400        MOVE CLAG-IDANSK TO WS-IDANSK                                     
223500                                                                          
223600        IF SPAR-ORAD-FLDIRLEV = NEJ                                       
223700*SVS FROG                                                                 
223800*          MOVE AVSP-IDORDER     TO W-301-IDORDER                         
223900*          MOVE AVSP-IDDC        TO W-301-IDDC                            
224000*          MOVE AVSP-IDPRODNR    TO W-301-IDPRODNR                        
224100*          MOVE AVSP-IDPLKLST    TO W-301-IDPLKLST                        
224200*          PERFORM IMS-GU-ORQA01                                          
224300                                                                          
224400           MOVE AVSP-IDDISTR     TO TEST-IDDISTR                          
224500           IF DIST20-EMBALLAGE-SVS                                        
224600           OR WS-ODEL-IDPRC = '2600'                                      
224700             COMPUTE CLAG-KVLS-SVS =                                      
224800                     CLAG-KVLS-SVS + WS-KVORAPP-PACK                      
224900             END-COMPUTE                                                  
225000           END-IF                                                         
225100                                                                          
225200           COMPUTE CLAG-KVLS    = CLAG-KVLS    + WS-KVORAPP-PACK          
225300           END-COMPUTE                                                    
225400           COMPUTE CLAG-KVEFRS  = CLAG-KVEFRS  - WS-KVORAPP-PACK          
225500           END-COMPUTE                                                    
225600                                                                          
225700           IF SPAR-ORAD-IDKAMPRF   >  0   AND                             
225800              SPAR-KART-KVRESS-ART >= 0                                   
225900             COMPUTE CLAG-KVRESS = CLAG-KVRESS + WS-KVSLATTAT             
226000             END-COMPUTE                                                  
226100           END-IF                                                         
226200        END-IF                                                            
226300                                                                          
226400        IF SPAR-ORAD-FLRESTN  = JA           AND                          
226500          (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                         
226600                              - SPAR-ORAD-KVANNANT                        
226700                              - SPAR-ORAD-KVSLATT)                        
226800           IF SPAR-ORAD-KDORDKL > 0                                       
226900               PERFORM S20-EV-LARM-2191-MID                               
227000                                                                          
227100             IF W-IDDC NOT = W-IDDC-B6                                    
227200                MOVE W-IDDC TO W-IDDC-B6                                  
227300                PERFORM IMS-GU-WDB601                                     
227400             END-IF                                                       
227500             IF DCS-CDC                                                   
227600               COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-TOTAL         
227700               END-COMPUTE                                                
227800             ELSE                                                         
227900               COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK          
228000               END-COMPUTE                                                
228100             END-IF                                                       
228200           END-IF                                                         
228300                                                                          
228400           IF CLAG-KVROS = WS-KVORAPP-TOTAL                               
228500              MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                           
228600           END-IF                                                         
228700        END-IF                                                            
228800        PERFORM IMS-REPL-ARTC                                             
228900        PERFORM S22-SKAPA-SALDOLOGG                                       
229000     END-IF                                                               
229100     .                                                                    
229200     EJECT                                                                
229300 IEC-UPDATE-SALDO-SDC      SECTION.                                       
229400                                                                          
229500     IF WS-FLLSBOK = JA                                                   
229600        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
229700        MOVE W-IDDC                   TO W-711-IDDC                       
229800        PERFORM IMS-GU-WDK722                                             
229900        IF SEGMENT-FINNS AND XLAG-IDANSK > 0                              
230000          MOVE XLAG-IDANSK            TO WS-IDANSK                        
230100        ELSE                                                              
230200          PERFORM IMS-GU-ARTC11                                           
230300          MOVE CLAG-IDANSK            TO WS-IDANSK                        
230400        END-IF                                                            
230500        PERFORM IMS-GHU-WDK711                                            
230600                                                                          
230700        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
230800        END-COMPUTE                                                       
230900        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
231000        END-COMPUTE                                                       
231100                                                                          
231200        IF NOT DCS-CHINA                                                  
231300           PERFORM IMS-REPL-WDK7                                          
231400           PERFORM S23-SKAPA-SALDOLOGG                                    
231500           PERFORM IECA-UPDATE-CLAG-KVROS                                 
231600        ELSE                                                              
231700           PERFORM IECB-UPDATE-SLAG-KVROS                                 
231800           PERFORM IMS-REPL-WDK7                                          
231900           PERFORM S23-SKAPA-SALDOLOGG                                    
232000        END-IF                                                            
232100     END-IF                                                               
232200     .                                                                    
232300     EJECT                                                                
232400 IECA-UPDATE-CLAG-KVROS    SECTION.                                       
232500                                                                          
232600     IF SPAR-ORAD-KDORDKL > 0  AND                                        
232700        WS-KVORAPP-TOTAL > ZERO                                           
232800                                                                          
232900       IF SPAR-ORAD-FLRESTN  = JA           AND                           
233000         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
233100                             - SPAR-ORAD-KVANNANT                         
233200                             - SPAR-ORAD-KVSLATT)                         
233300                                                                          
233400          PERFORM IMS-GHU-ARTC11                                          
233500          MOVE DCS-IDDC    TO W-711-IDDC                                  
233600          MOVE CLAG-IDANSK TO WS-IDANSK                                   
233700                                                                          
233800          PERFORM S20-EV-LARM-2191-MID                                    
233900          COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK               
234000          END-COMPUTE                                                     
234100                                                                          
234200          IF CLAG-KVROS = WS-KVORAPP-TOTAL                                
234300             MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                            
234400          END-IF                                                          
234500          PERFORM IMS-REPL-ARTC                                           
234600       END-IF                                                             
234700     END-IF                                                               
234800     .                                                                    
234900     EJECT                                                                
235000 IECB-UPDATE-SLAG-KVROS    SECTION.                                       
235100                                                                          
235200     IF SPAR-ORAD-KDORDKL > 0  AND                                        
235300        WS-KVORAPP-TOTAL > ZERO                                           
235400                                                                          
235500       IF SPAR-ORAD-FLRESTN  = JA           AND                           
235600         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
235700                             - SPAR-ORAD-KVANNANT                         
235800                             - SPAR-ORAD-KVSLATT)                         
235900                                                                          
236000          IF  SPAR-ORAD-IDDC-RO = W-IDDC                                  
236100            IF SLAG-IDDC-REF = SPACE                                      
236200               PERFORM S20-EV-LARM-2191-MID-CN-US                         
236300            END-IF                                                        
236400            IF SPAR-ORAD-KDORDKL > 1                                      
236500              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
236600                                      + WS-KVORAPP-PACK                   
236700            ELSE                                                          
236800              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
236900                                      + WS-KVORAPP-PACK                   
237000            END-IF                                                        
237100          ELSE                                                            
237200            PERFORM IMS-REPL-WDK7                                         
237300            MOVE SPAR-ORAD-IDDC-RO   TO W-711-IDDC                        
237400            PERFORM IMS-GHU-WDK711                                        
237500            IF SLAG-IDDC-REF = SPACE                                      
237600               PERFORM S20-EV-LARM-2191-MID-CN-US                         
237700            END-IF                                                        
237800            IF SPAR-ORAD-KDORDKL > 1                                      
237900              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
238000                                      + WS-KVORAPP-PACK                   
238100            ELSE                                                          
238200              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
238300                                      + WS-KVORAPP-PACK                   
238400              END-COMPUTE                                                 
238500            END-IF                                                        
238600            MOVE W-IDDC              TO W-711-IDDC                        
238700          END-IF                                                          
238800                                                                          
238900       END-IF                                                             
239000     END-IF                                                               
239100     .                                                                    
239200     EJECT                                                                
239300 IED-UPDATE-SALDO-NDC      SECTION.                                       
239400                                                                          
239500     IF  WS-FLLSBOK = JA                                                  
239600     AND SPAR-ORAD-FLDIRLEV = NEJ                                         
239700        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
239800        MOVE W-IDDC                   TO W-711-IDDC                       
239900        PERFORM IMS-GHU-WDK711                                            
240000                                                                          
240100        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
240200        END-COMPUTE                                                       
240300        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
240400        END-COMPUTE                                                       
240500        PERFORM S23-SKAPA-SALDOLOGG                                       
240600                                                                          
240700        IF  SPAR-ORAD-FLRESTN = JA                                        
240800        AND SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                        
240900                               - SPAR-ORAD-KVANNANT                       
241000                               - SPAR-ORAD-KVSLATT                        
241100        AND SPAR-ORAD-KDORDKL > 0                                         
241200          IF  SPAR-ORAD-IDDC-RO = W-IDDC                                  
241300            IF  DCS-NDC-CN                                                
241400            OR (DCS-NDC-NA AND DCS-USA)                                   
241500              IF SLAG-IDDC-REF = SPACE                                    
241600                PERFORM S20-EV-LARM-2191-MID-CN-US                        
241700              END-IF                                                      
241800            END-IF                                                        
241900            IF SPAR-ORAD-KDORDKL > 1                                      
242000              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
242100                                      + WS-KVORAPP-TOTAL                  
242200              END-COMPUTE                                                 
242300            ELSE                                                          
242400              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
242500                                      + WS-KVORAPP-TOTAL                  
242600              END-COMPUTE                                                 
242700            END-IF                                                        
242800            PERFORM IMS-REPL-WDK7                                         
242900          ELSE                                                            
243000            IF SPAR-ORAD-IDDC-RO = '11'                                   
243100              MOVE 'SPAR-ORAD-IDDC-RO HAR FEL IDDC'  TO  FELTEXT          
243200            END-IF                                                        
243300            PERFORM IMS-REPL-WDK7                                         
243400            MOVE SPAR-ORAD-IDDC-RO        TO W-711-IDDC                   
243500            PERFORM IMS-GHU-WDK711                                        
243600            IF  DCS-NDC-CN                                                
243700            OR (DCS-NDC-NA AND DCS-USA)                                   
243800              IF SLAG-IDDC-REF = SPACE                                    
243900                PERFORM S20-EV-LARM-2191-MID-CN-US                        
244000              END-IF                                                      
244100            END-IF                                                        
244200            IF SPAR-ORAD-KDORDKL > 1                                      
244300              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
244400                                      + WS-KVORAPP-TOTAL                  
244500              END-COMPUTE                                                 
244600            ELSE                                                          
244700              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
244800                                      + WS-KVORAPP-TOTAL                  
244900              END-COMPUTE                                                 
245000            END-IF                                                        
245100            PERFORM IMS-REPL-WDK7                                         
245200            MOVE W-IDDC                   TO W-711-IDDC                   
245300          END-IF                                                          
245400        ELSE                                                              
245500          PERFORM IMS-REPL-WDK7                                           
245600        END-IF                                                            
245700     END-IF                                                               
245800     .                                                                    
245900     EJECT                                                                
246000 IED-EV-UPDATE-REFILL-SDC  SECTION.                                       
246100*REFILLORDER                                                              
246200                                                                          
246300     IF WS-FLLSBOK = JA                                                   
246400                                                                          
246500******************************************************************        
246600*                                                                         
246700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
246800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
246900*                                                                         
247000******************************************************************        
247100                                                                          
247200       MOVE KORD-IDDISTR       TO W-TP4TRAN-IDDISTR                       
247300                                                                          
247400       PERFORM DB2-SELECT-TP4TRAN                                         
247500                                                                          
247600       MOVE KORD-IDDISTR            TO TEST-IDDISTR                       
247700                                                                          
247800                                                                          
247900*NDC&SDC OCH INTE RESTNOTERING.                                           
248000       IF  (DIST35-REFILL                                                 
248100        OR  DIST35-REFILL-INOM-NDC                                        
248200        OR  DIST35-REFILL-NA-JAP                                          
248300        OR  DIST35-NA-TRANSFER                                            
248400        OR  DIST35-PACIFIC-TRANSFER                                       
248500        OR  DIST35-REFILL-INOM-JP                                         
248600        OR  DIST35-CN-TRANSFER                                            
248700        OR  DIST35-NA-NDC-RETURNS                                         
248800        OR  RADER-FINNS)                                                  
248900       AND SPAR-ORAD-FLRESTN = NEJ                                        
249000       AND WS-KVORAPP-TOTAL > ZERO                                        
249100         IF RADER-FINNS                                                   
249200           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
249300         ELSE                                                             
249400           PERFORM IEDA-GET-SDC-IDDC-VALUE                                
249500         END-IF                                                           
249600         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
249700         PERFORM IMS-GHU-WDK711                                           
249800         SUBTRACT WS-KVORAPP-TOTAL  FROM SLAG-KVBEART                     
249900         PERFORM IMS-REPL-WDK7                                            
250000       END-IF                                                             
250100                                                                          
250200*NDC&SDC RESTNOTERING (SLATTGRÄNSEN ÖVERSKRIDEN).                         
250300       IF  (DIST35-REFILL                                                 
250400        OR  DIST35-REFILL-INOM-NDC                                        
250500        OR  DIST35-REFILL-NA-JAP                                          
250600        OR  DIST35-NA-TRANSFER                                            
250700        OR  DIST35-PACIFIC-TRANSFER                                       
250800        OR  DIST35-REFILL-INOM-JP                                         
250900        OR  DIST35-CN-TRANSFER                                            
251000        OR  DIST35-NA-NDC-RETURNS                                         
251100        OR  RADER-FINNS)                                                  
251200       AND SPAR-ORAD-FLRESTN = JA                                         
251300       AND WS-KVORAPP-TOTAL > ZERO                                        
251400       AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                      
251500                                 SPAR-ORAD-KVANNANT -                     
251600                                 SPAR-ORAD-KVSLATT                        
251700                                                                          
251800         IF RADER-FINNS                                                   
251900           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
252000         ELSE                                                             
252100           PERFORM IEDA-GET-SDC-IDDC-VALUE                                
252200         END-IF                                                           
252300         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
252400         PERFORM IMS-GHU-WDK711                                           
252500         SUBTRACT WS-KVORAPP-TOTAL FROM SLAG-KVBEART                      
252600         PERFORM IMS-REPL-WDK7                                            
252700       END-IF                                                             
252800     END-IF                                                               
252900     SKIP2                                                                
253000     .                                                                    
253100 IEDA-GET-SDC-IDDC-VALUE            SECTION.                              
253200                                                                          
253300     SEARCH ALL DIST57-REFILL-DC                                          
253400        AT END                                                            
253500           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
253600                            TO FELTEXT                                    
253700           CALL FELLOG                                                    
253800        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                 
253900           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-711-IDDC              
254000     END-SEARCH                                                           
254100     .                                                                    
254200     EJECT                                                                
254300 IF-UPDATE-ORDERREG        SECTION.                                       
254400                                                                          
254500     PERFORM IMS-GHU-KUNDORDER                                            
254600                                                                          
254700     MOVE KORD-KDFAKTYP             TO  WS-KDFAKTYP                       
254800     COMPUTE KORD-VKORDNTO ROUNDED =                                      
254900             KORD-VKORDNTO - SPAR-VKORDNTO-DEL                            
255000     END-COMPUTE                                                          
255100     COMPUTE KORD-VLORDNTO = KORD-VLORDNTO - SPAR-VLORDNTO-DEL            
255200     END-COMPUTE                                                          
255300     IF SPAR-ORAD-FLDIRLEV = JA                                           
255400                                                                          
255500          COMPUTE KORD-SUORDV-LEVPL-LOC = KORD-SUORDV-LEVPL-LOC           
255600                                         - SPAR-SUORDV-DEL-LOC            
255700           COMPUTE KORD-SUORDV-LEVPL-LOCPREL                              
255800          = KORD-SUORDV-LEVPL-LOCPREL - SPAR-SUORDV-DEL-LOCPREL           
255900           COMPUTE KORD-SUORDV-LEVPL = KORD-SUORDV-LEVPL                  
256000                                         - SPAR-SUORDV-DEL                
256100     ELSE                                                                 
256200           COMPUTE KORD-SUORDV-LOC = KORD-SUORDV-LOC                      
256300                               - SPAR-SUORDV-DEL-LOC                      
256400           COMPUTE KORD-SUORDV-LOCPREL = KORD-SUORDV-LOCPREL              
256500                               - SPAR-SUORDV-DEL-LOCPREL                  
256600           COMPUTE KORD-SUORDV = KORD-SUORDV                              
256700                               - SPAR-SUORDV-DEL                          
256800     END-IF                                                               
256900                                                                          
257000     IF DATUM-SW = 'Y'                                                    
257100       MOVE WS-DAGENS-DATUM      TO  E401-KORD-TIBEGPAC                   
257200       MOVE 'N'                  TO  DATUM-SW                             
257300     END-IF                                                               
257400                                                                          
257500       MOVE SPAR-KDVALISO        TO KORD-KDVALISO                         
257600       MOVE SPAR-KDVALISO-EXP    TO KORD-KDVALISO-EXP                     
257700                                                                          
257800     PERFORM IMS-REPL-WDE401                                              
257900*                                                                         
258000     MOVE ZERO TO SPAR-SUORDV-DEL                                         
258100                  SPAR-SUORDV-DEL-LOC                                     
258200                  SPAR-SUORDV-DEL-LOCPREL                                 
258300                  SPAR-VKORDNTO-DEL                                       
258400                  SPAR-VLORDNTO-DEL                                       
258500     .                                                                    
258600     EJECT                                                                
258700 IG-UPDATE-ROREG           SECTION.                                       
258800                                                                          
258900     PERFORM IGE-HAMTA-TPOTYP-FRAN-ROREG                                  
259000     MOVE WS-IDDISTR-NUM             TO TEST-IDDISTR                      
259100     IF W-IDDC NOT = W-IDDC-B6                                            
259200        MOVE W-IDDC TO W-IDDC-B6                                          
259300        PERFORM IMS-GU-WDB601                                             
259400     END-IF                                                               
259500                                                                          
259600     IF SPAR-ORAD-FLRESTN = JA AND                                        
259700        ( SPAR-ORAD-KVLEVART     < SPAR-ORAD-KVBEART                      
259800                                 - SPAR-ORAD-KVANNANT                     
259900                                 - SPAR-ORAD-KVSLATT )                    
260000                                                                          
260100        IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                           
260200           PERFORM IGD-SAMMANSL-EJ-BIPACKAD-RAD                           
260300                                                                          
260400           IF WS-SAMMANSLAGNING-RAD = JA                                  
260500              IF DCS-CDC  OR  DCS-NDC                                     
260600                ADD WS-KVORAPP-TOTAL  TO OLD-RAD-KVART                    
260700              ELSE                                                        
260800                ADD WS-KVORAPP-PACK   TO OLD-RAD-KVART                    
260900              END-IF                                                      
261000              PERFORM IMS-REPL-ORDP01-OLD                                 
261100           ELSE                                                           
261200              PERFORM IGA-KATEGORI                                        
261300              PERFORM IGB-FLYTTA-WDA5-POSTER                              
261400              IF RAD-KVART > ZERO                                         
261500                PERFORM IMS-ISRT-ORDP01                                   
261600                PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                     
261700                   ADD +1               TO RAD-IDLOPNR                    
261800                   PERFORM IMS-ISRT-ORDP01                                
261900                END-PERFORM                                               
262000              END-IF                                                      
262100           END-IF                                                         
262200        ELSE                                                              
262300           MOVE WS-SAVE-IDDISTR       TO W1-IDDISTR                       
262400           MOVE WS-SAVE-IDKUNDNR      TO W1-IDKUNDNR                      
262500           MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF                      
262600           MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                       
262700           MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                       
262800                                                                          
262900           PERFORM IMS-GHU-ORDP01-GE                                      
263000           IF SEGMENT-SAKNAS                                              
263100*FIX START****************************************************            
263200*********** NÄR PGM ÅKER PÅ GE MOT WDA5 (ORDP01) *************            
263300                                                                          
263400             PERFORM IGA-KATEGORI                                         
263500             MOVE W1-IDARTNR          TO W-IDARTNR                        
263600             PERFORM IMS-GHU-ARTC11                                       
263700             MOVE DCS-IDDC            TO W-711-IDDC                       
263800             PERFORM IMS-GU-WDK722                                        
263900             IF SEGMENT-FINNS AND XLAG-IDANSK > 0                         
264000                MOVE XLAG-IDANSK      TO WS-IDANSK                        
264100             ELSE                                                         
264200                MOVE CLAG-IDANSK      TO WS-IDANSK                        
264300             END-IF                                                       
264400                                                                          
264500             MOVE W1-IDDISTR          TO RAD-IDDISTR                      
264600             MOVE W1-IDKUNDNR         TO RAD-IDKUNDNR                     
264700             MOVE W1-IDKUNDRF         TO RAD-IDKUNDRF                     
264800             MOVE W1-IDARTNR          TO RAD-IDARTNR                      
264900             MOVE W1-IDLOPNR          TO RAD-IDLOPNR                      
265000             MOVE SPAR-ORAD-BERADREF  TO RAD-BERADREF                     
265100             MOVE NEJ                 TO RAD-FLERS                        
265200             MOVE WS-IDANSK           TO RAD-IDANSK                       
265300             MOVE SPAR-ORAD-IDANALYS  TO RAD-IDANALYS                     
265400             MOVE SPAR-ORAD-IDKONTO   TO RAD-IDKONTO                      
265500             MOVE SPAR-ORAD-IDKST     TO RAD-IDKST                        
265600             MOVE '00000     '        TO RAD-IDKUNDRF-LEV                 
265700             MOVE W-IDDC              TO RAD-IDDC                         
265800             MOVE SPAR-ORAD-IDDC-RO   TO RAD-IDDC-RO                      
265900             MOVE SPAR-ORAD-KDDSP     TO RAD-KDDSP                        
266000             MOVE KORD-KDFAKTYP       TO RAD-KDFAKTYP                     
266100             MOVE SPAR-ORAD-KDFRAKT   TO RAD-KDFRAKT                      
266200             MOVE SPAR-ORAD-KDKVBRYT  TO RAD-KDKVBRYT                     
266300             MOVE SPAR-ORAD-KDOI      TO RAD-KDOI                         
266400             MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP                  
266500             MOVE SPAR-ORAD-KDORDING  TO RAD-KDORDING                     
266600             MOVE KORD-KDORDKL        TO RAD-KDORDKL                      
266700             MOVE SPAR-ORAD-KDPRODSL  TO RAD-KDPRODSL                     
266800             MOVE WS-KDRAPRIO         TO RAD-KDRAPRIO                     
266900             MOVE +4                  TO RAD-KDROO                        
267000             MOVE '4'                 TO RAD-KDSTARAD                     
267100             MOVE WS-KDTPOTYP         TO RAD-KDTPOTYP                     
267200             MOVE SPAR-ORAD-KDVRINFO  TO RAD-KDVRINFO                     
267300             IF DCS-CDC  OR  DCS-NDC                                      
267400               MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                        
267500                                              RAD-KVRO                    
267600             ELSE                                                         
267700               MOVE WS-KVORAPP-PACK   TO RAD-KVART                        
267800                                              RAD-KVRO                    
267900             END-IF                                                       
268000             MOVE SPAR-ORAD-PRARTNTO  TO RAD-PRARTNTO                     
268100             MOVE SPAR-ORAD-PRARTNTO-LOC                                  
268200                                      TO RAD-PRARTNTO-LOC                 
268300             MOVE SPAR-ORAD-PRARTNTO-LOCPREL                              
268400                                      TO RAD-PRARTNTO-LOCPREL             
268500             MOVE SPAR-ORAD-REKSIFFR  TO RAD-REKSIFFR                     
268600             MOVE ZERO                TO RAD-TIAVBOKN                     
268700             MOVE DAT-TIAAMMDD        TO RAD-DARODAT                      
268800                                         RAD-TIREGDAT                     
268900             MOVE DAT-TISEKEL         TO RAD-DARODAT (1:2)                
269000             MOVE ZERO                TO RAD-TIRES                        
269100             MOVE +0                  TO RAD-TITPO                        
269200             MOVE SPAR-ORAD-KDPRTYP   TO RAD-KDPRTYP                      
269300             MOVE SPAR-ORAD-BEVOLREF  TO RAD-BEVOLREF                     
269400             MOVE SPAR-ORAD-FLINVEST  TO RAD-FLINVEST                     
269500             MOVE SPAR-ORAD-FLPRTILL  TO RAD-FLPRTILL                     
269600             MOVE JA                  TO RAD-FLTPOBEK                     
269700             MOVE SPAR-BEKUNDRF       TO RAD-BEKUNDRF                     
269800             MOVE SPAR-ORAD-IDKAMPRF  TO RAD-IDKAMPRF                     
269900             MOVE SPAR-ORAD-IDLEVNR   TO RAD-IDLEVNR                      
270000             MOVE SPAR-ORAD-IDSYSTEM  TO RAD-IDSYSTEM                     
270100             MOVE SPAR-ORAD-KVBEART   TO RAD-KVBEART-Q                    
270200             MOVE WS-TTMMSS           TO RAD-TIREGTID                     
270300             MOVE 0                   TO RAD-DASENDAT                     
270400             MOVE 0                   TO RAD-TISENBEK-KL                  
270500                                                                          
270600             MOVE OHUV-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                 
270700             MOVE OHUV-TIREPDAT       TO RAD-TIREPDAT                     
270800             MOVE SPAR-ORAD-IDKUNDRF-WIP TO RAD-IDKUNDRF-WIP              
270900             MOVE SPAR-ORAD-PRAVCOST  TO RAD-PRAVCOST                     
271000             MOVE ARB-KDROPACK        TO RAD-KDROPACK                     
271100             MOVE SPAR-ORAD-IDARBREF  TO RAD-IDARBREF                     
271200                                                                          
271300             PERFORM IMS-ISRT-ORDP01                                      
271400              IF RAD-KVART > ZERO                                         
271500                PERFORM IMS-ISRT-ORDP01                                   
271600                PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                     
271700                   ADD +1               TO RAD-IDLOPNR                    
271800                   PERFORM IMS-ISRT-ORDP01                                
271900                END-PERFORM                                               
272000              END-IF                                                      
272100              PERFORM IMS-GHU-ORDP01                                      
272200           END-IF                                                         
272300*********** FIX SLUT  ****************************************            
272400                                                                          
272500           IF RAD-DARODAT = ZERO                                          
272600             MOVE JA TO SW-TIRODAT-LIKA-MED-ZERO                          
272700           END-IF                                                         
272800                                                                          
272900                                                                          
273000           PERFORM IGC-SAMMANSL-BIPACKAD-RAD                              
273100                                                                          
273200           IF WS-SAMMANSLAGNING-RAD = JA                                  
273300              IF DCS-CDC  OR  DCS-NDC                                     
273400                IF WS-KVORAPP-TOTAL = RAD-KVART                           
273500                  ADD WS-KVORAPP-TOTAL     TO OLD-RAD-KVART               
273600                  PERFORM IMS-REPL-ORDP01-OLD                             
273700                                                                          
273800                  MOVE WS-IDDISTR-NUM        TO W1-IDDISTR                
273900                  MOVE WS-IDKUNDNR-NUM       TO W1-IDKUNDNR               
274000                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
274100                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
274200                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
274300                  PERFORM IMS-GHU-ORDP01                                  
274400                                                                          
274500                  PERFORM IMS-DLET-ORDP01                                 
274600                                                                          
274700                ELSE                                                      
274800                  SUBTRACT WS-KVORAPP-TOTAL  FROM RAD-KVART               
274900                  PERFORM IMS-REPL-ORDP01                                 
275000                                                                          
275100                  ADD WS-KVORAPP-TOTAL       TO OLD-RAD-KVART             
275200                  PERFORM IMS-REPL-ORDP01-OLD                             
275300                END-IF                                                    
275400              ELSE                                                        
275500                IF WS-KVORAPP-PACK = RAD-KVART                            
275600                  ADD WS-KVORAPP-PACK    TO OLD-RAD-KVART                 
275700                  PERFORM IMS-REPL-ORDP01-OLD                             
275800                                                                          
275900                  MOVE WS-IDDISTR-NUM        TO W1-IDDISTR                
276000                  MOVE WS-IDKUNDNR-NUM       TO W1-IDKUNDNR               
276100                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
276200                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
276300                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
276400                  PERFORM IMS-GHU-ORDP01                                  
276500                                                                          
276600                  PERFORM IMS-DLET-ORDP01                                 
276700                ELSE                                                      
276800                  SUBTRACT WS-KVORAPP-PACK   FROM RAD-KVART               
276900                  PERFORM IMS-REPL-ORDP01                                 
277000                                                                          
277100                  ADD WS-KVORAPP-PACK        TO OLD-RAD-KVART             
277200                  PERFORM IMS-REPL-ORDP01-OLD                             
277300                END-IF                                                    
277400              END-IF                                                      
277500           ELSE                                                           
277600             IF DCS-CDC  OR  DCS-NDC                                      
277700              IF WS-KVORAPP-TOTAL = RAD-KVART                             
277800                 IF RAD-DARODAT = 0                                       
277900                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
278000                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
278100                 END-IF                                                   
278200                                                                          
278300                 MOVE +0                TO RAD-TIRES                      
278400                 MOVE +0                TO RAD-TIAVBOKN                   
278500                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
278600                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
278700                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
278800                                           RAD-IDDC-RO                    
278900                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
279000                 MOVE '2'               TO RAD-KDSTARAD                   
279100                 PERFORM IMS-REPL-ORDP01                                  
279200              ELSE                                                        
279300                 SUBTRACT WS-KVORAPP-TOTAL FROM RAD-KVART                 
279400                 PERFORM IMS-REPL-ORDP01                                  
279500                                                                          
279600                 IF RAD-DARODAT = 0                                       
279700                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
279800                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
279900                 END-IF                                                   
280000                                                                          
280100                 MOVE +0                TO RAD-TIRES                      
280200                 MOVE +0                TO RAD-TIAVBOKN                   
280300                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
280400                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
280500                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
280600                                           RAD-IDDC-RO                    
280700                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
280800                 MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                      
280900                 MOVE '2'               TO RAD-KDSTARAD                   
281000                 ADD +1                 TO RAD-IDLOPNR                    
281100                 PERFORM IMS-ISRT-ORDP01                                  
281200                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
281300                   ADD +1               TO RAD-IDLOPNR                    
281400                                                                          
281500                   PERFORM IMS-ISRT-ORDP01                                
281600                 END-PERFORM                                              
281700              END-IF                                                      
281800             ELSE                                                         
281900              IF WS-KVORAPP-PACK = RAD-KVART                              
282000                 IF RAD-DARODAT = 0                                       
282100                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
282200                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
282300                 END-IF                                                   
282400                                                                          
282500                 MOVE +0                TO RAD-TIRES                      
282600                 MOVE +0                TO RAD-TIAVBOKN                   
282700                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
282800                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
282900                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
283000                                           RAD-IDDC-RO                    
283100                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
283200                 MOVE '2'               TO RAD-KDSTARAD                   
283300                 PERFORM IMS-REPL-ORDP01                                  
283400              ELSE                                                        
283500                 SUBTRACT WS-KVORAPP-PACK FROM RAD-KVART                  
283600                 PERFORM IMS-REPL-ORDP01                                  
283700                                                                          
283800                 IF RAD-DARODAT = 0                                       
283900                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
284000                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
284100                 END-IF                                                   
284200                                                                          
284300                 MOVE +0                TO RAD-TIRES                      
284400                 MOVE +0                TO RAD-TIAVBOKN                   
284500                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
284600                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
284700                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
284800                                           RAD-IDDC-RO                    
284900                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
285000                 MOVE WS-KVORAPP-PACK   TO RAD-KVART                      
285100                 MOVE '2'               TO RAD-KDSTARAD                   
285200                 ADD +1                 TO RAD-IDLOPNR                    
285300                 PERFORM IMS-ISRT-ORDP01                                  
285400                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
285500                   ADD +1               TO RAD-IDLOPNR                    
285600                   PERFORM IMS-ISRT-ORDP01                                
285700                 END-PERFORM                                              
285800              END-IF                                                      
285900             END-IF                                                       
286000           END-IF                                                         
286100        END-IF                                                            
286200     END-IF                                                               
286300     EJECT                                                                
286400     .                                                                    
286500 IGA-KATEGORI              SECTION.                                       
286600                                                                          
286700*    I DENNA SEKTION LÄSES STYRREG FÖR ATT BESTÄMMA PRIO FÖR              
286800*    DEN NYA ORDERKLASSEN.                                                
286900                                                                          
287000     MOVE LOW-VALUE                  TO W-WDGXKEY-N5-MIN                  
287100     MOVE HIGH-VALUE                 TO W-WDGXKEY-N5-MAX                  
287200     MOVE LOW-VALUE                  TO W-KDRAPRIO-N5-MIN-X               
287300     MOVE HIGH-VALUE                 TO W-KDRAPRIO-N5-MAX-X               
287400     MOVE WS-KDTPOTYP                TO W-KDTPOTYP-N5                     
287500     MOVE WS-IDDISTR-NUM             TO W-IDDISTR-FOM-N5                  
287600                                        W-IDDISTR-TOM-N5                  
287700     MOVE SPAR-ORAD-KDORDKL          TO W-KDORDKL-N5                      
287800     MOVE '4511'                     TO W-IDHTYP-N5                       
287900     MOVE LOW-VALUE                  TO W-VALFRI-N5                       
288000                                                                          
288100     PERFORM IMS-GU-XXJN                                                  
288200                                                                          
288300     MOVE STYR-4512-KDRAPRIO         TO WS-KDRAPRIO                       
288400     EJECT                                                                
288500     .                                                                    
288600 IGB-FLYTTA-WDA5-POSTER    SECTION.                                       
288700                                                                          
288800     MOVE WS-IDDISTR-NUM             TO RAD-IDDISTR                       
288900     MOVE WS-IDKUNDNR-NUM            TO RAD-IDKUNDNR                      
289000     MOVE WS-IDKUNDRF                TO RAD-IDKUNDRF                      
289100     MOVE SPAR-ORAD-IDARTNR          TO RAD-IDARTNR                       
289200     MOVE +1                         TO RAD-IDLOPNR                       
289300     MOVE SPAR-ORAD-BERADREF         TO RAD-BERADREF                      
289400     MOVE 'N'                        TO RAD-FLERS                         
289500                                                                          
289600     MOVE WS-IDANSK                  TO RAD-IDANSK                        
289700     MOVE '00000     '               TO RAD-IDKUNDRF-LEV                  
289800     MOVE SPAR-ORAD-IDDC-RO          TO RAD-IDDC                          
289900                                        RAD-IDDC-RO                       
290000     MOVE SPAR-ORAD-IDKONTO          TO RAD-IDKONTO                       
290100     MOVE SPAR-ORAD-IDKST            TO RAD-IDKST                         
290200     MOVE SPAR-ORAD-IDANALYS         TO RAD-IDANALYS                      
290300     MOVE SPAR-ORAD-KDOI             TO RAD-KDOI                          
290400     MOVE SPAR-ORAD-CLEARGROUP       TO RAD-CLEARGROUP                    
290500     MOVE SPAR-ORAD-KDDSP            TO RAD-KDDSP                         
290600     MOVE WS-KDFAKTYP                TO RAD-KDFAKTYP                      
290700     MOVE SPAR-ORAD-KDFRAKT          TO RAD-KDFRAKT                       
290800     MOVE SPAR-ORAD-KDKVBRYT         TO RAD-KDKVBRYT                      
290900     MOVE SPAR-ORAD-KDORDING         TO RAD-KDORDING                      
291000     MOVE WS-KDORDKL                 TO RAD-KDORDKL                       
291100     MOVE SPAR-ORAD-KDPRODSL         TO RAD-KDPRODSL                      
291200     MOVE WS-KDRAPRIO                TO RAD-KDRAPRIO                      
291300     MOVE +4                         TO RAD-KDROO                         
291400     MOVE '2'                        TO RAD-KDSTARAD                      
291500     MOVE WS-KDTPOTYP                TO RAD-KDTPOTYP                      
291600     MOVE SPAR-ORAD-KDVRINFO         TO RAD-KDVRINFO                      
291700     IF DCS-CDC  OR  DCS-NDC                                              
291800       MOVE WS-KVORAPP-TOTAL         TO RAD-KVART                         
291900                                        RAD-KVRO                          
292000     ELSE                                                                 
292100       MOVE WS-KVORAPP-PACK          TO RAD-KVART                         
292200                                        RAD-KVRO                          
292300     END-IF                                                               
292400     MOVE SPAR-ORAD-PRARTNTO         TO RAD-PRARTNTO                      
292500     MOVE SPAR-ORAD-DEAL-PR-LINE     TO RAD-DEAL-PR-LINE                  
292600     IF SPAR-ORAD-KDVALISO-EXP NOT = SPACE                                
292700        MOVE SPAR-ORAD-KDVALISO-EXP  TO RAD-KDVALISO                      
292800     END-IF                                                               
292900     MOVE SPAR-ORAD-REKSIFFR         TO RAD-REKSIFFR                      
293000     MOVE ZERO                       TO RAD-TIAVBOKN                      
293100     MOVE DAT-TIAAMMDD               TO RAD-DARODAT                       
293200                                        RAD-TIREGDAT                      
293300     MOVE DAT-TISEKEL                TO RAD-DARODAT (1:2)                 
293400     MOVE ZERO                       TO RAD-TIRES                         
293500     MOVE +0                         TO RAD-TITPO                         
293600     MOVE JA                         TO RAD-FLTPOBEK                      
293700     MOVE SPAR-BEKUNDRF              TO RAD-BEKUNDRF                      
293800     MOVE SPAR-ORAD-BEVOLREF         TO RAD-BEVOLREF                      
293900     MOVE SPAR-ORAD-IDKAMPRF         TO RAD-IDKAMPRF                      
294000     MOVE SPAR-ORAD-IDLEVNR          TO RAD-IDLEVNR                       
294100     MOVE SPAR-ORAD-IDSYSTEM         TO RAD-IDSYSTEM                      
294200     MOVE SPAR-ORAD-KVBEART          TO RAD-KVBEART-Q                     
294300     MOVE WS-TTMMSS                  TO RAD-TIREGTID                      
294400     MOVE 0                          TO RAD-DASENDAT                      
294500     MOVE 0                          TO RAD-TISENBEK-KL                   
294600     MOVE SPAR-ORAD-KDPRTYP          TO RAD-KDPRTYP                       
294700     MOVE SPAR-ORAD-FLINVEST         TO RAD-FLINVEST                      
294800     MOVE SPAR-ORAD-FLPRTILL         TO RAD-FLPRTILL                      
294900                                                                          
295000     MOVE OHUV-KDORDTYP-LDC          TO RAD-KDORDTYP-LDC                  
295100     MOVE OHUV-TIREPDAT              TO RAD-TIREPDAT                      
295200     MOVE SPAR-ORAD-IDKUNDRF-WIP     TO RAD-IDKUNDRF-WIP                  
295300     MOVE SPAR-ORAD-PRAVCOST         TO RAD-PRAVCOST                      
295400     MOVE ARB-KDROPACK               TO RAD-KDROPACK                      
295500     MOVE SPAR-ORAD-IDARBREF         TO RAD-IDARBREF                      
295600     PERFORM S36-ANDRA-WDC711                                             
295700     EJECT                                                                
295800     .                                                                    
295900 IGC-SAMMANSL-BIPACKAD-RAD SECTION.                                       
296000                                                                          
296100     MOVE RAD-IDDISTR                TO W1-IDDISTR W2-IDDISTR             
296200     MOVE RAD-IDKUNDNR               TO W1-IDKUNDNR W2-IDKUNDNR           
296300     MOVE RAD-IDKUNDRF               TO W1-IDKUNDRF W2-IDKUNDRF           
296400     MOVE RAD-IDARTNR                TO W1-IDARTNR  W2-IDARTNR            
296500     MOVE +0                         TO W1-IDLOPNR                        
296600     MOVE +999                       TO W2-IDLOPNR                        
296700     MOVE '2'                        TO W-KDSTARAD                        
296800                                                                          
296900     PERFORM IMS-GHU-ORDP01-OLD                                           
297000                                                                          
297100     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
297200                                                                          
297300     PERFORM UNTIL NOT SEGMENT-FINNS   OR                                 
297400                       WS-SAMMANSLAGNING-RAD = JA                         
297500                                                                          
297600        IF  OLD-RAD-KDFRAKT  = RAD-KDFRAKT                                
297700          AND OLD-RAD-KDORDKL  = RAD-KDORDKL                              
297800          AND OLD-RAD-PRARTNTO = RAD-PRARTNTO                             
297900          AND OLD-RAD-DEAL-PR-LINE =  RAD-DEAL-PR-LINE                    
298000          AND OLD-RAD-KDTPOTYP = RAD-KDTPOTYP                             
298100          AND OLD-RAD-IDKONTO  = RAD-IDKONTO                              
298200          AND OLD-RAD-IDKST    = RAD-IDKST                                
298300                                                                          
298400           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
298500        ELSE                                                              
298600           PERFORM IMS-GHN-ORDP01-OLD                                     
298700        END-IF                                                            
298800     END-PERFORM                                                          
298900     .                                                                    
299000     EJECT                                                                
299100 IGD-SAMMANSL-EJ-BIPACKAD-RAD SECTION.                                    
299200                                                                          
299300     MOVE WS-IDDISTR-NUM             TO W1-IDDISTR  W2-IDDISTR            
299400     MOVE WS-IDKUNDNR-NUM            TO W1-IDKUNDNR W2-IDKUNDNR           
299500     MOVE WS-IDKUNDRF                TO W1-IDKUNDRF W2-IDKUNDRF           
299600     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
299700     MOVE +0                         TO W1-IDLOPNR                        
299800     MOVE +999                       TO W2-IDLOPNR                        
299900     MOVE '2'                        TO W-KDSTARAD                        
300000                                                                          
300100     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
300200                                                                          
300300     PERFORM IMS-GHU-ORDP01-OLD                                           
300400                                                                          
300500     PERFORM UNTIL NOT SEGMENT-FINNS      OR                              
300600                       WS-SAMMANSLAGNING-RAD = JA                         
300700                                                                          
300800        IF  OLD-RAD-KDFRAKT  = WS-KDFRAKT                                 
300900          AND OLD-RAD-KDORDKL  = WS-KDORDKL                               
301000          AND OLD-RAD-PRARTNTO = SPAR-ORAD-PRARTNTO                       
301100          AND OLD-RAD-DEAL-PR-LINE =                                      
301200              SPAR-ORAD-DEAL-PR-LINE                                      
301300          AND OLD-RAD-KDTPOTYP = WS-KDTPOTYP                              
301400                                                                          
301500           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
301600        ELSE                                                              
301700           PERFORM IMS-GHN-ORDP01-OLD                                     
301800        END-IF                                                            
301900     END-PERFORM                                                          
302000     .                                                                    
302100     EJECT                                                                
302200 IGE-HAMTA-TPOTYP-FRAN-ROREG             SECTION.                         
302300                                                                          
302400     MOVE WS-IDDISTR-NUM           TO W1-IDDISTR  W2-IDDISTR              
302500     MOVE WS-IDKUNDNR-NUM          TO W1-IDKUNDNR W2-IDKUNDNR             
302600                                                                          
302700     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
302800       MOVE WS-IDKUNDRF            TO W1-IDKUNDRF W2-IDKUNDRF             
302900     ELSE                                                                 
303000       MOVE SPAR-ORAD-IDKUNDRF-RO    TO W1-IDKUNDRF W2-IDKUNDRF           
303100     END-IF                                                               
303200                                                                          
303300     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
303400     MOVE SPAR-ORAD-IDLOPNR-RO       TO W1-IDLOPNR  W2-IDLOPNR            
303500     MOVE '4'                        TO W-KDSTARAD                        
303600                                                                          
303700     PERFORM IMS-GHU-ORDP01-OLD                                           
303800                                                                          
303900     IF SEGMENT-FINNS                                                     
304000       MOVE OLD-RAD-KDTPOTYP         TO WS-KDTPOTYP                       
304100     ELSE                                                                 
304200       MOVE 0                        TO WS-KDTPOTYP                       
304300     END-IF                                                               
304400     .                                                                    
304500     EJECT                                                                
304600 IH-LAS-ARTREG            SECTION.                                        
304700                                                                          
304800     MOVE SPAR-ORAD-IDARTNR    TO W-IDARTNR                               
304900                                                                          
305000     PERFORM IMS-GU-ARTC11                                                
305100     MOVE KORD-IDDC            TO W-711-IDDC                              
305200     PERFORM IMS-GU-WDK722                                                
305300     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
305400        MOVE XLAG-IDANSK       TO WS-IDANSK                               
305500     ELSE                                                                 
305600        MOVE CLAG-IDANSK       TO WS-IDANSK                               
305700     END-IF                                                               
305800     .                                                                    
305900     EJECT                                                                
306000 IJ-BESTAM-ORDERBEKR-KOD  SECTION.                                        
306100                                                                          
306200     MOVE ZERO                      TO WS-KDORDBEK                        
306300     MOVE WS-SAVE-IDDISTR           TO TEST-IDDISTR                       
306400*    IF DIST35-REFILL OR DIST35-REFILL-INOM-CN                            
306500*       MOVE 80                           TO WS-KDORDBEK                  
306600*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
306700*    ELSE                                                                 
306800       IF SPAR-ORAD-KDORDKL > 0                                           
306900                                                                          
307000          IF SPAR-ORAD-KVSLATT > 0                                        
307100                                                                          
307200             IF SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART -                  
307300                                     SPAR-ORAD-KVANNANT -                 
307400                                     SPAR-ORAD-KVSLATT                    
307500                                                                          
307600                PERFORM IJA-SATT-KOD80-90-91                              
307700             ELSE                                                         
307800                MOVE 81                     TO WS-KDORDBEK                
307900*           81 = INTE RESTNOTERING, GÖR NY BESTÄLLNING, SLATT.            
308000             END-IF                                                       
308100          ELSE                                                            
308200             PERFORM IJA-SATT-KOD80-90-91                                 
308300          END-IF                                                          
308400       ELSE                                                               
308500         CONTINUE                                                         
308600*DET KAN BLI RESTNOTERING FRÅN UTSKRIFT.                                  
308700         MOVE 93                          TO WS-KDORDBEK                  
308800*           93 = VOR, RESTNOTERAD KVANT, FYSISK AVVIKELSE                 
308900       END-IF                                                             
309000*    END-IF                                                               
309100     .                                                                    
309200     EJECT                                                                
309300 IJA-SATT-KOD80-90-91     SECTION.                                        
309400                                                                          
309500     IF SPAR-ORAD-FLRESTN = JA                                            
309600        IF SPAR-ORAD-IDKUNDRF-RO > '00000     '                           
309700           MOVE 91                        TO WS-KDORDBEK                  
309800*           91 = RESTNOTERAD IGEN.                                        
309900        ELSE                                                              
310000           MOVE 90                        TO WS-KDORDBEK                  
310100*           90 = RESTNOTERAD                                              
310200        END-IF                                                            
310300     ELSE                                                                 
310400        MOVE 80                           TO WS-KDORDBEK                  
310500*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
310600     END-IF                                                               
310700     .                                                                    
310800     EJECT                                                                
310900 IK-UPDATE-EV-KAMP-REG     SECTION.                                       
311000                                                                          
311100     IF SPAR-ORAD-IDKAMPRF     > 0 AND                                    
311200        (WS-KDORDBEK           = 80 OR 81)                                
311300       COMPUTE WS-KVSLATTAT  = SPAR-ORAD-KVBEART -                        
311400                               SPAR-ORAD-KVLEVART -                       
311500                               SPAR-ORAD-KVANNANT                         
311600       PERFORM IKA-UPDATE-WDM211                                          
311700       PERFORM IKB-UPDATE-WDM221                                          
311800     END-IF                                                               
311900     .                                                                    
312000     EJECT                                                                
312100 IKA-UPDATE-WDM211   SECTION.                                             
312200                                                                          
312300     MOVE SPAR-ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                     
312400     MOVE KORD-IDDC                TO W-KAMP-IDDC                         
312500     MOVE SPAR-ORAD-IDARTNR        TO W-KART-IDARTNR                      
312600     PERFORM IMS-GHU-WDM211                                               
312700     IF SEGMENT-FINNS                                                     
312800        MOVE KART-KVRESS-ART       TO SPAR-KART-KVRESS-ART                
312900                                                                          
313000        IF KART-KVBEART-KUND     >= WS-KVSLATTAT                          
313100            SUBTRACT WS-KVSLATTAT     FROM KART-KVBEART-KUND              
313200            COMPUTE KART-KVRESS-ART = KART-KVRESS-ART                     
313300                                    + WS-KVSLATTAT                        
313400         ELSE                                                             
313500            MOVE 'WDM211 KART-KVBEART-KUND BLIR NEGATIV'                  
313600                                   TO FELTEXT                             
313700            CALL ABEND USING RKOD-ABEND-MED-DUMP                          
313800        END-IF                                                            
313900        PERFORM IMS-REPL-WDM211                                           
314000     END-IF                                                               
314100     .                                                                    
314200     EJECT                                                                
314300 IKB-UPDATE-WDM221   SECTION.                                             
314400                                                                          
314500     MOVE SPAR-ORAD-IDKAMPRF   TO W-KAMP-IDKAMPRF                         
314600     MOVE KORD-IDDC            TO W-KAMP-IDDC                             
314700     MOVE SPAR-ORAD-IDARTNR    TO W-KART-IDARTNR                          
314800     MOVE WS-IDDISTR-NUM       TO W-KMRK-IDDISTR-FOM                      
314900     MOVE WS-IDDISTR-NUM       TO W-KMRK-IDDISTR-TOM                      
315000     MOVE WS-IDKUNDNR-NUM      TO W-KMRK-IDKUNDNR-FOM                     
315100     MOVE WS-IDKUNDNR-NUM      TO W-KMRK-IDKUNDNR-TOM                     
315200                                                                          
315300     PERFORM S20-FINN-INTERVALL                                           
315400                                                                          
315500     PERFORM IMS-GHU-WDM221                                               
315600                                                                          
315700     IF SEGMENT-FINNS                                                     
315800         IF KMRK-KVBEART-KUND >= WS-KVSLATTAT                             
315900             SUBTRACT WS-KVSLATTAT                                        
316000                                FROM KMRK-KVBEART-KUND                    
316100          ELSE                                                            
316200             MOVE 'WDM221 KMRK-KVBEART-KUND BLIR NEGATIV'                 
316300                                TO FELTEXT                                
316400             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
316500         END-IF                                                           
316600         PERFORM IMS-REPL-WDM221                                          
316700     END-IF                                                               
316800     .                                                                    
316900     EJECT                                                                
317000 J-UPDATE-KUNDORDER SECTION.                                              
317100                                                                          
317200     PERFORM IMS-GHU-KUNDORDER                                            
317300                                                                          
317400     MOVE ZERO                   TO KORD-KDPAKOLL                         
317500                                                                          
317600     COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD    +                      
317700                                  KORD-KVORDRAD-LEVPL                     
317800     END-COMPUTE                                                          
317900                                                                          
318000     PERFORM IMS-REPL-WDE401                                              
318100     .                                                                    
318200     EJECT                                                                
318300                                                                          
318400 L-UPDATE-ORDERKO SECTION.                                                
318500     MOVE KORD-IDDISTR               TO TEST-IDDISTR                      
318600                                                                          
318700     IF NOT DIST19-SATS                                                   
318800       PERFORM LA-UPDATE-ORQA                                             
318900       PERFORM LB-UPDATE-ORQI                                             
319000     END-IF                                                               
319100     .                                                                    
319200     EJECT                                                                
319300 LA-UPDATE-ORQA                          SECTION.                         
319400                                                                          
319500     MOVE KORD-IDORDER                   TO W-WDQ301-IDORDER              
319600     MOVE KORD-IDDC                      TO W-WDQ301-IDDC                 
319700     MOVE KORD-IDPRODNR                  TO W-WDQ301-IDPRODNR             
319800     MOVE KORD-IDPLKLST                  TO W-WDQ301-IDPLKLST             
319900     PERFORM IMS-GHU-ORQA01                                               
320000                                                                          
320100     MOVE KORD-KVORDRAD-PACK             TO ODEL-KVPACKRAD-OD             
320200     MOVE 'P'                            TO ODEL-KDODELSTA                
320300     MOVE DAT-TIAAMMDD                   TO ODEL-TIPACKN                  
320400     MOVE WS-TTMMSS                      TO ODEL-TIPACTID                 
320500                                                                          
320600     MOVE ODEL-IDDC                      TO W-4447-IDDC                   
320700                                            W-4487-IDDC                   
320800     MOVE ODEL-IDPRC                     TO W-4448-IDPRC                  
320900     MOVE ODEL-DARFS                     TO W-4490-DARFS                  
321000     MOVE ODEL-IDPRODNR                  TO W-4490-IDPRODNR               
321100     MOVE ODEL-IDPLKLST                  TO W-4490-IDPLKLST               
321200                                                                          
321300     PERFORM IMS-REPL-ORQA01                                              
321400                                                                          
321500     PERFORM S06-BORTTAG-PRODTAB                                          
321600     .                                                                    
321700     EJECT                                                                
321800 LB-UPDATE-ORQI                          SECTION.                         
321900                                                                          
322000     MOVE NEJ                     TO KDORDSTA-SW                          
322100                                                                          
322200     MOVE KORD-IDORDER            TO W-Q301KY-MIN-IDORDER                 
322300                                     W-Q301KY-MAX-IDORDER                 
322400     MOVE W-IDDC                  TO W-Q301KY-MIN-IDDC                    
322500                                     W-Q301KY-MAX-IDDC                    
322600                                                                          
322700     MOVE 'R'                     TO W-KDODELST                           
322800     PERFORM IMS-GU-ORQA-STATUS                                           
322900     IF SEGMENT-FINNS                                                     
323000        MOVE 'R*'                 TO WS-KDORDSTA                          
323100        MOVE JA                   TO KDORDSTA-SW                          
323200     ELSE                                                                 
323300                                                                          
323400        MOVE 'U'                  TO W-KDODELST                           
323500        PERFORM IMS-GU-ORQA-STATUS                                        
323600        IF SEGMENT-FINNS                                                  
323700           MOVE 'U*'              TO WS-KDORDSTA                          
323800           MOVE JA                TO KDORDSTA-SW                          
323900        ELSE                                                              
324000           PERFORM LBA-KOLLA-KVKOLLI                                      
324100           IF KDORDSTA-KLAR                                               
324200               CONTINUE                                                   
324300           ELSE                                                           
324400               PERFORM LBB-TA-FRAM-KDORDSTA                               
324500           END-IF                                                         
324600        END-IF                                                            
324700     END-IF                                                               
324800                                                                          
324900     MOVE W-IDDC      TO W-IDDC-Q2                                        
325000     PERFORM IMS-GHNP-ORQI12                                              
325100     MOVE WS-KDORDSTA TO ARB-KDORDSTA                                     
325200     PERFORM IMS-REPL-ORQI12                                              
325300     .                                                                    
325400     EJECT                                                                
325500 LBA-KOLLA-KVKOLLI SECTION.                                               
325600                                                                          
325700     MOVE ZERO        TO SPAR-IDPRODNR                                    
325800     MOVE WS-IDPRODNR TO W-601-IDPRODNR                                   
325900     PERFORM IMS-GN-WDE401-ESEQ                                           
326000                                                                          
326100     MOVE KORD-IDGMTREF      TO W-E4ASEQ-IDGMTREF                         
326200     PERFORM IMS-GU-WDE401-SEQ                                            
326300                                                                          
326400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
326500                   KDORDSTA-KLAR                                          
326600                                                                          
326700        IF KORD-IDPRODNR              = SPAR-IDPRODNR OR                  
326800           KORD-IDDC                  NOT = W-SPAR-IDDC                   
326900            CONTINUE                                                      
327000        ELSE                                                              
327100            MOVE KORD-IDPRODNR        TO W-601-IDPRODNR                   
327200            PERFORM IMS-GHU-WDE601                                        
327300                                                                          
327400            IF (VORD-KVKOLLI-LAST     >  0    OR                          
327500               VORD-KVKOLLI-FAKT      >  0    OR                          
327600               VORD-KVKOLLI-FL        >  0)   OR                          
327700               VORD-KDORDSTA          =  5                                
327800                                                                          
327900               COMPUTE W-KVKOLLI      =  W-KVKOLLI + VORD-KVKOLLI         
328000               COMPUTE W-KVKOLLI-FAKT =                                   
328100                              W-KVKOLLI-FAKT + VORD-KVKOLLI-FAKT          
328200               COMPUTE W-KVKOLLI-LAST =                                   
328300                              W-KVKOLLI-LAST + VORD-KVKOLLI-LAST          
328400               COMPUTE W-KVKOLLI-FL   =                                   
328500                              W-KVKOLLI-FL   + VORD-KVKOLLI-FL            
328600            ELSE                                                          
328700               MOVE 'P'               TO WS-KDORDSTA                      
328800               MOVE JA                TO KDORDSTA-SW                      
328900            END-IF                                                        
329000                                                                          
329100            MOVE KORD-IDPRODNR        TO SPAR-IDPRODNR                    
329200        END-IF                                                            
329300        PERFORM IMS-GN-WDE401-SEQ                                         
329400     END-PERFORM                                                          
329500     .                                                                    
329600     EJECT                                                                
329700 LBB-TA-FRAM-KDORDSTA        SECTION.                                     
329800                                                                          
329900     IF VORD-KDORDSTA          <  4                                       
330000       MOVE 'P*'              TO WS-KDORDSTA                              
330100     ELSE                                                                 
330200       IF W-KVKOLLI-FL            = W-KVKOLLI                             
330300         IF W-KVKOLLI-FAKT = ZERO AND                                     
330400            W-KVKOLLI-LAST = ZERO                                         
330500           MOVE 'S'          TO WS-KDORDSTA                               
330600         ELSE                                                             
330700           IF W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                     
330800              W-KVKOLLI-LAST     NOT = W-KVKOLLI                          
330900             MOVE 'S*'       TO WS-KDORDSTA                               
331000           ELSE                                                           
331100             MOVE 'SF'       TO WS-KDORDSTA                               
331200           END-IF                                                         
331300         END-IF                                                           
331400       END-IF                                                             
331500     END-IF                                                               
331600     .                                                                    
331700     EJECT                                                                
331800 K-UPDATE-KOLLIREG         SECTION.                                       
331900                                                                          
332000     MOVE WS-IDPRODNR                TO  W-601-IDPRODNR                   
332100     PERFORM IMS-GHU-WDE601                                               
332200                                                                          
332300     IF SEGMENT-FINNS                                                     
332400         MOVE VORD-IDDISTR           TO  TEST-IDDISTR                     
332500         MOVE VORD-KVKOLPAC          TO  SPAR-KVKOLPAC                    
332600         MOVE VORD-KVKOLLI-FAKT      TO  SPAR-KVKOLLI-FAKT                
332700         PERFORM S07-UPPD-VORD-FRAN-SPAR                                  
332800                                                                          
332900         IF ORAPPORTERADE-RADER-SAKNAS                                    
333000             MOVE VORD-IDDC          TO W-IDDC                            
333100             MOVE VORD-KDFAKTYP      TO WS-KDFAKTYP                       
333200             IF VORD-KVORDRAD-PACK   =   VORD-KVORDRAD                    
333300                 IF VORD-KVKOLLI     =   VORD-KVKOLPAC                    
333400                                                                          
333500                   PERFORM KA-KONTR-OM-ODEL-EJ-UTSKRIVEN                  
333600                   IF  WS-ALLA-ODEL-UTSKRIVNA  = JA                       
333700*****************************************************************         
333800*** KOMMENTAR AV SVANTE B. 920219                             ***         
333900*****************************************************************         
334000*** EFTER DENNA KOMMENTAR GÖRS FYRA UPPDATERINGAR:            ***         
334100***                                                           ***         
334200*** A) VORD-TIPACKN-SK SÄTTS                                  ***         
334300*** B) "S19-GENERERA-KLAR-SV4" SKAPAR EN "RY6"-TRANS.         ***         
334400*** C) VORD-KDORDSTA SÄTTS                                    ***         
334500*** D) "S04-PACK-ORDER-LISTA" STARTAR 4342                    ***         
334600***                                                           ***         
334700*** OM MAN TAR BORT EN ORDERDEL I ORDER-ENTRY FÖR EN ORDER    ***         
334800*** DÄR RESTERANDE ORDERDELAR REDAN ÄR PACKADE FÅR MAN ETT    ***         
334900*** LÄGE DÄR ORDERN GÅR FRÅN OPACKAD TILL PACKAD. MAN MÅSTE   ***         
335000*** DÅ I W413AVSO UTFÖRA PUNKT A-D.                           ***         
335100***                                                           ***         
335200*** OM MAN I 4397 ELLER 4398 ÄNDRAR DESSA UPPDATERINGAR ELLER ***         
335300*** LÄGGER TILL NYA MÅSTE MAN DÄRFÖR ÄVEN GÖRA DETTA I        ***         
335400*** W413AVSO.                                                 ***         
335500*****************************************************************         
335600                                                                          
335700                     IF VORD-KVKOLPAC = 0                                 
335800                         MOVE DAT-TIAAMMDD  TO VORD-TIPACKN-SK            
335900                     END-IF                                               
336000                                                                          
336100                     IF  VORD-KDORDSTA < +3                               
336200                     AND VORD-KVKOLLI  >  +0                              
336300                     AND DIST03-SVERIGE-2                                 
336400                         PERFORM S19-GENERERA-KLAR-SV4                    
336500                     END-IF                                               
336600                                                                          
336700                     MOVE +3         TO  VORD-KDORDSTA                    
336800                                                                          
336900                     IF VORD-FLAUTFAK =  JA     AND                       
337000                        VORD-KVKOLLI  >  ZERO                             
337100                         MOVE JA     TO  SW-FLAUTFAK                      
337200                     END-IF                                               
337300                                                                          
337400                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FL                  
337500                         MOVE +4     TO  VORD-KDORDSTA                    
337600                     END-IF                                               
337700                                                                          
337800                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FAKT                
337900                     AND VORD-KVKOLLI =  VORD-KVKOLLI-LAST                
338000                         MOVE +5     TO  VORD-KDORDSTA                    
338100                     END-IF                                               
338200                                                                          
338300                     IF W-IDDC NOT = W-IDDC-B6                            
338400                        MOVE W-IDDC TO W-IDDC-B6                          
338500                        PERFORM IMS-GU-WDB601                             
338600                     END-IF                                               
338700                     IF DCS-NDC-NA                                        
338800                       PERFORM KB-SKRIV-DEL-NOTE                          
338900                     END-IF                                               
339000                   END-IF                                                 
339100                 END-IF                                                   
339200             END-IF                                                       
339300         END-IF                                                           
339400                                                                          
339500         PERFORM IMS-REPL-KOLLIREG                                        
339600                                                                          
339700     ELSE                                                                 
339800         MOVE FEL                       TO  WS-BEHANDLING-TEST            
339900     END-IF                                                               
340000     .                                                                    
340100 KA-KONTR-OM-ODEL-EJ-UTSKRIVEN      SECTION.                              
340200                                                                          
340300     MOVE JA                 TO WS-ALLA-ODEL-UTSKRIVNA                    
340400                                                                          
340500     MOVE W-IDDC             TO W-Q301-MIN-IDDC                           
340600                                W-Q301-MAX-IDDC                           
340700     MOVE WS-IDPRODNR        TO W-Q301-MIN-IDPRODNR                       
340800                                W-Q301-MAX-IDPRODNR                       
340900     MOVE WS-IDORDER         TO W-Q301-MIN-IDORDER                        
341000                                W-Q301-MAX-IDORDER                        
341100                                                                          
341200     PERFORM IMS-GU-ORQA-ODEL                                             
341300                                                                          
341400     PERFORM UNTIL ((NOT SEGMENT-FINNS)                                   
341500             OR   WS-ALLA-ODEL-UTSKRIVNA  = NEJ)                          
341600                                                                          
341700       IF  ODEL-KDODELSTA = 'R'                                           
341800         MOVE NEJ        TO WS-ALLA-ODEL-UTSKRIVNA                        
341900       ELSE                                                               
342000         PERFORM IMS-GN-ORQA-ODEL                                         
342100       END-IF                                                             
342200     END-PERFORM                                                          
342300     .                                                                    
342400     EJECT                                                                
342500                                                                          
342600                                                                          
342700 KB-SKRIV-DEL-NOTE SECTION.                                               
342800                                                                          
342900     MOVE WS-SAVE-IDDISTR             TO TEST-IDDISTR                     
343000     IF DIST07-USA-PRINT-DNOTE                                            
343100     OR DIST07-CAN-PRINT-DNOTE                                            
343200*       TRANS TILL 4349 FÖR ATT STARTA UTSKRIFT                           
343300        MOVE WS-SAVE-IDDISTR     TO 4349-MID-IDDISTR                      
343400        MOVE WS-SAVE-IDKUNDNR    TO 4349-MID-IDKUNDNR                     
343500        MOVE AVSP-IDORDNR        TO WS-IDORDNR7-NEW                       
343600        MOVE WS-IDORDNR7-NEW     TO 4349-MID-IDORDNR7                     
343700        MOVE AVSP-IDPRODNR       TO 4349-MID-IDPRODNR                     
343800        MOVE AVSP-IDDC           TO 4349-MID-IDDC                         
343900        MOVE AVSP-IDORDER        TO 4349-MID-IDORDER                      
344000        MOVE WS-KDMFSFOR         TO 4349-SPRAK                            
344100        MOVE 4349-MID-W4I34901   TO 4349-FILLER                           
344200                                                                          
344300        PERFORM IMS-INSERT-TRANS4349                                      
344400     END-IF                                                               
344500                                                                          
344600     MOVE WS-SAVE-IDDISTR             TO TEST-IDDISTR                     
344700     IF DIST07-USA-RETAILER-DNOTE                                         
344800     OR DIST07-CAN-RETAILER                                               
344900                                                                          
345000        INITIALIZE DNOT-ORDER-INFO                                        
345100                                                                          
345200*       UPPDATERING AV UTSKRIFTSDAG, WDQ5                                 
345300        MOVE 'W403AVSX'               TO DNOT-IDPGM                       
345400        MOVE KORD-IDORDER             TO DNOT-IDORDER                     
345500        MOVE KORD-IDDC                TO DNOT-IDDC                        
345600        CALL W411DNOT USING DNOT-W411DNOT                                 
345700                            DNOT-ORQP-PCB                                 
345800                            DNOT-ORQP2-PCB                                
345900                            DNOT-ORQP3-PCB                                
346000                            DNOT-4013-PCB                                 
346100                            DNOT-BENA-PCB                                 
346200                                                                          
346300     END-IF                                                               
346400     .                                                                    
346500     EJECT                                                                
346600                                                                          
346700                                                                          
346800 M-AVSLUT             SECTION.                                            
346900                                                                          
347000     IF WS-BEHANDLING-RATT                                                
347100                                                                          
347200         MOVE WS-IDDISTR-NUM          TO TEST-IDDISTR                     
347300         IF AUT-FAKTURA-SKRIVS-UT    AND                                  
347400            SPAR-KVKOLPAC > SPAR-KVKOLLI-FAKT                             
347500             PERFORM S05-AUTOMATFAKTURERING                               
347600                                                                          
347700             IF SEGMENT-SAKNAS                                            
347800                 MOVE SPACE           TO  IO-AREA3                        
347900                 MOVE WS-IDDISTR-NUM  TO  AUTFAKT-IDDISTR                 
348000                 MOVE WS-IDKUNDNR-NUM TO  AUTFAKT-IDKUNDNR                
348100                 MOVE W-IDDC          TO  AUTFAKT-IDDC                    
348200                 MOVE WS-KDFAKTYP     TO  AUTFAKT-KDFAKTYP                
348300                 PERFORM IMS-ISRT-AUTFAKTURA-ROT                          
348400                 MOVE SPACE           TO  IO-AREA3                        
348500                 PERFORM S05-AUTOMATFAKTURERING                           
348600             END-IF                                                       
348700         END-IF                                                           
348800         MOVE SPACE                   TO AVSP-KDSVAR                      
348900     ELSE                                                                 
349000         MOVE 'FEL FR. M-SEC'         TO WS-PGM-POSITION                  
349100         MOVE FEL                     TO AVSP-KDSVAR                      
349200     END-IF                                                               
349300                                                                          
349400     PERFORM MA-CLEAN-FG-TABEL                                            
349500     .                                                                    
349600     EJECT                                                                
349700                                                                          
349800 MA-CLEAN-FG-TABEL  SECTION.                                              
349900                                                                          
350000     MOVE +1 TO FG-INDX                                                   
350100     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
350200       MOVE ZERO                    TO TAB-IDPSN(FG-INDX)                 
350300                                       TAB-VKART-FG(FG-INDX)              
350400                                       TAB-VLFG(FG-INDX)                  
350500       ADD +1 TO FG-INDX                                                  
350600     END-PERFORM                                                          
350700                                                                          
350800     .                                                                    
350900     EJECT                                                                
351000                                                                          
351100 S01-UPPD-SPAR-UPPGIFTER SECTION.                                         
351200                                                                          
351300     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
351400                         ORAD-VKARTNTO * WS-KVORAPP-PACK                  
351500*                                                                         
351600     COMPUTE SPAR-VLORDNTO = SPAR-VLORDNTO +                              
351700                         ORAD-VLARTNTO * WS-KVORAPP-PACK                  
351800*                                                                         
351900     MOVE WS-IDDISTR-NUM    TO TEST-IDDISTR                               
352000     IF DIST79-DEALER-PRICE                                               
352100      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
352200       COMPUTE SPAR-SUORDV-LOCPREL ROUNDED = SPAR-SUORDV-LOCPREL +        
352300                 ORAD-PRARTNTO-LOCPREL * WS-KVORAPP-PACK                  
352400      ELSE                                                                
352500       COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +                
352600                 ORAD-PRARTNTO-LOC * WS-KVORAPP-PACK                      
352700      END-IF                                                              
352800     ELSE                                                                 
352900       IF DIST79-ECOM-PRICE                                               
353000                                                                          
353100         COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +              
353200                 ORAD-PRARTNTO-LOC * WS-KVORAPP-PACK                      
353300       ELSE                                                               
353400         COMPUTE SPAR-SUORDV ROUNDED = SPAR-SUORDV +                      
353500                        ORAD-PRARTNTO * WS-KVORAPP-PACK                   
353600         COMPUTE SPAR-SUORDV-EXP ROUNDED = SPAR-SUORDV +                  
353700                        ORAD-PRAVCOST * WS-KVORAPP-PACK                   
353800       END-IF                                                             
353900     END-IF                                                               
354000     MOVE ORAD-KDVALISO       TO SPAR-KDVALISO                            
354100     MOVE ORAD-KDVALISO-EXP   TO SPAR-KDVALISO-EXP                        
354200                                                                          
354300*                                                                         
354400     COMPUTE SPAR-SUORDV-DEL   = SPAR-SUORDV-DEL + SPAR-SUORDV            
354500     END-COMPUTE                                                          
354600     COMPUTE SPAR-SUORDV-DEL-LOC  = SPAR-SUORDV-DEL-LOC                   
354700                                  + SPAR-SUORDV-LOC                       
354800     END-COMPUTE                                                          
354900     COMPUTE SPAR-SUORDV-DEL-LOCPREL   = SPAR-SUORDV-DEL-LOCPREL          
355000                               + SPAR-SUORDV-LOCPREL                      
355100     END-COMPUTE                                                          
355200     COMPUTE SPAR-VKORDNTO-DEL = SPAR-VKORDNTO-DEL + SPAR-VKORDNTO        
355300     END-COMPUTE                                                          
355400     COMPUTE SPAR-VLORDNTO-DEL = SPAR-VLORDNTO-DEL + SPAR-VLORDNTO        
355500     END-COMPUTE                                                          
355600     COMPUTE SPAR-SUORDV-TOT   = SPAR-SUORDV-TOT + SPAR-SUORDV            
355700     END-COMPUTE                                                          
355800     COMPUTE SPAR-SUORDV-TOT-LOC = SPAR-SUORDV-TOT-LOC                    
355900                                 + SPAR-SUORDV-LOC                        
356000     END-COMPUTE                                                          
356100     COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL            
356200                                 + SPAR-SUORDV-LOCPREL                    
356300     END-COMPUTE                                                          
356400     COMPUTE SPAR-VKORDNTO-TOT ROUNDED =                                  
356500             SPAR-VKORDNTO-TOT + SPAR-VKORDNTO                            
356600     END-COMPUTE                                                          
356700     COMPUTE SPAR-VLORDNTO-TOT = SPAR-VLORDNTO-TOT + SPAR-VLORDNTO        
356800     END-COMPUTE                                                          
356900     EJECT                                                                
357000     .                                                                    
357100 S04-PACK-ORDER-LISTA   SECTION.                                          
357200                                                                          
357300     MOVE LOW-VALUE                TO  4342-MID                           
357400     MOVE '+++++++'                TO  4342-MID-IDPRODNR-IN               
357500     MOVE WS-IDPRODNR              TO  WS-IDPRODNR-RED                    
357600     MOVE WS-IDPRODNR-RED          TO  4342-MID-IDPRODNR-UT               
357700     MOVE 'W4T342U '               TO  MSG-KDTRANS-1                      
357800     MOVE '439G'                   TO  MSG-IDTRANS-1                      
357900     MOVE WS-KDMFSFOR              TO  MSG-KDMFSFOR-1                     
358000     MOVE +31                      TO  MSG-KVLL                           
358100     MOVE 4342-MID                 TO  MSG-INDATA-MINUS-1-TRANSKOD        
358200     PERFORM IMS-PURG-4342                                                
358300     EJECT                                                                
358400     .                                                                    
358500 S05-AUTOMATFAKTURERING      SECTION.                                     
358600                                                                          
358700     MOVE WS-IDDISTR-NUM                  TO  W-RDG-IDDISTR               
358800     MOVE WS-IDKUNDNR-NUM                 TO  W-RDG-IDKUNDNR              
358900     MOVE W-IDDC                          TO  W-RDG-IDDC                  
359000     MOVE WS-KDFAKTYP                     TO  W-RDG-KDFAKTYP              
359100     SKIP2                                                                
359200     MOVE WS-IDPRODNR                     TO  AUTFAKT-IDPRODNR            
359300     MOVE ZERO                            TO  AUTFAKT-PRFRAKT             
359400                                              AUTFAKT-IDSKEPPN            
359500     IF DIST19-SATS                                                       
359600        MOVE NEJ                          TO  W-4726-FLBATCH              
359700     ELSE                                                                 
359800                                                                          
359900*       IF DIST03-SVERIGE                                                 
360000*                                                                         
360100*          IF W-IDDC NOT = W-IDDC-B6                                      
360200*             MOVE W-IDDC TO W-IDDC-B6                                    
360300*             PERFORM IMS-GU-WDB601                                       
360400*          END-IF                                                         
360500*          IF  DCS-CDC                                                    
360600*          OR (DCS-SDC AND DCS-SWEDEN                                     
360700*          OR DIST18-SKROT                                                
360800*             MOVE JA                     TO  W-4726-FLBATCH              
360900*          ELSE                                                           
361000*             MOVE NEJ                    TO  W-4726-FLBATCH              
361100*          END-IF                                                         
361200*       ELSE                                                              
361300           MOVE NEJ                       TO  W-4726-FLBATCH              
361400*       END-IF                                                            
361500     END-IF                                                               
361600                                                                          
361700     IF DIST03-SVERIGE                                                    
361800         MOVE NEJ                         TO  AUTFAKT-FLLASTA             
361900     ELSE                                                                 
362000         MOVE JA                          TO  AUTFAKT-FLLASTA             
362100     END-IF                                                               
362200     MOVE '4726'                          TO W-4726-IDHTYP                
362300     MOVE LOW-VALUE                       TO W-4726-LOWVALUE              
362400     PERFORM IMS-ISRT-AUTFAKTURA                                          
362500     EJECT                                                                
362600     .                                                                    
362700 S06-BORTTAG-PRODTAB            SECTION.                                  
362800                                                                          
362900     PERFORM IMS-GU-XXKH11                                                
363000     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
363100                                                                          
363200     PERFORM IMS-GHU-WDGX4490                                             
363300     IF SEGMENT-FINNS                                                     
363400        PERFORM IMS-DLET-WDGX4490                                         
363500     END-IF                                                               
363600     .                                                                    
363700     SKIP2                                                                
363800 S07-UPPD-VORD-FRAN-SPAR   SECTION.                                       
363900     SKIP3                                                                
364000     SUBTRACT SPAR-VKORDNTO FROM VORD-VKORDNTO                            
364100     COMPUTE VORD-VLORDNTO = VORD-VLORDNTO - SPAR-VLORDNTO                
364200                               / 1000000                                  
364300     END-COMPUTE                                                          
364400     SUBTRACT SPAR-SUORDV-LOC  FROM VORD-SUORDV-LOC                       
364500     SUBTRACT SPAR-SUORDV-LOCPREL  FROM VORD-SUORDV-LOCPREL               
364600     SUBTRACT SPAR-SUORDV      FROM VORD-SUORDV                           
364700     SUBTRACT SPAR-SUORDV-EXP  FROM VORD-SUORDV-EXP                       
364800     .                                                                    
364900     SKIP2                                                                
365000 S08-HAMTA-MASKINDATUM     SECTION.                                       
365100     SKIP3                                                                
365200     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
365300     CALL WDATKONV USING DAT-KDDATFORM                                    
365400                         DAT-I-TIDATUM                                    
365500                         DAT-O-TIDATUM                                    
365600                         DAT-KDSVAR                                       
365700     EJECT                                                                
365800     .                                                                    
365900 S09-INIT-KOLLI-VALUE      SECTION.                                       
366000     SKIP3                                                                
366100     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
366200     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
366300     MOVE WS-IDDISTR-NUM    TO  KOLLI-IDDISTR                             
366400     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
366500     MOVE WS-ADFLOMR        TO  KOLLI-ADFLOMR                             
366600     MOVE WS-ADRUTNIV       TO  KOLLI-ADRUTNIV                            
366700     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
366800     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
366900     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
367000     MOVE WS-KDEMBTYP       TO  KOLLI-KDEMBTYP                            
367100     MOVE WS-VKORDBTO       TO  KOLLI-VKORDBTO-KOLLI                      
367200     MOVE W-IDDC            TO  KOLLI-IDDC                                
367300     MOVE WS-ADFLGEO        TO  KOLLI-ADFLGEO                             
367400     MOVE WS-KDKOLLI        TO  KOLLI-KDKOLLI                             
367500     MOVE WS-IDKOLLI-SAMP   TO  KOLLI-IDKOLLI-SAMP                        
367600     MOVE NEJ               TO  KOLLI-FLBANDST                            
367700                                KOLLI-FLFRSUTS                            
367800     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
367900                                KOLLI-IDTRPTNR                            
368000                                KOLLI-ADVMODUL                            
368100                                KOLLI-ADHMODUL                            
368200                                KOLLI-IDFAKLOP                            
368300                                KOLLI-IDFAKT                              
368400                                KOLLI-IDFAKT-EXP                          
368500                                KOLLI-DIDMODUL                            
368600                                KOLLI-DIHMODUL                            
368700                                KOLLI-KVFALRAD                            
368800                                KOLLI-KDKOLSTA                            
368900                                KOLLI-KDORDKL                             
369000                                KOLLI-TIFAKT                              
369100                                KOLLI-TIFAKT-EXP                          
369200                                KOLLI-TIFAKTID                            
369300                                KOLLI-TIFAKTID-EXP                        
369400                                KOLLI-TILASTN                             
369500                                KOLLI-TILASTID                            
369600                                KOLLI-TIPACKN                             
369700                                KOLLI-TIPACTID                            
369800                                KOLLI-TIPACTID                            
369900                                KOLLI-VKORDNTO-KOLLI                      
370000                                KOLLI-SUORDV-KOLLI                        
370100                                KOLLI-SUORDV-KLI-EXP                      
370200                                KOLLI-SUORDV-LOC                          
370300                                KOLLI-SUORDV-LOCPREL                      
370400                                KOLLI-KVORDRAD                            
370500                                KOLLI-TIAAVVD-PATR                        
370600                                KOLLI-KDFARLIG-KOLLI                      
370700                                KOLLI-KVFLAMP-KOLLI                       
370800                                KOLLI-IDLASTN                             
370900                                KOLLI-IDSHIPM                             
371000* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
371100* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
371200                                KOLLI-DASUPREF                            
371300                                KOLLI-TISUPTID                            
371400                                KOLLI-KDVIA                               
371500                                KOLLI-IDTULLNR                            
371600                                KOLLI-RETULKS                             
371700     MOVE SPACE             TO  KOLLI-IDLEVNR                             
371800                                KOLLI-KDVALISO                            
371900                                KOLLI-KDVALISO-EXP                        
372000                                KOLLI-KDSTASKLI                           
372100                                KOLLI-FILLERX2                            
372200*                                                                         
372300     MOVE +1 TO FG-INDX                                                   
372400     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
372500       MOVE ZERO            TO KOLLI-IDPSN(FG-INDX)                       
372600                               KOLLI-VKART-FG(FG-INDX)                    
372700                               KOLLI-VLFG(FG-INDX)                        
372800       ADD +1 TO FG-INDX                                                  
372900     END-PERFORM                                                          
373000     MOVE ZERO              TO KOLLI-SUEQFG                               
373100                               KOLLI-DARFS                                
373200*                                                                         
373300     MOVE SPACE             TO  KOLLI-FLUTLAST                            
373400                                KOLLI-FLAUTFAK                            
373500                                KOLLI-IDSUPREF                            
373600                                KOLLI-FLTULLG                             
373700                                KOLLI-IDLBBET                             
373800                                KOLLI-KDARTURS-KOLLI                      
373900                                KOLLI-IDTULFTG                            
374000     COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                               
374100       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
374200*--------------------------------------- KOLLI BREDD, HÖJD OCH            
374300*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
374400*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
374500     MOVE KOLLI-VLORDBTO-KOLLI TO WS-VLORDBTO                             
374600     .                                                                    
374700     EJECT                                                                
374800 S10-UPPD-SPAR-KOLLI    SECTION.                                          
374900                                                                          
375000     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
375100                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
375200*                                                                         
375300     COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                        
375400           SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                        
375500     COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +                
375600           SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                        
375700     COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC +                
375800       SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                        
375900     COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL +        
376000       SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART                    
376100     MOVE SPAR-KDVALISO          TO ARB-KOLLI-KDVALISO                    
376200     MOVE SPAR-KDVALISO-EXP      TO ARB-KOLLI-KDVALISO-EXP                
376300*                                                                         
376400     IF      SPAR-PRAD-KVFLAMP   >  ZERO                                  
376500        AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                     
376600        OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                 
376700         MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                     
376800     END-IF                                                               
376900                                                                          
377000     IF      SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                  
377100     AND     SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                    
377200       MOVE  SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                     
377300     END-IF                                                               
377400*                                                                         
377500     .                                                                    
377600     EJECT                                                                
377700 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
377800                                                                          
377900     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
378000     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
378100     ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
378200     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
378300     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
378400     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
378500     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
378600                                                                          
378700     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
378800     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
378900*                                                                         
379000     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
379100        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
379200        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
379300         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
379400     END-IF                                                               
379500*                                                                         
379600     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
379700         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
379800     END-IF                                                               
379900     SKIP2                                                                
380000     MOVE WS-KDKOLLI             TO KOLLI-KDKOLLI                         
380100     MOVE WS-KDORDKL             TO KOLLI-KDORDKL                         
380200     MOVE WS-FLAUTFAK            TO KOLLI-FLAUTFAK                        
380300     MOVE WS-DIKOLLIL            TO KOLLI-DIKOLLIL                        
380400     MOVE WS-DIKOLLIH            TO KOLLI-DIKOLLIH                        
380500     MOVE WS-DIKOLLIB            TO KOLLI-DIKOLLIB                        
380600     MOVE WS-KDEMBTYP            TO KOLLI-KDEMBTYP                        
380700     MOVE WS-VKORDBTO            TO KOLLI-VKORDBTO-KOLLI                  
380800*WEIGHT                                                                   
380900     IF   KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                     
381000          MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI               
381100     END-IF                                                               
381200*                                                                         
381300     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
381400       ADD 0.1                TO KOLLI-VKORDBTO-KOLLI                     
381500     END-IF                                                               
381600*                                                                         
381700     MOVE WS-IDTRPTNR               TO KOLLI-IDTRPTNR                     
381800     MOVE WS-ADFLGEO                TO KOLLI-ADFLGEO                      
381900     MOVE WS-ADFLOMR                TO KOLLI-ADFLOMR                      
382000     MOVE WS-ADRUTNIV               TO KOLLI-ADRUTNIV                     
382100     MOVE WS-DIHMODUL               TO KOLLI-DIHMODUL                     
382200     MOVE WS-DIDMODUL               TO KOLLI-DIDMODUL                     
382300     MOVE WS-ADVMODUL               TO KOLLI-ADVMODUL                     
382400     MOVE WS-ADHMODUL               TO KOLLI-ADHMODUL                     
382500                                                                          
382600     IF KOLLI-IDKOLLI-SAMP          >  ZERO                               
382700        MOVE NEJ                    TO KOLLI-FLUTLAST                     
382800     ELSE                                                                 
382900        MOVE WS-FLUTLAST           TO KOLLI-FLUTLAST                      
383000        IF KOLLI-FLAUTFAK = JA AND DIST03-SVERIGE-2                       
383100          MOVE NEJ                    TO KOLLI-FLUTLAST                   
383200        END-IF                                                            
383300     END-IF                                                               
383400                                                                          
383500     IF KOLLI-KDFARLIG-KOLLI = +4                                         
383600     OR KOLLI-KDFARLIG-KOLLI = +7                                         
383700       MOVE +950                    TO KOLLI-ADFLOMR                      
383800     END-IF                                                               
383900     .                                                                    
384000     EJECT                                                                
384100 S12-SKAPA-4322 SECTION.                                                  
384200                                                                          
384300*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
384400     IF DIST03-SVERIGE-100-799                                            
384500     OR DIST03-NORGE                                                      
384600     OR DIST03-DANMARK-900                                                
384700     OR DIST85-PU-VIA-VR                                                  
384800     OR DIST21-TYRE                                                       
384900     AND NOT DIST47-INTERNA                                               
385000        MOVE WS-IDPRODNR TO XXJK-4322-IDPRODNR                            
385100        MOVE KOLLI-IDKOLLI  TO XXJK-4322-IDKOLLI                          
385200        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
385300        PERFORM IMS-ISRT-4322-SEGM                                        
385400     END-IF                                                               
385500     .                                                                    
385600     EJECT                                                                
385700 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
385800     SKIP3                                                                
385900     MOVE +1 TO FG-INDX                                                   
386000     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
386100                                                                          
386200       IF TAB-IDPSN(FG-INDX) > ZERO                                       
386300         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
386400         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
386500                                       TAB-VKART-FG(FG-INDX) /            
386600                                       ARB-ANTAL-KOLLI                    
386700         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
386800                                       ARB-ANTAL-KOLLI                    
386900                                                                          
387000       ELSE                                                               
387100         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
387200                                       KOLLI-VKART-FG(FG-INDX)            
387300                                       KOLLI-VLFG(FG-INDX)                
387400       END-IF                                                             
387500                                                                          
387600       ADD +1 TO FG-INDX                                                  
387700     END-PERFORM                                                          
387800                                                                          
387900     IF TAB-IDPSN(1) > ZERO                                               
388000       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / ARB-ANTAL-KOLLI              
388100     ELSE                                                                 
388200       MOVE ZERO TO KOLLI-SUEQFG                                          
388300     END-IF                                                               
388400                                                                          
388500     .                                                                    
388600     EJECT                                                                
388700 S17-BERAEKNA-FG-FAELT SECTION.                                           
388800     SKIP3                                                                
388900     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
389000                                 (SPAR-VLFG        *                      
389100                                  KKOLLI-KVLEVART)                        
389200     IF SPAR-IDPSN = 10 OR 11                                             
389300       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
389400                                       (SPAR-VKART-FG        *            
389500                                        KKOLLI-KVLEVART)                  
389600     ELSE                                                                 
389700       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
389800     END-IF                                                               
389900                                                                          
390000     MOVE 10 TO FG-INDX                                                   
390100     .                                                                    
390200     EJECT                                                                
390300 S18-GENERERA-AVVIKELSE-TRANS SECTION.                                    
390400                                                                          
390500     PERFORM S18A-UPDATE-RY1-POST                                         
390600                                                                          
390700     MOVE WS-IDDISTR-NUM         TO TEST-IDDISTR                          
390800     IF NOT DIST19-SATS                                                   
390900       IF W-IDDC NOT = W-IDDC-B6                                          
391000          MOVE W-IDDC TO W-IDDC-B6                                        
391100          PERFORM IMS-GU-WDB601                                           
391200       END-IF                                                             
391300       IF DCS-CDC                                                         
391400       OR DCS-NDC OR (DCS-SDC AND DCS-CHINA)                              
391500       OR SPAR-ORAD-FLFYSAVV = JA                                         
391600         PERFORM S18B-UPDATE-ORDBEK-WDQ1                                  
391700                                                                          
391800         IF SPAR-ORAD-KDORDKL = 0                                         
391900            IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                        
392000                PERFORM S18C-UPDATE-VOR-QUE                               
392100            ELSE                                                          
392200                PERFORM S18D-UPDATE-VORKONY                               
392300            END-IF                                                        
392400            PERFORM S25-DELETE-PRICE-Q-LINE                               
392500         END-IF                                                           
392600       END-IF                                                             
392700     END-IF                                                               
392800                                                                          
392900     IF WS-KDORDBEK              = 90 OR                                  
393000       (WS-KDORDBEK              = 91 AND                                 
393100        SW-TIRODAT-LIKA-MED-ZERO = JA)                                    
393200       PERFORM S18E-SKAPA-RYK-TRANS                                       
393300     END-IF                                                               
393400     .                                                                    
393500     EJECT                                                                
393600 S18A-UPDATE-RY1-POST         SECTION.                                    
393700                                                                          
393800     IF LOGG-IDLOGLOP = 9                                                 
393900        MOVE ZERO                TO   LOGG-IDLOGLOP                       
394000     END-IF                                                               
394100                                                                          
394200     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
394300     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
394400     ADD +1                               TO   LOGG-IDLOGLOP              
394500     MOVE 'RY1'                           TO   RY1-IDPTYP                 
394600                                               LOGG-IDPTYP                
394700     MOVE SPAR-ORAD-BERADREF              TO   RY1-BERADREF               
394800     MOVE SPAR-ORAD-BEVOLREF              TO   RY1-BEVOLREF               
394900     MOVE WS-IDKUNDRF                     TO   RY1-IDKUNDRF               
395000     MOVE SPAR-ORAD-IDARTNR               TO   RY1-IDARTNR                
395100     MOVE SPAR-ORAD-FLRESTN               TO   RY1-FLRESTN                
395200     MOVE SPAR-ORAD-FLDIRLEV              TO   RY1-FLDIRLEV               
395300     MOVE SPAR-ORAD-IDKUNDRF-RO           TO   RY1-IDKUNDRF-RO            
395400     MOVE SPAR-ORAD-FLTILLK               TO   RY1-FLTILLK                
395500                                                                          
395600     IF SPAR-ORAD-KDDSP = 0                                               
395700        MOVE 1                            TO  RY1-KDDSP                   
395800     ELSE                                                                 
395900        MOVE SPAR-ORAD-KDDSP              TO  RY1-KDDSP                   
396000     END-IF                                                               
396100     MOVE WS-KDFAKTYP                     TO  RY1-KDFAKTYP                
396200                                                                          
396300     IF   WS-KDORDBEK = 90 AND WS-KDTPOTYP =  6                           
396400       MOVE 91                            TO  RY1-KDORDBEK                
396500     ELSE                                                                 
396600       MOVE WS-KDORDBEK                   TO  RY1-KDORDBEK                
396700     END-IF                                                               
396800                                                                          
396900     MOVE SPAR-ORAD-KDORDING              TO  RY1-KDORDING                
397000     MOVE SPAR-ORAD-KDORDTYP              TO  RY1-KDORDTYP                
397100     MOVE SPAR-ORAD-KDKVBRYT              TO  RY1-KDKVBRYT                
397200     MOVE WS-KDTPOTYP                     TO  RY1-KDTPOTYP                
397300     MOVE SPAR-ORAD-KDVRINFO              TO  RY1-KDVRINFO                
397400     MOVE SPAR-ORAD-KVBEART               TO  RY1-KVBEART                 
397500     MOVE SPAR-ORAD-KVAVBART              TO  RY1-KVAVBART                
397600     MOVE WS-KVORAPP-TOTAL                TO  RY1-KVAVART                 
397700     MOVE SPAR-ORAD-KVLEVART              TO  RY1-KVLEVART                
397800     MOVE SPAR-ORAD-REKSIFFR              TO  RY1-REKSIFFR                
397900                                                                          
398000     MOVE SPAR-ORAD-IDARTNR               TO  W-IDARTNR                   
398100                                                                          
398200     PERFORM IMS-GU-ARTC11                                                
398300     MOVE KORD-IDDC                       TO W-711-IDDC                   
398400     PERFORM IMS-GU-WDK722                                                
398500     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
398600        MOVE XLAG-IDANSK                  TO WS-IDANSK                    
398700     ELSE                                                                 
398800        MOVE CLAG-IDANSK                  TO WS-IDANSK                    
398900     END-IF                                                               
399000     MOVE CLAG-TIDISPIN                   TO  RY1-TIDISPIN                
399100                                              WS-TIDISPIN                 
399200                                                                          
399300     MOVE SPAR-ORAD-TIUTSKR               TO  RY1-TIORDREG                
399400     MOVE SPAR-ORAD-TIRODAT               TO  RY1-TIRODAT                 
399500                                                                          
399600     MOVE WS-IDORDER                      TO  W-201-IDORDER               
399700     PERFORM IMS-GHU-ORQI01                                               
399800                                                                          
399900     MOVE WS-IDDISTR-NUM                  TO  RY1S-IDDISTR                
400000     MOVE W-IDDC                          TO  RY1S-IDDC                   
400100     MOVE WS-IDKUNDNR-NUM                 TO  RY1S-IDKUNDNR               
400200     IF  OHUV-FLVORKO = JA                                                
400300     OR  OHUV-FLVORKO = YES                                               
400400         MOVE JA                          TO  RY1S-FLVORKO                
400500     ELSE                                                                 
400600         MOVE OHUV-FLVORKO                TO  RY1S-FLVORKO                
400700     END-IF                                                               
400800     MOVE OHUV-FLFORBI                    TO  RY1S-FLFORBI                
400900     MOVE OHUV-FLOVRLEV                   TO  RY1S-FLOVRLEV               
401000     MOVE SPAR-ORAD-KDPRODSL              TO  RY1S-KDPRODSL               
401100     MOVE SPAR-ORAD-IDSYSTEM              TO  RY1S-IDSYSTEM               
401200     MOVE WS-FLLSBOK                      TO  RY1S-FLLSBOK                
401300     MOVE WS-FLORDSPE                     TO  RY1S-FLORDSPE               
401400     MOVE SPAR-ORAD-KDFRAKT               TO  RY1S-KDFRAKT                
401500     MOVE SPAR-ORAD-KDORDKL               TO  RY1S-KDORDKL                
401600     MOVE SPAR-ORAD-KVANNANT              TO  RY1S-KVANNANT               
401700     MOVE SPAR-ORAD-KVSLATT               TO  RY1S-KVSLATT                
401800                                                                          
401900     MOVE RY1S-WDGZRY1S                   TO  LOGG-SORTPOST               
402000     MOVE RY1-WDGZRY1                     TO  LOGG-LOGGPOST               
402100*                                                                         
402200     PERFORM IMS-ISRT-AVVIKELSE                                           
402300*                                                                         
402400     PERFORM UNTIL SEGMENT-FINNS                                          
402500                                                                          
402600       IF LOGG-IDLOGLOP = 9                                               
402700         MOVE ZERO              TO LOGG-IDLOGLOP                          
402800         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
402900       END-IF                                                             
403000                                                                          
403100       ADD +1                   TO LOGG-IDLOGLOP                          
403200       PERFORM IMS-ISRT-AVVIKELSE                                         
403300     END-PERFORM                                                          
403400     EJECT                                                                
403500     .                                                                    
403600 S18B-UPDATE-ORDBEK-WDQ1      SECTION.                                    
403700                                                                          
403800     MOVE WS-IDORDER                      TO OBKR-IDORDER                 
403900     MOVE SPAR-ORAD-IDARTNR               TO OBKR-IDARTNR                 
404000*--- LÄS FRAM TILL FÖRSTA LEDIGA LÖPNR                                    
404100     MOVE OBKR-IDORDER                    TO W-IDORDER-Q1-MIN             
404200                                             W-IDORDER-Q1-MAX             
404300     MOVE OBKR-IDARTNR                    TO W-IDARTNR-Q1-MIN             
404400                                             W-IDARTNR-Q1-MAX             
404500     MOVE +1                              TO W-IDLOPNR-Q1-MIN             
404600                                             W-IDLOPNR-Q1-MAX             
404700                                             W-IDSEKVNR-Q1-MIN            
404800                                             W-IDSEKVNR-Q1-MAX            
404900     PERFORM IMS-GU-ORQM01                                                
405000     PERFORM UNTIL SEGMENT-SAKNAS                                         
405100        ADD +1                            TO W-IDLOPNR-Q1-MIN             
405200                                             W-IDLOPNR-Q1-MAX             
405300        PERFORM IMS-GU-ORQM01                                             
405400     END-PERFORM                                                          
405500     MOVE W-IDLOPNR-Q1-MIN                TO OBKR-IDLOPNR                 
405600     MOVE 1                               TO OBKR-IDSEKVNR                
405700     MOVE W-IDDC                          TO OBKR-IDDC                    
405800     MOVE SPAR-ORAD-IDDC-RO               TO OBKR-IDDC-RO                 
405900     MOVE SPAR-ORAD-KDOI                  TO OBKR-KDOI                    
406000     MOVE SPAR-ORAD-CLEARGROUP            TO OBKR-CLEARGROUP              
406100     MOVE WS-KDORDBEK                     TO OBKR-KDORDBEK                
406200     MOVE IDPGM                           TO OBKR-IDPGM                   
406300     MOVE SPACE                           TO OBKR-BEERS                   
406400     MOVE SPAR-BEKUNDRF                   TO OBKR-BEKUNDRF                
406500     MOVE SPAR-ORAD-BERADREF              TO OBKR-BERADREF                
406600     MOVE SPAR-ORAD-BEVOLREF              TO OBKR-BEVOLREF                
406700     MOVE SPAR-ORAD-IDKAMPRF              TO OBKR-IDKAMPRF                
406800     MOVE 0                               TO OBKR-DIERS-KVOT              
406900     MOVE NEJ                             TO OBKR-FLAKPLOC                
407000     MOVE NEJ                             TO OBKR-FLSLATT                 
407100     MOVE SPAR-ORAD-FLINVEST              TO OBKR-FLINVEST                
407200     MOVE JA                              TO OBKR-FLOBOK                  
407300     MOVE NEJ                             TO OBKR-FLOBTRAN                
407400     MOVE NEJ                             TO OBKR-FLOBPRT                 
407500     MOVE SPAR-ORAD-FLPRTILL              TO OBKR-FLPRTILL                
407600     MOVE SPAR-ORAD-FLRESTN               TO OBKR-FLRESTN                 
407700     MOVE NEJ                             TO OBKR-FLTILLK                 
407800     MOVE 0                               TO OBKR-IDARTNR-TILLK           
407900     MOVE WS-IDDISTR-NUM                  TO OBKR-IDDISTR                 
408000     MOVE WS-IDKUNDNR-NUM                 TO OBKR-IDKUNDNR                
408100                                                                          
408200     MOVE WS-IDKUNDRF                     TO WS-IDKUNDRF-OLD              
408300     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
408400     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
408500                                                                          
408600     MOVE SPAR-ORAD-IDKUNDRF-RO           TO WS-IDKUNDRF-OLD              
408700     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
408800     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
408900     MOVE SPAR-ORAD-IDLEVNR               TO OBKR-IDLEVNR                 
409000     MOVE SPAR-ORAD-IDLOPNR-RO            TO OBKR-IDLOPNR-RO              
409100     MOVE SPAR-ORAD-IDSYSTEM              TO OBKR-IDSYSTEM                
409200     MOVE SPAR-ORAD-KDDSP                 TO OBKR-KDDSP                   
409300     MOVE 0                               TO OBKR-KDERS                   
409400     MOVE SPAR-ORAD-KDKVBRYT              TO OBKR-KDKVBRYT                
409500     MOVE SPAR-ORAD-KDPRTYP               TO OBKR-KDPRTYP                 
409600     MOVE 0                               TO OBKR-KDTPOTYP                
409700     MOVE SPAR-ORAD-KDVRINFO              TO OBKR-KDVRINFO                
409800                                                                          
409900     EVALUATE TRUE                                                        
410000       WHEN WS-KDORDBEK = 90 OR 91                                        
410100         MOVE 0                          TO OBKR-KVANNANT                 
410200       WHEN WS-KDORDBEK = 80 OR 81 OR 93                                  
410300         IF DCS-CDC  OR  DCS-NDC                                          
410400           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVBEART                      
410500                                 - SPAR-ORAD-KVLEVART                     
410600                                 - SPAR-ORAD-KVANNANT                     
410700           END-COMPUTE                                                    
410800         ELSE                                                             
410900           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVAVBART                     
411000                                 - SPAR-ORAD-KVLEVART                     
411100           END-COMPUTE                                                    
411200         END-IF                                                           
411300       WHEN OTHER                                                         
411400         MOVE 'FELAKTIG ORDERBEKR-KOD'   TO RKOD-FELTEXT                  
411500         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
411600     END-EVALUATE                                                         
411700                                                                          
411800     MOVE SPAR-ORAD-KVAVBART              TO OBKR-KVAVBART                
411900     MOVE SPAR-ORAD-KVBEART               TO OBKR-KVBEART                 
412000                                             OBKR-KVBEART-Q               
412100     MOVE 0                               TO OBKR-KVBEART-TILLK           
412200                                             OBKR-KVPREAVB                
412300                                             OBKR-KVPRERO                 
412400     MOVE CLAG-KVQPACK-1                  TO OBKR-KVQPACK                 
412500*                                                                         
412600     IF WS-KDORDBEK = 90 OR 91                                            
412700        IF DCS-CDC  OR  DCS-NDC                                           
412800          MOVE WS-KVORAPP-TOTAL             TO OBKR-KVRO                  
412900        ELSE                                                              
413000          MOVE WS-KVORAPP-PACK              TO OBKR-KVRO                  
413100        END-IF                                                            
413200        MOVE WS-DAGENS-DATUM              TO OBKR-TIRODAT                 
413300     ELSE                                                                 
413400        MOVE 0                            TO OBKR-KVRO                    
413500        MOVE 000000                       TO OBKR-TIRODAT                 
413600     END-IF                                                               
413700*                                                                         
413800     MOVE SPAR-ORAD-KVSLATT               TO OBKR-KVSLATT                 
413900     MOVE SPAR-ORAD-PRARTNTO              TO OBKR-PRARTNTO                
414000     MOVE SPAR-ORAD-DEAL-PR-LINE                                          
414100                                     TO OBKR-DEAL-PR-LINE                 
414200     IF SPAR-ORAD-KDVALISO-EXP NOT = SPACE                                
414300        MOVE SPAR-ORAD-KDVALISO-EXP  TO OBKR-KDVALISO                     
414400     END-IF                                                               
414500                                                                          
414600     MOVE SPAR-ORAD-REKSIFFR              TO OBKR-REKSIFFR                
414700     MOVE 0                               TO OBKR-PRBPRIS                 
414800                                             OBKR-REKSIFFR-TILLK          
414900                                             OBKR-RERF-RAD                
415000                                             OBKR-TITPO                   
415100     MOVE WS-TIDISPIN                     TO OBKR-TIDISPIN                
415200     MOVE WS-TIORDREG                     TO OBKR-TIORDREG                
415300                                                                          
415400     MOVE ZERO                            TO WS-DAORDREG                  
415500     MOVE FUNCTION CURRENT-DATE(1:2)      TO WS-DAORDREG-TISS             
415600     MOVE OBKR-TIORDREG                   TO WS-DAORDREG-TIAAMMDD         
415700     COMPUTE OBKR-TITIORDD-9KOMPL =                                       
415800             WS-9KOMPL-GRUND - WS-DAORDREG                                
415900                                                                          
416000     MOVE SPAR-ORAD-TIPRIS                TO OBKR-TIPRIS                  
416100                                                                          
416200     MOVE WS-DAGENS-DATUM                 TO OBKR-TIREGDAT                
416300     MOVE   WS-TTMMSS                     TO OBKR-TIREGTID                
416400                                                                          
416500     IF WS-KDORDBEK = 90 OR 91                                            
416600        MOVE ZERO                         TO WS-DARODAT                   
416700        MOVE FUNCTION CURRENT-DATE(1:2)   TO WS-DARODAT-TISS              
416800        MOVE OBKR-TIRODAT                 TO WS-DARODAT-TIAAMMDD          
416900        COMPUTE OBKR-TITIREGD-9KOMPL =                                    
417000             WS-9KOMPL-GRUND - WS-DARODAT                                 
417100     ELSE                                                                 
417200        MOVE 0                            TO OBKR-TITIREGD-9KOMPL         
417300     END-IF                                                               
417400                                                                          
417500     MOVE SPAR-ORAD-KDFRAKT                TO OBKR-KDFRAKT                
417600     MOVE SPAR-ORAD-KDORDKL                TO OBKR-KDORDKL                
417700     MOVE SPACE                            TO OBKR-IDBIL                  
417800                                                                          
417900     MOVE OHUV-KDORDTYP-LDC                TO OBKR-KDORDTYP-LDC           
418000     MOVE OHUV-TIREPDAT                    TO OBKR-TIREPDAT               
418100     MOVE SPAR-ORAD-IDKUNDRF-WIP           TO OBKR-IDKUNDRF-WIP           
418200     MOVE ZERO                             TO OBKR-TIDLEVDAT              
418300     MOVE SPAR-ORAD-PRAVCOST               TO OBKR-PRAVCOST               
418400                                                                          
418500     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
418600        MOVE OBKR-IDARTNR         TO W-IDARTNR                            
418700        MOVE DCS-IDLANDX2         TO W-IDLAND                             
418800        PERFORM IMS-GU-WDK712                                             
418900        IF SEGMENT-FINNS AND LART-FLREFERAL = JA                          
419000           MOVE 98                TO OBKR-KDORDBEK                        
419100        END-IF                                                            
419200     END-IF                                                               
419300                                                                          
419400     PERFORM IMS-ISRT-ORQM01                                              
419500     .                                                                    
419600     EJECT                                                                
419700 S18C-UPDATE-VOR-QUE          SECTION.                                    
419800                                                                          
419900     IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                           
420000        MOVE SPAR-ORAD-BERADREF            TO 4542-BERADREF               
420100        MOVE WS-IDDISTR-NUM                TO 4542-IDDISTR                
420200        MOVE WS-IDANSK                     TO 4542-IDANSK                 
420300        MOVE OBKR-IDDC                     TO 4542-IDDC                   
420400        MOVE SPAR-ORAD-IDARTNR             TO 4542-IDARTNR                
420500        MOVE WS-IDKUNDNR-NUM               TO 4542-IDKUNDNR               
420600        MOVE WS-IDKUNDRF                   TO WS-IDKUNDRF-OLD             
420700        MOVE WS-IDORDNR5-OLD               TO WS-IDORDNR7-NEW             
420800        MOVE WS-IDKUNDRF-NEW               TO 4542-IDKUNDRF               
420900        MOVE WS-IDORDER                    TO 4542-IDORDER                
421000        MOVE SPACE                         TO 4542-IDUSER                 
421100        MOVE 93                            TO 4542-KDORDBEK               
421200        MOVE SPAR-ORAD-KDPRTYP             TO 4542-KDPRTYP                
421300        MOVE 0                             TO 4542-KDVORATG               
421400        MOVE SPAR-ORAD-KVBEART             TO 4542-KVBEART                
421500                                              4542-KVBEART-Q              
421600        IF SPAR-ORAD-FLFYSAVV = JA                                        
421700          IF DCS-SDC AND DCS-CHINA                                        
421800            MOVE ZERO                 TO WS-AVVIKELSE-UTSKR               
421900            COMPUTE WS-AVVIKELSE-UTSKR =                                  
422000                    SPAR-ORAD-KVBEART - SPAR-ORAD-KVAVBART                
422100            END-COMPUTE                                                   
422200            COMPUTE WS-SUMMA =                                            
422300                    SPAR-ORAD-KVLEVART + WS-AVVIKELSE-UTSKR               
422400            END-COMPUTE                                                   
422500            MOVE WS-SUMMA                  TO 4542-KVPREAVB               
422600          ELSE                                                            
422700            MOVE SPAR-ORAD-KVLEVART        TO 4542-KVPREAVB               
422800          END-IF                                                          
422900*         WS-SUMMA MINUS ORAD-KVBEART ÄR AVVIKELSE I PACKNINGEN           
423000*         VILKET ÄR DET SOM VISAS (RÄKNAS FRAM) PÅ 4224-BILDEN            
423100        ELSE                                                              
423200          MOVE SPAR-ORAD-KVLEVART          TO 4542-KVPREAVB               
423300        END-IF                                                            
423400        MOVE SPAR-ORAD-PRARTNTO           TO 4542-PRARTNTO                
423500        MOVE SPAR-ORAD-DEAL-PR-LINE                                       
423600                           TO 4542-DEAL-PR-LINE                           
423700        MOVE SPACE                        TO 4542-TEVORMRK                
423800        MOVE WS-DAGENS-DATUM              TO 4542-TIREGDAT                
423900        MOVE WS-TTMMSS                    TO 4542-TIREGTID                
424000        MOVE 0                            TO 4542-TIUPPDAT                
424100                                             4542-TIUPPTID                
424200        MOVE 1                            TO 4542-IDLOPNR                 
424300        MOVE SPAR-ORAD-IDLEVNR            TO 4542-IDLEVNR                 
424400*                                                                         
424500        PERFORM IMS-ISRT-4542                                             
424600        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
424700          ADD +1                           TO 4542-IDLOPNR                
424800          PERFORM IMS-ISRT-4542                                           
424900        END-PERFORM                                                       
425000                                                                          
425100        IF  DCS-NDC-CN                                                    
425200        OR (DCS-NDC-NA AND DCS-USA)                                       
425300          IF SLAG-IDDC-REF = SPACE                                        
425400            MOVE SPAR-ORAD-IDARTNR  TO W-IDARTNR                          
425500            MOVE OBKR-IDDC          TO W-711-IDDC                         
425600            MOVE WS-IDDISTR-NUM     TO S27-IDDISTR                        
425700            MOVE WS-IDKUNDNR-NUM    TO S27-IDKUNDNR                       
425800            MOVE SPAR-ORAD-IDARTNR  TO S27-IDARTNR                        
425900            PERFORM IMS-GU-WDK722                                         
426000            IF SEGMENT-FINNS AND XLAG-IDANSK > 0                          
426100              MOVE XLAG-IDANSK      TO S27-IDANSK                         
426200            ELSE                                                          
426300              PERFORM IMS-GU-ARTC11                                       
426400              MOVE CLAG-IDANSK      TO S27-IDANSK                         
426500            END-IF                                                        
426600            PERFORM S27-STARTA-W2T191X                                    
426700          END-IF                                                          
426800        END-IF                                                            
426900                                                                          
427000          IF SPAR-ORAD-IDLEVNR = SPACE AND                                
427100             WS-FLORDSPE = NEJ        AND                                 
427200             WS-FLOVRLEV = NEJ                                            
427300             IF  DCS-NDC OR                                               
427400                (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                     
427500               MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                    
427600               MOVE W-IDDC                TO W-711-IDDC                   
427700               PERFORM IMS-GHU-WDK711                                     
427800                                                                          
427900               IF DCS-NDC                                                 
428000                 COMPUTE SLAG-KVOKS-DAG =                                 
428100                         SLAG-KVOKS-DAG +                                 
428200                         (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)         
428300                 END-COMPUTE                                              
428400               ELSE                                                       
428500                 COMPUTE SLAG-KVOKS-DAG =                                 
428600                         SLAG-KVOKS-DAG +                                 
428700                        (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)         
428800                 END-COMPUTE                                              
428900               END-IF                                                     
429000                                                                          
429100               PERFORM IMS-REPL-WDK7                                      
429200             END-IF                                                       
429300          END-IF                                                          
429400                                                                          
429500     END-IF                                                               
429600     .                                                                    
429700     EJECT                                                                
429800 S18D-UPDATE-VORKONY          SECTION.                                    
429900                                                                          
430000     MOVE SPAR-ORAD-IDARTNR TO S28-IDARTNR                                
430100     MOVE DCS-IDDC          TO S28-IDDC                                   
430200     PERFORM S28-BESTAM-LENVR-ANSK                                        
430300                                                                          
430400*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
430500                                                                          
430600     PERFORM S26-SOK-RAD-VORKO                                            
430700                                                                          
430800     IF  TRAFF-VORKO                                                      
430900         IF DCS-CDC                                                       
431000             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
431100                                   - SPAR-ORAD-KVBEART                    
431200                                   + SPAR-ORAD-KVLEVART                   
431300             END-COMPUTE                                                  
431400             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
431500                                   - SPAR-ORAD-KVBEART                    
431600                                   + SPAR-ORAD-KVLEVART                   
431700             END-COMPUTE                                                  
431800         ELSE                                                             
431900             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
432000                                   - SPAR-ORAD-KVAVBART                   
432100                                   + SPAR-ORAD-KVLEVART                   
432200             END-COMPUTE                                                  
432300             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
432400                                   - SPAR-ORAD-KVAVBART                   
432500                                   + SPAR-ORAD-KVLEVART                   
432600             END-COMPUTE                                                  
432700         END-IF                                                           
432800         IF  VOR-KVPREAVB = 0                                             
432900             MOVE '7'              TO VOR-KDVORATG                        
433000             MOVE 93               TO VOR-KDORDBEK                        
433100             IF VOR-TIKLAR = ZERO                                         
433200                MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                       
433300                COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                  
433400                                      / 100                               
433500                END-COMPUTE                                               
433600             END-IF                                                       
433700         END-IF                                                           
433800         PERFORM IMS-REPL-SEQB-WDA601                                     
433900                                                                          
434000         MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                       
434100         ADD +1                 TO WS-VOR-TID-BRIST                       
434200         MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                       
434300         SUBTRACT WS-DAGENS-DATUM FROM 9999999                            
434400                                  GIVING VOR-TIREGDAT-AVV9                
434500         SUBTRACT WS-VOR-TID-BRIST FROM 999999999                         
434600                                   GIVING VOR-TIREGTID-AVV9               
434700         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
434800         MOVE 0                 TO VOR-TIREGDAT-LEV                       
434900         MOVE 0                 TO VOR-TIREGTID-LEV                       
435000         IF DCS-CDC                                                       
435100             SUBTRACT SPAR-ORAD-KVLEVART                                  
435200                                  FROM SPAR-ORAD-KVBEART                  
435300                                  GIVING VOR-KVBEART                      
435400                                         VOR-KVBEART-Q                    
435500         ELSE                                                             
435600             SUBTRACT SPAR-ORAD-KVLEVART                                  
435700                                  FROM SPAR-ORAD-KVAVBART                 
435800                                  GIVING VOR-KVBEART                      
435900                                         VOR-KVBEART-Q                    
436000         END-IF                                                           
436100         MOVE 0                 TO VOR-KVPREAVB                           
436200         MOVE W-IDDC            TO VOR-IDDC                               
436300         MOVE SPACE             TO VOR-IDUSER                             
436400         MOVE 93                TO VOR-KDORDBEK                           
436500         MOVE '0'               TO VOR-KDVORATG                           
436600         MOVE 0                 TO VOR-TIKLAR                             
436700         MOVE 0                 TO VOR-TIKLATID                           
436800                                                                          
436900         PERFORM IMS-ISRT-WDA601                                          
437000         PERFORM UNTIL ISRT-OK                                            
437100            ADD +1                TO WS-VOR-TID-BRIST                     
437200            MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                     
437300            SUBTRACT WS-VOR-TID-BRIST FROM 999999999                      
437400                                  GIVING VOR-TIREGTID-AVV9                
437500            PERFORM IMS-ISRT-WDA601                                       
437600         END-PERFORM                                                      
437700     ELSE                                                                 
437800*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
437900*                                        BORDE NOG INTE FÖREKOMMA         
438000       MOVE WS-IDDISTR-NUM      TO VOR-IDDISTR                            
438100       MOVE WS-IDKUNDNR-NUM     TO VOR-IDKUNDNR                           
438200       MOVE WS-IDKUNDRF         TO WS-IDKUNDRF-OLD                        
438300       MOVE WS-IDORDNR5-OLD     TO WS-IDORDNR7-NEW                        
438400       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF                           
438500       MOVE WS-TIORDREG         TO VOR-TIREGDAT-URSP                      
438600       MOVE SPAR-ORAD-IDARTNR   TO VOR-IDARTNR                            
438700       ADD +1                   TO WS-VOR-TID-BRIST                       
438800       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
438900       MOVE 0                   TO VOR-TIREGDAT-AVV                       
439000       MOVE 0                   TO VOR-TIREGTID-AVV                       
439100       SUBTRACT 0            FROM 9999999                                 
439200                              GIVING VOR-TIREGDAT-AVV9                    
439300       SUBTRACT 0            FROM 999999999                               
439400                              GIVING VOR-TIREGTID-AVV9                    
439500       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF-LEV                       
439600       MOVE WS-TIORDREG         TO VOR-TIREGDAT-LEV                       
439700       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
439800       MOVE S28-IDANSK          TO VOR-IDANSK                             
439900                                                                          
440000       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
440100       PERFORM IMS-GU-WDP4A1                                              
440200       IF SEGMENT-SAKNAS                                                  
440300          MOVE DEF-IDROLL       TO SEQA-IDROLL                            
440400       END-IF                                                             
440500       MOVE SEQA-IDROLL         TO VOR-IDROLL                             
440600                                                                          
440700       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
440800       MOVE SPAR-ORAD-BERADREF  TO VOR-BERADREF                           
440900       IF  DCS-CDC                                                        
441000           SUBTRACT SPAR-ORAD-KVLEVART                                    
441100                                FROM SPAR-ORAD-KVBEART                    
441200                                GIVING VOR-KVBEART-URSP                   
441300                                       VOR-KVBEART                        
441400       ELSE                                                               
441500           SUBTRACT SPAR-ORAD-KVLEVART                                    
441600                                FROM SPAR-ORAD-KVAVBART                   
441700                                GIVING VOR-KVBEART-URSP                   
441800                                       VOR-KVBEART                        
441900       END-IF                                                             
442000       MOVE 0                   TO VOR-KVPREAVB                           
442100                                   VOR-KVBEART-Q                          
442200       MOVE W-IDDC              TO VOR-IDDC                               
442300       MOVE SPACE               TO VOR-IDUSER                             
442400       MOVE 93                  TO VOR-KDORDBEK                           
442500       MOVE SPAR-ORAD-KDPRTYP   TO VOR-KDPRTYP                            
442600       MOVE '7'                  TO VOR-KDVORATG                          
442700       MOVE SPAR-ORAD-PRARTNTO  TO VOR-PRARTNTO                           
442800       MOVE '  '                TO VOR-TEVORMRK                           
442900*      MOVE '  '                TO VOR-TEVORMRK-SC                        
443000       MOVE 0                   TO VOR-TIUPPDAT                           
443100       MOVE 0                   TO VOR-TIUPPTID                           
443200       MOVE WS-DAGENS-DATUM     TO VOR-TIKLAR                             
443300       COMPUTE VOR-TIKLATID     = WS-VOR-TID-BRIST                        
443400                                / 100                                     
443500       END-COMPUTE                                                        
443600       MOVE SPAR-ORAD-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                    
443700       MOVE NEJ                 TO VOR-FLVORFK                            
443800       PERFORM IMS-ISRT-WDA601                                            
443900       PERFORM UNTIL ISRT-OK                                              
444000          ADD +1              TO VOR-TIREGTID-URSP                        
444100          ADD +1              TO VOR-TIREGTID-LEV                         
444200          PERFORM IMS-ISRT-WDA601                                         
444300       END-PERFORM                                                        
444400                                                                          
444500*--------------------------------------- EJ TRÄFF, AVVIK RAD              
444600       MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                         
444700       ADD +1                 TO WS-VOR-TID-BRIST                         
444800       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
444900       SUBTRACT WS-DAGENS-DATUM  FROM 9999999                             
445000                                 GIVING VOR-TIREGDAT-AVV9                 
445100       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
445200                                 GIVING VOR-TIREGTID-AVV9                 
445300       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
445400       MOVE 0                 TO VOR-TIREGDAT-LEV                         
445500       MOVE 0                 TO VOR-TIREGTID-LEV                         
445600       IF DCS-CDC                                                         
445700           SUBTRACT SPAR-ORAD-KVLEVART                                    
445800                                FROM SPAR-ORAD-KVBEART                    
445900                                GIVING VOR-KVBEART-Q                      
446000       ELSE                                                               
446100           SUBTRACT SPAR-ORAD-KVLEVART                                    
446200                                FROM SPAR-ORAD-KVAVBART                   
446300                                GIVING VOR-KVBEART-Q                      
446400       END-IF                                                             
446500       MOVE 0                 TO VOR-KVPREAVB                             
446600       MOVE 93                TO VOR-KDORDBEK                             
446700       MOVE '0'               TO VOR-KDVORATG                             
446800       MOVE 0                 TO VOR-TIKLAR                               
446900       MOVE 0                 TO VOR-TIKLATID                             
447000                                                                          
447100       PERFORM IMS-ISRT-WDA601                                            
447200       PERFORM UNTIL ISRT-OK                                              
447300          ADD +1                TO WS-VOR-TID-BRIST                       
447400          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
447500          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
447600                                GIVING VOR-TIREGTID-AVV9                  
447700          PERFORM IMS-ISRT-WDA601                                         
447800       END-PERFORM                                                        
447900     END-IF                                                               
448000                                                                          
448100     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
448200                                + VOR-KVBEART-Q                           
448300                                - VOR-KVPREAVB                            
448400     END-COMPUTE                                                          
448500                                                                          
448600     PERFORM IMS-REPL-WDK611                                              
448700                                                                          
448800     MOVE WS-IDDISTR-NUM        TO S27-IDDISTR                            
448900     MOVE WS-IDKUNDNR-NUM       TO S27-IDKUNDNR                           
449000     MOVE SPAR-ORAD-IDARTNR     TO S27-IDARTNR                            
449100     MOVE S28-IDANSK            TO S27-IDANSK                             
449200     PERFORM S27-STARTA-W2T191X                                           
449300                                                                          
449400*------------------------------SAMMA EFTERHANTERING SOM I S18C            
449500     IF SPAR-ORAD-IDLEVNR = SPACE AND                                     
449600        WS-FLORDSPE = NEJ        AND                                      
449700        WS-FLOVRLEV = NEJ                                                 
449800        IF  DCS-NDC                                                       
449900        OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                          
450000          MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                         
450100          MOVE W-IDDC                TO W-711-IDDC                        
450200          PERFORM IMS-GHU-WDK711                                          
450300                                                                          
450400          IF  DCS-NDC                                                     
450500            COMPUTE SLAG-KVOKS-DAG =                                      
450600                    SLAG-KVOKS-DAG +                                      
450700                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
450800            END-COMPUTE                                                   
450900          ELSE                                                            
451000            COMPUTE SLAG-KVOKS-DAG =                                      
451100                    SLAG-KVOKS-DAG +                                      
451200                   (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)              
451300            END-COMPUTE                                                   
451400          END-IF                                                          
451500                                                                          
451600          PERFORM IMS-REPL-WDK7                                           
451700        ELSE                                                              
451800          IF DCS-CDC                                                      
451900            MOVE SPAR-ORAD-IDARTNR        TO W-901-IDARTNR                
452000            PERFORM IMS-GHU-WDK901                                        
452100                                                                          
452200            COMPUTE ART-KVOKS-VOR =                                       
452300                    ART-KVOKS-VOR +                                       
452400                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
452500            END-COMPUTE                                                   
452600                                                                          
452700            PERFORM IMS-REPL-WDK901                                       
452800          END-IF                                                          
452900        END-IF                                                            
453000     END-IF                                                               
453100     .                                                                    
453200     EJECT                                                                
453300 S20-FINN-INTERVALL SECTION.                                              
453400                                                                          
453500     PERFORM IMS-GU-WDM211                                                
453600     IF SEGMENT-FINNS                                                     
453700       PERFORM IMS-GNP-WDM221                                             
453800       PERFORM UNTIL SEGMENT-SAKNAS                                       
453900         IF  WS-IDDISTR-NUM > KMRK-IDDISTR-TOM                            
454000         OR  WS-IDDISTR-NUM < KMRK-IDDISTR-FOM                            
454100           CONTINUE                                                       
454200         ELSE                                                             
454300           IF  WS-IDDISTR-NUM  = KMRK-IDDISTR-TOM                         
454400           AND WS-IDKUNDNR-NUM > KMRK-IDKUNDNR-TOM                        
454500             CONTINUE                                                     
454600           ELSE                                                           
454700             IF  WS-IDDISTR-NUM  = KMRK-IDDISTR-FOM                       
454800             AND WS-IDKUNDNR-NUM < KMRK-IDKUNDNR-FOM                      
454900               CONTINUE                                                   
455000             ELSE                                                         
455100               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
455200               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
455300               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
455400               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
455500             END-IF                                                       
455600           END-IF                                                         
455700         END-IF                                                           
455800         PERFORM IMS-GNP-WDM221                                           
455900       END-PERFORM                                                        
456000     END-IF                                                               
456100     .                                                                    
456200     EJECT                                                                
456300                                                                          
456400 S26-SOK-RAD-VORKO SECTION.                                               
456500                                                                          
456600     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
456700     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
456800                                                                          
456900     MOVE WS-IDDISTR-NUM    TO W-A601KY-MIN-IDDISTR                       
457000                               W-A601KY-MAX-IDDISTR                       
457100     MOVE WS-IDKUNDNR-NUM   TO W-A601KY-MIN-IDKUNDNR                      
457200                               W-A601KY-MAX-IDKUNDNR                      
457300     MOVE WS-IDKUNDRF       TO WS-IDKUNDRF-OLD                            
457400     MOVE WS-IDORDNR5-OLD   TO WS-IDORDNR7-NEW                            
457500     MOVE WS-IDKUNDRF-NEW   TO W-A601KY-MIN-IDKUNDRF                      
457600                               W-A601KY-MAX-IDKUNDRF                      
457700     MOVE WS-TIORDREG       TO W-A601KY-MIN-TIREGDAT                      
457800                               W-A601KY-MAX-TIREGDAT                      
457900     MOVE SPAR-ORAD-IDARTNR TO W-A601KY-MIN-IDARTNR                       
458000                               W-A601KY-MAX-IDARTNR                       
458100     MOVE NEJ               TO TRAFF-VORKO-SW                             
458200     IF W-IDDC NOT = W-IDDC-B6                                            
458300        MOVE W-IDDC TO W-IDDC-B6                                          
458400        PERFORM IMS-GU-WDB601                                             
458500     END-IF                                                               
458600                                                                          
458700     PERFORM IMS-GHU-SEQB-WDA601                                          
458800     PERFORM UNTIL SEGMENT-SAKNAS                                         
458900                OR SEGMENT-SLUT                                           
459000                OR TRAFF-VORKO                                            
459100       IF  DCS-CDC                                                        
459200       AND SPAR-ORAD-KVBEART = VOR-KVPREAVB                               
459300           MOVE JA       TO TRAFF-VORKO-SW                                
459400       ELSE                                                               
459500         IF  SPAR-ORAD-KVAVBART = VOR-KVPREAVB                            
459600             MOVE JA       TO TRAFF-VORKO-SW                              
459700         ELSE                                                             
459800            PERFORM IMS-GHN-SEQB-WDA601                                   
459900         END-IF                                                           
460000       END-IF                                                             
460100     END-PERFORM                                                          
460200     .                                                                    
460300     EJECT                                                                
460400 S27-STARTA-W2T191X  SECTION.                                             
460500                                                                          
460600     COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17                   
460700     MOVE +1                    TO 2191-MID-KDCLAGER                      
460800     MOVE S27-IDARTNR-X         TO 2191-MID-IDARTNR                       
460900     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
461000                                   2191-MID-TISENBEK-KL                   
461100     MOVE SPACE                 TO 2191-MID-IDKR                          
461200     MOVE S27-IDANSK-X          TO 2191-MID-IDANSK                        
461300     MOVE '500'                 TO 2191-MID-KDLARM                        
461400     MOVE S27-IDDISTR-X         TO 2191-MID-IDDISTR                       
461500     MOVE S27-IDKUNDNR-X        TO 2191-MID-IDKUNDNR                      
461600     MOVE WS-IDKUNDRF           TO WS-IDKUNDRF-OLD                        
461700     MOVE WS-IDORDNR5-OLD       TO WS-IDORDNR7-NEW                        
461800     MOVE WS-IDKUNDRF-NEW       TO 2191-MID-IDKUNDRF                      
461900     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
462000     IF (DCS-NDC AND DCS-CHINA)                                           
462100     OR (DCS-NDC-NA AND DCS-USA)                                          
462200        MOVE SLAG-IDDC          TO 2191-MID-IDDC                          
462300        MOVE SLAG-IDLEVNR       TO 2191-MID-IDLEVNR                       
462400     ELSE                                                                 
462500        MOVE WC-CDC-SE          TO 2191-MID-IDDC                          
462600        MOVE SPACE              TO 2191-MID-IDLEVNR                       
462700     END-IF                                                               
462800                                                                          
462900     PERFORM IMS-PURG-2191                                                
463000                                                                          
463100     MOVE SPACE                 TO 2191-MID-W2I19101                      
463200     .                                                                    
463300     EJECT                                                                
463400 S28-BESTAM-LENVR-ANSK SECTION.                                           
463500                                                                          
463600     MOVE S28-IDARTNR    TO W-IDARTNR                                     
463700     MOVE S28-IDDC       TO W-711-IDDC                                    
463800     PERFORM IMS-GU-WDK601                                                
463900     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
464000                                                                          
464100     PERFORM IMS-GHNP-WDK611                                              
464200     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
464300                                                                          
464400     MOVE CLAG-IDANSK    TO S28-IDANSK                                    
464500     .                                                                    
464600     EJECT                                                                
464700 S18E-SKAPA-RYK-TRANS SECTION.                                            
464800     MOVE 'STA S18E-SKAPA-RYK'  TO WS-PGM-POSITION                        
464900                                                                          
465000     IF LOGG-IDLOGLOP = 9                                                 
465100       MOVE ZERO                TO   LOGG-IDLOGLOP                        
465200     END-IF                                                               
465300                                                                          
465400     ACCEPT  LOGG-TIAAMMDD      FROM DATE                                 
465500     ACCEPT  LOGG-TIKLOCK       FROM TIME                                 
465600     ADD     +1                 TO   LOGG-IDLOGLOP                        
465700                                                                          
465800     MOVE    'RYK'              TO   RYK-IDPTYP                           
465900                                     LOGG-IDPTYP                          
466000                                                                          
466100     MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                          
466200     MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                         
466300                                                                          
466400     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
466500       MOVE  WS-IDORDER         TO   RYK-IDORDER                          
466600     ELSE                                                                 
466700       MOVE  WS-IDDISTR-NUM     TO   W-WDQ2CSEQ-IDDISTR                   
466800       MOVE  WS-IDKUNDNR-NUM    TO   W-WDQ2CSEQ-IDKUNDNR                  
466900       MOVE  SPAR-ORAD-IDKUNDRF-RO(1:5)                                   
467000                                TO   W-WDQ2CSEQ-IDKUNDRF(3:7)             
467100       PERFORM IMS-GU-ORQI01-CSEQ                                         
467200       IF SEGMENT-FINNS                                                   
467300         MOVE OHUV-IDORDER      TO   RYK-IDORDER                          
467400       ELSE                                                               
467500         MOVE ZERO              TO   RYK-IDORDER                          
467600       END-IF                                                             
467700                                                                          
467800     END-IF                                                               
467900                                                                          
468000     MOVE    SPAR-ORAD-IDARTNR  TO   RYK-IDARTNR                          
468100     MOVE    DAT-TIAAMMDD       TO   RYK-TIRODAT                          
468200     MOVE    SPAR-ORAD-KVLEVART TO   RYK-KVLEVART                         
468300     MOVE    SPAR-ORAD-KVBEART  TO   RYK-KVBEART-Q                        
468400     MOVE    SPAR-ORAD-KDORDKL  TO   RYK-KDORDKL                          
468500     MOVE    SPAR-ORAD-KDPRODSL TO   RYK-KDPRODSL                         
468600                                                                          
468700     MOVE    WS-KDORDBEK        TO   RYK-KDORDBEK                         
468800                                                                          
468900     MOVE    SPACE              TO   LOGG-SORTPOST                        
469000     MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                        
469100                                                                          
469200     PERFORM IMS-ISRT-AVVIKELSE                                           
469300                                                                          
469400     PERFORM UNTIL SEGMENT-FINNS                                          
469500                                                                          
469600       IF LOGG-IDLOGLOP = 9                                               
469700         MOVE ZERO              TO LOGG-IDLOGLOP                          
469800         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
469900       END-IF                                                             
470000       ADD +1                   TO LOGG-IDLOGLOP                          
470100       PERFORM IMS-ISRT-AVVIKELSE                                         
470200     END-PERFORM                                                          
470300     MOVE 'END S18E-SKAPA-RYK'  TO WS-PGM-POSITION                        
470400     .                                                                    
470500     SKIP2                                                                
470600 S19-GENERERA-KLAR-SV4        SECTION.                                    
470700                                                                          
470800     IF LOGG-IDLOGLOP = 9                                                 
470900       MOVE ZERO                TO   LOGG-IDLOGLOP                        
471000     END-IF                                                               
471100                                                                          
471200     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
471300     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
471400     ADD +1                               TO   LOGG-IDLOGLOP              
471500     MOVE 'RY6'                           TO   RY6-IDPTYP                 
471600                                               LOGG-IDPTYP                
471700     MOVE VORD-IDDISTR                    TO   RY6-IDDISTR                
471800     MOVE VORD-IDKUNDNR                   TO   RY6-IDKUNDNR               
471900     MOVE WS-IDKUNDRF                     TO   RY6-IDKUNDRF               
472000     MOVE VORD-IDDC                       TO   RY6-IDDC                   
472100     MOVE VORD-IDPRODNR                   TO   RY6-IDPRODNR               
472200     MOVE SPACE                           TO  LOGG-SORTPOST               
472300     MOVE RY6-WDGZRY6                     TO  LOGG-LOGGPOST               
472400*                                                                         
472500     PERFORM IMS-ISRT-KLAR-SV4                                            
472600*                                                                         
472700     PERFORM UNTIL SEGMENT-FINNS                                          
472800                                                                          
472900       IF LOGG-IDLOGLOP = 9                                               
473000         MOVE ZERO              TO LOGG-IDLOGLOP                          
473100         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
473200       END-IF                                                             
473300       ADD +1                   TO LOGG-IDLOGLOP                          
473400       PERFORM IMS-ISRT-KLAR-SV4                                          
473500     END-PERFORM                                                          
473600     .                                                                    
473700     EJECT                                                                
473800 S20-EV-LARM-2191-MID  SECTION.                                           
473900                                                                          
474000** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
474100     IF CLAG-KVROS = 0                                                    
474200       IF CLAG-KVAKS-CDC = 0                                              
474300         COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17               
474400         MOVE +1              TO 2191-MID-KDCLAGER                        
474500         MOVE ORAD-IDARTNR    TO 2191-MID-IDARTNR                         
474600         MOVE ZERO            TO 2191-MID-TISENBEK-DAG                    
474700                                 2191-MID-TISENBEK-KL                     
474800         MOVE SPACE           TO 2191-MID-IDKR                            
474900         MOVE WS-IDANSK       TO 2191-MID-IDANSK                          
475000         MOVE 210             TO 2191-MID-KDLARM                          
475100         MOVE WS-IDDISTR-NUM4 TO 2191-MID-IDDISTR                         
475200         MOVE WS-IDKUNDNR-NUM TO 2191-MID-IDKUNDNR                        
475300         MOVE WS-IDKUNDRF     TO 2191-MID-IDKUNDRF                        
475400         MOVE 'J'             TO 2191-MID-FLNYLARM                        
475500         MOVE WC-CDC-SE       TO 2191-MID-IDDC                            
475600         MOVE SPACE           TO 2191-MID-IDLEVNR                         
475700                                                                          
475800         PERFORM IMS-PURG-2191                                            
475900       END-IF                                                             
476000     END-IF                                                               
476100     .                                                                    
476200     EJECT                                                                
476300 S20-EV-LARM-2191-MID-CN-US SECTION.                                      
476400     MOVE 'S20-EV-LARM-2191-MID-CN-US' TO CURRENT-SECTION                 
476500                                                                          
476600** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
476700     IF SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                        
476800       IF SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                       
476900         COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17               
477000         MOVE +1              TO 2191-MID-KDCLAGER                        
477100         MOVE ORAD-IDARTNR    TO 2191-MID-IDARTNR                         
477200         MOVE ZERO            TO 2191-MID-TISENBEK-DAG                    
477300                                 2191-MID-TISENBEK-KL                     
477400         MOVE SPACE           TO 2191-MID-IDKR                            
477500         MOVE WS-IDANSK       TO 2191-MID-IDANSK                          
477600         MOVE 210             TO 2191-MID-KDLARM                          
477700         MOVE WS-IDDISTR-NUM4 TO 2191-MID-IDDISTR                         
477800         MOVE WS-IDKUNDNR-NUM TO 2191-MID-IDKUNDNR                        
477900         MOVE WS-IDKUNDRF     TO 2191-MID-IDKUNDRF                        
478000         MOVE 'J'             TO 2191-MID-FLNYLARM                        
478100         MOVE SLAG-IDDC       TO 2191-MID-IDDC                            
478200         MOVE SLAG-IDLEVNR    TO 2191-MID-IDLEVNR                         
478300                                                                          
478400         PERFORM IMS-PURG-2191                                            
478500                                                                          
478600       END-IF                                                             
478700     END-IF                                                               
478800     .                                                                    
478900     EJECT                                                                
479000 S21-INIT-WS-FIELDS    SECTION.                                           
479100                                                                          
479200     MOVE ZERO                  TO INX-TOT-ANT-RADER                      
479300                                   SPAR-VKORDNTO                          
479400                                   SPAR-VKORDNTO-TOT                      
479500                                   SPAR-VKORDNTO-DEL                      
479600                                   SPAR-VLORDNTO                          
479700                                   SPAR-VLORDNTO-TOT                      
479800                                   SPAR-VLORDNTO-DEL                      
479900                                   SPAR-KVKOLLI                           
480000                                   SPAR-KVKOLPAC                          
480100                                   SPAR-KVKOLLI-FAKT                      
480200                                   SPAR-SUORDV                            
480300                                   SPAR-SUORDV-EXP                        
480400                                   SPAR-SUORDV-LOC                        
480500                                   SPAR-SUORDV-LOCPREL                    
480600                                   SPAR-SUORDV-TOT                        
480700                                   SPAR-SUORDV-TOT-LOC                    
480800                                   SPAR-SUORDV-TOT-LOCPREL                
480900                                   SPAR-SUORDV-DEL                        
481000                                   SPAR-SUORDV-DEL-LOC                    
481100                                   SPAR-SUORDV-DEL-LOCPREL                
481200                                   SPAR-IDPRODNR                          
481300                                   LOGG-IDLOGLOP                          
481400                                   SPAR-PRAD-KVFLAMP                      
481500                                   SPAR-PRAD-KDFARLIG                     
481600                                   SPAR-PRAD-PRARTNTO                     
481700                                   SPAR-PRAD-PRAVCOST                     
481800                                   SPAR-PRAD-PRARTNTO-LOC                 
481900                                   SPAR-PRAD-PRARTNTO-LOCPREL             
482000                                   SPAR-PRAD-VKARTNTO                     
482100                                   SPAR-PRAD-KVLEVART                     
482200                                   SPAR-IDPSN                             
482300                                   SPAR-VKART-FG                          
482400                                   SPAR-VLFG                              
482500                                   SPAR-SUEQFG                            
482600                                   TOTAL-SUEQFG                           
482700                                   WS-TOT-ANT-RADER                       
482800                                   WS-RINT-ANT-FPACK-ORAD                 
482900                                   WS-KDORDKL                             
483000                                   WS-IDORDER                             
483100                                   WS-KDRAPRIO                            
483200                                   WS-KDORDBEK                            
483300                                   WS-SUMMA                               
483400                                   WS-EMBPROF                             
483500                                   WS-KDFAKTYP                            
483600                                   WS-FLLSBOK                             
483700                                   WS-FLORDSPE                            
483800                                   WS-FLOVRLEV                            
483900                                   WS-IDPURAD                             
484000                                   WS-START-RAD                           
484100                                   WS-SISTA-RAD                           
484200                                   WS-KDTPOTYP                            
484300                                   WS-KDFRAKT                             
484400                                   WS-VKORDBTO                            
484500                                   WS-VLORDBTO                            
484600                                   WS-KDEMBTYP                            
484700                                   WS-DIKOLLIL                            
484800                                   WS-DIKOLLIB                            
484900                                   WS-DIKOLLIH                            
485000                                   WS-ADFLOMR                             
485100                                   WS-ADRUTNIV                            
485200                                   WS-KVSLATTAT                           
485300                                   SPAR-KART-KVRESS-ART                   
485400                                   WS-KVPTID-MIN                          
485500                                   WS-KVPTID-TIM                          
485600                                   ANTAL-EJ-PACKRAP-ORDDEL                
485700                                   WS-DARFS                               
485800                                   WS-KDORDSTA                            
485900                                   WS-KVORDRAD-PACK                       
486000                                   WS-KVORDRAD                            
486100                                   WS-KVORAPP-TOTAL                       
486200                                   WS-KVORAPP-PACK                        
486300                                   WS-KVPRERO                             
486400                                   WS-AVVIKELSE-UTSKR                     
486500                                   WS-TIDISPIN                            
486600                                   WS-TTMMSS                              
486700                                   WS-HH                                  
486800                                   WS-IDORDNR5-OLD                        
486900                                   WS-IDORDNR7-NEW                        
487000                                   WS-SUPTID-TIM                          
487100                                   WS-SUPTID-MIN                          
487200                                   WS-HHMMSS                              
487300                                   WS-DD                                  
487400                                   ARB-KOLLI-VKORDNTO                     
487500                                   ARB-KOLLI-KVFLAMP                      
487600                                   ARB-KOLLI-KDFARLIG                     
487700                                   ARB-KOLLI-KVORDRAD                     
487800                                   ARB-KOLLI-KVFALRAD                     
487900                                   ARB-KOLLI-SUORDV                       
488000                                   ARB-KOLLI-SUORDV-EXP                   
488100                                   ARB-KOLLI-SUORDV-LOC                   
488200                                   ARB-KOLLI-SUORDV-LOCPREL               
488300                                   ARB-ANTAL-KOLLI-PLUS-1                 
488400                                   ARB-ANTAL-KOLLI                        
488500                                                                          
488600     MOVE SPACE                 TO WS-IDORDNR                             
488700                                   WS-ALLA-ODEL-UTSKRIVNA                 
488800                                   WS-SAMMANSLAGNING-RAD                  
488900                                   SW-TIRODAT-LIKA-MED-ZERO               
489000                                   WS-ADFLGEO                             
489100                                   WS-TRAEFF-PACKARE                      
489200                                   FL-420-SEGMENT                         
489300                                   WS-FLAUTFAK                            
489400                                   WS-INDATA-TEST                         
489500                                   WS-BEHANDLING-TEST                     
489600                                   SPAR-BEKUNDRF                          
489700     .                                                                    
489800     EJECT                                                                
489900 S22-SKAPA-SALDOLOGG SECTION.                                             
490000                                                                          
490100     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
490200     MOVE 9                         TO LOGG-IDSEKVNR                      
490300     MOVE AVSP-IDDC                 TO LOGG-IDDC                          
490400     MOVE 'OUTB'                    TO LOGG-IDHUVTYP                      
490500     MOVE 'PAC'                     TO LOGG-IDSUBTYP                      
490600     MOVE 'W403AVSP'                TO LOGG-IDPGM                         
490700     MOVE WS-IDTRANS                TO LOGG-IDTRANS                       
490800     MOVE AVSP-IDANSTNR             TO LOGG-IDUSER                        
490900     MOVE SPACE                     TO LOGG-REF                           
491000     MOVE AVSP-IDDISTR              TO LOGG-IDDISTR                       
491100     MOVE WS-IDPRODNR               TO LOGG-IDPRODNR                      
491200     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
491300     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
491400     MOVE '-'                       TO LOGG-IDTECKEN-KVEFRS               
491500     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
491600     MOVE WS-KVORAPP-PACK           TO LOGG-KVART-SALDO                   
491700     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                                
491800                          CLAG-KVAKS-T                                    
491900                                                                          
492000     MOVE CLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
492100     MOVE CLAG-KVEFRS               TO LOGG-KVEFRS                        
492200     MOVE CLAG-KVLS                 TO LOGG-KVLS                          
492300     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
492400     MOVE FUNCTION CURRENT-DATE(1:8) TO W-LOGG-DATUM                      
492500     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - W-LOGG-DATUM              
492600     ACCEPT W-LOGG-TID FROM TIME                                          
492700     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - W-LOGG-TID                 
492800                                                                          
492900     PERFORM IMS-ISRT-WDL9                                                
493000     IF SEGMENT-FINNS-REDAN                                               
493100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
493200          ADD -1 TO LOGG-IDSEKVNR                                         
493300          PERFORM IMS-ISRT-WDL9                                           
493400       END-PERFORM                                                        
493500     END-IF                                                               
493600     .                                                                    
493700     EJECT                                                                
493800                                                                          
493900 S23-SKAPA-SALDOLOGG SECTION.                                             
494000                                                                          
494100     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
494200     MOVE 9                         TO LOGG-IDSEKVNR                      
494300     MOVE AVSP-IDDC                 TO LOGG-IDDC                          
494400     MOVE 'OUTB'                    TO LOGG-IDHUVTYP                      
494500     MOVE 'PAC'                     TO LOGG-IDSUBTYP                      
494600     MOVE 'W403AVSP'                TO LOGG-IDPGM                         
494700     MOVE WS-IDTRANS                TO LOGG-IDTRANS                       
494800     MOVE AVSP-IDANSTNR             TO LOGG-IDUSER                        
494900     MOVE SPACE                     TO LOGG-REF                           
495000     MOVE AVSP-IDDISTR              TO LOGG-IDDISTR                       
495100     MOVE WS-IDPRODNR               TO LOGG-IDPRODNR                      
495200     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
495300     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
495400     MOVE '-'                       TO LOGG-IDTECKEN-KVEFRS               
495500     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
495600     MOVE WS-KVORAPP-PACK           TO LOGG-KVART-SALDO                   
495700     MOVE SLAG-KVAKS-SDC            TO LOGG-KVAKS                         
495800     MOVE SLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
495900     MOVE SLAG-KVEFRS               TO LOGG-KVEFRS                        
496000     MOVE SLAG-KVLS                 TO LOGG-KVLS                          
496100     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
496200     MOVE FUNCTION CURRENT-DATE(1:8) TO W-LOGG-DATUM                      
496300     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - W-LOGG-DATUM              
496400     ACCEPT W-LOGG-TID FROM TIME                                          
496500     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - W-LOGG-TID                 
496600                                                                          
496700     PERFORM IMS-ISRT-WDL9                                                
496800     IF SEGMENT-FINNS-REDAN                                               
496900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
497000          ADD -1 TO LOGG-IDSEKVNR                                         
497100          PERFORM IMS-ISRT-WDL9                                           
497200       END-PERFORM                                                        
497300     END-IF                                                               
497400     .                                                                    
497500     EJECT                                                                
497600                                                                          
497700                                                                          
497800 S24-PACK-UPPG-DEL-NOTE SECTION.                                          
497900                                                                          
498000     INITIALIZE DNOT-ORDER-INFO                                           
498100                                                                          
498200     MOVE IDPGM                    TO DNOT-IDPGM                          
498300     MOVE WS-SPAR-IDORDER          TO DNOT-IDORDER                        
498400     MOVE WS-SPAR-IDARTNR          TO DNOT-IDARTNR                        
498500     MOVE WS-SPAR-IDDC             TO DNOT-IDDC                           
498600     MOVE WS-SPAR-BEART            TO DNOT-BEART-USA                      
498700     MOVE WS-SPAR-KVBEART          TO DNOT-KVBEART                        
498800     MOVE WS-SPAR-FLTILLK          TO DNOT-FLTILLK                        
498900     MOVE WS-SPAR-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO                    
499000     MOVE WS-SPAR-IDPURAD          TO DNOT-IDPURAD                        
499100     MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                        
499200     MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                       
499300     MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                       
499400                                                                          
499500     CALL W411DNOT USING DNOT-W411DNOT                                    
499600                         DNOT-ORQP-PCB                                    
499700                         DNOT-ORQP2-PCB                                   
499800                         DNOT-ORQP3-PCB                                   
499900                         DNOT-4013-PCB                                    
500000                         DNOT-BENA-PCB                                    
500100     .                                                                    
500200     EJECT                                                                
500300 S25-DELETE-PRICE-Q-LINE SECTION.                                         
500400                                                                          
500500     IF DIST79-DEALER-PRICE                                               
500600       IF SPAR-ORAD-IDPRQUES > ZERO                                       
500700         INITIALIZE PRQU-W335PRQU                                         
500800         MOVE WS-IDDISTR-NUM          TO PRQU-IDDISTR                     
500900         MOVE WS-IDKUNDNR-NUM         TO PRQU-IDKUNDNR                    
501000         MOVE WS-IDKUNDRF-NEW         TO PRQU-IDKUNDRF                    
501100         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
501200         MOVE 4                       TO PRQU-KDCALL                      
501300         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
501400                                            PRQU-WDC7-PCB                 
501500                                            PRQU-SJKO-WDK6-PCB            
501600       END-IF                                                             
501700     END-IF                                                               
501800     .                                                                    
501900     EJECT                                                                
502000 S36-ANDRA-WDC711 SECTION.                                                
502100                                                                          
502200     IF DIST79-DEALER-PRICE AND WS-IDDISTR-NUM = 0778                     
502300       AND RAD-KDORDKL < 3                                                
502400*        FLYTTA PRARTNTO-LOC TILL PRARTNTO-LOCPREL OSV                    
502500*        PÅ WDA5                                                          
502600       IF SPAR-ORAD-IDPRQUES > ZERO                                       
502700         INITIALIZE PRQU-W335PRQU                                         
502800         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
502900         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
503000         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
503100         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
503200         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
503300         MOVE 6                       TO PRQU-KDCALL                      
503400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
503500                                            PRQU-WDC7-PCB                 
503600                                            PRQU-SJKO-WDK6-PCB            
503700         MOVE 'N'                     TO RAD-FLPRTILL                     
503800         IF RAD-PRARTNTO-LOC > +0                                         
503900           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
504000           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
504100         END-IF                                                           
504200         IF PRQU-KDCALL = -1                                              
504300           PERFORM S36-NY-FRAGA                                           
504400         END-IF                                                           
504500       ELSE                                                               
504600*        SKAPA NY PRISFRÅGA                                               
504700         PERFORM S36-NY-FRAGA                                             
504800       END-IF                                                             
504900     END-IF                                                               
505000     .                                                                    
505100     EJECT                                                                
505200 S36-NY-FRAGA  SECTION.                                                   
505300                                                                          
505400     MOVE ZERO                     TO WS-IDPRQUES                         
505500     IF WS-IDPRQUES                = +0                                   
505600        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
505700        MOVE +1                    TO PRNO-KDCALL                         
505800                                                                          
505900        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
506000                                                                          
506100        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
506200                                      WS-IDPRQUES                         
506300        MOVE +1                    TO PRQU-KDCALL                         
506400     END-IF                                                               
506500     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
506600     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
506700     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
506800     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
506900     MOVE ZERO                     TO PRQU-IDORDER                        
507000     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
507100     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
507200       AND RAD-DARODAT > 0                                                
507300       MOVE 4                      TO PRQU-KDORDKL                        
507400     END-IF                                                               
507500     MOVE 'Q'                      TO PRQU-KDPRSTA                        
507600     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
507700     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
507800     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
507900     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
508000     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
508100     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
508200                                                                          
508300     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
508400                                         PRQU-WDC7-PCB                    
508500                                         PRQU-SJKO-WDK6-PCB               
508600                                                                          
508700     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
508800                                       WS-IDPRQUES                        
508900     MOVE 'N'                      TO  RAD-FLPRTILL                       
509000                                                                          
509100     IF RAD-PRARTNTO-LOC = +0                                             
509200        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
509300                       RAD-PRARTNTO-LOCPREL                               
509400     ELSE                                                                 
509500        MOVE RAD-PRARTNTO-LOC  TO RAD-PRARTNTO-LOCPREL                    
509600        MOVE ZERO              TO RAD-PRARTNTO-LOC                        
509700     END-IF                                                               
509800                                                                          
509900     MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                     
510000     MOVE +3                      TO PRNO-KDCALL                          
510100                                                                          
510200     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
510300     .                                                                    
510400     EJECT                                                                
510500*IMS SEKTIONER                                                            
510600*                                                                         
510700*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
510800*                 III     III MM MMMMM MM SSSS   SSSS                     
510900*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
511000*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
511100*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
511200*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
511300*                 III     III MM MMMMM MM SSSS   SSSS                     
511400*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
511500*                                                                         
511600*                                                                         
511700                                                                          
511800 IMS-PURG-4342           SECTION.                                         
511900*                                                                         
512000     MOVE LOW-VALUE TO 4342-Z1 4342-Z2                                    
512100     MOVE SPACE TO GODK-STATUSKODER                                       
512200     CALL CBLTDLI USING ISRT 4342-PCB 4342-IO-AREA                        
512300     MOVE 4342-STATUS-CODE TO STATUS-WS                                   
512400     PERFORM IMS-STATUSKONTROLL                                           
512500     .                                                                    
512600     SKIP2                                                                
512700 IMS-PURG-2191           SECTION.                                         
512800*                                                                         
512900     MOVE LOW-VALUE TO 2191-Z1 2191-Z2                                    
513000     MOVE '  '  TO GODK-STATUSKODER                                       
513100     CALL CBLTDLI USING PURG 2191-PCB 2191-IO-AREA                        
513200     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
513300     PERFORM IMS-STATUSKONTROLL                                           
513400     .                                                                    
513500     SKIP2                                                                
513600 IMS-INSERT-TRANS4349 SECTION.                                            
513700                                                                          
513800     MOVE LOW-VALUE            TO 4349-Z1                                 
513900                                  4349-Z2                                 
514000     MOVE SPACE                TO GODK-STATUSKODER                        
514100     CALL CBLTDLI USING ISRT 4349-PCB 4349-MSG-IO-AREA                    
514200     MOVE 4349-STATUS-CODE   TO STATUS-WS                                 
514300     PERFORM IMS-STATUSKONTROLL                                           
514400     .                                                                    
514500     EJECT                                                                
514600 IMS-REPL-WDE401    SECTION.                                              
514700     MOVE '  '   TO GODK-STATUSKODER                                      
514800     CALL CBLTDLI USING REPL WDE41-PCB KORD-WDE401                        
514900     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
515000     PERFORM IMS-STATUSKONTROLL                                           
515100     SKIP2                                                                
515200     .                                                                    
515300 IMS-GU-WDE401              SECTION.                                      
515400     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
515500          DELIMITED BY SIZE INTO SSA1                                     
515600     MOVE '    ' TO GODK-STATUSKODER                                      
515700     CALL CBLTDLI USING GU    WDE4-PCB DLI-IO-WDE401 SSA1                 
515800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
515900     PERFORM IMS-STATUSKONTROLL                                           
516000     SKIP3                                                                
516100     .                                                                    
516200 IMS-GHU-WDE401             SECTION.                                      
516300     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
516400          DELIMITED BY SIZE INTO SSA1                                     
516500     MOVE '    ' TO GODK-STATUSKODER                                      
516600     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-WDE401 SSA1                
516700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
516800     PERFORM IMS-STATUSKONTROLL                                           
516900     SKIP3                                                                
517000     .                                                                    
517100 IMS-GHNP-WDE411            SECTION.                                      
517200     STRING 'WDE401  *D(WDE401KY =' W-WDE401-KUNDORDER-X ')'              
517300          DELIMITED BY SIZE INTO SSA1                                     
517400     MOVE 'WDE411 '            TO SSA2                                    
517500     MOVE '  GE' TO GODK-STATUSKODER                                      
517600     CALL CBLTDLI USING GHNP   WDE4-PCB DLI-IO-WDE40111 SSA1 SSA2         
517700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
517800     PERFORM IMS-STATUSKONTROLL                                           
517900     .                                                                    
518000 IMS-GU-WDE401-SEQ  SECTION.                                              
518100                                                                          
518200     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
518300            DELIMITED BY SIZE INTO SSA1                                   
518400     MOVE '  GEGB'              TO GODK-STATUSKODER                       
518500     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-WDE401 SSA1                   
518600     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
518700     PERFORM IMS-STATUSKONTROLL                                           
518800     .                                                                    
518900     SKIP3                                                                
519000 IMS-GN-WDE401-SEQ  SECTION.                                              
519100                                                                          
519200     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
519300            DELIMITED BY SIZE INTO SSA1                                   
519400     MOVE '  GEGB'              TO GODK-STATUSKODER                       
519500     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE401 SSA1                   
519600     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
519700     PERFORM IMS-STATUSKONTROLL                                           
519800     .                                                                    
519900 IMS-REPL-WDE411 SECTION.                                                 
520000     MOVE '    ' TO GODK-STATUSKODER                                      
520100     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE40111                     
520200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
520300     PERFORM IMS-STATUSKONTROLL                                           
520400     SKIP3                                                                
520500     .                                                                    
520600 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
520700     STRING 'WDE421  *F(WDE401KY =' W-WDE421-IDKOLLI-X ')'                
520800            DELIMITED BY SIZE INTO SSA1                                   
520900     MOVE '  ' TO GODK-STATUSKODER                                        
521000     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1                  
521100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
521200     PERFORM IMS-STATUSKONTROLL                                           
521300     SKIP3                                                                
521400     .                                                                    
521500 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
521600     MOVE '  '   TO GODK-STATUSKODER                                      
521700     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE421                       
521800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
521900     PERFORM IMS-STATUSKONTROLL                                           
522000     SKIP2                                                                
522100     .                                                                    
522200 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
522300     MOVE   'WDE421 '         TO   SSA1                                   
522400     MOVE '  II' TO GODK-STATUSKODER                                      
522500     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1                  
522600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
522700     PERFORM IMS-STATUSKONTROLL                                           
522800     .                                                                    
522900     EJECT                                                                
523000 IMS-GN-WDE401-ESEQ SECTION.                                              
523100     STRING 'WDE401  (WDE4ESEQ =' W-WDE601-IDPRODNR-X ')'                 
523200            DELIMITED BY SIZE INTO SSA1                                   
523300     MOVE '  GE' TO GODK-STATUSKODER                                      
523400     CALL CBLTDLI USING GN  WDE4E-PCB DLI-IO-WDE401 SSA1                  
523500     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
523600     PERFORM IMS-STATUSKONTROLL                                           
523700     SKIP2                                                                
523800     .                                                                    
523900 IMS-GHU-WDE601           SECTION.                                        
524000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
524100            DELIMITED BY SIZE INTO SSA1                                   
524200     MOVE '    ' TO GODK-STATUSKODER                                      
524300     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1                 
524400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
524500     PERFORM IMS-STATUSKONTROLL                                           
524600     .                                                                    
524700     EJECT                                                                
524800 IMS-REPL-KOLLIREG SECTION.                                               
524900     MOVE '    ' TO GODK-STATUSKODER                                      
525000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
525100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
525200     PERFORM IMS-STATUSKONTROLL                                           
525300     SKIP3                                                                
525400     .                                                                    
525500 IMS-GHU-KOLLI    SECTION.                                                
525600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
525700            DELIMITED BY SIZE INTO SSA1                                   
525800     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
525900            DELIMITED BY SIZE INTO SSA2                                   
526000     MOVE '  GE' TO GODK-STATUSKODER                                      
526100     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1 SSA2            
526200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
526300     PERFORM IMS-STATUSKONTROLL                                           
526400     SKIP3                                                                
526500     .                                                                    
526600 IMS-GHNP-KOLLI    SECTION.                                               
526700     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
526800            DELIMITED BY SIZE INTO SSA1                                   
526900     MOVE '  GE' TO GODK-STATUSKODER                                      
527000     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-AREA2 SSA1                   
527100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
527200     PERFORM IMS-STATUSKONTROLL                                           
527300     SKIP3                                                                
527400     .                                                                    
527500 IMS-REPL-KOLLI    SECTION.                                               
527600     MOVE '    ' TO GODK-STATUSKODER                                      
527700     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
527800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
527900     PERFORM IMS-STATUSKONTROLL                                           
528000     SKIP3                                                                
528100     .                                                                    
528200 IMS-ISRT-KOLLI   SECTION.                                                
528300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
528400            DELIMITED BY SIZE INTO SSA1                                   
528500     MOVE   'WDE611 '         TO   SSA2                                   
528600     MOVE '  II' TO GODK-STATUSKODER                                      
528700     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-AREA2 SSA1 SSA2              
528800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
528900     PERFORM IMS-STATUSKONTROLL                                           
529000     .                                                                    
529100     EJECT                                                                
529200 IMS-ISRT-WDE621 SECTION.                                                 
529300                                                                          
529400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
529500          DELIMITED BY SIZE INTO SSA1                                     
529600     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
529700          DELIMITED BY SIZE INTO SSA2                                     
529800     MOVE 'WDE621 ' TO SSA3                                               
529900     MOVE '  II'              TO GODK-STATUSKODER                         
530000     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
530100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
530200     PERFORM IMS-STATUSKONTROLL                                           
530300     .                                                                    
530400     EJECT                                                                
530500 IMS-GHU-KUNDORDER SECTION.                                               
530600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
530700            DELIMITED BY SIZE INTO SSA1                                   
530800     MOVE '    ' TO GODK-STATUSKODER                                      
530900     CALL CBLTDLI USING GHU    WDE41-PCB KORD-WDE401 SSA1                 
531000     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
531100     PERFORM IMS-STATUSKONTROLL                                           
531200     .                                                                    
531300     EJECT                                                                
531400 IMS-GU-WDE6E   SECTION.                                                  
531500     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-MIN-X                        
531600                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X ')'                    
531700            DELIMITED BY SIZE INTO SSA1                                   
531800     MOVE '  GE' TO GODK-STATUSKODER                                      
531900     CALL CBLTDLI USING GU    WDE6E-PCB SEQE-WDE6E1 SSA1                  
532000     MOVE WDE6E-STATUS-CODE TO STATUS-WS                                  
532100     PERFORM IMS-STATUSKONTROLL                                           
532200     .                                                                    
532300     EJECT                                                                
532400 IMS-GU-4726-ROT-KVAL SECTION.                                            
532500     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
532600            DELIMITED BY SIZE INTO SSA1                                   
532700     MOVE '  ' TO GODK-STATUSKODER                                        
532800     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-AREA3 SSA1                 
532900     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
533000     PERFORM IMS-STATUSKONTROLL                                           
533100     SKIP2                                                                
533200     .                                                                    
533300 IMS-GNP-4726-UNDERSEG-KVAL SECTION.                                      
533400     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
533500            DELIMITED BY SIZE INTO SSA1                                   
533600     MOVE '  GE' TO GODK-STATUSKODER                                      
533700     CALL CBLTDLI USING GNP    XXDV-PCB DLI-IO-AREA3 SSA1                 
533800     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
533900     PERFORM IMS-STATUSKONTROLL                                           
534000     SKIP2                                                                
534100     .                                                                    
534200 IMS-INSERT-4726-UNDERSEG SECTION.                                        
534300     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
534400            DELIMITED BY SIZE INTO SSA1                                   
534500     MOVE 'WLXXDV11 ' TO SSA2                                             
534600     MOVE '  ' TO GODK-STATUSKODER                                        
534700     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3 SSA1 SSA2              
534800     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
534900     PERFORM IMS-STATUSKONTROLL                                           
535000     SKIP2                                                                
535100     .                                                                    
535200 IMS-INSERT-4727 SECTION.                                                 
535300     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
535400            DELIMITED BY SIZE INTO SSA1                                   
535500     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
535600            DELIMITED BY SIZE INTO SSA2                                   
535700     MOVE 'WLXXDV21 ' TO SSA3                                             
535800     MOVE '  II' TO GODK-STATUSKODER                                      
535900     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3                        
536000                               SSA1 SSA2 SSA3                             
536100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
536200     PERFORM IMS-STATUSKONTROLL                                           
536300     .                                                                    
536400     EJECT                                                                
536500 IMS-ISRT-4322-SEGM SECTION.                                              
536600     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
536700            DELIMITED BY SIZE INTO SSA1                                   
536800     MOVE 'WLXXJK11*L' TO SSA2                                            
536900     MOVE '  ' TO GODK-STATUSKODER                                        
537000     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
537100     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
537200     PERFORM IMS-STATUSKONTROLL                                           
537300     .                                                                    
537400     EJECT                                                                
537500                                                                          
537600 IMS-GU-WDM211 SECTION.                                                   
537700                                                                          
537800     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
537900          DELIMITED BY SIZE INTO SSA1                                     
538000     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
538100          DELIMITED BY SIZE INTO SSA2                                     
538200     MOVE '  GE'              TO GODK-STATUSKODER                         
538300     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
538400     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
538500     PERFORM IMS-STATUSKONTROLL                                           
538600     .                                                                    
538700                                                                          
538800 IMS-GNP-WDM221 SECTION.                                                  
538900                                                                          
539000     MOVE 'WDM221 '           TO SSA1                                     
539100     MOVE '    GE'            TO GODK-STATUSKODER                         
539200     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
539300     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
539400     PERFORM IMS-STATUSKONTROLL                                           
539500     .                                                                    
539600                                                                          
539700 IMS-GHU-WDM211 SECTION.                                                  
539800                                                                          
539900     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
540000          DELIMITED BY SIZE INTO SSA1                                     
540100     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
540200          DELIMITED BY SIZE INTO SSA2                                     
540300     MOVE '  GE'              TO GODK-STATUSKODER                         
540400     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
540500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
540600     PERFORM IMS-STATUSKONTROLL                                           
540700     .                                                                    
540800                                                                          
540900 IMS-REPL-WDM211 SECTION.                                                 
541000                                                                          
541100     MOVE '  '             TO GODK-STATUSKODER                            
541200     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
541300     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
541400     PERFORM IMS-STATUSKONTROLL                                           
541500     .                                                                    
541600                                                                          
541700                                                                          
541800 IMS-GHU-WDM221 SECTION.                                                  
541900                                                                          
542000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
542100          DELIMITED BY SIZE INTO SSA1                                     
542200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
542300          DELIMITED BY SIZE INTO SSA2                                     
542400     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
542500          DELIMITED BY SIZE INTO SSA3                                     
542600     MOVE '  GE' TO GODK-STATUSKODER                                      
542700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
542800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
542900     PERFORM IMS-STATUSKONTROLL                                           
543000     .                                                                    
543100                                                                          
543200 IMS-REPL-WDM221 SECTION.                                                 
543300                                                                          
543400     MOVE '  '             TO GODK-STATUSKODER                            
543500     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
543600     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
543700     PERFORM IMS-STATUSKONTROLL                                           
543800     .                                                                    
543900     EJECT                                                                
544000 IMS-GU-ORQA01    SECTION.                                                
544100     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
544200            DELIMITED BY SIZE INTO SSA1                                   
544300     MOVE '  ' TO GODK-STATUSKODER                                        
544400     CALL CBLTDLI USING GU    ORQA-PCB DLI-IO-Q301 SSA1                   
544500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
544600     PERFORM IMS-STATUSKONTROLL                                           
544700     .                                                                    
544800     EJECT                                                                
544900 IMS-GU-ORQA-STATUS    SECTION.                                           
545000                                                                          
545100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
545200                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
545300                    '&KDODELST =' W-KDODELST     ')'                      
545400          DELIMITED BY SIZE INTO SSA1                                     
545500     MOVE '  GE' TO GODK-STATUSKODER                                      
545600     CALL CBLTDLI USING GU  ORQA-PCB DLI-IO-Q301 SSA1                     
545700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
545800     PERFORM IMS-STATUSKONTROLL                                           
545900     .                                                                    
546000 IMS-GU-ORQA-ODEL SECTION.                                                
546100                                                                          
546200     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
546300                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
546400            DELIMITED BY SIZE INTO SSA1                                   
546500     MOVE '  GE' TO GODK-STATUSKODER                                      
546600     CALL CBLTDLI USING GU ORQA2-PCB DLI-IO-Q301  SSA1                    
546700     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
546800     PERFORM IMS-STATUSKONTROLL                                           
546900     .                                                                    
547000     SKIP3                                                                
547100 IMS-GN-ORQA-ODEL SECTION.                                                
547200                                                                          
547300     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
547400                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
547500            DELIMITED BY SIZE INTO SSA1                                   
547600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
547700     CALL CBLTDLI USING GN ORQA2-PCB DLI-IO-Q301  SSA1                    
547800     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
547900     PERFORM IMS-STATUSKONTROLL                                           
548000     .                                                                    
548100     EJECT                                                                
548200 IMS-REPL-ORQA01        SECTION.                                          
548300     MOVE '    ' TO GODK-STATUSKODER                                      
548400     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-Q301                         
548500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
548600     PERFORM IMS-STATUSKONTROLL                                           
548700     .                                                                    
548800     EJECT                                                                
548900 IMS-GHU-ORQA01   SECTION.                                                
549000     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
549100            DELIMITED BY SIZE INTO SSA1                                   
549200     MOVE '  ' TO GODK-STATUSKODER                                        
549300     CALL CBLTDLI USING GHU    ORQA-PCB DLI-IO-Q301 SSA1                  
549400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
549500     PERFORM IMS-STATUSKONTROLL                                           
549600     .                                                                    
549700 IMS-GU-ORQM01 SECTION.                                                   
549800                                                                          
549900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
550000                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
550100          DELIMITED BY SIZE INTO SSA1                                     
550200     MOVE '  GE'               TO GODK-STATUSKODER                        
550300     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-Q101 SSA1                    
550400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
550500     PERFORM IMS-STATUSKONTROLL                                           
550600     .                                                                    
550700     SKIP2                                                                
550800 IMS-ISRT-ORQM01     SECTION.                                             
550900     MOVE 'WLORQM01 ' TO SSA1                                             
551000     MOVE '  '   TO GODK-STATUSKODER                                      
551100     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-Q101 SSA1                    
551200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
551300     PERFORM IMS-STATUSKONTROLL                                           
551400     .                                                                    
551500     EJECT                                                                
551600 IMS-GHU-ORQI01    SECTION.                                               
551700     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
551800            DELIMITED BY SIZE INTO SSA1                                   
551900     MOVE '    ' TO GODK-STATUSKODER                                      
552000     CALL CBLTDLI USING GHU   ORQI-PCB OHUV-WDQ201 SSA1                   
552100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
552200     PERFORM IMS-STATUSKONTROLL                                           
552300     .                                                                    
552400 IMS-GU-ORQI01    SECTION.                                                
552500     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
552600            DELIMITED BY SIZE INTO SSA1                                   
552700     MOVE '    ' TO GODK-STATUSKODER                                      
552800     CALL CBLTDLI USING GU   ORQI-PCB OHUV-WDQ201 SSA1                    
552900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
553000     PERFORM IMS-STATUSKONTROLL                                           
553100     .                                                                    
553200 IMS-GHNP-ORQI12    SECTION.                                              
553300     STRING 'WLORQI12*F(IDDC     =' W-IDDC-Q2-X ')'                       
553400            DELIMITED BY SIZE INTO SSA1                                   
553500     MOVE '    ' TO GODK-STATUSKODER                                      
553600     CALL CBLTDLI USING GHNP   ORQI-PCB ARB-WDQ212 SSA1                   
553700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
553800     PERFORM IMS-STATUSKONTROLL                                           
553900     .                                                                    
554000 IMS-REPL-ORQI12      SECTION.                                            
554100                                                                          
554200     MOVE '    ' TO GODK-STATUSKODER                                      
554300     CALL CBLTDLI USING REPL ORQI-PCB  ARB-WDQ212                         
554400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
554500     PERFORM IMS-STATUSKONTROLL                                           
554600     .                                                                    
554700 IMS-GHU-ORQI01-GE SECTION.                                               
554800     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
554900            DELIMITED BY SIZE INTO SSA1                                   
555000     MOVE '  GE' TO GODK-STATUSKODER                                      
555100     CALL CBLTDLI USING GHU   ORQI-PCB OHUV-WDQ201 SSA1                   
555200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
555300     PERFORM IMS-STATUSKONTROLL                                           
555400     .                                                                    
555500 IMS-GU-ORQI01-CSEQ-ORQL SECTION.                                         
555600                                                                          
555700     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2C-X ')'                          
555800             DELIMITED BY SIZE INTO    SSA1                               
555900     MOVE    '  GE'              TO    GODK-STATUSKODER                   
556000     CALL    CBLTDLI             USING GU   ORQL-PCB                      
556100                                            OHUV-WDQ201 SSA1              
556200     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
556300     PERFORM IMS-STATUSKONTROLL                                           
556400     .                                                                    
556500 IMS-GU-ORQI01-CSEQ SECTION.                                              
556600                                                                          
556700     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
556800             DELIMITED BY SIZE INTO SSA1                                  
556900     MOVE    '  GE'              TO GODK-STATUSKODER                      
557000     CALL    CBLTDLI USING       GU ORQICSQ-PCB OHUV-WDQ201               
557100                                    SSA1                                  
557200     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
557300     PERFORM IMS-STATUSKONTROLL                                           
557400     .                                                                    
557500     EJECT                                                                
557600 IMS-GHU-XXKW11       SECTION.                                            
557610                                                                          
557700     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
557800            DELIMITED BY SIZE INTO SSA1                                   
557900     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
558000            DELIMITED BY SIZE INTO SSA2                                   
558100     MOVE '  GE' TO GODK-STATUSKODER                                      
558200     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
558300     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
558400     PERFORM IMS-STATUSKONTROLL                                           
558500     .                                                                    
558600 IMS-REPL-XXKW11    SECTION.                                              
558700     MOVE '  '   TO GODK-STATUSKODER                                      
558800     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
558900     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
559000     PERFORM IMS-STATUSKONTROLL                                           
559100     .                                                                    
559200     EJECT                                                                
559300 IMS-GU-XXLB         SECTION.                                             
559400     STRING 'WLXXLB01(WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
559500            DELIMITED BY SIZE INTO SSA1                                   
559600     STRING 'WLXXLB11(WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
559700            DELIMITED BY SIZE INTO SSA2                                   
559800     MOVE '  GE' TO GODK-STATUSKODER                                      
559900     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA6 SSA1 SSA2             
560000     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
560100     PERFORM IMS-STATUSKONTROLL                                           
560200     .                                                                    
560300     EJECT                                                                
560400 IMS-ISRT-ZZAC01 SECTION.                                                 
560500     MOVE 'WLZZAC01' TO SSA1                                              
560600     MOVE '  II'     TO GODK-STATUSKODER                                  
560700     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
560800     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
560900     PERFORM IMS-STATUSKONTROLL                                           
561000     .                                                                    
561100     EJECT                                                                
561200                                                                          
561300 IMS-GHU-ORDP01   SECTION.                                                
561400                                                                          
561500     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
561600            DELIMITED BY SIZE INTO SSA1                                   
561700     MOVE '  '     TO GODK-STATUSKODER                                    
561800     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
561900     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
562000     PERFORM IMS-STATUSKONTROLL                                           
562100     SKIP3                                                                
562200     .                                                                    
562300 IMS-GHU-ORDP01-GE           SECTION.                                     
562400                                                                          
562500     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
562600            DELIMITED BY SIZE INTO SSA1                                   
562700     MOVE '  GE'   TO GODK-STATUSKODER                                    
562800     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
562900     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
563000     PERFORM IMS-STATUSKONTROLL                                           
563100     SKIP3                                                                
563200     .                                                                    
563300 IMS-REPL-ORDP01  SECTION.                                                
563400                                                                          
563500     MOVE '  '     TO GODK-STATUSKODER                                    
563600     CALL CBLTDLI USING REPL ORDP1-PCB DLI-IO-AREA6                       
563700     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
563800     PERFORM IMS-STATUSKONTROLL                                           
563900     SKIP3                                                                
564000     .                                                                    
564100 IMS-ISRT-ORDP01  SECTION.                                                
564200                                                                          
564300     MOVE 'WLORDP01 ' TO SSA1                                             
564400     MOVE '  II'   TO GODK-STATUSKODER                                    
564500     CALL CBLTDLI USING ISRT ORDP1-PCB DLI-IO-AREA6 SSA1                  
564600     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
564700     PERFORM IMS-STATUSKONTROLL                                           
564800     SKIP3                                                                
564900     .                                                                    
565000 IMS-DLET-ORDP01  SECTION.                                                
565100                                                                          
565200     MOVE '  '     TO GODK-STATUSKODER                                    
565300     CALL CBLTDLI USING DLET ORDP1-PCB DLI-IO-AREA6                       
565400     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
565500     PERFORM IMS-STATUSKONTROLL                                           
565600     EJECT                                                                
565700     .                                                                    
565800 IMS-GHU-ORDP01-OLD SECTION.                                              
565900                                                                          
566000     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
566100                    '&WDA501KY=<' W2-WDA501KY-X                           
566200                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
566300            DELIMITED BY SIZE INTO SSA1                                   
566400     MOVE '  GE'   TO GODK-STATUSKODER                                    
566500     CALL CBLTDLI USING GHU ORDP2-PCB DLI-IO-AREA7 SSA1                   
566600     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
566700     PERFORM IMS-STATUSKONTROLL                                           
566800     SKIP3                                                                
566900     .                                                                    
567000 IMS-GHN-ORDP01-OLD SECTION.                                              
567100                                                                          
567200     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
567300                    '&WDA501KY=<' W2-WDA501KY-X                           
567400                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
567500            DELIMITED BY SIZE INTO SSA1                                   
567600     MOVE '  GE'   TO GODK-STATUSKODER                                    
567700     CALL CBLTDLI USING GHN ORDP2-PCB DLI-IO-AREA7 SSA1                   
567800     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
567900     PERFORM IMS-STATUSKONTROLL                                           
568000     SKIP3                                                                
568100     .                                                                    
568200 IMS-REPL-ORDP01-OLD  SECTION.                                            
568300                                                                          
568400     MOVE '  '     TO GODK-STATUSKODER                                    
568500     CALL CBLTDLI USING REPL ORDP2-PCB DLI-IO-AREA7                       
568600     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
568700     PERFORM IMS-STATUSKONTROLL                                           
568800     EJECT                                                                
568900     .                                                                    
569000 IMS-GU-XXJN     SECTION.                                                 
569100                                                                          
569200     STRING 'WLXXJN01(WDGXKEY  =' W-WDGX01 ')'                            
569300            DELIMITED BY SIZE INTO SSA1                                   
569400     STRING 'WLXXJN11(WDGXKEY >=' W-WDGXKEY-N5-MIN                        
569500                    '&WDGXKEY <=' W-WDGXKEY-N5-MAX                        
569600                    '&KDRAPRIO>=' W-KDRAPRIO-N5-MIN-X                     
569700                    '&KDRAPRIO<=' W-KDRAPRIO-N5-MAX-X                     
569800                    '&KDTPOTYP =' W-KDTPOTYP-N5-X                         
569900                    '&KDORDKL  =' W-KDORDKL-N5-X                          
570000                    '&IDDISTRF<=' W-IDDISTR-FOM-N5-X                      
570100                    '&IDDISTRT>=' W-IDDISTR-TOM-N5-X ')'                  
570200            DELIMITED BY SIZE INTO SSA2                                   
570300     MOVE '  ' TO GODK-STATUSKODER                                        
570400     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA4 SSA1 SSA2                
570500     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
570600     PERFORM IMS-STATUSKONTROLL                                           
570700     EJECT                                                                
570800     .                                                                    
570900 IMS-GU-ARTC11 SECTION.                                                   
571000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
571100            DELIMITED BY SIZE INTO SSA1                                   
571200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
571300          DELIMITED BY SIZE INTO SSA2                                     
571400     MOVE '  '     TO GODK-STATUSKODER                                    
571500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2                
571600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
571700     PERFORM IMS-STATUSKONTROLL                                           
571800     SKIP3                                                                
571900     .                                                                    
572000 IMS-GHU-ARTC11     SECTION.                                              
572100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
572200            DELIMITED BY SIZE INTO SSA1                                   
572300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
572400          DELIMITED BY SIZE INTO SSA2                                     
572500     MOVE '  '   TO GODK-STATUSKODER                                      
572600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2               
572700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
572800     PERFORM IMS-STATUSKONTROLL                                           
572900     SKIP2                                                                
573000     .                                                                    
573100 IMS-REPL-ARTC       SECTION.                                             
573200     MOVE '  '   TO GODK-STATUSKODER                                      
573300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA8                        
573400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
573500     PERFORM IMS-STATUSKONTROLL                                           
573600     .                                                                    
573700     SKIP2                                                                
573800 IMS-GHU-WDK711     SECTION.                                              
573900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
574000            DELIMITED BY SIZE INTO SSA1                                   
574100     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
574200            DELIMITED BY SIZE INTO SSA2                                   
574300     MOVE '  GE'   TO GODK-STATUSKODER                                    
574400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA8 SSA1 SSA2               
574500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
574600     PERFORM IMS-STATUSKONTROLL                                           
574700     SKIP2                                                                
574800     .                                                                    
574900 IMS-GU-WDK722     SECTION.                                               
575000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
575100            DELIMITED BY SIZE INTO SSA1                                   
575200     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
575300            DELIMITED BY SIZE INTO SSA2                                   
575400     MOVE 'WDK722 '             TO SSA3                                   
575500     MOVE '  GE'   TO GODK-STATUSKODER                                    
575600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
575700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
575800     PERFORM IMS-STATUSKONTROLL                                           
575900     SKIP2                                                                
576000     .                                                                    
576100 IMS-GU-WDK712 SECTION.                                                   
576200                                                                          
576300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
576400          DELIMITED BY SIZE INTO SSA1                                     
576500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
576600          DELIMITED BY SIZE INTO SSA2                                     
576700     MOVE '  GE' TO GODK-STATUSKODER                                      
576800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
576900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
577000     PERFORM IMS-STATUSKONTROLL                                           
577100     .                                                                    
577200 IMS-REPL-WDK7       SECTION.                                             
577300     MOVE '  '   TO GODK-STATUSKODER                                      
577400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA8                        
577500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
577600     PERFORM IMS-STATUSKONTROLL                                           
577700     .                                                                    
577800     EJECT                                                                
577900 IMS-GHU-WDK901 SECTION.                                                  
578000     STRING 'WLARTM01(IDARTNR  =' W-WDK901-IDARTNR-X ')'                  
578100            DELIMITED BY SIZE INTO SSA1                                   
578200     MOVE '  '     TO GODK-STATUSKODER                                    
578300     CALL CBLTDLI USING GHU ARTM-PCB ART-WDK901 SSA1                      
578400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
578500     PERFORM IMS-STATUSKONTROLL                                           
578600     SKIP3                                                                
578700     .                                                                    
578800 IMS-REPL-WDK901 SECTION.                                                 
578900     MOVE '  '     TO GODK-STATUSKODER                                    
579000     CALL CBLTDLI USING REPL ARTM-PCB ART-WDK901                          
579100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
579200     PERFORM IMS-STATUSKONTROLL                                           
579300     SKIP3                                                                
579400     .                                                                    
579500 IMS-GU-XXKH11 SECTION.                                                   
579600                                                                          
579700     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X    ')'                         
579800            DELIMITED BY SIZE INTO SSA1                                   
579900     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X    ')'                         
580000            DELIMITED BY SIZE INTO SSA2                                   
580100     MOVE '  '   TO GODK-STATUSKODER                                      
580200     CALL CBLTDLI USING GU XXKH-PCB 4448-WDGX4448-CTX SSA1 SSA2           
580300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
580400     PERFORM IMS-STATUSKONTROLL                                           
580500     SKIP2                                                                
580600     .                                                                    
580700     EJECT                                                                
580800 IMS-ISRT-AUTFAKTURA-ROT SECTION.                                         
580900     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
581000            DELIMITED BY SIZE INTO SSA1                                   
581100     MOVE   'WLXXDV11 '         TO SSA2                                   
581200     MOVE '  '     TO GODK-STATUSKODER                                    
581300     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3 SSA1 SSA2              
581400     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
581500     PERFORM IMS-STATUSKONTROLL                                           
581600     SKIP2                                                                
581700     .                                                                    
581800 IMS-ISRT-AUTFAKTURA     SECTION.                                         
581900     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
582000            DELIMITED BY SIZE INTO SSA1                                   
582100     STRING 'WLXXDV11(WDGXKEY  =' W-WDGX11-WDGXKEY-X ')'                  
582200            DELIMITED BY SIZE INTO SSA2                                   
582300     MOVE 'WLXXDV21 ' TO SSA3                                             
582400     MOVE '  IIGE' TO GODK-STATUSKODER                                    
582500     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3                        
582600                                        SSA1 SSA2 SSA3                    
582700     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
582800     PERFORM IMS-STATUSKONTROLL                                           
582900     EJECT                                                                
583000     .                                                                    
583100 IMS-ISRT-4542 SECTION.                                                   
583200     STRING 'WL454101(WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
583300         DELIMITED BY SIZE INTO SSA1                                      
583400     MOVE 'WL454111 ' TO SSA2                                             
583500     MOVE '  II' TO GODK-STATUSKODER                                      
583600     CALL CBLTDLI USING ISRT 4541-PCB 4542-WDGX4542 SSA1 SSA2             
583700     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
583800     PERFORM IMS-STATUSKONTROLL                                           
583900     .                                                                    
584000     SKIP2                                                                
584100 IMS-GHU-WDGX4490 SECTION.                                                
584200     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
584300         DELIMITED BY SIZE INTO SSA1                                      
584400     STRING 'WDGX4488(KDPRCGRP =' W-KDPRCGRP-X ')'                        
584500         DELIMITED BY SIZE INTO SSA2                                      
584600     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
584700         DELIMITED BY SIZE INTO SSA3                                      
584800     MOVE '  GE' TO GODK-STATUSKODER                                      
584900     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-WDGX4490                      
585000                        SSA1 SSA2 SSA3                                    
585100     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
585200     PERFORM IMS-STATUSKONTROLL                                           
585300     .                                                                    
585400     SKIP2                                                                
585500 IMS-DLET-WDGX4490 SECTION.                                               
585600     MOVE '  ' TO GODK-STATUSKODER                                        
585700     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-WDGX4490                     
585800     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
585900     PERFORM IMS-STATUSKONTROLL                                           
586000     .                                                                    
586100     SKIP2                                                                
586200 IMS-ISRT-KLAR-SV4   SECTION.                                             
586300     MOVE 'WLZZAC01'  TO SSA1                                             
586400     MOVE '  II' TO GODK-STATUSKODER                                      
586500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
586600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
586700     PERFORM IMS-STATUSKONTROLL                                           
586800     SKIP2                                                                
586900     .                                                                    
587000 IMS-ISRT-AVVIKELSE  SECTION.                                             
587100     MOVE 'WLZZAC01'  TO SSA1                                             
587200     MOVE '  II' TO GODK-STATUSKODER                                      
587300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
587400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
587500     PERFORM IMS-STATUSKONTROLL                                           
587600     SKIP2                                                                
587700     .                                                                    
587800 IMS-ISRT-WDL9 SECTION.                                                   
587900     MOVE 'WLLOGA01 ' TO SSA1                                             
588000     MOVE '  II' TO GODK-STATUSKODER                                      
588100     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
588200     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
588300     PERFORM IMS-STATUSKONTROLL                                           
588400     .                                                                    
588500     SKIP2                                                                
588600 IMS-GU-WDK601                 SECTION.                                   
588700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
588800            DELIMITED BY SIZE INTO SSA1                                   
588900     MOVE '  '                   TO GODK-STATUSKODER                      
589000     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-AREA8  SSA1                
589100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
589200     PERFORM IMS-STATUSKONTROLL                                           
589300     .                                                                    
589400     SKIP2                                                                
589500 IMS-GHNP-WDK611               SECTION.                                   
589600     MOVE 'WDK611  '           TO SSA1                                    
589700     MOVE '  '                 TO GODK-STATUSKODER                        
589800     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-AREA8  SSA1                
589900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
590000     PERFORM IMS-STATUSKONTROLL                                           
590100     .                                                                    
590200     SKIP2                                                                
590300 IMS-REPL-WDK611               SECTION.                                   
590400     MOVE 'WDK611  '           TO SSA1                                    
590500     MOVE '    '               TO GODK-STATUSKODER                        
590600     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-AREA8  SSA1                
590700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
590800     PERFORM IMS-STATUSKONTROLL                                           
590900     .                                                                    
591000     SKIP2                                                                
591100 IMS-GHU-SEQB-WDA601            SECTION.                                  
591200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
591300                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
591400            DELIMITED BY SIZE INTO SSA1                                   
591500     MOVE '  GE'                 TO GODK-STATUSKODER                      
591600     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA11 SSA1              
591700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
591800     PERFORM IMS-STATUSKONTROLL                                           
591900     .                                                                    
592000     SKIP2                                                                
592100 IMS-GHN-SEQB-WDA601            SECTION.                                  
592200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
592300                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
592400            DELIMITED BY SIZE INTO SSA1                                   
592500     MOVE '  GEGB'               TO GODK-STATUSKODER                      
592600     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA11 SSA1              
592700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
592800     PERFORM IMS-STATUSKONTROLL                                           
592900     .                                                                    
593000     SKIP2                                                                
593100 IMS-REPL-SEQB-WDA601                 SECTION.                            
593200     MOVE 'WDA601  '           TO SSA1                                    
593300     MOVE '    '               TO GODK-STATUSKODER                        
593400     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA11 SSA1               
593500     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
593600     PERFORM IMS-STATUSKONTROLL                                           
593700     .                                                                    
593800     EJECT                                                                
593900 IMS-ISRT-WDA601            SECTION.                                      
594000     MOVE   'WDA601  '         TO SSA1                                    
594100     MOVE '  IINI' TO GODK-STATUSKODER                                    
594200     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA11 SSA1                
594300     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
594400     PERFORM IMS-STATUSKONTROLL                                           
594500     .                                                                    
594600     SKIP3                                                                
594700 IMS-GU-WDB601    SECTION.                                                
594800                                                                          
594900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
595000     DELIMITED BY SIZE INTO SSA1                                          
595100     MOVE '  GE' TO GODK-STATUSKODER                                      
595200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
595300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
595400     PERFORM IMS-STATUSKONTROLL                                           
595500     IF SEGMENT-SAKNAS                                                    
595600        MOVE SPACE TO DCS-KDDC                                            
595700     END-IF                                                               
595800     .                                                                    
595900                                                                          
596000 IMS-GU-WDP4A1 SECTION.                                                   
596100                                                                          
596200     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
596300                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
596400          DELIMITED BY SIZE INTO SSA1                                     
596500     MOVE '  GE' TO GODK-STATUSKODER                                      
596600     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
596700                           SSA1                                           
596800     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
596900     PERFORM IMS-STATUSKONTROLL                                           
597000     .                                                                    
597100                                                                          
597200     EJECT                                                                
597300 DB2-SELECT-TP4TRAN     SECTION.                                          
597400     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
597500                                                                          
597600     MOVE 000100 TO GODK-SQLCODEKODER                                     
597700                                                                          
597800     EXEC SQL                                                             
597900           SELECT  DISTINCT                                               
598000                   IDDC_REC                                               
598100                                                                          
598200           INTO   :TP4TRAN-IDDC-REC                                       
598300                                                                          
598400           FROM    TP4TRAN                                                
598500                                                                          
598600           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
598700     END-EXEC                                                             
598800                                                                          
598900     MOVE SQLCODE TO SQLCODE-WS                                           
599000     PERFORM DB2-STATUSKONTROLL                                           
599100     .                                                                    
599200     EJECT                                                                
599300 IMS-STATUSKONTROLL SECTION.                                              
599400     SET STATUS-IX TO 1                                                   
599500     SEARCH GODK-STATUS AT END CALL FELLOG                                
599600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
599700     END-SEARCH                                                           
599800     CONTINUE                                                             
599900     .                                                                    
600000 DB2-STATUSKONTROLL  SECTION.                                             
600100                                                                          
600200     SET SQLCODE-IX TO 1                                                  
600300     SEARCH GODK-SQLCODE                                                  
600400       AT END                                                             
600500          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
600600          DELIMITED BY SIZE INTO FELTEXT                                  
600700          CALL ABEND USING RKOD-ABEND-DB2                                 
600800       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
600900     END-SEARCH                                                           
601000     .                                                                    
