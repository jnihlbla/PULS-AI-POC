000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL019700.                                                
000400 AUTHOR.         ANDERSSON BERT.                                          
000500 DATE-WRITTEN.   08/01/16.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CASE REPORTING1 FOR WEB LDC PACK REPORTING OF ORDERPARTS.        
001000*        WL019700 PROGRAM IS A REPLICA OF W4031300 PROGRAM                
001100*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001200*        PACKNINGSRAPPORTERING AV ORDERDELAR.                             
001300*                                                                         
001400*        THE PROGRAM UPDATES   WDG2                                       
001500*        THE PROGRAM UPDATES   WDE4                                       
001600*        THE PROGRAM UPDATES   WDE6                                       
001700*        THE PROGRAM UPDATES   WDQ2                                       
001800*        THE PROGRAM UPDATES   WDQ3                                       
001900*        THE PROGRAM UPDATES   WDR1                                       
002000*        THE PROGRAM UPDATES   WDR4                                       
002100*                                                                         
002200*        KDPGMACT = 'E' EXECUTE                                           
002300*        KDPGMACT = 'S' SEARCH                                            
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: WLT197                                              
002700*        REQU:        WLI19701                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        RESP:        WLO19701                                            
003100*                                                                         
003200* CHANGE LOG:                                                             
003300*    2015-04-22  ETRACKER 10130993                                        
003400*                REDUCE NUMBER OF DELIVERY SCHEDULES                      
003500*                                                                         
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800                                                                          
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'WL019700'.            
004300                                                                          
004400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004500 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
004600 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004700 77  FILLER                      PIC X(08)   VALUE 'CURRENT'.             
004800 77  WS-CURRENT-SECTION          PIC X(64)   VALUE SPACE.                 
004900 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC'.             
005000 77  WS-CURRENT-IMS-SECTION      PIC X(64)   VALUE SPACE.                 
005100 77  WS-DB2-SECTION              PIC X(08)   VALUE 'DB2SECT:'.            
005200 77  WS-DB2-SEKTION              PIC X(64)   VALUE SPACE.                 
005300                                                                          
005400*    --- WORK FIELDS                                                      
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  YES                         PIC X       VALUE 'Y'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  DEF-IDROLL                  PIC X(5)    VALUE 'VOR99'.               
005900 77  DATUM-SW                    PIC X       VALUE 'N'.                   
006000 77  RAETT                       PIC X       VALUE 'R'.                   
006100 77  FEL                         PIC X       VALUE 'F'.                   
006200 77  SAKNAS                      PIC X       VALUE 'S'.                   
006300 77  W-IDDC                      PIC X(2)    VALUE SPACE.                 
006400 77  WS-MID-IDDC                 PIC X(2)    VALUE SPACE.                 
006500 77  WS-MID-IDLEVNR              PIC X(5)    VALUE SPACE.                 
006600 77  WS-MAX-500-RADER            PIC 9(5)    VALUE 500.                   
006700 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
006800 77  WS-ABSTRACT-ADRESS          PIC X(50)                                
006900            VALUE 'CARPARTS.LDC.CASEREPORTING3'.                          
007000                                                                          
007100 77 WS-ORAD-VLORDNTO             PIC  9(8)   VALUE ZERO.                  
007200 77 WS-ORAD-VLORDNTO-SUM         PIC  9(8)   VALUE ZERO.                  
007300 77 WS-KOLLI-VLORDBTO            PIC  9(8)   VALUE ZERO.                  
007400                                                                          
007500 77    INDX                      PIC S9(9)   VALUE +0   COMP-3.           
007600 77    RADIX                     PIC S9(9)   VALUE +0   COMP-3.           
007700 77    TMS-IX                    PIC S9(9)   VALUE +0   COMP-3.           
007800 77    RENSA-IND                 PIC S9(9)   VALUE +0   COMP-3.           
007900 77    RADIND                    PIC S9(9)   VALUE +0   COMP-3.           
008000 77    RADIND-9                  PIC  9(9)   VALUE  0.                    
008100 77    RADIND-NUM9               PIC S9(9)   VALUE +0   COMP-3.           
008200 77    JMF-IND                   PIC S9(9)   VALUE +0   COMP-3.           
008300 77    MAX-RADINDX               PIC S9(9)   VALUE +50  COMP-3.           
008400 77    WS-KVRADER                PIC 9(5).                                
008500 77    WS-REQU-KVRADER           PIC 9(5).                                
008600 77  FILLER                      PIC X(08)  VALUE 'A2A2A2A2'.             
008700 77    IND1                      PIC S9(9)  VALUE +0   COMP-3.            
008800 77    IND2                      PIC S9(9)  VALUE +0   COMP-3.            
008900 77    INDX                      PIC S9(9)  VALUE +0   COMP-3.            
009000 77    CNT                       PIC S9(9)  VALUE +0   COMP-3.            
009100 77    REST-INX                  PIC S9(9)  VALUE +0   COMP-3.            
009200 77    FG-INDX                   PIC S9(9)  VALUE +0   COMP-3.            
009300 77    FG-MAX-INDX               PIC S9(9)  VALUE +10  COMP-3.            
009400 77    GN-WDE6C1-RAKNARE         PIC  9(9)  VALUE  0.                     
009500 77    REBOOT-PROGRAM-IX         PIC S9(9)  VALUE +0   COMP-3.            
009600 77    RESTART-COUNT-MAX         PIC S9(9)  VALUE +300 COMP-3.            
009700 77    FILLER                    PIC  X(8)  VALUE 'SUBKVDLE'.             
009800 77    WS-SUB-KVDLEN             PIC  9(9)  VALUE 0.                      
009900 77    WS-IDLEVNR                PIC X(5)   VALUE SPACES.                 
010000 77    WS-ODEL-IDDC-EXP          PIC X(2)   VALUE SPACES.                 
010100*                                                                         
010200 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
010300*                                                                         
010400 01 W-RESP-AREA.                                                          
010500   03 W-RESP-RAD           OCCURS 500 TIMES.                              
010600     07 W-RESP-IDPRODNR    PIC 9(7).                                      
010700     07 W-RESP-IDKOLLI     PIC 9(5).                                      
010800*                                                                         
010900 01     FILLER                  PIC X(10)   VALUE 'SPAR-PRAD-'.           
011000 01     SPAR-PRAD.                                                        
011100*                                                                         
011200   03   SPAR-PRAD-UPPG-AREA.                                              
011300     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
011400     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
011500     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
011600     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
011700     05 SPAR-PRAD-PRARTNTO-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.          
011800     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
011900     05 SPAR-PRAD-VKARTNTO      PIC  S9(4)V9(3)   VALUE ZERO.             
012000     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
012100*                                                                         
012200   03   SPAR-FARLIGT-GODS-DATA.                                           
012300     05 SPAR-IDPSN              PIC  9(3)                VALUE 0.         
012400     05 SPAR-VKART-FG           PIC  S9(7)        COMP-3 VALUE 0.         
012500     05 SPAR-VLFG               PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
012600     05 SPAR-SUEQFG             PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
012700     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
012800*                                                                         
012900 77  FILLER                      PIC X(08)   VALUE 'CCCCCCCC'.            
013000 77    WS-ANT-RADER-ORDER        PIC S9(3)   VALUE +0    COMP-3.          
013100 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
013200 77    WS-IDORDER                PIC S9(7)  COMP-3.                       
013300 77    WS-KDRAPRIO               PIC S9(3)  COMP-3.                       
013400 77    WS-KDORDBEK               PIC 9(2).                                
013500 77    WS-SUMMA                  PIC S9(7)   VALUE +0.                    
013600 77    WS-ODEL-IDUSER            PIC X(8).                                
013700 77    WS-ODEL-IDTRP             PIC X(05)  VALUE SPACE.                  
013800 77    WS-ODEL-VLORDNTO          PIC S9(4)V9(3) VALUE +0 COMP-3.          
013900 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
014000*                                                                         
014100 77    W-SPAR-IDDC               PIC X(2).                                
014200 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
014300 77    WS-EMBPROF                PIC X(1)   VALUE SPACE.                  
014400 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
014500 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
014600 77    W-LOGG-DATUM              PIC S9(8)  VALUE ZERO.                   
014700 77    W-LOGG-TID                PIC S9(9)  VALUE ZERO.                   
014800 77    W-KVKOLLI                 PIC S9(7)  VALUE ZERO  COMP-3.           
014900 77    W-KVKOLLI-FAKT            PIC S9(7)  VALUE ZERO  COMP-3.           
015000 77    W-KVKOLLI-LAST            PIC S9(7)  VALUE ZERO  COMP-3.           
015100 77    W-KVKOLLI-FL              PIC S9(7)  VALUE ZERO  COMP-3.           
015200 77    S03-IDROLL                PIC X(5)    VALUE SPACE.                 
015300 77    S28-IDARTNR               PIC 9(9)    VALUE ZERO COMP-3.           
015400 77    S28-KVVORKO               PIC S9(7)   VALUE ZERO COMP-3.           
015500 77    S28-IDANSK                PIC 9(3)    VALUE ZERO COMP-3.           
015600 77    S28-IDLEVNR               PIC X(5)    VALUE SPACE.                 
015700 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
015800                                                                          
015900*01   -COPY WWDCKONS                                                      
016000                                                                          
016100*01   -COPY WWDC04                                                        
016200                                                                          
016300 77    WS-VOR-TID-BRIST          PIC 9(9)   VALUE ZERO.                   
016400 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
016500 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
016600 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
016700 77    WS-KORD-IDDISTR           PIC 9(5)   VALUE ZERO.                   
016800 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
016900 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
017000 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
017100 77    WS-IDDISTR-NUM4           PIC 9(4)   VALUE ZERO.                   
017200 77    WS-SAVE-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
017300 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
017400 77    WS-IDPLKLST-NUM           PIC  9(3)  VALUE ZERO.                   
017500 77    WS-REQU-IDLOTNR           PIC  9(3)  VALUE ZERO.                   
017600 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
017700 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
017800 77    WS-IDKOLLI-SAMP           PIC 9(5)   VALUE ZERO.                   
017900 77    WS-SAVE-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
018000 77    WS-TIORDREG               PIC S9(7) VALUE ZERO COMP-3.             
018100 77    WS-TIORDREG-NUM6          PIC 9(6)  VALUE ZERO.                    
018200 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
018300 77    WS-FLLSBOK                PIC X(1)   VALUE SPACE.                  
018400 77    WS-ODEL-IDPRC             PIC X(4)   VALUE SPACE.                  
018500 77    WS-FLORDSPE               PIC X(1)   VALUE SPACE.                  
018600 77    WS-FLOVRLEV               PIC X(1)   VALUE SPACE.                  
018700 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
018800 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
018900 77    WS-IDPRODNR-RED           PIC Z(6)9  VALUE ZERO.                   
019000 77    WS-PREV-IDPRODNR          PIC X(7)   VALUE SPACE.                  
019100 77    WS-IDPURAD                PIC 9(4)   VALUE ZERO.                   
019200 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
019300 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
019400 77    WS-KDKOLLI                PIC X(8)   VALUE SPACE.                  
019500 77    FILLER                    PIC X(8)   VALUE 'FFFFFFFF'.             
019600 77    WS-KDTPOTYP               PIC S9     COMP-3.                       
019700 77    WS-KDFRAKT                PIC  9(3)  VALUE ZERO.                   
019800 77    WS-KDFRAKT-NUM2           PIC  9(2)  VALUE ZERO.                   
019900 77    WS-VKORDBTO               PIC 9(6)V9 VALUE ZERO.                   
020000 77    WS-KOLLI-VKORDBTO-KOLLI   PIC 9(6)V9 VALUE ZERO.                   
020100 77    WS-VLORDBTO               PIC 9(4)V9(3)  VALUE ZERO.               
020200 77    WS-KDEMBTYP               PIC 9(2)   VALUE ZERO.                   
020300 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
020400 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
020500 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
020600 77    WS-KDKOLLID               PIC X(1)   VALUE 'L'.                    
020700 77    WS-ADFLGEO                PIC X(3)   VALUE SPACE.                  
020800 77    WS-ADFLOMR                PIC 9(3)   VALUE ZERO.                   
020900 77    WS-ADRUTNIV               PIC 9(3)   VALUE ZERO.                   
021000 77    WS-KVSLATTAT              PIC S9(7)  COMP-3  VALUE ZERO.           
021100 77    SW-TIRODAT-LIKA-MED-ZERO  PIC  X(1)   VALUE 'N'.                   
021200 77    SW-SET-RESP-KVRADER       PIC  X(1)   VALUE 'N'.                   
021300 77    SW-L0199                  PIC  X(1)   VALUE 'N'.                   
021400 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
021500 77    SPAR-KART-KVRESS-ART      PIC S9(7)  COMP-3.                       
021600 77    WS-TRAEFF-PACKARE         PIC X(01).                               
021700 77    MAX-RAD-ANTAL-PLUS-1      PIC S9(3)  VALUE +13  COMP-3.            
021800 77    WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.            
021900 77    WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.            
022000 77    FILLER                    PIC X(8)    VALUE 'HHHHHHHH'.            
022100 77    WS-SAMMANSLAGNING-RAD     PIC X.                                   
022200 77    ANTAL-EJ-PACKRAP-ORDDEL   PIC 9(1)   VALUE ZERO.                   
022300 77    WS-DARFS                  PIC 9(12)  VALUE ZERO.                   
022400*                                        ANTAL FÄRDIGPACKADE RADER        
022500*                                        I ETT RAD-INTERVALL.             
022600 77    WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.            
022700 77    FILLER                    PIC X(8)    VALUE 'IIIIIIII'.            
022800 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
022900 77    WS-KVORDRAD-PACK          PIC S9(5)   VALUE +0    COMP-3.          
023000 77    WS-KVORDRAD               PIC S9(5)   VALUE +0    COMP-3.          
023100 77    WS-KVORAPP-TOTAL          PIC S9(6)   VALUE +0.                    
023200 77    WS-KVORAPP-PACK           PIC S9(6)   VALUE +0.                    
023300 77    WS-KVPRERO                PIC S9(7)   VALUE +0.                    
023400 77    WS-AVVIKELSE-UTSKR        PIC S9(7)   VALUE +0.                    
023500 77    WS-TIDISPIN               PIC S9(7)  COMP-3.                       
023600 77    WS-ALLA-ODEL-UTSKRIVNA    PIC X(1).                                
023700 77    WS-FLAUTFAK               PIC X(01).                               
023800 77    FILLER                    PIC X(8)    VALUE 'I2I2I2I2'.            
023900 77    KDRC-DISPLAY              PIC 9(4)    VALUE ZERO.                  
024000 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
024100 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
024200 77    WS-IDPRQUES               PIC 9(7)    VALUE ZERO.                  
024300 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
024400 77    WS-VORD-IDLOTNR           PIC  9(3)   VALUE ZERO.                  
024500 77  FILWS-DARFS-SEKE            PIC X(08)   VALUE 'JJJJJJJJ'.            
024600 77    SPAR-VORD-IDPRODNR        PIC S9(7)   VALUE ZERO COMP-3.           
024700*RFS                                                                      
024800 01    WS-DARFS-SOEK.                                                     
024900   03  WS-DARFS-SEKEL            PIC 9(02)  VALUE ZERO.                   
025000   03  WS-REQU-TIRFSDAT          PIC 9(06)  VALUE ZERO.                   
025100   03  WS-REQU-TIRFSTID          PIC 9(04)  VALUE ZERO.                   
025200*                                                                         
025300*DATRPAVT                                                                 
025400 01    WS-DATRP-SOEK.                                                     
025500   03  WS-DATRP-SEKEL            PIC 9(02)  VALUE ZERO.                   
025600   03  WS-REQU-TITRPDAT          PIC 9(06)  VALUE ZERO.                   
025700   03  WS-REQU-TITRPTID          PIC 9(04)  VALUE ZERO.                   
025800*                                                                         
025900*                                                                         
026000 01   WS-TRP-GRP.                                                         
026100   03 WS-TRP-SEKEL             PIC 9(02)  VALUE ZERO.                     
026200   03 WS-TRP-YEAR              PIC 9(02)  VALUE ZERO.                     
026300   03 WS-TRP-MONTH             PIC 9(02)  VALUE ZERO.                     
026400   03 WS-TRP-DAY               PIC 9(02)  VALUE ZERO.                     
026500   03 WS-TRP-HOUR              PIC 9(02)  VALUE ZERO.                     
026600   03 WS-TRP-MIN               PIC 9(02)  VALUE ZERO.                     
026700*                                                                         
026800 01    WS-DATRP-GRP.                                                      
026900   03  WS-DATRP-SEKEL            PIC 9(02)  VALUE ZERO.                   
027000   03  WS-DATRP-DATE.                                                     
027100     05 WS-DATRP-YEAR            PIC 9(02)  VALUE ZERO.                   
027200     05 FILLER                   PIC X(01)  VALUE '-'.                    
027300     05 WS-DATRP-MONTH           PIC 9(02)  VALUE ZERO.                   
027400     05 FILLER                   PIC X(01)  VALUE '-'.                    
027500     05 WS-DATRP-DAY             PIC 9(02)  VALUE ZERO.                   
027600     05 FILLER                   PIC X(01)  VALUE 'T'.                    
027700   03 WS-DATRP-TIME.                                                      
027800     05 WS-DATRP-HOUR            PIC 9(02)  VALUE ZERO.                   
027900     05 FILLER                   PIC X(01)  VALUE ':'.                    
028000     05 WS-DATRP-MIN             PIC 9(02)  VALUE ZERO.                   
028100     05 FILLER                   PIC X(01)  VALUE ':'.                    
028200     05 WS-DATRP-SEC             PIC 9(02)  VALUE ZERO.                   
028300     05 FILLER                   PIC X(01)  VALUE ':'.                    
028400     05 WS-DATRP-HUN             PIC 9(03)  VALUE ZERO.                   
028500     05 FILLER                   PIC X(01)  VALUE 'Z'.                    
028600*                                                                         
028700 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
028800 01     ARBETSAREOR.                                                      
028900   03   ARB-KOLLI-UPPG-AREA.                                              
029000     05 ARB-KOLLI-VKORDNTO      PIC  9(6)V9(1)    VALUE ZERO.             
029100     05 ARB-KOLLI-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
029200     05 ARB-KOLLI-KDFARLIG      PIC  S9           VALUE ZERO.             
029300     05 ARB-KOLLI-KVORDRAD      PIC  S9(5)        VALUE ZERO.             
029400     05 ARB-KOLLI-KVFALRAD      PIC  S9(5)        VALUE ZERO.             
029500     05 ARB-KOLLI-SUORDV        PIC  S9(9)V9(2)   VALUE ZERO.             
029600     05 ARB-KOLLI-SUORDV-EXP    PIC  S9(9)V9(2)   VALUE ZERO.             
029700     05 ARB-KOLLI-SUORDV-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
029800     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
029900     05 ARB-KOLLI-KDVALISO      PIC X(3)          VALUE SPACE.            
030000     05 ARB-KOLLI-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
030100     SKIP2                                                                
030200   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5) VALUE ZERO.                     
030300   03   ARB-ANTAL-KOLLI          PIC 9(5) VALUE ZERO.                     
030400     SKIP2                                                                
030500   03   ARB-ADRESS.                                                       
030600     05 ARB-ADFLGEO             PIC  X(3)   VALUE SPACE.                  
030700     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
030800     05 ARB-ADFLOMR             PIC  9(3)   VALUE ZERO.                   
030900     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
031000     05 ARB-ADRUTNIV            PIC  9(3)   VALUE ZERO.                   
031100     SKIP2                                                                
031200 01     WS-TIDPUNKT-RED.                                                  
031300   03   WS-HHMMSS               PIC  9(6).                                
031400   03   WS-DD                   PIC  9(2).                                
031500                                                                          
031600*                                                                         
031700 01    FILLER                   PIC X(20)  VALUE 'SPAR-ORAD-AREA'.        
031800*01      WDE411   -COPY WDE411  -PRE SPAR-.                               
031900*                                                                         
032000     EJECT                                                                
032100 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
032200 01     FILLER REDEFINES TEST-IDDISTR.                                    
032300*  03   -COPY WWDIST03.                                                   
032400     SKIP2                                                                
032500 01     FILLER REDEFINES TEST-IDDISTR.                                    
032600*  03   -COPY WWDIST07.                                                   
032700     SKIP2                                                                
032800 01     FILLER REDEFINES TEST-IDDISTR.                                    
032900*  03   -COPY WWDIST18.                                                   
033000     SKIP2                                                                
033100 01     FILLER REDEFINES TEST-IDDISTR.                                    
033200*  03   -COPY WWDIST19.                                                   
033300     SKIP2                                                                
033400 01     FILLER REDEFINES TEST-IDDISTR.                                    
033500*  03   -COPY WWDIST20.                                                   
033600     SKIP2                                                                
033700 01     FILLER REDEFINES TEST-IDDISTR.                                    
033800*  03   -COPY WWDIST21.                                                   
033900     SKIP2                                                                
034000 01     FILLER REDEFINES TEST-IDDISTR.                                    
034100*  03   -COPY WWDIST35.                                                   
034200     EJECT                                                                
034300 01     FILLER REDEFINES TEST-IDDISTR.                                    
034400*  03   -COPY WWDIST47.                                                   
034500     EJECT                                                                
034600 01     FILLER REDEFINES TEST-IDDISTR.                                    
034700*  03   -COPY WWDIS128.                                                   
034800     EJECT                                                                
034900 01     FILLER REDEFINES TEST-IDDISTR.                                    
035000*  03   -COPY WWDIST79.                                                   
035100     EJECT                                                                
035200 01     FILLER REDEFINES TEST-IDDISTR.                                    
035300*  03   -COPY WWDIST85.                                                   
035400     EJECT                                                                
035500 01     FILLER REDEFINES TEST-IDDISTR.                                    
035600*  03   -COPY WWDIST44.                                                   
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
035900*   -COPY WWDIST57                                                        
036000     EJECT                                                                
036100 01  FILLER                      PIC X(08)  VALUE 'TESTKUND'.             
036200 01  FILLER                      PIC X(08)  VALUE 'WWFRAKT1'.             
036300*01     FILLER  -COPY WWFRAKT1                                            
036400     EJECT                                                                
036500 01     DAGDAT.                                                           
036600   03   DAGDAT-AAMMD             PIC 9(6).                                
036700   03   DAGDAT-AAMMDD-X      REDEFINES DAGDAT-AAMMD.                      
036800     05 DAGDAT-AAMMDD-AA         PIC 9(2).                                
036900     05 DAGDAT-AAMMDD-MM         PIC 9(2).                                
037000     05 DAGDAT-AAMMDD-DD         PIC 9(2).                                
037100*                                                                         
037200   03   DAGDAT-AAVVD             PIC 9(5).                                
037300   03   DAGDAT-AAVVD-X       REDEFINES DAGDAT-AAVVD.                      
037400     05 DAGDAT-AAVVD-AA          PIC 9(2).                                
037500     05 DAGDAT-AAVVD-VV          PIC 9(2).                                
037600     05 DAGDAT-AAVVD-D           PIC 9(1).                                
037700*                                                                         
037800*01  WDATAREA      -COPY WDATAREA.                                        
037900     EJECT                                                                
038000*                                                                         
038100 77  FILLER                      PIC X(08)   VALUE 'CCCCCCCC'.            
038200 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
038300 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
038400*VIKTIGT VIKTIGT                                                          
038500*                                                                         
038600 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
038700                                                                          
038800 01  WS-DCUSER.                                                           
038900     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
039000     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
039100     03 FILLER                   PIC X(1)   VALUE SPACE.                  
039200                                                                          
039300 01    WS-TID-W.                                                          
039400   03  WS-TTMMSS                 PIC 9(6).                                
039500   03  WS-HH                     PIC 9(2).                                
039600 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
039700                                                                          
039800 77    WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
039900 77    WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
040000 77    WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                   
040100 77    WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                  
040200 77    WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.            
040300 77    WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                  
040400 77    WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                  
040500 77    WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
040600                                                                          
040700 01     WS-IDKUNDRF.                                                      
040800   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
040900   03   FILLER                   PIC X(5)   VALUE SPACE.                  
041000                                                                          
041100 01     WS-IDKUNDRF-OLD.                                                  
041200   03   WS-IDORDNR5-OLD          PIC 9(5).                                
041300   03   FILLER                   PIC X(5)   VALUE SPACE.                  
041400                                                                          
041500 01     WS-IDKUNDRF-NEW.                                                  
041600   03   WS-IDORDNR7-NEW          PIC 9(7).                                
041700   03   FILLER                   PIC X(3)   VALUE SPACE.                  
041800                                                                          
041900 01  ARBETSFALT.                                                          
042000                                                                          
042100     03 WS-DAORDREG              PIC 9(8) VALUE ZERO.                     
042200     03 WS-DAORDREG-DELAR        REDEFINES WS-DAORDREG.                   
042300        05 WS-DAORDREG-TISS      PIC 9(2).                                
042400        05 WS-DAORDREG-TIAAMMDD  PIC 9(6).                                
042500                                                                          
042600     03 WS-DARODAT               PIC 9(8) VALUE ZERO.                     
042700     03 WS-DARODAT-DELAR         REDEFINES WS-DARODAT.                    
042800        05 WS-DARODAT-TISS       PIC 9(2).                                
042900        05 WS-DARODAT-TIAAMMDD   PIC 9(6).                                
043000                                                                          
043100     03 WS-9KOMPL-GRUND          PIC 9(9) VALUE 999999999.                
043200                                                                          
043300 77    FILLER                    PIC X(8)    VALUE 'KKKKKKKK'.            
043400                                                                          
043500 01    FL-420-SEGMENT            PIC X(01)  VALUE 'N'.                    
043600   88  ORAPPORTERADE-RADER-FINNS            VALUE 'J'.                    
043700   88  ORAPPORTERADE-RADER-SAKNAS           VALUE 'N'.                    
043800     SKIP2                                                                
043900 01    SW-FLAUTFAK               PIC X(01).                               
044000   88  AUT-FAK-SKRIVS-EJ-UT                 VALUE 'N'.                    
044100   88  AUT-FAKTURA-SKRIVS-UT                VALUE 'J'.                    
044200     SKIP2                                                                
044300 01    KDORDSTA-SW               PIC X(01).                               
044400   88  KDORDSTA-KLAR                        VALUE 'J'.                    
044500   88  KDORDSTA-EJ-KLAR                     VALUE 'N'.                    
044600     SKIP2                                                                
044700*                                                                         
044800 01  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
044900   88 TRAFF-VORKO                            VALUE 'J'.                   
045000*                                                                         
045100 77  SW-RESTART                  PIC  X      VALUE 'N'.                   
045200     88 RESTART                              VALUE 'J'.                   
045300*                                                                         
045400 77  INIT-SW                     PIC X       VALUE 'J'.                   
045500     88  INIT-OK                             VALUE 'J'.                   
045600     88  INIT-WRONG                          VALUE 'N'.                   
045700*                                                                         
045800 77  DUPLICATE-CHECK             PIC X       VALUE 'N'.                   
045900     88  DUP-KOLLI                           VALUE 'J'.                   
046000     88  NO-DUP-KOLLI                        VALUE 'N'.                   
046100*                                                                         
046200 77  RADER-KVAR-SW               PIC X       VALUE 'J'.                   
046300     88  RADER-KVAR                          VALUE 'J'.                   
046400     88  INGA-RADER-KVAR                     VALUE 'N'.                   
046500*                                                                         
046600     EJECT                                                                
046700 77    ABEND-MED1                PIC X(80)  VALUE  SPACE.                 
046800     SKIP2                                                                
046900                                                                          
047000 77    WS-IDTRANS                PIC X(04).                               
047100   88  WS-GODKAND-BILD                      VALUE '0121', '4313'.         
047200                                                                          
047300     SKIP2                                                                
047400 77    WS-INDATA-TEST            PIC X(01).                               
047500   88  WS-INDATA-FEL                        VALUE 'F'.                    
047600   88  WS-INDATA-RATT                       VALUE 'R'.                    
047700     SKIP2                                                                
047800 77    WS-BEHANDLING-TEST        PIC X(01).                               
047900   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
048000   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
048100     SKIP2                                                                
048200 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
048300 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
048400   03   WS-SUPTID-TIM           PIC 9(3).                                 
048500   03   WS-SUPTID-MIN           PIC 9(2).                                 
048600*                                                                         
048700 01  S27-IDANSK-X.                                                        
048800     03 S27-IDANSK               PIC 9(3).                                
048900                                                                          
049000 01  S27-IDARTNR-X.                                                       
049100     03 S27-IDARTNR              PIC 9(9).                                
049200                                                                          
049300 01  S27-IDDISTR-X.                                                       
049400     03 S27-IDDISTR              PIC 9(4).                                
049500                                                                          
049600 01  S27-IDKUNDNR-X.                                                      
049700     03 S27-IDKUNDNR             PIC 9(6).                                
049800                                                                          
049900 01 DB2-LASNING.                                                          
050000     03 FILLER                   PIC X(16)   VALUE                        
050100                                             'WS-DB2-SEKTION'.            
050200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
050300                                                                          
050400     EJECT                                                                
050500 01 NYCKLAR-TP4TRAN.                                                      
050600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
050700                                                                          
050800                                                                          
050900     SKIP2                                                                
051000 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREA '.           
051100 01     SPAR-AREA.                                                        
051200*                                                                         
051300   03   SPAR-AREA-GRP.                                                    
051400     05 SPAR-VKORDNTO           PIC  9(6)V9(1)    VALUE ZERO.             
051500     05 SPAR-VKORDNTO-TOT       PIC  9(6)V9(1)    VALUE ZERO.             
051600     05 SPAR-VKORDNTO-DEL       PIC  9(6)V9(1)    VALUE ZERO.             
051700     05 SPAR-VLORDNTO           PIC  9(4)V9(3)    VALUE ZERO.             
051800     05 SPAR-VLORDNTO-TOT       PIC  9(4)V9(3)    VALUE ZERO.             
051900     05 SPAR-VLORDNTO-DEL       PIC  9(4)V9(3)    VALUE ZERO.             
052000     05 SPAR-KVKOLLI            PIC  S9(5)        VALUE ZERO.             
052100     05 SPAR-KVKOLPAC           PIC  S9(5)        VALUE ZERO.             
052200     05 SPAR-KVKOLLI-FAKT       PIC  S9(5)        VALUE ZERO.             
052300     05 SPAR-SUORDV             PIC  S9(9)V9(2)   VALUE ZERO.             
052400     05 SPAR-SUORDV-LOC         PIC  S9(9)V9(2)   VALUE ZERO.             
052500     05 SPAR-SUORDV-LOCPREL     PIC  S9(9)V9(2)   VALUE ZERO.             
052600     05 SPAR-SUORDV-TOT         PIC  S9(9)V9(2)   VALUE ZERO.             
052700     05 SPAR-SUORDV-TOT-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
052800     05 SPAR-SUORDV-TOT-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
052900     05 SPAR-SUORDV-DEL         PIC  S9(9)V9(2)   VALUE ZERO.             
053000     05 SPAR-SUORDV-DEL-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
053100     05 SPAR-SUORDV-DEL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
053200     05 SPAR-IDPRODNR           PIC S9(7)         VALUE ZERO.             
053300     05 SPAR-BEKUNDRF           PIC X(15)         VALUE SPACE.            
053400     05 SPAR-KDVALISO           PIC X(3)          VALUE SPACE.            
053500     05 SPAR-KDVALISO-EXP       PIC X(3)          VALUE SPACE.            
053600     05 SPAR-FLDIRLEV           PIC X(1)          VALUE SPACE.            
053700                                                                          
053800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
053900     88  KEYS-OK                             VALUE 'J'.                   
054000     88  KEYS-WRONG                          VALUE 'N'.                   
054100*                                                                         
054200 77  IDTRPTNR-TIRFS-SW           PIC X       VALUE 'J'.                   
054300     88  IDTRPTNR-TIRFS-IFYLLD               VALUE 'J'.                   
054400     88  IDTRPTNR-TIRFS-TOM                  VALUE 'N'.                   
054500*                                                                         
054600 77  IDPRC-IDLOTNR-SW            PIC X       VALUE 'J'.                   
054700     88  IDPRC-IDLOTNR-IFYLLD                VALUE 'J'.                   
054800     88  IDPRC-IDLOTNR-TOM                   VALUE 'N'.                   
054900*                                                                         
055000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
055100     88  INDATA-OK                           VALUE 'J'.                   
055200     88  INDATA-FEL                          VALUE 'N'.                   
055300*                                                                         
055400 77  INGEN-INDATA-SW             PIC X       VALUE 'J'.                   
055500     88  INGEN-INDATA-OK                     VALUE 'J'.                   
055600     88  INGEN-INDATA-FEL                    VALUE 'N'.                   
055700                                                                          
055800 77  ORDERDEL-PACKAD-SW          PIC X       VALUE 'N'.                   
055900     88  ORDERDEL-PACKAD                     VALUE 'J'.                   
056000                                                                          
056100 77  INDATA-FINNS-SW             PIC X       VALUE 'J'.                   
056200     88  INDATA-FINNS                        VALUE 'J'.                   
056300     88  INDATA-SAKNAS                       VALUE 'N'.                   
056400                                                                          
056500 77  SW-TIKLAR-UPPDATERAD        PIC X(01).                               
056600*                                                                         
056700***************************************************************           
056800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
056900*                                                                         
057000                                                                          
057100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
057200 01  GENERAL-SUBPROGRAMS.                                                 
057300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
057400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
057500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
057600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
057700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
057800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
057900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
058000     03  WZ01CALL                PIC X(8)    VALUE 'WZ01CALL'.            
058100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
058200     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
058300     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
058400     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
058500     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
058600     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
058700*        PRISFRÅGA                                                        
058800     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
058900*        DATA TILL DEL NOTE NDC                                           
059000     SKIP2                                                                
059100 01    ABENDKODER.                                                        
059200     03 FILLER                  PIC X(16) VALUE 'ABENDKODER'.             
059300     03 RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.            
059400     03 RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +33.            
059500     03 RKOD-FELTEXT            PIC X(32) VALUE SPACE.                    
059600     EJECT                                                                
059700*                                                                         
059800*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
059900*                                                                         
060000 01  FILLER                      PIC X(16) VALUE 'SUB-CONTROL'.           
060100*01  -COPY WZ01SUB                                                        
060200     SKIP3                                                                
060300 01  FILLER                      PIC X(16) VALUE 'SEND-CONTROL'.          
060400*01  -COPY WZ01SEND                                                       
060500     SKIP3                                                                
060600 01  FILLER                      PIC X(08) VALUE 'WL01TIDZ'.              
060700*01  -COPY WL01TIDZ                                                       
060800     EJECT                                                                
060900*                                                                         
061000*    - SEND AREA FOR RESTARTING THIS PROGRAM                              
061100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
061200 01  SEND-AREA.                                                           
061300*    03  -COPY WZ01REQU -PRE SEND-                                        
061400*    03  -COPY WL0197I1 -PRE SEND-                                        
061500     EJECT                                                                
061600*                                                                         
061700 01  FILLER                      PIC X(08)  VALUE 'W411DNOT'.             
061800*01 -COPY W411DNOT                                                        
061900     EJECT                                                                
062000*                                                                         
062100 01  FILLER                      PIC X(8)   VALUE  'W335PRQU'.            
062200*    -COPY W335PRQU                                                       
062300*                                                                         
062400 01  FILLER                      PIC X(8)   VALUE  'W335PRNO'.            
062500*    -COPY W335PRNO                                                       
062600*                                                                         
062700 01  FILLER                      PIC X(08)   VALUE 'WZ01CALL'.            
062800*01  -COPY WZ01CALL                                                       
062900     EJECT                                                                
063000 01    FILLER                    PIC X(08)   VALUE 'DECAREA '.            
063100*01  -COPY WDECAREA                                                       
063200     EJECT                                                                
063300 01    FILLER                    PIC X(08)   VALUE 'WWOMVAND'.            
063400*01  -COPY WWOMVAND                                                       
063500     SKIP3                                                                
063600*01  COPY WMSGINIT                                                        
063700     SKIP3                                                                
063800*TMS PACKNING INFO                                                        
063900 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
064000*    -COPY W403TMS1                                                       
064100*                                                                         
064200                                                                          
064300 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
064400*                                                                         
064500 01  WSTAB-PLATS.                                                         
064600     05 WS-INGANG  OCCURS 100.                                            
064700        10 WSTAB-IDTRPTNR       PIC S9(3)   COMP-3.                       
064800        10 WSTAB-ADFLGEO        PIC X(3).                                 
064900        10 WSTAB-ADFLOMR        PIC S9(3)   COMP-3.                       
065000        10 WSTAB-ADRUTNIV       PIC S9(3)   COMP-3.                       
065100        10 WSTAB-DIHMODUL       PIC S9(3)   COMP-3.                       
065200        10 WSTAB-DIDMODUL       PIC S9(3)   COMP-3.                       
065300        10 WSTAB-ADVMODUL       PIC S9(3)   COMP-3.                       
065400        10 WSTAB-ADHMODUL       PIC S9(3)   COMP-3.                       
065500        10 WSTAB-FLUTLAST       PIC X(1).                                 
065600     EJECT                                                                
065700*                                                                         
065800 01  WS-EMB-INFO.                                                         
065900     03  WS-EMB-DATA  OCCURS 500 TIMES.                                   
066000         05 WS-EMB-KDEMBTYP     PIC S9         COMP-3.                    
066100         05 WS-EMB-VKTARA       PIC S9(6)V9(1) COMP-3.                    
066200         05 WS-EMB-DIKOLLIL     PIC S9(5)      COMP-3.                    
066300         05 WS-EMB-DIKOLLIB     PIC S9(3)      COMP-3.                    
066400         05 WS-EMB-DIKOLLIH     PIC S9(3)      COMP-3.                    
066500         05 WS-OLD-EMB-VKTARA   PIC  9(6)V9(1).                           
066600         05 WS-NEW-EMB-VKTARA   PIC  9(6)V9(1).                           
066700*                                                                         
066800 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
066900 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
067000 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
067100   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
067200   03   FILLER                  PIC  X(4).                                
067300     SKIP2                                                                
067400 01     HJALP-4472-TIRFS        PIC 9(11).                                
067500 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
067600   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
067700   03   FILLER                  PIC  X(4).                                
067800     EJECT                                                                
067900*    --- PARAMETERS TO ABEND                                              
068000                                                                          
068100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
068200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
068300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
068400     EJECT                                                                
068500*                                                                         
068600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
068700     SKIP3                                                                
068800 01  REQU-AREA.                                                           
068900*    03  -COPY WZ01REQU                                                   
069000*    03  -COPY WL0197I1                                                   
069100     EJECT                                                                
069200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
069300     SKIP3                                                                
069400 01  RESP-AREA.                                                           
069500*    03  -COPY WZ01RESP                                                   
069600*    03  -COPY WL0197O1                                                   
069700                                                                          
069800*                                                                         
069900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
070000*                                                                         
070100 01  SAVE-AREA.                                                           
070200     03  SAVE-IDTRANS           PIC X(4)    VALUE 'L197'.                 
070300     SKIP3                                                                
070400 01  MESAGE-CODES.                                                        
070500     03  INF-UPDATED             PIC X(3)    VALUE '001'.                 
070600     03  DATA-ENTER-BUT-NO-PRESSED                                        
070700                                    PIC X(3)    VALUE '013'.              
070800     03  ERR-NO-DATA-ENTERED     PIC X(3)    VALUE '014'.                 
070900     03  ERR-ORDER-NOT-PACKED    PIC X(3)    VALUE '021'.                 
071000     03  INVALID-KEY-FIELDS      PIC X(3)    VALUE '022'.                 
071100     03  IS-INVALID              PIC X(3)    VALUE '023'.                 
071200     03  MUST-BE-NUMERIC         PIC X(3)    VALUE '024'.                 
071300     03  MUST-BE-ENTERED         PIC X(3)    VALUE '026'.                 
071400     03  ERR-WRONG-LINES         PIC X(3)    VALUE '027'.                 
071500     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
071600     03  DUPLICATE-LINES         PIC X(3)    VALUE '029'.                 
071700     03  ERR-ORDER-REGISTRED     PIC X(3)    VALUE '030'.                 
071800     03  ERR-CASE-ALREADY-REPORT PIC X(3)    VALUE '146'.                 
071900     03  ERR-ORDER-PARTS-MISSING PIC X(3)    VALUE '041'.                 
072000     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '041'.                 
072100     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
072200     03  ERR-CASE-REPORT-STARTED PIC X(3)    VALUE '113'.                 
072300     03  NO-REPORTING-FOR-DIRECT-SUPPLI PIC X(3) VALUE '114'.             
072400     03  ERR-ORDER-PART-READY    PIC X(3)    VALUE '116'.                 
072500     03  ERR-LINE-ALREADY-ZEROED PIC X(3)    VALUE '118'.                 
072600     03  ERR-NO-DANGEROUS-CARGO  PIC X(3)    VALUE '119'.                 
072700     03  ERR-DISTRICT-NOT-APPROV PIC X(3)    VALUE '120'.                 
072800     03  ERR-MIXED-CASE-ON-OTHER-TRANS  PIC X(3) VALUE '121'.             
072900     03  ERR-MIXED-CASE-HAS-NO-TRANS    PIC X(3) VALUE '122'.             
073000     03  ERR-ERROR-IN-ADDRESS    PIC X(3)    VALUE '123'.                 
073100     03  ERR-SHOULD-NOT-BE-ZERO  PIC X(3)    VALUE '126'.                 
073200     03  ERR-WRONG-STATUS        PIC X(3)    VALUE '273'.                 
073300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
073400     03  INF-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
073500     03  CLICK-OK-FOR-REPORTING  PIC X(3)    VALUE '329'.                 
073600     03 ERR-ORDER-HAS-WRONG-STATUS  PIC  X(03)  VALUE '273'.              
073700     03 ERR-USESCREEN-L0199         PIC  X(03)  VALUE '190'.              
073800     SKIP3                                                                
073900*                                                                         
074000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
074100*                                                                         
074200******************************************************************        
074300*                                                                         
074400*                                                                         
074500 01  FILLER                      PIC X(16)   VALUE 'DLI-WS'.              
074600     SKIP3                                                                
074700 01  KEYS-TO-DLI.                                                         
074800   03    W-KDSEGKEY-X.                                                    
074900     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
075000*                                                                         
075100     03  W-IDPRODNR-X.                                                    
075200         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
075300*                                                                         
075400     03  W-KDKOLLI-X.                                                     
075500         05  W-KDKOLLI           PIC X(8)    VALUE SPACE.                 
075600*                                                                         
075700     03  W-IDKOLLI-X.                                                     
075800         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
075900*                                                                         
076000     03  W-IDKOLLI-MIN-X.                                                 
076100         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
076200*                                                                         
076300     03  W-IDKOLLI-MAX-X.                                                 
076400         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
076500*                                                                         
076600     03  W-WDE401KY-X.                                                    
076700         05  W-WDE401KY          PIC X(23)    VALUE SPACE.                
076800*                                                                         
076900     03  W-IDPURAD-X.                                                     
077000         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
077100*Q301                                                                     
077200     03  W-WDQ301KY-MIN-X.                                                
077300         05  W-IDORDER-Q301      PIC S9(7)   VALUE ZERO COMP-3.           
077400         05  W-IDDC-Q301         PIC  X(2)   VALUE SPACE.                 
077500         05  W-IDPRODNR-Q301      PIC S9(7)   VALUE ZERO COMP-3.          
077600         05  W-IDPLKLST-Q301     PIC S9(3)   VALUE ZERO COMP-3.           
077700*                                                                         
077800     03  W-WDQ301KY-MAX-X.                                                
077900         05  W-IDORDER-Q301-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
078000         05  W-IDDC-Q301-MAX     PIC  X(2)   VALUE SPACE.                 
078100         05  W-IDPRODNR-Q301-MAX  PIC S9(7)   VALUE ZERO COMP-3.          
078200         05  W-IDPLKLST-Q301-MAX PIC S9(3)   VALUE ZERO COMP-3.           
078300*                                                                         
078400     03  W-IDPLKLST-SOEK-X.                                               
078500         05  W-IDPLKLST-SOEK     PIC S9(3)  COMP-3.                       
078600*                                                                         
078700     03  W-KDODELSTA-SOEK-X.                                              
078800         05  W-KDODELSTA-SOEK    PIC  X(1)   VALUE SPACE.                 
078900*Q3H1                                                                     
079000     03  W-WDQ3HSEQ-X.                                                    
079100         05  W-IDDC-Q3H1         PIC  X(2)   VALUE SPACE.                 
079200         05  W-IDPRCPLK-Q3H1     PIC  X(4)   VALUE SPACE.                 
079300         05  W-IDLOTNR-PLK-Q3H1  PIC S9(03)  VALUE ZERO COMP-3.           
079400*Q3D1                                                                     
079500     03  W-WDQ3D1KY-MIN-X.                                                
079600         05  W-IDPRODNR-Q3D1     PIC S9(7)   VALUE ZERO COMP-3.           
079700         05  W-IDPLKLST-Q3D1     PIC S9(3)   VALUE ZERO COMP-3.           
079800         05  W-IDORDER-Q3D1      PIC S9(7)   VALUE ZERO COMP-3.           
079900         05  W-IDDC-Q3D1         PIC  X(2)   VALUE SPACE.                 
080000*                                                                         
080100     03  W-WDQ3D1KY-MAX-X.                                                
080200         05  W-IDPRODNR-Q3D1-MAX PIC S9(7)   VALUE ZERO COMP-3.           
080300         05  W-IDPLKLST-Q3D1-MAX PIC S9(3)   VALUE ZERO COMP-3.           
080400         05  W-IDORDER-Q3D1-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
080500         05  W-IDDC-Q3D1-MAX     PIC  X(2)   VALUE SPACE.                 
080600*E6C1                                                                     
080700     03  W-WDE6C1KY-MIN-X.                                                
080800         05  W-IDTRPTNR-E6C1     PIC S9(3)   VALUE ZERO COMP-3.           
080900         05  W-DARFS-E6C1        PIC  9(12)  VALUE ZERO.                  
081000         05  W-IDDC-E6C1         PIC  X(02)  VALUE SPACE.                 
081100         05  W-ADFLGEO-E6C1      PIC  X(03)  VALUE SPACE.                 
081200         05  W-ADFLOMR-E6C1      PIC S9(03)  VALUE ZERO COMP-3.           
081300         05  W-ADRUTNIV-E6C1     PIC S9(03)  VALUE ZERO COMP-3.           
081400         05  W-ADVMODUL-E6C1     PIC S9(03)  VALUE ZERO COMP-3.           
081500         05  W-IDDISTR-E6C1      PIC S9(5)   VALUE ZERO COMP-3.           
081600         05  W-IDKUNDNR-E6C1     PIC S9(7)   VALUE ZERO COMP-3.           
081700         05  W-IDPRODNR-E6C1     PIC S9(7)   VALUE ZERO COMP-3.           
081800         05  W-IDKOLLI-FLER-E6C1 PIC S9(5)   VALUE ZERO COMP-3.           
081900         05  W-IDKOLLI-E6C1      PIC S9(5)   VALUE ZERO COMP-3.           
082000*E611                                                                     
082100     03 W-IDPRODNR-WDE601-X.                                              
082200        05  W-IDPRODNR-WDE601    PIC S9(7)   VALUE ZERO  COMP-3.          
082300     03 W-IDKOLLI-WDE611-X.                                               
082400        05  W-IDKOLLI-WDE611     PIC S9(5)   VALUE ZERO  COMP-3.          
082500                                                                          
082600     03  W-WDE6C1KY-MAX-X.                                                
082700         05  W-IDTRPTNR-E6C1-MAX PIC S9(3)   VALUE ZERO COMP-3.           
082800         05  W-DARFS-E6C1-MAX    PIC  9(12)  VALUE ZERO.                  
082900         05  W-IDDC-E6C1-MAX     PIC  X(02)  VALUE SPACE.                 
083000         05  W-ADFLGEO-E6C1-MAX  PIC  X(03)  VALUE SPACE.                 
083100         05  W-ADFLOMR-E6C1-MAX  PIC S9(03)  VALUE ZERO COMP-3.           
083200         05  W-ADRUTNIV-E6C1-MAX PIC S9(03)  VALUE ZERO COMP-3.           
083300         05  W-ADVMODUL-E6C1-MAX PIC S9(03)  VALUE ZERO COMP-3.           
083400         05  W-IDDISTR-E6C1-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
083500         05  W-IDKUNDNR-E6C1-MAX PIC S9(7)   VALUE ZERO COMP-3.           
083600         05  W-IDPRODNR-E6C1-MAX PIC S9(7)   VALUE ZERO COMP-3.           
083700         05  W-IDKOLLI-FLER-E6C1-MAX PIC S9(5) VALUE ZERO COMP-3.         
083800         05  W-IDKOLLI-E6C1-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
083900*E4A1                                                                     
084000     03  W-WDE4A1-KUNDORDER-X.                                            
084100       05  W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
084200       05  W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
084300       05  W-4A1-IDKUNDRF.                                                
084400         07 W-4A1-IDORDNR        PIC  9(5)   VALUE ZERO.                  
084500         07 FILLER               PIC X(05)   VALUE SPACE.                 
084600*                                                                         
084700     03  W-WDE401-KUNDORDER-X.                                            
084800       05  W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
084900       05  W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
085000       05  W-401-IDKUNDRF.                                                
085100         07 W-401-IDORDNR        PIC  9(5)   VALUE ZERO.                  
085200         07 FILLER               PIC X(05)   VALUE SPACE.                 
085300       05  W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
085400       05  W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
085500*E4B                                                                      
085600     03  W-WDE4B-KEYSEQ-MIN-X.                                            
085700       05  W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
085800       05  W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
085900*                                                                         
086000     03  W-WDE4B-KEYSEQ-MAX-X.                                            
086100       05  W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
086200       05  W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
086300*                                                                         
086400     03  W-WDE4B-KEYSEQ-X.                                                
086500       05  W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
086600       05  W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
086700*                                                                         
086800     03  W-WDE4ASEQ-X.                                                    
086900      05   W-E4ASEQ-IDGMTREF         PIC X(17)  VALUE SPACE.              
087000                                                                          
087100     03  W-WDE411-IDPURAD-X.                                              
087200       05  W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
087300*                                                                         
087400     03  W-WDE421-IDKOLLI-X.                                              
087500       05  W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
087600       05  W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
087700*                                                                         
087800   03    W-WDA601KY-MIN-X.                                                
087900     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
088000     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
088100     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
088200     05    FILLER REDEFINES W-A601KY-MIN-IDKUNDRF.                        
088300        07  W-A601KY-MIN-IDORDNR     PIC 9(07).                           
088400        07  FILLER                   PIC X(03).                           
088500     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
088600     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
088700     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
088800     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
088900     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
089000     SKIP2                                                                
089100   03    W-WDA601KY-MAX-X.                                                
089200     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
089300     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
089400     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
089500     05    FILLER REDEFINES W-A601KY-MAX-IDKUNDRF.                        
089600        07  W-A601KY-MAX-IDORDNR     PIC 9(07).                           
089700        07  FILLER                   PIC X(03).                           
089800     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
089900     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
090000     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
090100     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
090200     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
090300*                                                                         
090400     03  W-WDQ101KY-MIN-X.                                                
090500         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
090600         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
090700         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
090800         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
090900         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
091000                                                                          
091100     03  W-WDQ101KY-MAX-X.                                                
091200         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
091300         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
091400         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
091500         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
091600         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
091700*                                                                         
091800   03    W-WDQ201-X.                                                      
091900     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
092000                                                                          
092100   03    W-WDQ212-X.                                                      
092200     05    W-212-IDDC            PIC X(02).                               
092300*Q2C                                                                      
092400   03  W-WDQ2CSEQ-X.                                                      
092500       05  W-WDQ2CSEQ-IDGMTREF.                                           
092600           07  W-WDQ2CSEQ-IDDISTR                                         
092700                               PIC S9(5) COMP-3 VALUE +0.                 
092800           07  W-WDQ2CSEQ-IDKUNDNR                                        
092900                               PIC S9(7) COMP-3 VALUE +0.                 
093000           07  W-WDQ2CSEQ-IDKUNDRF                                        
093100                               PIC X(10)     VALUE '0000000   '.          
093200*                                                                         
093300   03    W-WDQ2C-X.                                                       
093400     05    W-WDQ2C-IDGMTREF-X.                                            
093500       07    W-WDQ2C-IDDISTR     PIC S9(5) COMP-3 VALUE +0.               
093600       07    W-WDQ2C-IDKUNDNR    PIC S9(7) COMP-3 VALUE +0.               
093700       07    W-WDQ2C-IDKUNDRF.                                            
093800         09    FILLER            PIC  9(2)        VALUE ZERO.             
093900         09    W-WDQ2C-IDORDNR5                                           
094000                                 PIC  9(5)        VALUE ZERO.             
094100         09    FILLER            PIC  X(3)        VALUE SPACE.            
094200*                                                                         
094300   03    W-WDQ301-KEY-X.                                                  
094400     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
094500     05    W-WDQ301-IDDC         PIC X(2).                                
094600     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
094700     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
094800*                                                                         
094900   03  W-Q301-KEY-MIN-X.                                                  
095000         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
095100         05  W-Q301-MIN-IDDC     PIC X(2).                                
095200         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
095300         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
095400*                                                                         
095500   03  W-Q301-KEY-MAX-X.                                                  
095600         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
095700         05  W-Q301-MAX-IDDC     PIC X(2).                                
095800         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
095900         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
096000*                                                                         
096100   03  W-WDQ301KY-MIN.                                                    
096200         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
096300         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
096400         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
096500*                                                                         
096600   03    W-WDQ301-ORDERDEL-X.                                             
096700     05    W-301-IDORDER         PIC S9(7) COMP-3.                        
096800     05    W-301-IDDC            PIC X(2).                                
096900     05    W-301-IDPRODNR        PIC S9(7) COMP-3.                        
097000     05    W-301-IDPLKLST        PIC S9(3) COMP-3.                        
097100*                                                                         
097200   03  W-KDODELST                PIC X.                                   
097300*                                                                         
097400   03    W-IDARTNR-X.                                                     
097500     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
097600*                                                                         
097700   03    W-WDK711-IDDC-X.                                                 
097800     05    W-711-IDDC            PIC X(2).                                
097900*                                                                         
098000   03  W-IDLAND-X.                                                        
098100      05  W-IDLAND               PIC X(2).                                
098200*                                                                         
098300   03    W-WDK901-IDARTNR-X.                                              
098400     05    W-901-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.          
098500*                                                                         
098600   03    W-WDG6KEY-X.                                                     
098700     05    W-RDG-TIAAMMDD        PIC 9(6)    VALUE ZERO.                  
098800     05    W-RDG-TIKLOCK         PIC 9(8)    VALUE ZERO.                  
098900     05    W-RDG-IDLOGLOP        PIC 9(1)    VALUE ZERO.                  
099000     05    W-RDG-IDPTYP          PIC X(3)    VALUE 'RY1'.                 
099100*                                                                         
099200   03    W-WDGX11-WDGXKEY-X.                                              
099300     05    W-RDG-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
099400     05    W-RDG-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
099500     05    W-RDG-IDDC            PIC X(2).                                
099600     05    W-RDG-KDFAKTYP        PIC X(1)    VALUE SPACE.                 
099700*                                                                         
099800     03  W-WDQ301KY-MAX.                                                  
099900         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
100000         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
100100         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
100200*                                                                         
100300   03    W-2203-X.                                                        
100400     05    W-2203-IDHTYP         PIC X(4) VALUE '2203'.                   
100500     05    W-2203-IDDC           PIC X(2).                                
100600     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
100700*                                                                         
100800   03    W-4321-IDHTYP-X.                                                 
100900     05    W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                  
101000     05    W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.               
101100*                                                                         
101200   03    W-4447-X.                                                        
101300     05    FILLER                PIC X(4)  VALUE '4447'.                  
101400     05    W-4447-IDDC           PIC X(2).                                
101500     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
101600*                                                                         
101700   03    W-4448-X.                                                        
101800     05    W-4448-IDPRC          PIC X(4).                                
101900     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
102000*                                                                         
102100   03    W-4471-WDGXKEY-X.                                                
102200     05    W-4471-IDHTYP         PIC X(4)  VALUE '4471'.                  
102300     05    W-4471-IDDC           PIC X(2).                                
102400     05    W-4471-IDPRC.                                                  
102500       07    W-4471-IDPRCBAS     PIC X(3).                                
102600       07    W-4471-IDPRCVAR     PIC X(1).                                
102700     05    FILLER                PIC X(20) VALUE LOW-VALUE.               
102800*                                                                         
102900   03    W-4472-KDSEGKEY-X.                                               
103000     05    W-4472-KDSEGKEY       PIC X(1)  VALUE '1'.                     
103100*                                                                         
103200   03    W-4477-WDGXKEY-X.                                                
103300     05    W-4477-IDHTYP         PIC X(4)  VALUE '4477'.                  
103400     05    W-4477-IDDC           PIC X(2).                                
103500     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
103600*                                                                         
103700   03    W-4478-WDGXKEY-X.                                                
103800     05    W-4478-IDSHIFT        PIC X(1).                                
103900     05    W-4478-IDUSER         PIC X(8).                                
104000     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
104100*                                                                         
104200   03    W-4487-X.                                                        
104300     05    FILLER                PIC X(4)  VALUE '4487'.                  
104400     05    W-4487-IDDC           PIC X(2).                                
104500     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
104600*                                                                         
104700   03    W-KDPRCGRP-X.                                                    
104800     05    W-4488-KDPRCGRP       PIC X(5).                                
104900*                                                                         
105000   03    W-4490-X.                                                        
105100     05    W-4490-DARFS          PIC 9(12).                               
105200     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
105300     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
105400                                                                          
105500   03  W-WDM201-X.                                                        
105600       05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.            
105700       05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                  
105800                                                                          
105900   03  W-WDM211-X.                                                        
106000       05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.            
106100                                                                          
106200   03  W-WDM221-X.                                                        
106300       05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.             
106400       05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.             
106500       05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.             
106600       05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.             
106700*                                                                         
106800   03    W-WDGXKEY-4541-X.                                                
106900     05    W-WDGXKEY4541          PIC X(4)  VALUE '4541'.                 
107000     05    FILLER                 PIC X(26) VALUE LOW-VALUE.              
107100*                                                                         
107200   03    W-4726-WDGXKEY-ROT-X.                                            
107300     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
107400     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
107500     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
107600*                                                                         
107700   03    W-4726-WDGXKEY-UNDSEG-X.                                         
107800     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
107900     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
108000     05    W-4726-IDDC           PIC X(2).                                
108100     05    W-4726-KDFAKTYP       PIC X.                                   
108200*                                                                         
108300   03    W1-WDA501KY-X.                                                   
108400     05    W1-IDDISTR             PIC S9(5)   COMP-3.                     
108500     05    W1-IDKUNDNR            PIC S9(7)   COMP-3.                     
108600     05    W1-IDKUNDRF            PIC X(10).                              
108700     05    W1-IDARTNR             PIC S9(9)   COMP-3.                     
108800     05    W1-IDLOPNR             PIC S9(3)   COMP-3.                     
108900*                                                                         
109000   03    W2-WDA501KY-X.                                                   
109100     05    W2-IDDISTR             PIC S9(5)   COMP-3.                     
109200     05    W2-IDKUNDNR            PIC S9(7)   COMP-3.                     
109300     05    W2-IDKUNDRF            PIC X(10).                              
109400     05    W2-IDARTNR             PIC S9(9)   COMP-3.                     
109500     05    W2-IDLOPNR             PIC S9(3)   COMP-3.                     
109600*                                                                         
109700   03    W-KDSTARAD-X.                                                    
109800     05    W-KDSTARAD             PIC X.                                  
109900*                                                                         
110000   03    W-WDGX01.                                                        
110100     05    W-IDHTYP-N5           PIC X(04)   VALUE '4511'.                
110200     05    W-VALFRI-N5           PIC X(26)   VALUE SPACE.                 
110300*                                                                         
110400   03    W-WDGXKEY-N5-MIN.                                                
110500     05    FILLER                PIC X(10)   VALUE SPACE.                 
110600*                                                                         
110700   03    W-WDGXKEY-N5-MAX.                                                
110800     05    FILLER                PIC X(10)   VALUE SPACE.                 
110900*                                                                         
111000   03    W-KDRAPRIO-N5-MIN-X.                                             
111100     05    W-KDRAPRIO-N5-MIN     PIC S9(3) COMP-3 VALUE ZERO.             
111200*                                                                         
111300   03    W-KDRAPRIO-N5-MAX-X.                                             
111400     05    W-KDRAPRIO-N5-MAX     PIC S9(3) COMP-3 VALUE ZERO.             
111500*                                                                         
111600   03    W-KDTPOTYP-N5-X.                                                 
111700     05    W-KDTPOTYP-N5         PIC S9(1) COMP-3 VALUE ZERO.             
111800*                                                                         
111900   03    W-KDORDKL-N5-X.                                                  
112000     05    W-KDORDKL-N5          PIC S9(1) COMP-3 VALUE ZERO.             
112100*                                                                         
112200   03    W-IDDISTR-FOM-N5-X.                                              
112300     05    W-IDDISTR-FOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
112400*                                                                         
112500   03    W-IDDISTR-TOM-N5-X.                                              
112600     05    W-IDDISTR-TOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
112700                                                                          
112800   03    W-IDDC-B6-X.                                                     
112900     05    W-IDDC-B6             PIC  X(2).                               
113000                                                                          
113100   03    W-IDDISTR-P4-X.                                                  
113200     05 W-IDDISTR-P4               PIC S9(5)   VALUE ZERO  COMP-3.        
113300*                                                                         
113400*                                                                         
113500 01    IMS-WS.                                                            
113600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
113700     SKIP3                                                                
113800*                        **** STATUS-KOD FRÅN IMS                         
113900*                        **** STATUS-KOD FRÅN IMS                         
114000   03    STATUS-RAD-WS           PIC XX.                                  
114100     88    RAD-FINNS                         VALUE '  '.                  
114200     88    RAD-SAKNAS                        VALUE 'GE'.                  
114300   03    STATUS-WS               PIC XX.                                  
114400     88    SEGMENT-FOUND                     VALUE '  '.                  
114500     88    ISRT-OK                           VALUE '  '.                  
114600     88    SEGMENT-END                       VALUE 'GB'.                  
114700     88    SEGMENT-MISSING                   VALUE 'GE'.                  
114800     88    SEGMENT-FOUND-EXISTS              VALUE 'II'.                  
114900   03    Q3D1-STATUS-WS          PIC XX.                                  
115000     88    Q3D1-SEGMENT-FOUND                VALUE '  '.                  
115100     88    Q3D1-SEGMENT-MISSING              VALUE 'GE'.                  
115200     88    Q3D1-SEGMENT-END                  VALUE 'GB'.                  
115300     SKIP3                                                                
115400 01  FILLER                      PIC X(08)   VALUE 'SSA-AREA'.            
115500 01  GOOD-STATUSCODES.                                                    
115600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
115700     SKIP3                                                                
115800 01    SSA1                      PIC X(416).                              
115900 01    SSA2                      PIC X(384).                              
116000 01    SSA3                      PIC X(192).                              
116100 01    SSA4                      PIC X(160).                              
116200     EJECT                                                                
116300*                            DB2 FUNKTIONSKODER                           
116400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
116500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
116600                                                                          
116700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
116800 01  DB2-WS.                                                              
116900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
117000         88  CURSOR-OK                       VALUE 000.                   
117100         88  RADER-FINNS                     VALUE 000.                   
117200         88  RADER-SAKNAS                    VALUE 100.                   
117300         88  ATKOMST-FEL                     VALUE 904.                   
117400     03  GODK-SQLCODEKODER.                                               
117500         05  GODK-SQLCODE OCCURS 5                                        
117600             INDEXED BY SQLCODE-IX PIC 9(3).                              
117700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
117800     SKIP2                                                                
117900*                                                                         
118000 01  FILLER                     PIC X(16) VALUE '2191-IO-AREA'.           
118100 01  2191-IO-AREA.                                                        
118200                                                                          
118300  03     2191-LL                 PIC S9(4) COMP SYNC.                     
118400  03     2191-Z1                 PIC X(1)  VALUE LOW-VALUE.               
118500  03     2191-Z2                 PIC X(1)  VALUE LOW-VALUE.               
118600  03     2191-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
118700  03     2191-IDTRANS            PIC X(4)  VALUE '43AV'.                  
118800  03     2191-SPRAK              PIC X(1).                                
118900* 03     MID -COPY W2I19101   -PRE 2191-                                  
119000     EJECT                                                                
119100                                                                          
119200 01    FILLER                 PIC X(16) VALUE 'MID W4I34901 MID'.         
119300     SKIP3                                                                
119400*01    MID -COPY W4I34901  -PRE 4349-.                                    
119500     SKIP2                                                                
119600     EJECT                                                                
119700*---MSG-AERA FÖR HOPP TILL 4349-UTSKRIFT DELIVERY NOTE NA                 
119800 01  FILLER                PIC X(16)  VALUE '4349-MSG-IO-AREA'.           
119900 01  4349-MSG-IO-AREA.                                                    
120000     03  4349-LL              PIC S9(4)  VALUE +748 COMP SYNC.            
120100     03  4349-Z1              PIC X.                                      
120200     03  4349-Z2              PIC X.                                      
120300     03  4349-TRANSKOD        PIC X(8)   VALUE 'W4T349X '.                
120400     03  4349-IDTRANS         PIC X(4)   VALUE 'L197'.                    
120500     03  4349-SPRAK           PIC X.                                      
120600     03  4349-FILLER          PIC X(731).                                 
120700                                                                          
120800 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
120900     SKIP3                                                                
121000 01    EMB-TABELL.                                                        
121100   03    EMB-TAB-X.                                                       
121200     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
121300         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
121400                                         PIC S9(5)  COMP-3.               
121500   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
121600     05  FILLER.                                                          
121700         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
121800     05  FILLER.                                                          
121900         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
122000     05  FILLER.                                                          
122100         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
122200     EJECT                                                                
122300 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
122400                                                                          
122500 01    FG-TABELL.                                                         
122600   03    TAB-POST OCCURS 10.                                              
122700                                                                          
122800     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
122900                                                                          
123000     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
123100                                                                          
123200     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
123300     EJECT                                                                
123400******************************************************************        
123500*                                                                         
123600*01  XXJK  -COPY WDGX4322    -PRE XXJK-                                   
123700     EJECT                                                                
123800******************************************************************        
123900*                                                                         
124000*    --- IMS FUNCTION CODES                                               
124100*01  -COPY W0003                                                          
124200     EJECT                                                                
124300*    ---  DLI INPUT-OUTPUT AREA                                           
124400                                                                          
124500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
124600 01  DLI-IO-WDK501.                                                       
124700*    03  -COPY WDK501                                                     
124800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
124900 01  DLI-IO-WDE601.                                                       
125000*    03  -COPY WDE601                                                     
125100     EJECT                                                                
125200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
125300 01  DLI-IO-WDE611.                                                       
125400*    03  -COPY WDE611                                                     
125500     EJECT                                                                
125600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE6C1'.                      
125700 01  DLI-IO-WDE6C1.                                                       
125800*    03  -COPY WDE6C1                                                     
125900     EJECT                                                                
126000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
126100 01  DLI-IO-WDE401.                                                       
126200*    03  -COPY WDE401                                                     
126300     EJECT                                                                
126400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
126500 01  DLI-IO-WDE411.                                                       
126600*    03  -COPY WDE411                                                     
126700     EJECT                                                                
126800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
126900 01  DLI-IO-WDE421.                                                       
127000*    03  -COPY WDE421                                                     
127100     EJECT                                                                
127200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4E1'.                      
127300 01  DLI-IO-WDE4E1.                                                       
127400*    03  -COPY WDE4E1                                                     
127500     EJECT                                                                
127600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
127700 01  DLI-IO-WDQ301.                                                       
127800*    03  -COPY WDQ301                                                     
127900*                                                                         
128000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3D1'.                      
128100 01  DLI-IO-WDQ3D1.                                                       
128200*    03  -COPY WDQ3D1                                                     
128300*                                                                         
128400*FROM W403AVSP                                                            
128500*                                                                         
128600*01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
128700*01    DLI-IO-AREA2.                                                      
128800*  03    IO-AREA2                PIC X(400)  VALUE SPACE.                 
128900*    SKIP3                                                                
129000*  03    WDE601  COPY WDE601               -RED IO-AREA2.                 
129100*    EJECT                                                                
129200*  03    WDE611  COPY WDE611               -RED IO-AREA2.                 
129300*    EJECT                                                                
129400 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA3'.               
129500 01    DLI-IO-AREA3.                                                      
129600   03    IO-AREA3                PIC X(32)   VALUE SPACE.                 
129700     SKIP3                                                                
129800*  03    WLXXDV11 -COPY WDGX4726           -RED IO-AREA3.                 
129900*  03    WLXXDV21 -COPY WDGX4727           -RED IO-AREA3.                 
130000     EJECT                                                                
130100 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA4'.               
130200 01  DLI-IO-AREA4.                                                        
130300     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
130400*                                                                         
130500*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4.           
130600*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4.                       
130700     EJECT                                                                
130800*    03  WLXXJN    -COPY WDGX4512    -PRE STYR-  -RED IO-AREA4.           
130900     EJECT                                                                
131000*01    FILLER    PIC X(16)  VALUE 'DLI-IO-Q301'.                          
131100*01  DLI-IO-Q301.                                                         
131200*    03  WLORQA01   COPY WDQ301                                           
131300*                                                                         
131400 01    FILLER    PIC X(16)  VALUE 'DLI-IO-Q101'.                          
131500 01  DLI-IO-Q101.                                                         
131600*    03  WLORQM01  -COPY WDQ101                                           
131700     EJECT                                                                
131800 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA6'.               
131900 01  DLI-IO-AREA6.                                                        
132000     03  IO-AREA6                PIC X(1000) VALUE SPACE.                 
132100*                                                                         
132200*    03  WLXXKW11  -COPY WDGX4472            -RED IO-AREA6.               
132300     EJECT                                                                
132400*    03  WLXXLB11  -COPY WDGX4478            -RED IO-AREA6.               
132500     EJECT                                                                
132600*  03  WDA501     -COPY WDA501               -RED IO-AREA6.               
132700     EJECT                                                                
132800 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA7'.               
132900 01    DLI-IO-AREA7.                                                      
133000   03    IO-AREA7                PIC X(332)  VALUE SPACE.                 
133100     SKIP3                                                                
133200*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA7.               
133300     EJECT                                                                
133400*  03  WDA501     -COPY WDA501   -PRE OLD-   -RED IO-AREA7.               
133500     EJECT                                                                
133600 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA8'.               
133700 01    DLI-IO-AREA8.                                                      
133800   03    IO-AREA8                PIC X(900)  VALUE SPACE.                 
133900     SKIP3                                                                
134000*  03  WLARTM01   -COPY WDK901               -RED IO-AREA8.               
134100     EJECT                                                                
134200*  03  WDK711     -COPY WDK711               -RED IO-AREA8.               
134300     EJECT                                                                
134400*  03  WLARTC01   -COPY WDK601               -RED IO-AREA8.               
134500     EJECT                                                                
134600*  03  WLARTC11   -COPY WDK611               -RED IO-AREA8.               
134700     EJECT                                                                
134800*  03  WLOGAG01   -COPY WDE601   -PRE OGAG-  -RED IO-AREA8.               
134900     EJECT                                                                
135000*  03  WLOGAG12   -COPY WDE401   -PRE OGAG-  -RED IO-AREA8.               
135100     EJECT                                                                
135200 01  FILLER                      PIC X(16) VALUE 'WDK712 AREA'.           
135300 01  DLI-IO-WDK712.                                                       
135400*    03  -COPY WDK712                                                     
135500     EJECT                                                                
135600 01  FILLER                      PIC X(16) VALUE 'WDK722 AREA'.           
135700 01  DLI-IO-WDK722.                                                       
135800*    03  -COPY WDK722                                                     
135900     EJECT                                                                
136000 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
136100 01    DLI-IO-AREA11.                                                     
136200*  03    WDA601   -COPY WDA601                                            
136300     EJECT                                                                
136400 01  FILLER                      PIC X(16)  VALUE 'WDQ2-AREA'.            
136500     SKIP2                                                                
136600 01  -COPY WDQ201                                                         
136700 01  FILLER                      PIC X(16)  VALUE 'WDQ212-AREA'.          
136800     SKIP2                                                                
136900 01  -COPY WDQ212                                                         
137000     EJECT                                                                
137100 01  FILLER                      PIC X(16)  VALUE '4490-AREA'.            
137200     SKIP2                                                                
137300 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4490'.           
137400 01    DLI-IO-WDGX4490.                                                   
137500*  03    -COPY WDGX4490                                                   
137600     EJECT                                                                
137700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
137800 01  DLI-IO-WDM211.                                                       
137900*    03 -COPY WDM211                                                      
138000     EJECT                                                                
138100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
138200 01  DLI-IO-WDM221.                                                       
138300*    03 -COPY WDM221                                                      
138400     EJECT                                                                
138500 01  FILLER                      PIC X(16)  VALUE '4542-AREA'.            
138600 01  -COPY WDGX4542.                                                      
138700     EJECT                                                                
138800 01  FILLER                      PIC X(16)  VALUE '4448-AREA'.            
138900 01  WLXXKH11 -COPY WDGX4448.                                             
139000     EJECT                                                                
139100 01  FILLER                      PIC X(16)  VALUE '2204-AREA'.            
139200     SKIP2                                                                
139300 01  -COPY WDGX2204                                                       
139400     EJECT                                                                
139500 01  FILLER                      PIC X(16)  VALUE 'RY1-AREA'.             
139600 01  -COPY WDGZRY1.                                                       
139700     EJECT                                                                
139800 01  FILLER                      PIC X(16)  VALUE 'RY1S-AREA'.            
139900 01  -COPY WDGZRY1S.                                                      
140000     EJECT                                                                
140100 01  FILLER                      PIC X(16)  VALUE 'RY6-AREA'.             
140200 01  -COPY WDGZRY6.                                                       
140300     EJECT                                                                
140400 01  FILLER                      PIC X(16)  VALUE 'RYK-AREA'.             
140500 01  -COPY WDGZRYK.                                                       
140600     EJECT                                                                
140700 01  FILLER                      PIC X(16)  VALUE 'WLLOGA01'.             
140800*01  WLLOGA01 -COPY WDL901                                                
140900     EJECT                                                                
141000 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
141100 01  DLI-IO-AREA-B601.                                                    
141200*    03  -COPY WDB601                                                     
141300 01  FILLER                      PIC X(16)  VALUE 'WDP4A1 AREA'.          
141400 01   DLI-IO-AREA-WDP4A1.                                                 
141500*     03  -COPY WDP4A1                                                    
141600     EJECT                                                                
141700 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
141800                                                                          
141900                                                                          
142000*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
142100*                                                                         
142200     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
142300*                                                                         
142400 01  FILLER         PIC X(16) VALUE 'LINKAGE-SECTION:'.                   
142500 LINKAGE SECTION.                                                         
142600*01  -COPY W0009   -PRE MSG-                                              
142700                                                                          
142800*01  -COPY W0008   -PRE DETTAPGM-                                         
142900     05  FILLER                  PIC X.                                   
143000                                                                          
143100*01  -COPY W0008   -PRE 2191-                                             
143200     05  FILLER                  PIC X.                                   
143300                                                                          
143400*01  -COPY W0008   -PRE 4349-                                             
143500     05  FILLER                  PIC X.                                   
143600                                                                          
143700 01  TMS-CRE-PCB                 PIC X.                                   
143800 01  TMS-DEL-PCB                 PIC X.                                   
143900                                                                          
144000 01  ATAB-PCB                    PIC X.                                   
144100                                                                          
144200*01  -COPY W0008   -PRE WDK5-                                             
144300     05  FILLER                  PIC X.                                   
144400                                                                          
144500*01  -COPY W0008   -PRE WDE6-                                             
144600     05  FILLER                  PIC X.                                   
144700                                                                          
144800*01  -COPY W0008   -PRE WDE6C-                                            
144900     05  FILLER                  PIC X.                                   
145000                                                                          
145100*01  -COPY W0008   -PRE WDE4-                                             
145200     05  FILLER                  PIC X.                                   
145300                                                                          
145400*01  -COPY W0008   -PRE WDE41-                                            
145500     05  FILLER                  PIC X.                                   
145600                                                                          
145700*01  -COPY W0008   -PRE WDE4A-                                            
145800     05  FILLER                  PIC X.                                   
145900                                                                          
146000*01  -COPY W0008   -PRE WDE4E-                                            
146100     05  FILLER                  PIC X.                                   
146200                                                                          
146300*01  -COPY W0008   -PRE WDQ301-                                           
146400     05  FILLER                  PIC X.                                   
146500                                                                          
146600*01  -COPY W0008   -PRE WDQ3H-                                            
146700     05  FILLER                  PIC X.                                   
146800                                                                          
146900*01  -COPY W0008   -PRE WDQ3D-                                            
147000     05  FILLER                  PIC X.                                   
147100                                                                          
147200*01  -COPY W0008   -PRE XXDV-                                             
147300     05  FILLER                  PIC X.                                   
147400                                                                          
147500*01  -COPY W0008   -PRE XXKW-                                             
147600     05  FILLER                  PIC X.                                   
147700                                                                          
147800*01  -COPY W0008   -PRE XXLB-                                             
147900     05  FILLER                  PIC X.                                   
148000                                                                          
148100*01  -COPY W0008   -PRE XXJK-                                             
148200     05  FILLER                  PIC X.                                   
148300                                                                          
148400*01  -COPY W0008   -PRE ZZAC-                                             
148500     05  FILLER                  PIC X.                                   
148600                                                                          
148700*01  -COPY W0008   -PRE ORQI-                                             
148800     05  FILLER                  PIC X.                                   
148900                                                                          
149000*01  -COPY W0008   -PRE ORQL-                                             
149100     05  FILLER                  PIC X.                                   
149200                                                                          
149300*01  -COPY W0008   -PRE ORQICSQ-                                          
149400     05  FILLER                  PIC X.                                   
149500                                                                          
149600*01  -COPY W0008   -PRE ORQM-                                             
149700     05  FILLER                  PIC X.                                   
149800                                                                          
149900*01  -COPY W0008   -PRE WDM2-                                             
150000     05  FILLER                  PIC X.                                   
150100                                                                          
150200*01  -COPY W0008   -PRE ORQA2-                                            
150300     05  FILLER                  PIC X.                                   
150400                                                                          
150500*01  -COPY W0008   -PRE ORDP1-                                            
150600     05  FILLER                  PIC X.                                   
150700                                                                          
150800*01  -COPY W0008   -PRE ORDP2-                                            
150900     05  FILLER                  PIC X.                                   
151000                                                                          
151100*01  -COPY W0008   -PRE XXJN-                                             
151200     05  FILLER                  PIC X.                                   
151300                                                                          
151400*01  -COPY W0008   -PRE XXKH-                                             
151500     05  FILLER                  PIC X.                                   
151600                                                                          
151700*01  -COPY W0008   -PRE ARTC-                                             
151800     05  FILLER                  PIC X.                                   
151900                                                                          
152000*01  -COPY W0008   -PRE WDK7-                                             
152100     05  FILLER                  PIC X.                                   
152200                                                                          
152300*01  -COPY W0008   -PRE ARTM-                                             
152400     05  FILLER                  PIC X.                                   
152500                                                                          
152600*01  -COPY W0008   -PRE AUTF-                                             
152700     05  FILLER                  PIC X.                                   
152800                                                                          
152900*01  -COPY W0008   -PRE 4487-                                             
153000     05  FILLER                  PIC X.                                   
153100                                                                          
153200*01  -COPY W0008   -PRE 4541-                                             
153300     05  FILLER                  PIC X.                                   
153400                                                                          
153500*01  -COPY W0008   -PRE LOGA-                                             
153600     05  FILLER                  PIC X.                                   
153700                                                                          
153800*01  -COPY W0008   -PRE WDK6-                                             
153900     05  FILLER                  PIC X.                                   
154000                                                                          
154100*01  -COPY W0008   -PRE WDA6B-                                            
154200     05  FILLER                  PIC X.                                   
154300                                                                          
154400*01  -COPY W0008   -PRE WDA6-                                             
154500     05  FILLER                  PIC X.                                   
154600                                                                          
154700*01  -COPY W0008   -PRE WDB6-                                             
154800     05  FILLER                  PIC X.                                   
154900                                                                          
155000*01  -COPY W0008   -PRE PRQU-WDG2-                                        
155100     05  FILLER                  PIC X.                                   
155200                                                                          
155300*01  -COPY W0008   -PRE PRQU-WDC7-                                        
155400     05  FILLER                  PIC X.                                   
155500                                                                          
155600*01  -COPY W0008   -PRE PRQU-SJKO-WDK6-                                   
155700     05  FILLER                  PIC X.                                   
155800                                                                          
155900*01  -COPY W0008   -PRE WDP4A-                                            
156000     05  FILLER                  PIC X.                                   
156100                                                                          
156200*01  -COPY W0008   -PRE PRNO-3107-                                        
156300     05  FILLER                  PIC X.                                   
156400                                                                          
156500 01  DNOT-ORQP-PCB               PIC X.                                   
156600 01  DNOT-ORQP2-PCB              PIC X.                                   
156700 01  DNOT-ORQP3-PCB              PIC X.                                   
156800 01  DNOT-4013-PCB               PIC X.                                   
156900 01  DNOT-BENA-PCB               PIC X.                                   
157000                                                                          
157100 01  TMS-1165-PCB               PIC X.                                    
157200 01  TMS-4141-PCB               PIC X.                                    
157300 01  TMS-WDB2-PCB               PIC X.                                    
157400 01  TMS-WDB6-PCB               PIC X.                                    
157500 01  TMS-WDD3-PCB               PIC X.                                    
157600 01  TMS-WDB1-PCB               PIC X.                                    
157700 01  TMS-WDE4A-PCB              PIC X.                                    
157800 01  TMS-WDE4F-PCB              PIC X.                                    
157900 01  TMS-WDQ2-PCB               PIC X.                                    
158000 01  TMS-WDQ3-PCB               PIC X.                                    
158100 01  TMS-WDK6-PCB               PIC X.                                    
158200 01  TMS-WDE6-PCB               PIC X.                                    
158300 01  TMS-WDK5-PCB               PIC X.                                    
158400 01  TMS-WDQ2C-PCB              PIC X.                                    
158500     EJECT                                                                
158600                                                                          
158700 PROCEDURE DIVISION  USING MSG-PCB   DETTAPGM-PCB 2191-PCB                
158800     4349-PCB     TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                        
158900     WDK5-PCB     WDE6-PCB    WDE6C-PCB                                   
159000     WDE4-PCB     WDE41-PCB   WDE4A-PCB   WDE4E-PCB                       
159100     WDQ301-PCB   WDQ3H-PCB   WDQ3D-PCB                                   
159200     XXDV-PCB     XXKW-PCB     XXLB-PCB    XXJK-PCB                       
159300     ZZAC-PCB                                                             
159400     ORQI-PCB     ORQL-PCB    ORQICSQ-PCB  ORQM-PCB                       
159500     WDM2-PCB     ORQA2-PCB                                               
159600     ORDP1-PCB    ORDP2-PCB   XXJN-PCB    XXKH-PCB                        
159700     ARTC-PCB     WDK7-PCB    ARTM-PCB    AUTF-PCB                        
159800     4487-PCB     4541-PCB    LOGA-PCB                                    
159900     WDK6-PCB     WDA6B-PCB   WDA6-PCB    WDB6-PCB                        
160000     PRQU-WDG2-PCB   PRQU-WDC7-PCB    PRQU-SJKO-WDK6-PCB                  
160100     WDP4A-PCB                                                            
160200     PRNO-3107-PCB                                                        
160300     DNOT-ORQP-PCB                                                        
160400     DNOT-ORQP2-PCB                                                       
160500     DNOT-ORQP3-PCB                                                       
160600     DNOT-4013-PCB                                                        
160700     DNOT-BENA-PCB                                                        
160800     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
160900     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
161000     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
161100     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
161200                                                                          
161300 MAIN SECTION.                                                            
161400     ENTRY 'DLITCBL' USING MSG-PCB DETTAPGM-PCB 2191-PCB                  
161500     4349-PCB     TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                        
161600     WDK5-PCB     WDE6-PCB    WDE6C-PCB                                   
161700     WDE4-PCB     WDE41-PCB   WDE4A-PCB   WDE4E-PCB                       
161800     WDQ301-PCB   WDQ3H-PCB   WDQ3D-PCB                                   
161900     XXDV-PCB     XXKW-PCB     XXLB-PCB    XXJK-PCB                       
162000     ZZAC-PCB                                                             
162100     ORQI-PCB     ORQL-PCB    ORQICSQ-PCB  ORQM-PCB                       
162200     WDM2-PCB     ORQA2-PCB                                               
162300     ORDP1-PCB    ORDP2-PCB   XXJN-PCB    XXKH-PCB                        
162400     ARTC-PCB     WDK7-PCB    ARTM-PCB    AUTF-PCB                        
162500     4487-PCB     4541-PCB    LOGA-PCB                                    
162600     WDK6-PCB     WDA6B-PCB   WDA6-PCB    WDB6-PCB                        
162700     PRQU-WDG2-PCB   PRQU-WDC7-PCB    PRQU-SJKO-WDK6-PCB                  
162800     WDP4A-PCB                                                            
162900     PRNO-3107-PCB                                                        
163000     DNOT-ORQP-PCB                                                        
163100     DNOT-ORQP2-PCB                                                       
163200     DNOT-ORQP3-PCB                                                       
163300     DNOT-4013-PCB                                                        
163400     DNOT-BENA-PCB                                                        
163500     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
163600     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
163700     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
163800     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
163900                                                                          
164000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
164100     IF SUB-KDRC = 0                                                      
164200       PERFORM A-INIT                                                     
164300                                                                          
164400       PERFORM B-KONTROLL-KEYS                                            
164500       IF KEYS-OK                                                         
164600*                                                                         
164700         IF REQU-KDPGMACT = 'E'                                           
164800           MOVE REQU-FLOK-PACKRAPP    TO RESP-FLOK-PACKRAPP               
164900           PERFORM C-KONTROLL-READ-DB                                     
165000           IF INDATA-OK                                                   
165100             IF REQU-FLOK-PACKRAPP = YES                                  
165200                PERFORM E-UPDATE-IDKOLLI                                  
165300*                                                                         
165400                  MOVE NEJ              TO SW-RESTART                     
165500                  MOVE ZERO             TO REQU-RESTART-IX-SPAR           
165600             ELSE                                                         
165700               MOVE CLICK-OK-FOR-REPORTING TO RESP-IDMSG-INFO             
165800               MOVE 'FLOK-PACKRAPP'       TO  RESP-IDELMT-ERROR           
165900             END-IF                                                       
166000           END-IF                                                         
166100         END-IF                                                           
166200                                                                          
166300           IF INDATA-OK                                                   
166400           OR RESP-IDMSG-INFO = 001                                       
166500             PERFORM F-READ-SHOW-INFO                                     
166600*                                                                         
166700           END-IF                                                         
166800       END-IF                                                             
166900                                                                          
167000         PERFORM S02-RETURN-RESPONSE                                      
167100     END-IF                                                               
167200                                                                          
167300     MOVE ZERO TO RETURN-CODE                                             
167400     GOBACK                                                               
167500     .                                                                    
167600     EJECT                                                                
167700                                                                          
167800 A-INIT SECTION.                                                          
167900     MOVE 'STA A-INIT'  TO WS-CURRENT-SECTION                             
168000                                                                          
168100     MOVE ALL '+' TO RESP-AREA                                            
168200     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
168300                     RESP-IDMSG-INFO                                      
168400                     RESP-IDELMT-ERROR                                    
168500     MOVE '001'   TO RESP-IDMSGVER                                        
168600     MOVE ZERO                   TO RESP-L128-KVRADER                     
168700     MOVE ZERO                   TO RESP-L129-KVRADER                     
168800                                                                          
168900     MOVE REQU-FLSKRIV-CLABEL    TO RESP-FLSKRIV-CLABEL                   
169000     MOVE REQU-FLSKRIV-DELNOTE   TO RESP-FLSKRIV-DELNOTE                  
169100                                                                          
169200     MOVE ZERO       TO RESP-KVRADER                                      
169300                                                                          
169400     MOVE +0                   TO RADIND                                  
169500                                                                          
169600     MOVE LOW-VALUES           TO W-WDQ301KY-MIN-X                        
169700     MOVE LOW-VALUES           TO W-WDQ3D1KY-MIN-X                        
169800     MOVE LOW-VALUES           TO W-WDE6C1KY-MIN-X                        
169900                                                                          
170000     MOVE HIGH-VALUES          TO W-WDQ301KY-MAX-X                        
170100     MOVE HIGH-VALUES          TO W-WDQ3D1KY-MAX-X                        
170200     MOVE HIGH-VALUES          TO W-WDE6C1KY-MAX-X                        
170300                                                                          
170400     PERFORM S21-INIT-WS-FIELDS                                           
170500     INITIALIZE TMS-W403TMS1                                              
170600     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
170700     ACCEPT WS-TIDPUNKT                   FROM TIME                       
170800     ACCEPT WS-TID-W                      FROM TIME                       
170900     ACCEPT WS-VOR-TID-BRIST              FROM TIME                       
171000                                                                          
171100     PERFORM S08-HAMTA-MASKINDATUM                                        
171200     MOVE DAT-TIAAMMDD                  TO DAGDAT-AAMMD                   
171300     MOVE DAT-TIAAVVD                   TO DAGDAT-AAVVD                   
171400                                                                          
171500     MOVE JA                            TO INDATA-SW                      
171600     MOVE JA                            TO INIT-SW                        
171700     MOVE JA                            TO KEYS-SW                        
171800     MOVE NEJ                           TO RADER-KVAR-SW                  
171900     MOVE NEJ                           TO SW-RESTART                     
172000     MOVE NEJ                           TO SW-FLAUTFAK                    
172100     MOVE NEJ                           TO FL-420-SEGMENT                 
172200                                                                          
172300     MOVE '2'                           TO 2191-SPRAK                     
172400     MOVE '1'                           TO W-KDSEGKEY                     
172500     MOVE RAETT                         TO WS-INDATA-TEST                 
172600                                           WS-BEHANDLING-TEST             
172700                                                                          
172800     IF REQU-IDDC-KEY = 'NU' OR '++'                                      
172900* ---- FIX FOR REQUEST ERRORS                                             
173000       MOVE ERR-WRONG-KEY               TO RESP-IDELMT-ERROR              
173100       MOVE NEJ                         TO KEYS-SW INIT-SW                
173200     ELSE                                                                 
173300       IF REQU-KDPGMACT = 'S' OR 'E'                                      
173400         MOVE REQU-IDDC-KEY             TO RESP-IDDC-KEY                  
173500         MOVE REQU-IDDC-KEY             TO W-IDDC-B6                      
173600         PERFORM IMS-GU-WDB601                                            
173700       ELSE                                                               
173800         MOVE SYSTEM-ERROR              TO RESP-IDMSG-ERROR               
173900         MOVE NEJ                       TO KEYS-SW INIT-SW                
174000       END-IF                                                             
174100     END-IF                                                               
174200                                                                          
174300     IF INIT-OK                                                           
174400       MOVE '011'                       TO MSGI-KDCALL                    
174500       MOVE DCS-IDTIDZON                TO MSGI-IDTIDZON                  
174500       MOVE DCS-IDDC                    TO MSGI-IDDC                      
174600       MOVE WS-DAGENS-DATUM             TO MSGI-TILOKDAT                  
174700       MOVE WS-TIDPUNKT                 TO MSGI-TILOKTID                  
174800       CALL WL01TIDZ USING MSGI-WL01TIDZ                                  
174900       MOVE MSGI-TILOKDAT(1:6)          TO WS-DAGENS-DATUM                
175000       MOVE MSGI-TILOKTID(1:4)          TO WS-TIDPUNKT(1:4)               
175100       MOVE FUNCTION CURRENT-DATE(13:2) TO WS-TIDPUNKT(5:2)               
175200     END-IF                                                               
175300     .                                                                    
175400     SKIP2                                                                
175500 B-KONTROLL-KEYS          SECTION.                                        
175600     MOVE 'STA B-KONTROLL-KEYS      '  TO WS-CURRENT-SECTION              
175700                                                                          
175800     MOVE JA TO IDTRPTNR-TIRFS-SW                                         
175900     MOVE JA TO IDPRC-IDLOTNR-SW                                          
176000                                                                          
176100***  KONTROLL AV REQU-KDPGMACT                                            
176200     IF REQU-KDPGMACT = 'S' OR 'E'                                        
176300        CONTINUE                                                          
176400     ELSE                                                                 
176500        MOVE IS-INVALID         TO RESP-IDMSG-ERROR                       
176600        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
176700        MOVE NEJ                TO KEYS-SW                                
176800     END-IF                                                               
176900                                                                          
177000***  KONTROLL AV REQU-IDTRPTNR-KEY                                        
177100     IF KEYS-OK                                                           
177200       IF REQU-IDTRPTNR-KEY = ALL '+'                                     
177300         IF REQU-TIRFSDAT-KEY NOT = ALL '+'                               
177400            MOVE IS-INVALID     TO RESP-IDMSG-ERROR                       
177500            MOVE 'IDTRPTNR'     TO RESP-IDELMT-ERROR                      
177600            MOVE NEJ TO KEYS-SW                                           
177700         END-IF                                                           
177800         MOVE NEJ TO IDTRPTNR-TIRFS-SW                                    
177900       ELSE                                                               
178000         IF REQU-IDTRPTNR-KEY NUMERIC                                     
178100            IF REQU-IDTRPTNR-KEY > ZERO                                   
178200              MOVE REQU-IDTRPTNR-KEY  TO RESP-IDTRPTNR-KEY                
178300              MOVE JA TO IDTRPTNR-TIRFS-SW                                
178400            ELSE                                                          
178500              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
178600              MOVE 'IDTRPTNR'   TO RESP-IDELMT-ERROR                      
178700              MOVE NEJ TO KEYS-SW                                         
178800            END-IF                                                        
178900         ELSE                                                             
179000            MOVE MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                    
179100            MOVE 'IDTRPTNR'     TO RESP-IDELMT-ERROR                      
179200            MOVE NEJ TO KEYS-SW                                           
179300         END-IF                                                           
179400       END-IF                                                             
179500     END-IF                                                               
179600                                                                          
179700***  KONTROLL AV REQU-TIRFSDAT-KEY                                        
179800     IF KEYS-OK                                                           
179900       IF REQU-TIRFSDAT-KEY = ALL '+'                                     
180000         IF REQU-IDTRPTNR-KEY NOT = ALL '+'                               
180100            MOVE IS-INVALID     TO RESP-IDMSG-ERROR                       
180200            MOVE 'TIRFSDAT'     TO RESP-IDELMT-ERROR                      
180300            MOVE NEJ TO KEYS-SW                                           
180400         END-IF                                                           
180500         MOVE NEJ TO IDTRPTNR-TIRFS-SW                                    
180600       ELSE                                                               
180700         IF REQU-TIRFSDAT-KEY NUMERIC                                     
180800            IF REQU-TIRFSDAT-KEY > ZERO                                   
180900              MOVE REQU-TIRFSDAT-KEY    TO RESP-TIRFSDAT-KEY              
181000              MOVE JA TO IDTRPTNR-TIRFS-SW                                
181100            ELSE                                                          
181200              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
181300              MOVE 'TIRFSDAT'   TO RESP-IDELMT-ERROR                      
181400              MOVE NEJ TO KEYS-SW                                         
181500            END-IF                                                        
181600         ELSE                                                             
181700            MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                      
181800            MOVE 'TIRFSDAT'     TO RESP-IDELMT-ERROR                      
181900            MOVE NEJ TO KEYS-SW                                           
182000         END-IF                                                           
182100       END-IF                                                             
182200     END-IF                                                               
182300                                                                          
182400***  KONTROLL AV REQU-TIRFSTID-KEY                                        
182500     IF KEYS-OK                                                           
182600       IF REQU-TIRFSTID-KEY = ALL '+'                                     
182700         IF REQU-TIRFSDAT-KEY NOT = ALL '+'                               
182800            MOVE IS-INVALID        TO RESP-IDMSG-ERROR                    
182900            MOVE 'TIRFSTID'        TO RESP-IDELMT-ERROR                   
183000            MOVE NEJ TO KEYS-SW                                           
183100         END-IF                                                           
183200         MOVE NEJ TO IDTRPTNR-TIRFS-SW                                    
183300       ELSE                                                               
183400         IF REQU-TIRFSTID-KEY NUMERIC                                     
183500            MOVE REQU-TIRFSTID-KEY TO RESP-TIRFSTID-KEY                   
183600            MOVE JA TO IDTRPTNR-TIRFS-SW                                  
183700         ELSE                                                             
183800            MOVE MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                    
183900            MOVE 'TIRFSTID'        TO RESP-IDELMT-ERROR                   
184000            MOVE NEJ TO KEYS-SW                                           
184100         END-IF                                                           
184200       END-IF                                                             
184300     END-IF                                                               
184400                                                                          
184500***  KONTROLL AV REQU-IDPRC-KEY                                           
184600     IF KEYS-OK                                                           
184700       IF REQU-IDPRC-KEY = ALL '+'                                        
184800         IF REQU-IDLOTNR-KEY NOT = ALL '+'                                
184900           MOVE IS-INVALID      TO RESP-IDMSG-ERROR                       
185000           MOVE 'IDPRC'         TO RESP-IDELMT-ERROR                      
185100           MOVE NEJ TO KEYS-SW                                            
185200         END-IF                                                           
185300         MOVE NEJ TO IDPRC-IDLOTNR-SW                                     
185400       ELSE                                                               
185500         IF REQU-IDPRCBAS NUMERIC                                         
185600            IF REQU-IDPRCBAS > ZERO                                       
185700              MOVE REQU-IDPRC-KEY TO RESP-IDPRC-KEY                       
185800              MOVE JA TO IDPRC-IDLOTNR-SW                                 
185900            ELSE                                                          
186000              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
186100              MOVE 'IDPRC'      TO RESP-IDELMT-ERROR                      
186200              MOVE NEJ TO KEYS-SW                                         
186300            END-IF                                                        
186400         ELSE                                                             
186500            MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                      
186600            MOVE 'IDPRC'         TO RESP-IDELMT-ERROR                     
186700            MOVE NEJ TO KEYS-SW                                           
186800         END-IF                                                           
186900       END-IF                                                             
187000     END-IF                                                               
187100                                                                          
187200***  KONTROLL AV REQU-IDLOTNR-KEY                                         
187300     IF KEYS-OK                                                           
187400       IF REQU-IDLOTNR-KEY = ALL '+'                                      
187500         IF REQU-IDPRC-KEY NOT = ALL '+'                                  
187600           IF REQU-IDPRCBAS NUMERIC                                       
187700             MOVE IS-INVALID    TO RESP-IDMSG-ERROR                       
187800             MOVE 'IDLOTNR'     TO RESP-IDELMT-ERROR                      
187900             MOVE NEJ TO KEYS-SW                                          
188000           END-IF                                                         
188100         END-IF                                                           
188200         MOVE NEJ TO IDPRC-IDLOTNR-SW                                     
188300       ELSE                                                               
188400         IF REQU-IDLOTNR-KEY NUMERIC                                      
188500            IF REQU-IDLOTNR-KEY  > ZERO                                   
188600              MOVE REQU-IDLOTNR-KEY TO RESP-IDLOTNR-KEY                   
188700              MOVE REQU-IDLOTNR-KEY TO WS-REQU-IDLOTNR                    
188800              MOVE JA TO IDPRC-IDLOTNR-SW                                 
188900            ELSE                                                          
189000              MOVE ERR-SHOULD-NOT-BE-ZERO  TO RESP-IDMSG-ERROR            
189100              MOVE 'IDLOTNR'    TO RESP-IDELMT-ERROR                      
189200              MOVE NEJ TO KEYS-SW                                         
189300            END-IF                                                        
189400         ELSE                                                             
189500            MOVE MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                     
189600            MOVE 'IDLOTNR'        TO RESP-IDELMT-ERROR                    
189700            MOVE NEJ TO KEYS-SW                                           
189800         END-IF                                                           
189900       END-IF                                                             
190000     END-IF                                                               
190100                                                                          
190200     IF KEYS-OK                                                           
190300       IF IDTRPTNR-TIRFS-IFYLLD                                           
190400       AND IDPRC-IDLOTNR-IFYLLD                                           
190500                                                                          
190600         MOVE INVALID-KEY-FIELDS   TO RESP-IDMSG-ERROR                    
190700         MOVE 'IDTRPTNR'           TO RESP-IDELMT-ERROR                   
190800         MOVE NEJ TO KEYS-SW                                              
190900         PERFORM S15-RENSA-RAD                                            
191000                                                                          
191100       ELSE                                                               
191200         IF IDTRPTNR-TIRFS-TOM                                            
191300         AND IDPRC-IDLOTNR-TOM                                            
191400           IF REQU-KDPGMACT = 'E'                                         
191500             MOVE ERR-NO-DATA-ENTERED    TO RESP-IDMSG-INFO               
191600           ELSE                                                           
191700             MOVE 'IDPRC'                TO RESP-IDELMT-ERROR             
191800             MOVE KEYS-ARE-MISSING       TO RESP-IDMSG-ERROR              
191900           END-IF                                                         
192000           MOVE NEJ TO KEYS-SW                                            
192100           PERFORM S15-RENSA-RAD                                          
192200         END-IF                                                           
192300       END-IF                                                             
192400     END-IF                                                               
192500                                                                          
192600*      END-IF                                                             
192700     .                                                                    
192800     SKIP2                                                                
192900 C-KONTROLL-READ-DB         SECTION.                                      
193000     MOVE 'STA C-KONTROLL-READ-DB' TO WS-CURRENT-SECTION                  
193100     MOVE JA       TO INDATA-SW                                           
193200                                                                          
193300                                                                          
193400     IF REQU-KVRADER = ALL '+'                                            
193500       MOVE ZERO                    TO RESP-KVRADER                       
193600       MOVE ZERO                    TO WS-REQU-KVRADER                    
193700     ELSE                                                                 
193800       IF REQU-KVRADER NUMERIC                                            
193900         MOVE REQU-KVRADER          TO RESP-KVRADER                       
194000         MOVE REQU-KVRADER          TO WS-REQU-KVRADER                    
194100       ELSE                                                               
194200         MOVE ZERO                  TO RESP-KVRADER                       
194300         MOVE ZERO                  TO WS-REQU-KVRADER                    
194400       END-IF                                                             
194500     END-IF                                                               
194600                                                                          
194700       MOVE +1                 TO RADIND                                  
194800                                                                          
194900     IF WS-REQU-KVRADER = ZERO                                            
195000       MOVE +501            TO RADIND                                     
195100     END-IF                                                               
195200                                                                          
195300     PERFORM UNTIL RADIND > WS-MAX-500-RADER                              
195400                                                                          
195500       IF REQU-IDPRODNR(RADIND)  = LOW-VALUE                              
195600       OR REQU-IDPRODNR(RADIND)  = ALL '+'                                
195700       OR REQU-IDKOLLI (RADIND)  = LOW-VALUE                              
195800       OR REQU-IDKOLLI (RADIND)  = ALL '+'                                
195900          MOVE +501            TO RADIND                                  
196000       ELSE                                                               
196100                                                                          
196200         IF INDATA-OK                                                     
196300           PERFORM CE-CHECK-KDKOLSTA                                      
196400         END-IF                                                           
196500                                                                          
196600         IF INDATA-OK                                                     
196700           PERFORM CD-CHECK-KDKOLLI                                       
196800         END-IF                                                           
196900                                                                          
197000         IF INDATA-OK                                                     
197100           MOVE REQU-IDPRODNR(RADIND) TO RESP-IDPRODNR(RADIND)            
197200           MOVE REQU-IDPLKLST(RADIND) TO RESP-IDPLKLST(RADIND)            
197300                                                                          
197400           MOVE REQU-IDKOLLI (RADIND) TO RESP-IDKOLLI (RADIND)            
197500           MOVE REQU-KDKOLLI (RADIND) TO RESP-KDKOLLI (RADIND)            
197600                                                                          
197700           IF REQU-KDKOLLI-IN(RADIND) = ALL '+'                           
197800             MOVE SPACE         TO RESP-KDKOLLI-IN(RADIND)                
197900           ELSE                                                           
198000             MOVE REQU-KDKOLLI-IN(RADIND)                                 
198100               TO RESP-KDKOLLI-IN(RADIND)                                 
198200           END-IF                                                         
198300         ELSE                                                             
198400            MOVE +501            TO RADIND                                
198500         END-IF                                                           
198600       END-IF                                                             
198700       ADD +1                TO RADIND                                    
198800     END-PERFORM                                                          
198900                                                                          
199000     IF RADIND = 501                                                      
199100       MOVE ZERO               TO RADIND                                  
199200     END-IF                                                               
199300     .                                                                    
199400     SKIP2                                                                
199500 CD-CHECK-KDKOLLI                  SECTION.                               
199600     MOVE 'STA CD-CHECK-KDKOLLI'   TO WS-CURRENT-SECTION                  
199700                                                                          
199800     IF REQU-KDKOLLI-IN(RADIND) NOT = ALL '+'                             
199900       MOVE REQU-KDKOLLI-IN(RADIND)    TO W-KDKOLLI                       
200000     ELSE                                                                 
200100       MOVE REQU-KDKOLLI (RADIND)      TO W-KDKOLLI                       
200200     END-IF                                                               
200300                                                                          
200400     PERFORM IMS-01-GU-WDK501                                             
200500     IF SEGMENT-MISSING                                                   
200600        MOVE 'KDKOLLI'       TO RESP-IDELMT-ERROR                         
200700        MOVE IS-INVALID      TO RESP-IDMSG-ERROR                          
200800        MOVE 'ERR'           TO RESP-IDMSG-ERROR-RAD (RADIND)             
200900        MOVE NEJ             TO INDATA-SW                                 
201000     ELSE                                                                 
201100        MOVE JA              TO INDATA-SW                                 
201200        MOVE SPACE           TO RESP-IDMSG-ERROR-RAD (RADIND)             
201300                                                                          
201400        MOVE EMB-KDEMBTYP       TO WS-EMB-KDEMBTYP (RADIND)               
201500        MOVE EMB-VKTARA         TO WS-EMB-VKTARA   (RADIND)               
201600        MOVE EMB-DIKOLLIL       TO WS-EMB-DIKOLLIL (RADIND)               
201700        MOVE EMB-DIKOLLIB       TO WS-EMB-DIKOLLIB (RADIND)               
201800        MOVE EMB-DIKOLLIH       TO WS-EMB-DIKOLLIH (RADIND)               
201900                                                                          
202000        COMPUTE WS-KOLLI-VLORDBTO ROUNDED =                               
202100        WS-EMB-DIKOLLIL (RADIND) * WS-EMB-DIKOLLIB (RADIND) *             
202200                    WS-EMB-DIKOLLIH (RADIND)                              
202300        END-COMPUTE                                                       
202400     END-IF                                                               
202500                                                                          
202600*LK** VOLUME CONTROL *                                                    
202700     PERFORM IMS-GU-WDE401-ESEQ                                           
202800     MOVE KORD-IDDISTR      TO W-401-IDDISTR                              
202900                               TEST-IDDISTR                               
203000     MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                             
203100     MOVE KORD-IDORDNR5     TO W-401-IDORDNR                              
203200                                                                          
203300                                                                          
203400     IF NOT DIST44-SCRAP-DIST                                             
203500       MOVE REQU-IDDC-KEY       TO VOL-IDDC                               
203600       IF CONTROL-OF-CASE-NET-VOLUME                                      
203700                                                                          
203800         PERFORM IMS-17-GU-WDE401                                         
203900         PERFORM IMS-19-GNP-WDE411                                        
204000                                                                          
204100         PERFORM UNTIL SEGMENT-MISSING OR INDATA-FEL                      
204200           COMPUTE WS-ORAD-VLORDNTO ROUNDED =                             
204300                       ORAD-KVAVBART * ORAD-VLARTNTO                      
204400           END-COMPUTE                                                    
204500                                                                          
204600           ADD WS-ORAD-VLORDNTO     TO WS-ORAD-VLORDNTO-SUM               
204700                                                                          
204800           IF WS-ORAD-VLORDNTO-SUM > WS-KOLLI-VLORDBTO                    
204900                                                                          
205000             MOVE NEJ         TO INDATA-SW                                
205100             MOVE 'VLART'     TO RESP-IDELMT-ERROR                        
205200             MOVE '117'       TO RESP-IDMSG-ERROR                         
205300             MOVE 'VOL'       TO RESP-IDMSG-ERROR-RAD (RADIND)            
205400           END-IF                                                         
205500                                                                          
205600           MOVE ZERO          TO WS-ORAD-VLORDNTO                         
205700           PERFORM IMS-19-GNP-WDE411                                      
205800         END-PERFORM                                                      
205900         MOVE ZERO            TO WS-ORAD-VLORDNTO-SUM                     
206000       END-IF                                                             
206100     END-IF                                                               
206200     .                                                                    
206300     SKIP2                                                                
206400 CE-CHECK-KDKOLSTA  SECTION.                                              
206500     MOVE 'CE-CHECK-KDKOLSTA' TO WS-CURRENT-SECTION                       
206600                                                                          
206700     MOVE REQU-IDPRODNR(RADIND) TO W-IDPRODNR                             
206800                                   W-401-IDPRODNR                         
206900     MOVE REQU-IDPLKLST(RADIND) TO W-401-IDPLKLST                         
207000                                                                          
207100     PERFORM IMS-02-GU-WDE601                                             
207200     IF SEGMENT-FOUND                                                     
207300       MOVE REQU-IDKOLLI(RADIND)    TO W-IDKOLLI                          
207320       PERFORM IMS-03-GNP-WDE611                                          
207330         IF SEGMENT-FOUND                                                 
207340            IF KOLLI-KDKOLSTA > ZERO                                      
207350              MOVE 'IDPRODNR' TO RESP-IDELMT-ERROR                        
207360              MOVE ERR-CASE-ALREADY-REPORT TO RESP-IDMSG-ERROR            
207370              MOVE NEJ       TO INDATA-SW                                 
207380            END-IF                                                        
207386         END-IF                                                           
207387     END-IF                                                               
207388     IF SEGMENT-MISSING                                                   
208100       MOVE ERR-ORDER-PARTS-MISSING       TO RESP-IDMSG-ERROR             
208200       MOVE 'IDPRODNR'       TO RESP-IDELMT-ERROR                         
208300       MOVE 'ERR'            TO RESP-IDMSG-ERROR-RAD (RADIND)             
208400       MOVE NEJ              TO INDATA-SW                                 
208500     END-IF                                                               
208600     .                                                                    
208700     SKIP2                                                                
210900 E-UPDATE-IDKOLLI          SECTION.                                       
211000     MOVE 'E-UPDATE-IDKOLLI'  TO WS-CURRENT-SECTION                       
211100                                                                          
211200       MOVE +1                 TO RADIND                                  
211300     PERFORM UNTIL RADIND > WS-MAX-500-RADER                              
211400                                                                          
211500       IF REQU-IDPRODNR(RADIND)  = LOW-VALUE                              
211600       OR REQU-IDPRODNR(RADIND)  = ALL '+'                                
211700          CONTINUE                                                        
211800       ELSE                                                               
211900         IF REQU-IDKOLLI(RADIND) = LOW-VALUE                              
212000         OR REQU-IDKOLLI(RADIND) = ALL '+'                                
212100            CONTINUE                                                      
212200         ELSE                                                             
212300           PERFORM EA-FLYTTA-NYCKLAR                                      
212400                                                                          
212500           PERFORM ED-BEHANDLA-RADER                                      
212600                                                                          
212700           PERFORM EE-HAMTA-ADRESS                                        
212800           PERFORM EF-UPPDATERA-KOLLIREG                                  
212900                                                                          
213000           PERFORM EG-EV-UPDATE-4726-4727                                 
213100                                                                          
213200           PERFORM EI-BEHANDLA-INTERVALL                                  
213300                                                                          
213400           IF ORAPPORTERADE-RADER-SAKNAS                                  
213500              PERFORM EJ-UPDATE-KUNDORDER                                 
213600           END-IF                                                         
213700                                                                          
213800           PERFORM EK-UPDATE-KOLLIREG                                     
213900                                                                          
214000           IF ORAPPORTERADE-RADER-SAKNAS                                  
214100              PERFORM EL-UPDATE-ORDERKO                                   
214200           END-IF                                                         
214300                                                                          
214400           PERFORM EM-AVSLUT                                              
214500         END-IF                                                           
214600       END-IF                                                             
214700       PERFORM S21-INIT-WS-FIELDS                                         
214800       ADD +1      TO RADIND                                              
214900     END-PERFORM                                                          
215000                                                                          
215100     IF RADIND = 501                                                      
215200       MOVE NEJ                     TO RADER-KVAR-SW                      
215300     END-IF                                                               
215400     MOVE INF-UPDATED               TO RESP-IDMSG-INFO                    
215500                                                                          
215600     PERFORM S15-RENSA-RAD                                                
215700     .                                                                    
215800     SKIP2                                                                
215900 EA-FLYTTA-NYCKLAR  SECTION.                                              
216000     MOVE 'EA-FLYTTA-NYCKLAR' TO WS-CURRENT-SECTION                       
216100                                                                          
216200     MOVE REQU-IDDC-KEY             TO W-IDDC                             
216300     MOVE REQU-IDDC-KEY             TO W-IDDC-B6                          
216400                                       WS-DCUSER-IDDC                     
216500     MOVE REQU-IDPRODNR(RADIND)     TO WS-IDPRODNR                        
216600                                       W-IDPRODNR                         
216700                                       WS-IDPRODNR                        
216800                                       WS-JFR-IDPRODNR                    
216900                                       WS-IDPRODNR-RED                    
217000                                       W-IDPRODNR                         
217100                                       W-401-IDPRODNR                     
217200     MOVE REQU-IDPLKLST(RADIND)     TO WS-IDPLKLST                        
217300                                       W-401-IDPLKLST                     
217400                                       WS-IDPLKLST                        
217500                                       W-IDPLKLST-SOEK                    
217600                                       W-IDPLKLST-Q3D1                    
217700                                       W-IDPLKLST-Q3D1-MAX                
217800                                       W-401-IDPLKLST                     
217900     MOVE REQU-IDKOLLI(RADIND)      TO W-IDKOLLI                          
218000*                                                                         
218100     PERFORM IMS-02-GU-WDE601                                             
218200*                                                                         
218300     MOVE VORD-VKORDNTO             TO WS-VKORDBTO                        
218400     MOVE VORD-VLORDNTO             TO WS-VLORDBTO                        
218500***************                                                           
218600     MOVE REQU-IDKOLLI(RADIND)      TO WS-IDKOLLI                         
218700                                       WS-IDKOLLI-NUM                     
218800                                       W-IDKOLLI                          
218900*                                                                         
219000     PERFORM IMS-03-GNP-WDE611                                            
219100*                                                                         
219200     IF REQU-KDKOLLI-IN (RADIND) = ALL '+'                                
219300        MOVE KOLLI-KDKOLLI      TO W-KDKOLLI                              
219400        MOVE KOLLI-KDKOLLI      TO WS-KDKOLLI                             
219500                                                                          
219600        PERFORM IMS-01-GU-WDK501                                          
219700        IF SEGMENT-FOUND                                                  
219800*           ADD EMB-VKTARA      TO WS-VKORDBTO                            
219900           MOVE EMB-DIKOLLIL    TO KOLLI-DIKOLLIL                         
220000           MOVE EMB-DIKOLLIB    TO KOLLI-DIKOLLIB                         
220100           MOVE EMB-DIKOLLIH    TO KOLLI-DIKOLLIH                         
220200        END-IF                                                            
220300     ELSE                                                                 
220400                                                                          
220500       IF SEGMENT-FOUND                                                   
220600         MOVE KOLLI-KDKOLLI     TO W-KDKOLLI                              
220700                                                                          
220800         PERFORM IMS-01-GU-WDK501                                         
220900         MOVE EMB-VKTARA            TO WS-OLD-EMB-VKTARA(RADIND)          
221000                                                                          
221100*GET NEW TARA WEIGHT TO ADD TO CASE E611 AND VORD E601                    
221200         MOVE REQU-KDKOLLI-IN (RADIND) TO W-KDKOLLI                       
221300         MOVE REQU-KDKOLLI-IN (RADIND) TO WS-KDKOLLI                      
221400         PERFORM IMS-01-GU-WDK501                                         
221500                                                                          
221600         MOVE EMB-VKTARA            TO WS-NEW-EMB-VKTARA(RADIND)          
221700         MOVE WS-EMB-DIKOLLIL (RADIND) TO KOLLI-DIKOLLIL                  
221800         MOVE WS-EMB-DIKOLLIB (RADIND) TO KOLLI-DIKOLLIB                  
221900         MOVE WS-EMB-DIKOLLIH (RADIND) TO KOLLI-DIKOLLIH                  
222000       END-IF                                                             
222100     END-IF                                                               
222200*                                                                         
222300       COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                             
222400       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
222500       END-COMPUTE                                                        
222600*                                                                         
222700     MOVE KOLLI-KDEMBTYP            TO WS-KDEMBTYP                        
222800*     ADD KOLLI-VKORDNTO-KOLLI      TO WS-VKORDBTO                        
222900      ADD KOLLI-VLORDBTO-KOLLI      TO WS-VLORDBTO                        
223000*                                                                         
223100***************                                                           
223200     PERFORM IMS-GU-WDE401-ESEQ                                           
223300*                                                                         
223400     MOVE KORD-IDDISTR              TO WS-IDDISTR-NUM                     
223500                                       W-401-IDDISTR                      
223600                                       WS-IDDISTR                         
223700                                       WS-IDDISTR-NUM                     
223800                                       WS-IDDISTR-NUM4                    
223900                                       WS-SAVE-IDDISTR                    
224000     MOVE KORD-IDKUNDNR             TO W-401-IDKUNDNR                     
224100                                       WS-IDKUNDNR                        
224200                                       WS-IDKUNDNR-NUM                    
224300                                       WS-SAVE-IDKUNDNR                   
224400     MOVE KORD-IDORDNR5             TO WS-IDORDNR                         
224500                                       W-401-IDORDNR                      
224600     MOVE KORD-IDORDER              TO WS-IDORDER                         
224700     .                                                                    
224800     SKIP2                                                                
224900                                                                          
225000 ED-BEHANDLA-RADER     SECTION.                                           
225100     MOVE 'ED-BEHANDLA-RADER' TO WS-CURRENT-SECTION                       
225200                                                                          
225300     PERFORM EDA-BEHANDLA-RAD-I-INTERVALL                                 
225400     IF WS-BEHANDLING-RATT                                                
225500        MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                               
225600        IF NOT DIST19-SATS                                                
225700           PERFORM EDB-UPPDATERA-PRODTAB                                  
225800        END-IF                                                            
225900     END-IF                                                               
226000     .                                                                    
226100     EJECT                                                                
226200 EDA-BEHANDLA-RAD-I-INTERVALL SECTION.                                    
226300     MOVE 'EDA-BEHANDLA-RAD-I-INTERV' TO WS-CURRENT-SECTION               
226400                                                                          
226500     MOVE ZERO                        TO WS-RINT-ANT-FPACK-ORAD           
226600                                                                          
226700     PERFORM IMS-18-GHU-WDE401                                            
226800     MOVE KORD-IDDISTR           TO  WS-KORD-IDDISTR                      
226900     MOVE KORD-IDDISTR           TO  WS-IDDISTR-NUM                       
227000                                                                          
227100     MOVE KORD-IDKUNDRF          TO  WS-IDKUNDRF                          
227200     MOVE KORD-IDORDER           TO  WS-IDORDER                           
227300     MOVE KORD-TIORDREG          TO  WS-TIORDREG                          
227400     MOVE KORD-KDFRAKT           TO  WS-KDFRAKT                           
227500     MOVE KORD-KDORDKL           TO  WS-KDORDKL                           
227600     MOVE KORD-KDFAKTYP          TO  WS-KDFAKTYP                          
227700                                                                          
227800     MOVE KORD-IDORDER           TO WS-SPAR-IDORDER                       
227900     MOVE KORD-IDORDER           TO WS-IDORDER                            
228000     MOVE KORD-IDDC              TO WS-SPAR-IDDC                          
228100     PERFORM IMS-19-GHNP-WDE411                                           
228200     IF SEGMENT-FOUND                                                     
228300       MOVE ORAD-IDPURAD         TO WS-IDPURAD                            
228400     END-IF                                                               
228500*                                                                         
228600     MOVE KORD-IDORDER           TO W-201-IDORDER                         
228700     PERFORM IMS-GU-ORQI01                                                
228800                                                                          
228900*                                                                         
229000     PERFORM UNTIL SEGMENT-MISSING                                        
229100                                                                          
229200       PERFORM EDAB-UPPDATERA-RAD                                         
229300                                                                          
229400       PERFORM EDAC-LAGG-UPP-KOLLI-KOPPL                                  
229500                                                                          
229600       PERFORM IMS-19-GHNP-WDE411                                         
229700                                                                          
229800     END-PERFORM                                                          
229900                                                                          
230000     .                                                                    
230100     EJECT                                                                
230200 EDAB-UPPDATERA-RAD         SECTION.                                      
230300     MOVE 'EDAB-UPPDATERA-RAD'        TO WS-CURRENT-SECTION               
230400                                                                          
230500     MOVE ORAD-VKARTNTO               TO SPAR-PRAD-VKARTNTO               
230600     MOVE ORAD-KVFLAMP                TO SPAR-PRAD-KVFLAMP                
230700     MOVE ORAD-KDFARLIG               TO SPAR-PRAD-KDFARLIG               
230800     MOVE ORAD-PRARTNTO               TO SPAR-PRAD-PRARTNTO               
230900     MOVE ORAD-PRAVCOST               TO SPAR-PRAD-PRAVCOST               
231000     MOVE ORAD-PRARTNTO-LOC           TO SPAR-PRAD-PRARTNTO-LOC           
231100     MOVE ORAD-PRARTNTO-LOCPREL     TO  SPAR-PRAD-PRARTNTO-LOCPREL        
231200*                                                                         
231300     MOVE ORAD-KDVALISO               TO SPAR-KDVALISO                    
231400     MOVE ORAD-KDVALISO-EXP           TO SPAR-KDVALISO-EXP                
231500     MOVE ORAD-KDVALISO               TO ARB-KOLLI-KDVALISO               
231600     MOVE ORAD-KDVALISO-EXP           TO ARB-KOLLI-KDVALISO-EXP           
231700*                                                                         
231800     MOVE ORAD-IDPSN                  TO SPAR-IDPSN                       
231900     MOVE ORAD-VKART-FG               TO SPAR-VKART-FG                    
232000     MOVE ORAD-VLFG                   TO SPAR-VLFG                        
232100     MOVE ORAD-SUEQFG                 TO SPAR-SUEQFG                      
232200     MOVE ORAD-FLDIRLEV               TO SPAR-FLDIRLEV                    
232300*                                                                         
232400     MOVE ORAD-KVAVBART               TO SPAR-PRAD-KVLEVART               
232500     PERFORM S10-UPPD-SPAR-KOLLI                                          
232600                                                                          
232700*REDAN MOVAT I WL013440.                                                  
232800*    MOVE ORAD-KVAVBART        TO  ORAD-KVLEVART                          
232900                                                                          
233000     IF ORAD-KVLEVART = ORAD-KVAVBART                                     
233100        MOVE +4              TO ORAD-KDRADSTA                             
233200        MOVE 'Y'             TO DATUM-SW                                  
233300                                                                          
233400        ADD 1 TO WS-ANT-RADER-ORDER                                       
233500        ADD 1 TO WS-RINT-ANT-FPACK-ORAD                                   
233600        IF KORD-KDORDKL = +0                                              
233700           PERFORM EDABA-UPPDATERA-VOR-TIKLAR                             
233800        END-IF                                                            
233900     END-IF                                                               
234000                                                                          
234100     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
234200     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
234300     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
234400     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
234500     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
234600     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
234700                                                                          
234800     PERFORM IMS-22-REPL-WDE411                                           
234900     PERFORM EDABB-EV-SKAPA-RYK-TRANS                                     
235000     .                                                                    
235100     EJECT                                                                
235200 EDABA-UPPDATERA-VOR-TIKLAR SECTION.                                      
235300     MOVE 'STA EDABA-TIKLAR'       TO WS-CURRENT-SECTION                  
235400                                                                          
235500     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
235600     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
235700     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
235800                                    W-A601KY-MAX-IDDISTR                  
235900     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
236000                                    W-A601KY-MAX-IDKUNDNR                 
236100     MOVE SPACE                  TO W-A601KY-MIN-IDKUNDRF                 
236200                                    W-A601KY-MAX-IDKUNDRF                 
236300     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
236400                                    W-A601KY-MAX-IDORDNR                  
236500                                                                          
236600     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
236700                                                                          
236800     PERFORM IMS-GHN-SEQB-WDA601                                          
236900     PERFORM UNTIL SEGMENT-MISSING                                        
237000                OR SEGMENT-END                                            
237100                OR SW-TIKLAR-UPPDATERAD = JA                              
237200                                                                          
237300         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
237400         AND VOR-TIKLAR = +0                                              
237500                                                                          
237600             MOVE WS-DAGENS-DATUM   TO VOR-TIKLAR                         
237700             MOVE WS-TIDPUNKT       TO WS-TIDPUNKT-RED                    
237800             MOVE WS-HHMMSS         TO VOR-TIKLATID                       
237900             PERFORM IMS-REPL-SEQB-WDA601                                 
238000             MOVE JA                TO SW-TIKLAR-UPPDATERAD               
238100         END-IF                                                           
238200                                                                          
238300         PERFORM IMS-GHN-SEQB-WDA601                                      
238400     END-PERFORM                                                          
238500     MOVE 'END EDABA-TIKLAR'       TO WS-CURRENT-SECTION                  
238600     .                                                                    
238700 EDABB-EV-SKAPA-RYK-TRANS SECTION.                                        
238800     MOVE 'STA EDABB-EV-SKAPA'     TO WS-CURRENT-SECTION                  
238900                                                                          
239000     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
239100        ORAD-TIRODAT         > ZERO                                       
239200                                                                          
239300       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
239400       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
239500       ADD +1 TO LOGG-IDLOGLOP                                            
239600       IF LOGG-IDLOGLOP = 0                                               
239700         ADD +1 TO LOGG-TIKLOCK                                           
239800         MOVE +1 TO LOGG-IDLOGLOP                                         
239900       END-IF                                                             
240000                                                                          
240100       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
240200                                       W-WDQ2C-IDDISTR                    
240300       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
240400                                       W-WDQ2C-IDKUNDNR                   
240500       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
240600                                  TO   W-WDQ2C-IDORDNR5                   
240700                                                                          
240800       PERFORM IMS-GU-ORQI01-CSEQ-ORQL                                    
240900       MOVE    OHUV-IDORDER       TO   RYK-IDORDER                        
241000       MOVE    'RYK'              TO   RYK-IDPTYP                         
241100                                       LOGG-IDPTYP                        
241200                                                                          
241300       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
241400       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
241500       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
241600       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
241700       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
241800       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
241900       MOVE    ZERO               TO   RYK-KDORDBEK                       
242000                                                                          
242100       MOVE    SPACE              TO   LOGG-SORTPOST                      
242200       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
242300                                                                          
242400       PERFORM IMS-ISRT-ZZAC01                                            
242500                                                                          
242600       PERFORM UNTIL SEGMENT-FOUND                                        
242700         ACCEPT LOGG-TIKLOCK FROM TIME                                    
242800         ADD +1 TO LOGG-IDLOGLOP                                          
242900         IF LOGG-IDLOGLOP = 0                                             
243000            ADD +1 TO LOGG-TIKLOCK                                        
243100            MOVE +1 TO LOGG-IDLOGLOP                                      
243200         END-IF                                                           
243300         PERFORM IMS-ISRT-ZZAC01                                          
243400       END-PERFORM                                                        
243500     END-IF                                                               
243600     MOVE 'END EDABB-EV-SKAPA'     TO WS-CURRENT-SECTION                  
243700     .                                                                    
243800     EJECT                                                                
243900 EDAC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                      
244000     MOVE 'EDAC-LAGG-UPP-KOLLI-KOPPL'   TO WS-CURRENT-SECTION             
244100                                                                          
244200                                                                          
244300         IF SPAR-PRAD-KDFARLIG = +4                                       
244400         OR SPAR-PRAD-KDFARLIG = +7                                       
244500             ADD +1              TO ARB-KOLLI-KVFALRAD                    
244600         END-IF                                                           
244700*                                                                         
244800                                                                          
244900     IF SPAR-IDPSN > ZERO                                                 
245000       PERFORM EDACA-SPARA-FG-DATA                                        
245100     END-IF                                                               
245200     .                                                                    
245300     EJECT                                                                
245400 EDACA-SPARA-FG-DATA SECTION.                                             
245500     MOVE 'EDACA-SPARA-FG-DATA      '   TO WS-CURRENT-SECTION             
245600     SKIP3                                                                
245700     MOVE +1 TO FG-INDX                                                   
245800     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
245900                                                                          
246000       IF TAB-IDPSN(FG-INDX) = ZERO                                       
246100         MOVE SPAR-IDPSN TO TAB-IDPSN(FG-INDX)                            
246200         PERFORM S17-BERAEKNA-FG-FAELT                                    
246300                                                                          
246400       ELSE                                                               
246500         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
246600           PERFORM S17-BERAEKNA-FG-FAELT                                  
246700         END-IF                                                           
246800       END-IF                                                             
246900                                                                          
247000       ADD +1 TO FG-INDX                                                  
247100     END-PERFORM                                                          
247200                                                                          
247300     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
247400                            (SPAR-SUEQFG      *                           
247500                             SPAR-PRAD-KVLEVART)                          
247600     END-COMPUTE                                                          
247700     .                                                                    
247800     EJECT                                                                
247900 EDB-UPPDATERA-PRODTAB      SECTION.                                      
248000     MOVE 'EDB-UPPDATERA-PRODTAB    '   TO WS-CURRENT-SECTION             
248100                                                                          
248200     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
248300*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
248400                                                                          
248500                                                                          
248600       PERFORM IMS-17-GU-WDE401                                           
248700                                                                          
248800       PERFORM EDBA-LAES-SHIFTTAB                                         
248900                                                                          
249000       MOVE KORD-IDORDER         TO W-301-IDORDER                         
249100                                    WS-IDORDER                            
249200       MOVE KORD-IDDC            TO W-301-IDDC                            
249300       MOVE KORD-IDPRODNR        TO W-301-IDPRODNR                        
249400       MOVE KORD-IDPLKLST        TO W-301-IDPLKLST                        
249500       PERFORM IMS-GU-ORQA01                                              
249600       MOVE ODEL-IDPRC           TO WS-ODEL-IDPRC                         
249700       MOVE ODEL-IDTRP           TO WS-ODEL-IDTRP                         
249800       MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                            
249900       MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                      
250000*      MOVE ODEL-VLORDNTO        TO WS-ODEL-VLORDNTO                      
250100                                                                          
250200       IF  ODEL-KDPRODKL = 'B'                                            
250300       OR  ODEL-KDPRODKL = 'C'                                            
250400*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
250500                                                                          
250600         MOVE KORD-IDDC          TO W-4471-IDDC                           
250700         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
250800         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
250900         PERFORM IMS-GHU-XXKW11                                           
251000                                                                          
251100         IF  SEGMENT-FOUND                                                
251200*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
251300                                                                          
251400           MOVE 1                TO IND1                                  
251500           MOVE W-4478-IDSHIFT   TO IND2                                  
251600           MOVE ODEL-DARFS (3:10) TO HJALP-ODEL-TIRFS                     
251700           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
251800                                                                          
251900           PERFORM UNTIL IND1 = 30 OR                                     
252000                         4472-TIRFS (IND1) = ZERO OR                      
252100                         HJALP-ODEL-TIRFS-7 = HJALP-4472-TIRFS-7          
252200             ADD 1                TO IND1                                 
252300             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
252400           END-PERFORM                                                    
252500                                                                          
252600           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
252700           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
252800           ADD WS-RINT-ANT-FPACK-ORAD                                     
252900                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
253000           PERFORM EDBB-ADDERA-TOTAL-PRODTID                              
253100           PERFORM IMS-REPL-XXKW11                                        
253200         END-IF                                                           
253300       END-IF                                                             
253400     END-IF                                                               
253500     .                                                                    
253600     EJECT                                                                
253700 EDBA-LAES-SHIFTTAB         SECTION.                                      
253800     MOVE 'STA EDBA-LAES-SHIFTTAB       '   TO WS-CURRENT-SECTION         
253900                                                                          
254000     MOVE KORD-IDDC         TO W-4477-IDDC                                
254100     MOVE '1'               TO W-4478-IDSHIFT                             
254200     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
254300     PERFORM IMS-GU-XXLB                                                  
254400*                                                                         
254500     IF SEGMENT-MISSING                                                   
254600        MOVE '2'            TO W-4478-IDSHIFT                             
254700        PERFORM IMS-GU-XXLB                                               
254800*                                                                         
254900        IF SEGMENT-MISSING                                                
255000           MOVE '3'         TO W-4478-IDSHIFT                             
255100           PERFORM IMS-GU-XXLB                                            
255200*                                                                         
255300           IF SEGMENT-MISSING                                             
255400              MOVE '1'      TO W-4478-IDSHIFT                             
255500           END-IF                                                         
255600        END-IF                                                            
255700     END-IF                                                               
255800     .                                                                    
255900     EJECT                                                                
256000 EDBB-ADDERA-TOTAL-PRODTID             SECTION.                           
256100     MOVE 'STA EDBB-ADDERA-TOTAL-PRODTID'   TO WS-CURRENT-SECTION         
256200                                                                          
256300     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
256400                                                                          
256500     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
256600                                     ODEL-KVPTID                          
256700     END-COMPUTE                                                          
256800                                                                          
256900     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
257000     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
257100     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
257200                            (WS-KVPTID-TIM * 60)                          
257300     END-COMPUTE                                                          
257400                                                                          
257500     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
257600     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
257700     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
257800     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
257900                            (WS-KVPTID-TIM * 60)                          
258000     END-COMPUTE                                                          
258100     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
258200                                                                          
258300     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
258400     .                                                                    
258500     EJECT                                                                
258600                                                                          
258700 EE-HAMTA-ADRESS SECTION.                                                 
258800     MOVE 'STA EE-HAMTA-ADRESS '            TO WS-CURRENT-SECTION         
258900                                                                          
259000     PERFORM IMS-17-GU-WDE401                                             
259100     MOVE KORD-IDDISTR    TO W-401-IDDISTR                                
259200     MOVE KORD-IDKUNDNR   TO W-401-IDKUNDNR                               
259300     MOVE KORD-IDORDNR5   TO W-401-IDORDNR                                
259400     MOVE KORD-IDPRODNR   TO W-401-IDPRODNR                               
259500     MOVE KORD-IDPLKLST   TO W-401-IDPLKLST                               
259600     MOVE KORD-IDORDER    TO W-201-IDORDER                                
259700     MOVE KORD-IDDISTR    TO WS-SAVE-IDDISTR                              
259800     MOVE KORD-IDKUNDNR   TO WS-SAVE-IDKUNDNR                             
259900     MOVE KORD-IDDC       TO W-IDDC                                       
260000     MOVE KORD-FLLSBOK    TO WS-FLLSBOK                                   
260100     MOVE KORD-FLORDSPE   TO WS-FLORDSPE                                  
260200     MOVE KORD-FLOVRLEV   TO WS-FLOVRLEV                                  
260300     MOVE KORD-KDFRAKT    TO WS-KDFRAKT                                   
260400     MOVE KORD-KDFAKTYP   TO WS-KDFAKTYP                                  
260500     MOVE KORD-KDORDKL    TO WS-KDORDKL                                   
260600     MOVE KORD-KVORDRAD-PACK TO WS-KVORDRAD-PACK                          
260700                                                                          
260800     COMPUTE WS-KVORDRAD = KORD-KVORDRAD +                                
260900                           KORD-KVORDRAD-LEVPL                            
261000     END-COMPUTE                                                          
261100                                                                          
261200     MOVE    WS-IDORDER           TO W-201-IDORDER                        
261300     PERFORM IMS-GU-ORQI01                                                
261400     PERFORM IMS-GNP-ORQI12                                               
261500                                                                          
261600     MOVE OHUV-BEKUNDRF           TO SPAR-BEKUNDRF                        
261700     .                                                                    
261800     EJECT                                                                
261900 EF-UPPDATERA-KOLLIREG      SECTION.                                      
262000     MOVE 'STA EF-UPPDATERA-KOLLIREG'       TO WS-CURRENT-SECTION         
262100                                                                          
262200     PERFORM IMS-27-GHU-WDE601                                            
262300     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
262400     MOVE VORD-KDORDKL       TO  WS-KDORDKL                               
262500     MOVE VORD-FLAUTFAK      TO  WS-FLAUTFAK                              
262600     MOVE VORD-KDFAKTYP      TO  WS-KDFAKTYP                              
262700     MOVE VORD-DARFS         TO  WS-DARFS                                 
262800     MOVE 1                  TO  ARB-ANTAL-KOLLI                          
262900*                                                                         
263000     IF VORD-KDORDSTA        =   1                                        
263100         MOVE 2              TO  VORD-KDORDSTA                            
263200     END-IF                                                               
263300                                                                          
263400     COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + ARB-ANTAL-KOLLI              
263500     END-COMPUTE                                                          
263600                                                                          
263700*VORD-KVORDRAD-PACK                                                       
263800     COMPUTE VORD-KVORDRAD-PACK                                           
263900                         = VORD-KVORDRAD-PACK + WS-ANT-RADER-ORDER        
264000     END-COMPUTE                                                          
264100                                                                          
264200     MOVE WS-DAGENS-DATUM     TO  VORD-TIPACKN-SK                         
264300*    COMPUTE VORD-VKORDBTO    ROUNDED                                     
264400*                    = VORD-VKORDBTO    + WS-VKORDBTO                     
264500*                      * ARB-ANTAL-KOLLI                                  
264600*    END-COMPUTE                                                          
264700     MOVE ZERO                TO WS-VKORDBTO                              
264800                                                                          
264900*DECREASE EXISTING TARA WEIGHT BEFORE NEW TARA IS ADDED.                  
265000     COMPUTE VORD-VKORDBTO ROUNDED =                                      
265100             VORD-VKORDBTO - WS-OLD-EMB-VKTARA(RADIND)                    
265200     END-COMPUTE                                                          
265300                                                                          
265400*ADD NEW TARA WEIGHT FROM EA- SECTION                                     
265500     COMPUTE VORD-VKORDBTO ROUNDED =                                      
265600             VORD-VKORDBTO + WS-NEW-EMB-VKTARA(RADIND)                    
265700     END-COMPUTE                                                          
265800                                                                          
265900*NET WEIGHT                                                               
266000     IF VORD-VKORDNTO > VORD-VKORDBTO                                     
266100       MOVE VORD-VKORDBTO     TO VORD-VKORDNTO                            
266200     END-IF                                                               
266300*                                                                         
266400     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
266500       ADD 0.1                TO KOLLI-VKORDBTO-KOLLI                     
266600     END-IF                                                               
266700     MOVE KOLLI-VKORDBTO-KOLLI TO WS-KOLLI-VKORDBTO-KOLLI                 
266800*                                                                         
266900*VOLUME                                                                   
267000     COMPUTE VORD-VLORDBTO    ROUNDED                                     
267100                   = VORD-VLORDBTO    + WS-VLORDBTO                       
267200                       * ARB-ANTAL-KOLLI                                  
267300     END-COMPUTE                                                          
267400                                                                          
267500     COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                                 
267600                  = VORD-SUORDV-PACK-LOC + ARB-KOLLI-SUORDV-LOC           
267700                       * ARB-ANTAL-KOLLI                                  
267800     END-COMPUTE                                                          
267900     COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                             
268000          = VORD-SUORDV-PACK-LOCPREL + ARB-KOLLI-SUORDV-LOCPREL           
268100                       * ARB-ANTAL-KOLLI                                  
268200     END-COMPUTE                                                          
268300                                                                          
268400     IF ARB-KOLLI-SUORDV-EXP > ZERO                                       
268500       COMPUTE VORD-SUORDV-PACK ROUNDED                                   
268600                       = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV-EXP          
268700                       * ARB-ANTAL-KOLLI                                  
268800       END-COMPUTE                                                        
268900     ELSE                                                                 
269000       COMPUTE VORD-SUORDV-PACK ROUNDED                                   
269100                       = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV              
269200                       * ARB-ANTAL-KOLLI                                  
269300       END-COMPUTE                                                        
269400     END-IF                                                               
269500                                                                          
269600                                                                          
269700     MOVE ARB-KOLLI-KDVALISO      TO VORD-KDVALISO                        
269800     MOVE ARB-KOLLI-KDVALISO-EXP  TO VORD-KDVALISO-EXP                    
269900                                                                          
270000                                                                          
270100     IF W-IDDC NOT = W-IDDC-B6                                            
270200        MOVE W-IDDC TO W-IDDC-B6                                          
270300        PERFORM IMS-GU-WDB601                                             
270400     END-IF                                                               
270500     IF  VORD-KDORDSTA < +3                                               
270600     AND VORD-KVKOLLI  > +0                                               
270700     AND (DIST03-SVERIGE OR DIST35-CDC-1A-REFILL                          
270800                         OR DIST35-CDC-1B-REFILL)                         
270900     AND (DCS-CDC OR (DCS-SDC AND DCS-SWEDEN))                            
271000                                                                          
271100        MOVE VORD-KDFRAKT       TO WS-KDFRAKT-NUM2                        
271200        MOVE WS-KDFRAKT-NUM2    TO FRAK01-KDFRAKT                         
271300        IF  FRAK01-SVERIGE2                                               
271400        OR  FRAK01-NORDEN                                                 
271500        OR  FRAK01-KDFRAKT21                                              
271600        OR  FRAK01-KDFRAKT62                                              
271700        OR (WS-IDKOLLI-NUM > 149 AND WS-IDKOLLI-NUM < 200)                
271800        OR (WS-IDKOLLI-NUM > 349 AND WS-IDKOLLI-NUM < 400)                
271900          IF   VORD-FLDIRLEV = NEJ                                        
272000          AND  VORD-KDFRAKT  NOT = +17                                    
272100            IF NOT DIS128-FRAKTS                                          
272200              MOVE JA TO VORD-FLFRAKTS                                    
272300            END-IF                                                        
272400          END-IF                                                          
272500        END-IF                                                            
272600     END-IF                                                               
272700                                                                          
272800     PERFORM IMS-28-REPL-KOLLIREG                                         
272900                                                                          
273000     PERFORM EFA-BEHANDLA-KOLLI                                           
273100                                                                          
273200     MOVE ZERO               TO WS-VKORDBTO                               
273300     MOVE ZERO               TO WS-VLORDBTO                               
273400     .                                                                    
273500     EJECT                                                                
273600 EFA-BEHANDLA-KOLLI      SECTION.                                         
273700     MOVE 'STA EFA-BEHANDLA-KOLLI '         TO WS-CURRENT-SECTION         
273800     SKIP3                                                                
273900     MOVE REQU-IDPRODNR(RADIND) TO W-IDPRODNR                             
274000     MOVE REQU-IDKOLLI (RADIND) TO W-IDKOLLI                              
274100                                                                          
274200     PERFORM IMS-29-GHU-KOLLI                                             
274300                                                                          
274400     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
274500                                                                          
274600     IF REQU-KDKOLLI-IN (RADIND) = ALL '+'                                
274700        MOVE KOLLI-KDKOLLI      TO W-KDKOLLI                              
274800                                                                          
274900        PERFORM IMS-01-GU-WDK501                                          
275000        IF SEGMENT-FOUND                                                  
275100*DONT ADD TARA WEIGHT IF NOT ENTERED (TARA WEIGHT FROM WL0138)            
275200*          ADD  EMB-VKTARA      TO KOLLI-VKORDBTO-KOLLI                   
275300           MOVE EMB-KDEMBTYP    TO KOLLI-KDEMBTYP                         
275400           MOVE EMB-DIKOLLIL    TO KOLLI-DIKOLLIL                         
275500           MOVE EMB-DIKOLLIB    TO KOLLI-DIKOLLIB                         
275600           MOVE EMB-DIKOLLIH    TO KOLLI-DIKOLLIH                         
275700           COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                         
275800       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
275900           END-COMPUTE                                                    
276000        END-IF                                                            
276100     ELSE                                                                 
276200                                                                          
276300       IF SEGMENT-FOUND                                                   
276400         MOVE KOLLI-KDKOLLI            TO W-KDKOLLI                       
276500                                                                          
276600         MOVE REQU-KDKOLLI-IN (RADIND) TO KOLLI-KDKOLLI                   
276700                                                                          
276800*DECREASE EXISTING TARA WEIGHT BEFORE NEW TARA IS ADDED.                  
276900         COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                           
277000                 KOLLI-VKORDBTO-KOLLI - WS-OLD-EMB-VKTARA(RADIND)         
277100         END-COMPUTE                                                      
277200                                                                          
277300*NEW CASE CODE (KDKOLLI) FROM CD-CHECK SECTION                            
277400         MOVE WS-EMB-KDEMBTYP (RADIND) TO KOLLI-KDEMBTYP                  
277500                                                                          
277600*ADD NEW TARA WEIGHT FROM EA- SECTION                                     
277700         COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                           
277800                 KOLLI-VKORDBTO-KOLLI + WS-NEW-EMB-VKTARA(RADIND)         
277900         END-COMPUTE                                                      
278000                                                                          
278100         MOVE WS-EMB-DIKOLLIL (RADIND) TO KOLLI-DIKOLLIL                  
278200         MOVE WS-EMB-DIKOLLIB (RADIND) TO KOLLI-DIKOLLIB                  
278300         MOVE WS-EMB-DIKOLLIH (RADIND) TO KOLLI-DIKOLLIH                  
278400         MOVE WS-ODEL-IDTRP            TO KOLLI-IDTRP                     
278500*VOLUME                                                                   
278600         COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                           
278700       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
278800         END-COMPUTE                                                      
278900                                                                          
279000       END-IF                                                             
279100     END-IF                                                               
279200*KDKOLSTA                                                                 
279300     IF KOLLI-KDKOLSTA            =   ZERO                                
279400       MOVE 1                     TO  KOLLI-KDKOLSTA                      
279500       MOVE WS-DAGENS-DATUM       TO KOLLI-TIPACKN                        
279600       MOVE WS-TIDPUNKT           TO WS-TIDPUNKT-RED                      
279700       MOVE WS-HHMMSS             TO KOLLI-TIPACTID                       
279800       MOVE WS-DARFS              TO KOLLI-DARFS                          
279900       PERFORM S12-SKAPA-4322                                             
280000                                                                          
280100     END-IF                                                               
280200*                                                                         
280300     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
280400*                                                                         
280500     PERFORM IMS-REPL-KOLLI                                               
280600*                                                                         
280700     PERFORM EFAC-CLEAR-PSN-TABEL                                         
280800*                                                                         
280900*LK CASE INFO TO TMS                                                      
281000       MOVE KORD-IDDC              TO TMS-IDDC                            
281100       MOVE KORD-IDDISTR           TO TMS-IDDISTR                         
281200       MOVE KORD-IDKUNDNR          TO TMS-IDKUNDNR                        
281300       MOVE KORD-IDORDNR5          TO TMS-IDORDNR7                        
281400       MOVE KOLLI-IDKOLLI          TO TMS-IDKOLLI(1)                      
281500       CALL W403TMS1 USING TMS-W403TMS1                                   
281600               TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                           
281700               TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                     
281800               TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                     
281900               TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                   
282000               TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                     
282100               TMS-WDK5-PCB TMS-WDQ2C-PCB                                 
282200*                                                                         
282300*    MOVE REQU-FLSKRIV-CLABEL  TO RESP-FLSKRIV-CLABEL                     
282400     IF REQU-FLSKRIV-CLABEL = 'Y' OR 'J'                                  
282500        MOVE 'Y'     TO RESP-L128-FLBG                                    
282600        PERFORM EFAA-PRINT-CASE-LABEL                                     
282700     ELSE                                                                 
282800        IF REQU-FLSKRIV-CLABEL = 'N'                                      
282900           MOVE ZERO  TO RESP-L128-KVRADER                                
283000           MOVE 'N'   TO RESP-L128-FLBG                                   
283100        END-IF                                                            
283200     END-IF                                                               
283300                                                                          
283400*    MOVE REQU-FLSKRIV-DELNOTE  TO RESP-FLSKRIV-DELNOTE                   
283500     IF REQU-FLSKRIV-DELNOTE = 'Y' OR 'J'                                 
283600       MOVE 'Y'                    TO RESP-L129-FLBG                      
283700       PERFORM EFAB-PRINT-DEL-NOTE                                        
283800     ELSE                                                                 
283900       IF REQU-FLSKRIV-DELNOTE = 'N'                                      
284000         MOVE 'N'                  TO RESP-L129-FLBG                      
284100         MOVE ZERO                 TO RESP-L129-KVRADER                   
284200       END-IF                                                             
284300     END-IF                                                               
284400     .                                                                    
284500     EJECT                                                                
284600 EFAA-PRINT-CASE-LABEL         SECTION.                                   
284700     MOVE 'STA EFAA-PRINT-CASE-LABEL  '     TO WS-CURRENT-SECTION         
284800*L128-MID HAR OCCURS MAX 15                                               
284900                                                                          
285000     IF REQU-FLSKRIV-CLABEL = YES                                         
285100       IF RADIND <= MAX-RADINDX                                           
285200         MOVE YES              TO RESP-L128-CLABEL(RADIND)                
285300         MOVE REQU-IDDC-KEY    TO RESP-L128-IDDC-KEY (RADIND)             
285400         MOVE WS-IDDISTR       TO RESP-L128-IDDISTR-KEY(RADIND)           
285500         MOVE WS-IDKUNDNR-NUM  TO RESP-L128-IDKUNDNR-KEY(RADIND)          
285600         MOVE WS-IDORDNR       TO RESP-L128-IDORDNR-KEY (RADIND)          
285700         MOVE REQU-IDKOLLI(RADIND)                                        
285800           TO RESP-L128-IDKOLLI-KEY(RADIND)                               
285900         MOVE ZERO             TO RESP-L128-IDKOLLI-TOM(RADIND)           
286000         IF SPAR-FLDIRLEV = SPACE                                         
286100           MOVE ZERO           TO RESP-L128-IDPRODNR-KEY(RADIND)          
286200         ELSE                                                             
286300           MOVE REQU-IDPRODNR(RADIND)                                     
286400             TO RESP-L128-IDPRODNR-KEY(RADIND)                            
286500         END-IF                                                           
286600         MOVE RADIND           TO RESP-L128-KVRADER                       
286700       END-IF                                                             
286800       MOVE YES                TO RESP-L128-FLBG                          
286900     END-IF                                                               
287000     .                                                                    
287100     SKIP2                                                                
287200 EFAB-PRINT-DEL-NOTE           SECTION.                                   
287300     MOVE 'STA EFAB-PRINT-DEL-NOTE    '     TO WS-CURRENT-SECTION         
287400*L129-MID HAR OCCURS MAX 15                                               
287500                                                                          
287600     IF REQU-FLSKRIV-DELNOTE = YES                                        
287700       IF RADIND <= MAX-RADINDX                                           
287800                                                                          
287900         MOVE REQU-IDDC-KEY  TO RESP-L129-IDDC-KEY (RADIND)               
288000         MOVE WS-IDDISTR     TO RESP-L129-IDDISTR-KEY(RADIND)             
288100         MOVE WS-IDKUNDNR-NUM TO RESP-L129-IDKUNDNR-KEY(RADIND)           
288200         MOVE WS-IDORDNR     TO RESP-L129-IDORDNR-KEY(RADIND)             
288300         MOVE WS-IDKOLLI     TO RESP-L129-IDKOLLI-KEY(RADIND)             
288400         MOVE ZERO           TO RESP-L129-IDKOLLI-TOM(RADIND)             
288500         MOVE YES            TO RESP-L129-FLSKRIV-DELNOTE(RADIND)         
288600         MOVE RADIND         TO RESP-L129-KVRADER                         
288700       END-IF                                                             
288800       MOVE YES                TO RESP-L128-FLBG                          
288900     END-IF                                                               
289000     .                                                                    
289100     SKIP2                                                                
289200                                                                          
289300 EFAC-CLEAR-PSN-TABEL    SECTION.                                         
289400     MOVE 'EFAC-CLEAR-PSN-TABEL  ' TO WS-CURRENT-SECTION                  
289500     SKIP3                                                                
289600     MOVE +1 TO FG-INDX                                                   
289700     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
289800       MOVE ZERO                    TO TAB-IDPSN(FG-INDX)                 
289900                                     TAB-VKART-FG(FG-INDX)                
290000                                     TAB-VLFG (FG-INDX)                   
290100       ADD +1 TO FG-INDX                                                  
290200     END-PERFORM                                                          
290300     .                                                                    
290400     SKIP2                                                                
290500                                                                          
290600 EG-EV-UPDATE-4726-4727        SECTION.                                   
290700     MOVE 'STA EG-EV-UPDATE-4726-4727 '     TO WS-CURRENT-SECTION         
290800                                                                          
290900     IF WS-FLAUTFAK     = JA                                              
291000     AND DIST03-SVERIGE-2                                                 
291100                                                                          
291200         PERFORM EGA-ISRT-4726-4727                                       
291300                                                                          
291400     END-IF                                                               
291500     .                                                                    
291600     SKIP2                                                                
291700 EGA-ISRT-4726-4727       SECTION.                                        
291800     MOVE 'STA EGA-ISRT-4726-4727     '     TO WS-CURRENT-SECTION         
291900                                                                          
292000     MOVE '4726'                  TO W-4726-IDHTYP                        
292100     IF W-IDDC NOT = W-IDDC-B6                                            
292200        MOVE W-IDDC TO W-IDDC-B6                                          
292300        PERFORM IMS-GU-WDB601                                             
292400     END-IF                                                               
292500     MOVE NEJ                     TO W-4726-FLBATCH                       
292600     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
292700                                                                          
292800     PERFORM IMS-GU-4726-ROT-KVAL                                         
292900                                                                          
293000     MOVE WS-IDDISTR-NUM          TO W-4726-IDDISTR                       
293100     MOVE WS-IDKUNDNR-NUM         TO W-4726-IDKUNDNR                      
293200     MOVE W-IDDC                  TO W-4726-IDDC                          
293300     MOVE WS-KDFAKTYP             TO W-4726-KDFAKTYP                      
293400                                                                          
293500     PERFORM IMS-GNP-4726-UNDERSEG-KVAL                                   
293600                                                                          
293700     IF  SEGMENT-MISSING                                                  
293800         MOVE WS-IDDISTR-NUM      TO AUTFAKT-IDDISTR                      
293900         MOVE WS-IDKUNDNR-NUM     TO AUTFAKT-IDKUNDNR                     
294000         MOVE W-IDDC              TO AUTFAKT-IDDC                         
294100         MOVE WS-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
294200                                                                          
294300         PERFORM IMS-INSERT-4726-UNDERSEG                                 
294400     END-IF                                                               
294500     MOVE WS-IDPRODNR             TO AUTFAKT-IDPRODNR                     
294600     MOVE ZERO                    TO AUTFAKT-IDSKEPPN                     
294700                                     AUTFAKT-PRFRAKT                      
294800                                                                          
294900     IF  DIST03-SVERIGE                                                   
295000         MOVE NEJ                 TO AUTFAKT-FLLASTA                      
295100     ELSE                                                                 
295200         MOVE JA                  TO AUTFAKT-FLLASTA                      
295300     END-IF                                                               
295400                                                                          
295500     PERFORM IMS-INSERT-4727                                              
295600                                                                          
295700     .                                                                    
295800     EJECT                                                                
295900                                                                          
296000 EI-BEHANDLA-INTERVALL  SECTION.                                          
296100     MOVE 'STA EI-BEHANDLA-INTERVALL  '     TO WS-CURRENT-SECTION         
296200                                                                          
296300*                                                                         
296400     MOVE WS-IDPURAD TO W-420-IDPURAD                                     
296500     PERFORM IMS-18-GHU-WDE401                                            
296600     PERFORM IMS-19-GHNP-WDE411                                           
296700     MOVE ZERO                    TO  INX-TOT-ANT-RADER                   
296800*                                                                         
296900     PERFORM UNTIL SEGMENT-MISSING                                        
297000                                                                          
297100        IF (ORAD-KDRADSTA >= 4      AND                                   
297200            ORAD-KVBEART     > ORAD-KVAVBART + ORAD-KVANNANT)             
297300                                                                          
297400*         * AVVIKELSE VID UTSKRIFT                                        
297500           ADD +1                  TO  INX-TOT-ANT-RADER                  
297600                                                                          
297700           MOVE ORAD-WDE411        TO SPAR-ORAD-WDE411                    
297800           MOVE ZERO               TO WS-KVSLATTAT                        
297900                                   SPAR-KART-KVRESS-ART                   
298000           PERFORM EIB-BEHANDLA-RAD-I-INTERVALL                           
298100           PERFORM IMS-22-REPL-WDE411                                     
298200           PERFORM EIH-LAS-ARTREG                                         
298300           PERFORM EIJ-BESTAM-ORDERBEKR-KOD                               
298400           PERFORM EIK-UPDATE-EV-KAMP-REG                                 
298500           PERFORM EIG-UPDATE-ROREG                                       
298600           PERFORM EIE-UPDATE-ARTREG                                      
298700           PERFORM S18-GENERERA-AVVIKELSE-TRANS                           
298800        END-IF                                                            
298900        PERFORM IMS-19-GHNP-WDE411                                        
299000     END-PERFORM                                                          
299100                                                                          
299200     IF RAD-FINNS                                                         
299300       MOVE ORAD-IDPURAD TO WS-IDPURAD                                    
299400       MOVE ORAD-KDVALISO      TO SPAR-KDVALISO                           
299500       MOVE ORAD-KDVALISO-EXP  TO SPAR-KDVALISO-EXP                       
299600       MOVE JA                 TO FL-420-SEGMENT                          
299700     END-IF                                                               
299800                                                                          
299900     PERFORM EIF-UPDATE-ORDERREG                                          
300000     .                                                                    
300100     EJECT                                                                
300200 EIB-BEHANDLA-RAD-I-INTERVALL SECTION.                                    
300300     MOVE 'STA EIB-BEHANDLA-RAD-I-INT '  TO WS-CURRENT-SECTION            
300400                                                                          
300500     COMPUTE WS-KVORAPP-PACK  = ORAD-KVAVBART -                           
300600                             ORAD-KVLEVART                                
300700     END-COMPUTE                                                          
300800                                                                          
300900     COMPUTE WS-KVORAPP-TOTAL = ORAD-KVBEART -                            
301000                             ORAD-KVANNANT -                              
301100                             ORAD-KVLEVART                                
301200     END-COMPUTE                                                          
301300                                                                          
301400     IF ORAD-KDRADSTA = 4   AND                                           
301500        ORAD-KVAVBART = 0   AND                                           
301600        ORAD-KVANNANT > 0                                                 
301700        MOVE 0               TO WS-KVPRERO                                
301800     ELSE                                                                 
301900        IF W-IDDC NOT = W-IDDC-B6                                         
302000           MOVE W-IDDC TO W-IDDC-B6                                       
302100           PERFORM IMS-GU-WDB601                                          
302200        END-IF                                                            
302300        IF DCS-CDC                                                        
302400           COMPUTE WS-KVPRERO    = ORAD-KVBEART  -                        
302500                                   ORAD-KVANNANT -                        
302600                                   ORAD-KVAVBART                          
302700           END-COMPUTE                                                    
302800        ELSE                                                              
302900           MOVE 0               TO WS-KVPRERO                             
303000        END-IF                                                            
303100     END-IF                                                               
303200                                                                          
303300     MOVE ORAD-KVLEVART                 TO  ORAD-KVAVBART                 
303400     MOVE +4                            TO  ORAD-KDRADSTA                 
303500     MOVE 'Y'                           TO  DATUM-SW                      
303600                                                                          
303700*    HÄR BORDE INTE TIKLAR(WDA6) BEHÖVA UPPDATERAS                        
303800*    SEKTIONEN ANROPAS BARA OM KDRADSTA >= 4 OCH DÅ                       
303900*    ÄR TIKLAR REDAN SATT                                                 
304000                                                                          
304100     PERFORM S01-UPPD-SPAR-UPPGIFTER                                      
304200     EJECT                                                                
304300     .                                                                    
304400 EIE-UPDATE-ARTREG  SECTION.                                              
304500     MOVE 'EIE-UPDATE-ARTREG '            TO WS-CURRENT-SECTION           
304600                                                                          
304700     IF SPAR-ORAD-FLDIRLEV = NEJ OR                                       
304800        SPAR-ORAD-FLRESTN  = JA                                           
304900                                                                          
305000        IF W-IDDC NOT = W-IDDC-B6                                         
305100           MOVE W-IDDC TO W-IDDC-B6                                       
305200           PERFORM IMS-GU-WDB601                                          
305300        END-IF                                                            
305400        IF DCS-CDC                                                        
305500          PERFORM EIEA-UPDATE-PRERO-WDK9                                  
305600          PERFORM EIEB-UPDATE-SALDO-CDC                                   
305700        ELSE                                                              
305800          IF DCS-SDC                                                      
305900            PERFORM EIEC-UPDATE-SALDO-SDC                                 
306000          ELSE                                                            
306100            IF DCS-NDC                                                    
306200              PERFORM EIED-UPDATE-SALDO-NDC                               
306300            END-IF                                                        
306400          END-IF                                                          
306500        END-IF                                                            
306600     END-IF                                                               
306700     PERFORM EIED-EV-UPDATE-REFILL-SDC                                    
306800     EJECT                                                                
306900     .                                                                    
307000 EIEA-UPDATE-PRERO-WDK9  SECTION.                                         
307100     MOVE 'EIEA-UPDATE-PRERO-WDK9 '       TO WS-CURRENT-SECTION           
307200                                                                          
307300     IF WS-FLLSBOK  = JA                                                  
307400       IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                         
307500         MOVE SPAR-ORAD-IDARTNR       TO W-901-IDARTNR                    
307600         PERFORM IMS-GHU-WDK901                                           
307700                                                                          
307800         PERFORM EIEAB-UPDATE-PRERO-CDC                                   
307900                                                                          
308000         PERFORM IMS-REPL-WDK901                                          
308100       END-IF                                                             
308200     END-IF                                                               
308300     .                                                                    
308400     EJECT                                                                
308500 EIEAB-UPDATE-PRERO-CDC                SECTION.                           
308600     MOVE 'EIEAB-UPDATE-PRERO-CDC '       TO WS-CURRENT-SECTION           
308700                                                                          
308800     IF SPAR-ORAD-KDORDKL = 1                                             
308900       COMPUTE ART-KVPRERO-DAG =                                          
309000               ART-KVPRERO-DAG -                                          
309100               WS-KVPRERO                                                 
309200       END-COMPUTE                                                        
309300     ELSE                                                                 
309400       IF SPAR-ORAD-KDORDKL = 2 OR 3 OR 4                                 
309500         COMPUTE ART-KVPRERO-BULK =                                       
309600                 ART-KVPRERO-BULK -                                       
309700                 WS-KVPRERO                                               
309800         END-COMPUTE                                                      
309900       END-IF                                                             
310000     END-IF                                                               
310100     .                                                                    
310200     EJECT                                                                
310300 EIEB-UPDATE-SALDO-CDC SECTION.                                           
310400     MOVE 'EIEB-UPDATE-SALDO-CDC  '       TO WS-CURRENT-SECTION           
310500                                                                          
310600     IF WS-FLLSBOK = JA                                                   
310700        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
310800        PERFORM IMS-GHU-ARTC11                                            
310900                                                                          
311000        IF SPAR-ORAD-FLDIRLEV = NEJ                                       
311100                                                                          
311200           MOVE WS-KORD-IDDISTR     TO TEST-IDDISTR                       
311300           IF DIST20-EMBALLAGE-SVS                                        
311400           OR WS-ODEL-IDPRC = '2600'                                      
311500             COMPUTE CLAG-KVLS-SVS =                                      
311600                     CLAG-KVLS-SVS + WS-KVORAPP-PACK                      
311700             END-COMPUTE                                                  
311800           END-IF                                                         
311900                                                                          
312000           COMPUTE CLAG-KVLS    = CLAG-KVLS    + WS-KVORAPP-PACK          
312100           END-COMPUTE                                                    
312200           COMPUTE CLAG-KVEFRS  = CLAG-KVEFRS  - WS-KVORAPP-PACK          
312300           END-COMPUTE                                                    
312400                                                                          
312500           IF SPAR-ORAD-IDKAMPRF   >  0   AND                             
312600              SPAR-KART-KVRESS-ART >= 0                                   
312700             COMPUTE CLAG-KVRESS = CLAG-KVRESS + WS-KVSLATTAT             
312800             END-COMPUTE                                                  
312900           END-IF                                                         
313000        END-IF                                                            
313100                                                                          
313200        IF SPAR-ORAD-FLRESTN  = JA           AND                          
313300          (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                         
313400                              - SPAR-ORAD-KVANNANT                        
313500                              - SPAR-ORAD-KVSLATT)                        
313600           IF SPAR-ORAD-KDORDKL > 0                                       
313700               PERFORM S20-EV-LARM-2191-MID                               
313800                                                                          
313900             IF W-IDDC NOT = W-IDDC-B6                                    
314000                MOVE W-IDDC TO W-IDDC-B6                                  
314100                PERFORM IMS-GU-WDB601                                     
314200             END-IF                                                       
314300             IF DCS-CDC                                                   
314400               COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-TOTAL         
314500               END-COMPUTE                                                
314600             ELSE                                                         
314700               COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK          
314800               END-COMPUTE                                                
314900             END-IF                                                       
315000           END-IF                                                         
315100                                                                          
315200           IF CLAG-KVROS = WS-KVORAPP-TOTAL                               
315300              MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                           
315400           END-IF                                                         
315500        END-IF                                                            
315600        PERFORM IMS-REPL-ARTC                                             
315700        PERFORM S22-SKAPA-SALDOLOGG                                       
315800     END-IF                                                               
315900     .                                                                    
316000     EJECT                                                                
316100 EIEC-UPDATE-SALDO-SDC      SECTION.                                      
316200     MOVE 'EIEC-UPDATE-SALDO-SDC  '       TO WS-CURRENT-SECTION           
316300                                                                          
316400     IF WS-FLLSBOK = JA                                                   
316500        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
316600        MOVE W-IDDC                   TO W-711-IDDC                       
316700        PERFORM IMS-GU-WDK722                                             
316800        PERFORM IMS-GHU-WDK711                                            
316900                                                                          
317000        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
317100        END-COMPUTE                                                       
317200        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
317300        END-COMPUTE                                                       
317400                                                                          
317500        IF NOT DCS-CHINA                                                  
317600           PERFORM IMS-REPL-WDK7                                          
317700           PERFORM S23-SKAPA-SALDOLOGG                                    
317800           PERFORM EIECA-UPDATE-CLAG-KVROS                                
317900        ELSE                                                              
318000           PERFORM EIECB-UPDATE-SLAG-KVROS                                
318100           PERFORM IMS-REPL-WDK7                                          
318200           PERFORM S23-SKAPA-SALDOLOGG                                    
318300        END-IF                                                            
318400     END-IF                                                               
318500     .                                                                    
318600     EJECT                                                                
318700 EIECA-UPDATE-CLAG-KVROS    SECTION.                                      
318800     MOVE 'EIECA-UPDATE-CLAG-KVROS '      TO WS-CURRENT-SECTION           
318900                                                                          
319000     IF SPAR-ORAD-KDORDKL > 0  AND                                        
319100        WS-KVORAPP-TOTAL > ZERO                                           
319200                                                                          
319300       IF SPAR-ORAD-FLRESTN  = JA           AND                           
319400         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
319500                             - SPAR-ORAD-KVANNANT                         
319600                             - SPAR-ORAD-KVSLATT)                         
319700                                                                          
319800          PERFORM IMS-GHU-ARTC11                                          
319900                                                                          
320000          PERFORM S20-EV-LARM-2191-MID                                    
320100          COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK               
320200          END-COMPUTE                                                     
320300                                                                          
320400          IF CLAG-KVROS = WS-KVORAPP-TOTAL                                
320500             MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                            
320600          END-IF                                                          
320700          PERFORM IMS-REPL-ARTC                                           
320800       END-IF                                                             
320900     END-IF                                                               
321000     .                                                                    
321100     EJECT                                                                
321200 EIECB-UPDATE-SLAG-KVROS    SECTION.                                      
321300     MOVE 'EIECB-UPDATE-SLAG-KVROS '      TO WS-CURRENT-SECTION           
321400                                                                          
321500     IF SPAR-ORAD-KDORDKL > 0  AND                                        
321600        WS-KVORAPP-TOTAL > ZERO                                           
321700                                                                          
321800       IF SPAR-ORAD-FLRESTN  = JA           AND                           
321900         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
322000                             - SPAR-ORAD-KVANNANT                         
322100                             - SPAR-ORAD-KVSLATT)                         
322200                                                                          
322300          IF  SPAR-ORAD-IDDC-RO = W-IDDC                                  
322400            IF SLAG-IDDC-REF = SPACE                                      
322500               PERFORM S20-EV-LARM-2191-MID-CN-US                         
322600            END-IF                                                        
322700            IF SPAR-ORAD-KDORDKL > 1                                      
322800              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
322900                                      + WS-KVORAPP-PACK                   
323000              END-COMPUTE                                                 
323100            ELSE                                                          
323200              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
323300                                      + WS-KVORAPP-PACK                   
323400              END-COMPUTE                                                 
323500            END-IF                                                        
323600          ELSE                                                            
323700            PERFORM IMS-REPL-WDK7                                         
323800            MOVE SPAR-ORAD-IDDC-RO   TO W-711-IDDC                        
323900            PERFORM IMS-GU-WDK722                                         
324000            PERFORM IMS-GHU-WDK711                                        
324100            IF SLAG-IDDC-REF = SPACE                                      
324200               PERFORM S20-EV-LARM-2191-MID-CN-US                         
324300            END-IF                                                        
324400            IF SPAR-ORAD-KDORDKL > 1                                      
324500              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
324600                                      + WS-KVORAPP-PACK                   
324700              END-COMPUTE                                                 
324800            ELSE                                                          
324900              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
325000                                      + WS-KVORAPP-PACK                   
325100              END-COMPUTE                                                 
325200            END-IF                                                        
325300            MOVE W-IDDC              TO W-711-IDDC                        
325400          END-IF                                                          
325500                                                                          
325600       END-IF                                                             
325700     END-IF                                                               
325800     .                                                                    
325900     EJECT                                                                
326000 EIED-UPDATE-SALDO-NDC      SECTION.                                      
326100     MOVE 'EIED-UPDATE-SALDO-NDC   '      TO WS-CURRENT-SECTION           
326200                                                                          
326300     IF  WS-FLLSBOK = JA                                                  
326400     AND SPAR-ORAD-FLDIRLEV = NEJ                                         
326500        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
326600        MOVE W-IDDC                   TO W-711-IDDC                       
326700        PERFORM IMS-GU-WDK722                                             
326800        PERFORM IMS-GHU-WDK711                                            
326900                                                                          
327000        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
327100        END-COMPUTE                                                       
327200        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
327300        END-COMPUTE                                                       
327400        PERFORM S23-SKAPA-SALDOLOGG                                       
327500                                                                          
327600        IF  SPAR-ORAD-FLRESTN = JA                                        
327700        AND SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                        
327800                               - SPAR-ORAD-KVANNANT                       
327900                               - SPAR-ORAD-KVSLATT                        
328000        AND SPAR-ORAD-KDORDKL > 0                                         
328100          IF  SPAR-ORAD-IDDC-RO = W-IDDC                                  
328200            IF  DCS-NDC-CN                                                
328300            OR (DCS-NDC-NA AND DCS-USA)                                   
328400              IF SLAG-IDDC-REF = SPACE                                    
328500                PERFORM S20-EV-LARM-2191-MID-CN-US                        
328600              END-IF                                                      
328700            END-IF                                                        
328800            IF SPAR-ORAD-KDORDKL > 1                                      
328900              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
329000                                      + WS-KVORAPP-TOTAL                  
329100              END-COMPUTE                                                 
329200            ELSE                                                          
329300              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
329400                                      + WS-KVORAPP-TOTAL                  
329500              END-COMPUTE                                                 
329600            END-IF                                                        
329700            PERFORM IMS-REPL-WDK7                                         
329800          ELSE                                                            
329900            IF SPAR-ORAD-IDDC-RO = '11'                                   
330000              MOVE 'SPAR-ORAD-IDDC-RO HAR FEL IDDC'                       
330100                TO ERROR-TEXT                                             
330200            END-IF                                                        
330300            PERFORM IMS-REPL-WDK7                                         
330400            MOVE SPAR-ORAD-IDDC-RO        TO W-711-IDDC                   
330500            PERFORM IMS-GU-WDK722                                         
330600            PERFORM IMS-GHU-WDK711                                        
330700            IF  DCS-NDC-CN                                                
330800            OR (DCS-NDC-NA AND DCS-USA)                                   
330900              IF SLAG-IDDC-REF = SPACE                                    
331000                 PERFORM S20-EV-LARM-2191-MID-CN-US                       
331100              END-IF                                                      
331200            END-IF                                                        
331300            IF SPAR-ORAD-KDORDKL > 1                                      
331400              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
331500                                      + WS-KVORAPP-TOTAL                  
331600              END-COMPUTE                                                 
331700            ELSE                                                          
331800              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
331900                                      + WS-KVORAPP-TOTAL                  
332000              END-COMPUTE                                                 
332100            END-IF                                                        
332200            PERFORM IMS-REPL-WDK7                                         
332300            MOVE W-IDDC                   TO W-711-IDDC                   
332400          END-IF                                                          
332500        ELSE                                                              
332600          PERFORM IMS-REPL-WDK7                                           
332700        END-IF                                                            
332800     END-IF                                                               
332900     .                                                                    
333000     EJECT                                                                
333100 EIED-EV-UPDATE-REFILL-SDC  SECTION.                                      
333200     MOVE 'EIED-EV-UPDATE-REFILL-SDC '    TO WS-CURRENT-SECTION           
333300*REFILLORDER                                                              
333400                                                                          
333500     IF WS-FLLSBOK = JA                                                   
333600                                                                          
333700******************************************************************        
333800*                                                                         
333900*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
334000*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
334100*                                                                         
334200******************************************************************        
334300                                                                          
334400       MOVE KORD-IDDISTR       TO W-TP4TRAN-IDDISTR                       
334500                                                                          
334600       PERFORM DB2-SELECT-TP4TRAN                                         
334700                                                                          
334800       MOVE KORD-IDDISTR            TO TEST-IDDISTR                       
334900                                                                          
335000                                                                          
335100*NDC&SDC OCH INTE RESTNOTERING.                                           
335200       IF  (DIST35-REFILL                                                 
335300        OR  DIST35-REFILL-INOM-NDC                                        
335400        OR  DIST35-REFILL-NA-JAP                                          
335500        OR  DIST35-NA-TRANSFER                                            
335600        OR  DIST35-NA-NDC-RETURNS                                         
335700        OR  DIST35-PACIFIC-TRANSFER                                       
335800        OR  DIST35-REFILL-INOM-JP                                         
335900        OR  DIST35-CN-TRANSFER                                            
336000        OR  RADER-FINNS)                                                  
336100       AND SPAR-ORAD-FLRESTN = NEJ                                        
336200       AND WS-KVORAPP-TOTAL > ZERO                                        
336300         IF RADER-FINNS                                                   
336400           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
336500         ELSE                                                             
336600           PERFORM EIEDA-GET-SDC-IDDC-VALUE                               
336700         END-IF                                                           
336800         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
336900         PERFORM IMS-GHU-WDK711                                           
337000         SUBTRACT WS-KVORAPP-TOTAL  FROM SLAG-KVBEART                     
337100         PERFORM IMS-REPL-WDK7                                            
337200       ELSE                                                               
337300         IF (DIST35-NONVCC-REFILL        OR                               
337320             DIST35-NONVCC-NONVCC-TRANSFER)                               
337400         AND SPAR-ORAD-FLRESTN = NEJ                                      
337500         AND WS-KVORAPP-TOTAL > ZERO                                      
337600                                                                          
337700           PERFORM EIEDA-GET-SDC-IDDC-VALUE                               
337800                                                                          
337900           MOVE SPAR-ORAD-IDARTNR   TO W-IDARTNR                          
338000           PERFORM IMS-GHU-ARTC11                                         
338100           SUBTRACT WS-KVORAPP-TOTAL FROM CLAG-KVBEART                    
338200           PERFORM IMS-REPL-ARTC                                          
338300         END-IF                                                           
338400       END-IF                                                             
338500                                                                          
338600*NDC&SDC RESTNOTERING (SLATTGRÄNSEN ÖVERSKRIDEN).                         
338700       IF  (DIST35-REFILL                                                 
338800        OR  DIST35-REFILL-INOM-NDC                                        
338900        OR  DIST35-REFILL-NA-JAP                                          
339000        OR  DIST35-NA-TRANSFER                                            
339100        OR  DIST35-NA-NDC-RETURNS                                         
339200        OR  DIST35-PACIFIC-TRANSFER                                       
339300        OR  DIST35-REFILL-INOM-JP                                         
339400        OR  DIST35-CN-TRANSFER                                            
339500        OR  RADER-FINNS)                                                  
339600       AND SPAR-ORAD-FLRESTN = JA                                         
339700       AND WS-KVORAPP-TOTAL > ZERO                                        
339800       AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                      
339900                                 SPAR-ORAD-KVANNANT -                     
340000                                 SPAR-ORAD-KVSLATT                        
340100                                                                          
340200         IF RADER-FINNS                                                   
340300           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
340400         ELSE                                                             
340500           PERFORM EIEDA-GET-SDC-IDDC-VALUE                               
340600         END-IF                                                           
340700         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
340800         PERFORM IMS-GHU-WDK711                                           
340900         SUBTRACT WS-KVORAPP-TOTAL FROM SLAG-KVBEART                      
341000         PERFORM IMS-REPL-WDK7                                            
341100       ELSE                                                               
341200         IF (DIST35-NONVCC-REFILL        OR                               
341220             DIST35-NONVCC-NONVCC-TRANSFER)                               
341300         AND SPAR-ORAD-FLRESTN = JA                                       
341400         AND WS-KVORAPP-TOTAL > ZERO                                      
341500         AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                    
341600                                   SPAR-ORAD-KVANNANT -                   
341700                                   SPAR-ORAD-KVSLATT                      
341800                                                                          
341900            PERFORM EIEDA-GET-SDC-IDDC-VALUE                              
342000                                                                          
342100            MOVE SPAR-ORAD-IDARTNR   TO W-IDARTNR                         
342200            PERFORM IMS-GHU-ARTC11                                        
342300            SUBTRACT WS-KVORAPP-TOTAL FROM CLAG-KVBEART                   
342400            PERFORM IMS-REPL-ARTC                                         
342500         END-IF                                                           
342600       END-IF                                                             
342700     END-IF                                                               
342800     SKIP2                                                                
342900     .                                                                    
343000 EIEDA-GET-SDC-IDDC-VALUE            SECTION.                             
343100     MOVE 'EIEDA-GET-SDC-IDDC-VALUE  '    TO WS-CURRENT-SECTION           
343200                                                                          
343300     SEARCH ALL DIST57-REFILL-DC                                          
343400        AT END                                                            
343500           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
343600             TO ERROR-TEXT                                                
343700           CALL FELLOG                                                    
343800        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                 
343900           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-711-IDDC              
344000     END-SEARCH                                                           
344100     .                                                                    
344200     EJECT                                                                
344300 EIF-UPDATE-ORDERREG        SECTION.                                      
344400     MOVE 'EIF-UPDATE-ORDERREG '          TO WS-CURRENT-SECTION           
344500                                                                          
344600     PERFORM IMS-18-GHU-WDE401                                            
344700                                                                          
344800     MOVE KORD-KDFAKTYP             TO  WS-KDFAKTYP                       
344900     COMPUTE KORD-VKORDNTO ROUNDED =                                      
345000             KORD-VKORDNTO - SPAR-VKORDNTO-DEL                            
345100     END-COMPUTE                                                          
345200     COMPUTE KORD-VLORDNTO ROUNDED =                                      
345300             KORD-VLORDNTO - SPAR-VLORDNTO-DEL                            
345400     END-COMPUTE                                                          
345500     IF SPAR-ORAD-FLDIRLEV = JA                                           
345600                                                                          
345700          COMPUTE KORD-SUORDV-LEVPL-LOC = KORD-SUORDV-LEVPL-LOC           
345800                                         - SPAR-SUORDV-DEL-LOC            
345900          END-COMPUTE                                                     
346000           COMPUTE KORD-SUORDV-LEVPL-LOCPREL                              
346100          = KORD-SUORDV-LEVPL-LOCPREL - SPAR-SUORDV-DEL-LOCPREL           
346200           COMPUTE KORD-SUORDV-LEVPL = KORD-SUORDV-LEVPL                  
346300                                         - SPAR-SUORDV-DEL                
346400          END-COMPUTE                                                     
346500     ELSE                                                                 
346600           COMPUTE KORD-SUORDV-LOC = KORD-SUORDV-LOC                      
346700                               - SPAR-SUORDV-DEL-LOC                      
346800           END-COMPUTE                                                    
346900           COMPUTE KORD-SUORDV-LOCPREL = KORD-SUORDV-LOCPREL              
347000                               - SPAR-SUORDV-DEL-LOCPREL                  
347100           END-COMPUTE                                                    
347200           COMPUTE KORD-SUORDV = KORD-SUORDV                              
347300                               - SPAR-SUORDV-DEL                          
347400           END-COMPUTE                                                    
347500     END-IF                                                               
347600     IF DATUM-SW = 'Y'                                                    
347700           MOVE WS-DAGENS-DATUM  TO KORD-TIBEGPAC                         
347800           MOVE 'N'              TO DATUM-SW                              
347900     END-IF                                                               
348000     MOVE SPAR-KDVALISO          TO KORD-KDVALISO                         
348100     MOVE SPAR-KDVALISO-EXP      TO KORD-KDVALISO-EXP                     
348200                                                                          
348300                                                                          
348400     PERFORM IMS-16-REPL-WDE401                                           
348500*                                                                         
348600     MOVE ZERO TO SPAR-SUORDV-DEL                                         
348700                  SPAR-SUORDV-DEL-LOC                                     
348800                  SPAR-SUORDV-DEL-LOCPREL                                 
348900                  SPAR-VKORDNTO-DEL                                       
349000                  SPAR-VLORDNTO-DEL                                       
349100     .                                                                    
349200     EJECT                                                                
349300 EIG-UPDATE-ROREG           SECTION.                                      
349400     MOVE 'EIG-UPDATE-ROREG    '          TO WS-CURRENT-SECTION           
349500                                                                          
349600     PERFORM EIGE-HAMTA-TPOTYP-FRAN-ROREG                                 
349700     MOVE WS-IDDISTR-NUM             TO TEST-IDDISTR                      
349800     IF W-IDDC NOT = W-IDDC-B6                                            
349900        MOVE W-IDDC TO W-IDDC-B6                                          
350000        PERFORM IMS-GU-WDB601                                             
350100     END-IF                                                               
350200                                                                          
350300     IF SPAR-ORAD-FLRESTN = JA AND                                        
350400        ( SPAR-ORAD-KVLEVART     < SPAR-ORAD-KVBEART                      
350500                                 - SPAR-ORAD-KVANNANT                     
350600                                 - SPAR-ORAD-KVSLATT )                    
350700                                                                          
350800        IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                           
350900           PERFORM EIGD-SAMMANSL-EJ-BIPACKAD-RAD                          
351000                                                                          
351100           IF WS-SAMMANSLAGNING-RAD = JA                                  
351200              IF DCS-CDC                                                  
351300              OR DCS-NDC                                                  
351400                ADD WS-KVORAPP-TOTAL  TO OLD-RAD-KVART                    
351500              ELSE                                                        
351600                ADD WS-KVORAPP-PACK   TO OLD-RAD-KVART                    
351700              END-IF                                                      
351800              PERFORM IMS-REPL-ORDP01-OLD                                 
351900           ELSE                                                           
352000              PERFORM EIGA-KATEGORI                                       
352100              PERFORM EIGB-FLYTTA-WDA5-POSTER                             
352200              IF RAD-KVART > ZERO                                         
352300                PERFORM IMS-ISRT-ORDP01                                   
352400                PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                    
352500                   ADD +1               TO RAD-IDLOPNR                    
352600                   PERFORM IMS-ISRT-ORDP01                                
352700                END-PERFORM                                               
352800              END-IF                                                      
352900           END-IF                                                         
353000        ELSE                                                              
353100           MOVE WS-SAVE-IDDISTR       TO W1-IDDISTR                       
353200           MOVE WS-SAVE-IDKUNDNR      TO W1-IDKUNDNR                      
353300           MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF                      
353400           MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                       
353500           MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                       
353600                                                                          
353700           PERFORM IMS-GHU-ORDP01-GE                                      
353800           IF SEGMENT-MISSING                                             
353900*FIX START****************************************************            
354000*********** NÄR PGM ÅKER PÅ GE MOT WDA5 (ORDP01) *************            
354100                                                                          
354200             PERFORM EIGA-KATEGORI                                        
354300             MOVE W1-IDARTNR          TO W-IDARTNR                        
354400             PERFORM IMS-GHU-ARTC11                                       
354500                                                                          
354600             MOVE W1-IDDISTR          TO RAD-IDDISTR                      
354700             MOVE W1-IDKUNDNR         TO RAD-IDKUNDNR                     
354800             MOVE W1-IDKUNDRF         TO RAD-IDKUNDRF                     
354900             MOVE W1-IDARTNR          TO RAD-IDARTNR                      
355000             MOVE W1-IDLOPNR          TO RAD-IDLOPNR                      
355100             MOVE SPAR-ORAD-BERADREF  TO RAD-BERADREF                     
355200             MOVE NEJ                 TO RAD-FLERS                        
355300             MOVE CLAG-IDANSK         TO RAD-IDANSK                       
355400             MOVE SPAR-ORAD-IDANALYS  TO RAD-IDANALYS                     
355500             MOVE SPAR-ORAD-IDKONTO   TO RAD-IDKONTO                      
355600             MOVE SPAR-ORAD-IDKST     TO RAD-IDKST                        
355700             MOVE '00000     '        TO RAD-IDKUNDRF-LEV                 
355800             MOVE W-IDDC              TO RAD-IDDC                         
355900             MOVE SPAR-ORAD-IDDC-RO   TO RAD-IDDC-RO                      
356000             MOVE SPAR-ORAD-KDDSP     TO RAD-KDDSP                        
356100             MOVE KORD-KDFAKTYP       TO RAD-KDFAKTYP                     
356200             MOVE SPAR-ORAD-KDFRAKT   TO RAD-KDFRAKT                      
356300             MOVE SPAR-ORAD-KDKVBRYT  TO RAD-KDKVBRYT                     
356400             MOVE SPAR-ORAD-KDOI      TO RAD-KDOI                         
356500             MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP                  
356600             MOVE SPAR-ORAD-KDORDING  TO RAD-KDORDING                     
356700             MOVE KORD-KDORDKL        TO RAD-KDORDKL                      
356800             MOVE SPAR-ORAD-KDPRODSL  TO RAD-KDPRODSL                     
356900             MOVE WS-KDRAPRIO         TO RAD-KDRAPRIO                     
357000             MOVE +4                  TO RAD-KDROO                        
357100             MOVE '4'                 TO RAD-KDSTARAD                     
357200             MOVE WS-KDTPOTYP         TO RAD-KDTPOTYP                     
357300             MOVE SPAR-ORAD-KDVRINFO  TO RAD-KDVRINFO                     
357400             IF DCS-CDC                                                   
357500             OR DCS-NDC                                                   
357600               MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                        
357700                                              RAD-KVRO                    
357800             ELSE                                                         
357900               MOVE WS-KVORAPP-PACK   TO RAD-KVART                        
358000                                              RAD-KVRO                    
358100             END-IF                                                       
358200             MOVE SPAR-ORAD-PRARTNTO  TO RAD-PRARTNTO                     
358300             MOVE SPAR-ORAD-PRARTNTO-LOC                                  
358400                                      TO RAD-PRARTNTO-LOC                 
358500             MOVE SPAR-ORAD-PRARTNTO-LOCPREL                              
358600                                      TO RAD-PRARTNTO-LOCPREL             
358700             MOVE SPAR-ORAD-REKSIFFR  TO RAD-REKSIFFR                     
358800             MOVE ZERO                TO RAD-TIAVBOKN                     
358900             MOVE DAT-TIAAMMDD        TO RAD-DARODAT                      
359000                                         RAD-TIREGDAT                     
359100             MOVE DAT-TISEKEL         TO RAD-DARODAT (1:2)                
359200             MOVE ZERO                TO RAD-TIRES                        
359300             MOVE +0                  TO RAD-TITPO                        
359400             MOVE SPAR-ORAD-KDPRTYP   TO RAD-KDPRTYP                      
359500             MOVE SPAR-ORAD-BEVOLREF  TO RAD-BEVOLREF                     
359600             MOVE SPAR-ORAD-FLINVEST  TO RAD-FLINVEST                     
359700             MOVE SPAR-ORAD-FLPRTILL  TO RAD-FLPRTILL                     
359800             MOVE JA                  TO RAD-FLTPOBEK                     
359900             MOVE SPAR-BEKUNDRF       TO RAD-BEKUNDRF                     
360000             MOVE SPAR-ORAD-IDKAMPRF  TO RAD-IDKAMPRF                     
360100             MOVE SPAR-ORAD-IDLEVNR   TO RAD-IDLEVNR                      
360200             MOVE SPAR-ORAD-IDSYSTEM  TO RAD-IDSYSTEM                     
360300             MOVE SPAR-ORAD-KVBEART   TO RAD-KVBEART-Q                    
360400             MOVE WS-TTMMSS           TO RAD-TIREGTID                     
360500             MOVE 0                   TO RAD-DASENDAT                     
360600             MOVE 0                   TO RAD-TISENBEK-KL                  
360700             MOVE SPAR-ORAD-DEAL-PR-LINE TO RAD-DEAL-PR-LINE              
360800                                                                          
360900             IF SPAR-ORAD-KDVALISO-EXP NOT = SPACE                        
361000               MOVE SPAR-ORAD-KDVALISO-EXP TO RAD-KDVALISO                
361100             END-IF                                                       
361200                                                                          
361300             MOVE OHUV-KDORDTYP-LDC TO RAD-KDORDTYP-LDC                   
361400             MOVE OHUV-TIREPDAT     TO RAD-TIREPDAT                       
361500             MOVE SPAR-ORAD-IDKUNDRF-WIP TO RAD-IDKUNDRF-WIP              
361600             MOVE SPAR-ORAD-PRAVCOST  TO RAD-PRAVCOST                     
361700             MOVE ARB-KDROPACK        TO RAD-KDROPACK                     
361800             MOVE SPAR-ORAD-IDARBREF  TO RAD-IDARBREF                     
361900                                                                          
362000             PERFORM IMS-ISRT-ORDP01                                      
362100              IF RAD-KVART > ZERO                                         
362200                PERFORM IMS-ISRT-ORDP01                                   
362300                PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                    
362400                   ADD +1               TO RAD-IDLOPNR                    
362500                   PERFORM IMS-ISRT-ORDP01                                
362600                END-PERFORM                                               
362700              END-IF                                                      
362800              PERFORM IMS-GHU-ORDP01                                      
362900           END-IF                                                         
363000*********** FIX SLUT  ****************************************            
363100                                                                          
363200           IF RAD-DARODAT = ZERO                                          
363300             MOVE JA TO SW-TIRODAT-LIKA-MED-ZERO                          
363400           END-IF                                                         
363500                                                                          
363600                                                                          
363700           PERFORM EIGC-SAMMANSL-BIPACKAD-RAD                             
363800                                                                          
363900           IF WS-SAMMANSLAGNING-RAD = JA                                  
364000              IF DCS-CDC                                                  
364100              OR DCS-NDC                                                  
364200                IF WS-KVORAPP-TOTAL = RAD-KVART                           
364300                  ADD WS-KVORAPP-TOTAL     TO OLD-RAD-KVART               
364400                  PERFORM IMS-REPL-ORDP01-OLD                             
364500                                                                          
364600                  MOVE WS-IDDISTR-NUM        TO W1-IDDISTR                
364700                  MOVE WS-IDKUNDNR-NUM       TO W1-IDKUNDNR               
364800                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
364900                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
365000                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
365100                  PERFORM IMS-GHU-ORDP01                                  
365200                                                                          
365300                  PERFORM IMS-DLET-ORDP01                                 
365400                                                                          
365500                ELSE                                                      
365600                  SUBTRACT WS-KVORAPP-TOTAL  FROM RAD-KVART               
365700                  PERFORM IMS-REPL-ORDP01                                 
365800                                                                          
365900                  ADD WS-KVORAPP-TOTAL       TO OLD-RAD-KVART             
366000                  PERFORM IMS-REPL-ORDP01-OLD                             
366100                END-IF                                                    
366200              ELSE                                                        
366300                IF WS-KVORAPP-PACK = RAD-KVART                            
366400                  ADD WS-KVORAPP-PACK    TO OLD-RAD-KVART                 
366500                  PERFORM IMS-REPL-ORDP01-OLD                             
366600                                                                          
366700                  MOVE WS-IDDISTR-NUM        TO W1-IDDISTR                
366800                  MOVE WS-IDKUNDNR-NUM       TO W1-IDKUNDNR               
366900                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
367000                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
367100                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
367200                  PERFORM IMS-GHU-ORDP01                                  
367300                                                                          
367400                  PERFORM IMS-DLET-ORDP01                                 
367500                ELSE                                                      
367600                  SUBTRACT WS-KVORAPP-PACK   FROM RAD-KVART               
367700                  PERFORM IMS-REPL-ORDP01                                 
367800                                                                          
367900                  ADD WS-KVORAPP-PACK        TO OLD-RAD-KVART             
368000                  PERFORM IMS-REPL-ORDP01-OLD                             
368100                END-IF                                                    
368200              END-IF                                                      
368300           ELSE                                                           
368400             IF DCS-CDC                                                   
368500             OR DCS-NDC                                                   
368600              IF WS-KVORAPP-TOTAL = RAD-KVART                             
368700                 IF RAD-DARODAT = 0                                       
368800                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
368900                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
369000                 END-IF                                                   
369100                                                                          
369200                 MOVE +0                TO RAD-TIRES                      
369300                 MOVE +0                TO RAD-TIAVBOKN                   
369400                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
369500                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
369600                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
369700                                           RAD-IDDC-RO                    
369800                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
369900                 MOVE '2'               TO RAD-KDSTARAD                   
370000                 PERFORM IMS-REPL-ORDP01                                  
370100              ELSE                                                        
370200                 SUBTRACT WS-KVORAPP-TOTAL FROM RAD-KVART                 
370300                 PERFORM IMS-REPL-ORDP01                                  
370400                                                                          
370500                 IF RAD-DARODAT = 0                                       
370600                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
370700                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
370800                 END-IF                                                   
370900                                                                          
371000                 MOVE +0                TO RAD-TIRES                      
371100                 MOVE +0                TO RAD-TIAVBOKN                   
371200                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
371300                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
371400                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
371500                                           RAD-IDDC-RO                    
371600                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
371700                 MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                      
371800                 MOVE '2'               TO RAD-KDSTARAD                   
371900                 MOVE SPAR-ORAD-PRAVCOST   TO RAD-PRAVCOST                
372000                 ADD +1                 TO RAD-IDLOPNR                    
372100                 PERFORM IMS-ISRT-ORDP01                                  
372200                 PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                   
372300                   ADD +1               TO RAD-IDLOPNR                    
372400                                                                          
372500                   PERFORM IMS-ISRT-ORDP01                                
372600                 END-PERFORM                                              
372700              END-IF                                                      
372800             ELSE                                                         
372900              IF WS-KVORAPP-PACK = RAD-KVART                              
373000                 IF RAD-DARODAT = 0                                       
373100                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
373200                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
373300                 END-IF                                                   
373400                                                                          
373500                 MOVE +0                TO RAD-TIRES                      
373600                 MOVE +0                TO RAD-TIAVBOKN                   
373700                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
373800                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
373900                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
374000                                           RAD-IDDC-RO                    
374100                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
374200                 MOVE '2'               TO RAD-KDSTARAD                   
374300                 PERFORM IMS-REPL-ORDP01                                  
374400              ELSE                                                        
374500                 SUBTRACT WS-KVORAPP-PACK FROM RAD-KVART                  
374600                 PERFORM IMS-REPL-ORDP01                                  
374700                                                                          
374800                 IF RAD-DARODAT = 0                                       
374900                    MOVE DAT-TIAAMMDD   TO RAD-DARODAT                    
375000                    MOVE DAT-TISEKEL    TO RAD-DARODAT (1:2)              
375100                 END-IF                                                   
375200                                                                          
375300                 MOVE +0                TO RAD-TIRES                      
375400                 MOVE +0                TO RAD-TIAVBOKN                   
375500                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
375600                 MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP              
375700                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
375800                                           RAD-IDDC-RO                    
375900                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
376000                 MOVE WS-KVORAPP-PACK   TO RAD-KVART                      
376100                 MOVE '2'               TO RAD-KDSTARAD                   
376200                 MOVE SPAR-ORAD-PRAVCOST   TO RAD-PRAVCOST                
376300                 ADD +1                 TO RAD-IDLOPNR                    
376400                 PERFORM IMS-ISRT-ORDP01                                  
376500                 PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                   
376600                   ADD +1               TO RAD-IDLOPNR                    
376700                   PERFORM IMS-ISRT-ORDP01                                
376800                 END-PERFORM                                              
376900              END-IF                                                      
377000             END-IF                                                       
377100           END-IF                                                         
377200        END-IF                                                            
377300     END-IF                                                               
377400     EJECT                                                                
377500     .                                                                    
377600 EIGA-KATEGORI              SECTION.                                      
377700     MOVE 'EIGA-KATEGORI       '          TO WS-CURRENT-SECTION           
377800                                                                          
377900*    I DENNA SEKTION LÄSES STYRREG FÖR ATT BESTÄMMA PRIO FÖR              
378000*    DEN NYA ORDERKLASSEN.                                                
378100                                                                          
378200     MOVE LOW-VALUE                  TO W-WDGXKEY-N5-MIN                  
378300     MOVE HIGH-VALUE                 TO W-WDGXKEY-N5-MAX                  
378400     MOVE LOW-VALUE                  TO W-KDRAPRIO-N5-MIN-X               
378500     MOVE HIGH-VALUE                 TO W-KDRAPRIO-N5-MAX-X               
378600     MOVE WS-KDTPOTYP                TO W-KDTPOTYP-N5                     
378700     MOVE WS-IDDISTR-NUM             TO W-IDDISTR-FOM-N5                  
378800                                        W-IDDISTR-TOM-N5                  
378900     MOVE SPAR-ORAD-KDORDKL          TO W-KDORDKL-N5                      
379000     MOVE '4511'                     TO W-IDHTYP-N5                       
379100     MOVE LOW-VALUE                  TO W-VALFRI-N5                       
379200                                                                          
379300     PERFORM IMS-GU-XXJN                                                  
379400                                                                          
379500     MOVE STYR-4512-KDRAPRIO         TO WS-KDRAPRIO                       
379600     PERFORM EIGAA-OVERRIDA-EV-KDRAPRIO                                   
379700     EJECT                                                                
379800     .                                                                    
379900 EIGAA-OVERRIDA-EV-KDRAPRIO SECTION.                                      
380000     MOVE 'EIGAA-OVERRIDA-EV-KDRAPRIO '   TO WS-CURRENT-SECTION           
380100                                                                          
380200*SECTIONEN KAN TAS BORT EFTER NDC-INSTALLATIONEN.                         
380300     MOVE WS-IDDISTR-NUM           TO TEST-IDDISTR                        
380400                                                                          
380500     IF DCS-CDC AND                                                       
380600        DIST07-USA-RETAILER       OR                                      
380700        DIST07-USA-SUPPL-FROM-CDC OR                                      
380800        DIST35-REFILL-NA                                                  
380900       IF WS-KDFRAKT = 17 AND WS-KDORDKL = 4                              
381000         MOVE 40 TO WS-KDRAPRIO                                           
381100       END-IF                                                             
381200     END-IF                                                               
381300     .                                                                    
381400     EJECT                                                                
381500 EIGB-FLYTTA-WDA5-POSTER    SECTION.                                      
381600     MOVE 'EIGB-FLYTTA-WDA5-POSTER    '   TO WS-CURRENT-SECTION           
381700                                                                          
381800     MOVE WS-IDDISTR-NUM             TO RAD-IDDISTR                       
381900     MOVE WS-IDKUNDNR-NUM            TO RAD-IDKUNDNR                      
382000     MOVE WS-IDKUNDRF                TO RAD-IDKUNDRF                      
382100     MOVE SPAR-ORAD-IDARTNR          TO RAD-IDARTNR                       
382200     MOVE +1                         TO RAD-IDLOPNR                       
382300     MOVE SPAR-ORAD-BERADREF         TO RAD-BERADREF                      
382400     MOVE 'N'                        TO RAD-FLERS                         
382500                                                                          
382600     MOVE CLAG-IDANSK                TO RAD-IDANSK                        
382700     MOVE '00000     '               TO RAD-IDKUNDRF-LEV                  
382800     MOVE SPAR-ORAD-IDDC-RO          TO RAD-IDDC                          
382900                                        RAD-IDDC-RO                       
383000     MOVE SPAR-ORAD-IDKONTO          TO RAD-IDKONTO                       
383100     MOVE SPAR-ORAD-IDKST            TO RAD-IDKST                         
383200     MOVE SPAR-ORAD-IDANALYS         TO RAD-IDANALYS                      
383300     MOVE SPAR-ORAD-KDOI             TO RAD-KDOI                          
383400     MOVE SPAR-ORAD-CLEARGROUP       TO RAD-CLEARGROUP                    
383500     MOVE SPAR-ORAD-KDDSP            TO RAD-KDDSP                         
383600     MOVE WS-KDFAKTYP                TO RAD-KDFAKTYP                      
383700     MOVE SPAR-ORAD-KDFRAKT          TO RAD-KDFRAKT                       
383800     MOVE SPAR-ORAD-KDKVBRYT         TO RAD-KDKVBRYT                      
383900     MOVE SPAR-ORAD-KDORDING         TO RAD-KDORDING                      
384000     MOVE WS-KDORDKL                 TO RAD-KDORDKL                       
384100     MOVE SPAR-ORAD-KDPRODSL         TO RAD-KDPRODSL                      
384200     MOVE WS-KDRAPRIO                TO RAD-KDRAPRIO                      
384300     MOVE +4                         TO RAD-KDROO                         
384400     MOVE '2'                        TO RAD-KDSTARAD                      
384500     MOVE WS-KDTPOTYP                TO RAD-KDTPOTYP                      
384600     MOVE SPAR-ORAD-KDVRINFO         TO RAD-KDVRINFO                      
384700     IF DCS-CDC                                                           
384800     OR DCS-NDC                                                           
384900       MOVE WS-KVORAPP-TOTAL         TO RAD-KVART                         
385000                                        RAD-KVRO                          
385100     ELSE                                                                 
385200       MOVE WS-KVORAPP-PACK          TO RAD-KVART                         
385300                                        RAD-KVRO                          
385400     END-IF                                                               
385500     MOVE SPAR-ORAD-PRARTNTO         TO RAD-PRARTNTO                      
385600     MOVE SPAR-ORAD-REKSIFFR         TO RAD-REKSIFFR                      
385700     MOVE ZERO                       TO RAD-TIAVBOKN                      
385800     MOVE DAT-TIAAMMDD               TO RAD-DARODAT                       
385900                                        RAD-TIREGDAT                      
386000     MOVE DAT-TISEKEL                TO RAD-DARODAT (1:2)                 
386100     MOVE SPAR-ORAD-DEAL-PR-LINE     TO RAD-DEAL-PR-LINE                  
386200                                                                          
386300     IF SPAR-ORAD-KDVALISO-EXP NOT = SPACE                                
386400       MOVE SPAR-ORAD-KDVALISO-EXP   TO RAD-KDVALISO                      
386500     END-IF                                                               
386600                                                                          
386700     MOVE ZERO                       TO RAD-TIRES                         
386800     MOVE +0                         TO RAD-TITPO                         
386900     MOVE JA                         TO RAD-FLTPOBEK                      
387000     MOVE SPAR-BEKUNDRF              TO RAD-BEKUNDRF                      
387100     MOVE SPAR-ORAD-BEVOLREF         TO RAD-BEVOLREF                      
387200     MOVE SPAR-ORAD-IDKAMPRF         TO RAD-IDKAMPRF                      
387300     MOVE SPAR-ORAD-IDLEVNR          TO RAD-IDLEVNR                       
387400     MOVE SPAR-ORAD-IDSYSTEM         TO RAD-IDSYSTEM                      
387500     MOVE SPAR-ORAD-KVBEART          TO RAD-KVBEART-Q                     
387600     MOVE WS-TTMMSS                  TO RAD-TIREGTID                      
387700     MOVE 0                          TO RAD-DASENDAT                      
387800     MOVE 0                          TO RAD-TISENBEK-KL                   
387900     MOVE SPAR-ORAD-KDPRTYP          TO RAD-KDPRTYP                       
388000     MOVE SPAR-ORAD-FLINVEST         TO RAD-FLINVEST                      
388100     MOVE SPAR-ORAD-FLPRTILL         TO RAD-FLPRTILL                      
388200                                                                          
388300     MOVE OHUV-KDORDTYP-LDC          TO RAD-KDORDTYP-LDC                  
388400     MOVE OHUV-TIREPDAT              TO RAD-TIREPDAT                      
388500     MOVE SPAR-ORAD-IDKUNDRF-WIP     TO RAD-IDKUNDRF-WIP                  
388600     MOVE SPAR-ORAD-PRAVCOST         TO RAD-PRAVCOST                      
388700     MOVE ARB-KDROPACK               TO RAD-KDROPACK                      
388800     MOVE SPAR-ORAD-IDARBREF         TO RAD-IDARBREF                      
388900     PERFORM S36-ANDRA-WDC711                                             
389000     EJECT                                                                
389100     .                                                                    
389200 EIGC-SAMMANSL-BIPACKAD-RAD SECTION.                                      
389300     MOVE 'EIGC-SAMMANSL-BIPACKAD-RAD '   TO WS-CURRENT-SECTION           
389400                                                                          
389500     MOVE RAD-IDDISTR                TO W1-IDDISTR W2-IDDISTR             
389600     MOVE RAD-IDKUNDNR               TO W1-IDKUNDNR W2-IDKUNDNR           
389700     MOVE RAD-IDKUNDRF               TO W1-IDKUNDRF W2-IDKUNDRF           
389800     MOVE RAD-IDARTNR                TO W1-IDARTNR  W2-IDARTNR            
389900     MOVE +0                         TO W1-IDLOPNR                        
390000     MOVE +999                       TO W2-IDLOPNR                        
390100     MOVE '2'                        TO W-KDSTARAD                        
390200                                                                          
390300     PERFORM IMS-GHU-ORDP01-OLD                                           
390400                                                                          
390500     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
390600                                                                          
390700     PERFORM UNTIL NOT SEGMENT-FOUND   OR                                 
390800                       WS-SAMMANSLAGNING-RAD = JA                         
390900                                                                          
391000        IF  OLD-RAD-KDFRAKT  = RAD-KDFRAKT                                
391100          AND OLD-RAD-KDORDKL  = RAD-KDORDKL                              
391200          AND OLD-RAD-PRARTNTO = RAD-PRARTNTO                             
391300          AND OLD-RAD-DEAL-PR-LINE =  RAD-DEAL-PR-LINE                    
391400          AND OLD-RAD-KDTPOTYP = RAD-KDTPOTYP                             
391500          AND OLD-RAD-IDKONTO  = RAD-IDKONTO                              
391600          AND OLD-RAD-IDKST    = RAD-IDKST                                
391700                                                                          
391800           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
391900        ELSE                                                              
392000           PERFORM IMS-GHN-ORDP01-OLD                                     
392100        END-IF                                                            
392200     END-PERFORM                                                          
392300     .                                                                    
392400     EJECT                                                                
392500 EIGD-SAMMANSL-EJ-BIPACKAD-RAD SECTION.                                   
392600     MOVE 'EIGD-SAMMANSL-EJ-BIPACKAD-RAD' TO WS-CURRENT-SECTION           
392700                                                                          
392800     MOVE WS-IDDISTR-NUM             TO W1-IDDISTR  W2-IDDISTR            
392900     MOVE WS-IDKUNDNR-NUM            TO W1-IDKUNDNR W2-IDKUNDNR           
393000     MOVE WS-IDKUNDRF                TO W1-IDKUNDRF W2-IDKUNDRF           
393100     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
393200     MOVE +0                         TO W1-IDLOPNR                        
393300     MOVE +999                       TO W2-IDLOPNR                        
393400     MOVE '2'                        TO W-KDSTARAD                        
393500                                                                          
393600     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
393700                                                                          
393800     PERFORM IMS-GHU-ORDP01-OLD                                           
393900                                                                          
394000     PERFORM UNTIL NOT SEGMENT-FOUND      OR                              
394100                       WS-SAMMANSLAGNING-RAD = JA                         
394200                                                                          
394300        IF  OLD-RAD-KDFRAKT  = WS-KDFRAKT                                 
394400          AND OLD-RAD-KDORDKL  = WS-KDORDKL                               
394500          AND OLD-RAD-PRARTNTO = SPAR-ORAD-PRARTNTO                       
394600          AND OLD-RAD-DEAL-PR-LINE =                                      
394700              SPAR-ORAD-DEAL-PR-LINE                                      
394800          AND OLD-RAD-KDTPOTYP = WS-KDTPOTYP                              
394900                                                                          
395000           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
395100        ELSE                                                              
395200           PERFORM IMS-GHN-ORDP01-OLD                                     
395300        END-IF                                                            
395400     END-PERFORM                                                          
395500     .                                                                    
395600     EJECT                                                                
395700 EIGE-HAMTA-TPOTYP-FRAN-ROREG             SECTION.                        
395800     MOVE 'EIGE-HAMTA-TPOTYP-FRAN-ROREG'  TO WS-CURRENT-SECTION           
395900                                                                          
396000     MOVE WS-IDDISTR-NUM           TO W1-IDDISTR  W2-IDDISTR              
396100     MOVE WS-IDKUNDNR-NUM          TO W1-IDKUNDNR W2-IDKUNDNR             
396200                                                                          
396300     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
396400       MOVE WS-IDKUNDRF            TO W1-IDKUNDRF W2-IDKUNDRF             
396500     ELSE                                                                 
396600       MOVE SPAR-ORAD-IDKUNDRF-RO    TO W1-IDKUNDRF W2-IDKUNDRF           
396700     END-IF                                                               
396800                                                                          
396900     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
397000     MOVE SPAR-ORAD-IDLOPNR-RO       TO W1-IDLOPNR  W2-IDLOPNR            
397100     MOVE '4'                        TO W-KDSTARAD                        
397200                                                                          
397300     PERFORM IMS-GHU-ORDP01-OLD                                           
397400                                                                          
397500     IF SEGMENT-FOUND                                                     
397600       MOVE OLD-RAD-KDTPOTYP         TO WS-KDTPOTYP                       
397700     ELSE                                                                 
397800       MOVE 0                        TO WS-KDTPOTYP                       
397900     END-IF                                                               
398000     .                                                                    
398100     EJECT                                                                
398200 EIH-LAS-ARTREG            SECTION.                                       
398300     MOVE 'EIH-LAS-ARTREG '               TO WS-CURRENT-SECTION           
398400                                                                          
398500     MOVE SPAR-ORAD-IDARTNR             TO W-IDARTNR                      
398600                                                                          
398700     PERFORM IMS-GU-ARTC11                                                
398800     .                                                                    
398900     EJECT                                                                
399000 EIJ-BESTAM-ORDERBEKR-KOD  SECTION.                                       
399100     MOVE 'EIJ-BESTAM-ORDERBEKR-KOD '     TO WS-CURRENT-SECTION           
399200                                                                          
399300     MOVE ZERO                      TO WS-KDORDBEK                        
399400     MOVE WS-SAVE-IDDISTR           TO TEST-IDDISTR                       
399500       IF SPAR-ORAD-KDORDKL > 0                                           
399600                                                                          
399700          IF SPAR-ORAD-KVSLATT > 0                                        
399800                                                                          
399900             IF SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART -                  
400000                                     SPAR-ORAD-KVANNANT -                 
400100                                     SPAR-ORAD-KVSLATT                    
400200                                                                          
400300                PERFORM EIJA-SATT-KOD80-90-91                             
400400             ELSE                                                         
400500                MOVE 81                     TO WS-KDORDBEK                
400600*           81 = INTE RESTNOTERING, GÖR NY BESTÄLLNING, SLATT.            
400700             END-IF                                                       
400800          ELSE                                                            
400900             PERFORM EIJA-SATT-KOD80-90-91                                
401000          END-IF                                                          
401100       ELSE                                                               
401200         CONTINUE                                                         
401300*DET KAN BLI RESTNOTERING FRÅN UTSKRIFT.                                  
401400         MOVE 93                          TO WS-KDORDBEK                  
401500*           93 = VOR, RESTNOTERAD KVANT, FYSISK AVVIKELSE                 
401600       END-IF                                                             
401700     .                                                                    
401800     EJECT                                                                
401900 EIJA-SATT-KOD80-90-91     SECTION.                                       
402000     MOVE 'EIJA-SATT-KOD80-90-91    '     TO WS-CURRENT-SECTION           
402100                                                                          
402200     IF SPAR-ORAD-FLRESTN = JA                                            
402300        IF SPAR-ORAD-IDKUNDRF-RO > '00000     '                           
402400           MOVE 91                        TO WS-KDORDBEK                  
402500*           91 = RESTNOTERAD IGEN.                                        
402600        ELSE                                                              
402700           MOVE 90                        TO WS-KDORDBEK                  
402800*           90 = RESTNOTERAD                                              
402900        END-IF                                                            
403000     ELSE                                                                 
403100        MOVE 80                           TO WS-KDORDBEK                  
403200*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
403300     END-IF                                                               
403400     .                                                                    
403500     EJECT                                                                
403600 EIK-UPDATE-EV-KAMP-REG     SECTION.                                      
403700     MOVE 'EIK-UPDATE-EV-KAMP-REG   '     TO WS-CURRENT-SECTION           
403800                                                                          
403900     IF SPAR-ORAD-IDKAMPRF     > 0 AND                                    
404000        (WS-KDORDBEK           = 80 OR 81)                                
404100       COMPUTE WS-KVSLATTAT  = SPAR-ORAD-KVBEART -                        
404200                               SPAR-ORAD-KVLEVART -                       
404300                               SPAR-ORAD-KVANNANT                         
404400       END-COMPUTE                                                        
404500       PERFORM EIKA-UPDATE-WDM211                                         
404600       PERFORM EIKB-UPDATE-WDM221                                         
404700     END-IF                                                               
404800     .                                                                    
404900     EJECT                                                                
405000 EIKA-UPDATE-WDM211    SECTION.                                           
405100     MOVE 'EIKA-UPDATE-WDM211  '          TO WS-CURRENT-SECTION           
405200                                                                          
405300     MOVE SPAR-ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                     
405400     MOVE SPAR-ORAD-IDDC-RO        TO W-KAMP-IDDC                         
405500     MOVE SPAR-ORAD-IDARTNR        TO W-KART-IDARTNR                      
405600     PERFORM IMS-GHU-WDM211                                               
405700     MOVE KART-KVRESS-ART          TO SPAR-KART-KVRESS-ART                
405800                                                                          
405900     IF KART-KVBEART-KUND     >= WS-KVSLATTAT                             
406000         SUBTRACT WS-KVSLATTAT     FROM KART-KVBEART-KUND                 
406100         COMPUTE KART-KVRESS-ART = KART-KVRESS-ART                        
406200                                 + WS-KVSLATTAT                           
406300       END-COMPUTE                                                        
406400      ELSE                                                                
406500         MOVE 'WDM211 KART-KVBEART-KUND BLIR NEGATIV'                     
406600                                  TO ERROR-TEXT                           
406700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
406800     END-IF                                                               
406900     PERFORM IMS-REPL-WDM211                                              
407000     .                                                                    
407100     EJECT                                                                
407200 EIKB-UPDATE-WDM221   SECTION.                                            
407300     MOVE 'EIKB-UPDATE-WDM221 '           TO WS-CURRENT-SECTION           
407400                                                                          
407500     MOVE SPAR-ORAD-IDKAMPRF   TO W-KAMP-IDKAMPRF                         
407600     MOVE SPAR-ORAD-IDDC-RO    TO W-KAMP-IDDC                             
407700     MOVE SPAR-ORAD-IDARTNR    TO W-KART-IDARTNR                          
407800     MOVE WS-IDDISTR-NUM       TO W-KMRK-IDDISTR-FOM                      
407900     MOVE WS-IDDISTR-NUM       TO W-KMRK-IDDISTR-TOM                      
408000     MOVE WS-IDKUNDNR-NUM      TO W-KMRK-IDKUNDNR-FOM                     
408100     MOVE WS-IDKUNDNR-NUM      TO W-KMRK-IDKUNDNR-TOM                     
408200                                                                          
408300     PERFORM S20-FINN-INTERVALL                                           
408400                                                                          
408500     PERFORM IMS-GHU-WDM221                                               
408600                                                                          
408700     IF SEGMENT-FOUND                                                     
408800         IF KMRK-KVBEART-KUND >= WS-KVSLATTAT                             
408900             SUBTRACT WS-KVSLATTAT                                        
409000                                FROM KMRK-KVBEART-KUND                    
409100          ELSE                                                            
409200             MOVE 'WDM2 KMRK-KVBEART-KUND BLIR NEGATIV'                   
409300                                TO ERROR-TEXT                             
409400             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
409500         END-IF                                                           
409600         PERFORM IMS-REPL-WDM221                                          
409700     END-IF                                                               
409800     .                                                                    
409900     EJECT                                                                
410000 EJ-UPDATE-KUNDORDER SECTION.                                             
410100     MOVE 'EJ-UPDATE-KUNDORDER  '         TO WS-CURRENT-SECTION           
410200                                                                          
410300     PERFORM IMS-GHU-KUNDORDER                                            
410400                                                                          
410500     MOVE ZERO                   TO KORD-KDPAKOLL                         
410600                                                                          
410700     COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD    +                      
410800                                  KORD-KVORDRAD-LEVPL                     
410900     END-COMPUTE                                                          
411000                                                                          
411100     PERFORM IMS-16-REPL-WDE401                                           
411200     .                                                                    
411300     EJECT                                                                
411400                                                                          
411500 EL-UPDATE-ORDERKO SECTION.                                               
411600     MOVE 'EL-UPDATE-ORDERKO    '         TO WS-CURRENT-SECTION           
411700                                                                          
411800     MOVE KORD-IDDISTR               TO TEST-IDDISTR                      
411900                                                                          
412000     IF NOT DIST19-SATS                                                   
412100       PERFORM ELA-UPDATE-ORQA                                            
412200       PERFORM ELB-UPDATE-ORQI                                            
412300     END-IF                                                               
412400     .                                                                    
412500     EJECT                                                                
412600 ELA-UPDATE-ORQA                          SECTION.                        
412700     MOVE 'ELA-UPDATE-ORQA      '         TO WS-CURRENT-SECTION           
412800                                                                          
412900     MOVE KORD-IDORDER                   TO W-WDQ301-IDORDER              
413000     MOVE KORD-IDDC                      TO W-WDQ301-IDDC                 
413100     MOVE KORD-IDPRODNR                  TO W-WDQ301-IDPRODNR             
413200     MOVE KORD-IDPLKLST                  TO W-WDQ301-IDPLKLST             
413300     PERFORM IMS-GHU-ORQA01                                               
413400                                                                          
413500     MOVE KORD-KVORDRAD-PACK             TO ODEL-KVPACKRAD-OD             
413600     MOVE 'P'                            TO ODEL-KDODELSTA                
413700     MOVE DAT-TIAAMMDD                   TO ODEL-TIPACKN                  
413800     MOVE WS-TTMMSS                      TO ODEL-TIPACTID                 
413900                                                                          
414000     MOVE ODEL-IDDC                      TO W-4447-IDDC                   
414100                                            W-4487-IDDC                   
414200     MOVE ODEL-IDPRC                     TO W-4448-IDPRC                  
414300     MOVE ODEL-DARFS                     TO W-4490-DARFS                  
414400     MOVE ODEL-IDPRODNR                  TO W-4490-IDPRODNR               
414500     MOVE ODEL-IDPLKLST                  TO W-4490-IDPLKLST               
414600                                                                          
414700     MOVE ODEL-IDUSER                    TO WS-ODEL-IDUSER                
414800                                                                          
414900     PERFORM IMS-REPL-ORQA01                                              
415000                                                                          
415100     PERFORM S06-BORTTAG-PRODTAB                                          
415200     .                                                                    
415300     EJECT                                                                
415400 ELB-UPDATE-ORQI                          SECTION.                        
415500     MOVE 'ELB-UPDATE-ORQI      '         TO WS-CURRENT-SECTION           
415600                                                                          
415700     MOVE NEJ                     TO KDORDSTA-SW                          
415800                                                                          
415900     MOVE KORD-IDORDER            TO W-Q301KY-MIN-IDORDER                 
416000                                     W-Q301KY-MAX-IDORDER                 
416100     MOVE W-IDDC                  TO W-Q301KY-MIN-IDDC                    
416200                                     W-Q301KY-MAX-IDDC                    
416300                                                                          
416400     MOVE 'R'                     TO W-KDODELST                           
416500     PERFORM IMS-GU-WDQ301-STATUS                                         
416600     IF SEGMENT-FOUND                                                     
416700        MOVE 'R*'                 TO WS-KDORDSTA                          
416800        MOVE JA                   TO KDORDSTA-SW                          
416900     ELSE                                                                 
417000                                                                          
417100        MOVE 'U'                  TO W-KDODELST                           
417200        PERFORM IMS-GU-WDQ301-STATUS                                      
417300        IF SEGMENT-FOUND                                                  
417400           MOVE 'U*'              TO WS-KDORDSTA                          
417500           MOVE JA                TO KDORDSTA-SW                          
417600        ELSE                                                              
417700           PERFORM ELBA-KOLLA-KVKOLLI                                     
417800           IF KDORDSTA-KLAR                                               
417900               CONTINUE                                                   
418000           ELSE                                                           
418100               PERFORM ELBB-TA-FRAM-KDORDSTA                              
418200           END-IF                                                         
418300        END-IF                                                            
418400     END-IF                                                               
418500                                                                          
418600     MOVE    KORD-IDORDER         TO W-201-IDORDER                        
418700     PERFORM IMS-GU-ORQI01-GE                                             
418800                                                                          
418900     IF SEGMENT-FOUND                                                     
419000        MOVE W-IDDC               TO W-212-IDDC                           
419100        PERFORM IMS-GHNP-ORQI12                                           
419200        MOVE WS-KDORDSTA          TO ARB-KDORDSTA                         
419300        PERFORM IMS-REPL-ORQI12                                           
419400     ELSE                                                                 
419500       CONTINUE                                                           
419600     END-IF                                                               
419700     .                                                                    
419800     EJECT                                                                
419900 ELBA-KOLLA-KVKOLLI SECTION.                                              
420000     MOVE 'ELBA-KOLLA-KVKOLLI   '         TO WS-CURRENT-SECTION           
420100                                                                          
420200     MOVE ZERO        TO SPAR-IDPRODNR                                    
420300                                                                          
420400     MOVE WS-IDPRODNR TO W-IDPRODNR                                       
420500     PERFORM IMS-26-GN-WDE401-ESEQ                                        
420600                                                                          
420700     MOVE KORD-IDGMTREF      TO W-E4ASEQ-IDGMTREF                         
420800     PERFORM IMS-20-GU-WDE401-SEQ                                         
420900                                                                          
421000     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END OR                      
421100                   KDORDSTA-KLAR                                          
421200                                                                          
421300        IF KORD-IDPRODNR              = SPAR-IDPRODNR OR                  
421400           KORD-IDDC                  NOT = W-SPAR-IDDC                   
421500            CONTINUE                                                      
421600        ELSE                                                              
421700            MOVE KORD-IDPRODNR        TO W-IDPRODNR                       
421800            PERFORM IMS-27-GHU-WDE601                                     
421900                                                                          
422000            IF (VORD-KVKOLLI-LAST     >  0    OR                          
422100               VORD-KVKOLLI-FAKT      >  0    OR                          
422200               VORD-KVKOLLI-FL        >  0)   OR                          
422300               VORD-KDORDSTA          =  5                                
422400                                                                          
422500               COMPUTE W-KVKOLLI      =  W-KVKOLLI + VORD-KVKOLLI         
422600               END-COMPUTE                                                
422700               COMPUTE W-KVKOLLI-FAKT =                                   
422800                              W-KVKOLLI-FAKT + VORD-KVKOLLI-FAKT          
422900               END-COMPUTE                                                
423000               COMPUTE W-KVKOLLI-LAST =                                   
423100                              W-KVKOLLI-LAST + VORD-KVKOLLI-LAST          
423200               END-COMPUTE                                                
423300               COMPUTE W-KVKOLLI-FL   =                                   
423400                              W-KVKOLLI-FL   + VORD-KVKOLLI-FL            
423500               END-COMPUTE                                                
423600            ELSE                                                          
423700               MOVE 'P'               TO WS-KDORDSTA                      
423800               MOVE JA                TO KDORDSTA-SW                      
423900            END-IF                                                        
424000                                                                          
424100            MOVE KORD-IDPRODNR        TO SPAR-IDPRODNR                    
424200        END-IF                                                            
424300        PERFORM IMS-21-GN-WDE401-SEQ                                      
424400     END-PERFORM                                                          
424500     .                                                                    
424600     EJECT                                                                
424700 ELBB-TA-FRAM-KDORDSTA        SECTION.                                    
424800     MOVE 'ELBB-TA-FRAM-KDORDSTA  '       TO WS-CURRENT-SECTION           
424900                                                                          
425000     IF VORD-KDORDSTA          <  4                                       
425100       MOVE 'P*'              TO WS-KDORDSTA                              
425200     ELSE                                                                 
425300       IF W-KVKOLLI-FL            = W-KVKOLLI                             
425400         IF W-KVKOLLI-FAKT = ZERO AND                                     
425500            W-KVKOLLI-LAST = ZERO                                         
425600           MOVE 'S'          TO WS-KDORDSTA                               
425700         ELSE                                                             
425800           IF W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                     
425900              W-KVKOLLI-LAST     NOT = W-KVKOLLI                          
426000             MOVE 'S*'       TO WS-KDORDSTA                               
426100           ELSE                                                           
426200             MOVE 'SF'       TO WS-KDORDSTA                               
426300           END-IF                                                         
426400         END-IF                                                           
426500       END-IF                                                             
426600     END-IF                                                               
426700     .                                                                    
426800     EJECT                                                                
426900 EK-UPDATE-KOLLIREG         SECTION.                                      
427000     MOVE 'EK-UPDATE-KOLLIREG     '       TO WS-CURRENT-SECTION           
427100                                                                          
427200     MOVE WS-IDPRODNR                TO  W-IDPRODNR                       
427300     PERFORM IMS-27-GHU-WDE601                                            
427400                                                                          
427500     IF SEGMENT-FOUND                                                     
427600         MOVE VORD-IDDISTR           TO  TEST-IDDISTR                     
427700         MOVE VORD-KVKOLPAC          TO  SPAR-KVKOLPAC                    
427800         MOVE VORD-KVKOLLI-FAKT      TO  SPAR-KVKOLLI-FAKT                
427900         PERFORM S07-UPPD-VORD-FRAN-SPAR                                  
428000                                                                          
428100         IF ORAPPORTERADE-RADER-SAKNAS                                    
428200             MOVE VORD-IDDC          TO W-IDDC                            
428300             MOVE VORD-KDFAKTYP      TO WS-KDFAKTYP                       
428400             IF VORD-KVORDRAD-PACK   =   VORD-KVORDRAD                    
428500                 IF VORD-KVKOLLI     =   VORD-KVKOLPAC                    
428600                                                                          
428700                   PERFORM EKA-KONTR-OM-ODEL-EJ-UTSKRIVEN                 
428800                   IF  WS-ALLA-ODEL-UTSKRIVNA  = JA                       
428900*****************************************************************         
429000*** KOMMENTAR AV SVANTE B. 920219                             ***         
429100*****************************************************************         
429200*** EFTER DENNA KOMMENTAR GÖRS FYRA UPPDATERINGAR:            ***         
429300***                                                           ***         
429400*** A) VORD-TIPACKN-SK SÄTTS                                  ***         
429500*** B) "S19-GENERERA-KLAR-SV4" SKAPAR EN "RY6"-TRANS.         ***         
429600*** C) VORD-KDORDSTA SÄTTS                                    ***         
429700*** D) "S04-PACK-ORDER-LISTA" STARTAR 4342                    ***         
429800***                                                           ***         
429900*** OM MAN TAR BORT EN ORDERDEL I ORDER-ENTRY FÖR EN ORDER    ***         
430000*** DÄR RESTERANDE ORDERDELAR REDAN ÄR PACKADE FÅR MAN ETT    ***         
430100*** LÄGE DÄR ORDERN GÅR FRÅN OPACKAD TILL PACKAD. MAN MÅSTE   ***         
430200*** DÅ I W413AVSO UTFÖRA PUNKT A-D.                           ***         
430300***                                                           ***         
430400*** OM MAN I 4397 ELLER 4398 ÄNDRAR DESSA UPPDATERINGAR ELLER ***         
430500*** LÄGGER TILL NYA MÅSTE MAN DÄRFÖR ÄVEN GÖRA DETTA I        ***         
430600*** W413AVSO.                                                 ***         
430700*****************************************************************         
430800                                                                          
430900                     IF VORD-KVKOLPAC = 0                                 
431000                         MOVE DAT-TIAAMMDD  TO VORD-TIPACKN-SK            
431100                     END-IF                                               
431200                                                                          
431300                     IF  VORD-KDORDSTA < +3                               
431400                     AND VORD-KVKOLLI  >  +0                              
431500                     AND DIST03-SVERIGE-2                                 
431600                         PERFORM S19-GENERERA-KLAR-SV4                    
431700                     END-IF                                               
431800                                                                          
431900                     MOVE +3         TO  VORD-KDORDSTA                    
432000                     MOVE +3         TO  VORD-KDMETOD                     
432100                                                                          
432200                     IF VORD-FLAUTFAK =  JA     AND                       
432300                        VORD-KVKOLLI  >  ZERO                             
432400                         MOVE JA     TO  SW-FLAUTFAK                      
432500                     END-IF                                               
432600                                                                          
432700                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FL                  
432800                         MOVE +4     TO  VORD-KDORDSTA                    
432900                     END-IF                                               
433000                                                                          
433100                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FAKT                
433200                     AND VORD-KVKOLLI =  VORD-KVKOLLI-LAST                
433300                         MOVE +5     TO  VORD-KDORDSTA                    
433400                     END-IF                                               
433500                                                                          
433600                     IF W-IDDC NOT = W-IDDC-B6                            
433700                        MOVE W-IDDC TO W-IDDC-B6                          
433800                        PERFORM IMS-GU-WDB601                             
433900                     END-IF                                               
434000                                                                          
434100                     IF DCS-NDC-NA                                        
434200                       PERFORM EKB-SKRIV-DEL-NOTE                         
434300                     END-IF                                               
434400                   END-IF                                                 
434500                 END-IF                                                   
434600             END-IF                                                       
434700         END-IF                                                           
434800                                                                          
434900         PERFORM IMS-28-REPL-KOLLIREG                                     
435000                                                                          
435100     ELSE                                                                 
435200         MOVE FEL                       TO  WS-BEHANDLING-TEST            
435300     END-IF                                                               
435400     .                                                                    
435500 EKA-KONTR-OM-ODEL-EJ-UTSKRIVEN      SECTION.                             
435600     MOVE 'EKA-KONTR-OM-ODEL-EJ-UTSKRIV'  TO WS-CURRENT-SECTION           
435700                                                                          
435800     MOVE JA                 TO WS-ALLA-ODEL-UTSKRIVNA                    
435900                                                                          
436000     MOVE W-IDDC             TO W-Q301-MIN-IDDC                           
436100                                W-Q301-MAX-IDDC                           
436200     MOVE WS-IDPRODNR        TO W-Q301-MIN-IDPRODNR                       
436300                                W-Q301-MAX-IDPRODNR                       
436400     MOVE WS-IDORDER         TO W-Q301-MIN-IDORDER                        
436500                                W-Q301-MAX-IDORDER                        
436600                                                                          
436700     PERFORM IMS-GU-WDQ301-ODEL                                           
436800                                                                          
436900     PERFORM UNTIL ((NOT SEGMENT-FOUND)                                   
437000             OR   WS-ALLA-ODEL-UTSKRIVNA  = NEJ)                          
437100                                                                          
437200       IF  ODEL-KDODELSTA = 'R'                                           
437300         MOVE NEJ        TO WS-ALLA-ODEL-UTSKRIVNA                        
437400       ELSE                                                               
437500         PERFORM IMS-GN-WDQ301-ODEL                                       
437600       END-IF                                                             
437700     END-PERFORM                                                          
437800     .                                                                    
437900     EJECT                                                                
438000                                                                          
438100                                                                          
438200 EKB-SKRIV-DEL-NOTE SECTION.                                              
438300                                                                          
438400     MOVE WS-SAVE-IDDISTR             TO TEST-IDDISTR                     
438500     IF DIST07-USA-PRINT-DNOTE                                            
438600     OR DIST07-CAN-PRINT-DNOTE                                            
438700*       TRANS TILL 4349 FÖR ATT STARTA UTSKRIFT                           
438800        MOVE WS-SAVE-IDDISTR     TO 4349-MID-IDDISTR                      
438900        MOVE WS-SAVE-IDKUNDNR    TO 4349-MID-IDKUNDNR                     
439000        MOVE WS-IDORDNR          TO WS-IDORDNR7-NEW                       
439100        MOVE WS-IDORDNR7-NEW     TO 4349-MID-IDORDNR7                     
439200        MOVE WS-IDPRODNR         TO 4349-MID-IDPRODNR                     
439300        MOVE W-IDDC              TO 4349-MID-IDDC                         
439400        MOVE WS-IDORDER          TO 4349-MID-IDORDER                      
439500        MOVE WS-KDMFSFOR         TO 4349-SPRAK                            
439600        MOVE 4349-MID-W4I34901   TO 4349-FILLER                           
439700                                                                          
439800        PERFORM IMS-PURG-4349                                             
439900     END-IF                                                               
440000                                                                          
440100     MOVE WS-SAVE-IDDISTR             TO TEST-IDDISTR                     
440200     IF DIST07-USA-RETAILER-DNOTE                                         
440300     OR DIST07-CAN-RETAILER                                               
440400                                                                          
440500        INITIALIZE DNOT-ORDER-INFO                                        
440600                                                                          
440700*       UPPDATERING AV UTSKRIFTSDAG, WDQ5                                 
440800        MOVE 'WL019700'               TO DNOT-IDPGM                       
440900        MOVE KORD-IDORDER             TO DNOT-IDORDER                     
441000        MOVE KORD-IDDC                TO DNOT-IDDC                        
441100        CALL W411DNOT USING DNOT-W411DNOT                                 
441200                            DNOT-ORQP-PCB                                 
441300                            DNOT-ORQP2-PCB                                
441400                            DNOT-ORQP3-PCB                                
441500                            DNOT-4013-PCB                                 
441600                            DNOT-BENA-PCB                                 
441700                                                                          
441800     END-IF                                                               
441900     .                                                                    
442000     EJECT                                                                
442100                                                                          
442200 EM-AVSLUT             SECTION.                                           
442300     MOVE 'EM-AVSLUT '                   TO WS-CURRENT-SECTION            
442400                                                                          
442500     IF WS-BEHANDLING-RATT                                                
442600                                                                          
442700         MOVE WS-IDDISTR-NUM          TO TEST-IDDISTR                     
442800         IF AUT-FAKTURA-SKRIVS-UT    AND                                  
442900            SPAR-KVKOLPAC > SPAR-KVKOLLI-FAKT                             
443000             PERFORM S05-AUTOMATFAKTURERING                               
443100                                                                          
443200             IF SEGMENT-MISSING                                           
443300                 MOVE SPACE           TO  IO-AREA3                        
443400                 MOVE WS-IDDISTR-NUM  TO  AUTFAKT-IDDISTR                 
443500                 MOVE WS-IDKUNDNR-NUM TO  AUTFAKT-IDKUNDNR                
443600                 MOVE W-IDDC          TO  AUTFAKT-IDDC                    
443700                 MOVE WS-KDFAKTYP     TO  AUTFAKT-KDFAKTYP                
443800                 PERFORM IMS-ISRT-AUTFAKTURA-ROT                          
443900                 MOVE SPACE           TO  IO-AREA3                        
444000                 PERFORM S05-AUTOMATFAKTURERING                           
444100             END-IF                                                       
444200         END-IF                                                           
444300     END-IF                                                               
444400     IF REQU-IDPRODNR(RADIND) NOT = WS-PREV-IDPRODNR                      
444500       MOVE ZERO                    TO WS-ANT-RADER-ORDER                 
444600     END-IF                                                               
444700     .                                                                    
444800     EJECT                                                                
444900 F-READ-SHOW-INFO SECTION.                                                
445000     MOVE 'F-READ-SHOW-INFO'   TO WS-CURRENT-SECTION                      
445100                                                                          
445200     IF REQU-KDPGMACT = 'E'                                               
445300       MOVE ZERO                    TO RADIND                             
445400     END-IF                                                               
445500                                                                          
445600     IF (REQU-IDTRPTNR-KEY     = ALL '+'                                  
445700     AND REQU-TIRFSDAT-KEY     = ALL '+'                                  
445800     AND REQU-TIRFSTID-KEY     = ALL '+')                                 
445900       IF (REQU-IDPRC-KEY      = ALL '+'                                  
446000       AND REQU-IDLOTNR-KEY    = ALL '+')                                 
446100          MOVE KEYS-ARE-MISSING      TO RESP-IDMSG-INFO                   
446200          MOVE 'IDPRC'               TO RESP-IDELMT-ERROR                 
446300       ELSE                                                               
446400                                                                          
446500*ALT 2 - LÄS MED IDPRC                                                    
446600         PERFORM FA-READ-IDPRC-IDLOTNR                                    
446700       END-IF                                                             
446800     ELSE                                                                 
446900*ALT 1 - LÄS MED TRPID                                                    
447000       PERFORM FB-READ-IDTRP-TIRFS                                        
447100     END-IF                                                               
447200                                                                          
447300     MOVE RADIND TO RESP-KVRADER                                          
447400                                                                          
447500     IF SW-SET-RESP-KVRADER = 'N'                                         
447600                                                                          
447700       IF REQU-FLSKRIV-CLABEL  = 'Y'                                      
447800       OR REQU-FLSKRIV-DELNOTE = 'Y'                                      
447900         MOVE 500  TO RESP-KVRADER                                        
448000       END-IF                                                             
448100     END-IF                                                               
448200                                                                          
448300     IF REQU-KDPGMACT = 'S'                                               
448400       MOVE 'N'                   TO RESP-FLOK-PACKRAPP                   
448500       IF RESP-KVRADER >= WS-MAX-500-RADER                                
448600         MOVE TOO-MANY-LINES TO RESP-IDMSG-INFO                           
448700       END-IF                                                             
448800       IF RADIND = +00000                                                 
448900         MOVE INF-LINES-NOT-FOUND TO RESP-IDMSG-INFO                      
449000       END-IF                                                             
449100     END-IF                                                               
449200     .                                                                    
449300     EJECT                                                                
449400 FA-READ-IDPRC-IDLOTNR      SECTION.                                      
449500     MOVE 'FA-READ-IDPRC-IDLOTNR '  TO WS-CURRENT-SECTION                 
449600*ALT2-IDPRC-IDLOTNR                                                       
449700                                                                          
449800     MOVE REQU-IDDC-KEY             TO W-IDDC-Q3H1                        
449900     MOVE REQU-IDPRC-KEY            TO W-IDPRCPLK-Q3H1                    
450000     MOVE REQU-IDLOTNR-KEY          TO W-IDLOTNR-PLK-Q3H1                 
450100                                                                          
450200     MOVE +0                        TO RADIND                             
450300     MOVE NEJ                       TO SW-SET-RESP-KVRADER                
450400     PERFORM IMS-10-GU-WDQ301                                             
450500                                                                          
450600     PERFORM UNTIL SEGMENT-MISSING                                        
450700                OR SEGMENT-END                                            
450800                OR RADIND > WS-MAX-500-RADER                              
450900                                                                          
451000       MOVE ODEL-IDPRODNR      TO W-401-IDPRODNR                          
451100       MOVE ODEL-IDPLKLST      TO W-401-IDPLKLST                          
451200       MOVE ODEL-IDDISTR       TO W-401-IDDISTR                           
451300       MOVE ODEL-IDKUNDNR      TO W-401-IDKUNDNR                          
451400       MOVE SPACES             TO W-401-IDKUNDRF                          
451500       MOVE ODEL-IDORDNR7      TO W-401-IDORDNR                           
451600*-NEW CODE----------------------------------------------------            
451700       MOVE ODEL-IDPRODNR         TO SPAR-VORD-IDPRODNR                   
451800                                                                          
451900       PERFORM IMS-17-GU-WDE401                                           
452000                                                                          
452100       PERFORM IMS-GNP-WDE421-OKVAL                                       
452200       PERFORM UNTIL SEGMENT-MISSING                                      
452300               OR SW-L0199 = 'Y'                                          
452400           MOVE KKOLLI-IDPRODNR    TO W-IDPRODNR-WDE601                   
452500           MOVE KKOLLI-IDKOLLI     TO W-IDKOLLI-WDE611                    
452600           IF RADIND > 0                                                  
452700             PERFORM CHECK-FOR-DUPLICATE-KOLLI                            
452800           END-IF                                                         
452900           IF NO-DUP-KOLLI                                                
453000             PERFORM IMS-GU-WDE611                                        
453100             IF SEGMENT-FOUND                                             
453200               IF KOLLI-KDKOLSTA = 0                                      
453300                IF KOLLI-DIKOLLIL = 0                                     
453400                 ADD +1                TO RADIND                          
453500                 MOVE ODEL-IDPRODNR    TO RESP-IDPRODNR(RADIND)           
453600                                          W-RESP-IDPRODNR(RADIND)         
453700                 MOVE ODEL-IDPLKLST    TO RESP-IDPLKLST(RADIND)           
453800                 MOVE KOLLI-IDKOLLI    TO RESP-IDKOLLI (RADIND)           
453900                                          W-RESP-IDKOLLI (RADIND)         
454000                 MOVE KOLLI-KDKOLLI    TO RESP-KDKOLLI(RADIND)            
454100                 MOVE SPACE            TO RESP-KDKOLLI-IN(RADIND)         
454200                 MOVE SPACE     TO RESP-IDMSG-ERROR-RAD (RADIND)          
454300                 MOVE JA            TO SW-SET-RESP-KVRADER                
454400                ELSE                                                      
454500                 MOVE ERR-USESCREEN-L0199 TO RESP-IDMSG-ERROR             
454600                 MOVE YES           TO SW-L0199                           
454700                END-IF                                                    
454800               END-IF                                                     
454900             END-IF                                                       
455000           END-IF                                                         
455100           PERFORM IMS-GNP-WDE421-OKVAL                                   
455200       END-PERFORM                                                        
455300*-------------------------------------------------------------            
455400       PERFORM IMS-14-GN-WDQ301                                           
455500     END-PERFORM                                                          
455600     .                                                                    
455700     EJECT                                                                
455800 CHECK-FOR-DUPLICATE-KOLLI    SECTION.                                    
455900     MOVE 1 TO CNT                                                        
456000     SET NO-DUP-KOLLI TO TRUE                                             
456100     PERFORM UNTIL CNT > RADIND OR DUPLICATE-CHECK = 'J'                  
456200       IF (KKOLLI-IDKOLLI = W-RESP-IDKOLLI (CNT) AND                      
456300          ODEL-IDPRODNR = W-RESP-IDPRODNR(CNT) )                          
456400         SET DUP-KOLLI TO TRUE                                            
456500       END-IF                                                             
456600       ADD  1 TO CNT                                                      
456700     END-PERFORM                                                          
456800     .                                                                    
456900     EJECT                                                                
457000 FB-READ-IDTRP-TIRFS    SECTION.                                          
457100     MOVE 'FB-READ-IDTRP-TIRFS '  TO WS-CURRENT-SECTION                   
457200*ALT1-IDTRP-TIRFS                                                         
457300                                                                          
457400     MOVE REQU-IDDC-KEY             TO W-IDDC-E6C1                        
457500     MOVE REQU-IDDC-KEY             TO W-IDDC-E6C1-MAX                    
457600     MOVE REQU-IDTRPTNR-KEY         TO W-IDTRPTNR-E6C1                    
457700     MOVE REQU-IDTRPTNR-KEY         TO W-IDTRPTNR-E6C1-MAX                
457800     MOVE REQU-TIRFSDAT-KEY         TO WS-REQU-TIRFSDAT                   
457900     MOVE REQU-TIRFSTID-KEY         TO WS-REQU-TIRFSTID                   
458000     MOVE '20'                      TO WS-DARFS-SEKEL                     
458100     MOVE WS-DARFS-SOEK             TO W-DARFS-E6C1                       
458200     MOVE WS-DARFS-SOEK             TO W-DARFS-E6C1-MAX                   
458300     MOVE +0                        TO RADIND                             
458400     MOVE NEJ                       TO SW-SET-RESP-KVRADER                
458500                                                                          
458600     PERFORM IMS-13-GU-WDE6C1                                             
458700                                                                          
458800     PERFORM UNTIL SEGMENT-MISSING                                        
458900                OR SEGMENT-END                                            
459000                OR RADIND > WS-MAX-500-RADER                              
459100                                                                          
459200       MOVE SEQC-IDPRODNR           TO W-IDPRODNR-Q3D1                    
459300       MOVE SEQC-IDPRODNR           TO W-IDPRODNR-Q3D1-MAX                
459400                                                                          
459500       IF SEQC-IDPRODNR NOT = SPAR-VORD-IDPRODNR                          
459600         MOVE SEQC-IDPRODNR         TO W-IDPRODNR                         
459700         PERFORM IMS-02-GU-WDE601                                         
459800         MOVE VORD-IDPRODNR         TO SPAR-VORD-IDPRODNR                 
459900                                                                          
460000         IF VORD-KDORDSTA = +2                                            
460100           PERFORM IMS-09-GU-WDQ3D1                                       
460200                                                                          
460300           MOVE SEQD-IDORDER        TO W-301-IDORDER                      
460400           MOVE SEQC-IDDC           TO W-301-IDDC                         
460500           MOVE SEQD-IDPRODNR       TO W-301-IDPRODNR                     
460600           MOVE SEQD-IDPLKLST       TO W-301-IDPLKLST                     
460700           PERFORM IMS-GU-ORQA01                                          
460800           IF SEGMENT-FOUND                                               
460900             IF ODEL-KDODELSTA = 'U'                                      
461000             IF SEQC-KDKOLSTA = 0                                         
461100               ADD +1               TO RADIND                             
461200               MOVE SEQD-IDPRODNR   TO RESP-IDPRODNR(RADIND)              
461300               MOVE SEQD-IDPLKLST   TO RESP-IDPLKLST(RADIND)              
461400               MOVE SEQC-IDKOLLI    TO RESP-IDKOLLI (RADIND)              
461500               MOVE SEQC-KDKOLLI    TO RESP-KDKOLLI(RADIND)               
461600               MOVE SPACE           TO RESP-KDKOLLI-IN(RADIND)            
461700               MOVE SPACE                                                 
461800                 TO RESP-IDMSG-ERROR-RAD (RADIND)                         
461900               MOVE JA              TO SW-SET-RESP-KVRADER                
462000             END-IF                                                       
462100             END-IF                                                       
462200           END-IF                                                         
462300         END-IF                                                           
462400       END-IF                                                             
462500       PERFORM IMS-08-GN-WDE6C1                                           
462600     END-PERFORM                                                          
462700     .                                                                    
462800     EJECT                                                                
462900                                                                          
463000*TAG BORT OM KODEN OM DEN EJ BEHÖVS EFTER TESTER.                         
463100 S15-RENSA-RAD SECTION.                                                   
463200     MOVE 'STA S15-RENSA-RAD' TO WS-CURRENT-SECTION                       
463300*RENSA RESPONSE-RAD:                                                      
463400     MOVE +1                      TO RENSA-IND                            
463500                                                                          
463600     PERFORM UNTIL RENSA-IND > WS-MAX-500-RADER                           
463700          OR REQU-IDPRODNR(RENSA-IND) = LOW-VALUE                         
463800          OR REQU-IDPRODNR(RENSA-IND) = ALL '+'                           
463900       MOVE ZERO             TO RESP-IDPRODNR      (RENSA-IND)            
464000                                RESP-IDPLKLST      (RENSA-IND)            
464100                                RESP-IDKOLLI       (RENSA-IND)            
464200       MOVE SPACE            TO RESP-KDKOLLI       (RENSA-IND)            
464300                                RESP-IDMSG-ERROR-RAD (RENSA-IND)          
464400       ADD +1      TO RENSA-IND                                           
464500     END-PERFORM                                                          
464600     .                                                                    
464700     SKIP2                                                                
464800*    --- DISPATCHER SECTIONS                                              
464900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
465000     MOVE 'STA S01-FETCH-REQUEST'    TO WS-CURRENT-SECTION                
465100                                                                          
465200     MOVE 'GETARG'               TO SUB-KDFUNC                            
465300     MOVE WS-ABSTRACT-ADRESS     TO SUB-ADDISPABS                         
465400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
465500                                                                          
465600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
465700                                                                          
465800     IF SUB-KDRC > 0                                                      
465900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
466000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
466100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
466200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
466300     END-IF                                                               
466400     .                                                                    
466500     SKIP3                                                                
466600 S02-RETURN-RESPONSE SECTION.                                             
466700     MOVE 'STA S02-RETURN-RESPONSE'    TO WS-CURRENT-SECTION              
466800                                                                          
466900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
467000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
467100     MOVE SUB-KVDLEN                 TO WS-SUB-KVDLEN                     
467200                                                                          
467300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
467400                                                                          
467500     IF SUB-KDRC > 0                                                      
467600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
467700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
467800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
467900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
468000     END-IF                                                               
468100     .                                                                    
468200     EJECT                                                                
468300 S01-UPPD-SPAR-UPPGIFTER SECTION.                                         
468400     MOVE 'S01-UPPD-SPAR-UPPGIFTER'    TO WS-CURRENT-SECTION              
468500                                                                          
468600     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
468700                         ORAD-VKARTNTO * WS-KVORAPP-PACK                  
468800     END-COMPUTE                                                          
468900*                                                                         
469000     COMPUTE SPAR-VLORDNTO = SPAR-VLORDNTO +                              
469100                         ORAD-VLARTNTO * WS-KVORAPP-PACK                  
469200     END-COMPUTE                                                          
469300*                                                                         
469400     MOVE WS-IDDISTR-NUM    TO TEST-IDDISTR                               
469500     IF DIST79-DEALER-PRICE                                               
469600      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
469700       COMPUTE SPAR-SUORDV-LOCPREL ROUNDED = SPAR-SUORDV-LOCPREL +        
469800                 ORAD-PRARTNTO-LOCPREL * WS-KVORAPP-PACK                  
469900       END-COMPUTE                                                        
470000      ELSE                                                                
470100       COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +                
470200                 ORAD-PRARTNTO-LOC * WS-KVORAPP-PACK                      
470300       END-COMPUTE                                                        
470400      END-IF                                                              
470500     ELSE                                                                 
470600       COMPUTE SPAR-SUORDV ROUNDED = SPAR-SUORDV +                        
470700                      ORAD-PRARTNTO * WS-KVORAPP-PACK                     
470800       END-COMPUTE                                                        
470900     END-IF                                                               
471000     MOVE ORAD-KDVALISO      TO SPAR-KDVALISO                             
471100     MOVE ORAD-KDVALISO-EXP  TO SPAR-KDVALISO-EXP                         
471200                                                                          
471300*                                                                         
471400     COMPUTE SPAR-SUORDV-DEL   = SPAR-SUORDV-DEL + SPAR-SUORDV            
471500     END-COMPUTE                                                          
471600     COMPUTE SPAR-SUORDV-DEL-LOC  = SPAR-SUORDV-DEL-LOC                   
471700                                  + SPAR-SUORDV-LOC                       
471800     END-COMPUTE                                                          
471900     COMPUTE SPAR-SUORDV-DEL-LOCPREL   = SPAR-SUORDV-DEL-LOCPREL          
472000                               + SPAR-SUORDV-LOCPREL                      
472100     END-COMPUTE                                                          
472200     COMPUTE SPAR-VKORDNTO-DEL = SPAR-VKORDNTO-DEL + SPAR-VKORDNTO        
472300     END-COMPUTE                                                          
472400     COMPUTE SPAR-VLORDNTO-DEL = SPAR-VLORDNTO-DEL + SPAR-VLORDNTO        
472500     END-COMPUTE                                                          
472600     COMPUTE SPAR-SUORDV-TOT   = SPAR-SUORDV-TOT + SPAR-SUORDV            
472700     END-COMPUTE                                                          
472800     COMPUTE SPAR-SUORDV-TOT-LOC = SPAR-SUORDV-TOT-LOC                    
472900                                 + SPAR-SUORDV-LOC                        
473000     END-COMPUTE                                                          
473100     COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL            
473200                                 + SPAR-SUORDV-LOCPREL                    
473300     END-COMPUTE                                                          
473400     COMPUTE SPAR-VKORDNTO-TOT = SPAR-VKORDNTO-TOT + SPAR-VKORDNTO        
473500     END-COMPUTE                                                          
473600     COMPUTE SPAR-VLORDNTO-TOT = SPAR-VLORDNTO-TOT + SPAR-VLORDNTO        
473700     END-COMPUTE                                                          
473800     EJECT                                                                
473900     .                                                                    
474000 S05-AUTOMATFAKTURERING      SECTION.                                     
474100     MOVE 'S05-AUTOMATFAKTURERING '    TO WS-CURRENT-SECTION              
474200                                                                          
474300     MOVE WS-IDDISTR-NUM                  TO  W-RDG-IDDISTR               
474400     MOVE WS-IDKUNDNR-NUM                 TO  W-RDG-IDKUNDNR              
474500     MOVE W-IDDC                          TO  W-RDG-IDDC                  
474600     MOVE WS-KDFAKTYP                     TO  W-RDG-KDFAKTYP              
474700     SKIP2                                                                
474800     MOVE WS-IDPRODNR                     TO  AUTFAKT-IDPRODNR            
474900     MOVE ZERO                            TO  AUTFAKT-PRFRAKT             
475000                                              AUTFAKT-IDSKEPPN            
475100     MOVE NEJ                             TO  W-4726-FLBATCH              
475200                                                                          
475300     IF DIST03-SVERIGE                                                    
475400         MOVE NEJ                         TO  AUTFAKT-FLLASTA             
475500     ELSE                                                                 
475600         MOVE JA                          TO  AUTFAKT-FLLASTA             
475700     END-IF                                                               
475800     MOVE '4726'                          TO W-4726-IDHTYP                
475900     MOVE LOW-VALUE                       TO W-4726-LOWVALUE              
476000     PERFORM IMS-ISRT-AUTFAKTURA                                          
476100     EJECT                                                                
476200     .                                                                    
476300 S06-BORTTAG-PRODTAB            SECTION.                                  
476400     MOVE 'S06-BORTTAG-PRODTAB    '    TO WS-CURRENT-SECTION              
476500                                                                          
476600     PERFORM IMS-GU-XXKH11                                                
476700     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
476800                                                                          
476900     PERFORM IMS-GHU-WDGX4490                                             
477000     IF SEGMENT-FOUND                                                     
477100        PERFORM IMS-DLET-WDGX4490                                         
477200     END-IF                                                               
477300     .                                                                    
477400     SKIP2                                                                
477500 S07-UPPD-VORD-FRAN-SPAR   SECTION.                                       
477600     MOVE 'S07-UPPD-VORD-FRAN-SPAR'    TO WS-CURRENT-SECTION              
477700     SKIP3                                                                
477800     SUBTRACT SPAR-VKORDNTO FROM VORD-VKORDNTO                            
477900     COMPUTE VORD-VLORDNTO = VORD-VLORDNTO - SPAR-VLORDNTO                
478000                               / 1000000                                  
478100     END-COMPUTE                                                          
478200     SUBTRACT SPAR-SUORDV-LOC  FROM VORD-SUORDV-LOC                       
478300     SUBTRACT SPAR-SUORDV-LOCPREL  FROM VORD-SUORDV-LOCPREL               
478400     SUBTRACT SPAR-SUORDV      FROM VORD-SUORDV                           
478500     .                                                                    
478600     SKIP2                                                                
478700 S08-HAMTA-MASKINDATUM     SECTION.                                       
478800     MOVE 'S08-HAMTA-MASKINDATUM  '    TO WS-CURRENT-SECTION              
478900     SKIP3                                                                
479000     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
479100     CALL WDATKONV USING DAT-KDDATFORM                                    
479200                         DAT-I-TIDATUM                                    
479300                         DAT-O-TIDATUM                                    
479400                         DAT-KDSVAR                                       
479500     EJECT                                                                
479600     .                                                                    
479700 S10-UPPD-SPAR-KOLLI    SECTION.                                          
479800     MOVE 'S10-UPPD-SPAR-KOLLI    '    TO WS-CURRENT-SECTION              
479900                                                                          
480000     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
480100                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
480200     END-COMPUTE                                                          
480300*                                                                         
480400     COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                        
480500             SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                      
480600     END-COMPUTE                                                          
480700*                                                                         
480800     COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +                
480900           SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                        
481000     END-COMPUTE                                                          
481100*                                                                         
481200     COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC +                
481300       SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                        
481400     END-COMPUTE                                                          
481500*                                                                         
481600     COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL +        
481700       SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART                    
481800     END-COMPUTE                                                          
481900*                                                                         
482000     MOVE SPAR-KDVALISO          TO ARB-KOLLI-KDVALISO                    
482100     MOVE SPAR-KDVALISO-EXP      TO ARB-KOLLI-KDVALISO-EXP                
482200*                                                                         
482300     IF      SPAR-PRAD-KVFLAMP   >  ZERO                                  
482400        AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                     
482500        OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                 
482600         MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                     
482700     END-IF                                                               
482800                                                                          
482900     IF      SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                  
483000     AND     SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                    
483100       MOVE  SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                     
483200     END-IF                                                               
483300*                                                                         
483400     .                                                                    
483500     EJECT                                                                
483600 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
483700     MOVE 'S11-UPPD-KOLLI-FRAN-ARB'    TO WS-CURRENT-SECTION              
483800                                                                          
483900     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
484000     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
484100*REM ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
484200     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
484300     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
484400     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
484500     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
484600     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
484700     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
484800*                                                                         
484900     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
485000        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
485100        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
485200         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
485300     END-IF                                                               
485400*                                                                         
485500     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
485600         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
485700     END-IF                                                               
485800     SKIP2                                                                
485900     MOVE WS-KDKOLLI             TO KOLLI-KDKOLLI                         
486000     MOVE WS-KDORDKL             TO KOLLI-KDORDKL                         
486100     MOVE WS-FLAUTFAK            TO KOLLI-FLAUTFAK                        
486200     MOVE WS-KDEMBTYP            TO KOLLI-KDEMBTYP                        
486300*    MOVE WS-VKORDBTO            TO KOLLI-VKORDBTO-KOLLI                  
486400                                                                          
486500     IF   KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                     
486600          MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI               
486700     END-IF                                                               
486800*                                                                         
486900     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
487000       ADD 0.1                TO KOLLI-VKORDBTO-KOLLI                     
487100     END-IF                                                               
487200     MOVE KOLLI-VKORDBTO-KOLLI TO WS-KOLLI-VKORDBTO-KOLLI                 
487300*                                                                         
487400     IF KOLLI-IDKOLLI-SAMP          >  ZERO                               
487500        MOVE NEJ                    TO KOLLI-FLUTLAST                     
487600     ELSE                                                                 
487700        IF KOLLI-FLAUTFAK = JA AND DIST03-SVERIGE-2                       
487800          MOVE NEJ                    TO KOLLI-FLUTLAST                   
487900        END-IF                                                            
488000     END-IF                                                               
488100                                                                          
488200     IF KOLLI-KDFARLIG-KOLLI = +4                                         
488300     OR KOLLI-KDFARLIG-KOLLI = +7                                         
488400       MOVE +950                    TO KOLLI-ADFLOMR                      
488500     END-IF                                                               
488600*                                                                         
488700     MOVE ZERO                TO ARB-KOLLI-KVFALRAD                       
488800     MOVE ZERO                TO ARB-KOLLI-KVORDRAD                       
488900     MOVE ZERO                TO ARB-KOLLI-VKORDNTO                       
489000     MOVE ZERO                TO ARB-KOLLI-SUORDV-LOC                     
489100     MOVE ZERO                TO ARB-KOLLI-SUORDV-LOCPREL                 
489200     MOVE ZERO                TO ARB-KOLLI-SUORDV                         
489300     MOVE ZERO                TO ARB-KOLLI-SUORDV-EXP                     
489400     MOVE ZERO                TO ARB-KOLLI-KDVALISO                       
489500     MOVE ZERO                TO ARB-KOLLI-KDVALISO-EXP                   
489600     MOVE ZERO                TO ARB-KOLLI-KVFLAMP                        
489700     MOVE ZERO                TO ARB-KOLLI-KDFARLIG                       
489800*                                                                         
489900     .                                                                    
490000     EJECT                                                                
490100 S12-SKAPA-4322 SECTION.                                                  
490200     MOVE 'S12-SKAPA-4322 '            TO WS-CURRENT-SECTION              
490300                                                                          
490400*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
490500     IF DIST03-SVERIGE-100-799                                            
490600     OR DIST03-NORGE                                                      
490700     OR DIST03-DANMARK-900                                                
490800     OR DIST85-PU-VIA-VR                                                  
490900     OR DIST21-TYRE                                                       
491000     AND NOT DIST47-INTERNA                                               
491100        MOVE WS-IDPRODNR TO XXJK-4322-IDPRODNR                            
491200        MOVE KOLLI-IDKOLLI  TO XXJK-4322-IDKOLLI                          
491300        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
491400        PERFORM IMS-ISRT-4322-SEGM                                        
491500     END-IF                                                               
491600     .                                                                    
491700     EJECT                                                                
491800 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
491900     MOVE 'S16-UPPD-FARLIGT-GODS-DATA' TO WS-CURRENT-SECTION              
492000     SKIP3                                                                
492100     MOVE +1 TO FG-INDX                                                   
492200     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
492300                                                                          
492400       IF TAB-IDPSN(FG-INDX) > ZERO                                       
492500         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
492600         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
492700                                       TAB-VKART-FG(FG-INDX) /            
492800                                       ARB-ANTAL-KOLLI                    
492900         END-COMPUTE                                                      
493000         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
493100                                       ARB-ANTAL-KOLLI                    
493200         END-COMPUTE                                                      
493300                                                                          
493400       ELSE                                                               
493500         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
493600                                       KOLLI-VKART-FG(FG-INDX)            
493700                                       KOLLI-VLFG(FG-INDX)                
493800       END-IF                                                             
493900                                                                          
494000       ADD +1 TO FG-INDX                                                  
494100     END-PERFORM                                                          
494200                                                                          
494300     IF TAB-IDPSN(1) > ZERO                                               
494400       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / ARB-ANTAL-KOLLI              
494500       END-COMPUTE                                                        
494600     ELSE                                                                 
494700       MOVE ZERO TO KOLLI-SUEQFG                                          
494800     END-IF                                                               
494900     .                                                                    
495000     EJECT                                                                
495100 S17-BERAEKNA-FG-FAELT SECTION.                                           
495200     MOVE 'S17-BERAEKNA-FG-FAELT '     TO WS-CURRENT-SECTION              
495300                                                                          
495400     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
495500                                 (SPAR-VLFG        *                      
495600                                  SPAR-PRAD-KVLEVART)                     
495700     END-COMPUTE                                                          
495800     IF SPAR-IDPSN = 10 OR 11                                             
495900       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
496000                                       (SPAR-VKART-FG        *            
496100                                        SPAR-PRAD-KVLEVART)               
496200       END-COMPUTE                                                        
496300     ELSE                                                                 
496400       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
496500     END-IF                                                               
496600                                                                          
496700     MOVE 10 TO FG-INDX                                                   
496800     .                                                                    
496900     EJECT                                                                
497000 S18-GENERERA-AVVIKELSE-TRANS SECTION.                                    
497100     MOVE 'S18-GENERERA-AVVIKELSE-TRANS' TO WS-CURRENT-SECTION            
497200                                                                          
497300     PERFORM S18A-UPDATE-RY1-POST                                         
497400                                                                          
497500     MOVE WS-IDDISTR-NUM         TO TEST-IDDISTR                          
497600     IF NOT DIST19-SATS                                                   
497700       IF W-IDDC NOT = W-IDDC-B6                                          
497800          MOVE W-IDDC TO W-IDDC-B6                                        
497900          PERFORM IMS-GU-WDB601                                           
498000       END-IF                                                             
498100       IF DCS-CDC                                                         
498200       OR DCS-NDC                                                         
498300       OR (DCS-SDC AND DCS-CHINA)                                         
498400       OR SPAR-ORAD-FLFYSAVV = JA                                         
498500         PERFORM S18B-UPDATE-ORDBEK-WDQ1                                  
498600                                                                          
498700         IF SPAR-ORAD-KDORDKL = 0                                         
498800            IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                        
498900                PERFORM S18C-UPDATE-VOR-QUE                               
499000            ELSE                                                          
499100                PERFORM S18D-UPDATE-VORKONY                               
499200            END-IF                                                        
499300            PERFORM S25-DELETE-PRICE-Q-LINE                               
499400         END-IF                                                           
499500       END-IF                                                             
499600     END-IF                                                               
499700                                                                          
499800     IF WS-KDORDBEK              = 90 OR                                  
499900       (WS-KDORDBEK              = 91 AND                                 
500000        SW-TIRODAT-LIKA-MED-ZERO = JA)                                    
500100       PERFORM S18E-SKAPA-RYK-TRANS                                       
500200     END-IF                                                               
500300     .                                                                    
500400     EJECT                                                                
500500 S18A-UPDATE-RY1-POST         SECTION.                                    
500600     MOVE 'S18A-UPDATE-RY1-POST '       TO WS-CURRENT-SECTION             
500700                                                                          
500800     IF LOGG-IDLOGLOP = 9                                                 
500900        MOVE ZERO                TO   LOGG-IDLOGLOP                       
501000     END-IF                                                               
501100                                                                          
501200     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
501300     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
501400     ADD +1                               TO   LOGG-IDLOGLOP              
501500     MOVE 'RY1'                           TO   RY1-IDPTYP                 
501600                                               LOGG-IDPTYP                
501700     MOVE SPAR-ORAD-BERADREF              TO   RY1-BERADREF               
501800     MOVE SPAR-ORAD-BEVOLREF              TO   RY1-BEVOLREF               
501900     MOVE WS-IDKUNDRF                     TO   RY1-IDKUNDRF               
502000     MOVE SPAR-ORAD-IDARTNR               TO   RY1-IDARTNR                
502100     MOVE SPAR-ORAD-FLRESTN               TO   RY1-FLRESTN                
502200     MOVE SPAR-ORAD-FLDIRLEV              TO   RY1-FLDIRLEV               
502300     MOVE SPAR-ORAD-IDKUNDRF-RO           TO   RY1-IDKUNDRF-RO            
502400     MOVE SPAR-ORAD-FLTILLK               TO   RY1-FLTILLK                
502500                                                                          
502600     IF SPAR-ORAD-KDDSP = 0                                               
502700        MOVE 1                            TO  RY1-KDDSP                   
502800     ELSE                                                                 
502900        MOVE SPAR-ORAD-KDDSP              TO  RY1-KDDSP                   
503000     END-IF                                                               
503100     MOVE WS-KDFAKTYP                     TO  RY1-KDFAKTYP                
503200                                                                          
503300     IF   WS-KDORDBEK = 90 AND WS-KDTPOTYP =  6                           
503400       MOVE 91                            TO  RY1-KDORDBEK                
503500     ELSE                                                                 
503600       MOVE WS-KDORDBEK                   TO  RY1-KDORDBEK                
503700     END-IF                                                               
503800                                                                          
503900     MOVE SPAR-ORAD-KDORDING              TO  RY1-KDORDING                
504000     MOVE SPAR-ORAD-KDORDTYP              TO  RY1-KDORDTYP                
504100     MOVE SPAR-ORAD-KDKVBRYT              TO  RY1-KDKVBRYT                
504200     MOVE WS-KDTPOTYP                     TO  RY1-KDTPOTYP                
504300     MOVE SPAR-ORAD-KDVRINFO              TO  RY1-KDVRINFO                
504400     MOVE SPAR-ORAD-KVBEART               TO  RY1-KVBEART                 
504500     MOVE SPAR-ORAD-KVAVBART              TO  RY1-KVAVBART                
504600     MOVE WS-KVORAPP-TOTAL                TO  RY1-KVAVART                 
504700     MOVE SPAR-ORAD-KVLEVART              TO  RY1-KVLEVART                
504800     MOVE SPAR-ORAD-REKSIFFR              TO  RY1-REKSIFFR                
504900                                                                          
505000     MOVE SPAR-ORAD-IDARTNR               TO  W-IDARTNR                   
505100                                                                          
505200     PERFORM IMS-GU-ARTC11                                                
505300     MOVE CLAG-TIDISPIN                   TO  RY1-TIDISPIN                
505400                                              WS-TIDISPIN                 
505500                                                                          
505600     MOVE SPAR-ORAD-TIUTSKR               TO  RY1-TIORDREG                
505700     MOVE SPAR-ORAD-TIRODAT               TO  RY1-TIRODAT                 
505800                                                                          
505900     MOVE WS-IDORDER                      TO  W-201-IDORDER               
506000     PERFORM IMS-GU-ORQI01                                                
506100                                                                          
506200     MOVE WS-IDDISTR-NUM                  TO  RY1S-IDDISTR                
506300     MOVE W-IDDC                          TO  RY1S-IDDC                   
506400     MOVE WS-IDKUNDNR-NUM                 TO  RY1S-IDKUNDNR               
506500     IF  OHUV-FLVORKO = JA                                                
506600     OR  OHUV-FLVORKO = YES                                               
506700         MOVE JA                          TO  RY1S-FLVORKO                
506800     ELSE                                                                 
506900         MOVE OHUV-FLVORKO                TO  RY1S-FLVORKO                
507000     END-IF                                                               
507100     MOVE OHUV-FLFORBI                    TO  RY1S-FLFORBI                
507200     MOVE OHUV-FLOVRLEV                   TO  RY1S-FLOVRLEV               
507300     MOVE SPAR-ORAD-KDPRODSL              TO  RY1S-KDPRODSL               
507400     MOVE SPAR-ORAD-IDSYSTEM              TO  RY1S-IDSYSTEM               
507500     MOVE WS-FLLSBOK                      TO  RY1S-FLLSBOK                
507600     MOVE WS-FLORDSPE                     TO  RY1S-FLORDSPE               
507700     MOVE SPAR-ORAD-KDFRAKT               TO  RY1S-KDFRAKT                
507800     MOVE SPAR-ORAD-KDORDKL               TO  RY1S-KDORDKL                
507900     MOVE SPAR-ORAD-KVANNANT              TO  RY1S-KVANNANT               
508000     MOVE SPAR-ORAD-KVSLATT               TO  RY1S-KVSLATT                
508100                                                                          
508200     MOVE RY1S-WDGZRY1S                   TO  LOGG-SORTPOST               
508300     MOVE RY1-WDGZRY1                     TO  LOGG-LOGGPOST               
508400*                                                                         
508500     PERFORM IMS-ISRT-AVVIKELSE                                           
508600*                                                                         
508700     PERFORM UNTIL SEGMENT-FOUND                                          
508800                                                                          
508900       IF LOGG-IDLOGLOP = 9                                               
509000         MOVE ZERO              TO LOGG-IDLOGLOP                          
509100         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
509200       END-IF                                                             
509300                                                                          
509400       ADD +1                   TO LOGG-IDLOGLOP                          
509500       PERFORM IMS-ISRT-AVVIKELSE                                         
509600     END-PERFORM                                                          
509700     EJECT                                                                
509800     .                                                                    
509900 S18B-UPDATE-ORDBEK-WDQ1      SECTION.                                    
510000     MOVE 'S18B-UPDATE-ORDBEK-WDQ1'     TO WS-CURRENT-SECTION             
510100                                                                          
510200     MOVE WS-IDORDER                      TO OBKR-IDORDER                 
510300     MOVE SPAR-ORAD-IDARTNR               TO OBKR-IDARTNR                 
510400*--- LÄS FRAM TILL FÖRSTA LEDIGA LÖPNR                                    
510500     MOVE OBKR-IDORDER                    TO W-IDORDER-Q1-MIN             
510600                                             W-IDORDER-Q1-MAX             
510700     MOVE OBKR-IDARTNR                    TO W-IDARTNR-Q1-MIN             
510800                                             W-IDARTNR-Q1-MAX             
510900     MOVE +1                              TO W-IDLOPNR-Q1-MIN             
511000                                             W-IDLOPNR-Q1-MAX             
511100                                             W-IDSEKVNR-Q1-MIN            
511200                                             W-IDSEKVNR-Q1-MAX            
511300     PERFORM IMS-GU-ORQM01                                                
511400     PERFORM UNTIL SEGMENT-MISSING                                        
511500        ADD +1                            TO W-IDLOPNR-Q1-MIN             
511600                                             W-IDLOPNR-Q1-MAX             
511700        PERFORM IMS-GU-ORQM01                                             
511800     END-PERFORM                                                          
511900     MOVE W-IDLOPNR-Q1-MIN                TO OBKR-IDLOPNR                 
512000     MOVE 1                               TO OBKR-IDSEKVNR                
512100     MOVE W-IDDC                          TO OBKR-IDDC                    
512200     MOVE SPAR-ORAD-IDDC-RO               TO OBKR-IDDC-RO                 
512300     MOVE SPAR-ORAD-KDOI                  TO OBKR-KDOI                    
512400     MOVE SPAR-ORAD-CLEARGROUP            TO OBKR-CLEARGROUP              
512500     MOVE WS-KDORDBEK                     TO OBKR-KDORDBEK                
512600     MOVE IDPGM                           TO OBKR-IDPGM                   
512700     MOVE SPACE                           TO OBKR-BEERS                   
512800     MOVE SPAR-BEKUNDRF                   TO OBKR-BEKUNDRF                
512900     MOVE SPAR-ORAD-BERADREF              TO OBKR-BERADREF                
513000     MOVE SPAR-ORAD-BEVOLREF              TO OBKR-BEVOLREF                
513100     MOVE SPAR-ORAD-IDKAMPRF              TO OBKR-IDKAMPRF                
513200     MOVE 0                               TO OBKR-DIERS-KVOT              
513300     MOVE NEJ                             TO OBKR-FLAKPLOC                
513400     MOVE NEJ                             TO OBKR-FLSLATT                 
513500     MOVE SPAR-ORAD-FLINVEST              TO OBKR-FLINVEST                
513600     MOVE JA                              TO OBKR-FLOBOK                  
513700     MOVE NEJ                             TO OBKR-FLOBTRAN                
513800     MOVE NEJ                             TO OBKR-FLOBPRT                 
513900     MOVE SPAR-ORAD-FLPRTILL              TO OBKR-FLPRTILL                
514000     MOVE SPAR-ORAD-FLRESTN               TO OBKR-FLRESTN                 
514100     MOVE NEJ                             TO OBKR-FLTILLK                 
514200     MOVE 0                               TO OBKR-IDARTNR-TILLK           
514300     MOVE WS-IDDISTR-NUM                  TO OBKR-IDDISTR                 
514400     MOVE WS-IDKUNDNR-NUM                 TO OBKR-IDKUNDNR                
514500                                                                          
514600     MOVE WS-IDKUNDRF                     TO WS-IDKUNDRF-OLD              
514700     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
514800     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
514900                                                                          
515000     MOVE SPAR-ORAD-IDKUNDRF-RO           TO WS-IDKUNDRF-OLD              
515100     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
515200     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
515300     MOVE SPAR-ORAD-IDLEVNR               TO OBKR-IDLEVNR                 
515400     MOVE SPAR-ORAD-IDLOPNR-RO            TO OBKR-IDLOPNR-RO              
515500     MOVE SPAR-ORAD-IDSYSTEM              TO OBKR-IDSYSTEM                
515600     MOVE SPAR-ORAD-KDDSP                 TO OBKR-KDDSP                   
515700     MOVE 0                               TO OBKR-KDERS                   
515800     MOVE SPAR-ORAD-KDKVBRYT              TO OBKR-KDKVBRYT                
515900     MOVE SPAR-ORAD-KDPRTYP               TO OBKR-KDPRTYP                 
516000     MOVE 0                               TO OBKR-KDTPOTYP                
516100     MOVE SPAR-ORAD-KDVRINFO              TO OBKR-KDVRINFO                
516200                                                                          
516300     EVALUATE TRUE                                                        
516400       WHEN WS-KDORDBEK = 90 OR 91                                        
516500         MOVE 0                          TO OBKR-KVANNANT                 
516600       WHEN WS-KDORDBEK = 80 OR 81 OR 93                                  
516700         IF DCS-CDC                                                       
516800         OR DCS-NDC                                                       
516900           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVBEART                      
517000                                 - SPAR-ORAD-KVLEVART                     
517100                                 - SPAR-ORAD-KVANNANT                     
517200           END-COMPUTE                                                    
517300         ELSE                                                             
517400           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVAVBART                     
517500                                 - SPAR-ORAD-KVLEVART                     
517600           END-COMPUTE                                                    
517700         END-IF                                                           
517800       WHEN OTHER                                                         
517900         MOVE 'FELAKTIG ORDERBEKR-KOD'   TO RKOD-FELTEXT                  
518000         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
518100     END-EVALUATE                                                         
518200                                                                          
518300     MOVE SPAR-ORAD-KVAVBART              TO OBKR-KVAVBART                
518400     MOVE SPAR-ORAD-KVBEART               TO OBKR-KVBEART                 
518500                                             OBKR-KVBEART-Q               
518600     MOVE 0                               TO OBKR-KVBEART-TILLK           
518700                                             OBKR-KVPREAVB                
518800                                             OBKR-KVPRERO                 
518900     MOVE CLAG-KVQPACK-1                  TO OBKR-KVQPACK                 
519000*                                                                         
519100     IF WS-KDORDBEK = 90 OR 91                                            
519200        IF DCS-CDC                                                        
519300        OR DCS-NDC                                                        
519400          MOVE WS-KVORAPP-TOTAL             TO OBKR-KVRO                  
519500        ELSE                                                              
519600          MOVE WS-KVORAPP-PACK              TO OBKR-KVRO                  
519700        END-IF                                                            
519800        MOVE WS-DAGENS-DATUM              TO OBKR-TIRODAT                 
519900     ELSE                                                                 
520000        MOVE 0                            TO OBKR-KVRO                    
520100        MOVE 000000                       TO OBKR-TIRODAT                 
520200     END-IF                                                               
520300*                                                                         
520400     MOVE SPAR-ORAD-KVSLATT               TO OBKR-KVSLATT                 
520500     MOVE SPAR-ORAD-PRARTNTO              TO OBKR-PRARTNTO                
520600     MOVE SPAR-ORAD-DEAL-PR-LINE          TO OBKR-DEAL-PR-LINE            
520700                                                                          
520800     IF SPAR-ORAD-KDVALISO-EXP NOT = SPACE                                
520900       MOVE SPAR-ORAD-KDVALISO-EXP        TO OBKR-KDVALISO                
521000     END-IF                                                               
521100                                                                          
521200     MOVE SPAR-ORAD-REKSIFFR              TO OBKR-REKSIFFR                
521300     MOVE 0                               TO OBKR-PRBPRIS                 
521400                                             OBKR-REKSIFFR-TILLK          
521500                                             OBKR-RERF-RAD                
521600                                             OBKR-TITPO                   
521700     MOVE WS-TIDISPIN                     TO OBKR-TIDISPIN                
521800     MOVE WS-TIORDREG                     TO OBKR-TIORDREG                
521900                                                                          
522000     MOVE ZERO                            TO WS-DAORDREG                  
522100     MOVE FUNCTION CURRENT-DATE(1:2)      TO WS-DAORDREG-TISS             
522200     MOVE OBKR-TIORDREG                   TO WS-DAORDREG-TIAAMMDD         
522300     COMPUTE OBKR-TITIORDD-9KOMPL =                                       
522400             WS-9KOMPL-GRUND - WS-DAORDREG                                
522500     END-COMPUTE                                                          
522600     MOVE SPAR-ORAD-TIPRIS                TO OBKR-TIPRIS                  
522700                                                                          
522800     MOVE WS-DAGENS-DATUM                 TO OBKR-TIREGDAT                
522900     MOVE   WS-TTMMSS                     TO OBKR-TIREGTID                
523000                                                                          
523100     IF WS-KDORDBEK = 90 OR 91                                            
523200        MOVE ZERO                         TO WS-DARODAT                   
523300        MOVE FUNCTION CURRENT-DATE(1:2)   TO WS-DARODAT-TISS              
523400        MOVE OBKR-TIRODAT                 TO WS-DARODAT-TIAAMMDD          
523500        COMPUTE OBKR-TITIREGD-9KOMPL =                                    
523600             WS-9KOMPL-GRUND - WS-DARODAT                                 
523700        END-COMPUTE                                                       
523800     ELSE                                                                 
523900        MOVE 0                            TO OBKR-TITIREGD-9KOMPL         
524000     END-IF                                                               
524100                                                                          
524200     MOVE SPAR-ORAD-KDFRAKT                TO OBKR-KDFRAKT                
524300     MOVE SPAR-ORAD-KDORDKL                TO OBKR-KDORDKL                
524400     MOVE SPACE                            TO OBKR-IDBIL                  
524500                                                                          
524600     MOVE OHUV-KDORDTYP-LDC                TO OBKR-KDORDTYP-LDC           
524700     MOVE OHUV-TIREPDAT                    TO OBKR-TIREPDAT               
524800     MOVE SPAR-ORAD-IDKUNDRF-WIP           TO OBKR-IDKUNDRF-WIP           
524900     MOVE ZERO                             TO OBKR-TIDLEVDAT              
525000     MOVE SPAR-ORAD-PRAVCOST               TO OBKR-PRAVCOST               
525100                                                                          
525200     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
525300        MOVE OBKR-IDARTNR         TO W-IDARTNR                            
525400        MOVE DCS-IDLANDX2         TO W-IDLAND                             
525500        PERFORM IMS-GU-WDK712                                             
525600        IF SEGMENT-FOUND AND LART-FLREFERAL = JA                          
525700           MOVE 98                TO OBKR-KDORDBEK                        
525800        END-IF                                                            
525900     END-IF                                                               
526000                                                                          
526100     PERFORM IMS-ISRT-ORQM01                                              
526200     .                                                                    
526300     EJECT                                                                
526400 S18C-UPDATE-VOR-QUE          SECTION.                                    
526500     MOVE 'S18C-UPDATE-VOR-QUE    '     TO WS-CURRENT-SECTION             
526600                                                                          
526700     IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                           
526800        MOVE SPAR-ORAD-BERADREF            TO 4542-BERADREF               
526900        MOVE WS-IDDISTR-NUM                TO 4542-IDDISTR                
527000        MOVE CLAG-IDANSK                   TO 4542-IDANSK                 
527100        MOVE OBKR-IDDC                     TO 4542-IDDC                   
527200        MOVE SPAR-ORAD-IDARTNR             TO 4542-IDARTNR                
527300        MOVE WS-IDKUNDNR-NUM               TO 4542-IDKUNDNR               
527400        MOVE WS-IDKUNDRF                   TO WS-IDKUNDRF-OLD             
527500        MOVE WS-IDORDNR5-OLD               TO WS-IDORDNR7-NEW             
527600        MOVE WS-IDKUNDRF-NEW               TO 4542-IDKUNDRF               
527700        MOVE WS-IDORDER                    TO 4542-IDORDER                
527800        MOVE SPACE                         TO 4542-IDUSER                 
527900        MOVE 93                            TO 4542-KDORDBEK               
528000        MOVE SPAR-ORAD-KDPRTYP             TO 4542-KDPRTYP                
528100        MOVE 0                             TO 4542-KDVORATG               
528200        MOVE SPAR-ORAD-KVBEART             TO 4542-KVBEART                
528300                                              4542-KVBEART-Q              
528400        IF SPAR-ORAD-FLFYSAVV = JA                                        
528500          IF DCS-SDC                                                      
528600            MOVE ZERO                 TO WS-AVVIKELSE-UTSKR               
528700            COMPUTE WS-AVVIKELSE-UTSKR =                                  
528800                    SPAR-ORAD-KVBEART - SPAR-ORAD-KVAVBART                
528900            END-COMPUTE                                                   
529000            COMPUTE WS-SUMMA =                                            
529100                    SPAR-ORAD-KVLEVART + WS-AVVIKELSE-UTSKR               
529200            END-COMPUTE                                                   
529300            MOVE WS-SUMMA                  TO 4542-KVPREAVB               
529400          ELSE                                                            
529500            MOVE SPAR-ORAD-KVLEVART        TO 4542-KVPREAVB               
529600          END-IF                                                          
529700*         WS-SUMMA MINUS ORAD-KVBEART ÄR AVVIKELSE I PACKNINGEN           
529800*         VILKET ÄR DET SOM VISAS (RÄKNAS FRAM) PÅ 4224-BILDEN            
529900        ELSE                                                              
530000          MOVE SPAR-ORAD-KVLEVART          TO 4542-KVPREAVB               
530100        END-IF                                                            
530200        MOVE SPAR-ORAD-PRARTNTO           TO 4542-PRARTNTO                
530300        MOVE SPAR-ORAD-DEAL-PR-LINE                                       
530400                           TO 4542-DEAL-PR-LINE                           
530500        MOVE SPACE                        TO 4542-TEVORMRK                
530600        MOVE WS-DAGENS-DATUM              TO 4542-TIREGDAT                
530700        MOVE WS-TTMMSS                    TO 4542-TIREGTID                
530800        MOVE 0                            TO 4542-TIUPPDAT                
530900                                             4542-TIUPPTID                
531000        MOVE 1                            TO 4542-IDLOPNR                 
531100        MOVE SPAR-ORAD-IDLEVNR            TO 4542-IDLEVNR                 
531200*                                                                         
531300        PERFORM IMS-ISRT-4542                                             
531400        PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                            
531500          ADD +1                           TO 4542-IDLOPNR                
531600          PERFORM IMS-ISRT-4542                                           
531700        END-PERFORM                                                       
531800                                                                          
531900        IF  DCS-NDC-CN                                                    
532000        OR (DCS-NDC-NA AND DCS-USA)                                       
532100           IF SLAG-IDDC-REF = SPACE                                       
532200                                                                          
532300             MOVE WS-IDDISTR-NUM    TO S27-IDDISTR                        
532400             MOVE WS-IDKUNDNR-NUM   TO S27-IDKUNDNR                       
532500             MOVE SPAR-ORAD-IDARTNR TO S27-IDARTNR                        
532600                                       W-IDARTNR                          
532700             MOVE W-IDDC            TO W-711-IDDC                         
532800             PERFORM IMS-GU-WDK722                                        
532900             MOVE XLAG-IDANSK       TO S27-IDANSK                         
533000             MOVE SLAG-IDDC         TO WS-MID-IDDC                        
533100             MOVE SLAG-IDLEVNR      TO WS-MID-IDLEVNR                     
533200             PERFORM S27-STARTA-W2T191X                                   
533300           END-IF                                                         
533400        END-IF                                                            
533500                                                                          
533600        IF SPAR-ORAD-IDLEVNR = SPACE AND                                  
533700           WS-FLORDSPE = NEJ          AND                                 
533800           WS-FLOVRLEV = NEJ                                              
533900           IF  DCS-NDC                                                    
534000           OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                       
534100             MOVE SPAR-ORAD-IDARTNR       TO W-IDARTNR                    
534200             MOVE W-IDDC                  TO W-711-IDDC                   
534300             PERFORM IMS-GHU-WDK711                                       
534400                                                                          
534500             IF DCS-NDC                                                   
534600               COMPUTE SLAG-KVOKS-DAG =                                   
534700                       SLAG-KVOKS-DAG +                                   
534800                       (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)           
534900               END-COMPUTE                                                
535000             ELSE                                                         
535100               COMPUTE SLAG-KVOKS-DAG =                                   
535200                       SLAG-KVOKS-DAG +                                   
535300                      (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)           
535400               END-COMPUTE                                                
535500             END-IF                                                       
535600                                                                          
535700             PERFORM IMS-REPL-WDK7                                        
535800           ELSE                                                           
535900             IF DCS-CDC                                                   
536000               MOVE SPAR-ORAD-IDARTNR          TO W-901-IDARTNR           
536100               PERFORM IMS-GHU-WDK901                                     
536200                                                                          
536300               COMPUTE ART-KVOKS-VOR =                                    
536400                       ART-KVOKS-VOR +                                    
536500                       (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)           
536600               END-COMPUTE                                                
536700                                                                          
536800               PERFORM IMS-REPL-WDK901                                    
536900             END-IF                                                       
537000           END-IF                                                         
537100        END-IF                                                            
537200     END-IF                                                               
537300     .                                                                    
537400     EJECT                                                                
537500 S18D-UPDATE-VORKONY          SECTION.                                    
537600     MOVE 'S18D-UPDATE-VORKONY    '     TO WS-CURRENT-SECTION             
537700                                                                          
537800     MOVE SPAR-ORAD-IDARTNR TO S28-IDARTNR                                
537900     PERFORM S28-BESTAM-LENVR-ANSK                                        
538000                                                                          
538100*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
538200                                                                          
538300     PERFORM S26-SOK-RAD-VORKO                                            
538400                                                                          
538500     IF  TRAFF-VORKO                                                      
538600       IF DCS-CDC                                                         
538700           COMPUTE VOR-KVPREAVB    = VOR-KVPREAVB                         
538800                                 - SPAR-ORAD-KVBEART                      
538900                                 + SPAR-ORAD-KVLEVART                     
539000           END-COMPUTE                                                    
539100           COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                          
539200                                 - SPAR-ORAD-KVBEART                      
539300                                 + SPAR-ORAD-KVLEVART                     
539400           END-COMPUTE                                                    
539500       ELSE                                                               
539600           COMPUTE VOR-KVPREAVB    = VOR-KVPREAVB                         
539700                                 - SPAR-ORAD-KVAVBART                     
539800                                 + SPAR-ORAD-KVLEVART                     
539900           END-COMPUTE                                                    
540000           COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                          
540100                                 - SPAR-ORAD-KVAVBART                     
540200                                 + SPAR-ORAD-KVLEVART                     
540300           END-COMPUTE                                                    
540400       END-IF                                                             
540500       IF    VOR-KVPREAVB = 0                                             
540600           MOVE '7'                TO VOR-KDVORATG                        
540700           MOVE 93                 TO VOR-KDORDBEK                        
540800           IF VOR-TIKLAR = ZERO                                           
540900              MOVE WS-DAGENS-DATUM TO VOR-TIKLAR                          
541000              COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                    
541100                                    / 100                                 
541200              END-COMPUTE                                                 
541300           END-IF                                                         
541400       END-IF                                                             
541500       PERFORM IMS-REPL-SEQB-WDA601                                       
541600                                                                          
541700       MOVE WS-DAGENS-DATUM     TO VOR-TIREGDAT-AVV                       
541800       ADD +1                   TO WS-VOR-TID-BRIST                       
541900       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-AVV                       
542000       SUBTRACT WS-DAGENS-DATUM FROM 9999999                              
542100                                GIVING VOR-TIREGDAT-AVV9                  
542200       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
542300                                 GIVING VOR-TIREGTID-AVV9                 
542400       MOVE '0000000   '        TO VOR-IDKUNDRF-LEV                       
542500       MOVE 0                   TO VOR-TIREGDAT-LEV                       
542600       MOVE 0                   TO VOR-TIREGTID-LEV                       
542700       IF DCS-CDC                                                         
542800           SUBTRACT SPAR-ORAD-KVLEVART                                    
542900                                FROM SPAR-ORAD-KVBEART                    
543000                                GIVING VOR-KVBEART                        
543100                                       VOR-KVBEART-Q                      
543200       ELSE                                                               
543300           SUBTRACT SPAR-ORAD-KVLEVART                                    
543400                                FROM SPAR-ORAD-KVAVBART                   
543500                                GIVING VOR-KVBEART                        
543600                                       VOR-KVBEART-Q                      
543700       END-IF                                                             
543800       MOVE 0                   TO VOR-KVPREAVB                           
543900       MOVE W-IDDC              TO VOR-IDDC                               
544000       MOVE SPACE               TO VOR-IDUSER                             
544100       MOVE 93                  TO VOR-KDORDBEK                           
544200       MOVE '0'                 TO VOR-KDVORATG                           
544300       MOVE 0                   TO VOR-TIKLAR                             
544400       MOVE 0                   TO VOR-TIKLATID                           
544500                                                                          
544600       PERFORM IMS-ISRT-WDA601                                            
544700       PERFORM UNTIL ISRT-OK                                              
544800          ADD +1                  TO WS-VOR-TID-BRIST                     
544900          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
545000          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
545100                                GIVING VOR-TIREGTID-AVV9                  
545200          PERFORM IMS-ISRT-WDA601                                         
545300       END-PERFORM                                                        
545400     ELSE                                                                 
545500*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
545600*                                        BORDE NOG INTE FÖREKOMMA         
545700       MOVE WS-IDDISTR-NUM      TO VOR-IDDISTR                            
545800       MOVE WS-IDKUNDNR-NUM     TO VOR-IDKUNDNR                           
545900       MOVE WS-IDKUNDRF         TO WS-IDKUNDRF-OLD                        
546000       MOVE WS-IDORDNR5-OLD     TO WS-IDORDNR7-NEW                        
546100       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF                           
546200       MOVE WS-TIORDREG         TO VOR-TIREGDAT-URSP                      
546300       MOVE SPAR-ORAD-IDARTNR   TO VOR-IDARTNR                            
546400       ADD +1                   TO WS-VOR-TID-BRIST                       
546500       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
546600       MOVE 0                   TO VOR-TIREGDAT-AVV                       
546700       MOVE 0                   TO VOR-TIREGTID-AVV                       
546800       SUBTRACT 0            FROM 9999999                                 
546900                              GIVING VOR-TIREGDAT-AVV9                    
547000       SUBTRACT 0            FROM 999999999                               
547100                              GIVING VOR-TIREGTID-AVV9                    
547200       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF-LEV                       
547300       MOVE WS-TIORDREG         TO VOR-TIREGDAT-LEV                       
547400       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
547500       MOVE S28-IDANSK          TO VOR-IDANSK                             
547600                                                                          
547700       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
547800       PERFORM IMS-GU-WDP4A1                                              
547900       IF SEGMENT-FOUND                                                   
548000          MOVE SEQA-IDROLL      TO VOR-IDROLL                             
548100       ELSE                                                               
548200          MOVE DEF-IDROLL       TO VOR-IDROLL                             
548300       END-IF                                                             
548400                                                                          
548500       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
548600       MOVE SPAR-ORAD-BERADREF  TO VOR-BERADREF                           
548700       IF  DCS-CDC                                                        
548800           SUBTRACT SPAR-ORAD-KVLEVART                                    
548900                                FROM SPAR-ORAD-KVBEART                    
549000                                GIVING VOR-KVBEART-URSP                   
549100                                       VOR-KVBEART                        
549200       ELSE                                                               
549300           SUBTRACT SPAR-ORAD-KVLEVART                                    
549400                                FROM SPAR-ORAD-KVAVBART                   
549500                                GIVING VOR-KVBEART-URSP                   
549600                                       VOR-KVBEART                        
549700       END-IF                                                             
549800       MOVE 0                   TO VOR-KVPREAVB                           
549900                                   VOR-KVBEART-Q                          
550000       MOVE W-IDDC              TO VOR-IDDC                               
550100       MOVE SPACE               TO VOR-IDUSER                             
550200       MOVE 93                  TO VOR-KDORDBEK                           
550300       MOVE SPAR-ORAD-KDPRTYP   TO VOR-KDPRTYP                            
550400       MOVE '7'                  TO VOR-KDVORATG                          
550500       MOVE SPAR-ORAD-PRARTNTO  TO VOR-PRARTNTO                           
550600       MOVE '  '                TO VOR-TEVORMRK                           
550700*      MOVE '  '                TO VOR-TEVORMRK-SC                        
550800       MOVE 0                   TO VOR-TIUPPDAT                           
550900       MOVE 0                   TO VOR-TIUPPTID                           
551000       IF VOR-TIKLAR = ZERO                                               
551100          MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                             
551200          COMPUTE VOR-TIKLATID   = WS-VOR-TID-BRIST                       
551300                                 / 100                                    
551400          END-COMPUTE                                                     
551500       END-IF                                                             
551600       MOVE SPAR-ORAD-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                    
551700       PERFORM IMS-ISRT-WDA601                                            
551800       PERFORM UNTIL ISRT-OK                                              
551900          ADD +1              TO VOR-TIREGTID-URSP                        
552000          ADD +1              TO VOR-TIREGTID-LEV                         
552100          PERFORM IMS-ISRT-WDA601                                         
552200       END-PERFORM                                                        
552300                                                                          
552400*--------------------------------------- EJ TRÄFF, AVVIK RAD              
552500       MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                         
552600       ADD +1                 TO WS-VOR-TID-BRIST                         
552700       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
552800       SUBTRACT WS-DAGENS-DATUM  FROM 9999999                             
552900                                 GIVING VOR-TIREGDAT-AVV9                 
553000       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
553100                                 GIVING VOR-TIREGTID-AVV9                 
553200       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
553300       MOVE 0                 TO VOR-TIREGDAT-LEV                         
553400       MOVE 0                 TO VOR-TIREGTID-LEV                         
553500       IF DCS-CDC                                                         
553600           SUBTRACT SPAR-ORAD-KVLEVART                                    
553700                                FROM SPAR-ORAD-KVBEART                    
553800                                GIVING VOR-KVBEART-Q                      
553900       ELSE                                                               
554000           SUBTRACT SPAR-ORAD-KVLEVART                                    
554100                                FROM SPAR-ORAD-KVAVBART                   
554200                                GIVING VOR-KVBEART-Q                      
554300       END-IF                                                             
554400       MOVE 0                 TO VOR-KVPREAVB                             
554500       MOVE 93                TO VOR-KDORDBEK                             
554600       MOVE '0'               TO VOR-KDVORATG                             
554700       MOVE 0                 TO VOR-TIKLAR                               
554800       MOVE 0                 TO VOR-TIKLATID                             
554900                                                                          
555000       PERFORM IMS-ISRT-WDA601                                            
555100       PERFORM UNTIL ISRT-OK                                              
555200          ADD +1                TO WS-VOR-TID-BRIST                       
555300          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
555400          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
555500                                GIVING VOR-TIREGTID-AVV9                  
555600          PERFORM IMS-ISRT-WDA601                                         
555700       END-PERFORM                                                        
555800     END-IF                                                               
555900                                                                          
556000     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
556100                                + VOR-KVBEART-Q                           
556200                                - VOR-KVPREAVB                            
556300     END-COMPUTE                                                          
556400                                                                          
556500     PERFORM IMS-REPL-WDK611                                              
556600                                                                          
556700     MOVE WS-IDDISTR-NUM        TO S27-IDDISTR                            
556800     MOVE WS-IDKUNDNR-NUM       TO S27-IDKUNDNR                           
556900     MOVE SPAR-ORAD-IDARTNR     TO S27-IDARTNR                            
557000     MOVE S28-IDANSK            TO S27-IDANSK                             
557100     MOVE WC-CDC-SE             TO WS-MID-IDDC                            
557200     MOVE SPACE                 TO WS-MID-IDLEVNR                         
557300     PERFORM S27-STARTA-W2T191X                                           
557400                                                                          
557500*------------------------------SAMMA EFTERHANTERING SOM I S18C            
557600     IF SPAR-ORAD-IDLEVNR = SPACE AND                                     
557700        WS-FLORDSPE = NEJ        AND                                      
557800        WS-FLOVRLEV = NEJ                                                 
557900        IF  DCS-NDC                                                       
558000        OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                          
558100          MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                         
558200          MOVE W-IDDC                TO W-711-IDDC                        
558300          PERFORM IMS-GHU-WDK711                                          
558400                                                                          
558500          IF  DCS-NDC                                                     
558600            COMPUTE SLAG-KVOKS-DAG =                                      
558700                    SLAG-KVOKS-DAG +                                      
558800                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
558900            END-COMPUTE                                                   
559000          ELSE                                                            
559100            COMPUTE SLAG-KVOKS-DAG =                                      
559200                    SLAG-KVOKS-DAG +                                      
559300                   (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)              
559400            END-COMPUTE                                                   
559500          END-IF                                                          
559600                                                                          
559700          PERFORM IMS-REPL-WDK7                                           
559800        ELSE                                                              
559900          IF DCS-CDC                                                      
560000            MOVE SPAR-ORAD-IDARTNR        TO W-901-IDARTNR                
560100            PERFORM IMS-GHU-WDK901                                        
560200                                                                          
560300            COMPUTE ART-KVOKS-VOR =                                       
560400                    ART-KVOKS-VOR +                                       
560500                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
560600            END-COMPUTE                                                   
560700                                                                          
560800            PERFORM IMS-REPL-WDK901                                       
560900          END-IF                                                          
561000        END-IF                                                            
561100     END-IF                                                               
561200     .                                                                    
561300     EJECT                                                                
561400 S20-FINN-INTERVALL SECTION.                                              
561500     MOVE 'S20-FINN-INTERVALL     '     TO WS-CURRENT-SECTION             
561600                                                                          
561700     PERFORM IMS-GU-WDM211                                                
561800     PERFORM IMS-GNP-WDM221                                               
561900     IF SEGMENT-FOUND                                                     
562000       PERFORM UNTIL SEGMENT-MISSING                                      
562100         IF  WS-IDDISTR-NUM > KMRK-IDDISTR-TOM                            
562200         OR  WS-IDDISTR-NUM < KMRK-IDDISTR-FOM                            
562300           CONTINUE                                                       
562400         ELSE                                                             
562500           IF  WS-IDDISTR-NUM  = KMRK-IDDISTR-TOM                         
562600           AND WS-IDKUNDNR-NUM > KMRK-IDKUNDNR-TOM                        
562700             CONTINUE                                                     
562800           ELSE                                                           
562900             IF  WS-IDDISTR-NUM  = KMRK-IDDISTR-FOM                       
563000             AND WS-IDKUNDNR-NUM < KMRK-IDKUNDNR-FOM                      
563100               CONTINUE                                                   
563200             ELSE                                                         
563300               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
563400               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
563500               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
563600               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
563700             END-IF                                                       
563800           END-IF                                                         
563900         END-IF                                                           
564000         PERFORM IMS-GNP-WDM221                                           
564100       END-PERFORM                                                        
564200     END-IF                                                               
564300     .                                                                    
564400     EJECT                                                                
564500                                                                          
564600 S26-SOK-RAD-VORKO SECTION.                                               
564700     MOVE 'S26-SOK-RAD-VORKO      '     TO WS-CURRENT-SECTION             
564800                                                                          
564900     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
565000     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
565100                                                                          
565200     MOVE WS-IDDISTR-NUM    TO W-A601KY-MIN-IDDISTR                       
565300                               W-A601KY-MAX-IDDISTR                       
565400     MOVE WS-IDKUNDNR-NUM   TO W-A601KY-MIN-IDKUNDNR                      
565500                               W-A601KY-MAX-IDKUNDNR                      
565600     MOVE WS-IDKUNDRF       TO WS-IDKUNDRF-OLD                            
565700     MOVE WS-IDORDNR5-OLD   TO WS-IDORDNR7-NEW                            
565800     MOVE WS-IDKUNDRF-NEW   TO W-A601KY-MIN-IDKUNDRF                      
565900                               W-A601KY-MAX-IDKUNDRF                      
566000     MOVE WS-TIORDREG       TO W-A601KY-MIN-TIREGDAT                      
566100                               W-A601KY-MAX-TIREGDAT                      
566200     MOVE SPAR-ORAD-IDARTNR TO W-A601KY-MIN-IDARTNR                       
566300                               W-A601KY-MAX-IDARTNR                       
566400     MOVE NEJ               TO TRAFF-VORKO-SW                             
566500     IF W-IDDC NOT = W-IDDC-B6                                            
566600        MOVE W-IDDC TO W-IDDC-B6                                          
566700        PERFORM IMS-GU-WDB601                                             
566800     END-IF                                                               
566900                                                                          
567000     PERFORM IMS-GHU-SEQB-WDA601                                          
567100     PERFORM UNTIL SEGMENT-MISSING                                        
567200                OR SEGMENT-END                                            
567300                OR TRAFF-VORKO                                            
567400       IF  DCS-CDC                                                        
567500       AND SPAR-ORAD-KVBEART = VOR-KVPREAVB                               
567600           MOVE JA       TO TRAFF-VORKO-SW                                
567700       ELSE                                                               
567800         IF  SPAR-ORAD-KVAVBART = VOR-KVPREAVB                            
567900             MOVE JA       TO TRAFF-VORKO-SW                              
568000         ELSE                                                             
568100            PERFORM IMS-GHN-SEQB-WDA601                                   
568200         END-IF                                                           
568300       END-IF                                                             
568400     END-PERFORM                                                          
568500     .                                                                    
568600     EJECT                                                                
568700 S27-STARTA-W2T191X  SECTION.                                             
568800     MOVE 'S27-STARTA-W2T191X     '     TO WS-CURRENT-SECTION             
568900                                                                          
569000     COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17                   
569100     END-COMPUTE                                                          
569200     MOVE +1                    TO 2191-MID-KDCLAGER                      
569300     MOVE S27-IDARTNR-X         TO 2191-MID-IDARTNR                       
569400     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
569500                                   2191-MID-TISENBEK-KL                   
569600     MOVE SPACE                 TO 2191-MID-IDKR                          
569700     MOVE S27-IDANSK-X          TO 2191-MID-IDANSK                        
569800     MOVE '500'                 TO 2191-MID-KDLARM                        
569900     MOVE S27-IDDISTR-X         TO 2191-MID-IDDISTR                       
570000     MOVE S27-IDKUNDNR-X        TO 2191-MID-IDKUNDNR                      
570100     MOVE WS-IDKUNDRF           TO WS-IDKUNDRF-OLD                        
570200     MOVE WS-IDORDNR5-OLD       TO WS-IDORDNR7-NEW                        
570300     MOVE WS-IDKUNDRF-NEW       TO 2191-MID-IDKUNDRF                      
570400     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
570500     MOVE WS-MID-IDDC           TO 2191-MID-IDDC                          
570600     MOVE WS-MID-IDLEVNR        TO 2191-MID-IDLEVNR                       
570700                                                                          
570800     PERFORM IMS-15-PURG-2191                                             
570900                                                                          
571000     MOVE SPACE                 TO 2191-MID-W2I19101                      
571100     .                                                                    
571200     EJECT                                                                
571300 S28-BESTAM-LENVR-ANSK SECTION.                                           
571400     MOVE 'S28-BESTAM-LENVR-ANSK  '     TO WS-CURRENT-SECTION             
571500                                                                          
571600     MOVE S28-IDARTNR    TO W-IDARTNR                                     
571700     PERFORM IMS-GU-WDK601                                                
571800     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
571900                                                                          
572000     PERFORM IMS-GHNP-WDK611                                              
572100     MOVE CLAG-IDANSK    TO S28-IDANSK                                    
572200     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
572300     .                                                                    
572400     EJECT                                                                
572500 S18E-SKAPA-RYK-TRANS SECTION.                                            
572600     MOVE 'STA S18E-SKAPA-RYK'  TO WS-CURRENT-SECTION                     
572700                                                                          
572800     IF LOGG-IDLOGLOP = 9                                                 
572900       MOVE ZERO                TO   LOGG-IDLOGLOP                        
573000     END-IF                                                               
573100                                                                          
573200     ACCEPT  LOGG-TIAAMMDD      FROM DATE                                 
573300     ACCEPT  LOGG-TIKLOCK       FROM TIME                                 
573400     ADD     +1                 TO   LOGG-IDLOGLOP                        
573500                                                                          
573600     MOVE    'RYK'              TO   RYK-IDPTYP                           
573700                                     LOGG-IDPTYP                          
573800                                                                          
573900     MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                          
574000     MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                         
574100                                                                          
574200     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
574300       MOVE  WS-IDORDER         TO   RYK-IDORDER                          
574400     ELSE                                                                 
574500       MOVE  WS-IDDISTR-NUM     TO   W-WDQ2CSEQ-IDDISTR                   
574600       MOVE  WS-IDKUNDNR-NUM    TO   W-WDQ2CSEQ-IDKUNDNR                  
574700       MOVE  SPAR-ORAD-IDKUNDRF-RO(1:5)                                   
574800                                TO   W-WDQ2CSEQ-IDKUNDRF(3:7)             
574900       PERFORM IMS-GU-ORQI01-CSEQ                                         
575000       MOVE  OHUV-IDORDER       TO   RYK-IDORDER                          
575100     END-IF                                                               
575200                                                                          
575300     MOVE    SPAR-ORAD-IDARTNR  TO   RYK-IDARTNR                          
575400     MOVE    DAT-TIAAMMDD       TO   RYK-TIRODAT                          
575500     MOVE    SPAR-ORAD-KVLEVART TO   RYK-KVLEVART                         
575600     MOVE    SPAR-ORAD-KVBEART  TO   RYK-KVBEART-Q                        
575700     MOVE    SPAR-ORAD-KDORDKL  TO   RYK-KDORDKL                          
575800     MOVE    SPAR-ORAD-KDPRODSL TO   RYK-KDPRODSL                         
575900                                                                          
576000     MOVE    WS-KDORDBEK        TO   RYK-KDORDBEK                         
576100                                                                          
576200     MOVE    SPACE              TO   LOGG-SORTPOST                        
576300     MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                        
576400                                                                          
576500     PERFORM IMS-ISRT-AVVIKELSE                                           
576600                                                                          
576700     PERFORM UNTIL SEGMENT-FOUND                                          
576800                                                                          
576900       IF LOGG-IDLOGLOP = 9                                               
577000         MOVE ZERO              TO LOGG-IDLOGLOP                          
577100         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
577200       END-IF                                                             
577300       ADD +1                   TO LOGG-IDLOGLOP                          
577400       PERFORM IMS-ISRT-AVVIKELSE                                         
577500     END-PERFORM                                                          
577600     MOVE 'END S18E-SKAPA-RYK'  TO WS-CURRENT-SECTION                     
577700     .                                                                    
577800     SKIP2                                                                
577900 S19-GENERERA-KLAR-SV4        SECTION.                                    
578000     MOVE 'STA S19-GENERERA-KLAR-SV4' TO WS-CURRENT-SECTION               
578100                                                                          
578200     IF LOGG-IDLOGLOP = 9                                                 
578300       MOVE ZERO                TO   LOGG-IDLOGLOP                        
578400     END-IF                                                               
578500                                                                          
578600     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
578700     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
578800     ADD +1                               TO   LOGG-IDLOGLOP              
578900     MOVE 'RY6'                           TO   RY6-IDPTYP                 
579000                                               LOGG-IDPTYP                
579100     MOVE VORD-IDDISTR                    TO   RY6-IDDISTR                
579200     MOVE VORD-IDKUNDNR                   TO   RY6-IDKUNDNR               
579300     MOVE WS-IDKUNDRF                     TO   RY6-IDKUNDRF               
579400     MOVE VORD-IDDC                       TO   RY6-IDDC                   
579500     MOVE VORD-IDPRODNR                   TO   RY6-IDPRODNR               
579600     MOVE SPACE                           TO  LOGG-SORTPOST               
579700     MOVE RY6-WDGZRY6                     TO  LOGG-LOGGPOST               
579800*                                                                         
579900     PERFORM IMS-ISRT-KLAR-SV4                                            
580000*                                                                         
580100     PERFORM UNTIL SEGMENT-FOUND                                          
580200                                                                          
580300       IF LOGG-IDLOGLOP = 9                                               
580400         MOVE ZERO              TO LOGG-IDLOGLOP                          
580500         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
580600       END-IF                                                             
580700       ADD +1                   TO LOGG-IDLOGLOP                          
580800       PERFORM IMS-ISRT-KLAR-SV4                                          
580900     END-PERFORM                                                          
581000     .                                                                    
581100     EJECT                                                                
581200 S20-EV-LARM-2191-MID  SECTION.                                           
581300     MOVE 'S20-EV-LARM-2191-MID   '     TO WS-CURRENT-SECTION             
581400                                                                          
581500** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
581600     IF CLAG-KVROS = 0                                                    
581700       IF CLAG-KVAKS-CDC = 0                                              
581800         COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17               
581900         END-COMPUTE                                                      
582000         MOVE +1              TO 2191-MID-KDCLAGER                        
582100         MOVE ORAD-IDARTNR    TO 2191-MID-IDARTNR                         
582200         MOVE ZERO            TO 2191-MID-TISENBEK-DAG                    
582300                                 2191-MID-TISENBEK-KL                     
582400         MOVE SPACE           TO 2191-MID-IDKR                            
582500         MOVE CLAG-IDANSK     TO 2191-MID-IDANSK                          
582600         MOVE 210             TO 2191-MID-KDLARM                          
582700         MOVE WS-IDDISTR-NUM4 TO 2191-MID-IDDISTR                         
582800         MOVE WS-IDKUNDNR-NUM TO 2191-MID-IDKUNDNR                        
582900         MOVE WS-IDKUNDRF     TO 2191-MID-IDKUNDRF                        
583000         MOVE 'J'             TO 2191-MID-FLNYLARM                        
583100         MOVE WC-CDC-SE       TO 2191-MID-IDDC                            
583200         MOVE SPACE           TO 2191-MID-IDLEVNR                         
583300                                                                          
583400         PERFORM IMS-15-PURG-2191                                         
583500       END-IF                                                             
583600     END-IF                                                               
583700     .                                                                    
583800     EJECT                                                                
583900 S20-EV-LARM-2191-MID-CN-US SECTION.                                      
584000     MOVE 'S20-EV-LARM-2191-MID-CN-US'  TO WS-CURRENT-SECTION             
584100                                                                          
584200** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
584300     IF SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                        
584400       IF SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC                           
584500         COMPUTE 2191-LL = LENGTH OF 2191-MID-W2I19101 + 17               
584600         END-COMPUTE                                                      
584700         MOVE +1              TO 2191-MID-KDCLAGER                        
584800         MOVE ORAD-IDARTNR    TO 2191-MID-IDARTNR                         
584900         MOVE ZERO            TO 2191-MID-TISENBEK-DAG                    
585000                                 2191-MID-TISENBEK-KL                     
585100         MOVE SPACE           TO 2191-MID-IDKR                            
585200         MOVE XLAG-IDANSK     TO 2191-MID-IDANSK                          
585300         MOVE 210             TO 2191-MID-KDLARM                          
585400         MOVE WS-IDDISTR-NUM4 TO 2191-MID-IDDISTR                         
585500         MOVE WS-IDKUNDNR-NUM TO 2191-MID-IDKUNDNR                        
585600         MOVE WS-IDKUNDRF     TO 2191-MID-IDKUNDRF                        
585700         MOVE 'J'             TO 2191-MID-FLNYLARM                        
585800         MOVE SLAG-IDDC       TO 2191-MID-IDDC                            
585900         MOVE SLAG-IDLEVNR    TO 2191-MID-IDLEVNR                         
586000                                                                          
586100         PERFORM IMS-15-PURG-2191                                         
586200       END-IF                                                             
586300     END-IF                                                               
586400     .                                                                    
586500     EJECT                                                                
586600 S21-INIT-WS-FIELDS    SECTION.                                           
586700     MOVE 'S21-INIT-WS-FIELDS     '     TO WS-CURRENT-SECTION             
586800                                                                          
586900     MOVE ZERO                  TO INX-TOT-ANT-RADER                      
587000                                   SPAR-VKORDNTO                          
587100                                   SPAR-VKORDNTO-TOT                      
587200                                   SPAR-VKORDNTO-DEL                      
587300                                   SPAR-VLORDNTO                          
587400                                   SPAR-VLORDNTO-TOT                      
587500                                   SPAR-VLORDNTO-DEL                      
587600                                   SPAR-KVKOLLI                           
587700                                   SPAR-KVKOLPAC                          
587800                                   SPAR-KVKOLLI-FAKT                      
587900                                   SPAR-SUORDV                            
588000                                   SPAR-SUORDV-LOC                        
588100                                   SPAR-SUORDV-LOCPREL                    
588200                                   SPAR-SUORDV-TOT                        
588300                                   SPAR-SUORDV-TOT-LOC                    
588400                                   SPAR-SUORDV-TOT-LOCPREL                
588500                                   SPAR-SUORDV-DEL                        
588600                                   SPAR-SUORDV-DEL-LOC                    
588700                                   SPAR-SUORDV-DEL-LOCPREL                
588800                                   SPAR-IDPRODNR                          
588900                                   LOGG-IDLOGLOP                          
589000                                   SPAR-PRAD-KVFLAMP                      
589100                                   SPAR-PRAD-KDFARLIG                     
589200                                   SPAR-PRAD-PRARTNTO                     
589300                                   SPAR-PRAD-PRARTNTO-LOC                 
589400                                   SPAR-PRAD-PRARTNTO-LOCPREL             
589500                                   SPAR-PRAD-VKARTNTO                     
589600                                   SPAR-PRAD-KVLEVART                     
589700                                   SPAR-IDPSN                             
589800                                   SPAR-VKART-FG                          
589900                                   SPAR-VLFG                              
590000                                   SPAR-SUEQFG                            
590100                                   TOTAL-SUEQFG                           
590200                                   WS-ANT-RADER-ORDER                     
590300                                   WS-RINT-ANT-FPACK-ORAD                 
590400                                   WS-KDORDKL                             
590500                                   WS-IDORDER                             
590600                                   WS-KDRAPRIO                            
590700                                   WS-KDORDBEK                            
590800                                   WS-SUMMA                               
590900                                   WS-EMBPROF                             
591000                                   WS-KDFAKTYP                            
591100                                   WS-FLLSBOK                             
591200                                   WS-FLORDSPE                            
591300                                   WS-FLOVRLEV                            
591400                                   WS-IDPURAD                             
591500                                   WS-START-RAD                           
591600                                   WS-SISTA-RAD                           
591700                                   WS-KDTPOTYP                            
591800                                   WS-KDFRAKT                             
591900                                   WS-VKORDBTO                            
592000                                   WS-VLORDBTO                            
592100                                   WS-KDEMBTYP                            
592200                                   WS-ADFLOMR                             
592300                                   WS-ADRUTNIV                            
592400                                   WS-KVSLATTAT                           
592500                                   SPAR-KART-KVRESS-ART                   
592600                                   WS-KVPTID-MIN                          
592700                                   WS-KVPTID-TIM                          
592800                                   ANTAL-EJ-PACKRAP-ORDDEL                
592900                                   WS-DARFS                               
593000                                   WS-KDORDSTA                            
593100                                   WS-KVORDRAD-PACK                       
593200                                   WS-KVORDRAD                            
593300                                   WS-KVORAPP-TOTAL                       
593400                                   WS-KVORAPP-PACK                        
593500                                   WS-KVPRERO                             
593600                                   WS-AVVIKELSE-UTSKR                     
593700                                   WS-TIDISPIN                            
593800                                   WS-TTMMSS                              
593900                                   WS-HH                                  
594000                                   WS-IDORDNR5-OLD                        
594100                                   WS-IDORDNR7-NEW                        
594200                                   WS-SUPTID-TIM                          
594300                                   WS-SUPTID-MIN                          
594400                                   WS-HHMMSS                              
594500                                   WS-DD                                  
594600                                   ARB-KOLLI-VKORDNTO                     
594700                                   ARB-KOLLI-KVFLAMP                      
594800                                   ARB-KOLLI-KDFARLIG                     
594900                                   ARB-KOLLI-KVORDRAD                     
595000                                   ARB-KOLLI-KVFALRAD                     
595100                                   ARB-KOLLI-SUORDV                       
595200                                   ARB-KOLLI-SUORDV-LOC                   
595300                                   ARB-KOLLI-SUORDV-LOCPREL               
595400                                   ARB-ANTAL-KOLLI-PLUS-1                 
595500                                   ARB-ANTAL-KOLLI                        
595600                                                                          
595700     MOVE SPACE                 TO WS-IDORDNR                             
595800                                   WS-ALLA-ODEL-UTSKRIVNA                 
595900                                   WS-SAMMANSLAGNING-RAD                  
596000                                   SW-TIRODAT-LIKA-MED-ZERO               
596100                                   WS-ADFLGEO                             
596200                                   WS-TRAEFF-PACKARE                      
596300                                   WS-FLAUTFAK                            
596400                                   WS-INDATA-TEST                         
596500                                   WS-BEHANDLING-TEST                     
596600                                   SPAR-BEKUNDRF                          
596700                                   WS-IDORDNR                             
596800     MOVE NEJ                   TO FL-420-SEGMENT                         
596900     .                                                                    
597000     EJECT                                                                
597100 S22-SKAPA-SALDOLOGG SECTION.                                             
597200     MOVE 'S22-SKAPA-SALDOLOGG    '     TO WS-CURRENT-SECTION             
597300                                                                          
597400     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
597500     MOVE 9                         TO LOGG-IDSEKVNR                      
597600     MOVE REQU-IDDC-KEY             TO LOGG-IDDC                          
597700     MOVE 'OUTB'                    TO LOGG-IDHUVTYP                      
597800     MOVE 'PAC'                     TO LOGG-IDSUBTYP                      
597900     MOVE 'WL019700'                TO LOGG-IDPGM                         
598000     MOVE SAVE-IDTRANS              TO LOGG-IDTRANS                       
598100     MOVE WS-ODEL-IDUSER            TO LOGG-IDUSER                        
598200     MOVE SPACE                     TO LOGG-REF                           
598300     MOVE WS-KORD-IDDISTR           TO LOGG-IDDISTR                       
598400     MOVE WS-IDPRODNR               TO LOGG-IDPRODNR                      
598500     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
598600     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
598700     MOVE '-'                       TO LOGG-IDTECKEN-KVEFRS               
598800     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
598900     MOVE WS-KVORAPP-PACK           TO LOGG-KVART-SALDO                   
599000     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                                
599100                          CLAG-KVAKS-T                                    
599200     END-COMPUTE                                                          
599300     MOVE CLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
599400     MOVE CLAG-KVEFRS               TO LOGG-KVEFRS                        
599500     MOVE CLAG-KVLS                 TO LOGG-KVLS                          
599600     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
599700     MOVE FUNCTION CURRENT-DATE(1:8) TO W-LOGG-DATUM                      
599800     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - W-LOGG-DATUM              
599900     END-COMPUTE                                                          
600000     ACCEPT W-LOGG-TID FROM TIME                                          
600100     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - W-LOGG-TID                 
600200     END-COMPUTE                                                          
600300                                                                          
600400     PERFORM IMS-ISRT-WDL9                                                
600500     IF SEGMENT-FOUND-EXISTS                                              
600600       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
600700          ADD -1 TO LOGG-IDSEKVNR                                         
600800          PERFORM IMS-ISRT-WDL9                                           
600900       END-PERFORM                                                        
601000     END-IF                                                               
601100     .                                                                    
601200     EJECT                                                                
601300                                                                          
601400 S23-SKAPA-SALDOLOGG SECTION.                                             
601500     MOVE 'S23-SKAPA-SALDOLOGG    '     TO WS-CURRENT-SECTION             
601600                                                                          
601700     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
601800     MOVE 9                         TO LOGG-IDSEKVNR                      
601900     MOVE REQU-IDDC-KEY             TO LOGG-IDDC                          
602000     MOVE 'OUTB'                    TO LOGG-IDHUVTYP                      
602100     MOVE 'PAC'                     TO LOGG-IDSUBTYP                      
602200     MOVE 'WL019700'                TO LOGG-IDPGM                         
602300     MOVE SAVE-IDTRANS              TO LOGG-IDTRANS                       
602400     MOVE WS-ODEL-IDUSER            TO LOGG-IDUSER                        
602500     MOVE SPACE                     TO LOGG-REF                           
602600     MOVE WS-KORD-IDDISTR           TO LOGG-IDDISTR                       
602700     MOVE WS-IDPRODNR               TO LOGG-IDPRODNR                      
602800     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
602900     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
603000     MOVE '-'                       TO LOGG-IDTECKEN-KVEFRS               
603100     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
603200     MOVE WS-KVORAPP-PACK           TO LOGG-KVART-SALDO                   
603300     MOVE SLAG-KVAKS-SDC            TO LOGG-KVAKS                         
603400     MOVE SLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
603500     MOVE SLAG-KVEFRS               TO LOGG-KVEFRS                        
603600     MOVE SLAG-KVLS                 TO LOGG-KVLS                          
603700     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
603800     MOVE FUNCTION CURRENT-DATE(1:8) TO W-LOGG-DATUM                      
603900     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - W-LOGG-DATUM              
604000     END-COMPUTE                                                          
604100     ACCEPT W-LOGG-TID FROM TIME                                          
604200     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - W-LOGG-TID                 
604300     END-COMPUTE                                                          
604400     PERFORM IMS-ISRT-WDL9                                                
604500     IF SEGMENT-FOUND-EXISTS                                              
604600       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
604700          ADD -1 TO LOGG-IDSEKVNR                                         
604800          PERFORM IMS-ISRT-WDL9                                           
604900       END-PERFORM                                                        
605000     END-IF                                                               
605100     .                                                                    
605200     EJECT                                                                
605300                                                                          
605400                                                                          
605500 S25-DELETE-PRICE-Q-LINE SECTION.                                         
605600     MOVE 'S25-DELETE-PRICE-Q-LINE'     TO WS-CURRENT-SECTION             
605700                                                                          
605800     IF DIST79-DEALER-PRICE                                               
605900       IF SPAR-ORAD-IDPRQUES > ZERO                                       
606000         INITIALIZE PRQU-W335PRQU                                         
606100         MOVE WS-IDDISTR-NUM          TO PRQU-IDDISTR                     
606200         MOVE WS-IDKUNDNR-NUM         TO PRQU-IDKUNDNR                    
606300         MOVE WS-IDKUNDRF-NEW         TO PRQU-IDKUNDRF                    
606400         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
606500         MOVE 4                       TO PRQU-KDCALL                      
606600         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
606700                                            PRQU-WDC7-PCB                 
606800                                            PRQU-SJKO-WDK6-PCB            
606900                                                                          
607000       END-IF                                                             
607100     END-IF                                                               
607200     .                                                                    
607300     EJECT                                                                
607400 S36-ANDRA-WDC711 SECTION.                                                
607500     MOVE 'S36-ANDRA-WDC711 '           TO WS-CURRENT-SECTION             
607600                                                                          
607700     IF DIST79-DEALER-PRICE AND WS-IDDISTR-NUM = 0778                     
607800       AND RAD-KDORDKL < 3                                                
607900*        FLYTTA PRARTNTO-LOC TILL PRARTNTO-LOCPREL OSV                    
608000*        PÅ WDA5                                                          
608100       IF SPAR-ORAD-IDPRQUES > ZERO                                       
608200         INITIALIZE PRQU-W335PRQU                                         
608300         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
608400         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
608500         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
608600         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
608700         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
608800         MOVE 6                       TO PRQU-KDCALL                      
608900         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
609000                                            PRQU-WDC7-PCB                 
609100                                            PRQU-SJKO-WDK6-PCB            
609200         MOVE 'N'                     TO RAD-FLPRTILL                     
609300         IF RAD-PRARTNTO-LOC > +0                                         
609400           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
609500           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
609600         END-IF                                                           
609700         IF PRQU-KDCALL = -1                                              
609800           PERFORM S36-NY-FRAGA                                           
609900         END-IF                                                           
610000       ELSE                                                               
610100*        SKAPA NY PRISFRÅGA                                               
610200         PERFORM S36-NY-FRAGA                                             
610300       END-IF                                                             
610400     END-IF                                                               
610500     .                                                                    
610600     EJECT                                                                
610700 S36-NY-FRAGA  SECTION.                                                   
610800     MOVE 'S36-NY-FRAGA     '           TO WS-CURRENT-SECTION             
610900                                                                          
611000     MOVE ZERO                     TO WS-IDPRQUES                         
611100     IF WS-IDPRQUES                = +0                                   
611200        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
611300        MOVE +1                    TO PRNO-KDCALL                         
611400                                                                          
611500        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
611600                                                                          
611700        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
611800                                      WS-IDPRQUES                         
611900        MOVE +1                    TO PRQU-KDCALL                         
612000     END-IF                                                               
612100     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
612200     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
612300     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
612400     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
612500     MOVE ZERO                     TO PRQU-IDORDER                        
612600     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
612700     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
612800       AND RAD-DARODAT > 0                                                
612900       MOVE 4                      TO PRQU-KDORDKL                        
613000     END-IF                                                               
613100     MOVE 'Q'                      TO PRQU-KDPRSTA                        
613200     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
613300     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
613400     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
613500     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
613600     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
613700     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
613800*    PERFORM IMS-GU-GMTA-WDB201                                           
613900                                                                          
614000     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
614100                                         PRQU-WDC7-PCB                    
614200                                         PRQU-SJKO-WDK6-PCB               
614300                                                                          
614400     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
614500                                       WS-IDPRQUES                        
614600     MOVE 'N'                      TO  RAD-FLPRTILL                       
614700                                                                          
614800     IF RAD-PRARTNTO-LOC = +0                                             
614900        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
615000                       RAD-PRARTNTO-LOCPREL                               
615100     ELSE                                                                 
615200        MOVE RAD-PRARTNTO-LOC  TO RAD-PRARTNTO-LOCPREL                    
615300        MOVE ZERO              TO RAD-PRARTNTO-LOC                        
615400     END-IF                                                               
615500                                                                          
615600     MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                     
615700     MOVE +3                      TO PRNO-KDCALL                          
615800                                                                          
615900     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
616000     .                                                                    
616100     EJECT                                                                
616200*IMS SEKTIONER                                                            
616300*                                                                         
616400*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
616500*                 III     III MM MMMMM MM SSSS   SSSS                     
616600*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
616700*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
616800*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
616900*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
617000*                 III     III MM MMMMM MM SSSS   SSSS                     
617100*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
617200*                                                                         
617300*                                                                         
617400* --- IMS SECTIONS ---                                                    
617500     SKIP3                                                                
617600                                                                          
617700 IMS-01-GU-WDK501 SECTION.                                                
617800     MOVE 'IMS-01-GU-WDK501'     TO WS-CURRENT-IMS-SECTION                
617900                                                                          
618000     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-X ')'                         
618100          DELIMITED BY SIZE INTO SSA1                                     
618200     MOVE '  GE' TO GOOD-STATUSCODES                                      
618300     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
618400     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
618500     PERFORM IMS-STATUSCHECK                                              
618600     .                                                                    
618700     EJECT                                                                
618800 IMS-02-GU-WDE601 SECTION.                                                
618900     MOVE 'IMS-02-GU-WDE601'     TO WS-CURRENT-IMS-SECTION                
619000                                                                          
619100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
619200          DELIMITED BY SIZE INTO SSA1                                     
619300     MOVE '  GE' TO GOOD-STATUSCODES                                      
619400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
619500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
619600     PERFORM IMS-STATUSCHECK                                              
619700     .                                                                    
619800     EJECT                                                                
619900 IMS-03-GNP-WDE611 SECTION.                                               
620000     MOVE 'IMS-03-GNP-WDE611'    TO WS-CURRENT-IMS-SECTION                
620100                                                                          
620200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
620300          DELIMITED BY SIZE INTO SSA1                                     
620400     MOVE '  GE' TO GOOD-STATUSCODES                                      
620500     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
620600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
620700     PERFORM IMS-STATUSCHECK                                              
620800     .                                                                    
620900     EJECT                                                                
621000 IMS-04-GNP-WDE611 SECTION.                                               
621100     MOVE 'IMS-04-GNP-WDE611'    TO WS-CURRENT-IMS-SECTION                
621200                                                                          
621300     STRING 'WDE611  (IDKOLLI =>' W-IDKOLLI-MIN-X                         
621400                    '&IDKOLLI =<' W-IDKOLLI-MAX-X ')'                     
621500          DELIMITED BY SIZE INTO SSA1                                     
621600     MOVE '  GE' TO GOOD-STATUSCODES                                      
621700     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
621800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
621900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
622000     PERFORM IMS-STATUSCHECK                                              
622100     .                                                                    
622200     EJECT                                                                
622300 IMS-GU-WDE611 SECTION.                                                   
622400                                                                          
622500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-WDE601-X ')'                 
622600          DELIMITED BY SIZE INTO SSA1                                     
622700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-WDE611-X ')'                  
622800          DELIMITED BY SIZE INTO SSA2                                     
622900     MOVE '  GE' TO GOOD-STATUSCODES                                      
623000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
623100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
623200     PERFORM IMS-STATUSCHECK                                              
623300     .                                                                    
623400     EJECT                                                                
623500*IMS-04-GU-WDE401 SECTION.                                                
623600*    MOVE 'IMS-04-GU-WDE401'     TO WS-CURRENT-IMS-SECTION                
623700*                                                                         
623800*    STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
623900*         DELIMITED BY SIZE INTO SSA1                                     
624000*    MOVE '  GE' TO GOOD-STATUSCODES                                      
624100*    CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
624200*    MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
624300*    PERFORM IMS-STATUSCHECK                                              
624400*    .                                                                    
624500*    EJECT                                                                
624600*IMS-05-GNP-WDE411 SECTION.                                               
624700*    MOVE 'IMS-05-GNP-WDE411'    TO WS-CURRENT-IMS-SECTION                
624800*                                                                         
624900*    STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
625000*         DELIMITED BY SIZE INTO SSA1                                     
625100*    MOVE '  GE' TO GOOD-STATUSCODES                                      
625200*    CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
625300*    MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
625400*    PERFORM IMS-STATUSCHECK                                              
625500*    .                                                                    
625600*    EJECT                                                                
625700*IMS-06-GNP-WDE421 SECTION.                                               
625800*    MOVE 'IMS-06-GNP-WDE421'    TO WS-CURRENT-IMS-SECTION                
625900*                                                                         
626000*    STRING 'WDE421  (IDPRODNR =' W-IDPRODNR-X ')'                        
626100*         DELIMITED BY SIZE INTO SSA1                                     
626200*    MOVE '  GE' TO GOOD-STATUSCODES                                      
626300*    CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1                   
626400*    MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
626500*    PERFORM IMS-STATUSCHECK                                              
626600*    .                                                                    
626700*    EJECT                                                                
626800*IMS-07-GU-WDQ301 SECTION.                                                
626900*    MOVE 'IMS-07-GU-WDQ301'     TO WS-CURRENT-IMS-SECTION                
627000*                                                                         
627100*    STRING 'WDQ301  (WDQ301KY =' W-WDQ301-KEY-X ')'                      
627200*         DELIMITED BY SIZE INTO SSA1                                     
627300*    MOVE '  GE' TO GOOD-STATUSCODES                                      
627400*    CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
627500*    MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
627600*    PERFORM IMS-STATUSCHECK                                              
627700*    .                                                                    
627800*    EJECT                                                                
627900 IMS-08-GN-WDE6C1 SECTION.                                                
628000     MOVE 'IMS-08-GN-WDE6C1'     TO WS-CURRENT-IMS-SECTION                
628100                                                                          
628200     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
628300                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X ')'                    
628400          DELIMITED BY SIZE INTO SSA1                                     
628500     MOVE '  GE' TO GOOD-STATUSCODES                                      
628600     CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-WDE6C1 SSA1                   
628700     MOVE WDE6C-STATUS-CODE TO STATUS-WS STATUS-WS                        
628800     PERFORM IMS-STATUSCHECK                                              
628900     .                                                                    
629000     EJECT                                                                
629100 IMS-07-GU-WDQ3D1 SECTION.                                                
629200     MOVE 'IMS-07-GU-WDQ3D1'     TO WS-CURRENT-IMS-SECTION                
629300                                                                          
629400     STRING 'WDQ3D1  (WDQ3D1KY>=' W-WDQ3D1KY-MIN-X                        
629500                    '&WDQ3D1KY<=' W-WDQ3D1KY-MAX-X ')'                    
629600          DELIMITED BY SIZE INTO SSA1                                     
629700     MOVE '  GE' TO GOOD-STATUSCODES                                      
629800     CALL CBLTDLI USING GU WDQ3D-PCB DLI-IO-WDQ3D1 SSA1                   
629900     MOVE WDQ3D-STATUS-CODE TO STATUS-WS                                  
630000     MOVE WDQ3D-STATUS-CODE TO Q3D1-STATUS-WS                             
630100     PERFORM IMS-STATUSCHECK                                              
630200     .                                                                    
630300     EJECT                                                                
630400 IMS-09-GU-WDQ3D1 SECTION.                                                
630500     MOVE 'IMS-09-GU-WDQ3D1'     TO WS-CURRENT-IMS-SECTION                
630600                                                                          
630700     STRING 'WDQ3D1  (WDQ3D1KY>=' W-WDQ3D1KY-MIN-X                        
630800                    '&WDQ3D1KY<=' W-WDQ3D1KY-MAX-X ')'                    
630900          DELIMITED BY SIZE INTO SSA1                                     
631000     MOVE '    ' TO GOOD-STATUSCODES                                      
631100     CALL CBLTDLI USING GU WDQ3D-PCB DLI-IO-WDQ3D1 SSA1                   
631200     MOVE WDQ3D-STATUS-CODE TO STATUS-WS                                  
631300     PERFORM IMS-STATUSCHECK                                              
631400     .                                                                    
631500     EJECT                                                                
631600 IMS-10-GU-WDQ301 SECTION.                                                
631700     MOVE 'IMS-10-GU-WDQ301'     TO WS-CURRENT-IMS-SECTION                
631800                                                                          
631900     STRING 'WDQ301  (WDQ3HSEQ =' W-WDQ3HSEQ-X ')'                        
632000          DELIMITED BY SIZE INTO SSA1                                     
632100     MOVE '  GE' TO GOOD-STATUSCODES                                      
632200     CALL CBLTDLI USING GU WDQ3H-PCB DLI-IO-WDQ301 SSA1                   
632300     MOVE WDQ3H-STATUS-CODE TO STATUS-WS                                  
632400     PERFORM IMS-STATUSCHECK                                              
632500     .                                                                    
632600     EJECT                                                                
632700 IMS-11-GHU-WDE611 SECTION.                                               
632800     MOVE 'IMS-11-GHU-WDE611'    TO WS-CURRENT-IMS-SECTION                
632900                                                                          
633000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
633100          DELIMITED BY SIZE INTO SSA1                                     
633200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
633300          DELIMITED BY SIZE INTO SSA2                                     
633400     MOVE '  GE' TO GOOD-STATUSCODES                                      
633500     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
633600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
633700     PERFORM IMS-STATUSCHECK                                              
633800     .                                                                    
633900     EJECT                                                                
634000 IMS-12-REPL-WDE611 SECTION.                                              
634100     MOVE 'IMS-12-REPL-WDE611'    TO WS-CURRENT-IMS-SECTION               
634200      ADD +1                      TO REBOOT-PROGRAM-IX                    
634300                                                                          
634400     MOVE '    ' TO GOOD-STATUSCODES                                      
634500     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
634600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
634700     PERFORM IMS-STATUSCHECK                                              
634800     .                                                                    
634900     EJECT                                                                
635000 IMS-13-GU-WDE6C1 SECTION.                                                
635100     MOVE 'IMS-13-GU-WDE6C1'     TO WS-CURRENT-IMS-SECTION                
635200                                                                          
635300     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
635400                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X ')'                    
635500          DELIMITED BY SIZE INTO SSA1                                     
635600     MOVE '  GE' TO GOOD-STATUSCODES                                      
635700     CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-WDE6C1 SSA1                   
635800     MOVE WDE6C-STATUS-CODE TO STATUS-WS STATUS-WS                        
635900     PERFORM IMS-STATUSCHECK                                              
636000     .                                                                    
636100     EJECT                                                                
636200 IMS-14-GN-WDQ301 SECTION.                                                
636300     MOVE 'IMS-14-GN-WDQ301'     TO WS-CURRENT-IMS-SECTION                
636400                                                                          
636500     STRING 'WDQ301  (WDQ3HSEQ =' W-WDQ3HSEQ-X ')'                        
636600          DELIMITED BY SIZE INTO SSA1                                     
636700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
636800     CALL CBLTDLI USING GN WDQ3H-PCB DLI-IO-WDQ301 SSA1                   
636900     MOVE WDQ3H-STATUS-CODE TO STATUS-WS                                  
637000     PERFORM IMS-STATUSCHECK                                              
637100     .                                                                    
637200     EJECT                                                                
637300*FROM W403AVSP                                                            
637400 IMS-15-PURG-2191           SECTION.                                      
637500     MOVE 'IMS-15-PURG-2191'     TO WS-CURRENT-IMS-SECTION                
637600                                                                          
637700     MOVE LOW-VALUE TO 2191-Z1 2191-Z2                                    
637800     MOVE '  '  TO GOOD-STATUSCODES                                       
637900     CALL CBLTDLI USING PURG 2191-PCB 2191-IO-AREA                        
638000     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
638100     PERFORM IMS-STATUSCHECK                                              
638200     .                                                                    
638300     SKIP2                                                                
638400 IMS-PURG-4349 SECTION.                                                   
638500                                                                          
638600     MOVE LOW-VALUE            TO 4349-Z1                                 
638700                                  4349-Z2                                 
638800     MOVE '  '                 TO GOOD-STATUSCODES                        
638900     CALL CBLTDLI USING PURG 4349-PCB 4349-MSG-IO-AREA                    
639000     MOVE 4349-STATUS-CODE   TO STATUS-WS                                 
639100     PERFORM IMS-STATUSCHECK                                              
639200     .                                                                    
639300     EJECT                                                                
639400 IMS-16-REPL-WDE401    SECTION.                                           
639500     MOVE 'IMS-16-REPL-WDE401'   TO WS-CURRENT-IMS-SECTION                
639600      ADD +2                      TO REBOOT-PROGRAM-IX                    
639700                                                                          
639800     MOVE '  '   TO GOOD-STATUSCODES                                      
639900     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
640000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
640100     PERFORM IMS-STATUSCHECK                                              
640200     SKIP2                                                                
640300     .                                                                    
640400 IMS-17-GU-WDE401              SECTION.                                   
640500     MOVE 'IMS-17-GU-WDE401  '   TO WS-CURRENT-IMS-SECTION                
640600                                                                          
640700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
640800          DELIMITED BY SIZE INTO SSA1                                     
640900     MOVE '    ' TO GOOD-STATUSCODES                                      
641000     CALL CBLTDLI USING GU    WDE4-PCB DLI-IO-WDE401 SSA1                 
641100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
641200     PERFORM IMS-STATUSCHECK                                              
641300     SKIP3                                                                
641400     .                                                                    
641500 IMS-GNP-WDE421-OKVAL SECTION.                                            
641600     MOVE 'IMS-GNP-WDE421-OKVAL'    TO WS-CURRENT-IMS-SECTION             
641700                                                                          
641800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
641900            DELIMITED BY SIZE INTO SSA1                                   
642000     MOVE 'WDE421'            TO SSA2                                     
642100     MOVE '  GE' TO GOOD-STATUSCODES                                      
642200     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2              
642300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
642400     PERFORM IMS-STATUSCHECK                                              
642500     .                                                                    
642600     EJECT                                                                
642700 IMS-18-GHU-WDE401             SECTION.                                   
642800     MOVE 'IMS-18-GHU-WDE401 '   TO WS-CURRENT-IMS-SECTION                
642900                                                                          
643000     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
643100          DELIMITED BY SIZE INTO SSA1                                     
643200     MOVE '    ' TO GOOD-STATUSCODES                                      
643300     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-WDE401 SSA1                
643400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
643500     PERFORM IMS-STATUSCHECK                                              
643600     SKIP3                                                                
643700     .                                                                    
643800 IMS-19-GHNP-WDE411            SECTION.                                   
643900     MOVE 'IMS-19-GHNP-WDE411'   TO WS-CURRENT-IMS-SECTION                
644000                                                                          
644100     MOVE 'WDE411 '            TO SSA1                                    
644200     MOVE '  GE' TO GOOD-STATUSCODES                                      
644300     CALL CBLTDLI USING GHNP   WDE4-PCB DLI-IO-WDE411 SSA1                
644400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
644500     PERFORM IMS-STATUSCHECK                                              
644600     .                                                                    
644700 IMS-19-GNP-WDE411            SECTION.                                    
644800     MOVE 'IMS-19-GHNP-WDE411'   TO WS-CURRENT-IMS-SECTION                
644900                                                                          
645000     MOVE 'WDE411 '            TO SSA1                                    
645100     MOVE '  GE' TO GOOD-STATUSCODES                                      
645200     CALL CBLTDLI USING GNP   WDE4-PCB DLI-IO-WDE411 SSA1                 
645300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
645400     PERFORM IMS-STATUSCHECK                                              
645500     .                                                                    
645600 IMS-20-GU-WDE401-SEQ  SECTION.                                           
645700     MOVE 'IMS-20-GU-WDE401-SEQ' TO WS-CURRENT-IMS-SECTION                
645800                                                                          
645900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
646000            DELIMITED BY SIZE INTO SSA1                                   
646100     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
646200     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-WDE401 SSA1                   
646300     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
646400     PERFORM IMS-STATUSCHECK                                              
646500     .                                                                    
646600     SKIP3                                                                
646700 IMS-21-GN-WDE401-SEQ  SECTION.                                           
646800     MOVE 'IMS-21-GN-WDE401-SEQ' TO WS-CURRENT-IMS-SECTION                
646900                                                                          
647000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
647100            DELIMITED BY SIZE INTO SSA1                                   
647200     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
647300     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE401 SSA1                   
647400     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
647500     PERFORM IMS-STATUSCHECK                                              
647600     .                                                                    
647700 IMS-22-REPL-WDE411 SECTION.                                              
647800     MOVE 'IMS-22-REPL-WDE411' TO WS-CURRENT-IMS-SECTION                  
647900      ADD +2                     TO REBOOT-PROGRAM-IX                     
648000                                                                          
648100     MOVE '    ' TO GOOD-STATUSCODES                                      
648200     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE411                       
648300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
648400     PERFORM IMS-STATUSCHECK                                              
648500     SKIP3                                                                
648600     .                                                                    
648700 IMS-23-GHNP-KOLLI-KOPPL    SECTION.                                      
648800     MOVE 'IMS-23-GHNP-KOLLI-KOPPL' TO WS-CURRENT-IMS-SECTION             
648900                                                                          
649000     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
649100            DELIMITED BY SIZE INTO SSA1                                   
649200     MOVE '  ' TO GOOD-STATUSCODES                                        
649300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1                  
649400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
649500     PERFORM IMS-STATUSCHECK                                              
649600     SKIP3                                                                
649700     .                                                                    
649800 IMS-24-REPL-KOLLI-KOPPL  SECTION.                                        
649900     MOVE 'IMS-24-REPL-KOLLI-KOPPL' TO WS-CURRENT-IMS-SECTION             
650000      ADD +1                     TO REBOOT-PROGRAM-IX                     
650100                                                                          
650200     MOVE '  '   TO GOOD-STATUSCODES                                      
650300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE421                       
650400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
650500     PERFORM IMS-STATUSCHECK                                              
650600     SKIP2                                                                
650700     .                                                                    
650800 IMS-25-ISRT-KOLLI-KOPPL  SECTION.                                        
650900     MOVE 'IMS-25-ISRT-KOLLI-KOPPL' TO WS-CURRENT-IMS-SECTION             
651000      ADD +1                     TO REBOOT-PROGRAM-IX                     
651100                                                                          
651200     MOVE   'WDE421 '         TO   SSA1                                   
651300     MOVE '  II' TO GOOD-STATUSCODES                                      
651400     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1                  
651500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
651600     PERFORM IMS-STATUSCHECK                                              
651700     .                                                                    
651800     EJECT                                                                
651900 IMS-26-GN-WDE401-ESEQ SECTION.                                           
652000     MOVE 'IMS-26-GN-WDE401-ESEQ'   TO WS-CURRENT-IMS-SECTION             
652100                                                                          
652200     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
652300            DELIMITED BY SIZE INTO SSA1                                   
652400     MOVE '  GE' TO GOOD-STATUSCODES                                      
652500     CALL CBLTDLI USING GN  WDE4E-PCB DLI-IO-WDE401 SSA1                  
652600     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
652700     PERFORM IMS-STATUSCHECK                                              
652800     SKIP2                                                                
652900     .                                                                    
653000 IMS-27-GHU-WDE601           SECTION.                                     
653100     MOVE 'IMS-27-GHU-WDE601 '   TO WS-CURRENT-IMS-SECTION                
653200                                                                          
653300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
653400            DELIMITED BY SIZE INTO SSA1                                   
653500     MOVE '    ' TO GOOD-STATUSCODES                                      
653600     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-WDE601 SSA1                
653700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
653800     PERFORM IMS-STATUSCHECK                                              
653900     .                                                                    
654000     EJECT                                                                
654100 IMS-28-REPL-KOLLIREG SECTION.                                            
654200     MOVE 'IMS-28-REPL-KOLLIREG' TO WS-CURRENT-IMS-SECTION                
654300      ADD +1                     TO REBOOT-PROGRAM-IX                     
654400                                                                          
654500     MOVE '    ' TO GOOD-STATUSCODES                                      
654600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
654700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
654800     PERFORM IMS-STATUSCHECK                                              
654900     SKIP3                                                                
655000     .                                                                    
655100 IMS-29-GHU-KOLLI    SECTION.                                             
655200     MOVE 'IMS-29-GHU-KOLLI'    TO WS-CURRENT-IMS-SECTION                 
655300                                                                          
655400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
655500            DELIMITED BY SIZE INTO SSA1                                   
655600     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
655700            DELIMITED BY SIZE INTO SSA2                                   
655800     MOVE '  GE' TO GOOD-STATUSCODES                                      
655900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-WDE611 SSA1 SSA2           
656000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
656100     PERFORM IMS-STATUSCHECK                                              
656200     SKIP3                                                                
656300     .                                                                    
656400 IMS-REPL-KOLLI    SECTION.                                               
656500     MOVE 'IMS-REPL-KOLLI '    TO WS-CURRENT-IMS-SECTION                  
656600      ADD +1                     TO REBOOT-PROGRAM-IX                     
656700                                                                          
656800     MOVE '    ' TO GOOD-STATUSCODES                                      
656900     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
657000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
657100     PERFORM IMS-STATUSCHECK                                              
657200     SKIP3                                                                
657300     .                                                                    
657400 IMS-GHU-KUNDORDER SECTION.                                               
657500     MOVE 'IMS-GHU-KUNDORDER'  TO WS-CURRENT-IMS-SECTION                  
657600                                                                          
657700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
657800            DELIMITED BY SIZE INTO SSA1                                   
657900     MOVE '    ' TO GOOD-STATUSCODES                                      
658000     CALL CBLTDLI USING GHU    WDE41-PCB KORD-WDE401 SSA1                 
658100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
658200     PERFORM IMS-STATUSCHECK                                              
658300     .                                                                    
658400     EJECT                                                                
658500 IMS-GU-4726-ROT-KVAL SECTION.                                            
658600     MOVE 'IMS-GU-4726-ROT-KVAL'   TO WS-CURRENT-IMS-SECTION              
658700                                                                          
658800     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
658900            DELIMITED BY SIZE INTO SSA1                                   
659000     MOVE '  ' TO GOOD-STATUSCODES                                        
659100     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-AREA3 SSA1                 
659200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
659300     PERFORM IMS-STATUSCHECK                                              
659400     SKIP2                                                                
659500     .                                                                    
659600 IMS-GNP-4726-UNDERSEG-KVAL SECTION.                                      
659700     MOVE 'IMS-GNP-4726-UNDERSEG-KVAL' TO  WS-CURRENT-IMS-SECTION         
659800                                                                          
659900     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
660000            DELIMITED BY SIZE INTO SSA1                                   
660100     MOVE '  GE' TO GOOD-STATUSCODES                                      
660200     CALL CBLTDLI USING GNP    XXDV-PCB DLI-IO-AREA3 SSA1                 
660300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
660400     PERFORM IMS-STATUSCHECK                                              
660500     SKIP2                                                                
660600     .                                                                    
660700 IMS-INSERT-4726-UNDERSEG SECTION.                                        
660800     MOVE 'IMS-INSERT-4726-UNDERSEG'   TO  WS-CURRENT-IMS-SECTION         
660900      ADD +1                     TO REBOOT-PROGRAM-IX                     
661000                                                                          
661100     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
661200            DELIMITED BY SIZE INTO SSA1                                   
661300     MOVE 'WLXXDV11 ' TO SSA2                                             
661400     MOVE '  ' TO GOOD-STATUSCODES                                        
661500     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3 SSA1 SSA2              
661600     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
661700     PERFORM IMS-STATUSCHECK                                              
661800     SKIP2                                                                
661900     .                                                                    
662000 IMS-INSERT-4727 SECTION.                                                 
662100     MOVE 'IMS-INSERT-4727'            TO  WS-CURRENT-IMS-SECTION         
662200      ADD +1                     TO REBOOT-PROGRAM-IX                     
662300                                                                          
662400     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
662500            DELIMITED BY SIZE INTO SSA1                                   
662600     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
662700            DELIMITED BY SIZE INTO SSA2                                   
662800     MOVE 'WLXXDV21 ' TO SSA3                                             
662900     MOVE '  II' TO GOOD-STATUSCODES                                      
663000     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3                        
663100                               SSA1 SSA2 SSA3                             
663200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
663300     PERFORM IMS-STATUSCHECK                                              
663400     .                                                                    
663500     EJECT                                                                
663600 IMS-ISRT-4322-SEGM SECTION.                                              
663700     MOVE 'IMS-ISRT-4322-SEGM'         TO  WS-CURRENT-IMS-SECTION         
663800      ADD +1                     TO REBOOT-PROGRAM-IX                     
663900                                                                          
664000     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
664100            DELIMITED BY SIZE INTO SSA1                                   
664200     MOVE 'WLXXJK11*L' TO SSA2                                            
664300     MOVE '  ' TO GOOD-STATUSCODES                                        
664400     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
664500     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
664600     PERFORM IMS-STATUSCHECK                                              
664700     .                                                                    
664800     EJECT                                                                
664900                                                                          
665000 IMS-GU-WDM211 SECTION.                                                   
665100     MOVE 'IMS-GU-WDM211       ' TO WS-CURRENT-IMS-SECTION                
665200                                                                          
665300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
665400          DELIMITED BY SIZE INTO SSA1                                     
665500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
665600          DELIMITED BY SIZE INTO SSA2                                     
665700     MOVE '  GE'              TO GOOD-STATUSCODES                         
665800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
665900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
666000     PERFORM IMS-STATUSCHECK                                              
666100     .                                                                    
666200                                                                          
666300 IMS-GNP-WDM221 SECTION.                                                  
666400     MOVE 'IMS-GNP-WDM221             ' TO WS-CURRENT-IMS-SECTION         
666500                                                                          
666600     MOVE 'WDM221 '           TO SSA1                                     
666700     MOVE '    GE'            TO GOOD-STATUSCODES                         
666800     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
666900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
667000     PERFORM IMS-STATUSCHECK                                              
667100     .                                                                    
667200                                                                          
667300 IMS-GHU-WDM211 SECTION.                                                  
667400     MOVE 'IMS-GU-WDM211      ' TO WS-CURRENT-IMS-SECTION                 
667500                                                                          
667600     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
667700          DELIMITED BY SIZE INTO SSA1                                     
667800     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
667900          DELIMITED BY SIZE INTO SSA2                                     
668000     MOVE '  GE'              TO GOOD-STATUSCODES                         
668100     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
668200     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
668300     PERFORM IMS-STATUSCHECK                                              
668400     .                                                                    
668500                                                                          
668600 IMS-REPL-WDM211 SECTION.                                                 
668700     MOVE 'IMS-REPL-WDM211     ' TO WS-CURRENT-IMS-SECTION                
668800                                                                          
668900     MOVE '  '             TO GOOD-STATUSCODES                            
669000     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
669100     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
669200     PERFORM IMS-STATUSCHECK                                              
669300     .                                                                    
669400                                                                          
669500 IMS-GHU-WDM221 SECTION.                                                  
669600     MOVE 'IMS-GHU-WDM221      ' TO WS-CURRENT-IMS-SECTION                
669700                                                                          
669800     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
669900          DELIMITED BY SIZE INTO SSA1                                     
670000     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
670100          DELIMITED BY SIZE INTO SSA2                                     
670200     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
670300          DELIMITED BY SIZE INTO SSA3                                     
670400     MOVE '  GE' TO GOOD-STATUSCODES                                      
670500     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
670600     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
670700     PERFORM IMS-STATUSCHECK                                              
670800     .                                                                    
670900                                                                          
671000 IMS-REPL-WDM221 SECTION.                                                 
671100     MOVE 'IMS-REPL-WDM221     ' TO WS-CURRENT-IMS-SECTION                
671200                                                                          
671300     MOVE '  '             TO GOOD-STATUSCODES                            
671400     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
671500     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
671600     PERFORM IMS-STATUSCHECK                                              
671700     .                                                                    
671800     EJECT                                                                
671900 IMS-GU-ORQA01    SECTION.                                                
672000     MOVE 'IMS-GU-ORQA01'              TO  WS-CURRENT-IMS-SECTION         
672100                                                                          
672200     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
672300            DELIMITED BY SIZE INTO SSA1                                   
672400     MOVE '  ' TO GOOD-STATUSCODES                                        
672500     CALL CBLTDLI USING GU    WDQ301-PCB DLI-IO-WDQ301 SSA1               
672600     MOVE WDQ301-STATUS-CODE TO STATUS-WS                                 
672700     PERFORM IMS-STATUSCHECK                                              
672800     .                                                                    
672900     EJECT                                                                
673000 IMS-GU-WDQ301-STATUS  SECTION.                                           
673100     MOVE 'IMS-GU-WDQ301-STATUS'       TO  WS-CURRENT-IMS-SECTION         
673200                                                                          
673300     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN                          
673400                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
673500                    '&KDODELST =' W-KDODELST     ')'                      
673600          DELIMITED BY SIZE INTO SSA1                                     
673700     MOVE '  GE' TO GOOD-STATUSCODES                                      
673800     CALL CBLTDLI USING GU  WDQ301-PCB DLI-IO-WDQ301 SSA1                 
673900     MOVE WDQ301-STATUS-CODE TO STATUS-WS                                 
674000     PERFORM IMS-STATUSCHECK                                              
674100     .                                                                    
674200 IMS-GU-WDQ301-ODEL SECTION.                                              
674300     MOVE 'IMS-GU-WDQ301-ODEL  '       TO  WS-CURRENT-IMS-SECTION         
674400                                                                          
674500     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
674600                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
674700            DELIMITED BY SIZE INTO SSA1                                   
674800     MOVE '  GE' TO GOOD-STATUSCODES                                      
674900     CALL CBLTDLI USING GU ORQA2-PCB DLI-IO-WDQ301 SSA1                   
675000     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
675100     PERFORM IMS-STATUSCHECK                                              
675200     .                                                                    
675300     SKIP3                                                                
675400 IMS-GN-WDQ301-ODEL SECTION.                                              
675500     MOVE 'IMS-GN-WDQ301-ODEL  '       TO  WS-CURRENT-IMS-SECTION         
675600                                                                          
675700     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
675800                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
675900            DELIMITED BY SIZE INTO SSA1                                   
676000     MOVE '  GE' TO GOOD-STATUSCODES                                      
676100     CALL CBLTDLI USING GN ORQA2-PCB DLI-IO-WDQ301 SSA1                   
676200     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
676300     PERFORM IMS-STATUSCHECK                                              
676400     .                                                                    
676500     SKIP3                                                                
676600 IMS-NY-GU-WDQ301-ODEL SECTION.                                           
676700     MOVE 'IMS-NY-GU-WDQ301-ODEL '  TO  WS-CURRENT-IMS-SECTION            
676800                                                                          
676900     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-MIN-X ')'                    
677000            DELIMITED BY SIZE INTO SSA1                                   
677100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
677200     CALL CBLTDLI USING GU ORQA2-PCB DLI-IO-WDQ301 SSA1                   
677300     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
677400     PERFORM IMS-STATUSCHECK                                              
677500     .                                                                    
677600     EJECT                                                                
677700 IMS-REPL-ORQA01        SECTION.                                          
677800     MOVE 'IMS-REPL-ORQA01   '         TO  WS-CURRENT-IMS-SECTION         
677900      ADD +2                     TO REBOOT-PROGRAM-IX                     
678000                                                                          
678100     MOVE '    ' TO GOOD-STATUSCODES                                      
678200     CALL CBLTDLI USING REPL WDQ301-PCB DLI-IO-WDQ301                     
678300     MOVE WDQ301-STATUS-CODE TO STATUS-WS                                 
678400     PERFORM IMS-STATUSCHECK                                              
678500     .                                                                    
678600     EJECT                                                                
678700 IMS-GHU-ORQA01   SECTION.                                                
678800     MOVE 'IMS-GHU-ORQA01    '         TO  WS-CURRENT-IMS-SECTION         
678900                                                                          
679000     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-KEY-X ')'                      
679100            DELIMITED BY SIZE INTO SSA1                                   
679200     MOVE '  ' TO GOOD-STATUSCODES                                        
679300     CALL CBLTDLI USING GHU    WDQ301-PCB DLI-IO-WDQ301 SSA1              
679400     MOVE WDQ301-STATUS-CODE TO STATUS-WS                                 
679500     PERFORM IMS-STATUSCHECK                                              
679600     .                                                                    
679700 IMS-GU-ORQM01 SECTION.                                                   
679800     MOVE 'IMS-GU-ORQM01     '         TO  WS-CURRENT-IMS-SECTION         
679900                                                                          
680000     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
680100                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
680200          DELIMITED BY SIZE INTO SSA1                                     
680300     MOVE '  GE'               TO GOOD-STATUSCODES                        
680400     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-Q101 SSA1                    
680500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
680600     PERFORM IMS-STATUSCHECK                                              
680700     .                                                                    
680800     SKIP2                                                                
680900 IMS-ISRT-ORQM01     SECTION.                                             
681000     MOVE 'IMS-ISRT-ORQM01 '         TO  WS-CURRENT-IMS-SECTION           
681100      ADD +1                     TO REBOOT-PROGRAM-IX                     
681200                                                                          
681300     MOVE 'WLORQM01 ' TO SSA1                                             
681400     MOVE '  '   TO GOOD-STATUSCODES                                      
681500     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-Q101 SSA1                    
681600     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
681700     PERFORM IMS-STATUSCHECK                                              
681800     .                                                                    
681900     EJECT                                                                
682000                                                                          
682100 IMS-GU-ORQI01    SECTION.                                                
682200     MOVE 'IMS-GU-ORQI01  '         TO  WS-CURRENT-IMS-SECTION            
682300                                                                          
682400     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
682500            DELIMITED BY SIZE INTO SSA1                                   
682600     MOVE '    ' TO GOOD-STATUSCODES                                      
682700     CALL CBLTDLI USING GU   ORQI-PCB OHUV-WDQ201 SSA1                    
682800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
682900     PERFORM IMS-STATUSCHECK                                              
683000     .                                                                    
683100 IMS-GNP-ORQI12    SECTION.                                               
683200                                                                          
683300     MOVE 'WLORQI12 ' TO SSA1                                             
683400     MOVE '    ' TO GOOD-STATUSCODES                                      
683500     CALL CBLTDLI USING GNP   ORQI-PCB ARB-WDQ212 SSA1                    
683600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
683700     PERFORM IMS-STATUSCHECK                                              
683800     .                                                                    
683900 IMS-GU-ORQI01-GE SECTION.                                                
684000     MOVE 'IMS-GU-ORQI01-GE'        TO  WS-CURRENT-IMS-SECTION            
684100                                                                          
684200     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
684300            DELIMITED BY SIZE INTO SSA1                                   
684400     MOVE '  GE' TO GOOD-STATUSCODES                                      
684500     CALL CBLTDLI USING GU   ORQI-PCB OHUV-WDQ201 SSA1                    
684600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
684700     PERFORM IMS-STATUSCHECK                                              
684800     .                                                                    
684900 IMS-GHNP-ORQI12 SECTION.                                                 
685000     MOVE 'IMS-GHNP-ORQI12'          TO  WS-CURRENT-IMS-SECTION           
685100                                                                          
685200     STRING 'WLORQI12*F(IDDC     =' W-WDQ212-X ')'                        
685300            DELIMITED BY SIZE INTO SSA1                                   
685400     MOVE '    ' TO GOOD-STATUSCODES                                      
685500     CALL CBLTDLI USING GHNP  ORQI-PCB ARB-WDQ212 SSA1                    
685600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
685700     PERFORM IMS-STATUSCHECK                                              
685800     .                                                                    
685900 IMS-REPL-ORQI12      SECTION.                                            
686000     MOVE 'IMS-REPL-ORQI12 '       TO  WS-CURRENT-IMS-SECTION             
686100     ADD +1                        TO REBOOT-PROGRAM-IX                   
686200                                                                          
686300     MOVE '    ' TO GOOD-STATUSCODES                                      
686400     CALL CBLTDLI USING REPL ORQI-PCB ARB-WDQ212                          
686500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
686600     PERFORM IMS-STATUSCHECK                                              
686700     .                                                                    
686800     EJECT                                                                
686900 IMS-GU-ORQI01-CSEQ-ORQL SECTION.                                         
687000     MOVE 'IMS-GU-ORQI01-CSEQ-ORQL'  TO  WS-CURRENT-IMS-SECTION           
687100                                                                          
687200     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2C-X ')'                          
687300             DELIMITED BY SIZE INTO    SSA1                               
687400     MOVE    '    '              TO    GOOD-STATUSCODES                   
687500     CALL    CBLTDLI             USING GU   ORQL-PCB                      
687600                                            OHUV-WDQ201 SSA1              
687700     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
687800     PERFORM IMS-STATUSCHECK                                              
687900     .                                                                    
688000 IMS-GU-ORQI01-CSEQ SECTION.                                              
688100     MOVE 'IMS-GU-ORQI01-CSEQ'     TO  WS-CURRENT-IMS-SECTION             
688200                                                                          
688300     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
688400             DELIMITED BY SIZE INTO SSA1                                  
688500     MOVE    '  '                TO GOOD-STATUSCODES                      
688600     CALL    CBLTDLI USING       GU ORQICSQ-PCB OHUV-WDQ201               
688700                                    SSA1                                  
688800     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
688900     PERFORM IMS-STATUSCHECK                                              
689000     .                                                                    
689100                                                                          
689200 IMS-GHU-XXKW11       SECTION.                                            
689300     MOVE 'IMS-GHU-XXKW11  '       TO  WS-CURRENT-IMS-SECTION             
689400                                                                          
689500     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
689600            DELIMITED BY SIZE INTO SSA1                                   
689700     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
689800            DELIMITED BY SIZE INTO SSA2                                   
689900     MOVE '  GE' TO GOOD-STATUSCODES                                      
690000     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
690100     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
690200     PERFORM IMS-STATUSCHECK                                              
690300     .                                                                    
690400 IMS-REPL-XXKW11    SECTION.                                              
690500     MOVE 'IMS-REPL-XXKW11 '       TO  WS-CURRENT-IMS-SECTION             
690600      ADD +1                     TO REBOOT-PROGRAM-IX                     
690700                                                                          
690800     MOVE '  '   TO GOOD-STATUSCODES                                      
690900     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
691000     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
691100     PERFORM IMS-STATUSCHECK                                              
691200     .                                                                    
691300     EJECT                                                                
691400 IMS-GU-XXLB         SECTION.                                             
691500     MOVE 'IMS-GU-XXLB     '       TO  WS-CURRENT-IMS-SECTION             
691600                                                                          
691700     STRING 'WLXXLB01(WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
691800            DELIMITED BY SIZE INTO SSA1                                   
691900     STRING 'WLXXLB11(WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
692000            DELIMITED BY SIZE INTO SSA2                                   
692100     MOVE '  GE' TO GOOD-STATUSCODES                                      
692200     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA6 SSA1 SSA2             
692300     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
692400     PERFORM IMS-STATUSCHECK                                              
692500     .                                                                    
692600     EJECT                                                                
692700 IMS-ISRT-ZZAC01 SECTION.                                                 
692800     MOVE 'IMS-ISRT-ZZAC01 '       TO  WS-CURRENT-IMS-SECTION             
692900      ADD +1                     TO REBOOT-PROGRAM-IX                     
693000                                                                          
693100     MOVE 'WLZZAC01' TO SSA1                                              
693200     MOVE '  II'     TO GOOD-STATUSCODES                                  
693300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
693400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
693500     PERFORM IMS-STATUSCHECK                                              
693600     .                                                                    
693700     EJECT                                                                
693800                                                                          
693900 IMS-GHU-ORDP01   SECTION.                                                
694000     MOVE 'IMS-GHU-ORDP01  '       TO  WS-CURRENT-IMS-SECTION             
694100                                                                          
694200     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
694300            DELIMITED BY SIZE INTO SSA1                                   
694400     MOVE '  '     TO GOOD-STATUSCODES                                    
694500     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
694600     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
694700     PERFORM IMS-STATUSCHECK                                              
694800     SKIP3                                                                
694900     .                                                                    
695000 IMS-GHU-ORDP01-GE           SECTION.                                     
695100     MOVE 'IMS-GHU-ORDP01-GE '   TO  WS-CURRENT-IMS-SECTION               
695200                                                                          
695300     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
695400            DELIMITED BY SIZE INTO SSA1                                   
695500     MOVE '  GE'   TO GOOD-STATUSCODES                                    
695600     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
695700     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
695800     PERFORM IMS-STATUSCHECK                                              
695900     SKIP3                                                                
696000     .                                                                    
696100 IMS-REPL-ORDP01  SECTION.                                                
696200     MOVE 'IMS-REPL-ORDP01  '   TO  WS-CURRENT-IMS-SECTION                
696300      ADD +1                     TO REBOOT-PROGRAM-IX                     
696400                                                                          
696500     MOVE '  '     TO GOOD-STATUSCODES                                    
696600     CALL CBLTDLI USING REPL ORDP1-PCB DLI-IO-AREA6                       
696700     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
696800     PERFORM IMS-STATUSCHECK                                              
696900     SKIP3                                                                
697000     .                                                                    
697100 IMS-ISRT-ORDP01  SECTION.                                                
697200     MOVE 'IMS-ISRT-ORDP01  '   TO  WS-CURRENT-IMS-SECTION                
697300      ADD +1                     TO REBOOT-PROGRAM-IX                     
697400                                                                          
697500     MOVE 'WLORDP01 ' TO SSA1                                             
697600     MOVE '  II'   TO GOOD-STATUSCODES                                    
697700     CALL CBLTDLI USING ISRT ORDP1-PCB DLI-IO-AREA6 SSA1                  
697800     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
697900     PERFORM IMS-STATUSCHECK                                              
698000     SKIP3                                                                
698100     .                                                                    
698200 IMS-DLET-ORDP01  SECTION.                                                
698300     MOVE 'IMS-DLET-ORDP01  '   TO  WS-CURRENT-IMS-SECTION                
698400      ADD +1                     TO REBOOT-PROGRAM-IX                     
698500                                                                          
698600     MOVE '  '     TO GOOD-STATUSCODES                                    
698700     CALL CBLTDLI USING DLET ORDP1-PCB DLI-IO-AREA6                       
698800     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
698900     PERFORM IMS-STATUSCHECK                                              
699000     EJECT                                                                
699100     .                                                                    
699200 IMS-GHU-ORDP01-OLD SECTION.                                              
699300     MOVE 'IMS-GHU-ORDP01-OLD'  TO  WS-CURRENT-IMS-SECTION                
699400                                                                          
699500     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
699600                    '&WDA501KY=<' W2-WDA501KY-X                           
699700                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
699800            DELIMITED BY SIZE INTO SSA1                                   
699900     MOVE '  GE'   TO GOOD-STATUSCODES                                    
700000     CALL CBLTDLI USING GHU ORDP2-PCB DLI-IO-AREA7 SSA1                   
700100     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
700200     PERFORM IMS-STATUSCHECK                                              
700300     SKIP3                                                                
700400     .                                                                    
700500 IMS-GHN-ORDP01-OLD SECTION.                                              
700600     MOVE 'IMS-GHN-ORDP01-OLD'  TO  WS-CURRENT-IMS-SECTION                
700700                                                                          
700800     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
700900                    '&WDA501KY=<' W2-WDA501KY-X                           
701000                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
701100            DELIMITED BY SIZE INTO SSA1                                   
701200     MOVE '  GE'   TO GOOD-STATUSCODES                                    
701300     CALL CBLTDLI USING GHN ORDP2-PCB DLI-IO-AREA7 SSA1                   
701400     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
701500     PERFORM IMS-STATUSCHECK                                              
701600     SKIP3                                                                
701700     .                                                                    
701800 IMS-REPL-ORDP01-OLD  SECTION.                                            
701900     MOVE 'IMS-REPL-ORDP01-OLD' TO  WS-CURRENT-IMS-SECTION                
702000      ADD +1                     TO REBOOT-PROGRAM-IX                     
702100                                                                          
702200     MOVE '  '     TO GOOD-STATUSCODES                                    
702300     CALL CBLTDLI USING REPL ORDP2-PCB DLI-IO-AREA7                       
702400     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
702500     PERFORM IMS-STATUSCHECK                                              
702600     EJECT                                                                
702700     .                                                                    
702800 IMS-GU-XXJN     SECTION.                                                 
702900     MOVE 'IMS-GU-XXJN  '       TO  WS-CURRENT-IMS-SECTION                
703000                                                                          
703100     STRING 'WLXXJN01(WDGXKEY  =' W-WDGX01 ')'                            
703200            DELIMITED BY SIZE INTO SSA1                                   
703300     STRING 'WLXXJN11(WDGXKEY >=' W-WDGXKEY-N5-MIN                        
703400                    '&WDGXKEY <=' W-WDGXKEY-N5-MAX                        
703500                    '&KDRAPRIO>=' W-KDRAPRIO-N5-MIN-X                     
703600                    '&KDRAPRIO<=' W-KDRAPRIO-N5-MAX-X                     
703700                    '&KDTPOTYP =' W-KDTPOTYP-N5-X                         
703800                    '&KDORDKL  =' W-KDORDKL-N5-X                          
703900                    '&IDDISTRF<=' W-IDDISTR-FOM-N5-X                      
704000                    '&IDDISTRT>=' W-IDDISTR-TOM-N5-X ')'                  
704100            DELIMITED BY SIZE INTO SSA2                                   
704200     MOVE '  ' TO GOOD-STATUSCODES                                        
704300     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA4 SSA1 SSA2                
704400     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
704500     PERFORM IMS-STATUSCHECK                                              
704600     EJECT                                                                
704700     .                                                                    
704800 IMS-GU-ARTC11 SECTION.                                                   
704900     MOVE 'IMS-GU-ARTC11'       TO  WS-CURRENT-IMS-SECTION                
705000                                                                          
705100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
705200            DELIMITED BY SIZE INTO SSA1                                   
705300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
705400          DELIMITED BY SIZE INTO SSA2                                     
705500     MOVE '  '     TO GOOD-STATUSCODES                                    
705600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2                
705700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
705800     PERFORM IMS-STATUSCHECK                                              
705900     SKIP3                                                                
706000     .                                                                    
706100 IMS-GHU-ARTC11     SECTION.                                              
706200     MOVE 'IMS-GHU-ARTC11 '     TO  WS-CURRENT-IMS-SECTION                
706300                                                                          
706400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
706500            DELIMITED BY SIZE INTO SSA1                                   
706600     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
706700          DELIMITED BY SIZE INTO SSA2                                     
706800     MOVE '  '   TO GOOD-STATUSCODES                                      
706900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2               
707000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
707100     PERFORM IMS-STATUSCHECK                                              
707200     SKIP2                                                                
707300     .                                                                    
707400 IMS-REPL-ARTC       SECTION.                                             
707500     MOVE 'IMS-REPL-ARTC  '     TO  WS-CURRENT-IMS-SECTION                
707600      ADD +2                     TO REBOOT-PROGRAM-IX                     
707700                                                                          
707800     MOVE '  '   TO GOOD-STATUSCODES                                      
707900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA8                        
708000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
708100     PERFORM IMS-STATUSCHECK                                              
708200     .                                                                    
708300     SKIP2                                                                
708400 IMS-GHU-WDK711     SECTION.                                              
708500     MOVE 'IMS-GHU-WDK711 '     TO  WS-CURRENT-IMS-SECTION                
708600                                                                          
708700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
708800            DELIMITED BY SIZE INTO SSA1                                   
708900     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
709000            DELIMITED BY SIZE INTO SSA2                                   
709100     MOVE '  GE'   TO GOOD-STATUSCODES                                    
709200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA8 SSA1 SSA2               
709300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
709400     PERFORM IMS-STATUSCHECK                                              
709500     SKIP2                                                                
709600     .                                                                    
709700 IMS-GU-WDK712 SECTION.                                                   
709800     MOVE 'IMS-GU-WDK712  '     TO  WS-CURRENT-IMS-SECTION                
709900                                                                          
710000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
710100          DELIMITED BY SIZE INTO SSA1                                     
710200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
710300          DELIMITED BY SIZE INTO SSA2                                     
710400     MOVE '  GE' TO GOOD-STATUSCODES                                      
710500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
710600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
710700     PERFORM IMS-STATUSCHECK                                              
710800     .                                                                    
710900 IMS-GU-WDK722     SECTION.                                               
711000     MOVE 'IMS-GU-WDK711  '     TO  WS-CURRENT-IMS-SECTION                
711100                                                                          
711200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
711300            DELIMITED BY SIZE INTO SSA1                                   
711400     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
711500            DELIMITED BY SIZE INTO SSA2                                   
711600     MOVE 'WDK722 '             TO SSA3                                   
711700     MOVE '  GE'   TO GOOD-STATUSCODES                                    
711800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
711900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
712000     PERFORM IMS-STATUSCHECK                                              
712100     SKIP2                                                                
712200     .                                                                    
712300 IMS-REPL-WDK7       SECTION.                                             
712400     MOVE 'IMS-REPL-WDK7  '     TO  WS-CURRENT-IMS-SECTION                
712500      ADD +2                     TO REBOOT-PROGRAM-IX                     
712600                                                                          
712700     MOVE '  '   TO GOOD-STATUSCODES                                      
712800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA8                        
712900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
713000     PERFORM IMS-STATUSCHECK                                              
713100     .                                                                    
713200     EJECT                                                                
713300 IMS-GHU-WDK901 SECTION.                                                  
713400     MOVE 'IMS-GHU-WDK901 '     TO  WS-CURRENT-IMS-SECTION                
713500                                                                          
713600     STRING 'WLARTM01(IDARTNR  =' W-WDK901-IDARTNR-X ')'                  
713700            DELIMITED BY SIZE INTO SSA1                                   
713800     MOVE '  '     TO GOOD-STATUSCODES                                    
713900     CALL CBLTDLI USING GHU ARTM-PCB ART-WDK901 SSA1                      
714000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
714100     PERFORM IMS-STATUSCHECK                                              
714200     SKIP3                                                                
714300     .                                                                    
714400 IMS-REPL-WDK901 SECTION.                                                 
714500     MOVE 'IMS-REPL-WDK901'     TO  WS-CURRENT-IMS-SECTION                
714600      ADD +1                     TO REBOOT-PROGRAM-IX                     
714700                                                                          
714800     MOVE '  '     TO GOOD-STATUSCODES                                    
714900     CALL CBLTDLI USING REPL ARTM-PCB ART-WDK901                          
715000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
715100     PERFORM IMS-STATUSCHECK                                              
715200     SKIP3                                                                
715300     .                                                                    
715400 IMS-GU-XXKH11 SECTION.                                                   
715500     MOVE 'IMS-GU-XXKH11  '     TO  WS-CURRENT-IMS-SECTION                
715600                                                                          
715700     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X    ')'                         
715800            DELIMITED BY SIZE INTO SSA1                                   
715900     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X    ')'                         
716000            DELIMITED BY SIZE INTO SSA2                                   
716100     MOVE '  '   TO GOOD-STATUSCODES                                      
716200     CALL CBLTDLI USING GU XXKH-PCB 4448-WDGX4448-CTX SSA1 SSA2           
716300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
716400     PERFORM IMS-STATUSCHECK                                              
716500     SKIP2                                                                
716600     .                                                                    
716700     EJECT                                                                
716800 IMS-ISRT-AUTFAKTURA-ROT SECTION.                                         
716900     MOVE 'IMS-ISRT-AUTFAKTURA-ROT' TO WS-CURRENT-IMS-SECTION             
717000      ADD +1                     TO REBOOT-PROGRAM-IX                     
717100                                                                          
717200     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
717300            DELIMITED BY SIZE INTO SSA1                                   
717400     MOVE   'WLXXDV11 '         TO SSA2                                   
717500     MOVE '  '     TO GOOD-STATUSCODES                                    
717600     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3 SSA1 SSA2              
717700     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
717800     PERFORM IMS-STATUSCHECK                                              
717900     SKIP2                                                                
718000     .                                                                    
718100 IMS-ISRT-AUTFAKTURA     SECTION.                                         
718200     MOVE 'IMS-ISRT-AUTFAKTURA'     TO WS-CURRENT-IMS-SECTION             
718300      ADD +1                     TO REBOOT-PROGRAM-IX                     
718400                                                                          
718500     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
718600            DELIMITED BY SIZE INTO SSA1                                   
718700     STRING 'WLXXDV11(WDGXKEY  =' W-WDGX11-WDGXKEY-X ')'                  
718800            DELIMITED BY SIZE INTO SSA2                                   
718900     MOVE 'WLXXDV21 ' TO SSA3                                             
719000     MOVE '  IIGE' TO GOOD-STATUSCODES                                    
719100     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3                        
719200                                        SSA1 SSA2 SSA3                    
719300     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
719400     PERFORM IMS-STATUSCHECK                                              
719500     EJECT                                                                
719600     .                                                                    
719700 IMS-ISRT-4542 SECTION.                                                   
719800     MOVE 'IMS-ISRT-4542 '          TO WS-CURRENT-IMS-SECTION             
719900      ADD +1                     TO REBOOT-PROGRAM-IX                     
720000                                                                          
720100     STRING 'WL454101(WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
720200         DELIMITED BY SIZE INTO SSA1                                      
720300     MOVE 'WL454111 ' TO SSA2                                             
720400     MOVE '  II' TO GOOD-STATUSCODES                                      
720500     CALL CBLTDLI USING ISRT 4541-PCB 4542-WDGX4542 SSA1 SSA2             
720600     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
720700     PERFORM IMS-STATUSCHECK                                              
720800     .                                                                    
720900     SKIP2                                                                
721000 IMS-GHU-WDGX4490 SECTION.                                                
721100     MOVE 'IMS-GHU-WDGX4490'        TO WS-CURRENT-IMS-SECTION             
721200                                                                          
721300     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
721400         DELIMITED BY SIZE INTO SSA1                                      
721500     STRING 'WDGX4488(KDPRCGRP =' W-KDPRCGRP-X ')'                        
721600         DELIMITED BY SIZE INTO SSA2                                      
721700     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
721800         DELIMITED BY SIZE INTO SSA3                                      
721900     MOVE '  GE' TO GOOD-STATUSCODES                                      
722000     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-WDGX4490                      
722100                        SSA1 SSA2 SSA3                                    
722200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
722300     PERFORM IMS-STATUSCHECK                                              
722400     .                                                                    
722500     SKIP2                                                                
722600 IMS-DLET-WDGX4490 SECTION.                                               
722700     MOVE 'IMS-DLET-WDGX4490 '      TO WS-CURRENT-IMS-SECTION             
722800      ADD +1                     TO REBOOT-PROGRAM-IX                     
722900                                                                          
723000     MOVE '  ' TO GOOD-STATUSCODES                                        
723100     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-WDGX4490                     
723200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
723300     PERFORM IMS-STATUSCHECK                                              
723400     .                                                                    
723500     SKIP2                                                                
723600 IMS-ISRT-KLAR-SV4   SECTION.                                             
723700     MOVE 'IMS-ISRT-KLAR-SV4 '      TO WS-CURRENT-IMS-SECTION             
723800      ADD +1                     TO REBOOT-PROGRAM-IX                     
723900                                                                          
724000     MOVE 'WLZZAC01'  TO SSA1                                             
724100     MOVE '  II' TO GOOD-STATUSCODES                                      
724200     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
724300     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
724400     PERFORM IMS-STATUSCHECK                                              
724500     SKIP2                                                                
724600     .                                                                    
724700 IMS-ISRT-AVVIKELSE  SECTION.                                             
724800     MOVE 'IMS-ISRT-AVVIKELSE'      TO WS-CURRENT-IMS-SECTION             
724900      ADD +1                     TO REBOOT-PROGRAM-IX                     
725000                                                                          
725100     MOVE 'WLZZAC01'  TO SSA1                                             
725200     MOVE '  II' TO GOOD-STATUSCODES                                      
725300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
725400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
725500     PERFORM IMS-STATUSCHECK                                              
725600     SKIP2                                                                
725700     .                                                                    
725800 IMS-ISRT-WDL9 SECTION.                                                   
725900     MOVE 'IMS-ISRT-WDL9     '      TO WS-CURRENT-IMS-SECTION             
726000      ADD +1                     TO REBOOT-PROGRAM-IX                     
726100                                                                          
726200     MOVE 'WLLOGA01 ' TO SSA1                                             
726300     MOVE '  II' TO GOOD-STATUSCODES                                      
726400     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
726500     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
726600     PERFORM IMS-STATUSCHECK                                              
726700     .                                                                    
726800     SKIP2                                                                
726900 IMS-GU-WDK601                 SECTION.                                   
727000     MOVE 'IMS-GU-WDK601     '      TO WS-CURRENT-IMS-SECTION             
727100                                                                          
727200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
727300            DELIMITED BY SIZE INTO SSA1                                   
727400     MOVE '  '                   TO GOOD-STATUSCODES                      
727500     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-AREA8  SSA1                
727600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
727700     PERFORM IMS-STATUSCHECK                                              
727800     .                                                                    
727900     SKIP2                                                                
728000 IMS-GHNP-WDK611               SECTION.                                   
728100     MOVE 'IMS-GHNP-WDK611   '      TO WS-CURRENT-IMS-SECTION             
728200                                                                          
728300     MOVE 'WDK611  '           TO SSA1                                    
728400     MOVE '  '                 TO GOOD-STATUSCODES                        
728500     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-AREA8  SSA1                
728600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
728700     PERFORM IMS-STATUSCHECK                                              
728800     .                                                                    
728900     SKIP2                                                                
729000 IMS-REPL-WDK611               SECTION.                                   
729100     MOVE 'IMS-REPL-WDK611   '      TO WS-CURRENT-IMS-SECTION             
729200      ADD +2                     TO REBOOT-PROGRAM-IX                     
729300                                                                          
729400     MOVE 'WDK611  '           TO SSA1                                    
729500     MOVE '    '               TO GOOD-STATUSCODES                        
729600     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-AREA8  SSA1                
729700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
729800     PERFORM IMS-STATUSCHECK                                              
729900     .                                                                    
730000     SKIP2                                                                
730100 IMS-GHU-SEQB-WDA601            SECTION.                                  
730200     MOVE 'IMS-GHU-SEQB-WDA601'     TO WS-CURRENT-IMS-SECTION             
730300                                                                          
730400     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
730500                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
730600            DELIMITED BY SIZE INTO SSA1                                   
730700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
730800     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA11 SSA1              
730900     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
731000     PERFORM IMS-STATUSCHECK                                              
731100     .                                                                    
731200     SKIP2                                                                
731300 IMS-GHN-SEQB-WDA601            SECTION.                                  
731400     MOVE 'IMS-GHN-SEQB-WDA601'     TO WS-CURRENT-IMS-SECTION             
731500                                                                          
731600     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
731700                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
731800            DELIMITED BY SIZE INTO SSA1                                   
731900     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
732000     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA11 SSA1              
732100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
732200     PERFORM IMS-STATUSCHECK                                              
732300     .                                                                    
732400     SKIP2                                                                
732500 IMS-REPL-SEQB-WDA601                 SECTION.                            
732600     MOVE 'IMS-REPL-SEQB-WDA601'    TO WS-CURRENT-IMS-SECTION             
732700      ADD +5                     TO REBOOT-PROGRAM-IX                     
732800                                                                          
732900     MOVE 'WDA601  '           TO SSA1                                    
733000     MOVE '    '               TO GOOD-STATUSCODES                        
733100     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA11 SSA1               
733200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
733300     PERFORM IMS-STATUSCHECK                                              
733400     .                                                                    
733500     EJECT                                                                
733600 IMS-ISRT-WDA601            SECTION.                                      
733700     MOVE 'IMS-ISRT-WDA601     '    TO WS-CURRENT-IMS-SECTION             
733800      ADD +5                     TO REBOOT-PROGRAM-IX                     
733900                                                                          
734000     MOVE   'WDA601  '         TO SSA1                                    
734100     MOVE '  IINI' TO GOOD-STATUSCODES                                    
734200     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA11 SSA1                
734300     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
734400     PERFORM IMS-STATUSCHECK                                              
734500     .                                                                    
734600     SKIP3                                                                
734700 IMS-GU-WDB601    SECTION.                                                
734800     MOVE 'IMS-GU-WDB601       '    TO WS-CURRENT-IMS-SECTION             
734900                                                                          
735000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
735100     DELIMITED BY SIZE INTO SSA1                                          
735200     MOVE '  '    TO GOOD-STATUSCODES                                     
735300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
735400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
735500     PERFORM IMS-STATUSCHECK                                              
735600     IF SEGMENT-MISSING                                                   
735700        MOVE SPACE TO DCS-KDDC                                            
735800     END-IF                                                               
735900     .                                                                    
736000     EJECT                                                                
736100 IMS-GU-WDE401-ESEQ SECTION.                                              
736200     MOVE 'IMS-GU-WDE401-ESEQ  '    TO WS-CURRENT-IMS-SECTION             
736300                                                                          
736400     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
736500            DELIMITED BY SIZE INTO SSA1                                   
736600     MOVE '    ' TO GOOD-STATUSCODES                                      
736700     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-WDE401 SSA1                   
736800     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
736900     PERFORM IMS-STATUSCHECK                                              
737000     .                                                                    
737100     SKIP3                                                                
737200                                                                          
737300 IMS-GU-WDP4A1 SECTION.                                                   
737400     MOVE 'IMS-GU-WDP4A1       '    TO WS-CURRENT-IMS-SECTION             
737500                                                                          
737600     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
737700                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
737800          DELIMITED BY SIZE INTO SSA1                                     
737900     MOVE '  GE' TO GOOD-STATUSCODES                                      
738000     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
738100                           SSA1                                           
738200     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
738300     PERFORM IMS-STATUSCHECK                                              
738400     .                                                                    
738500                                                                          
738600 DB2-SELECT-TP4TRAN     SECTION.                                          
738700*    MOVE 'DB2-SELECT-TP4TRAN  '    TO WS-CURRENT-IMS-SECTION             
738800     MOVE 'DB2-SELECT-TP4TRAN  '    TO WS-DB2-SECTION                     
738900                                                                          
739000     MOVE 000100 TO GODK-SQLCODEKODER                                     
739100                                                                          
739200     EXEC SQL                                                             
739300           SELECT  DISTINCT                                               
739400                   IDDC_REC                                               
739500                                                                          
739600           INTO   :TP4TRAN-IDDC-REC                                       
739700                                                                          
739800           FROM    TP4TRAN                                                
739900                                                                          
740000           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
740100     END-EXEC                                                             
740200                                                                          
740300     MOVE SQLCODE TO SQLCODE-WS                                           
740400     PERFORM DB2-STATUSKONTROLL                                           
740500     .                                                                    
740600     EJECT                                                                
740700 DB2-STATUSKONTROLL  SECTION.                                             
740800                                                                          
740900     SET SQLCODE-IX TO 1                                                  
741000     SEARCH GODK-SQLCODE                                                  
741100       AT END                                                             
741200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
741300          DELIMITED BY SIZE INTO ERROR-TEXT                               
741400          CALL ABEND USING RKOD-ABEND-DB2                                 
741500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
741600     END-SEARCH                                                           
741700     .                                                                    
741800 IMS-STATUSCHECK SECTION.                                                 
741900                                                                          
742000     SET STATUS-IX TO 1                                                   
742100     SEARCH GOOD-STATUS                                                   
742200       AT END                                                             
742300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
742400         DELIMITED BY SIZE INTO ERROR-TEXT                                
742500         CALL FELLOG                                                      
742600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
742700         CONTINUE                                                         
742800     END-SEARCH                                                           
742900     .                                                                    
