000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL013420    *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4037600.                                                
000800 AUTHOR.         ROGER OLSSON.                                            
000900 DATE-WRITTEN.   90/06/18.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*1777545      LYNK & CO - CROSS REFERENCE TABLE                           
001300*                                                                         
001400*THIS VERSION TO PRINT 1481822 - INDIA MRP KIT LABEL                      
001500*                                                                         
001600*1721642 - INDIA KIT LABEL KDP                                            
001700*                                                                         
001800*    FUNKTION.                                                            
001900*        SKRIVER PLOCKETIKETTER.                                          
002000*        LÄSER AKTUELL PLOCKSATS (WL400311).                              
002100*        BEHANDLAR ALLA PLE-RADER SOM INGÅR I PLOCKSATSEN.                
002200*        PROGRAMMET STARTAS OM EFTER ETT ANTAL BEHANDLADE SIDOR.          
002300*        NÄR ALLA PLE-RADER HAR BEHANDLATS TAGES PLOCKSATS BORT.          
002400*        VARJE RAD I UTSKRIFTEN BESTÅR AV TVÅ ETIKETTER.                  
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T376X                                             
002800*        MID:         W4I37601                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        TRANSAKTION: W4T376X (GÄLLER VID OMSTART AV PGM)                 
003200*                                                                         
003300*9864093 - PRINT PICKING LABEL ON LASER                                   
003400*                                                                         
003500                                                                          
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W4037600'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
004800 77  ERROR-TEXT                  PIC X(99)   VALUE SPACE.                 
004900 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
005000 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
005100 77  FILLER                      PIC X(08)   VALUE 'IMS-POS:'.            
005200 77  WS-CURRENT-IMS-SECTION      PIC X(32)   VALUE SPACE.                 
005300 77  FILLER                      PIC X(08)   VALUE 'CM-READ:'.            
005400 77  WS-CM-READ-SECTION          PIC X(32)   VALUE SPACE.                 
005500                                                                          
005600 77  FILLER                      PIC X(08)   VALUE 'KDRCDIS:'.            
005700 77  KDRC-DISPLAY                PIC Z(5).                                
005800                                                                          
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  TECKEN-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  ANT-LAGOMR-IX               PIC S9(9)  VALUE +0    COMP SYNC.        
006400 77  ANT-ETIK-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
006500 77  ANT-ETIK-IX-WS              PIC  9(9)  VALUE  0.                     
006600 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
006700 77  ANT-RAD-IX                  PIC S9(9)  VALUE +0   COMP SYNC.         
006800 77  ANT-RAD-IX-WS               PIC  9(9)  VALUE  0.                     
006900 77  ANT-PICKUP-IX               PIC  9(9)  VALUE  0.                     
007000 77  ANT-SID-IX                  PIC S9(9)  VALUE +0    COMP SYNC.        
007100 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
007200 77  LAGOMR-IX                   PIC  9(3)  VALUE  0.                     
007300 77  ETIKETT-IX                  PIC S9(9)  VALUE +0    COMP SYNC.        
007400 77  SID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
007500 77  FILLER                      PIC X(08)   VALUE 'CCCCCCCC'.            
007600 77  ANT-MRP-IX                  PIC  9(9)  VALUE  0.                     
007700 77  PAGE-MRP-IX                 PIC  9(9)  VALUE  0.                     
007800 77  PAGE-MRP-KIT-IX             PIC  9(9)  VALUE  0.                     
007900 77  SHIP-INDX                   PIC  9(9)  VALUE  0.                     
008000 77  KIT-TAB-IX                  PIC  9(9)  VALUE  0.                     
008100 77  LABEL-IX                    PIC  9(9)  VALUE  0.                     
008200 77  PARTS-OF-KIT-PART-IX        PIC  9(9)  VALUE  0.                     
008300 77  NUM-PARTS-IN-KIT-PART       PIC  9(9)  VALUE  0.                     
008400 77  FILLER                      PIC X(08)   VALUE 'DDDDDDDD'.            
008500 77  MAX-SIDA                    PIC S9(9)  VALUE +1000 COMP SYNC.        
008600 77  MAX-LAGOMR                  PIC S9(9)  VALUE +99   COMP SYNC.        
008700 77  MAX-ETIKETT                 PIC S9(9)  VALUE +6    COMP SYNC.        
008800 77  MAX-NO-OF-CONTAIN-PARTS     PIC S9(9)  VALUE +300  COMP SYNC.        
008900 77  MAX-NO-OF-PARTS-LABEL       PIC S9(9)  VALUE +10   COMP SYNC.        
009000 77  WS-IDLOPNR-ORD              PIC  9(2).                               
009100 77  WS-IDZON                    PIC  X(2).                               
009200 77  FILLER                      PIC X(08)   VALUE 'EEEEEEEE'.            
009300 77  WS-TIME-DELAY               PIC S9(9)   VALUE ZERO  BINARY.          
009400 77  WS-IDAFPRCD                 PIC X(10)   VALUE SPACE.                 
009500 77  WS-MRP-IDAFPRCD             PIC X(10)   VALUE SPACE.                 
009600 77  WS-MRP-KIT-IDAFPRCD         PIC X(10)   VALUE SPACE.                 
009700 77  WS-HDR-IDOUTREC             PIC X(08).                               
009800 77  WS-PRARTBTO-SC              PIC 9(7)V9(2)  VALUE ZERO.               
009900 77  WS-CLAG-PRARTSJK            PIC 9(7)V9(2)  VALUE ZERO.               
010000 77  WS-IDARTNR                  PIC  9(9).                               
010100 77  FILLER                      PIC X(08)   VALUE 'GGGGGGGG'.            
010200 77  WS-CLAG-KVQPACK-0           PIC  9(5).                               
010300 77  WS-CLAG-KVQPACK-1           PIC  9(5).                               
010400 77  WS-CLAG-IDARTNR-EMBQ0       PIC  9(9).                               
010500 77  WS-4006-ADLAGOMR            PIC S9(3).                               
010600 77  WS-RAD-TISTODAT             PIC  9(6).                               
010700 77  WS-RAD-REANTPSA             PIC S9(2)V9(3).                          
010800 77  WS-ART-IDLEVNR              PIC X(05).                               
010900 77  WS-ART-KDSORT               PIC X(02).                               
011000 77  WS-KDBENHOM                 PIC S9      VALUE ZERO  COMP-3.          
011100 77  SPAR-RAD-KDBENHOM           PIC S9      VALUE ZERO  COMP-3.          
011200 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
011300 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
011400                                                                          
011500 01  WS-PRARTBTO.                                                         
011600     03 WS-PRARTBTO-INT          PIC 9(08).                               
011700     03 WS-PRARTBTO-DEC          PIC 9(02) VALUE 0.                       
011800                                                                          
011900 01  WS-ROUNDED-UP.                                                       
012000     03 NUMBER-TO-ROUNDUP        PIC S9(2)V99.                            
012100     03 NUMBER-ROUNDEDUP         PIC S9(2)V99.                            
012200     03 INTEGER-N2               PIC S9(2).                               
012300                                                                          
012400 01  FROM-KIT-LABEL-COUNTER      PIC  9(9)  VALUE  0.                     
012500                                                                          
012600 01  WS-LABEL-COUNT-GROUP.                                                
012700     03  FILLER                  PIC X(1)  VALUE '('.                     
012800     03  WS-LABEL-FROM           PIC Z(2).                                
012900     03  FILLER                  PIC X(1)  VALUE SPACE.                   
013000     03  FILLER                  PIC X(2)  VALUE 'OF'.                    
013100     03  FILLER                  PIC X(1)  VALUE SPACE.                   
013200     03  WS-LABEL-TO             PIC Z(2).                                
013300     03  FILLER                  PIC X(1)  VALUE ')'.                     
013400                                                                          
013500 01  WS-ADPLATS-ORD              PIC X(5).                                
013600 01  FILLER  REDEFINES WS-ADPLATS-ORD.                                    
013700     03 WS-ADPLATSNR             PIC X(3).                                
013800     03 WS-ADPLNIV               PIC X(2).                                
013900                                                                          
014000 01  SPAR-PRINTER                PIC X(3).                                
014100                                                                          
014200 77  SPAR-KDSS-PLE               PIC X(1).                                
014300 77  SPAR-ADLAGOMR               PIC S9(3)  COMP-3.                       
014400 77  SPAR-RADSKIP                PIC S9(3)  VALUE ZERO  COMP-3.           
014500                                                                          
014600 01  WS-KVAVBART                 PIC  9(6).                               
014700 01  WS-KVAVBART-MRP             PIC  9(6).                               
014800                                                                          
014900 01  WS-DATUM                    PIC  X(6).                               
015000                                                                          
015100 01  WS-TIPRTDAT.                                                         
015200     03 WS-TIMM                  PIC 9(2).                                
015300     03 WS-HYPEN                 PIC X(1) VALUE '-'.                      
015400     03 WS-CENTURY               PIC X(2) VALUE '20'.                     
015500     03 WS-TIYY                  PIC 9(2).                                
015600                                                                          
015700 01  WS-KLOCKAN.                                                          
015800     03 WS-TIHHMM                PIC 9(4).                                
015900     03 FILLER                   PIC X(4).                                
016000                                                                          
016100 01  WS-CURRENT-DATE-TIME.                                                
016200     03  WS-YEAR                 PIC 9(4).                                
016300     03  WS-MONTH                PIC 9(2).                                
016400     03  WS-DAY                  PIC 9(2).                                
016500     03  WS-HOUR                 PIC 9(2).                                
016600     03  WS-MINUTE               PIC 9(2).                                
016700 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
016800     03  FILLER                  PIC X(2).                                
016900     03  WS-TIYYMMDDHHMM         PIC X(10).                               
017000                                                                          
017100 01  WS-DCUSER.                                                           
017200     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
017300     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
017400     03 FILLER                   PIC X(1)   VALUE SPACE.                  
017500                                                                          
017600 01  WS-BERADREF                 PIC X(10).                               
017700 01  FILLER  REDEFINES WS-BERADREF.                                       
017800     03 WS-ADLAGOMR              PIC X(2).                                
017900     03 WS-ADGANG                PIC X(2).                                
018000     03 WS-ADPLATS               PIC X(5).                                
018100     03 FILLER                   PIC X.                                   
018200                                                                          
018300 01  WS-BERADREF-RED             PIC X(10).                               
018400 01  FILLER  REDEFINES WS-BERADREF-RED.                                   
018500     03 WS-ADLAGOMR-RED          PIC X(2).                                
018600     03 FILLER                   PIC X(1).                                
018700     03 WS-ADGANG-RED            PIC X(2).                                
018800     03 WS-ADPLATS-RED           PIC Z(4)9.                               
018900                                                                          
019000 01  WS-BERADREF-RED-US          PIC X(10).                               
019100 01  FILLER  REDEFINES WS-BERADREF-RED-US.                                
019200     03 WS-ADLAGOMR-RED-US       PIC X(2).                                
019300     03 FILLER                   PIC X(1).                                
019400     03 WS-ADGANG-RED-US         PIC X(2).                                
019500     03 WS-ADPLATS-RED-US        PIC Z(4)9.                               
019600                                                                          
019700 01  WS-ADPLATS-X6               PIC X(6).                                
019800 01  FILLER  REDEFINES WS-ADPLATS-X6.                                     
019900     03 WS-ADPLATS-X4            PIC X(4).                                
020000     03 WS-SPACE                 PIC X.                                   
020100     03 WS-ADPLATS-X             PIC X.                                   
020200                                                                          
020300 01  WS-SPAR-TOTALRADER.                                                  
020400     03  WS-SPAR-TOTALRAD        PIC X(50)  OCCURS 3.                     
020500                                                                          
020600 01  REANTPSA-OUT-LINE       PIC X(6)   VALUE SPACE.                      
020700 01  FILLER REDEFINES REANTPSA-OUT-LINE.                                  
020800   03 REANTPSA               PIC Z9(1).9(3).                              
020900*                                                                         
021000 01  MRP-KIT-TAB.                                                         
021100     03  FILLER    OCCURS 300.                                            
021200         05 MRP-TAB-REANTPSA   PIC 9(2)V9(3).                             
021300         05 MRP-TAB-BEART      PIC X(15).                                 
021400         05 MRP-TAB-KDSORT     PIC X(05).                                 
021500                                                                          
021600 77  ALLT-SW                     PIC X.                                   
021700     88  ALLT-OK                             VALUE 'J'.                   
021800     88  ALLT-FEL                            VALUE 'N'.                   
021900                                                                          
022000 77  PLOCK-SW                    PIC X.                                   
022100     88  PLOCKSATS-EJ-KLAR                   VALUE 'N'.                   
022200     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
022300                                                                          
022400 77  PLE-SW                      PIC X.                                   
022500     88  PLE-CLOSE                           VALUE 'N'.                   
022600     88  PLE-OPEN                            VALUE 'J'.                   
022700                                                                          
022800 77  PRINTER-SW                  PIC X.                                   
022900     88  EJ-NY-PRINTER                       VALUE 'N'.                   
023000     88  NY-PRINTER                          VALUE 'J'.                   
023100                                                                          
023200 77  SKRIVKLAR-SW                PIC X.                                   
023300     88  SKRIVKLAR                           VALUE 'J'.                   
023400                                                                          
023500 77  TOTAL-SW                    PIC X.                                   
023600     88  TOTAL-EJ-SKRIVEN                    VALUE 'N'.                   
023700     88  TOTAL-SKRIVEN                       VALUE 'J'.                   
023800                                                                          
023900 77  TOT-FORSTA-SW               PIC X.                                   
024000     88  FORSTA-TOTETIK                      VALUE 'J'.                   
024100                                                                          
024200 77  SKRIVARTYP-SW               PIC X.                                   
024300     88  LASER-SKRIVARE                      VALUE 'J'.                   
024400     88  MATRIS-SKRIVARE                     VALUE 'N'.                   
024500                                                                          
024600 77  FIRST-TIME-SW               PIC X.                                   
024700     88  FIRST-TIME                          VALUE 'J'.                   
024800                                                                          
024900 77  FIRST-TIME-KIT-SW           PIC X.                                   
025000     88  FIRST-TIME-KIT                      VALUE 'J'.                   
025100                                                                          
025200 77  KIT-STATUS-SW               PIC X.                                   
025300     88  KIT-STATUS-OK                       VALUE 'J'.                   
025400                                                                          
025500 77  LESS-THAN-TEN-SW            PIC X.                                   
025600     88  LESS-THAN-TEN                       VALUE 'J'.                   
025700                                                                          
025800*    -- SWITCH FÖR WZ01SEND                                               
025900 77  SEND-OPEN-SW                PIC X       VALUE 'N'.                   
026000     88  SEND-NOT-OPEN                       VALUE 'N'.                   
026100     88  SEND-OPEN                           VALUE 'J'.                   
026200                                                                          
026300*    -- SWITCH FÖR WZ01SEND TO MRP STICKERS                               
026400 77  SEND-MRP-OPEN-SW                PIC X       VALUE 'N'.               
026500     88  SEND-MRP-NOT-OPEN                       VALUE 'N'.               
026600     88  SEND-MRP-OPEN                           VALUE 'J'.               
026700                                                                          
026800*    -- SWITCH FÖR WZ01SEND TO MRP STICKERS                               
026900 77  SEND-MRP-KIT-OPEN-SW            PIC X       VALUE 'N'.               
027000     88  SEND-MRP-KIT-NOT-OPEN                   VALUE 'N'.               
027100     88  SEND-MRP-KIT-OPEN                       VALUE 'J'.               
027200                                                                          
027300*    -- SWITCH FÖR WDJ411 DB                                              
027400 77  SW-WDJ4-DATA                    PIC X       VALUE 'N'.               
027500     88  WDJ4-DATA                               VALUE 'J'.               
027600                                                                          
027700*01  -COPY WWPRODSL                                                       
027800                                                                          
027900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
028000                                                                          
028100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
028200     88  GODK-MID                            VALUE '4375' '4376'.         
028300*                                                                         
028400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
028500 01  GENERELLA-SUBPROGRAM.                                                
028600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
028900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
029000     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
029100     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
029200     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
029300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
029400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
029500     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
029600     03  W488ORCR                PIC X(8)    VALUE 'W488ORCR'.            
029700     EJECT                                                                
029800* VARIABLER TILL SUBPROGRAM W400ARTU                                      
029900*    --- PARAMETERS FOR SUB PROGRAM W488ORCR                              
030000 01  FILLER                      PIC X(16)   VALUE 'W488ORCR'.            
030100*01 -COPY W488ORCR                                                        
030200     EJECT                                                                
030300*01  -COPY W400ARTU                                                       
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
030600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
030700*01 -COPY WMSGINIT                                                        
030800     EJECT                                                                
030900 01  FILLER                      PIC X(16)  VALUE 'SVERIGE-DIST'.         
031000*   -COPY WWDIST13                                                        
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)  VALUE 'SDC-DISTRIKT'.         
031300*   -COPY WWDIST34                                                        
031400     EJECT                                                                
031500 01  FILLER                      PIC X(16)  VALUE 'REFILLDISTR'.          
031600*   -COPY WWDIST35                                                        
031700     EJECT                                                                
031800*    --- AREA FÖR SUBPROGRAM W006PRR1                                     
031900*                                                                         
032000 01  FILLER                      PIC X(16)  VALUE 'W006PRR1'.             
032100                                                                          
032200*01  -COPY W006PRAR                                                       
032300*                                                                         
032400 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
032500*   -COPY W006PRT                                                         
032600*                                                                         
032700 01  FILLER                      PIC X(16)  VALUE 'W510CURR '.            
032800*   -COPY W510CURR                                                        
032900*                                                                         
033000     SKIP3                                                                
033100******************************************************************        
033200 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-DLI'.          
033300*                                                                         
033400 01  WS-PLE-AREA.                                                         
033500     03  WS-IDPRTLST.                                                     
033600        05 WS-SYSTDEL            PIC X(1).                                
033700        05 WS-LISTTYP            PIC X(2).                                
033800        05 WS-KDPRT              PIC X(3).                                
033900        05 FILLER                PIC X(2)    VALUE SPACE.                 
034000     03 WS-PLE-LISTID.                                                    
034100        05 WS-PLE-IDPRC          PIC X(4).                                
034200        05 WS-PLE-STRECK         PIC X(1).                                
034300        05 WS-PLE-IDLOPNR        PIC X(5).                                
034400     03 WS-MRP-LISTID.                                                    
034500        05 WS-MRP-IDPRC          PIC X(4).                                
034600        05 WS-MRP-STRECK         PIC X(1).                                
034700        05 WS-MRP-IDLOPNR        PIC X(5).                                
034800     03 WS-PLE-LISTRAD.                                                   
034900        05 WS-PLE-RAD            PIC X(132).                              
035000     03 WS-DUMMY                 PIC X(1).                                
035100     SKIP3                                                                
035200*                                                                         
035300 01  ARB-RAD-AREA.                                                        
035400     03 ARB-RAD        OCCURS 3.                                          
035500        05 ARB-RAD-V             PIC X(50).                               
035600        05 FILLER                PIC X(03)  VALUE SPACE.                  
035700        05 ARB-RAD-H             PIC X(50).                               
035800        05 FILLER                PIC X(29)  VALUE SPACE.                  
035900*                                                                         
036000 01  NDC-RAD-AREA.                                                        
036100     03 NDC-RAD        OCCURS 3.                                          
036200        05 NDC-RAD-V             PIC X(50).                               
036300        05 FILLER                PIC X(01)  VALUE SPACE.                  
036400        05 NDC-RAD-H             PIC X(50).                               
036500        05 FILLER                PIC X(31)  VALUE SPACE.                  
036600*                                                                         
036700 01  JAP-RAD-AREA.                                                        
036800     03 JAP-RAD        OCCURS 3.                                          
036900        05 JAP-RAD-V             PIC X(50).                               
037000        05 FILLER                PIC X(03)  VALUE SPACE.                  
037100        05 JAP-RAD-H             PIC X(50).                               
037200        05 FILLER                PIC X(29)  VALUE SPACE.                  
037300     EJECT                                                                
037400*    --- MID-AREA                                                         
037500*                                                                         
037600 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
037700                                                                          
037800*01  -COPY W4I37601                                                       
037900     EJECT                                                                
038000*    --- AREOR FÖR LASERBLANKETT                                          
038100*                                                                         
038200 01  FILLER                      PIC X(16)  VALUE 'W4037601'.             
038300                                                                          
038400*01  -COPY W4037601                                                       
038500     EJECT                                                                
038600*                                                                         
038700 01  FILLER                      PIC X(16)  VALUE 'W4037602'.             
038800                                                                          
038900*01  -COPY W4037602                                                       
039000     EJECT                                                                
039100*                                                                         
039200 01  FILLER                      PIC X(16)  VALUE 'WNDCADRE '.            
039300*   -COPY WNDCADRE                                                        
039400     EJECT                                                                
039500*    --- AREOR FÖR MSG-HANTERING                                          
039600*                                                                         
039700 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
039800                                                                          
039900*01  -COPY WMSGAREA                                                       
040000     EJECT                                                                
040100 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
040200 01  P-TO-P-SW1.                                                          
040300     03  PTOP1-LL                PIC S9(4)   VALUE 34 COMP SYNC.          
040400     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
040500     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
040600     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T376X'.             
040700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
040800     03  FILLER                  PIC  X(4)   VALUE '4376'.                
040900     03  PTOP1-KDMFSFOR          PIC  X(1).                               
041000     03  -COPY W4I37601  -PRE PTOP1-                                      
041100     EJECT                                                                
041200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
041300*                                                                         
041400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
041500                                                                          
041600 01  NYCKLAR-TILL-DLI.                                                    
041700*----> KUNDREG.                                                           
041800                                                                          
041900*----> PLOCKSATS.                                                         
042000                                                                          
042100     03  W-4001-IDHTYP-X.                                                 
042200         05  W-4001-IDHTYP       PIC  X(4)  VALUE '4003'.                 
042300         05  W-4001-IDPRODNR     PIC  9(7).                               
042400         05  W-4001-IDPLKLST     PIC  9(3).                               
042500         05  W-4001-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
042600                                                                          
042700*----> DIREKTNYCKEL TILL PLE-RADEN.                                       
042800                                                                          
042900     03  W-4006-IDHTYP-X.                                                 
043000         05  W-4006-KDPRT        PIC  X(3).                               
043100         05  W-4006-KDSS-PLE     PIC  X(1).                               
043200         05  W-4006-ADLAGOMR     PIC S9(3) COMP-3.                        
043300         05  W-4006-ADGANG       PIC S9(3) COMP-3.                        
043400         05  W-4006-ADPLATS      PIC S9(5) COMP-3.                        
043500         05  W-4006-IDARTNR      PIC S9(9) COMP-3.                        
043600         05  W-4006-IDLOPNR      PIC S9(3) COMP-3.                        
043700                                                                          
043800     03  W-4006-MAX-KDPRT        PIC X(3)   VALUE '999'.                  
043900                                                                          
044000     03  W-IDDC-B6-X.                                                     
044100         05 W-IDDC-B6            PIC X(2).                                
044200                                                                          
044300     03  W-WDC301KY-X.                                                    
044400         05  W-IDARTNR-WDC3      PIC S9(9)   COMP-3 VALUE ZERO.           
044500     03  W-WDC311KY-X.                                                    
044600         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
044700                                                                          
044800*----> SEKUNDÄR INDEX ARTIKELBENÄMNING                                    
044900                                                                          
045000     03  W-WDD3BSEQ-X.                                                    
045100         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
045200                                                                          
045300     03  W-IDSKYLT-X             PIC X(3).                                
045400                                                                          
045500     03 W-BEART-X.                                                        
045600         05 W-BEART          PIC  X(25)  VALUE SPACE.                     
045700*NEW                                                                      
045800**   WDJ401-KDP KIT PART                                                  
045900 01      W-IDARTNR-SATS-X.                                                
046000   03    W-IDARTNR-SATS  PIC S9(9)   VALUE ZERO  COMP-3.                  
046100                                                                          
046200**   ARTIKEL-CLAGER INFO                                                  
046300 01      W-IDARTNR-X.                                                     
046400   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
046500                                                                          
046600 01      W-KDSEGKEY-X.                                                    
046700   03    W-KDSEGKEY      PIC X(1)    VALUE '1'.                           
046800                                                                          
046900*    --- STATUS-KOD FRÅN IMS                                              
047000 01  FILLER                      PIC X(08)   VALUE 'STATUSWS'.            
047100                                                                          
047200 01  STATUS-WS                   PIC  X(02).                              
047300     88  SEGMENT-FINNS                       VALUE '  '.                  
047400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
047500     88  END-OF-DATA                         VALUE 'GB'.                  
047600     SKIP2                                                                
047700 01  GODK-STATUSKODER.                                                    
047800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047900                                                                          
048000 01  FILLER                      PIC X(08)   VALUE 'SSA-AREA'.            
048100*                                                                         
048200 01  SSA1                        PIC X(192).                              
048300 01  SSA2                        PIC X(192).                              
048400 01  SSA3                        PIC X(192).                              
048500     EJECT                                                                
048600*********************************                                         
048700*RADER FÖR PLOCKETIKETT         *                                         
048800*********************************                                         
048900 01  PLE-RAD1.                                                            
049000     03   RAD1-ADLAGOMR           PIC Z9.                                 
049100     03   FILLER  REDEFINES RAD1-ADLAGOMR.                                
049200          05 RAD1-ADLAGOMR-LDC-SE PIC 99.                                 
049300     03   RAD1-ADGANG             PIC Z9.                                 
049400     03   FILLER  REDEFINES RAD1-ADGANG.                                  
049500          05 RAD1-ADGANG-LDC-SE   PIC 99.                                 
049600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
049700     03   RAD1-ADPLATS            PIC X(6).                               
049800     03   RAD1-FLAKPLOC           PIC X(1).                               
049900     03   RAD1-IDARTNR            PIC Z(7)9.                              
050000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
050100     03   RAD1-BEART              PIC X(15).                              
050200     03   RAD1-KVAVBART           PIC Z(6).                               
050300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
050400     03   RAD1-GRP.                                                       
050500         05  RAD1-KDSORT         PIC X(2).                                
050600         05  FILLER-RAD1-GRP     PIC X(1)   VALUE SPACE.                  
050700         05  RAD1-KDARTURS       PIC X(2).                                
050800*                                                                         
050900 01  PLE-RAD2.                                                            
051000     03  RAD2-BERADREF-GRP.                                               
051100         05  FILLER              PIC X(1)   VALUE SPACE.                  
051200         05  RAD2-RUBRADREF      PIC X(4).                                
051300         05  RAD2-BERADREF       PIC X(10).                               
051400         05  RAD2-COPY           PIC X(6).                                
051500     03  RAD2-BERADREF-GRP-US REDEFINES RAD2-BERADREF-GRP.                
051600         05  RAD2-RUBRADREF-US   PIC X(1).                                
051700         05  RAD2-BERADREF-US    PIC X(10).                               
051800         05  FILLER1             PIC X(4).                                
051900         05  RAD2-COPY-US        PIC X(6).                                
052000     03  RAD2-BERADREF-GRP-DEPT REDEFINES RAD2-BERADREF-GRP.              
052100         05  FILLER              PIC X(1).                                
052200         05  RAD2-BERADREF-DEPT  PIC X(10).                               
052300         05  RAD2-FILLER-DEPT    PIC X(1).                                
052400         05  RAD2-IDDEPT-PRE-D   PIC X(1).                                
052500         05  RAD2-IDDEPT         PIC X(2).                                
052600         05  RAD2-COPY-DEPT      PIC X(6).                                
052700*                                                                         
052800     03  KDARTHNT-GRP.                                                    
052900         05  RAD2-KDARTHNT       PIC Z(5)9.                               
053000     03  IDZON-GRP  REDEFINES KDARTHNT-GRP.                               
053100         05  FILLER-ZON          PIC X(4).                                
053200         05  RAD2-IDZON          PIC X(2).                                
053300*                                                                         
053400     03  FILLER                  PIC X.                                   
053500     03  KDFARLIG-GRP.                                                    
053600         05  FILLER-FARL         PIC X(2)   VALUE SPACE.                  
053700         05  RAD2-KDFARLIG       PIC Z(1).                                
053800     03  IDPSN-GRP  REDEFINES KDFARLIG-GRP.                               
053900         05  RAD2-IDPSN          PIC Z(3).                                
054000*                                                                         
054100     03  FILLER                  PIC X(6)   VALUE SPACE.                  
054200     03  RAD2-KDEMBAL            PIC X(1).                                
054300     03  FILLER                  PIC X(2)   VALUE SPACE.                  
054400     03  RAD2-IDSPECEMB          PIC X(6)   VALUE SPACE.                  
054500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
054600     03  RAD2-IDBORD             PIC X(3).                                
054700                                                                          
054800 01  PLE-RAD3.                                                            
054900     03   RAD3-IDDISTR            PIC Z(3)9.                              
055000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
055100     03   RAD3-IDKUNDNR           PIC Z(5)9.                              
055200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
055300     03   RAD3-IDPRODNR           PIC Z(6)9.                              
055400     03   FILLER                  PIC X(1)  VALUE '-'.                    
055500     03   RAD3-IDPLKLST           PIC 9(3).                               
055600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
055700     03   RAD3-IDKUNDRF           PIC X(7).                               
055800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
055900     03   RAD3-IDRADNR            PIC Z(3)9.                              
056000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
056100     03   RAD3-KDORDKL            PIC 9.                                  
056200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
056300     03   RAD3-IDPRC              PIC X(4).                               
056400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
056500     03   RAD3-IDLOPNR-PL         PIC Z(2)9.                              
056600     03   RAD3-STAR               PIC X(1).                               
056700     03   RAD3-IDLOPNR-ORD        PIC X(2).                               
056800                                                                          
056900*************************************************************             
057000*NDC ETIKETT-RADER                                                        
057100*************************************************************             
057200                                                                          
057300 01  PLE-RAD1-NDC.                                                        
057400     03   RAD1-ADLAGOMR-NDC       PIC 99.                                 
057500     03   RAD1-ADGANG-NDC         PIC 99.                                 
057600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
057700     03   RAD1-ADPLATS-NDC        PIC 9(5).                               
057800     03   RAD1-FLAKPLOC-NDC       PIC X(1).                               
057900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
058000     03   RAD1-IDARTNR-NDC        PIC Z(7)9.                              
058100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
058200     03   RAD1-BEART-NDC          PIC X(14).                              
058300     03   RAD1-KVAVBART-NDC       PIC Z(6)9.                              
058400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
058500     03  KDSORTNDC-GRP.                                                   
058600         05  RAD1-KDSORT-NDC     PIC X(5).                                
058700*                                                                         
058800 01  PLE-RAD2-NDC.                                                        
058900     03  RAD2-BERADREF-NDC       PIC X(10).                               
059000     03  FILLER                  PIC X      VALUE SPACE.                  
059100     03  RAD2-COPY-NDC           PIC X(7).                                
059200     03  RAD2-BEARTURS-NDC       PIC X(15).                               
059300     03  FILLER                  PIC X(2)   VALUE SPACE.                  
059400     03  RAD2-IDPSN-NDC          PIC Z(3).                                
059500     03  FILLER                  PIC X(3)   VALUE SPACE.                  
059600     03  RAD2-IDSPECEMB-NDC      PIC Z(4).                                
059700     03  FILLER                  PIC X(2)   VALUE SPACE.                  
059800     03  RAD2-IDBORD-NDC         PIC X(3).                                
059900                                                                          
060000 01  PLE-RAD3-NDC.                                                        
060100     03   RAD3-IDDISTR-NDC        PIC Z(3)9.                              
060200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
060300     03   RAD3-IDKUNDNR-NDC       PIC Z(5)9.                              
060400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
060500     03   RAD3-IDPRODNR-NDC       PIC Z(6)9.                              
060600     03   FILLER                  PIC X(1)  VALUE '-'.                    
060700     03   RAD3-IDPLKLST-NDC       PIC 9(3).                               
060800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
060900     03   RAD3-IDKUNDRF-NDC       PIC X(7).                               
061000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
061100     03   RAD3-IDRADNR-NDC        PIC Z(3)9.                              
061200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
061300     03   RAD3-KDORDKL-NDC        PIC 9.                                  
061400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
061500     03   RAD3-IDPRC-NDC          PIC X(4).                               
061600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
061700     03   RAD3-IDLOPNR-PL-NDC     PIC Z(2)9.                              
061800     03   RAD3-IDLOPNR-ORD-NDC    PIC Z(2).                               
061900*************************************************************             
062000*JAPAN  ETIKETT-RADER                                                     
062100*************************************************************             
062200                                                                          
062300 01  PLE-RAD1-JAP.                                                        
062400     03   RAD1-ADLAGOMR-JAP       PIC 99.                                 
062500     03   RAD1-ADGANG-JAP         PIC 99.                                 
062600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
062700     03   RAD1-ADPLATS-JAP        PIC 9(5).                               
062800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
062900     03   RAD1-FLAKPLOC-JAP       PIC X(1).                               
063000     03   RAD1-IDARTNR-JAP        PIC Z(7)9.                              
063100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
063200     03   RAD1-BEART-JAP          PIC X(15).                              
063300     03   RAD1-KVAVBART-JAP       PIC Z(6)9.                              
063400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
063500     03  KDSORTJAP-GRP.                                                   
063600         05  RAD1-KDSORT-JAP     PIC X(5)   VALUE SPACE.                  
063700*                                                                         
063800 01  PLE-RAD2-JAP.                                                        
063900     03  RAD2-BERADREF-JAP       PIC X(10).                               
064000     03  FILLER                  PIC X      VALUE SPACE.                  
064100     03  RAD2-COPY-JAP           PIC X(7).                                
064200     03  RAD2-BEARTURS-JAP       PIC X(15).                               
064300     03  FILLER                  PIC X(2)   VALUE SPACE.                  
064400     03  RAD2-IDPSN-JAP          PIC Z(3).                                
064500     03  FILLER                  PIC X(3)   VALUE SPACE.                  
064600     03  RAD2-IDSPECEMB-JAP      PIC Z(4).                                
064700     03  FILLER                  PIC X(2)   VALUE SPACE.                  
064800     03  RAD2-IDBORD-JAP         PIC X(3).                                
064900                                                                          
065000 01  PLE-RAD3-JAP.                                                        
065100     03   RAD3-IDDISTR-JAP        PIC Z(3)9.                              
065200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
065300     03   RAD3-IDKUNDNR-JAP       PIC Z(5)9.                              
065400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
065500     03   RAD3-IDPRODNR-JAP       PIC Z(6)9.                              
065600     03   FILLER                  PIC X(1)  VALUE '-'.                    
065700     03   RAD3-IDPLKLST-JAP       PIC 9(3).                               
065800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
065900     03   RAD3-IDKUNDRF-JAP       PIC X(7).                               
066000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
066100     03   RAD3-IDRADNR-JAP        PIC Z(3)9.                              
066200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
066300     03   RAD3-KDORDKL-JAP        PIC 9.                                  
066400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
066500     03   RAD3-IDPRC-JAP          PIC X(4).                               
066600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
066700     03   RAD3-IDLOPNR-PL-JAP     PIC Z(2)9.                              
066800     03   RAD3-IDLOPNR-ORD-JAP    PIC Z(2).                               
066900                                                                          
067000******************************************************************        
067100*SVE                                                                      
067200 01  PLE-TOTRAD1-SVE.                                                     
067300     03   FILLER                  PIC X(13) VALUE '** TOTAL FÖR '.        
067400     03   FILLER                  PIC X(10) VALUE 'PLOCKSATS '.           
067500     03   TOTRAD1-IDPRC-SVE       PIC X(4).                               
067600     03   FILLER                  PIC X(1)  VALUE '-'.                    
067700     03   TOTRAD1-IDLOPNR-PL-SVE  PIC 9(3).                               
067800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
067900     03   FILLER                  PIC X(12) VALUE 'ANTAL RADER '.         
068000     03   TOTRAD1-ANTAL-SVE       PIC Z(3)9.                              
068100                                                                          
068200 01  PLE-TOTRAD2-SVE.                                                     
068300     03   FILLER                  PIC X(15)                               
068400                            VALUE 'UTSKRIFTSDATUM '.                      
068500     03   TOTRAD2-DATUM-SVE       PIC X(6).                               
068600     03   FILLER                  PIC X(1) VALUE SPACE.                   
068700     03   TOTRAD2-TIHH-SVE        PIC X(2).                               
068800     03   FILLER                  PIC X(1) VALUE ':'.                     
068900     03   TOTRAD2-TIMM-SVE        PIC X(2).                               
069000                                                                          
069100 01  PLE-TOTRAD3-SVE.                                                     
069200     03   FILLER                  PIC X(6) VALUE 'ANTAL'.                 
069300     03   TOTRAD3-LAGOMRADE-SVE.                                          
069400       05 FILLER                  OCCURS 5.                               
069500          07 TOTRAD3-ADLAGOMR-SVE PIC ZZB.                                
069600          07 TOTRAD3-ANTAL-SVE    PIC ZZZ.                                
069700          07 TOTRAD3-KOMMA-SVE    PIC X(2) VALUE SPACE.                   
069800     EJECT                                                                
069900*ENG                                                                      
070000 01  PLE-TOTRAD1-ENG.                                                     
070100     03   FILLER                  PIC X(4)  VALUE 'PRC'.                  
070200     03   TOTRAD1-IDPRC-ENG       PIC X(5).                               
070300     03   FILLER              PIC X(09) VALUE '*TOT QTY.'.                
070400     03   FILLER              PIC X(12) VALUE 'PICKING UNIT'.             
070500     03   TOTRAD1-IDLOPNR-PL-ENG  PIC Z(2)9.                              
070600     03   FILLER                  PIC X     VALUE SPACE.                  
070700     03   FILLER                  PIC X(10)                               
070800                            VALUE 'TOT LINES:'.                           
070900     03   TOTRAD1-ANTAL-ENG       PIC Z(3)9.                              
071000                                                                          
071100 01  PLE-TOTRAD2-ENG.                                                     
071200     03   FILLER                  PIC X(15)                               
071300                            VALUE 'PRINT DATE:   '.                       
071400     03   TOTRAD2-DATUM-ENG       PIC X(6).                               
071500     03   FILLER                  PIC X(1) VALUE SPACE.                   
071600     03   TOTRAD2-TIHH-ENG        PIC X(2).                               
071700     03   FILLER                  PIC X(1) VALUE ':'.                     
071800     03   TOTRAD2-TIMM-ENG        PIC X(2).                               
071900                                                                          
072000 01  PLE-TOTRAD3-ENG.                                                     
072100     03   FILLER                  PIC X(8) VALUE 'QUANTITY'.              
072200     03   TOTRAD3-LAGOMRADE-ENG.                                          
072300       05 FILLER                  OCCURS 5.                               
072400          07 TOTRAD3-ADLAGOMR-ENG PIC ZZB.                                
072500          07 TOTRAD3-ANTAL-ENG    PIC ZZZ.                                
072600          07 TOTRAD3-KOMMA-ENG    PIC X(2) VALUE SPACE.                   
072700*FRA                                                                      
072800 01  PLE-TOTRAD1-FRA.                                                     
072900     03   FILLER                  PIC X(4)  VALUE 'PRC'.                  
073000     03   TOTRAD1-IDPRC-FRA       PIC X(5).                               
073100     03   FILLER              PIC X(09) VALUE '*TOT.ALL.'.                
073200     03   FILLER              PIC X(12) VALUE 'PICKING UNIT'.             
073300     03   TOTRAD1-IDLOPNR-PL-FRA  PIC Z(2)9.                              
073400     03   FILLER                  PIC X     VALUE SPACE.                  
073500     03   FILLER                  PIC X(10)                               
073600                            VALUE 'TOT LINES:'.                           
073700     03   TOTRAD1-ANTAL-FRA       PIC Z(3)9.                              
073800                                                                          
073900 01  PLE-TOTRAD2-FRA.                                                     
074000     03   FILLER                  PIC X(15)                               
074100                            VALUE 'DATE EDITION:  '.                      
074200     03   TOTRAD2-DATUM-FRA       PIC X(6).                               
074300     03   FILLER                  PIC X(1) VALUE SPACE.                   
074400     03   TOTRAD2-TIHH-FRA        PIC X(2).                               
074500     03   FILLER                  PIC X(1) VALUE ':'.                     
074600     03   TOTRAD2-TIMM-FRA        PIC X(2).                               
074700                                                                          
074800 01  PLE-TOTRAD3-FRA.                                                     
074900     03   FILLER                  PIC X(8) VALUE 'ALLOUEE '.              
075000     03   TOTRAD3-LAGOMRADE-FRA.                                          
075100       05 FILLER                  OCCURS 5.                               
075200          07 TOTRAD3-ADLAGOMR-FRA PIC ZZB.                                
075300          07 TOTRAD3-ANTAL-FRA    PIC ZZZ.                                
075400          07 TOTRAD3-KOMMA-FRA    PIC X(2) VALUE SPACE.                   
075500*ITA                                                                      
075600 01  PLE-TOTRAD1-ITA.                                                     
075700     03   FILLER                  PIC X(21)                               
075800                             VALUE 'CANALE DI PRODUZIONE:'.               
075900     03   TOTRAD1-IDPRC-ITA       PIC X(5).                               
076000                                                                          
076100 01  PLE-TOTRAD2-ITA.                                                     
076200     03   FILLER                  PIC X(23)                               
076300                            VALUE 'TOTALE DI PICKING UNIT:'.              
076400     03   TOTRAD2-IDLOPNR-PL-ITA  PIC Z(2)9.                              
076500     03   FILLER                  PIC X     VALUE SPACE.                  
076600     03   FILLER                  PIC X(13)                               
076700                            VALUE 'TOTALE LINEA '.                        
076800     03   TOTRAD2-ANTAL-ITA       PIC Z(3)9.                              
076900                                                                          
077000 01  PLE-TOTRAD3-ITA.                                                     
077100     03   FILLER                  PIC X(14)                               
077200                            VALUE 'DATA STAMPA:  '.                       
077300     03   TOTRAD3-DATUM-ITA       PIC X(6).                               
077400     03   FILLER                  PIC X(1) VALUE SPACE.                   
077500     03   TOTRAD3-TIHH-ITA        PIC X(2).                               
077600     03   FILLER                  PIC X(1) VALUE ':'.                     
077700     03   TOTRAD3-TIMM-ITA        PIC X(2).                               
077800                                                                          
077900 01  PLE-TOTRAD4-ITA.                                                     
078000     03   FILLER                  PIC X(8) VALUE 'QUANTITY'.              
078100     03   TOTRAD4-LAGOMRADE-ITA.                                          
078200       05 FILLER                  OCCURS 5.                               
078300          07 TOTRAD4-ADLAGOMR-ITA PIC ZZB.                                
078400          07 TOTRAD4-ANTAL-ITA    PIC ZZZ.                                
078500          07 TOTRAD4-KOMMA-ITA    PIC X(2) VALUE SPACE.                   
078600*SPA                                                                      
078700 01  PLE-TOTRAD1-SPA.                                                     
078800     03   FILLER                  PIC X(4)  VALUE 'PRC'.                  
078900     03   TOTRAD1-IDPRC-SPA       PIC X(5).                               
079000     03   FILLER              PIC X(34)                                   
079100           VALUE '*CANTIDAD TOT. POR UNIDAD PICKING:'.                    
079200     03   TOTRAD1-IDLOPNR-PL-SPA  PIC Z(2)9.                              
079300     03   FILLER                  PIC X     VALUE SPACE.                  
079400                                                                          
079500 01  PLE-TOTRAD2-SPA.                                                     
079600     03   FILLER                  PIC X(11)                               
079700                            VALUE 'TOT LINEAS:'.                          
079800     03   TOTRAD1-ANTAL-SPA       PIC Z(3)9.                              
079900     03   FILLER                  PIC X(15)                               
080000                            VALUE '   *FECHA IMPR:'.                      
080100     03   TOTRAD2-DATUM-SPA       PIC X(6).                               
080200     03   FILLER                  PIC X(1) VALUE SPACE.                   
080300     03   TOTRAD2-TIHH-SPA        PIC X(2).                               
080400     03   FILLER                  PIC X(1) VALUE ':'.                     
080500     03   TOTRAD2-TIMM-SPA        PIC X(2).                               
080600                                                                          
080700 01  PLE-TOTRAD3-SPA.                                                     
080800     03   FILLER                  PIC X(8) VALUE 'CANTIDAD'.              
080900     03   TOTRAD3-LAGOMRADE-SPA.                                          
081000       05 FILLER                  OCCURS 5.                               
081100          07 TOTRAD3-ADLAGOMR-SPA PIC ZZB.                                
081200          07 TOTRAD3-ANTAL-SPA    PIC ZZZ.                                
081300          07 TOTRAD3-KOMMA-SPA    PIC X(2) VALUE SPACE.                   
081400*OST                                                                      
081500 01  PLE-TOTRAD1-OST.                                                     
081600     03   FILLER                  PIC X(4)  VALUE 'PRC'.                  
081700     03   TOTRAD1-IDPRC-OST       PIC X(5).                               
081800     03   FILLER              PIC X(25)                                   
081900           VALUE '**TOTAL FUR PFLUCK SATZ: '.                             
082000     03   TOTRAD1-IDLOPNR-PL-OST  PIC Z(2)9.                              
082100     03   FILLER                  PIC X     VALUE SPACE.                  
082200                                                                          
082300 01  PLE-TOTRAD2-OST.                                                     
082400     03   FILLER                  PIC X(11)                               
082500                            VALUE 'TOT ZEILEN:'.                          
082600     03   TOTRAD1-ANTAL-OST       PIC Z(3)9.                              
082700     03   FILLER                  PIC X(15)                               
082800                            VALUE '*DRUCK DATUM  :'.                      
082900     03   TOTRAD2-DATUM-OST       PIC X(6).                               
083000     03   FILLER                  PIC X(1) VALUE SPACE.                   
083100     03   TOTRAD2-TIHH-OST        PIC X(2).                               
083200     03   FILLER                  PIC X(1) VALUE ':'.                     
083300     03   TOTRAD2-TIMM-OST        PIC X(2).                               
083400                                                                          
083500 01  PLE-TOTRAD3-OST.                                                     
083600     03   FILLER                  PIC X(9) VALUE 'QUANTITÄT'.             
083700     03   TOTRAD3-LAGOMRADE-OST.                                          
083800       05 FILLER                  OCCURS 5.                               
083900          07 TOTRAD3-ADLAGOMR-OST PIC ZZB.                                
084000          07 TOTRAD3-ANTAL-OST    PIC ZZZ.                                
084100          07 TOTRAD3-KOMMA-OST    PIC X(2) VALUE SPACE.                   
084200*NDC-JAPAN                                                                
084300 01  PLE-TOTRAD1-JAP.                                                     
084400     03   FILLER                  PIC X(4)  VALUE 'PRC'.                  
084500     03   TOTRAD1-IDPRC-JAP       PIC X(5).                               
084600     03   FILLER              PIC X(09) VALUE '*TOT QTY.'.                
084700     03   FILLER              PIC X(12) VALUE 'PICKING UNIT'.             
084800     03   TOTRAD1-IDLOPNR-PL-JAP  PIC Z(2)9.                              
084900     03   FILLER                  PIC X     VALUE SPACE.                  
085000     03   FILLER                  PIC X(10)                               
085100                            VALUE 'TOT LINES:'.                           
085200     03   TOTRAD1-ANTAL-JAP       PIC Z(3)9.                              
085300                                                                          
085400 01  PLE-TOTRAD2-JAP.                                                     
085500     03   FILLER                  PIC X(15)                               
085600                            VALUE 'PRINT DATE:   '.                       
085700     03   TOTRAD2-DATUM-JAP       PIC X(6).                               
085800     03   FILLER                  PIC X(1) VALUE SPACE.                   
085900     03   TOTRAD2-TIHH-JAP        PIC X(2).                               
086000     03   FILLER                  PIC X(1) VALUE ':'.                     
086100     03   TOTRAD2-TIMM-JAP        PIC X(2).                               
086200                                                                          
086300 01  PLE-TOTRAD3-JAP.                                                     
086400     03   FILLER                  PIC X(8) VALUE 'QUANTITY'.              
086500     03   TOTRAD3-LAGOMRADE-JAP.                                          
086600       05 FILLER                  OCCURS 5.                               
086700          07 TOTRAD3-ADLAGOMR-JAP PIC ZZB.                                
086800          07 TOTRAD3-ANTAL-JAP    PIC ZZZ.                                
086900          07 TOTRAD3-KOMMA-JAP    PIC X(2) VALUE SPACE.                   
087000**END PRINT LINES**************************************                   
087100     EJECT                                                                
087200 01  LASER-TOTRAD.                                                        
087300     03   ANTAL-TEXT              PIC X(6) VALUE 'ANTAL'.                 
087400     03   LASER-TOTRAD-LAGOMRADE.                                         
087500       05 FILLER                  OCCURS 8.                               
087600          07 LASER-TOTRAD-ADLAGOMR      PIC ZZB.                          
087700          07 LASER-TOTRAD-ANTAL         PIC ZZZ.                          
087800          07 LASER-TOTRAD-KOMMA         PIC X(2) VALUE SPACE.             
087900**END LASER TOTAL LINES*********************************                  
088000     EJECT                                                                
088100*    --- IMS FUNKTIONSKODER                                               
088200*01  -COPY W0003                                                          
088300     EJECT                                                                
088400*    ---  DLI INPUT-OUTPUT AREA                                           
088500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
088600                                                                          
088700 01  DLI-IO-AREA1.                                                        
088800     03  IO-AREA1                PIC X(500) VALUE SPACE.                  
088900                                                                          
089000     03  WL400301 REDEFINES IO-AREA1.                                     
089100*        05  -COPY WDGX4003                                               
089200     EJECT                                                                
089300     03  WL400311 REDEFINES IO-AREA1.                                     
089400*        05  -COPY WDGX4004                                               
089500     EJECT                                                                
089600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
089700                                                                          
089800 01  DLI-IO-AREA2.                                                        
089900     03  WL400321.                                                        
090000*        05  -COPY WDGX4006                                               
090100                                                                          
090200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
090300 01   DLI-IO-AREA-B601.                                                   
090400*     03  -COPY WDB601                                                    
090500     EJECT                                                                
090600*                                                                         
090700 01  FILLER                      PIC X(16) VALUE 'WDC301 AREA'.           
090800 01  DLI-IO-WDC301.                                                       
090900*    05  -COPY WDC301                                                     
091000 01  FILLER                      PIC X(16) VALUE 'WDC311 AREA'.           
091100 01  DLI-IO-WDC311.                                                       
091200*    05  -COPY WDC311                                                     
091300     EJECT                                                                
091400                                                                          
091500*                                                                         
091600 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
091700 01   DLI-IO-WDD311.                                                      
091800*     03  -COPY WDD311                                                    
091900     EJECT                                                                
092000                                                                          
092100                                                                          
092200**   ARTIKEL-CLAGER INFO                                                  
092300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
092400 01  DLI-IO-WDK601.                                                       
092500*  03  -COPY WDK601.                                                      
092600                                                                          
092700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
092800 01  DLI-IO-WDK611.                                                       
092900*  03  -COPY WDK611.                                                      
093000*                                                                         
093100**   KIT PART                                                             
093200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ101'.           
093300 01  DLI-IO-WDJ101.                                                       
093400*  03  -COPY WDJ101.                                                      
093500                                                                          
093600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ111'.           
093700 01  DLI-IO-WDJ111.                                                       
093800*  03  -COPY WDJ111.                                                      
093900                                                                          
094000*                                                                         
094100**   KDP KIT PART                                                         
094200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ401'.           
094300 01  DLI-IO-WDJ401.                                                       
094400*  03  -COPY WDJ401.                                                      
094500*                                                                         
094600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ411'.           
094700 01  DLI-IO-WDJ411.                                                       
094800*  03  -COPY WDJ411.                                                      
094900*                                                                         
095000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
095100 01  DLI-IO-WDD301.                                                       
095200*  03  -COPY WDD301                                                       
095300*                                                                         
095400*LYNK CROSS REF                                                           
095500  01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF502'.            
095600  01  DLI-IO-WDF502.                                                      
095700 *  03  -COPY WDF502                                                      
095800*                                                                         
095900     EJECT                                                                
096000*    --- PARAMETERS TO WZ01SEND                                           
096100 01  FILLER                      PIC X(08)   VALUE 'WZ01SEND'.            
096200                                                                          
096300*01  -COPY WZ01SEND                                                       
096400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
096500                                                                          
096600*    --- PARAMETERS TO WZ01SEND FOR MRP LABEL                             
096700 01  FILLER                  PIC X(16)   VALUE 'WZ01SEND-FOR-MRP'.        
096800                                                                          
096900*01  -COPY WZ01SEND   -PRE MRP-                                           
097000 01  FILLER                  PIC X(16)   VALUE 'SEND-AREA-MRP'.           
097100 01  SEND-AREA-TO-MRP-LABEL.                                              
097200*    03  -COPY W403MRP                                                    
097300*                                                                         
097400*    --- PARAMETERS TO WZ01SEND FOR MRP KIT LABEL                         
097500 01  FILLER                  PIC X(16)   VALUE 'WZ01SEND-FOR-KIT'.        
097600*01  -COPY WZ01SEND   -PRE MRP-KIT-                                       
097700 01  FILLER                  PIC X(16)   VALUE 'SENDAREA-MRPKIT'.         
097800 01  SEND-AREA-TO-MRP-KIT-LABEL.                                          
097900*    03  -COPY WMRPKIT                                                    
098000                                                                          
098100 01  HDR-AREA.                                                            
098200*    03 -COPY WZ01REQU -PRE HDR-                                          
098300*    03 -COPY WZ04HDR                                                     
098400     EJECT                                                                
098500                                                                          
098600 01  HDR-KIT-AREA.                                                        
098700*    03 -COPY WZ01REQU -PRE KIT-                                          
098800*    03 -COPY WZ04HDR  -PRE KIT-                                          
098900     EJECT                                                                
099000                                                                          
099100 LINKAGE SECTION.                                                         
099200                                                                          
099300*01  -COPY W0009      -PRE MSG-                                           
099400     EJECT                                                                
099500*01  -COPY W0009      -PRE ALT1-                                          
099600     EJECT                                                                
099700*01  -COPY W0009      -PRE ALT2-                                          
099800     SKIP2                                                                
099900*01  -COPY W0009      -PRE DISTRDOC-                                      
100000     SKIP2                                                                
100100*01  -COPY W0009      -PRE DISTRDO2-                                      
100200     SKIP2                                                                
100300*01  -COPY W0009      -PRE SYNQ-                                          
100400     SKIP2                                                                
100500*01  -COPY W0008      -PRE USEA-                                          
100600     05  FILLER                  PIC X.                                   
100700     EJECT                                                                
100800*01  -COPY W0008      -PRE LISB-                                          
100900     05  FILLER                  PIC X.                                   
101000     EJECT                                                                
101100*01  -COPY W0008      -PRE 4003-                                          
101200     05  FILLER                  PIC X.                                   
101300     EJECT                                                                
101400*01  -COPY W0008      -PRE WDB6-                                          
101500     05  FILLER                  PIC X.                                   
101600     EJECT                                                                
101700*01  -COPY W0008      -PRE WDC3-                                          
101800     05  FILLER                  PIC X.                                   
101900     EJECT                                                                
102000*01  -COPY W0008      -PRE WDD3-                                          
102100     05  FILLER                  PIC X.                                   
102200     EJECT                                                                
102300*01  -COPY W0008      -PRE WDD3A-                                         
102400     05  FILLER                  PIC X.                                   
102500     EJECT                                                                
102600*01  -COPY W0008      -PRE WDK6-                                          
102700     05  FILLER                  PIC X.                                   
102800     EJECT                                                                
102900*01  -COPY W0008      -PRE WDG2-                                          
103000     05  FILLER                  PIC X.                                   
103100     EJECT                                                                
103200*01  -COPY W0008      -PRE WDJ1-                                          
103300     05  FILLER                  PIC X.                                   
103400     EJECT                                                                
103500*01  -COPY W0008      -PRE WDJ4-                                          
103600     05  FILLER                  PIC X.                                   
103700     EJECT                                                                
103800*01  -COPY W0008      -PRE WDF5-                                          
103900     05  FILLER                  PIC X.                                   
104000     EJECT                                                                
104100 01  SYNQ-ATAB-PCB             PIC X.                                     
104200 01  WDQ3-PCB                  PIC X.                                     
104300 PROCEDURE DIVISION  USING MSG-PCB                                        
104400                           ALT1-PCB                                       
104500                           ALT2-PCB                                       
104600                           DISTRDOC-PCB                                   
104700                           DISTRDO2-PCB                                   
104800                           SYNQ-PCB                                       
104900                           USEA-PCB                                       
105000                           LISB-PCB                                       
105100                           4003-PCB                                       
105200                           WDB6-PCB                                       
105300                           WDC3-PCB                                       
105400                           WDD3-PCB                                       
105500                           WDD3A-PCB                                      
105600                           WDK6-PCB                                       
105700                           WDG2-PCB                                       
105800                           WDJ1-PCB                                       
105900                           WDJ4-PCB                                       
106000                           WDF5-PCB                                       
106100                           SYNQ-ATAB-PCB WDQ3-PCB.                        
106200 MAIN SECTION.                                                            
106300     ENTRY 'DLITCBL' USING MSG-PCB                                        
106400                           ALT1-PCB                                       
106500                           ALT2-PCB                                       
106600                           DISTRDOC-PCB                                   
106700                           DISTRDO2-PCB                                   
106800                           SYNQ-PCB                                       
106900                           USEA-PCB                                       
107000                           LISB-PCB                                       
107100                           4003-PCB                                       
107200                           WDB6-PCB                                       
107300                           WDC3-PCB                                       
107400                           WDD3-PCB                                       
107500                           WDD3A-PCB                                      
107600                           WDK6-PCB                                       
107700                           WDG2-PCB                                       
107800                           WDJ1-PCB                                       
107900                           WDJ4-PCB                                       
108000                           WDF5-PCB                                       
108100                           SYNQ-ATAB-PCB WDQ3-PCB.                        
108200     PERFORM IMS-GU-MSG                                                   
108300     IF SEGMENT-FINNS                                                     
108400        PERFORM A-INIT                                                    
108500        IF ALLT-OK                                                        
108600           PERFORM B-LAES-PLOCKSATS                                       
108700           PERFORM C-PROCESS-PLE-LINES                                    
108800           PERFORM D-UPPDAT-PLOCKSATS                                     
108900           PERFORM E-SKICKA-IMSTRANS                                      
109000        END-IF                                                            
109100     END-IF                                                               
109200                                                                          
109300     MOVE ZERO TO RETURN-CODE                                             
109400     GOBACK                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 A-INIT SECTION.                                                          
109800                                                                          
109900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
110000     MOVE '013'             TO MSGI-KDCALL                                
110100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
110200     MOVE '4375'            TO MSGI-IDTRANS                               
110300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
110400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
110500                                                                          
110600     MOVE JA  TO ALLT-SW                                                  
110700     MOVE JA  TO FIRST-TIME-SW                                            
110800     MOVE JA  TO FIRST-TIME-KIT-SW                                        
110900     MOVE NEJ TO PLE-SW                                                   
111000                 PRINTER-SW                                               
111100                 PLOCK-SW                                                 
111200                 SKRIVKLAR-SW                                             
111300                 TOTAL-SW                                                 
111400                 SKRIVARTYP-SW                                            
111500     MOVE  NEJ     TO SEND-MRP-OPEN-SW                                    
111600     MOVE  NEJ     TO SEND-MRP-KIT-OPEN-SW                                
111700                                                                          
111800     MOVE ZERO                   TO ANT-RAD-IX                            
111900     MOVE ZERO                   TO PAGE-MRP-IX                           
112000     MOVE ZERO                   TO PAGE-MRP-KIT-IX                       
112100     MOVE ZERO                   TO NUMBER-TO-ROUNDUP                     
112200     MOVE ZERO                   TO NUMBER-ROUNDEDUP                      
112300     MOVE ZERO                   TO INTEGER-N2                            
112400                                                                          
112500     MOVE SPACE TO SPAR-PRINTER                                           
112600                   SPAR-KDSS-PLE                                          
112700                   WS-MRP-IDAFPRCD                                        
112800                   WS-MRP-KIT-IDAFPRCD                                    
112900     MOVE 999   TO SPAR-ADLAGOMR                                          
113000                                                                          
113100     IF MSG-KDTRANS-1  NOT = 'W4T376X '                                   
113200        MOVE NEJ TO ALLT-SW                                               
113300     END-IF                                                               
113400                                                                          
113500     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
113600     IF NOT GODK-MID                                                      
113700        MOVE NEJ TO ALLT-SW                                               
113800     END-IF                                                               
113900                                                                          
114000     IF ALLT-OK                                                           
114100        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I37601                  
114200        MOVE MSG-KDMFSFOR-1              TO PTOP1-KDMFSFOR                
114300                                                                          
114400        MOVE MID-IDPRC                   TO WS-PLE-IDPRC                  
114500        MOVE '-'                         TO WS-PLE-STRECK                 
114600        MOVE MID-IDLOPNR                 TO WS-PLE-IDLOPNR                
114700                                                                          
114800        MOVE 'MRP-'                      TO WS-MRP-IDPRC                  
114900        MOVE '-'                         TO WS-MRP-STRECK                 
115000        MOVE MID-IDLOPNR                 TO WS-MRP-IDLOPNR                
115100     END-IF                                                               
115200                                                                          
115300     ACCEPT WS-DATUM   FROM DATE                                          
115400     ACCEPT WS-KLOCKAN FROM TIME                                          
115500                                                                          
115600     MOVE FUNCTION CURRENT-DATE(1:12) TO WS-CURRENT-DATE-TIME             
115700                                                                          
115800     MOVE MID-IDDC TO W-IDDC-B6                                           
115900     PERFORM IMS-GU-WDB601                                                
116000                                                                          
116100     IF  DCS-CDC OR                                                       
116200        (DCS-SDC AND DCS-IDLANDX2 = 'GB') OR                              
116300         DCS-NDC-NA OR DCS-NDC-PF OR DCS-INDIA                            
116400                                                                          
116500       MOVE '011'                TO MSGI-KDCALL                           
116600       MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                           
116700                                    MSGI-IDLTERM-USER                     
116800       MOVE WS-DATUM             TO MSGI-TILOKDAT                         
116900       MOVE WS-TIHHMM            TO MSGI-TILOKTID                         
117000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
117100       MOVE MSGI-TILOKDAT        TO WS-DATUM                              
117200       MOVE MSGI-TILOKTID        TO WS-TIHHMM                             
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
117600 B-LAES-PLOCKSATS SECTION.                                                
117700     MOVE 'B-LAES-PLOCKSATS   '  TO WS-CURRENT-SECTION                    
117800                                                                          
117900     IF DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                                
118000       MOVE 150                 TO WS-TIME-DELAY                          
118100       CALL W009WAIT USING WS-TIME-DELAY                                  
118200     END-IF                                                               
118300                                                                          
118400     MOVE MID-IDPRODNR  TO W-4001-IDPRODNR                                
118500                                                                          
118600     MOVE MID-IDPLKLST  TO W-4001-IDPLKLST                                
118700                                                                          
118800     PERFORM IMS-GHU-4003-WL400311                                        
118900                                                                          
119000     IF 4004-NYCKEL-GRP NOT = LOW-VALUE                                   
119100        MOVE 4004-NYCKEL-GRP    TO W-4006-IDHTYP-X                        
119200        PERFORM IMS-GNP-4003-WL400321-KVAL                                
119300        MOVE 4006-KDPRT         TO SPAR-PRINTER                           
119400        MOVE 4006-KDSS-PLE      TO SPAR-KDSS-PLE                          
119500        MOVE 4006-ADLAGOMR      TO SPAR-ADLAGOMR                          
119600        MOVE 4006-ADLAGOMR      TO WS-4006-ADLAGOMR                       
119700     END-IF                                                               
119800     .                                                                    
119900     EJECT                                                                
120000 C-PROCESS-PLE-LINES     SECTION.                                         
120100     MOVE 'C-PROCESS-PLE-LINES'    TO WS-CURRENT-SECTION                  
120200                                                                          
120300     MOVE 0 TO SID-IX                                                     
120400               ETIKETT-IX                                                 
120500                                                                          
120600     PERFORM UNTIL SID-IX > MAX-SIDA                                      
120700                                                                          
120800        PERFORM CA-LAES-PLOCKRAD                                          
120900        IF PLOCKSATS-EJ-KLAR                                              
121000                                                                          
121100           PERFORM CB-BRYT-PRINTER                                        
121200                                                                          
121300           IF LASER-SKRIVARE                                              
121400*9864093                                                                  
121500             PERFORM CG-FLYTTA-LASERDATA                                  
121600             PERFORM CZ-SET-LINE-SKIP-FOR-LASER                           
121700             PERFORM S07-PRINT-LINE                                       
121800*MRP INDIA LABEL                                                          
121900             MOVE 4006-IDDISTR   TO DIST34-IDDISTR                        
122000             IF DCS-CDC                                                   
122100             IF DIST34-INDIA-NDC                                          
122200*lk          OR DIST35-CDC-IN-REFILL                                      
122300               MOVE 4006-KVAVBART     TO WS-KVAVBART-MRP                  
122400                                                                          
122500               IF WS-KVAVBART-MRP > 4000                                  
122600*MAX 4000 LABELS                                                          
122700                 MOVE 4000            TO WS-KVAVBART-MRP                  
122800               END-IF                                                     
122900                                                                          
123000               IF SEND-MRP-NOT-OPEN                                       
123100                 PERFORM S20-OPEN-MRP                                     
123200                 MOVE    JA      TO SEND-MRP-OPEN-SW                      
123300               END-IF                                                     
123400*STD MRP LABEL                                                            
123500               PERFORM CH-MOVE-MRP-LABEL                                  
123600                                                                          
123700               IF FIRST-TIME                                              
123800                 PERFORM CK-PUT-HEADER                                    
123900                 MOVE NEJ             TO FIRST-TIME-SW                    
124000                 MOVE 4006-ADLAGOMR   TO WS-4006-ADLAGOMR                 
124100               END-IF                                                     
124200                                                                          
124300               IF WS-ART-IDLEVNR = '1001' OR '1002'                       
124400               OR WS-ART-KDSORT  = 'SA'                                   
124500*KIT SUPPLIER CODE 1002 KITS ARE BUILT IN CDC                             
124600                 IF SEND-MRP-KIT-NOT-OPEN                                 
124700                   PERFORM S30-OPEN-MRP-KIT                               
124800                   MOVE JA       TO SEND-MRP-KIT-OPEN-SW                  
124900                 END-IF                                                   
125000                                                                          
125100                 IF FIRST-TIME-KIT                                        
125200                   PERFORM CA-PUT-MRP-KIT-HEADER                          
125300                   MOVE NEJ           TO FIRST-TIME-KIT-SW                
125400                 END-IF                                                   
125500*INGÅENDE ARTIKLAR                                                        
125600                 PERFORM CP-CLEAR-TABEL                                   
125700                 PERFORM CM-READ-KIT-PARTS-TO-TABEL                       
125800               END-IF                                                     
125900                                                                          
126000               IF PAGE-MRP-IX = 0                                         
126100               OR PAGE-MRP-IX = 25                                        
126200                 MOVE +1           TO PAGE-MRP-IX                         
126300                 MOVE '          ' TO MRP-IDAFPRCD                        
126400                 MOVE '          ' TO WS-MRP-IDAFPRCD                     
126500               END-IF                                                     
126600                                                                          
126700               MOVE 1             TO ANT-MRP-IX                           
126800               PERFORM UNTIL ANT-MRP-IX > WS-KVAVBART-MRP                 
126900*PRINT MRP LABEL                                                          
127000                 PERFORM CI-SET-MRP-RADSKIP                               
127100                 PERFORM CJ-PUT-LINE                                      
127200                 ADD  1               TO PAGE-MRP-IX                      
127300*PRINT MRP KIT LABEL                                                      
127400                                                                          
127500                 IF WS-ART-IDLEVNR = '1001' OR '1002'                     
127600                 OR WS-ART-KDSORT = 'SA'                                  
127700*PRINT KIT PART                                                           
127800                   PERFORM CR-PRINT-KIT-PART                              
127900                 END-IF                                                   
128000                 ADD  1               TO ANT-MRP-IX                       
128100               END-PERFORM                                                
128200             END-IF                                                       
128300             END-IF                                                       
128400                                                                          
128500           ELSE                                                           
128600             PERFORM CC-REDIGERA-PRINTRADER                               
128700             PERFORM CD-SAETT-RADSKIP                                     
128800             PERFORM CE-SKRIV-ETIKETT                                     
128900           END-IF                                                         
129000        ELSE                                                              
129100           IF PLE-OPEN                                                    
129200              MOVE SPACE TO SPAR-PRINTER                                  
129300              PERFORM CB-BRYT-PRINTER                                     
129400           ELSE                                                           
129500              IF (PLE-CLOSE AND 4004-NYCKEL-GRP NOT = LOW-VALUE)          
129600                 MOVE '4'               TO WS-SYSTDEL                     
129700                 MOVE 'PE'              TO WS-LISTTYP                     
129800                 MOVE 4006-KDPRT        TO WS-KDPRT                       
129900                 PERFORM S01-OPEN-PLE                                     
130000                 MOVE JA                TO PLE-SW                         
130100                 MOVE SPACE             TO SPAR-PRINTER                   
130200                 PERFORM CB-BRYT-PRINTER                                  
130300              END-IF                                                      
130400           END-IF                                                         
130500           MOVE 2000 TO SID-IX                                            
130600        END-IF                                                            
130700     END-PERFORM                                                          
130800                                                                          
130900*MRP INDIA LABEL                                                          
131000     IF DIST34-INDIA-NDC                                                  
131100*lk  OR DIST35-CDC-IN-REFILL                                              
131200       IF SEND-MRP-OPEN                                                   
131300         PERFORM S21-CLOSE-MRP                                            
131400       END-IF                                                             
131500                                                                          
131600       IF SEND-MRP-KIT-OPEN                                               
131700         PERFORM S31-CLOSE-MRP-KIT                                        
131800       END-IF                                                             
131900     END-IF                                                               
132000                                                                          
132100     IF PLE-OPEN                                                          
132200        IF TOTAL-EJ-SKRIVEN                                               
132300           PERFORM S06-KOLLA-SKRIVKLAR                                    
132400        END-IF                                                            
132500        PERFORM S02-CLOSE-PLE                                             
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 CA-LAES-PLOCKRAD SECTION.                                                
133000     MOVE 'CA-LAES-PLOCKRAD'     TO WS-CURRENT-SECTION                    
133100                                                                          
133200     PERFORM IMS-GNP-4003-WL400321-OKVAL                                  
133300                                                                          
133400     IF SEGMENT-SAKNAS                                                    
133500        MOVE JA TO PLOCK-SW                                               
133600     ELSE                                                                 
133700        IF MID-IDDC = 11                                                  
133800         IF (4006-ADLAGOMR-ORD = 90 OR 98)                                
133900          IF (4006-IDPRC NOT = '991Q' AND '9920')                         
134000           MOVE 'PCK' TO SYNQ-ORDERTYPE                                   
134100           MOVE MID-IDDC TO SYNQ-IDDC                                     
134200           MOVE 4006-IDARTNR TO SYNQ-IDARTNR                              
134300           MOVE 4006-KVAVBART TO SYNQ-KVBEST                              
134400           MOVE 4006-TIRFSDAT TO SYNQ-TIRFSDAT                            
134500           MOVE 4006-IDPRODNR TO SYNQ-IDPRODNR                            
134600           MOVE 4006-IDRADNR TO SYNQ-IDRADNR                              
134700           MOVE 4006-KDORDKL TO SYNQ-KDORDKL                              
134800           MOVE 4006-IDBORD TO SYNQ-IDBORD                                
134900           MOVE 4006-IDKUNDRF TO SYNQ-IDKUNDRF                            
135000           MOVE 4006-IDPLKLST TO SYNQ-IDPLKLST                            
135100           MOVE 4006-IDPRC    TO SYNQ-IDPRC                               
135200           MOVE NEJ TO SYNQ-FLSATS                                        
135300           CALL W488ORCR USING  SYNQ-W488ORCR SYNQ-PCB                    
135400                              SYNQ-ATAB-PCB WDQ3-PCB                      
135500          END-IF                                                          
135600         END-IF                                                           
135700        END-IF                                                            
135800     END-IF                                                               
135900     .                                                                    
136000     EJECT                                                                
136100 CB-BRYT-PRINTER SECTION.                                                 
136200     MOVE 'CB-BRYT-PRINTER  '    TO WS-CURRENT-SECTION                    
136300                                                                          
136400     IF PLE-CLOSE                                                         
136500        MOVE '4'                 TO WS-SYSTDEL                            
136600        MOVE 'PE'                TO WS-LISTTYP                            
136700        MOVE 4006-KDPRT          TO WS-KDPRT                              
136800        PERFORM S01-OPEN-PLE                                              
136900        MOVE JA                  TO PLE-SW                                
137000     ELSE                                                                 
137100        IF 4006-KDPRT NOT = SPAR-PRINTER                                  
137200           IF LASER-SKRIVARE                                              
137300             PERFORM CBB-REDIGERA-TOTAL                                   
137400             PERFORM S08-SKRIV-TOTALRAD                                   
137500             PERFORM S03-PURGE-PLE                                        
137600           ELSE                                                           
137700             PERFORM CBA-SKRIV-TOTALER                                    
137800             PERFORM S03-PURGE-PLE                                        
137900           END-IF                                                         
138000                                                                          
138100           PERFORM S09-KOLLA-OM-LASER                                     
138200           IF LASER-SKRIVARE                                              
138300             MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                          
138400           END-IF                                                         
138500                                                                          
138600           MOVE '4'              TO WS-SYSTDEL                            
138700           MOVE 'PE'             TO WS-LISTTYP                            
138800           MOVE 4006-KDPRT       TO WS-KDPRT                              
138900        END-IF                                                            
139000     END-IF                                                               
139100                                                                          
139200     MOVE 4006-KDPRT TO SPAR-PRINTER                                      
139300     .                                                                    
139400     EJECT                                                                
139500                                                                          
139600 CBA-SKRIV-TOTALER SECTION.                                               
139700     MOVE 'CBA-SKRIV-TOTALER '    TO WS-CURRENT-SECTION                   
139800                                                                          
139900     IF ETIKETT-IX = MAX-ETIKETT                                          
140000        MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                               
140100        MOVE 0               TO ETIKETT-IX                                
140200        ADD  1               TO SID-IX                                    
140300     ELSE                                                                 
140400        MOVE PRT-AFTER-3     TO PRT-RADSKIP                               
140500     END-IF                                                               
140600                                                                          
140700     EVALUATE TRUE                                                        
140800       WHEN  DCS-CDC                                                      
140900         OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                             
141000          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-SVE                
141100          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-SVE           
141200          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-SVE                
141300          MOVE WS-DATUM               TO TOTRAD2-DATUM-SVE                
141400          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-SVE                 
141500          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-SVE                 
141600                                                                          
141700          IF SKRIVKLAR                                                    
141800             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
141900             MOVE PLE-TOTRAD1-SVE     TO ARB-RAD-H (1)                    
142000             MOVE PLE-TOTRAD2-SVE     TO ARB-RAD-H (2)                    
142100             MOVE ARB-RAD (1)         TO WS-PLE-RAD                       
142200             PERFORM S04-SKRIV-RAD                                        
142300             MOVE ARB-RAD (2)         TO WS-PLE-RAD                       
142400             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
142500             PERFORM S04-SKRIV-RAD                                        
142600          ELSE                                                            
142700             MOVE PLE-TOTRAD1-SVE     TO ARB-RAD-V (1)                    
142800             MOVE PLE-TOTRAD2-SVE     TO ARB-RAD-V (2)                    
142900          END-IF                                                          
143000                                                                          
143100          MOVE JA                     TO TOT-FORSTA-SW                    
143200          PERFORM CBAA-TOT-PER-LAGOMR-SVE                                 
143300                                                                          
143400       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
143500          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-ITA                
143600          MOVE 4006-IDLOPNR-PL        TO TOTRAD2-IDLOPNR-PL-ITA           
143700          MOVE 4004-KVRADER (100)     TO TOTRAD2-ANTAL-ITA                
143800          MOVE WS-DATUM               TO TOTRAD3-DATUM-ITA                
143900          MOVE WS-TIHHMM (1:2)        TO TOTRAD3-TIHH-ITA                 
144000          MOVE WS-TIHHMM (3:2)        TO TOTRAD3-TIMM-ITA                 
144100                                                                          
144200          IF SKRIVKLAR                                                    
144300             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
144400             MOVE PLE-TOTRAD1-ITA     TO ARB-RAD-H (1)                    
144500             MOVE PLE-TOTRAD2-ITA     TO ARB-RAD-H (2)                    
144600             MOVE ARB-RAD (1)         TO WS-PLE-RAD                       
144700             PERFORM S04-SKRIV-RAD                                        
144800             MOVE ARB-RAD (2)         TO WS-PLE-RAD                       
144900             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
145000             PERFORM S04-SKRIV-RAD                                        
145100          ELSE                                                            
145200             MOVE PLE-TOTRAD1-ITA     TO ARB-RAD-V (1)                    
145300             MOVE PLE-TOTRAD2-ITA     TO ARB-RAD-V (2)                    
145400          END-IF                                                          
145500          MOVE JA                     TO TOT-FORSTA-SW                    
145600          PERFORM CBAC-TOTAL-PER-LAGOMR-ITA                               
145700                                                                          
145800       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
145900          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-SPA                
146000          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-SPA           
146100          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-SPA                
146200          MOVE WS-DATUM               TO TOTRAD2-DATUM-SPA                
146300          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-SPA                 
146400          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-SPA                 
146500                                                                          
146600          IF SKRIVKLAR                                                    
146700             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
146800             MOVE PLE-TOTRAD1-SPA     TO ARB-RAD-H (1)                    
146900             MOVE PLE-TOTRAD2-SPA     TO ARB-RAD-H (2)                    
147000             MOVE ARB-RAD (1)         TO WS-PLE-RAD                       
147100             PERFORM S04-SKRIV-RAD                                        
147200             MOVE ARB-RAD (2)         TO WS-PLE-RAD                       
147300             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
147400             PERFORM S04-SKRIV-RAD                                        
147500          ELSE                                                            
147600             MOVE PLE-TOTRAD1-SPA     TO ARB-RAD-V (1)                    
147700             MOVE PLE-TOTRAD2-SPA     TO ARB-RAD-V (2)                    
147800          END-IF                                                          
147900          MOVE JA                     TO TOT-FORSTA-SW                    
148000          PERFORM CBAD-TOTAL-PER-LAGOMR-SPA                               
148100                                                                          
148200       WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                               
148300          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-OST                
148400          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-OST           
148500          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-OST                
148600          MOVE WS-DATUM               TO TOTRAD2-DATUM-OST                
148700          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-OST                 
148800          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-OST                 
148900                                                                          
149000          IF SKRIVKLAR                                                    
149100             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
149200             MOVE PLE-TOTRAD1-OST     TO ARB-RAD-H (1)                    
149300             MOVE PLE-TOTRAD2-OST     TO ARB-RAD-H (2)                    
149400             MOVE ARB-RAD (1)         TO WS-PLE-RAD                       
149500             PERFORM S04-SKRIV-RAD                                        
149600             MOVE ARB-RAD (2)         TO WS-PLE-RAD                       
149700             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
149800             PERFORM S04-SKRIV-RAD                                        
149900          ELSE                                                            
150000             MOVE PLE-TOTRAD1-OST     TO ARB-RAD-V (1)                    
150100             MOVE PLE-TOTRAD2-OST     TO ARB-RAD-V (2)                    
150200          END-IF                                                          
150300          MOVE JA                     TO TOT-FORSTA-SW                    
150400          PERFORM CBAF-TOTAL-PER-LAGOMR-OST                               
150500                                                                          
150600       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
150700          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-JAP                
150800          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-JAP           
150900          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-JAP                
151000          MOVE WS-DATUM               TO TOTRAD2-DATUM-JAP                
151100          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-JAP                 
151200          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-JAP                 
151300                                                                          
151400          IF SKRIVKLAR                                                    
151500             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
151600             MOVE PLE-TOTRAD1-JAP     TO JAP-RAD-H (1)                    
151700             MOVE PLE-TOTRAD2-JAP     TO JAP-RAD-H (2)                    
151800             MOVE JAP-RAD (1)         TO WS-PLE-RAD                       
151900             PERFORM S04-SKRIV-RAD                                        
152000             MOVE JAP-RAD (2)         TO WS-PLE-RAD                       
152100             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
152200             PERFORM S04-SKRIV-RAD                                        
152300          ELSE                                                            
152400             MOVE PLE-TOTRAD1-JAP     TO JAP-RAD-V (1)                    
152500             MOVE PLE-TOTRAD2-JAP     TO JAP-RAD-V (2)                    
152600          END-IF                                                          
152700          MOVE JA                     TO TOT-FORSTA-SW                    
152800          PERFORM CBAF-TOTAL-PER-LAGOMR-JAP                               
152900                                                                          
153000       WHEN DCS-NDC-NA                                                    
153100          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-ENG                
153200          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-ENG           
153300          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-ENG                
153400          MOVE WS-DATUM               TO TOTRAD2-DATUM-ENG                
153500          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-ENG                 
153600          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-ENG                 
153700                                                                          
153800          IF SKRIVKLAR                                                    
153900             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
154000             MOVE PLE-TOTRAD1-ENG     TO NDC-RAD-H (1)                    
154100             MOVE PLE-TOTRAD2-ENG     TO NDC-RAD-H (2)                    
154200             MOVE NDC-RAD (1)         TO WS-PLE-RAD                       
154300             PERFORM S04-SKRIV-RAD                                        
154400             MOVE NDC-RAD (2)         TO WS-PLE-RAD                       
154500             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
154600             PERFORM S04-SKRIV-RAD                                        
154700          ELSE                                                            
154800             MOVE PLE-TOTRAD1-ENG     TO NDC-RAD-V (1)                    
154900             MOVE PLE-TOTRAD2-ENG     TO NDC-RAD-V (2)                    
155000          END-IF                                                          
155100          MOVE JA                     TO TOT-FORSTA-SW                    
155200          PERFORM CBAE-TOTAL-PER-LAGOMR-ENG                               
155300                                                                          
155400       WHEN OTHER                                                         
155500*       MOVE PRT-NYSIDA-RAD1          TO PRT-RADSKIP                      
155600          MOVE 4006-IDPRC             TO TOTRAD1-IDPRC-ENG                
155700          MOVE 4006-IDLOPNR-PL        TO TOTRAD1-IDLOPNR-PL-ENG           
155800          MOVE 4004-KVRADER (100)     TO TOTRAD1-ANTAL-ENG                
155900          MOVE WS-DATUM               TO TOTRAD2-DATUM-ENG                
156000          MOVE WS-TIHHMM (1:2)        TO TOTRAD2-TIHH-ENG                 
156100          MOVE WS-TIHHMM (3:2)        TO TOTRAD2-TIMM-ENG                 
156200                                                                          
156300          IF SKRIVKLAR                                                    
156400             MOVE SPAR-RADSKIP        TO PRT-RADSKIP                      
156500             MOVE PLE-TOTRAD1-ENG     TO ARB-RAD-H (1)                    
156600             MOVE PLE-TOTRAD2-ENG     TO ARB-RAD-H (2)                    
156700             MOVE ARB-RAD (1)         TO WS-PLE-RAD                       
156800             PERFORM S04-SKRIV-RAD                                        
156900             MOVE ARB-RAD (2)         TO WS-PLE-RAD                       
157000             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
157100             PERFORM S04-SKRIV-RAD                                        
157200          ELSE                                                            
157300             MOVE PLE-TOTRAD1-ENG     TO ARB-RAD-V (1)                    
157400             MOVE PLE-TOTRAD2-ENG     TO ARB-RAD-V (2)                    
157500          END-IF                                                          
157600          MOVE JA                     TO TOT-FORSTA-SW                    
157700          PERFORM CBAE-TOTAL-PER-LAGOMR-ENG                               
157800                                                                          
157900     END-EVALUATE                                                         
158000                                                                          
158100     .                                                                    
158200     EJECT                                                                
158300 CBAA-TOT-PER-LAGOMR-SVE SECTION.                                         
158400                                                                          
158500     PERFORM CBAAA-NOLLSTAL-PLE-TOTRAD3-SVE                               
158600     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
158700     MOVE 1 TO ANT-LAGOMR-IX                                              
158800     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
158900                                                                          
159000     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
159100        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
159200           ADD 1    TO ANT-ETIK-IX                                        
159300           MOVE ANT-LAGOMR-IX TO                                          
159400                TOTRAD3-ADLAGOMR-SVE (ANT-ETIK-IX)                        
159500           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
159600                    TO TOTRAD3-ANTAL-SVE   (ANT-ETIK-IX)                  
159700           MOVE ',' TO TOTRAD3-KOMMA-SVE   (ANT-ETIK-IX)                  
159800                                                                          
159900           IF ANT-ETIK-IX = 5                                             
160000                                                                          
160100     MOVE SPACE                  TO ERROR-TEXT                            
160200     MOVE ANT-ETIK-IX            TO ANT-ETIK-IX-WS                        
160300              MOVE SPACE        TO TOTRAD3-KOMMA-SVE (ANT-ETIK-IX)        
160400              ADD 1               TO ANT-SID-IX                           
160500              MOVE PLE-TOTRAD3-SVE TO                                     
160600                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
160700              PERFORM CBAAA-NOLLSTAL-PLE-TOTRAD3-SVE                      
160800              MOVE 0           TO ANT-ETIK-IX                             
160900                                                                          
161000              IF ANT-SID-IX = 3 OR                                        
161100                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
161200                                                                          
161300                 PERFORM S05-SKRIV-TOTALRADER                             
161400                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
161500                 MOVE 0        TO ANT-SID-IX                              
161600              END-IF                                                      
161700           END-IF                                                         
161800        END-IF                                                            
161900        ADD 1 TO ANT-LAGOMR-IX                                            
162000     END-PERFORM                                                          
162100                                                                          
162200     IF ANT-ETIK-IX > 0                                                   
162300        MOVE SPACE          TO TOTRAD3-KOMMA-SVE (ANT-ETIK-IX)            
162400        ADD 1               TO ANT-SID-IX                                 
162500        MOVE PLE-TOTRAD3-SVE TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
162600     END-IF                                                               
162700     IF TOTAL-EJ-SKRIVEN                                                  
162800        PERFORM S05-SKRIV-TOTALRADER                                      
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 CBAAA-NOLLSTAL-PLE-TOTRAD3-SVE SECTION.                                  
163300                                                                          
163400     MOVE 1 TO ANT-ETIK-IX                                                
163500                                                                          
163600     PERFORM UNTIL ANT-ETIK-IX > 5                                        
163700        MOVE ZERO  TO TOTRAD3-ADLAGOMR-SVE (ANT-ETIK-IX)                  
163800                      TOTRAD3-ANTAL-SVE   (ANT-ETIK-IX)                   
163900        MOVE SPACE TO TOTRAD3-KOMMA-SVE   (ANT-ETIK-IX)                   
164000        ADD 1 TO ANT-ETIK-IX                                              
164100     END-PERFORM                                                          
164200     .                                                                    
164300     EJECT                                                                
164400 CBAC-TOTAL-PER-LAGOMR-ITA SECTION.                                       
164500                                                                          
164600     PERFORM CBACA-NOLLSTAL-PLE-TOTRAD3-ITA                               
164700     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
164800     MOVE 1 TO ANT-LAGOMR-IX                                              
164900     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
165000                                                                          
165100     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
165200        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
165300           ADD 1    TO ANT-ETIK-IX                                        
165400           MOVE ANT-LAGOMR-IX TO                                          
165500                TOTRAD4-ADLAGOMR-ITA (ANT-ETIK-IX)                        
165600           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
165700                    TO TOTRAD4-ANTAL-ITA   (ANT-ETIK-IX)                  
165800           MOVE ',' TO TOTRAD4-KOMMA-ITA   (ANT-ETIK-IX)                  
165900                                                                          
166000           IF ANT-ETIK-IX = 5                                             
166100              MOVE SPACE      TO TOTRAD4-KOMMA-ITA (ANT-ETIK-IX)          
166200              ADD 1               TO ANT-SID-IX                           
166300              MOVE PLE-TOTRAD3-ITA TO                                     
166400                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
166500              PERFORM CBACA-NOLLSTAL-PLE-TOTRAD3-ITA                      
166600              MOVE 0           TO ANT-ETIK-IX                             
166700                                                                          
166800              IF ANT-SID-IX = 3 OR                                        
166900                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
167000                 PERFORM S05-SKRIV-TOTALRADER                             
167100                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
167200                 MOVE 0        TO ANT-SID-IX                              
167300              END-IF                                                      
167400           END-IF                                                         
167500        END-IF                                                            
167600        ADD 1 TO ANT-LAGOMR-IX                                            
167700     END-PERFORM                                                          
167800                                                                          
167900     IF ANT-ETIK-IX > 0                                                   
168000        MOVE SPACE          TO TOTRAD4-KOMMA-ITA (ANT-ETIK-IX)            
168100        ADD 1               TO ANT-SID-IX                                 
168200        MOVE PLE-TOTRAD3-ITA TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
168300     END-IF                                                               
168400                                                                          
168500     IF TOTAL-EJ-SKRIVEN                                                  
168600        PERFORM S05-SKRIV-TOTALRADER                                      
168700     END-IF                                                               
168800     .                                                                    
168900     EJECT                                                                
169000 CBACA-NOLLSTAL-PLE-TOTRAD3-ITA SECTION.                                  
169100                                                                          
169200     MOVE 1 TO ANT-ETIK-IX                                                
169300                                                                          
169400     PERFORM UNTIL ANT-ETIK-IX > 5                                        
169500        MOVE ZERO  TO TOTRAD4-ADLAGOMR-ITA (ANT-ETIK-IX)                  
169600                      TOTRAD4-ANTAL-ITA   (ANT-ETIK-IX)                   
169700        MOVE SPACE TO TOTRAD4-KOMMA-ITA   (ANT-ETIK-IX)                   
169800        ADD 1 TO ANT-ETIK-IX                                              
169900     END-PERFORM                                                          
170000     .                                                                    
170100     EJECT                                                                
170200 CBAD-TOTAL-PER-LAGOMR-SPA SECTION.                                       
170300                                                                          
170400     PERFORM CBADA-NOLLSTAL-PLE-TOTRAD3-SPA                               
170500     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
170600     MOVE 1 TO ANT-LAGOMR-IX                                              
170700     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
170800                                                                          
170900     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
171000        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
171100           ADD 1    TO ANT-ETIK-IX                                        
171200           MOVE ANT-LAGOMR-IX TO                                          
171300                TOTRAD3-ADLAGOMR-SPA (ANT-ETIK-IX)                        
171400           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
171500                    TO TOTRAD3-ANTAL-SPA   (ANT-ETIK-IX)                  
171600           MOVE ',' TO TOTRAD3-KOMMA-SPA   (ANT-ETIK-IX)                  
171700                                                                          
171800           IF ANT-ETIK-IX = 5                                             
171900              MOVE SPACE      TO TOTRAD3-KOMMA-SPA (ANT-ETIK-IX)          
172000              ADD 1               TO ANT-SID-IX                           
172100              MOVE PLE-TOTRAD3-SPA TO                                     
172200                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
172300              PERFORM CBADA-NOLLSTAL-PLE-TOTRAD3-SPA                      
172400              MOVE 0           TO ANT-ETIK-IX                             
172500                                                                          
172600              IF ANT-SID-IX = 3 OR                                        
172700                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
172800                 PERFORM S05-SKRIV-TOTALRADER                             
172900                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
173000                 MOVE 0        TO ANT-SID-IX                              
173100              END-IF                                                      
173200           END-IF                                                         
173300        END-IF                                                            
173400        ADD 1 TO ANT-LAGOMR-IX                                            
173500     END-PERFORM                                                          
173600                                                                          
173700     IF ANT-ETIK-IX > 0                                                   
173800        MOVE SPACE          TO TOTRAD3-KOMMA-SPA (ANT-ETIK-IX)            
173900        ADD 1               TO ANT-SID-IX                                 
174000        MOVE PLE-TOTRAD3-SPA TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
174100     END-IF                                                               
174200                                                                          
174300     IF TOTAL-EJ-SKRIVEN                                                  
174400        PERFORM S05-SKRIV-TOTALRADER                                      
174500     END-IF                                                               
174600     .                                                                    
174700     EJECT                                                                
174800 CBADA-NOLLSTAL-PLE-TOTRAD3-SPA SECTION.                                  
174900                                                                          
175000     MOVE 1 TO ANT-ETIK-IX                                                
175100                                                                          
175200     PERFORM UNTIL ANT-ETIK-IX > 5                                        
175300        MOVE ZERO  TO TOTRAD3-ADLAGOMR-SPA (ANT-ETIK-IX)                  
175400                      TOTRAD3-ANTAL-SPA   (ANT-ETIK-IX)                   
175500        MOVE SPACE TO TOTRAD3-KOMMA-SPA   (ANT-ETIK-IX)                   
175600        ADD 1 TO ANT-ETIK-IX                                              
175700     END-PERFORM                                                          
175800     .                                                                    
175900     EJECT                                                                
176000 CBAF-TOTAL-PER-LAGOMR-OST SECTION.                                       
176100                                                                          
176200     PERFORM CBAFA-NOLLSTAL-PLE-TOTRAD3-OST                               
176300     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
176400     MOVE 1 TO ANT-LAGOMR-IX                                              
176500     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
176600                                                                          
176700     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
176800        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
176900           ADD 1    TO ANT-ETIK-IX                                        
177000           MOVE ANT-LAGOMR-IX TO                                          
177100                TOTRAD3-ADLAGOMR-OST (ANT-ETIK-IX)                        
177200           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
177300                    TO TOTRAD3-ANTAL-OST   (ANT-ETIK-IX)                  
177400           MOVE ',' TO TOTRAD3-KOMMA-OST   (ANT-ETIK-IX)                  
177500                                                                          
177600           IF ANT-ETIK-IX = 5                                             
177700              MOVE SPACE      TO TOTRAD3-KOMMA-OST (ANT-ETIK-IX)          
177800              ADD 1               TO ANT-SID-IX                           
177900              MOVE PLE-TOTRAD3-OST TO                                     
178000                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
178100              PERFORM CBAFA-NOLLSTAL-PLE-TOTRAD3-OST                      
178200              MOVE 0           TO ANT-ETIK-IX                             
178300                                                                          
178400              IF ANT-SID-IX = 3 OR                                        
178500                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
178600                 PERFORM S05-SKRIV-TOTALRADER                             
178700                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
178800                 MOVE 0        TO ANT-SID-IX                              
178900              END-IF                                                      
179000           END-IF                                                         
179100        END-IF                                                            
179200        ADD 1 TO ANT-LAGOMR-IX                                            
179300     END-PERFORM                                                          
179400                                                                          
179500     IF ANT-ETIK-IX > 0                                                   
179600        MOVE SPACE          TO TOTRAD3-KOMMA-OST (ANT-ETIK-IX)            
179700        ADD 1               TO ANT-SID-IX                                 
179800        MOVE PLE-TOTRAD3-OST TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
179900     END-IF                                                               
180000                                                                          
180100     IF TOTAL-EJ-SKRIVEN                                                  
180200        PERFORM S05-SKRIV-TOTALRADER                                      
180300     END-IF                                                               
180400     .                                                                    
180500     EJECT                                                                
180600 CBAFA-NOLLSTAL-PLE-TOTRAD3-OST SECTION.                                  
180700                                                                          
180800     MOVE 1 TO ANT-ETIK-IX                                                
180900                                                                          
181000     PERFORM UNTIL ANT-ETIK-IX > 5                                        
181100        MOVE ZERO  TO TOTRAD3-ADLAGOMR-OST (ANT-ETIK-IX)                  
181200                      TOTRAD3-ANTAL-OST   (ANT-ETIK-IX)                   
181300        MOVE SPACE TO TOTRAD3-KOMMA-OST   (ANT-ETIK-IX)                   
181400        ADD 1 TO ANT-ETIK-IX                                              
181500     END-PERFORM                                                          
181600     .                                                                    
181700     EJECT                                                                
181800 CBAE-TOTAL-PER-LAGOMR-ENG SECTION.                                       
181900                                                                          
182000     PERFORM CBAEA-NOLLSTAL-PLE-TOTRAD3-ENG                               
182100     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
182200     MOVE 1 TO ANT-LAGOMR-IX                                              
182300     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
182400                                                                          
182500     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
182600        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
182700           ADD 1    TO ANT-ETIK-IX                                        
182800           MOVE ANT-LAGOMR-IX TO                                          
182900                TOTRAD3-ADLAGOMR-ENG (ANT-ETIK-IX)                        
183000           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
183100                    TO TOTRAD3-ANTAL-ENG   (ANT-ETIK-IX)                  
183200           MOVE ',' TO TOTRAD3-KOMMA-ENG   (ANT-ETIK-IX)                  
183300                                                                          
183400           IF ANT-ETIK-IX = 5                                             
183500              MOVE SPACE      TO TOTRAD3-KOMMA-ENG (ANT-ETIK-IX)          
183600              ADD 1               TO ANT-SID-IX                           
183700              MOVE PLE-TOTRAD3-ENG TO                                     
183800                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
183900              PERFORM CBAEA-NOLLSTAL-PLE-TOTRAD3-ENG                      
184000              MOVE 0           TO ANT-ETIK-IX                             
184100                                                                          
184200              IF ANT-SID-IX = 3 OR                                        
184300                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
184400                 PERFORM S05-SKRIV-TOTALRADER                             
184500                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
184600                 MOVE 0        TO ANT-SID-IX                              
184700              END-IF                                                      
184800           END-IF                                                         
184900        END-IF                                                            
185000        ADD 1 TO ANT-LAGOMR-IX                                            
185100     END-PERFORM                                                          
185200                                                                          
185300     IF ANT-ETIK-IX > 0                                                   
185400        MOVE SPACE          TO TOTRAD3-KOMMA-ENG (ANT-ETIK-IX)            
185500        ADD 1               TO ANT-SID-IX                                 
185600        MOVE PLE-TOTRAD3-ENG TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
185700     END-IF                                                               
185800                                                                          
185900     IF TOTAL-EJ-SKRIVEN                                                  
186000        PERFORM S05-SKRIV-TOTALRADER                                      
186100     END-IF                                                               
186200     .                                                                    
186300     EJECT                                                                
186400 CBAEA-NOLLSTAL-PLE-TOTRAD3-ENG SECTION.                                  
186500                                                                          
186600     MOVE 1 TO ANT-ETIK-IX                                                
186700                                                                          
186800     PERFORM UNTIL ANT-ETIK-IX > 5                                        
186900        MOVE ZERO  TO TOTRAD3-ADLAGOMR-ENG (ANT-ETIK-IX)                  
187000                      TOTRAD3-ANTAL-ENG   (ANT-ETIK-IX)                   
187100        MOVE SPACE TO TOTRAD3-KOMMA-ENG   (ANT-ETIK-IX)                   
187200        ADD 1 TO ANT-ETIK-IX                                              
187300     END-PERFORM                                                          
187400     .                                                                    
187500     EJECT                                                                
187600 CBAF-TOTAL-PER-LAGOMR-JAP    SECTION.                                    
187700                                                                          
187800     PERFORM CBAFA-NOLLSTAL-PLE-TOTRAD3-JAP                               
187900     MOVE SPACE TO WS-SPAR-TOTALRADER                                     
188000     MOVE 1 TO ANT-LAGOMR-IX                                              
188100     MOVE 0 TO ANT-ETIK-IX ANT-SID-IX                                     
188200                                                                          
188300     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
188400        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
188500           ADD 1    TO ANT-ETIK-IX                                        
188600           MOVE ANT-LAGOMR-IX TO                                          
188700                TOTRAD3-ADLAGOMR-ENG (ANT-ETIK-IX)                        
188800           MOVE 4004-KVRADER               (ANT-LAGOMR-IX)                
188900                    TO TOTRAD3-ANTAL-ENG   (ANT-ETIK-IX)                  
189000           MOVE ',' TO TOTRAD3-KOMMA-ENG   (ANT-ETIK-IX)                  
189100                                                                          
189200           IF ANT-ETIK-IX = 5                                             
189300              MOVE SPACE      TO TOTRAD3-KOMMA-ENG (ANT-ETIK-IX)          
189400              ADD 1               TO ANT-SID-IX                           
189500              MOVE PLE-TOTRAD3-ENG TO                                     
189600                   WS-SPAR-TOTALRAD (ANT-SID-IX)                          
189700              PERFORM CBAFA-NOLLSTAL-PLE-TOTRAD3-JAP                      
189800              MOVE 0           TO ANT-ETIK-IX                             
189900                                                                          
190000              IF ANT-SID-IX = 3 OR                                        
190100                 (ANT-SID-IX = 1 AND FORSTA-TOTETIK)                      
190200                 PERFORM S05-SKRIV-TOTALRADER                             
190300                 MOVE SPACE    TO WS-SPAR-TOTALRADER                      
190400                 MOVE 0        TO ANT-SID-IX                              
190500              END-IF                                                      
190600           END-IF                                                         
190700        END-IF                                                            
190800        ADD 1 TO ANT-LAGOMR-IX                                            
190900     END-PERFORM                                                          
191000                                                                          
191100     IF ANT-ETIK-IX > 0                                                   
191200        MOVE SPACE          TO TOTRAD3-KOMMA-ENG (ANT-ETIK-IX)            
191300        ADD 1               TO ANT-SID-IX                                 
191400        MOVE PLE-TOTRAD3-ENG TO WS-SPAR-TOTALRAD (ANT-SID-IX)             
191500     END-IF                                                               
191600                                                                          
191700     IF TOTAL-EJ-SKRIVEN                                                  
191800        PERFORM S05-SKRIV-TOTALRADER                                      
191900     END-IF                                                               
192000     .                                                                    
192100     EJECT                                                                
192200 CBAFA-NOLLSTAL-PLE-TOTRAD3-JAP SECTION.                                  
192300                                                                          
192400     MOVE 1 TO ANT-ETIK-IX                                                
192500                                                                          
192600     PERFORM UNTIL ANT-ETIK-IX > 5                                        
192700        MOVE ZERO  TO TOTRAD3-ADLAGOMR-JAP  (ANT-ETIK-IX)                 
192800                      TOTRAD3-ANTAL-JAP     (ANT-ETIK-IX)                 
192900        MOVE SPACE TO TOTRAD3-KOMMA-JAP     (ANT-ETIK-IX)                 
193000        ADD 1 TO ANT-ETIK-IX                                              
193100     END-PERFORM                                                          
193200     .                                                                    
193300     EJECT                                                                
193400                                                                          
193500 CBB-REDIGERA-TOTAL SECTION.                                              
193600     MOVE 'CBB-REDIGERA-TOTAL'    TO WS-CURRENT-SECTION                   
193700                                                                          
193800     PERFORM CBBA-FLYTTA-TOTAL-ETIK                                       
193900                                                                          
194000     PERFORM CBBB-NOLLSTAL-LASER-TOTRAD                                   
194100                                                                          
194200     MOVE 1 TO ANT-LAGOMR-IX                                              
194300     MOVE 0 TO ANT-PICKUP-IX                                              
194400                                                                          
194500     PERFORM UNTIL ANT-LAGOMR-IX > MAX-LAGOMR                             
194600        IF 4004-KVRADER (ANT-LAGOMR-IX) > 0                               
194700           ADD 1    TO ANT-PICKUP-IX                                      
194800                                                                          
194900           MOVE 'Antal '      TO TOTAL-ANTAL-TEXT                         
195000           MOVE ANT-LAGOMR-IX TO LAGOMR-IX                                
195100           MOVE LAGOMR-IX TO TOTAL-ADLAGOMR(ANT-PICKUP-IX)                
195200           MOVE 4004-KVRADER  (ANT-LAGOMR-IX)                             
195300             TO TOTAL-KVANTAL (ANT-PICKUP-IX)                             
195400           MOVE ', ' TO TOTAL-KOMMA-TEXT(ANT-PICKUP-IX)                   
195500                                                                          
195600           IF ANT-PICKUP-IX = 8                                           
195700             PERFORM S08-SKRIV-TOTALRAD                                   
195800*                                                                         
195900             ADD 1               TO ANT-RAD-IX                            
196000             IF ANT-RAD-IX = 15                                           
196100               MOVE +1           TO ANT-RAD-IX                            
196200               MOVE '          ' TO LINE-IDAFPRCD                         
196300               MOVE '          ' TO WS-IDAFPRCD                           
196400             END-IF                                                       
196500*                                                                         
196600             PERFORM CBBB-NOLLSTAL-LASER-TOTRAD                           
196700             MOVE 0           TO ANT-PICKUP-IX                            
196800           END-IF                                                         
196900        END-IF                                                            
197000        ADD 1 TO ANT-LAGOMR-IX                                            
197100     END-PERFORM                                                          
197200                                                                          
197300     MOVE +0                      TO ANT-RAD-IX                           
197400     MOVE '          '            TO LINE-IDAFPRCD                        
197500     MOVE '          '            TO WS-IDAFPRCD                          
197600                                                                          
197700     MOVE 'END CBB-REDIGERA-TOTAL' TO WS-CURRENT-SECTION                  
197800     .                                                                    
197900     EJECT                                                                
198000 CBBA-FLYTTA-TOTAL-ETIK       SECTION.                                    
198100     MOVE 'CBBA-FLYTTA-TOTAL-ETIK'  TO WS-CURRENT-SECTION                 
198200                                                                          
198300     MOVE '3'                     TO TOTAL-IDAFPRCD                       
198400                                                                          
198500     MOVE 4006-IDLOPNR-PL         TO TOTAL-IDLOPNR-PL                     
198600     MOVE 4006-IDLOPNR-ORD        TO TOTAL-IDLOPNR-ORD                    
198700     MOVE 4006-IDPRC              TO TOTAL-IDPRC                          
198800                                                                          
198900     MOVE 4004-KVRADER (100)      TO TOTAL-KVRADER                        
199000                                                                          
199100     MOVE WS-DATUM                TO TOTAL-TIPRTDAT                       
199200     MOVE WS-TIHHMM               TO TOTAL-TIPRTTID                       
199300     .                                                                    
199400     EJECT                                                                
199500 CBBB-NOLLSTAL-LASER-TOTRAD    SECTION.                                   
199600     MOVE 'CBBB-NOLLSTAL-LASER-TOTRAD' TO WS-CURRENT-SECTION              
199700                                                                          
199800     MOVE 1 TO ANT-PICKUP-IX                                              
199900                                                                          
200000     PERFORM UNTIL ANT-PICKUP-IX > 8                                      
200100        MOVE ZERO      TO TOTAL-ADLAGOMR  (ANT-PICKUP-IX)                 
200200        MOVE ZERO      TO TOTAL-KVANTAL   (ANT-PICKUP-IX)                 
200300        MOVE SPACE     TO TOTAL-KOMMA-TEXT(ANT-PICKUP-IX)                 
200400        ADD 1 TO ANT-PICKUP-IX                                            
200500     END-PERFORM                                                          
200600                                                                          
200700     MOVE SPACE        TO TOTAL-ANTAL-TEXT                                
200800     .                                                                    
200900     EJECT                                                                
201000                                                                          
201100 CC-REDIGERA-PRINTRADER SECTION.                                          
201200     MOVE 'CC-REDIGERA-PRINTRADER '    TO WS-CURRENT-SECTION              
201300                                                                          
201400     EVALUATE TRUE                                                        
201500       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
201600         MOVE 4006-ADLAGOMR-ORD  TO RAD1-ADLAGOMR-JAP                     
201700         MOVE 4006-ADGANG        TO RAD1-ADGANG-JAP                       
201800       WHEN DCS-NDC-NA                                                    
201900         MOVE 4006-ADLAGOMR-ORD  TO RAD1-ADLAGOMR-NDC                     
202000         MOVE 4006-ADGANG        TO RAD1-ADGANG-NDC                       
202100       WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                               
202200         MOVE 4006-ADLAGOMR-ORD  TO RAD1-ADLAGOMR-LDC-SE                  
202300         MOVE 4006-ADGANG        TO RAD1-ADGANG-LDC-SE                    
202400       WHEN OTHER                                                         
202500         MOVE 4006-ADLAGOMR-ORD  TO RAD1-ADLAGOMR                         
202600         IF DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
202700         AND (ANT-ETIK-IX = +1 OR ANT-ETIK-IX = +2)                       
202800           MOVE 4006-ADLAGOMR-ORD TO RAD2-RUBRADREF                       
202900         END-IF                                                           
203000         MOVE 4006-ADGANG        TO RAD1-ADGANG                           
203100     END-EVALUATE                                                         
203200*                                                                         
203300     IF ((DCS-CDC OR                                                      
203400         (DCS-SDC AND DCS-IDLANDX2 = 'SE'))                               
203500        AND                                                               
203600       (RAD1-ADLAGOMR = 10 OR 20 OR 21 OR 25 OR 71))                      
203700       MOVE 4006-ADPLATS-ORD     TO RAD1-ADPLATS                          
203800     ELSE                                                                 
203900       EVALUATE TRUE                                                      
204000         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
204100           MOVE 4006-ADPLATS-ORD TO RAD1-ADPLATS-JAP                      
204200         WHEN DCS-NDC-NA                                                  
204300           MOVE 4006-ADPLATS-ORD TO RAD1-ADPLATS-NDC                      
204400         WHEN OTHER                                                       
204500           MOVE 4006-ADPLATS-ORD TO RAD1-ADPLATS                          
204600       END-EVALUATE                                                       
204700     END-IF                                                               
204800                                                                          
204900     IF DCS-CDC OR                                                        
205000       (DCS-SDC AND (DCS-IDLANDX2 = 'NL' OR 'GB' OR 'ES' OR 'IT'))        
205100       INSPECT RAD1-ADPLATS REPLACING LEADING ZERO BY SPACE               
205200     END-IF                                                               
205300                                                                          
205400     IF 4006-FLAKPLOC = JA                                                
205500       EVALUATE TRUE                                                      
205600         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
205700           MOVE '*'              TO RAD1-FLAKPLOC-JAP                     
205800         WHEN DCS-NDC-NA                                                  
205900           MOVE '*'              TO RAD1-FLAKPLOC-NDC                     
206000         WHEN OTHER                                                       
206100           MOVE '*'              TO RAD1-FLAKPLOC                         
206200       END-EVALUATE                                                       
206300     ELSE                                                                 
206400       EVALUATE TRUE                                                      
206500         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
206600           MOVE SPACE            TO RAD1-FLAKPLOC-JAP                     
206700         WHEN DCS-NDC-NA                                                  
206800           MOVE SPACE            TO RAD1-FLAKPLOC-NDC                     
206900         WHEN OTHER                                                       
207000           MOVE SPACE            TO RAD1-FLAKPLOC                         
207100       END-EVALUATE                                                       
207200     END-IF                                                               
207300                                                                          
207400     EVALUATE TRUE                                                        
207500       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
207600         MOVE 4006-IDARTNR       TO RAD1-IDARTNR-JAP                      
207700         MOVE 4006-BEART         TO RAD1-BEART-JAP                        
207800         MOVE 4006-KVAVBART      TO RAD1-KVAVBART-JAP                     
207900       WHEN DCS-NDC-NA                                                    
208000         MOVE 4006-IDARTNR       TO RAD1-IDARTNR-NDC                      
208100         MOVE 4006-BEART         TO RAD1-BEART-NDC                        
208200         MOVE 4006-KVAVBART      TO RAD1-KVAVBART-NDC                     
208300       WHEN OTHER                                                         
208400         MOVE 4006-IDARTNR       TO RAD1-IDARTNR                          
208500         MOVE 4006-BEART         TO RAD1-BEART                            
208600                                                                          
208700         MOVE 4006-IDDISTR       TO DIST13-IDDISTR                        
208800         IF  DCS-CDC                                                      
208900         AND DIST13-SVERIGE                                               
209000           MOVE 4006-KVAVBART    TO WS-KVAVBART                           
209100           MOVE 000000           TO RAD1-KVAVBART                         
209200           EVALUATE TRUE                                                  
209300           WHEN WS-KVAVBART(1:1) > 0                                      
209400             MOVE 4006-KVAVBART  TO RAD1-KVAVBART                         
209500           WHEN WS-KVAVBART(2:1) > 0                                      
209600             MOVE 4006-KVAVBART  TO RAD1-KVAVBART                         
209700           WHEN WS-KVAVBART(3:1) > 0                                      
209800             MOVE WS-KVAVBART(3:1)  TO RAD1-KVAVBART(2:1)                 
209900             MOVE WS-KVAVBART(4:1)  TO RAD1-KVAVBART(3:1)                 
210000             MOVE WS-KVAVBART(5:1)  TO RAD1-KVAVBART(4:1)                 
210100             MOVE WS-KVAVBART(6:1)  TO RAD1-KVAVBART(5:1)                 
210200           WHEN WS-KVAVBART(4:1) > 0                                      
210300             MOVE WS-KVAVBART(4:1)  TO RAD1-KVAVBART(3:1)                 
210400             MOVE WS-KVAVBART(5:1)  TO RAD1-KVAVBART(4:1)                 
210500             MOVE WS-KVAVBART(6:1)  TO RAD1-KVAVBART(5:1)                 
210600           WHEN WS-KVAVBART(5:1) > 0                                      
210700             MOVE WS-KVAVBART(5:1)  TO RAD1-KVAVBART(4:1)                 
210800             MOVE WS-KVAVBART(6:1)  TO RAD1-KVAVBART(5:1)                 
210900           WHEN WS-KVAVBART(6:1) > 0                                      
211000             MOVE WS-KVAVBART(6:1)  TO RAD1-KVAVBART(4:1)                 
211100           END-EVALUATE                                                   
211200         ELSE                                                             
211300             MOVE 4006-KVAVBART  TO RAD1-KVAVBART                         
211400         END-IF                                                           
211500     END-EVALUATE                                                         
211600                                                                          
211700     IF DCS-NDC-NA                                                        
211800       PERFORM CCA-KONVERTERA-KDSORT                                      
211900     ELSE                                                                 
212000       MOVE SPACE                TO RAD1-GRP                              
212100       MOVE 4006-KDSORT          TO RAD1-KDSORT                           
212200       MOVE 4006-KDARTURS        TO RAD1-KDARTURS                         
212300     END-IF                                                               
212400                                                                          
212500     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
212600                                    DIST35-IDDISTR                        
212700                                                                          
212800     MOVE SPACE                    TO PLE-RAD2                            
212900                                                                          
213000     EVALUATE TRUE                                                        
213100     WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                              
213200       IF DIST35-PACIFIC-TRANSFER                                         
213300         IF (4006-BERADREF = SPACE OR                                     
213400             4006-BERADREF = ZERO)                                        
213500           MOVE 'NEW '             TO RAD2-BERADREF-JAP                   
213600         ELSE                                                             
213700           MOVE 4006-BERADREF      TO WS-BERADREF                         
213800           MOVE WS-ADLAGOMR        TO WS-ADLAGOMR-RED                     
213900           MOVE WS-ADGANG          TO WS-ADGANG-RED                       
214000           MOVE WS-ADPLATS         TO WS-ADPLATS-RED                      
214100           MOVE WS-BERADREF-RED    TO RAD2-BERADREF-JAP                   
214200         END-IF                                                           
214300       ELSE                                                               
214400         MOVE 4006-BERADREF        TO RAD2-BERADREF-JAP                   
214500       END-IF                                                             
214600       MOVE '*COPY*'               TO RAD2-COPY-JAP                       
214700                                                                          
214800       MOVE 4006-KDARTURS          TO ARTU-KDARTURS                       
214900       MOVE 4006-IDDISTR           TO ARTU-IDDISTR                        
215000       MOVE MID-IDDC               TO ARTU-IDDC                           
215100       CALL W400ARTU USING ARTU-W400ARTU                                  
215200       MOVE ARTU-BEARTURS-ENG      TO RAD2-BEARTURS-JAP                   
215300                                                                          
215400       MOVE 4006-IDPSN             TO RAD2-IDPSN-JAP                      
215500       MOVE 4006-IDSPECEMB         TO RAD2-IDSPECEMB-JAP                  
215600       INSPECT RAD2-IDSPECEMB-JAP REPLACING ALL ZEROES BY SPACE           
215700       MOVE 4006-IDBORD            TO RAD2-IDBORD-JAP                     
215800     WHEN DCS-NDC-NA                                                      
215900       IF DIST35-NA-TRANSFER                                              
216000       OR DIST35-NA-NDC-RETURNS                                           
216100         IF (4006-BERADREF = SPACE OR                                     
216200             4006-BERADREF = ZERO)                                        
216300           MOVE 'NEW '             TO RAD2-BERADREF-NDC                   
216400         ELSE                                                             
216500           MOVE 4006-BERADREF      TO WS-BERADREF                         
216600           MOVE WS-ADLAGOMR        TO WS-ADLAGOMR-RED                     
216700           MOVE WS-ADGANG          TO WS-ADGANG-RED                       
216800           MOVE WS-ADPLATS         TO WS-ADPLATS-RED                      
216900           MOVE WS-BERADREF-RED    TO RAD2-BERADREF-NDC                   
217000         END-IF                                                           
217100       ELSE                                                               
217200         MOVE 4006-IDDISTR         TO DIST35-IDDISTR                      
217300         IF DIST35-REFILL-NA                                              
217400           MOVE SPACE              TO RAD2-BERADREF-NDC                   
217500                                      WS-BERADREF                         
217600                                      RAD2-BERADREF-US                    
217700                                      RAD2-BERADREF                       
217800         ELSE                                                             
217900           MOVE 4006-BERADREF      TO RAD2-BERADREF-NDC                   
218000         END-IF                                                           
218100       END-IF                                                             
218200       MOVE '*COPY*'               TO RAD2-COPY-NDC                       
218300                                                                          
218400       MOVE 4006-KDARTURS          TO ARTU-KDARTURS                       
218500       MOVE 4006-IDDISTR           TO ARTU-IDDISTR                        
218600       MOVE MID-IDDC               TO ARTU-IDDC                           
218700       CALL W400ARTU USING ARTU-W400ARTU                                  
218800       MOVE ARTU-BEARTURS-ENG      TO RAD2-BEARTURS-NDC                   
218900                                                                          
219000       MOVE 4006-IDPSN             TO RAD2-IDPSN-NDC                      
219100       MOVE 4006-IDSPECEMB         TO RAD2-IDSPECEMB-NDC                  
219200       INSPECT RAD2-IDSPECEMB-NDC REPLACING ALL ZEROES BY SPACE           
219300       MOVE 4006-IDBORD            TO RAD2-IDBORD-NDC                     
219400     WHEN OTHER                                                           
219500       IF DIST35-REFILL            OR                                     
219600          DIST35-NONVCC-CDC-REFILL                                        
219700         MOVE 4006-IDDISTR         TO DIST35-IDDISTR                      
219800         IF DIST35-REFILL-NA                                              
219900           MOVE SPACE              TO RAD2-BERADREF-NDC                   
220000           MOVE SPACE              TO RAD2-BERADREF-US                    
220100                                      WS-BERADREF                         
220200                                      RAD2-BERADREF                       
220300         ELSE                                                             
220400           IF (4006-BERADREF = SPACE OR                                   
220500               4006-BERADREF = ZERO)                                      
220600             MOVE 'NEW '           TO RAD2-RUBRADREF                      
220700             MOVE SPACE            TO RAD2-BERADREF                       
220800           ELSE                                                           
220900             MOVE 'L'              TO RAD2-RUBRADREF-US                   
221000             MOVE 4006-BERADREF    TO WS-BERADREF                         
221100             MOVE WS-ADLAGOMR      TO WS-ADLAGOMR-RED                     
221200             MOVE WS-ADGANG        TO WS-ADGANG-RED                       
221300             MOVE WS-ADPLATS       TO WS-ADPLATS-RED                      
221400             MOVE WS-BERADREF-RED  TO RAD2-BERADREF-US                    
221500             MOVE SPACE            TO FILLER1                             
221600           END-IF                                                         
221700         END-IF                                                           
221800       ELSE                                                               
221900         IF DCS-CDC OR                                                    
222000           (DCS-SDC AND DCS-IDLANDX2 NOT = 'GB')                          
222100           MOVE 4006-BERADREF      TO RAD2-BERADREF                       
222200*IDDEPT                                                                   
222300           MOVE 4006-IDDISTR       TO DIST13-IDDISTR                      
222400           IF DCS-CDC                                                     
222500           AND DIST13-SVERIGE                                             
222600           AND 4006-IDDEPT > ZERO                                         
222700*LDC - KONTROLL AV LDC-KUND FINNS I W4037500                              
222800             MOVE 4006-BERADREF    TO RAD2-BERADREF-DEPT                  
222900             MOVE SPACE            TO RAD2-FILLER-DEPT                    
223000             MOVE 'D'              TO RAD2-IDDEPT-PRE-D                   
223100             MOVE 4006-IDDEPT      TO RAD2-IDDEPT                         
223200                                                                          
223300             MOVE 1 TO TECKEN-IX                                          
223400             PERFORM UNTIL TECKEN-IX > 10                                 
223500               IF RAD2-BERADREF-DEPT(TECKEN-IX:1) < SPACE                 
223600                 MOVE SPACE TO RAD2-BERADREF-DEPT(TECKEN-IX:1)            
223700               END-IF                                                     
223800               ADD 1 TO TECKEN-IX                                         
223900             END-PERFORM                                                  
224000           ELSE                                                           
224100                                                                          
224200             MOVE 1 TO TECKEN-IX                                          
224300             PERFORM UNTIL TECKEN-IX > 10                                 
224400               IF RAD2-BERADREF(TECKEN-IX:1) < SPACE                      
224500                 MOVE SPACE TO RAD2-BERADREF(TECKEN-IX:1)                 
224600               END-IF                                                     
224700               ADD 1 TO TECKEN-IX                                         
224800             END-PERFORM                                                  
224900           END-IF                                                         
225000         ELSE                                                             
225100           IF DIST34-ENGLAND-SDC                                          
225200             MOVE 4006-BERADREF    TO RAD2-BERADREF                       
225300*LDC-GB  INSPECT RAD2-BERADREF REPLACING LEADING ZERO BY SPACE            
225400           ELSE                                                           
225500             CONTINUE                                                     
225600           END-IF                                                         
225700         END-IF                                                           
225800       END-IF                                                             
225900                                                                          
226000       MOVE '*COPY*'               TO RAD2-COPY                           
226100       MOVE '*COPY*'               TO RAD2-COPY-US                        
226200       MOVE '*COPY*'               TO RAD2-COPY-DEPT                      
226300                                                                          
226400       IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                                 
226500         MOVE SPACE                TO FILLER-ZON                          
226600         MOVE 4006-IDZON           TO RAD2-IDZON                          
226700       ELSE                                                               
226800         MOVE 4006-KDARTHNT        TO RAD2-KDARTHNT                       
226900       END-IF                                                             
227000                                                                          
227100       IF DCS-SDC AND DCS-IDLANDX2 = 'NL'                                 
227200         MOVE 4006-IDPSN           TO RAD2-IDPSN                          
227300       ELSE                                                               
227400         MOVE SPACE                TO FILLER-FARL                         
227500         MOVE 4006-KDFARLIG        TO RAD2-KDFARLIG                       
227600       END-IF                                                             
227700                                                                          
227800       MOVE 4006-KDEMBAL           TO RAD2-KDEMBAL                        
227900       IF 4006-FLLDCKND = JA                                              
228000         IF 4006-TIRFSDAT = ZERO                                          
228100           MOVE SPACE              TO RAD2-IDSPECEMB                      
228200         ELSE                                                             
228300           MOVE 4006-TIRFSDAT      TO RAD2-IDSPECEMB                      
228400         END-IF                                                           
228500       ELSE                                                               
228600         MOVE 4006-IDSPECEMB     TO RAD2-IDSPECEMB                        
228700       INSPECT RAD2-IDSPECEMB REPLACING ALL ZEROES BY SPACE               
228800       END-IF                                                             
228900       MOVE 4006-IDBORD            TO RAD2-IDBORD                         
229000     END-EVALUATE                                                         
229100                                                                          
229200     EVALUATE TRUE                                                        
229300       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
229400         MOVE 4006-IDDISTR       TO RAD3-IDDISTR-JAP                      
229500         MOVE 4006-IDKUNDNR      TO RAD3-IDKUNDNR-JAP                     
229600         MOVE 4006-IDPRODNR      TO RAD3-IDPRODNR-JAP                     
229700         MOVE 4006-IDPLKLST      TO RAD3-IDPLKLST-JAP                     
229800         MOVE 4006-IDKUNDRF      TO RAD3-IDKUNDRF-JAP                     
229900         INSPECT RAD3-IDKUNDRF-JAP REPLACING LEADING ZERO BY SPACE        
230000         MOVE 4006-IDRADNR       TO RAD3-IDRADNR-JAP                      
230100       WHEN DCS-NDC-NA                                                    
230200         MOVE 4006-IDDISTR       TO RAD3-IDDISTR-NDC                      
230300         MOVE 4006-IDKUNDNR      TO RAD3-IDKUNDNR-NDC                     
230400         MOVE 4006-IDPRODNR      TO RAD3-IDPRODNR-NDC                     
230500         MOVE 4006-IDPLKLST      TO RAD3-IDPLKLST-NDC                     
230600         MOVE 4006-IDKUNDRF      TO RAD3-IDKUNDRF-NDC                     
230700         INSPECT RAD3-IDKUNDRF-NDC REPLACING LEADING ZERO BY SPACE        
230800         MOVE 4006-IDRADNR       TO RAD3-IDRADNR-NDC                      
230900       WHEN OTHER                                                         
231000         MOVE 4006-IDDISTR       TO RAD3-IDDISTR                          
231100         MOVE 4006-IDKUNDNR      TO RAD3-IDKUNDNR                         
231200         MOVE 4006-IDPRODNR      TO RAD3-IDPRODNR                         
231300         MOVE 4006-IDPLKLST      TO RAD3-IDPLKLST                         
231400         MOVE 4006-IDKUNDRF      TO RAD3-IDKUNDRF                         
231500         INSPECT RAD3-IDKUNDRF REPLACING LEADING ZERO BY SPACE            
231600         MOVE 4006-IDRADNR       TO RAD3-IDRADNR                          
231700     END-EVALUATE                                                         
231800                                                                          
231900*FIX START, TILLFÄLLIG ÄNDRING TILLS ALLA SDC23-ÅF HAR BLIVIT             
232000*NEW CONCEPT ANSLUTNA. T.O.M. NOVEMBER 96?.                               
232100*    IF  SDC-GB                                                           
232200*    AND 4006-KDORDKL = 1                                                 
232300*    AND 4006-KDFRAKT = 31                                                
232400*      MOVE +3                   TO RAD3-KDORDKL                          
232500*    ELSE                                                                 
232600     EVALUATE TRUE                                                        
232700       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
232800         MOVE 4006-KDORDKL       TO RAD3-KDORDKL-JAP                      
232900       WHEN DCS-NDC-NA                                                    
233000         MOVE 4006-KDORDKL       TO RAD3-KDORDKL-NDC                      
233100       WHEN OTHER                                                         
233200         MOVE 4006-KDORDKL       TO RAD3-KDORDKL                          
233300     END-EVALUATE                                                         
233400*    END-IF                                                               
233500*FIX END,  TILLFÄLLIG ÄNDRING.                                            
233600                                                                          
233700     EVALUATE TRUE                                                        
233800       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
233900         MOVE 4006-IDPRC         TO RAD3-IDPRC-JAP                        
234000         MOVE 4006-IDLOPNR-PL    TO RAD3-IDLOPNR-PL-JAP                   
234100         MOVE 4006-IDLOPNR-ORD   TO WS-IDLOPNR-ORD                        
234200         MOVE WS-IDLOPNR-ORD     TO RAD3-IDLOPNR-ORD-JAP                  
234300      INSPECT RAD3-IDLOPNR-ORD-JAP REPLACING LEADING ZERO BY SPACE        
234400       WHEN DCS-NDC-NA                                                    
234500         MOVE 4006-IDPRC         TO RAD3-IDPRC-NDC                        
234600         MOVE 4006-IDLOPNR-PL    TO RAD3-IDLOPNR-PL-NDC                   
234700         MOVE 4006-IDLOPNR-ORD   TO WS-IDLOPNR-ORD                        
234800         MOVE WS-IDLOPNR-ORD     TO RAD3-IDLOPNR-ORD-NDC                  
234900      INSPECT RAD3-IDLOPNR-ORD-NDC REPLACING LEADING ZERO BY SPACE        
235000     WHEN OTHER                                                           
235100       MOVE 4006-IDPRC           TO RAD3-IDPRC                            
235200       MOVE 4006-IDLOPNR-PL      TO RAD3-IDLOPNR-PL                       
235300       MOVE 4006-IDLOPNR-ORD     TO WS-IDLOPNR-ORD                        
235400       MOVE SPACE                TO RAD3-STAR                             
235500       MOVE WS-IDLOPNR-ORD       TO RAD3-IDLOPNR-ORD                      
235600       INSPECT RAD3-IDLOPNR-ORD REPLACING LEADING ZERO BY SPACE           
235700     END-EVALUATE                                                         
235800                                                                          
235900     .                                                                    
236000     EJECT                                                                
236100 CCA-KONVERTERA-KDSORT    SECTION.                                        
236200                                                                          
236300     EVALUATE 4006-KDSORT                                                 
236400        WHEN 'ST'                                                         
236500              MOVE 'PCS  '       TO RAD1-KDSORT-NDC                       
236600        WHEN 'PA'                                                         
236700              MOVE 'PAIR '       TO RAD1-KDSORT-NDC                       
236800        WHEN 'MS'                                                         
236900              MOVE 'AVREG'       TO RAD1-KDSORT-NDC                       
237000        WHEN 'KG'                                                         
237100*             MOVE 'LBS  '       TO RAD1-KDSORT-NDC                       
237200              MOVE 'KG   '       TO RAD1-KDSORT-NDC                       
237300        WHEN 'M '                                                         
237400*             MOVE 'FT   '       TO RAD1-KDSORT-NDC                       
237500              MOVE 'METER'       TO RAD1-KDSORT-NDC                       
237600        WHEN 'L '                                                         
237700*             MOVE 'GAL  '       TO RAD1-KDSORT-NDC                       
237800              MOVE 'LITRE'       TO RAD1-KDSORT-NDC                       
237900        WHEN 'SA'                                                         
238000              MOVE 'KIT  '       TO RAD1-KDSORT-NDC                       
238100        WHEN 'MM'                                                         
238200*             MOVE 'INCH '       TO RAD1-KDSORT-NDC                       
238300              MOVE 'MM   '       TO RAD1-KDSORT-NDC                       
238400        WHEN 'G '                                                         
238500*             MOVE 'OZ   '       TO RAD1-KDSORT-NDC                       
238600              MOVE 'GRAM '       TO RAD1-KDSORT-NDC                       
238700        WHEN 'C2'                                                         
238800*             MOVE 'SQ.IN'       TO RAD1-KDSORT-NDC                       
238900              MOVE 'CM2  '       TO RAD1-KDSORT-NDC                       
239000        WHEN 'M2'                                                         
239100*             MOVE 'SQ.FT'       TO RAD1-KDSORT-NDC                       
239200              MOVE 'M2   '       TO RAD1-KDSORT-NDC                       
239300        WHEN 'M3'                                                         
239400*             MOVE 'QU.FT'       TO RAD1-KDSORT-NDC                       
239500              MOVE 'M3   '       TO RAD1-KDSORT-NDC                       
239600        WHEN 'ML'                                                         
239700*             MOVE 'CU.IN'       TO RAD1-KDSORT-NDC                       
239800              MOVE 'ML   '       TO RAD1-KDSORT-NDC                       
239900        WHEN 'RA'                                                         
240000              MOVE 'LINES'       TO RAD1-KDSORT-NDC                       
240100        WHEN 'SW'                                                         
240200              MOVE 'SOFTW'       TO RAD1-KDSORT-NDC                       
240300        WHEN 'HW'                                                         
240400              MOVE 'HARDW'       TO RAD1-KDSORT-NDC                       
240500        WHEN 'TM'                                                         
240600              MOVE 'TMO  '       TO RAD1-KDSORT-NDC                       
240700        WHEN OTHER                                                        
240800              MOVE SPACE         TO RAD1-KDSORT-NDC                       
240900     END-EVALUATE                                                         
241000     .                                                                    
241100     EJECT                                                                
241200 CD-SAETT-RADSKIP SECTION.                                                
241300     MOVE 'CD-SAETT-RADSKIP  '    TO WS-CURRENT-SECTION                   
241400                                                                          
241500     IF NY-PRINTER                                                        
241600        OR                                                                
241700        (4006-KDSS-PLE NOT = SPAR-KDSS-PLE)                               
241800        OR                                                                
241900        (ETIKETT-IX = MAX-ETIKETT)                                        
242000        IF ETIKETT-IX = 1 OR 3 OR 5                                       
242100           PERFORM S06-KOLLA-SKRIVKLAR                                    
242200        END-IF                                                            
242300        MOVE PRT-NYSIDA-RAD1  TO PRT-RADSKIP                              
242400     ELSE                                                                 
242500        IF SPAR-KDSS-PLE = SPACE                                          
242600           AND                                                            
242700           4006-ADLAGOMR NOT = SPAR-ADLAGOMR                              
242800           IF ETIKETT-IX = 1 OR 3 OR 5                                    
242900              PERFORM S06-KOLLA-SKRIVKLAR                                 
243000           END-IF                                                         
243100           PERFORM CDA-KOLLA-ETIKETT-IX                                   
243200        ELSE                                                              
243300           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
243400        END-IF                                                            
243500     END-IF                                                               
243600                                                                          
243700     IF PRT-RADSKIP = PRT-NYSIDA-RAD1                                     
243800        MOVE 0          TO ETIKETT-IX                                     
243900        ADD  1          TO SID-IX                                         
244000     END-IF                                                               
244100     MOVE NEJ           TO PRINTER-SW                                     
244200     MOVE 4006-KDSS-PLE TO SPAR-KDSS-PLE                                  
244300     MOVE 4006-ADLAGOMR TO SPAR-ADLAGOMR                                  
244400     .                                                                    
244500     EJECT                                                                
244600 CDA-KOLLA-ETIKETT-IX SECTION.                                            
244700                                                                          
244800     IF ETIKETT-IX = 1                                                    
244900        MOVE 11 TO PRT-RADSKIP                                            
245000        ADD 3 TO ETIKETT-IX                                               
245100     ELSE                                                                 
245200        IF ETIKETT-IX = 2                                                 
245300           MOVE 11 TO PRT-RADSKIP                                         
245400           ADD 2 TO ETIKETT-IX                                            
245500        ELSE                                                              
245600           MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                            
245700        END-IF                                                            
245800     END-IF                                                               
245900     .                                                                    
246000     EJECT                                                                
246100 CE-SKRIV-ETIKETT SECTION.                                                
246200                                                                          
246300     IF ETIKETT-IX = 0 OR 2 OR 4                                          
246400       MOVE PRT-RADSKIP TO SPAR-RADSKIP                                   
246500       EVALUATE TRUE                                                      
246600         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
246700           MOVE PLE-RAD1-JAP TO JAP-RAD-V (1)                             
246800           MOVE PLE-RAD2-JAP TO JAP-RAD-V (2)                             
246900           MOVE PLE-RAD3-JAP TO JAP-RAD-V (3)                             
247000         WHEN DCS-NDC-NA                                                  
247100           MOVE PLE-RAD1-NDC TO NDC-RAD-V (1)                             
247200           MOVE PLE-RAD2-NDC TO NDC-RAD-V (2)                             
247300           MOVE PLE-RAD3-NDC TO NDC-RAD-V (3)                             
247400         WHEN OTHER                                                       
247500           MOVE PLE-RAD1     TO ARB-RAD-V (1)                             
247600           MOVE PLE-RAD2     TO ARB-RAD-V (2)                             
247700           MOVE PLE-RAD3     TO ARB-RAD-V (3)                             
247800       END-EVALUATE                                                       
247900       MOVE  JA TO SKRIVKLAR-SW                                           
248000     ELSE                                                                 
248100       EVALUATE TRUE                                                      
248200         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
248300           MOVE PLE-RAD1-JAP TO JAP-RAD-H (1)                             
248400           MOVE PLE-RAD2-JAP TO JAP-RAD-H (2)                             
248500           MOVE PLE-RAD3-JAP TO JAP-RAD-H (3)                             
248600         WHEN DCS-NDC-NA                                                  
248700           MOVE PLE-RAD1-NDC TO NDC-RAD-H (1)                             
248800           MOVE PLE-RAD2-NDC TO NDC-RAD-H (2)                             
248900           MOVE PLE-RAD3-NDC TO NDC-RAD-H (3)                             
249000         WHEN OTHER                                                       
249100           MOVE PLE-RAD1     TO ARB-RAD-H (1)                             
249200           MOVE PLE-RAD2     TO ARB-RAD-H (2)                             
249300           MOVE PLE-RAD3     TO ARB-RAD-H (3)                             
249400       END-EVALUATE                                                       
249500       MOVE SPAR-RADSKIP TO PRT-RADSKIP                                   
249600       EVALUATE TRUE                                                      
249700         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
249800           MOVE JAP-RAD (1) TO WS-PLE-RAD                                 
249900         WHEN DCS-NDC-NA                                                  
250000           MOVE NDC-RAD (1) TO WS-PLE-RAD                                 
250100         WHEN OTHER                                                       
250200           MOVE ARB-RAD (1) TO WS-PLE-RAD                                 
250300       END-EVALUATE                                                       
250400       PERFORM S04-SKRIV-RAD                                              
250500                                                                          
250600       EVALUATE TRUE                                                      
250700         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
250800           MOVE JAP-RAD (2) TO WS-PLE-RAD                                 
250900         WHEN DCS-NDC-NA                                                  
251000           MOVE NDC-RAD (2) TO WS-PLE-RAD                                 
251100         WHEN OTHER                                                       
251200           MOVE ARB-RAD (2) TO WS-PLE-RAD                                 
251300       END-EVALUATE                                                       
251400       MOVE PRT-AFTER-3 TO PRT-RADSKIP                                    
251500       PERFORM S04-SKRIV-RAD                                              
251600                                                                          
251700       EVALUATE TRUE                                                      
251800         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
251900           MOVE JAP-RAD (3) TO WS-PLE-RAD                                 
252000         WHEN DCS-NDC-NA                                                  
252100           MOVE NDC-RAD (3) TO WS-PLE-RAD                                 
252200         WHEN OTHER                                                       
252300           MOVE ARB-RAD (3) TO WS-PLE-RAD                                 
252400       END-EVALUATE                                                       
252500       MOVE PRT-AFTER-2 TO PRT-RADSKIP                                    
252600       PERFORM S04-SKRIV-RAD                                              
252700                                                                          
252800       MOVE SPACE TO WS-PLE-RAD                                           
252900       MOVE NEJ   TO SKRIVKLAR-SW                                         
253000     END-IF                                                               
253100     ADD 1          TO ETIKETT-IX                                         
253200     .                                                                    
253300     EJECT                                                                
253400 CG-FLYTTA-LASERDATA          SECTION.                                    
253500     MOVE 'CG-FLYTTA-LASERDATA'  TO WS-CURRENT-SECTION                    
253600                                                                          
253700     ADD 1                       TO ANT-RAD-IX                            
253800     MOVE ANT-RAD-IX             TO ANT-RAD-IX-WS                         
253900*                                                                         
254000     IF ANT-RAD-IX = 15                                                   
254100       MOVE +1                   TO ANT-RAD-IX                            
254200       MOVE '          '         TO LINE-IDAFPRCD                         
254300       MOVE '          '         TO WS-IDAFPRCD                           
254400     END-IF                                                               
254500*                                                                         
254600     EVALUATE WS-IDAFPRCD                                                 
254700       WHEN '          '                                                  
254800         MOVE '0         '       TO LINE-IDAFPRCD                         
254900         MOVE '0         '       TO WS-IDAFPRCD                           
255000       WHEN '0         '                                                  
255100         MOVE '2         '       TO LINE-IDAFPRCD                         
255200         MOVE '2         '       TO WS-IDAFPRCD                           
255300       WHEN '1         '                                                  
255400         MOVE '2         '       TO LINE-IDAFPRCD                         
255500         MOVE '2         '       TO WS-IDAFPRCD                           
255600       WHEN '2         '                                                  
255700         MOVE '1         '       TO LINE-IDAFPRCD                         
255800         MOVE '1         '       TO WS-IDAFPRCD                           
255900     END-EVALUATE                                                         
256000                                                                          
256100     MOVE 4006-ADLAGOMR-ORD      TO LINE-ADLAGOMR                         
256200     MOVE 4006-ADGANG            TO LINE-ADGANG                           
256300                                                                          
256400     MOVE 4006-ADPLATS-ORD       TO WS-ADPLATS-ORD                        
256500     MOVE WS-ADPLATSNR           TO LINE-ADPLATSNR                        
256600     MOVE WS-ADPLNIV(1:1)        TO LINE-ADPLNIV-LEFT                     
256700     MOVE WS-ADPLNIV(2:1)        TO LINE-ADPLNIV-RIGHT                    
256800                                                                          
256900     IF 4006-FLAKPLOC = JA                                                
257000         MOVE '*'                TO LINE-FLAKPLOC                         
257100     ELSE                                                                 
257200         MOVE SPACE              TO LINE-FLAKPLOC                         
257300     END-IF                                                               
257400                                                                          
257500     MOVE 4006-IDARTNR           TO LINE-IDARTNR                          
257600     MOVE 4006-BEART             TO LINE-BEART                            
257700     MOVE 4006-KVAVBART          TO LINE-KVAVBART                         
257800                                                                          
257900     MOVE 4006-KDSORT            TO LINE-KDSORT                           
258000     MOVE 4006-KDARTURS          TO LINE-KDARTURS                         
258100                                                                          
258200     MOVE 4006-IDDISTR           TO DIST35-IDDISTR                        
258300     IF DIST35-REFILL-NA                                                  
258400       MOVE SPACE                TO LINE-BERADREF                         
258500     ELSE                                                                 
258600       MOVE 4006-BERADREF        TO LINE-BERADREF                         
258700     END-IF                                                               
258800                                                                          
258900     IF 4006-IDSYSTEM (1:3) = 'LYN'                                       
259000        MOVE 4006-IDARTNR        TO W-IDARTNR                             
259100        PERFORM IMS-GU-WDF502                                             
259200        IF SEGMENT-FINNS                                                  
259300          MOVE XLEV-IDLEVART     TO LINE-IDLEVART                         
259400        END-IF                                                            
259500     ELSE                                                                 
259600        MOVE SPACE               TO LINE-IDLEVART                         
259700     END-IF                                                               
259800                                                                          
259900     MOVE 1 TO TECKEN-IX                                                  
260000     PERFORM UNTIL TECKEN-IX > 10                                         
260100        IF LINE-BERADREF(TECKEN-IX:1) < SPACE                             
260200           MOVE SPACE TO LINE-BERADREF(TECKEN-IX:1)                       
260300        END-IF                                                            
260400        ADD      1 TO TECKEN-IX                                           
260500     END-PERFORM                                                          
260600                                                                          
260700     IF DCS-NDC-NA                                                        
260800       MOVE 4006-IDPSN           TO LINE-KDARTHNT                         
260900     ELSE                                                                 
261000       MOVE 4006-KDARTHNT        TO LINE-KDARTHNT                         
261100     END-IF                                                               
261200     MOVE 4006-KDFARLIG          TO LINE-KDFARLIG                         
261300                                                                          
261400     MOVE 4006-TIRFSDAT          TO LINE-TIRFSDAT                         
261500                                                                          
261600*    MOVE 4006-KDEMBAL           TO LINE-KDEMBAL                          
261700                                                                          
261800     MOVE 4006-IDBORD            TO LINE-IDBORD                           
261900                                                                          
262000     MOVE '*COPY*'               TO LINE-COPY                             
262100                                                                          
262200     MOVE 4006-IDDISTR           TO LINE-IDDISTR                          
262300     MOVE 4006-IDKUNDNR          TO LINE-IDKUNDNR                         
262400     MOVE 4006-IDPRODNR          TO LINE-IDPRODNR                         
262500     MOVE 4006-IDPLKLST          TO LINE-IDPLKLST                         
262600*    MOVE 4006-IDKUNDRF          TO LINE-IDKUNDRF                         
262700     MOVE 4006-IDKUNDRF(3:5)     TO LINE-IDORDNR5                         
262800*    INSPECT LINE-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
262900     MOVE 4006-IDRADNR           TO LINE-IDRADNR                          
263000                                                                          
263100     MOVE 4006-KDORDKL           TO LINE-KDORDKL                          
263200                                                                          
263300     MOVE 4006-IDPRC             TO LINE-IDPRC                            
263400     MOVE 4006-IDLOPNR-PL        TO LINE-IDLOPNR-PL                       
263500     MOVE 4006-IDLOPNR-ORD       TO WS-IDLOPNR-ORD                        
263600     MOVE WS-IDLOPNR-ORD         TO LINE-IDLOPNR-ORD                      
263700*    MOVE SPACE                  TO LINE-FILLERX2                         
263800     .                                                                    
263900     EJECT                                                                
264000                                                                          
264100 CZ-SET-LINE-SKIP-FOR-LASER   SECTION.                                    
264200     MOVE 'CZ-SET-LINE-SKIP-FOR-LASER'  TO WS-CURRENT-SECTION             
264300*LASER                                                                    
264400*LASER                                                                    
264500*LASER                                                                    
264600     MOVE ANT-RAD-IX             TO ANT-RAD-IX-WS                         
264700*ADLAGOMR                                                                 
264800     MOVE 4006-ADLAGOMR          TO WS-ADLAGOMR                           
264900     MOVE SPAR-ADLAGOMR          TO WS-ADLAGOMR                           
265000                                                                          
265100     IF NY-PRINTER                                                        
265200     OR (4006-KDSS-PLE NOT = SPAR-KDSS-PLE)                               
265300     OR (ANT-RAD-IX = 15)                                                 
265400                                                                          
265500       MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                           
265600       MOVE +1                   TO ANT-RAD-IX                            
265700       MOVE '0         '         TO LINE-IDAFPRCD                         
265800       MOVE '0         '         TO WS-IDAFPRCD                           
265900     ELSE                                                                 
266000        IF SPAR-KDSS-PLE = SPACE                                          
266100        AND                                                               
266200        4006-ADLAGOMR NOT = SPAR-ADLAGOMR                                 
266300                                                                          
266400          MOVE PRT-NYSIDA-RAD1   TO PRT-RADSKIP                           
266500          MOVE +1                TO ANT-RAD-IX                            
266600          MOVE '0         '      TO LINE-IDAFPRCD                         
266700          MOVE '0         '      TO WS-IDAFPRCD                           
266800        END-IF                                                            
266900     END-IF                                                               
267000                                                                          
267100     MOVE NEJ           TO PRINTER-SW                                     
267200     MOVE 4006-KDSS-PLE TO SPAR-KDSS-PLE                                  
267300     MOVE 4006-ADLAGOMR TO SPAR-ADLAGOMR                                  
267400     .                                                                    
267500     EJECT                                                                
267600                                                                          
267700 CH-MOVE-MRP-LABEL SECTION.                                               
267800     MOVE 'CH-MOVE-MRP-LABEL '   TO WS-CURRENT-SECTION                    
267900*MRP INDIA LABEL                                                          
268000*MRP INDIA LABEL                                                          
268100*MRP INDIA LABEL                                                          
268200*MRP INDIA LABEL                                                          
268300                                                                          
268400     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
268500                                    DIST35-IDDISTR                        
268600                                                                          
268700     IF PAGE-MRP-IX =  0                                                  
268800       MOVE +1                   TO PAGE-MRP-IX                           
268900       MOVE '          '         TO MRP-IDAFPRCD                          
269000       MOVE '          '         TO WS-MRP-IDAFPRCD                       
269100     END-IF                                                               
269200     IF PAGE-MRP-IX = 25                                                  
269300       MOVE +1                   TO PAGE-MRP-IX                           
269400       MOVE '          '         TO MRP-IDAFPRCD                          
269500       MOVE '          '         TO WS-MRP-IDAFPRCD                       
269600*      MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                           
269700     END-IF                                                               
269800                                                                          
269900*W400ARTU                                                                 
270000     MOVE 4006-KDARTURS            TO ARTU-KDARTURS                       
270100     MOVE 4006-IDDISTR             TO ARTU-IDDISTR                        
270200     MOVE MID-IDDC                 TO ARTU-IDDC                           
270300     CALL W400ARTU USING ARTU-W400ARTU                                    
270400     MOVE ARTU-BEARTURS-ENG        TO MRP-BEARTURS                        
270500                                                                          
270600*GB IDSKYLT FOR INDIA.                                                    
270700     MOVE 'GB'         TO W-IDSKYLT-X                                     
270800     MOVE 4006-IDARTNR TO W-D3BSEQ-IDARTNR                                
270900                                                                          
271000     PERFORM IMS-GU-WDD311                                                
271100     IF SEGMENT-FINNS                                                     
271200        MOVE TEXT-BEART       TO MRP-BEART                                
271300        MOVE TEXT-BEART       TO MRP-KIT-BEART-HEAD                       
271400     ELSE                                                                 
271500        MOVE 'NAME MISSING'   TO MRP-BEART                                
271600        MOVE 'NAME MISSING'   TO MRP-KIT-BEART-HEAD                       
271700     END-IF                                                               
271800*                                                                         
271900     MOVE 4006-IDARTNR           TO MRP-IDARTNR                           
272000                                                                          
272100*SHIPPER-INFO FINNS I SHIPPER-TAB I PROGRAMMET.                           
272200     MOVE +10                         TO SHIP-INDX                        
272300                                                                          
272400     MOVE SHIPPER-COMPANY (SHIP-INDX) TO MRP-BEGMT-RAD1                   
272500     MOVE SHIPPER-NAME    (SHIP-INDX) TO MRP-BEGMT-RAD2                   
272600     MOVE SHIPPER-STREET  (SHIP-INDX) TO MRP-ADGMT-GATA                   
272700     MOVE SHIPPER-CITY    (SHIP-INDX) TO MRP-ADGMT-PADR                   
272800     MOVE SHIPPER-COUNTRY (SHIP-INDX) TO MRP-ADGMT-LAND                   
272900                                                                          
273000*TELEFAX CONTAINS CITY INFO IN WNDCADRE.                                  
273100**   MOVE SHIPPER-TELEFAX (SHIP-INDX) TO MRP-ADCITY                       
273200     MOVE SHIPPER-TEL     (SHIP-INDX) TO MRP-IDTFN                        
273300     MOVE SHIPPER-IDMAIL  (SHIP-INDX) TO MRP-IDMAIL                       
273400                                                                          
273500     MOVE  WS-DATUM(1:2)              TO WS-TIYY                          
273600                                         W-DATE-AAMM(1:2)                 
273700     MOVE  WS-DATUM(3:2)              TO WS-TIMM                          
273800                                         W-DATE-AAMM(3:2)                 
273900     MOVE  WS-TIPRTDAT                TO MRP-TIPRTDAT                     
274000                                                                          
274100     MOVE 4006-IDARTNR                TO WS-IDARTNR                       
274200     MOVE 4006-IDARTNR                TO W-IDARTNR                        
274300     MOVE 4006-IDARTNR                TO W-IDARTNR-WDC3                   
274400     MOVE 'IN'                        TO W-IDLANDX2                       
274500     PERFORM IMS-GU-WDK601                                                
274600     MOVE ART-KDPRODSL                TO TEST-KDPRODSL                    
274700     MOVE ART-IDLEVNR                 TO WS-ART-IDLEVNR                   
274800     MOVE ART-KDSORT                  TO WS-ART-KDSORT                    
274900                                                                          
275000*IF YOU CHANGE THESE RULES REGARDING                                      
275100*(ADLAGOMR OR KDPRODSL OR KVQPACK)                                        
275200*CHANGE ALSO TO SAME RULES/CONDITION ON TWO OTHER PLACES.                 
275300*                                                                         
275400     IF 4006-ADLAGOMR = 10                                                
275500     OR 4006-ADLAGOMR = 11                                                
275600       PERFORM IMS-GNP-WDK611                                             
275700                                                                          
275800       IF KDPRODSL-EMB                                                    
275900         MOVE CLAG-KVQPACK-0    TO WS-CLAG-KVQPACK-0                      
276000         MOVE CLAG-KVQPACK-1    TO WS-CLAG-KVQPACK-1                      
276100         MOVE CLAG-IDARTNR-EMBQ0 TO WS-CLAG-IDARTNR-EMBQ0                 
276200                                                                          
276300         IF CLAG-KVQPACK-0 > +00000                                       
276400         OR CLAG-KVQPACK-1 > +00000                                       
276500         OR CLAG-IDARTNR-EMBQ0 = +006769100                               
276600           MOVE 4006-KVAVBART    TO MRP-KVAVBART                          
276700         ELSE                                                             
276800           MOVE 1                TO MRP-KVAVBART                          
276900         END-IF                                                           
277000       ELSE                                                               
277100         MOVE 1                  TO MRP-KVAVBART                          
277200       END-IF                                                             
277300     ELSE                                                                 
277400       MOVE 1                    TO MRP-KVAVBART                          
277500     END-IF                                                               
277600                                                                          
277700     IF WS-ART-IDLEVNR = '1001' OR '1002'                                 
277800     OR WS-ART-KDSORT  = 'SA'                                             
277900       MOVE 'KIT PART'           TO MRP-ADCITY                            
278000     ELSE                                                                 
278100       MOVE SPACE                TO MRP-ADCITY                            
278200     END-IF                                                               
278300                                                                          
278400     PERFORM IMS-GU-WDC311                                                
278500     IF SEGMENT-SAKNAS                                                    
278600       PERFORM IMS-GNP-WDK611                                             
278700*                                                                         
278800*IF YOU CHANGE THESE RULES REGARDING                                      
278900*(ADLAGOMR OR KDPRODSL OR KVQPACK)                                        
279000*CHANGE ALSO TO SAME RULES/CONDITION ON TWO OTHER PLACES.                 
279100*                                                                         
279200       IF 4006-ADLAGOMR = 10                                              
279300       OR 4006-ADLAGOMR = 11                                              
279400                                                                          
279500         IF KDPRODSL-EMB                                                  
279600           IF CLAG-KVQPACK-0 > +00000                                     
279700           OR CLAG-KVQPACK-1 > +00000                                     
279800           OR CLAG-IDARTNR-EMBQ0 = +006769100                             
279900                                                                          
280000             MOVE CLAG-PRARTSJK       TO WS-CLAG-PRARTSJK                 
280100             COMPUTE WS-CLAG-PRARTSJK =                                   
280200                     WS-CLAG-PRARTSJK * 4006-KVAVBART                     
280300             END-COMPUTE                                                  
280400           ELSE                                                           
280500             MOVE CLAG-PRARTSJK       TO WS-CLAG-PRARTSJK                 
280600           END-IF                                                         
280700         ELSE                                                             
280800           MOVE CLAG-PRARTSJK         TO WS-CLAG-PRARTSJK                 
280900         END-IF                                                           
281000       ELSE                                                               
281100         MOVE CLAG-PRARTSJK           TO WS-CLAG-PRARTSJK                 
281200       END-IF                                                             
281300                                                                          
281400*      ARTIKELNS SJÄLVKOSTNAD (WDK611)                                    
281500       COMPUTE WS-CLAG-PRARTSJK = WS-CLAG-PRARTSJK * 4                    
281600       END-COMPUTE                                                        
281700                                                                          
281800*      Tax @ 18% ( On Suggested Retail Price)                             
281900       COMPUTE WS-CLAG-PRARTSJK ROUNDED = WS-CLAG-PRARTSJK * 1.18         
282000       END-COMPUTE                                                        
282100                                                                          
282200*      Mark-up @10%                                                       
282300       COMPUTE WS-CLAG-PRARTSJK ROUNDED = WS-CLAG-PRARTSJK * 1.10         
282400       END-COMPUTE                                                        
282500                                                                          
282600       MOVE WS-CLAG-PRARTSJK          TO WS-PRARTBTO-SC                   
282700                                                                          
282800       MOVE 'INR'                     TO CURR-KDVALISO-ROW                
282900       MOVE W-DATE-AAMM               TO CURR-TIAAMM                      
283000       MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV                
283100       MOVE 'M'                       TO CURR-KDVALTYP                    
283200                                                                          
283300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
283400       IF CURR-KDSVAR = ' '                                               
283500         COMPUTE WS-PRARTBTO-SC ROUNDED =                                 
283600                 WS-PRARTBTO-SC / CURR-PRKURS-NEW                         
283700         END-COMPUTE                                                      
283800*SUGGESTED   RETAIL PRICE IN INR (INDIAN RUPIES)                          
283900*        MOVE WS-PRARTBTO-SC          TO WS-PRARTBTO-INT                  
284000         MOVE WS-PRARTBTO-SC          TO MRP-PRARTBTO-SC                  
284100       ELSE                                                               
284200         MOVE 0                       TO MRP-PRARTBTO-SC                  
284300         MOVE 0                       TO WS-PRARTBTO-SC                   
284400       END-IF                                                             
284500       MOVE 00                        TO MRP-PRARTBTO-SC(11:2)            
284600     ELSE                                                                 
284700*fetch suggested retail on WDC301 for a fixed market (INR)                
284800*                                                                         
284900*IF YOU CHANGE THESE RULES REGARDING                                      
285000*(ADLAGOMR OR KDPRODSL OR KVQPACK)                                        
285100*CHANGE ALSO TO SAME RULES/CONDITION ON TWO OTHER PLACES.                 
285200*                                                                         
285300       IF 4006-ADLAGOMR = 10                                              
285400       OR 4006-ADLAGOMR = 11                                              
285500                                                                          
285600         IF KDPRODSL-EMB                                                  
285700           IF CLAG-KVQPACK-0 > +00000                                     
285800           OR CLAG-KVQPACK-1 > +00000                                     
285900           OR CLAG-IDARTNR-EMBQ0 = +006769100                             
286000                                                                          
286100             MOVE SRP-PRARTBTO-SC     TO WS-PRARTBTO-SC                   
286200             COMPUTE WS-PRARTBTO-SC =                                     
286300                     WS-PRARTBTO-SC * 4006-KVAVBART                       
286400             END-COMPUTE                                                  
286500           ELSE                                                           
286600             MOVE SRP-PRARTBTO-SC     TO WS-PRARTBTO-SC                   
286700           END-IF                                                         
286800         ELSE                                                             
286900           MOVE SRP-PRARTBTO-SC       TO WS-PRARTBTO-SC                   
287000         END-IF                                                           
287100       ELSE                                                               
287200         MOVE SRP-PRARTBTO-SC         TO WS-PRARTBTO-SC                   
287300       END-IF                                                             
287400                                                                          
287500*      Tax @ 18% ( On Suggested Retail Price)                             
287600       COMPUTE WS-PRARTBTO-SC ROUNDED = WS-PRARTBTO-SC * 1.18             
287700       END-COMPUTE                                                        
287800                                                                          
287900*      Mark-up @10%                                                       
288000       COMPUTE WS-PRARTBTO-SC ROUNDED = WS-PRARTBTO-SC * 1.10             
288100       END-COMPUTE                                                        
288200                                                                          
288300*      MOVE WS-PRARTBTO-SC            TO WS-PRARTBTO-INT                  
288400       MOVE WS-PRARTBTO-SC            TO MRP-PRARTBTO-SC                  
288500       MOVE 00                        TO MRP-PRARTBTO-SC(11:2)            
288600                                                                          
288700     END-IF                                                               
288800     .                                                                    
288900     EJECT                                                                
289000                                                                          
289100 CRA-MOVE-FROM-KIT-MRP-TABLE  SECTION.                                    
289200     MOVE 'CRA-MOVE-FROM-KIT-MRP-TABLE' TO WS-CURRENT-SECTION             
289300                                                                          
289400*REANTPSA                                                                 
289500     IF MRP-TAB-BEART(KIT-TAB-IX) > SPACE                                 
289600       IF MRP-TAB-REANTPSA(KIT-TAB-IX) > ZERO                             
289700         MOVE MRP-TAB-REANTPSA(KIT-TAB-IX)                                
289800           TO REANTPSA                                                    
289900         MOVE REANTPSA-OUT-LINE                                           
290000           TO MRP-KIT-REANTPSA(LABEL-IX)                                  
290100       END-IF                                                             
290200     END-IF                                                               
290300                                                                          
290400     INSPECT MRP-KIT-REANTPSA(LABEL-IX)                                   
290500     REPLACING LEADING ZERO BY SPACE                                      
290600                                                                          
290700*BEART                                                                    
290800     MOVE MRP-TAB-BEART(KIT-TAB-IX)                                       
290900       TO MRP-KIT-BEART(LABEL-IX)                                         
291000*KDSORT                                                                   
291100     MOVE MRP-TAB-KDSORT(KIT-TAB-IX)                                      
291200       TO MRP-KIT-KDSORT(LABEL-IX)                                        
291300     .                                                                    
291400     EJECT                                                                
291500                                                                          
291600 CL-MOVE-KIT-MRP-LABEL       SECTION.                                     
291700     MOVE 'CL-MOVE-KIT-MRP-LABEL'   TO WS-CURRENT-SECTION                 
291800                                                                          
291900*MRP INDIA KIT LABEL                                                      
292000*MRP INDIA KIT LABEL                                                      
292100*MRP INDIA KIT LABEL                                                      
292200*MRP INDIA KIT LABEL                                                      
292300                                                                          
292400     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
292500                                    DIST35-IDDISTR                        
292600                                                                          
292700     IF PAGE-MRP-KIT-IX = 0                                               
292800       MOVE +1                   TO PAGE-MRP-KIT-IX                       
292900       MOVE '          '         TO MRP-KIT-IDAFPRCD                      
293000       MOVE '          '         TO WS-MRP-KIT-IDAFPRCD                   
293100     END-IF                                                               
293200     IF PAGE-MRP-KIT-IX = 25                                              
293300       MOVE +1                   TO PAGE-MRP-KIT-IX                       
293400       MOVE '          '         TO MRP-KIT-IDAFPRCD                      
293500       MOVE '          '         TO WS-MRP-KIT-IDAFPRCD                   
293600     END-IF                                                               
293700     .                                                                    
293800     EJECT                                                                
293900 CM-READ-KIT-PARTS-TO-TABEL  SECTION.                                     
294000     MOVE 'CM-READ-KIT-PARTS-TO-TABEL'    TO  WS-CURRENT-SECTION          
294100                                                                          
294200     MOVE NEJ                    TO SW-WDJ4-DATA                          
294300     MOVE 4006-IDARTNR           TO W-IDARTNR                             
294400     MOVE 4006-IDARTNR           TO W-IDARTNR-SATS                        
294500     MOVE 4006-IDARTNR           TO MRP-KIT-IDARTNR                       
294600                                                                          
294700*NEW START MRP KIT KDP                                                    
294800     IF WS-ART-IDLEVNR = '1001' OR '1002'                                 
294900       PERFORM IMS-GU-WDJ101                                              
295000     ELSE                                                                 
295100       IF WS-ART-KDSORT          = 'SA'                                   
295200         PERFORM IMS-GU-WDJ101                                            
295300         IF SEGMENT-SAKNAS                                                
295400           MOVE 'IMS-GU-WDJ401' TO WS-CM-READ-SECTION                     
295500           PERFORM IMS-GU-WDJ401                                          
295600           IF SEGMENT-FINNS                                               
295700             MOVE JA             TO SW-WDJ4-DATA                          
295800           END-IF                                                         
295900         END-IF                                                           
296000       ELSE                                                               
296100         MOVE 'GE'               TO STATUS-WS                             
296200       END-IF                                                             
296300     END-IF                                                               
296400                                                                          
296500     IF SEGMENT-FINNS                                                     
296600       MOVE  0                   TO NUM-PARTS-IN-KIT-PART                 
296700                                                                          
296800       IF WS-ART-IDLEVNR = '1001' OR '1002'                               
296900         PERFORM IMS-GNP-WDJ111                                           
297000       ELSE                                                               
297100         IF WS-ART-KDSORT        = 'SA'                                   
297200           MOVE 'IMS-GNP-WDJ411' TO  WS-CM-READ-SECTION                   
297300           IF WDJ4-DATA                                                   
297400             MOVE 'GE'           TO STATUS-WS                             
297500           ELSE                                                           
297600             PERFORM IMS-GNP-WDJ111                                       
297700           END-IF                                                         
297800           IF SEGMENT-SAKNAS                                              
297900             PERFORM IMS-GU-WDJ401                                        
298000             IF SEGMENT-FINNS                                             
298100               PERFORM IMS-GNP-WDJ411                                     
298200               MOVE JA           TO SW-WDJ4-DATA                          
298300             ELSE                                                         
298400               MOVE NEJ          TO SW-WDJ4-DATA                          
298500             END-IF                                                       
298600           ELSE                                                           
298700             MOVE 'IMS-GNP-WDJ111' TO WS-CM-READ-SECTION                  
298800             MOVE NEJ            TO SW-WDJ4-DATA                          
298900           END-IF                                                         
299000         END-IF                                                           
299100       END-IF                                                             
299200                                                                          
299300       IF SEGMENT-FINNS                                                   
299400         MOVE +1                 TO KIT-TAB-IX                            
299500         PERFORM UNTIL SEGMENT-SAKNAS                                     
299600           IF WDJ4-DATA                                                   
299700             MOVE 'CM-GET-KDP-DATA' TO WS-CM-READ-SECTION                 
299800             PERFORM CME-GET-KDP-DATA                                     
299900             ADD +1          TO KIT-TAB-IX                                
300000           ELSE                                                           
300100             PERFORM CMA-CHECK-KIT-ROW-STATUS                             
300200                                                                          
300300             IF KIT-STATUS-OK                                             
300400                                                                          
300500*FROM    W1021300 IS THIS RULE NEEDED?                                    
300600               IF RAD-IDARTNR > ZERO                                      
300700*KIT   TABEL  MOVE DATA TO TABEL                                          
300800                 PERFORM CMB-GET-KIT-DESCRIPTION                          
300900                 ADD +1          TO KIT-TAB-IX                            
301000               END-IF                                                     
301100             END-IF                                                       
301200           END-IF                                                         
301300*READ   WDJ111 OR WDJ411 FOR PARTS WITHIN A KITPART                       
301400           IF WDJ4-DATA                                                   
301500             MOVE 'IMS-GNP-WDJ411B' TO  WS-CM-READ-SECTION                
301600             PERFORM IMS-GNP-WDJ411                                       
301700           ELSE                                                           
301800             MOVE 'IMS-GNP-WDJ111B' TO  WS-CM-READ-SECTION                
301900             PERFORM IMS-GNP-WDJ111                                       
302000           END-IF                                                         
302100                                                                          
302200         END-PERFORM                                                      
302300       ELSE                                                               
302400         PERFORM CRD-CLEAR-TABEL                                          
302500       END-IF                                                             
302600     END-IF                                                               
302700     MOVE NEJ                    TO SW-WDJ4-DATA                          
302800     .                                                                    
302900                                                                          
303000 CME-GET-KDP-DATA              SECTION.                                   
303100     MOVE 'CME-GET-KDP-DATA        '  TO  WS-CURRENT-SECTION              
303200*NEW                                                                      
303300     MOVE KSRAD-IDARTNR          TO W-IDARTNR                             
303400     MOVE KSRAD-IDARTNR          TO WS-IDARTNR                            
303500     MOVE KSRAD-KVANTAL          TO MRP-TAB-REANTPSA(KIT-TAB-IX)          
303600     MOVE WS-ART-KDSORT          TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
303700                                                                          
303800     EVALUATE WS-ART-KDSORT                                               
303900       WHEN '  '                                                          
304000         MOVE 'N    '            TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
304100       WHEN 'ST'                                                          
304200         MOVE 'N    '            TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
304300     END-EVALUATE                                                         
304400                                                                          
304500     ADD      1 TO NUM-PARTS-IN-KIT-PART                                  
304600     IF KSRAD-BEART           > SPACE                                     
304700       MOVE KSRAD-BEART          TO MRP-TAB-BEART(KIT-TAB-IX)             
304800     ELSE                                                                 
304900*NEW REMOVE BEFORE INSTALL                                                
305000       MOVE 'TEXT MISSIN2'       TO MRP-TAB-BEART(KIT-TAB-IX)             
305100     END-IF                                                               
305200     .                                                                    
305300                                                                          
305400 CMA-CHECK-KIT-ROW-STATUS          SECTION.                               
305500     MOVE 'CMA-CHECK-KIT-ROW-STATUS'  TO  WS-CURRENT-SECTION              
305600                                                                          
305700     IF RAD-KDISATS = 'N' OR 'T' OR 'E'                                   
305800       MOVE JA                   TO KIT-STATUS-SW                         
305900     ELSE                                                                 
306000       MOVE RAD-TISTODAT         TO TMP1-YYMMDD                           
306100       MOVE WS-DATUM             TO TMP2-YYMMDD                           
306200       PERFORM WY2000P1                                                   
306300                                                                          
306400       IF RAD-KDISATS = 'U'                                               
306500       AND TMP1-YYMMDD > TMP2-YYMMDD                                      
306600         MOVE JA                 TO KIT-STATUS-SW                         
306700       ELSE                                                               
306800         MOVE NEJ                TO KIT-STATUS-SW                         
306900                                                                          
307000         MOVE RAD-TISTODAT       TO TMP1-YYMMDD                           
307100         MOVE WS-DATUM           TO TMP2-YYMMDD                           
307200         PERFORM WY2000P1                                                 
307300                                                                          
307400         IF RAD-KDISATS = ' '                                             
307500         AND TMP1-YYMMDD > TMP2-YYMMDD                                    
307600            MOVE JA           TO KIT-STATUS-SW                            
307700             MOVE RAD-TISTADAT   TO TMP1-YYMMDD                           
307800             MOVE WS-DATUM       TO TMP2-YYMMDD                           
307900             PERFORM WY2000P1                                             
308000             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
308100               CONTINUE                                                   
308200             END-IF                                                       
308300         ELSE                                                             
308400           MOVE NEJ              TO KIT-STATUS-SW                         
308500         END-IF                                                           
308600       END-IF                                                             
308700     END-IF                                                               
308800     .                                                                    
308900     EJECT                                                                
309000                                                                          
309100 CMB-GET-KIT-DESCRIPTION            SECTION.                              
309200     MOVE 'CMB-GET-KIT-DESCRIPTION'  TO  WS-CURRENT-SECTION               
309300*                                                                         
309400*BEART FRÅN WDK6 ELLER WDD3                                               
309500*                                                                         
309600     IF RAD-IDARTNR > ZERO                                                
309700       MOVE RAD-IDARTNR          TO W-IDARTNR                             
309800       MOVE RAD-IDARTNR          TO WS-IDARTNR                            
309900       MOVE RAD-REANTPSA         TO MRP-TAB-REANTPSA(KIT-TAB-IX)          
310000       MOVE RAD-KDSORT           TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
310100                                                                          
310200       EVALUATE RAD-KDSORT                                                
310300         WHEN '  '                                                        
310400           MOVE 'N    '          TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
310500         WHEN 'ST'                                                        
310600           MOVE 'N    '          TO MRP-TAB-KDSORT  (KIT-TAB-IX)          
310700       END-EVALUATE                                                       
310800                                                                          
310900*GB IDSKYLT FOR INDIA.                                                    
311000       ADD    1 TO NUM-PARTS-IN-KIT-PART                                  
311100       MOVE 'GB'                   TO W-IDSKYLT-X                         
311200       MOVE RAD-IDARTNR            TO W-D3BSEQ-IDARTNR                    
311300       PERFORM IMS-GU-WDD311                                              
311400       IF SEGMENT-FINNS                                                   
311500         MOVE TEXT-BEART TO MRP-TAB-BEART(KIT-TAB-IX)                     
311600*          FLYTTA IN I TABELL                                             
311700       ELSE                                                               
311800*NEW REMOVE BEFORE INSTALL                                                
311900         MOVE 'TEXT MISSIN1' TO MRP-TAB-BEART(KIT-TAB-IX)                 
312000         PERFORM GMBA-GET-BEART                                           
312100       END-IF                                                             
312200     END-IF                                                               
312300     .                                                                    
312400     EJECT                                                                
312500                                                                          
312600 GMBA-GET-BEART            SECTION.                                       
312700     MOVE 'GMBA-GET-BEART '         TO WS-CURRENT-SECTION                 
312800                                                                          
312900     IF RAD-IDARTNR NOT = ZERO                                            
313000       MOVE RAD-IDARTNR          TO W-IDARTNR                             
313100                                                                          
313200       IF RAD-BEART-SVE = SPACE                                           
313300         PERFORM IMS-GU-WDK601                                            
313400         IF SEGMENT-FINNS                                                 
313500           MOVE 'GB'               TO W-IDSKYLT-X                         
313600           MOVE RAD-IDARTNR        TO W-D3BSEQ-IDARTNR                    
313700           PERFORM IMS-GU-WDD311                                          
313800           IF SEGMENT-FINNS                                               
313900             MOVE TEXT-BEART TO MRP-TAB-BEART(KIT-TAB-IX)                 
314000           END-IF                                                         
314100         END-IF                                                           
314200       ELSE                                                               
314300         IF W-IDSKYLT-X NOT = 'S  '                                       
314400           MOVE 'S'              TO W-IDSKYLT-X                           
314500           MOVE RAD-BEART-SVE TO W-BEART                                  
314600           MOVE RAD-KDBENHOM       TO SPAR-RAD-KDBENHOM                   
314700           PERFORM IMS-GU-WDD301-ASEQ                                     
314800           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
314900                      (BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)                 
315000             IF SEGMENT-FINNS                                             
315100               IF BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                       
315200                 CONTINUE                                                 
315300               ELSE                                                       
315400                 PERFORM IMS-GN-WDD301-ASEQ                               
315500               END-IF                                                     
315600             ELSE                                                         
315700               MOVE 'TEXT MISSING' TO MRP-TAB-BEART(KIT-TAB-IX)           
315800             END-IF                                                       
315900           END-PERFORM                                                    
316000                                                                          
316100           IF SEGMENT-FINNS                                               
316200             MOVE 'GB'           TO W-IDSKYLT-X                           
316300             PERFORM IMS-GNP-WDD311-ASEQ                                  
316400             IF SEGMENT-FINNS                                             
316500               MOVE TEXT-BEART   TO MRP-TAB-BEART(KIT-TAB-IX)             
316600             ELSE                                                         
316700               MOVE 'TEXT MISSING'   TO MRP-TAB-BEART(KIT-TAB-IX)         
316800             END-IF                                                       
316900           END-IF                                                         
317000         ELSE                                                             
317100           MOVE 'TEXT MISSING'       TO MRP-TAB-BEART(KIT-TAB-IX)         
317200         END-IF                                                           
317300       END-IF                                                             
317400     ELSE                                                                 
317500       IF W-IDSKYLT-X NOT = 'S  '                                         
317600         MOVE 'S'                TO W-IDSKYLT-X                           
317700         MOVE RAD-BEART-SVE TO W-BEART                                    
317800         MOVE RAD-KDBENHOM       TO SPAR-RAD-KDBENHOM                     
317900         PERFORM IMS-GU-WDD301-ASEQ                                       
318000         PERFORM UNTIL (SEGMENT-SAKNAS) OR                                
318100                       (BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)                
318200           IF SEGMENT-FINNS                                               
318300             IF BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                         
318400               CONTINUE                                                   
318500             ELSE                                                         
318600               PERFORM IMS-GN-WDD301-ASEQ                                 
318700             END-IF                                                       
318800           ELSE                                                           
318900             MOVE 'TEXT MISSING' TO MRP-TAB-BEART(KIT-TAB-IX)             
319000           END-IF                                                         
319100         END-PERFORM                                                      
319200                                                                          
319300         IF SEGMENT-FINNS                                                 
319400           MOVE 'GB'             TO W-IDSKYLT-X                           
319500           PERFORM IMS-GNP-WDD311-ASEQ                                    
319600           IF SEGMENT-FINNS                                               
319700             MOVE TEXT-BEART TO MRP-TAB-BEART(KIT-TAB-IX)                 
319800           ELSE                                                           
319900             MOVE 'TEXT MISSING' TO MRP-TAB-BEART(KIT-TAB-IX)             
320000           END-IF                                                         
320100         END-IF                                                           
320200       ELSE                                                               
320300*        MOVE RAD-BEART-SVE  TO MRP-TAB-BEART(KIT-TAB-IX)                 
320400         MOVE 'TEXT MISSING' TO MRP-TAB-BEART(KIT-TAB-IX)                 
320500       END-IF                                                             
320600     END-IF                                                               
320700     .                                                                    
320800     EJECT                                                                
320900                                                                          
321000 CP-CLEAR-TABEL               SECTION.                                    
321100     MOVE 'CP-CLEAR-TABEL '         TO WS-CURRENT-SECTION                 
321200                                                                          
321300     MOVE 1                 TO KIT-TAB-IX                                 
321400     PERFORM UNTIL KIT-TAB-IX > MAX-NO-OF-CONTAIN-PARTS                   
321500                                                                          
321600       MOVE ZERO      TO MRP-TAB-REANTPSA(KIT-TAB-IX)                     
321700       MOVE SPACE     TO MRP-TAB-BEART(KIT-TAB-IX)                        
321800       MOVE SPACE     TO MRP-TAB-KDSORT(KIT-TAB-IX)                       
321900       ADD  1         TO KIT-TAB-IX                                       
322000     END-PERFORM                                                          
322100     MOVE 1                 TO KIT-TAB-IX                                 
322200     .                                                                    
322300     EJECT                                                                
322400                                                                          
322500 CI-SET-MRP-RADSKIP  SECTION.                                             
322600     MOVE 'CI-SET-MRP-RADSKIP ' TO WS-CURRENT-SECTION                     
322700*MRP INDIA LABEL                                                          
322800*MRP INDIA LABEL                                                          
322900*MRP INDIA LABEL                                                          
323000                                                                          
323100     IF PAGE-MRP-IX < 25                                                  
323200     EVALUATE WS-MRP-IDAFPRCD                                             
323300       WHEN '          '                                                  
323400         MOVE '0         '       TO MRP-IDAFPRCD                          
323500         MOVE '0         '       TO WS-MRP-IDAFPRCD                       
323600       WHEN '0         '                                                  
323700         MOVE '2         '       TO MRP-IDAFPRCD                          
323800         MOVE '2         '       TO WS-MRP-IDAFPRCD                       
323900       WHEN '1         '                                                  
324000         MOVE '2         '       TO MRP-IDAFPRCD                          
324100         MOVE '2         '       TO WS-MRP-IDAFPRCD                       
324200       WHEN '2         '                                                  
324300         MOVE '3         '       TO MRP-IDAFPRCD                          
324400         MOVE '3         '       TO WS-MRP-IDAFPRCD                       
324500       WHEN '3         '                                                  
324600         MOVE '4         '       TO MRP-IDAFPRCD                          
324700         MOVE '4         '       TO WS-MRP-IDAFPRCD                       
324800       WHEN '4         '                                                  
324900         MOVE '1         '       TO MRP-IDAFPRCD                          
325000         MOVE '1         '       TO WS-MRP-IDAFPRCD                       
325100     END-EVALUATE                                                         
325200     END-IF                                                               
325300                                                                          
325400     IF (PAGE-MRP-IX = 25)                                                
325500                                                                          
325600       MOVE +1                   TO PAGE-MRP-IX                           
325700       MOVE '0         '         TO MRP-IDAFPRCD                          
325800       MOVE '0         '         TO WS-MRP-IDAFPRCD                       
325900       MOVE '0         '         TO MRP-KIT-IDAFPRCD                      
326000     END-IF                                                               
326100     .                                                                    
326200     EJECT                                                                
326300                                                                          
326400 CJ-PUT-LINE SECTION.                                                     
326500     MOVE 'CJ-PUT-LINE '         TO WS-CURRENT-SECTION                    
326600                                                                          
326700     MOVE 'PUT'                   TO MRP-SEND-KDFUNC                      
326800     MOVE LENGTH OF SEND-AREA-TO-MRP-LABEL TO MRP-SEND-KVDLEN             
326900     CALL WZ01SEND             USING MRP-SEND-CONTROL-AREA                
327000                                     MRP-SEND-KVDLEN                      
327100                                     SEND-AREA-TO-MRP-LABEL               
327200                                                                          
327300     IF MRP-SEND-KDRC > ZERO                                              
327400       MOVE MRP-SEND-KDRC        TO KDRC-DISPLAY                          
327500       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
327600       DELIMITED BY SIZE       INTO ERROR-TEXT                            
327700       DISPLAY ERROR-TEXT                                                 
327800       CALL FELLOG                                                        
327900     END-IF                                                               
328000     .                                                                    
328100     SKIP2                                                                
328200                                                                          
328300 CK-PUT-HEADER SECTION.                                                   
328400     MOVE 'CK-PUT-HEADER   '      TO WS-CURRENT-SECTION                   
328500                                                                          
328600     MOVE 1                       TO HDR-REQU-IDMSGVER                    
328700     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
328800     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
328900     MOVE 'MRP-LABEL'             TO HDR-IDOUTTYPE                        
329000     MOVE WS-HDR-IDOUTREC         TO HDR-IDOUTREC                         
329100                                                                          
329200     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
329300     MOVE 'PUT'                   TO MRP-SEND-KDFUNC                      
329400     MOVE LENGTH OF HDR-AREA      TO MRP-SEND-KVDLEN                      
329500     CALL WZ01SEND             USING MRP-SEND-CONTROL-AREA                
329600                                     MRP-SEND-KVDLEN                      
329700                                     HDR-AREA                             
329800                                                                          
329900     IF MRP-SEND-KDRC > ZERO                                              
330000       MOVE MRP-SEND-KDRC        TO KDRC-DISPLAY                          
330100       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
330200       DELIMITED BY SIZE       INTO ERROR-TEXT                            
330300       DISPLAY ERROR-TEXT                                                 
330400       CALL FELLOG                                                        
330500     END-IF                                                               
330600     .                                                                    
330700     EJECT                                                                
330800                                                                          
330900 CR-PRINT-KIT-PART  SECTION.                                              
331000     MOVE 'CR-PRINT-KIT-PART    '   TO WS-CURRENT-SECTION                 
331100*                                                                         
331200     MOVE 1                           TO KIT-TAB-IX                       
331300     MOVE 1                           TO LABEL-IX                         
331400                                                                          
331500     IF PAGE-MRP-KIT-IX = 0                                               
331600     OR PAGE-MRP-KIT-IX = 25                                              
331700       MOVE +1           TO PAGE-MRP-KIT-IX                               
331800       MOVE '          ' TO MRP-KIT-IDAFPRCD                              
331900       MOVE '          ' TO WS-MRP-KIT-IDAFPRCD                           
332000     END-IF                                                               
332100*                                                                         
332200     PERFORM UNTIL                                                        
332300       KIT-TAB-IX > NUM-PARTS-IN-KIT-PART                                 
332400                                                                          
332500*      FLYTTA FRÅN TAB TILL MRP KIT LABEL                                 
332600       PERFORM CRA-MOVE-FROM-KIT-MRP-TABLE                                
332700       IF LABEL-IX = MAX-NO-OF-PARTS-LABEL                                
332800         PERFORM CRB-SET-MRP-KIT-RADSKIP                                  
332900         PERFORM CRE-MOVE-COUNTER-LABELS                                  
333000         PERFORM CRC-PUT-MRP-KIT-LINE                                     
333100         PERFORM CRD-CLEAR-TABEL                                          
333200         MOVE ZERO        TO LABEL-IX                                     
333300         MOVE NEJ         TO LESS-THAN-TEN-SW                             
333400       ELSE                                                               
333500         MOVE JA          TO LESS-THAN-TEN-SW                             
333600       END-IF                                                             
333700                                                                          
333800*      PRINT KIT LABEL                                                    
333900       IF PAGE-MRP-KIT-IX = 25                                            
334000         PERFORM CRB-SET-MRP-KIT-RADSKIP                                  
334100       END-IF                                                             
334200                                                                          
334300       ADD 1                        TO LABEL-IX                           
334400       ADD 1                        TO KIT-TAB-IX                         
334500     END-PERFORM                                                          
334600     MOVE ZERO                      TO WS-LABEL-FROM                      
334700     MOVE ZERO                      TO WS-LABEL-TO                        
334800                                                                          
334900*END MRP KIT PART LINE                                                    
335000                                                                          
335100     IF LESS-THAN-TEN                                                     
335200       PERFORM CRB-SET-MRP-KIT-RADSKIP                                    
335300       PERFORM CRE-MOVE-COUNTER-LABELS                                    
335400       PERFORM CRC-PUT-MRP-KIT-LINE                                       
335500       PERFORM CRD-CLEAR-TABEL                                            
335600     END-IF                                                               
335700     MOVE ZERO                      TO FROM-KIT-LABEL-COUNTER             
335800     .                                                                    
335900     EJECT                                                                
336000                                                                          
336100 CA-PUT-MRP-KIT-HEADER     SECTION.                                       
336200     MOVE 'CA-PUT-MRP-KIT-HEADER'  TO WS-CURRENT-SECTION                  
336300*                                                                         
336400     MOVE 1                       TO KIT-REQU-IDMSGVER                    
336500     MOVE SPACE                   TO KIT-REQU-KDPGMACT                    
336600     MOVE IDPGM                   TO KIT-REQU-IDUSER                      
336700     MOVE 'MRP-KIT-LABEL'         TO KIT-HDR-IDOUTTYPE                    
336800     MOVE WS-HDR-IDOUTREC         TO KIT-HDR-IDOUTREC                     
336900                                                                          
337000     MOVE WS-TIYYMMDDHHMM         TO KIT-HDR-IDLIST                       
337100     MOVE 'PUT'                   TO MRP-KIT-SEND-KDFUNC                  
337200     MOVE LENGTH OF HDR-KIT-AREA  TO MRP-KIT-SEND-KVDLEN                  
337300     CALL WZ01SEND             USING MRP-KIT-SEND-CONTROL-AREA            
337400                                     MRP-KIT-SEND-KVDLEN                  
337500                                     HDR-KIT-AREA                         
337600                                                                          
337700     IF MRP-KIT-SEND-KDRC > ZERO                                          
337800       MOVE MRP-KIT-SEND-KDRC        TO KDRC-DISPLAY                      
337900       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
338000       DELIMITED BY SIZE       INTO ERROR-TEXT                            
338100       DISPLAY ERROR-TEXT                                                 
338200       CALL FELLOG                                                        
338300     END-IF                                                               
338400     .                                                                    
338500     EJECT                                                                
338600                                                                          
338700 CRB-SET-MRP-KIT-RADSKIP SECTION.                                         
338800     MOVE 'CRB-SET-MRP-RADSKIP ' TO WS-CURRENT-SECTION                    
338900*MRP KIT LABEL                                                            
339000*MRP KIT LABEL                                                            
339100*MRP KIT LABEL                                                            
339200                                                                          
339300     IF PAGE-MRP-KIT-IX < 25                                              
339400     EVALUATE WS-MRP-KIT-IDAFPRCD                                         
339500       WHEN '          '                                                  
339600         MOVE '0         '       TO MRP-KIT-IDAFPRCD                      
339700         MOVE '0         '       TO WS-MRP-KIT-IDAFPRCD                   
339800       WHEN '0         '                                                  
339900         MOVE '2         '       TO MRP-KIT-IDAFPRCD                      
340000         MOVE '2         '       TO WS-MRP-KIT-IDAFPRCD                   
340100       WHEN '1         '                                                  
340200         MOVE '2         '       TO MRP-KIT-IDAFPRCD                      
340300         MOVE '2         '       TO WS-MRP-KIT-IDAFPRCD                   
340400       WHEN '2         '                                                  
340500         MOVE '3         '       TO MRP-KIT-IDAFPRCD                      
340600         MOVE '3         '       TO WS-MRP-KIT-IDAFPRCD                   
340700       WHEN '3         '                                                  
340800         MOVE '4         '       TO MRP-KIT-IDAFPRCD                      
340900         MOVE '4         '       TO WS-MRP-KIT-IDAFPRCD                   
341000       WHEN '4         '                                                  
341100         MOVE '1         '       TO MRP-KIT-IDAFPRCD                      
341200         MOVE '1         '       TO WS-MRP-KIT-IDAFPRCD                   
341300     END-EVALUATE                                                         
341400     END-IF                                                               
341500                                                                          
341600     IF PAGE-MRP-KIT-IX = 25                                              
341700       MOVE '          '         TO WS-MRP-KIT-IDAFPRCD                   
341800       MOVE '          '         TO MRP-KIT-IDAFPRCD                      
341900       MOVE +1                   TO PAGE-MRP-KIT-IX                       
342000     END-IF                                                               
342100                                                                          
342200     .                                                                    
342300     EJECT                                                                
342400                                                                          
342500 CRC-PUT-MRP-KIT-LINE          SECTION.                                   
342600     MOVE 'CRC-PUT-MRP-KIT-LINE'  TO WS-CURRENT-SECTION                   
342700                                                                          
342800     ADD 1                        TO PAGE-MRP-KIT-IX                      
342900     MOVE 'PUT'                   TO MRP-KIT-SEND-KDFUNC                  
343000     MOVE LENGTH OF SEND-AREA-TO-MRP-KIT-LABEL                            
343100       TO MRP-KIT-SEND-KVDLEN                                             
343200       CALL WZ01SEND           USING MRP-KIT-SEND-CONTROL-AREA            
343300                                     MRP-KIT-SEND-KVDLEN                  
343400                                     SEND-AREA-TO-MRP-KIT-LABEL           
343500                                                                          
343600     IF MRP-KIT-SEND-KDRC > ZERO                                          
343700       MOVE MRP-KIT-SEND-KDRC        TO KDRC-DISPLAY                      
343800       STRING 'WZ01SEND MRP KIT GET ERROR RC= ' KDRC-DISPLAY              
343900       DELIMITED BY SIZE       INTO ERROR-TEXT                            
344000       DISPLAY ERROR-TEXT                                                 
344100       CALL FELLOG                                                        
344200     END-IF                                                               
344300     .                                                                    
344400     SKIP2                                                                
344500                                                                          
344600 CRD-CLEAR-TABEL              SECTION.                                    
344700     MOVE 'CRD-CLEAR-TABEL '        TO WS-CURRENT-SECTION                 
344800                                                                          
344900     MOVE 1                 TO LABEL-IX                                   
345000     PERFORM UNTIL LABEL-IX > MAX-NO-OF-PARTS-LABEL                       
345100                                                                          
345200       MOVE SPACE     TO MRP-KIT-REANTPSA(LABEL-IX)                       
345300       MOVE SPACE     TO MRP-KIT-BEART(LABEL-IX)                          
345400       MOVE SPACE     TO MRP-KIT-KDSORT(LABEL-IX)                         
345500       ADD  1         TO LABEL-IX                                         
345600     END-PERFORM                                                          
345700     MOVE ZERO              TO LABEL-IX                                   
345800     .                                                                    
345900     EJECT                                                                
346000                                                                          
346100 CRE-MOVE-COUNTER-LABELS    SECTION.                                      
346200     MOVE 'CRE-MOVE-COUNTER-LABELS'   TO WS-CURRENT-SECTION               
346300                                                                          
346400     ADD  1                        TO FROM-KIT-LABEL-COUNTER              
346500     MOVE FROM-KIT-LABEL-COUNTER   TO WS-LABEL-FROM                       
346600                                                                          
346700     COMPUTE NUMBER-TO-ROUNDUP =                                          
346800             NUM-PARTS-IN-KIT-PART / MAX-NO-OF-PARTS-LABEL                
346900     END-COMPUTE                                                          
347000                                                                          
347100     MOVE NUMBER-TO-ROUNDUP        TO INTEGER-N2                          
347200     IF INTEGER-N2 = NUMBER-TO-ROUNDUP                                    
347300       IF NUMBER-ROUNDEDUP = ZERO                                         
347400         MOVE NUMBER-TO-ROUNDUP    TO NUMBER-ROUNDEDUP                    
347500       END-IF                                                             
347600     ELSE                                                                 
347700       ADD 1                       TO INTEGER-N2                          
347800       MOVE INTEGER-N2             TO NUMBER-ROUNDEDUP                    
347900     END-IF                                                               
348000     MOVE NUMBER-ROUNDEDUP         TO WS-LABEL-TO                         
348100     MOVE WS-LABEL-COUNT-GROUP     TO MRP-KIT-LABEL-COUNT                 
348200                                                                          
348300     .                                                                    
348400     EJECT                                                                
348500 D-UPPDAT-PLOCKSATS SECTION.                                              
348600     MOVE 'D-UPPDAT-PLOCKSATS'    TO WS-CURRENT-SECTION                   
348700                                                                          
348800     IF PLOCKSATS-KLAR                                                    
348900        PERFORM IMS-GHU-4003-WL400301                                     
349000        PERFORM IMS-DLET-4003-WL400301                                    
349100     ELSE                                                                 
349200        PERFORM IMS-GHU-4003-WL400311                                     
349300        MOVE 4006-KDPRT     TO 4004-KDPRT                                 
349400        MOVE 4006-KDSS-PLE  TO 4004-KDSS                                  
349500        MOVE 4006-ADLAGOMR  TO 4004-ADLAGOMR                              
349600        MOVE 4006-ADGANG    TO 4004-ADGANG                                
349700        MOVE 4006-ADPLATS   TO 4004-ADPLATS                               
349800        MOVE 4006-IDARTNR   TO 4004-IDARTNR                               
349900        MOVE 4006-IDLOPNR   TO 4004-IDLOPNR                               
350000        PERFORM IMS-REPL-4003-WL400311                                    
350100     END-IF                                                               
350200     .                                                                    
350300     EJECT                                                                
350400 E-SKICKA-IMSTRANS SECTION.                                               
350500     MOVE 'E-SKICKA-IMSTRANS '    TO WS-CURRENT-SECTION                   
350600                                                                          
350700     IF PLOCKSATS-EJ-KLAR                                                 
350800        MOVE MID-W4I37601 TO PTOP1-MID-W4I37601                           
350900        PERFORM IMS-ISRT-MSG-ALT1                                         
351000     END-IF                                                               
351100     .                                                                    
351200     EJECT                                                                
351300 S01-OPEN-PLE SECTION.                                                    
351400     MOVE 'S01-OPEN-PLE      '    TO WS-CURRENT-SECTION                   
351500                                                                          
351600     PERFORM S09-KOLLA-OM-LASER                                           
351700                                                                          
351800     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
351900                         PRT-OPEN                                         
352000                         WS-IDPRTLST                                      
352100                         ALT2-PCB                                         
352200                         LISB-PCB                                         
352300                         WS-PLE-LISTID                                    
352400                         WS-DUMMY                                         
352500                         WS-DUMMY                                         
352600                                                                          
352700     MOVE JA TO PRINTER-SW                                                
352800     .                                                                    
352900     EJECT                                                                
353000 S02-CLOSE-PLE SECTION.                                                   
353100     MOVE 'S02-CLOSE-PLE     '    TO WS-CURRENT-SECTION                   
353200                                                                          
353300     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
353400                         PRT-CLOSE                                        
353500                         WS-IDPRTLST                                      
353600                         ALT2-PCB                                         
353700                         LISB-PCB                                         
353800                         WS-PLE-LISTID                                    
353900                         WS-DUMMY                                         
354000                         WS-DUMMY                                         
354100     .                                                                    
354200     EJECT                                                                
354300 S03-PURGE-PLE SECTION.                                                   
354400     MOVE 'S03-PURGE-PLE     '    TO WS-CURRENT-SECTION                   
354500                                                                          
354600     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
354700                         PRT-PURGE                                        
354800                         WS-IDPRTLST                                      
354900                         ALT2-PCB                                         
355000                         LISB-PCB                                         
355100                         WS-PLE-LISTID                                    
355200                         WS-DUMMY                                         
355300                         WS-DUMMY                                         
355400                                                                          
355500     MOVE JA TO PRINTER-SW                                                
355600                                                                          
355700     .                                                                    
355800     EJECT                                                                
355900 S04-SKRIV-RAD SECTION.                                                   
356000     MOVE 'S04-SKRIV-RAD     '    TO WS-CURRENT-SECTION                   
356100                                                                          
356200     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
356300                         PRT-WRITE                                        
356400                         WS-IDPRTLST                                      
356500                         ALT2-PCB                                         
356600                         LISB-PCB                                         
356700                         WS-PLE-LISTID                                    
356800                         PRT-RADSKIP                                      
356900                         WS-PLE-LISTRAD                                   
357000     .                                                                    
357100     EJECT                                                                
357200 S07-PRINT-LINE SECTION.                                                  
357300     MOVE 'S07-PRINT-LINE     '   TO WS-CURRENT-SECTION                   
357400                                                                          
357500     IF PRT-RADSKIP = PRT-NYSIDA-RAD1                                     
357600       CONTINUE                                                           
357700     ELSE                                                                 
357800       MOVE PRT-AFTER-1          TO PRT-RADSKIP                           
357900     END-IF                                                               
358000                                                                          
358100     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
358200                         PRT-WRITE                                        
358300                         WS-IDPRTLST                                      
358400                         ALT2-PCB                                         
358500                         LISB-PCB                                         
358600                         WS-PLE-LISTID                                    
358700                         PRT-RADSKIP                                      
358800                         LINE-W40376                                      
358900     .                                                                    
359000     EJECT                                                                
359100 S08-SKRIV-TOTALRAD SECTION.                                              
359200     MOVE 'S08-SKRIV-TOTALRAD'   TO WS-CURRENT-SECTION                    
359300                                                                          
359400     MOVE PRT-AFTER-1            TO PRT-RADSKIP                           
359500                                                                          
359600     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
359700                         PRT-WRITE                                        
359800                         WS-IDPRTLST                                      
359900                         ALT2-PCB                                         
360000                         LISB-PCB                                         
360100                         WS-PLE-LISTID                                    
360200                         PRT-RADSKIP                                      
360300                         TOTAL-W40376                                     
360400     .                                                                    
360500     EJECT                                                                
360600                                                                          
360700 S09-KOLLA-OM-LASER   SECTION.                                            
360800     MOVE 'S09-KOLLA-OM-LASER'    TO WS-CURRENT-SECTION                   
360900                                                                          
361000*    -- CHECK OF IDPRTLST                                                 
361100     MOVE SPACE                 TO PRT-IDPRTLST                           
361200     MOVE '4'                   TO WS-SYSTDEL                             
361300     MOVE 'PE'                  TO WS-LISTTYP                             
361400     MOVE 4006-KDPRT            TO WS-KDPRT                               
361500                                                                          
361600     MOVE WS-IDPRTLST           TO PRT-IDPRTLST                           
361700     MOVE 001                   TO PRT-KDCALL                             
361800                                                                          
361900     CALL W006PRT USING PRT-W006PRT                                       
362000     IF PRT-KDSVAR = 'R'                                                  
362100        IF PRT-BEPRTLST(1:5) = 'LASER'                                    
362200          MOVE 'W40378' TO PRT-PFDEF-OVR                                  
362300          MOVE JA       TO SKRIVARTYP-SW                                  
362400        ELSE                                                              
362500          MOVE '      ' TO PRT-PFDEF-OVR                                  
362600          MOVE NEJ      TO SKRIVARTYP-SW                                  
362700        END-IF                                                            
362800                                                                          
362900        MOVE PRT-IDLTERM  TO WS-HDR-IDOUTREC                              
363000     END-IF                                                               
363100     .                                                                    
363200     EJECT                                                                
363300 S05-SKRIV-TOTALRADER SECTION.                                            
363400     MOVE 'S05-SKRIV-TOTALRADER'  TO WS-CURRENT-SECTION                   
363500                                                                          
363600     IF ETIKETT-IX = MAX-ETIKETT                                          
363700        MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                               
363800        MOVE 0             TO ETIKETT-IX                                  
363900        ADD  1             TO SID-IX                                      
364000     ELSE                                                                 
364100        IF FORSTA-TOTETIK                                                 
364200           MOVE PRT-AFTER-2 TO PRT-RADSKIP                                
364300        ELSE                                                              
364400           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
364500        END-IF                                                            
364600     END-IF                                                               
364700                                                                          
364800     IF SKRIVKLAR                                                         
364900       MOVE PRT-AFTER-2               TO PRT-RADSKIP                      
365000                                                                          
365100       EVALUATE TRUE                                                      
365200         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
365300           IF ANT-SID-IX > 0                                              
365400              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO JAP-RAD-H (3)         
365500           ELSE                                                           
365600              MOVE SPACE              TO JAP-RAD-H (3)                    
365700           END-IF                                                         
365800         WHEN DCS-NDC-NA                                                  
365900           IF ANT-SID-IX > 0                                              
366000              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO NDC-RAD-H (3)         
366100           ELSE                                                           
366200              MOVE SPACE              TO NDC-RAD-H (3)                    
366300           END-IF                                                         
366400         WHEN OTHER                                                       
366500           IF ANT-SID-IX > 0                                              
366600              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO ARB-RAD-H (3)         
366700           ELSE                                                           
366800              MOVE SPACE              TO ARB-RAD-H (3)                    
366900           END-IF                                                         
367000       END-EVALUATE                                                       
367100                                                                          
367200       EVALUATE TRUE                                                      
367300         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
367400           MOVE JAP-RAD (3)           TO WS-PLE-RAD                       
367500         WHEN DCS-NDC-NA                                                  
367600           MOVE NDC-RAD (3)           TO WS-PLE-RAD                       
367700         WHEN OTHER                                                       
367800           MOVE ARB-RAD (3)           TO WS-PLE-RAD                       
367900       END-EVALUATE                                                       
368000       PERFORM S04-SKRIV-RAD                                              
368100       MOVE JA TO TOTAL-SW                                                
368200                                                                          
368300       IF FORSTA-TOTETIK                                                  
368400          MOVE NEJ                  TO TOT-FORSTA-SW                      
368500       ELSE                                                               
368600         EVALUATE TRUE                                                    
368700           WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                        
368800             MOVE SPAR-RADSKIP      TO PRT-RADSKIP                        
368900             MOVE WS-SPAR-TOTALRAD (2) TO JAP-RAD-H (2)                   
369000             MOVE JAP-RAD (2)       TO WS-PLE-RAD                         
369100                                                                          
369200             PERFORM S04-SKRIV-RAD                                        
369300             MOVE PRT-AFTER-2       TO PRT-RADSKIP                        
369400             MOVE WS-SPAR-TOTALRAD (3) TO JAP-RAD-H (3)                   
369500             MOVE JAP-RAD (3)       TO WS-PLE-RAD                         
369600           WHEN DCS-NDC-NA                                                
369700             MOVE SPAR-RADSKIP      TO PRT-RADSKIP                        
369800             MOVE WS-SPAR-TOTALRAD (2) TO NDC-RAD-H (2)                   
369900             MOVE NDC-RAD (2)       TO WS-PLE-RAD                         
370000                                                                          
370100             PERFORM S04-SKRIV-RAD                                        
370200             MOVE PRT-AFTER-2       TO PRT-RADSKIP                        
370300             MOVE WS-SPAR-TOTALRAD (3) TO NDC-RAD-H (3)                   
370400             MOVE NDC-RAD (3)       TO WS-PLE-RAD                         
370500           WHEN OTHER                                                     
370600             MOVE SPAR-RADSKIP      TO PRT-RADSKIP                        
370700             MOVE WS-SPAR-TOTALRAD (2) TO ARB-RAD-H (2)                   
370800             MOVE ARB-RAD (2)       TO WS-PLE-RAD                         
370900             IF DCS-CDC                                                   
371000               MOVE PRT-AFTER-3     TO PRT-RADSKIP                        
371100             END-IF                                                       
371200                                                                          
371300             PERFORM S04-SKRIV-RAD                                        
371400             MOVE PRT-AFTER-2       TO PRT-RADSKIP                        
371500             MOVE WS-SPAR-TOTALRAD (3) TO ARB-RAD-H (3)                   
371600             MOVE ARB-RAD (3)       TO WS-PLE-RAD                         
371700           END-EVALUATE                                                   
371800                                                                          
371900          PERFORM S04-SKRIV-RAD                                           
372000       END-IF                                                             
372100       MOVE NEJ TO SKRIVKLAR-SW                                           
372200     ELSE                                                                 
372300       EVALUATE TRUE                                                      
372400         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
372500           MOVE PRT-AFTER-3        TO PRT-RADSKIP                         
372600           MOVE SPACE              TO JAP-RAD-H (1)                       
372700                                        JAP-RAD-H (2)                     
372800                                        JAP-RAD-H (3)                     
372900           IF ANT-SID-IX > 0                                              
373000              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO JAP-RAD-V (3)         
373100           ELSE                                                           
373200              MOVE SPACE              TO JAP-RAD-V (3)                    
373300           END-IF                                                         
373400                                                                          
373500           MOVE JAP-RAD (1)           TO WS-PLE-RAD                       
373600           PERFORM S04-SKRIV-RAD                                          
373700                                                                          
373800           MOVE JAP-RAD (2)           TO WS-PLE-RAD                       
373900           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
374000           PERFORM S04-SKRIV-RAD                                          
374100                                                                          
374200           MOVE JAP-RAD (3)           TO WS-PLE-RAD                       
374300           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
374400         WHEN DCS-NDC-NA                                                  
374500           MOVE PRT-AFTER-3        TO PRT-RADSKIP                         
374600           MOVE SPACE              TO NDC-RAD-H (1)                       
374700                                        NDC-RAD-H (2)                     
374800                                        NDC-RAD-H (3)                     
374900           IF ANT-SID-IX > 0                                              
375000              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO NDC-RAD-V (3)         
375100           ELSE                                                           
375200              MOVE SPACE              TO NDC-RAD-V (3)                    
375300           END-IF                                                         
375400                                                                          
375500           MOVE NDC-RAD (1)           TO WS-PLE-RAD                       
375600           PERFORM S04-SKRIV-RAD                                          
375700                                                                          
375800           MOVE NDC-RAD (2)           TO WS-PLE-RAD                       
375900           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
376000           PERFORM S04-SKRIV-RAD                                          
376100                                                                          
376200           MOVE NDC-RAD (3)           TO WS-PLE-RAD                       
376300           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
376400         WHEN OTHER                                                       
376500           MOVE PRT-AFTER-3        TO PRT-RADSKIP                         
376600           MOVE SPACE              TO ARB-RAD-H (1)                       
376700                                        ARB-RAD-H (2)                     
376800                                        ARB-RAD-H (3)                     
376900           IF ANT-SID-IX > 0                                              
377000              MOVE WS-SPAR-TOTALRAD (ANT-SID-IX) TO ARB-RAD-V (3)         
377100           ELSE                                                           
377200              MOVE SPACE              TO ARB-RAD-V (3)                    
377300           END-IF                                                         
377400                                                                          
377500           MOVE ARB-RAD (1)           TO WS-PLE-RAD                       
377600           PERFORM S04-SKRIV-RAD                                          
377700                                                                          
377800           MOVE ARB-RAD (2)           TO WS-PLE-RAD                       
377900           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
378000           IF DCS-CDC                                                     
378100             MOVE PRT-AFTER-3         TO PRT-RADSKIP                      
378200           END-IF                                                         
378300           PERFORM S04-SKRIV-RAD                                          
378400                                                                          
378500           MOVE ARB-RAD (3)           TO WS-PLE-RAD                       
378600           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
378700         END-EVALUATE                                                     
378800       PERFORM S04-SKRIV-RAD                                              
378900       MOVE JA TO TOTAL-SW                                                
379000                                                                          
379100*      IF FORSTA-TOTETIK                                                  
379200*         MOVE NEJ                  TO TOT-FORSTA-SW                      
379300*      ELSE                                                               
379400*         MOVE PRT-AFTER-3          TO PRT-RADSKIP                        
379500*         MOVE WS-SPAR-TOTALRAD (2) TO WS-PLE-RAD                         
379600*         PERFORM S04-SKRIV-RAD                                           
379700*         MOVE SPACE                TO WS-SPAR-TOTALRAD (2)               
379800*         MOVE PRT-AFTER-2          TO PRT-RADSKIP                        
379900*         MOVE WS-SPAR-TOTALRAD (3) TO WS-PLE-RAD                         
380000*         PERFORM S04-SKRIV-RAD                                           
380100*         MOVE SPACE                TO WS-SPAR-TOTALRAD (2)               
380200*      END-IF                                                             
380300     END-IF                                                               
380400*PRC GÄLLER FÖR PRC 190X (RENAULT ARTIKLAR).                              
380500     IF DCS-CDC                                                           
380600       IF MID-IDPRCBAS     = '190'                                        
380700         CONTINUE                                                         
380800       ELSE                                                               
380900         MOVE PRT-NYSIDA-RAD1  TO PRT-RADSKIP                             
381000       END-IF                                                             
381100     ELSE                                                                 
381200       IF DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
381300         CONTINUE                                                         
381400       ELSE                                                               
381500         MOVE PRT-NYSIDA-RAD1  TO PRT-RADSKIP                             
381600       END-IF                                                             
381700     END-IF                                                               
381800     MOVE SPACE TO WS-PLE-RAD                                             
381900     PERFORM S04-SKRIV-RAD                                                
382000     ADD 1 TO ETIKETT-IX                                                  
382100                                                                          
382200     IF DCS-SDC AND DCS-IDLANDX2 = 'IT'                                   
382300*SDC25-ITALIEN VILL HA TVÅ TOM-SIDOR EFTER SISTA SIDAN. DE HAR            
382400*LISTA MED EN ETIKETT/SIDA. DVS STÄLLT IN SKRIVAREN MED                   
382500*8 RADER/SIDA.                                                            
382600       MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                             
382700       MOVE SPACE TO WS-PLE-RAD                                           
382800       PERFORM S04-SKRIV-RAD                                              
382900       ADD 1 TO ETIKETT-IX                                                
383000     END-IF                                                               
383100     MOVE NEJ TO TOTAL-SW                                                 
383200     .                                                                    
383300     EJECT                                                                
383400 S06-KOLLA-SKRIVKLAR SECTION.                                             
383500                                                                          
383600     IF SKRIVKLAR                                                         
383700       EVALUATE TRUE                                                      
383800         WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                          
383900           MOVE SPACE   TO JAP-RAD-H (1)                                  
384000                             JAP-RAD-H (2)                                
384100                             JAP-RAD-H (3)                                
384200           MOVE SPAR-RADSKIP TO PRT-RADSKIP                               
384300           MOVE JAP-RAD (1) TO WS-PLE-RAD                                 
384400           PERFORM S04-SKRIV-RAD                                          
384500                                                                          
384600           MOVE JAP-RAD (2) TO WS-PLE-RAD                                 
384700           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
384800           PERFORM S04-SKRIV-RAD                                          
384900                                                                          
385000           MOVE JAP-RAD (3) TO WS-PLE-RAD                                 
385100           MOVE PRT-AFTER-2 TO PRT-RADSKIP                                
385200           PERFORM S04-SKRIV-RAD                                          
385300         WHEN DCS-NDC-NA                                                  
385400           MOVE SPACE   TO NDC-RAD-H (1)                                  
385500                             NDC-RAD-H (2)                                
385600                             NDC-RAD-H (3)                                
385700           MOVE SPAR-RADSKIP TO PRT-RADSKIP                               
385800           MOVE NDC-RAD (1) TO WS-PLE-RAD                                 
385900           PERFORM S04-SKRIV-RAD                                          
386000                                                                          
386100           MOVE NDC-RAD (2) TO WS-PLE-RAD                                 
386200           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
386300           PERFORM S04-SKRIV-RAD                                          
386400                                                                          
386500           MOVE NDC-RAD (3) TO WS-PLE-RAD                                 
386600           MOVE PRT-AFTER-2 TO PRT-RADSKIP                                
386700           PERFORM S04-SKRIV-RAD                                          
386800         WHEN OTHER                                                       
386900           MOVE SPACE   TO ARB-RAD-H (1)                                  
387000                             ARB-RAD-H (2)                                
387100                             ARB-RAD-H (3)                                
387200           MOVE SPAR-RADSKIP TO PRT-RADSKIP                               
387300           MOVE ARB-RAD (1) TO WS-PLE-RAD                                 
387400           PERFORM S04-SKRIV-RAD                                          
387500                                                                          
387600           MOVE ARB-RAD (2) TO WS-PLE-RAD                                 
387700           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
387800           PERFORM S04-SKRIV-RAD                                          
387900                                                                          
388000           MOVE ARB-RAD (3) TO WS-PLE-RAD                                 
388100           MOVE PRT-AFTER-2 TO PRT-RADSKIP                                
388200           PERFORM S04-SKRIV-RAD                                          
388300       END-EVALUATE                                                       
388400                                                                          
388500       MOVE SPACE TO WS-PLE-RAD                                           
388600       MOVE NEJ   TO SKRIVKLAR-SW                                         
388700     END-IF                                                               
388800     .                                                                    
388900                                                                          
389000     EJECT                                                                
389100 S20-OPEN-MRP SECTION.                                                    
389200     MOVE 'S20-OPEN-MRP      '    TO WS-CURRENT-SECTION                   
389300                                                                          
389400     PERFORM S09-KOLLA-OM-LASER                                           
389500                                                                          
389600     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
389700                                    DIST35-IDDISTR                        
389800                                                                          
389900     IF DIST34-INDIA-NDC                                                  
390000*lk  OR DIST35-CDC-IN-REFILL                                              
390100                                                                          
390200*MRP INDIA LABEL                                                          
390300*                                                                         
390400       MOVE 'CARPARTS.DAP.DISTRDOC' TO MRP-SEND-ADDISPABS                 
390500       MOVE 'OPEN'                TO MRP-SEND-KDFUNC                      
390600       CALL WZ01SEND           USING MRP-SEND-CONTROL-AREA                
390700                                       MRP-SEND-OPEN-AREA                 
390800       IF MRP-SEND-KDRC > ZERO                                            
390900         MOVE MRP-SEND-KDRC      TO KDRC-DISPLAY                          
391000         STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                   
391100         DELIMITED BY SIZE INTO ERROR-TEXT                                
391200         DISPLAY ERROR-TEXT                                               
391300         CALL FELLOG                                                      
391400       END-IF                                                             
391500     END-IF                                                               
391600                                                                          
391700     .                                                                    
391800     EJECT                                                                
391900 S21-CLOSE-MRP SECTION.                                                   
392000     MOVE 'S21-CLOSE-MRP     '    TO WS-CURRENT-SECTION                   
392100                                                                          
392200     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
392300                                    DIST35-IDDISTR                        
392400                                                                          
392500     IF DIST34-INDIA-NDC                                                  
392600*lk  OR DIST35-CDC-IN-REFILL                                              
392700                                                                          
392800*MRP INDIA LABEL                                                          
392900                                                                          
393000       MOVE 'CLOSE'              TO MRP-SEND-KDFUNC                       
393100       CALL WZ01SEND          USING MRP-SEND-CONTROL-AREA                 
393200       IF SEND-KDRC > 0                                                   
393300         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
393400         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
393500         DELIMITED BY SIZE     INTO ERROR-TEXT                            
393600         DISPLAY ERROR-TEXT                                               
393700         CALL FELLOG                                                      
393800       END-IF                                                             
393900     END-IF                                                               
394000     .                                                                    
394100     EJECT                                                                
394200 S30-OPEN-MRP-KIT  SECTION.                                               
394300     MOVE 'S30-OPEN-MRP-KIT  '    TO WS-CURRENT-SECTION                   
394400                                                                          
394500     PERFORM S09-KOLLA-OM-LASER                                           
394600                                                                          
394700     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
394800                                    DIST35-IDDISTR                        
394900                                                                          
395000     IF DIST34-INDIA-NDC                                                  
395100*lk  OR DIST35-CDC-IN-REFILL                                              
395200                                                                          
395300*MRP INDIA LABEL                                                          
395400*                                                                         
395500       MOVE 'CARPARTS.DAP.DISTRDO2'  TO MRP-KIT-SEND-ADDISPABS            
395600       MOVE 'OPEN'                TO MRP-KIT-SEND-KDFUNC                  
395700       CALL WZ01SEND           USING MRP-KIT-SEND-CONTROL-AREA            
395800                                       MRP-KIT-SEND-OPEN-AREA             
395900       IF MRP-KIT-SEND-KDRC > ZERO                                        
396000         MOVE MRP-KIT-SEND-KDRC      TO KDRC-DISPLAY                      
396100         STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                   
396200         DELIMITED BY SIZE INTO ERROR-TEXT                                
396300         DISPLAY ERROR-TEXT                                               
396400         CALL FELLOG                                                      
396500       END-IF                                                             
396600     END-IF                                                               
396700                                                                          
396800     .                                                                    
396900     EJECT                                                                
397000                                                                          
397100 S31-CLOSE-MRP-KIT SECTION.                                               
397200     MOVE 'S31-CLOSE-MRP-KIT   '    TO WS-CURRENT-SECTION                 
397300                                                                          
397400     MOVE 4006-IDDISTR           TO DIST34-IDDISTR                        
397500                                    DIST35-IDDISTR                        
397600                                                                          
397700     IF DIST34-INDIA-NDC                                                  
397800*lk  OR DIST35-CDC-IN-REFILL                                              
397900                                                                          
398000*MRP INDIA LABEL                                                          
398100                                                                          
398200       MOVE 'CLOSE'              TO MRP-KIT-SEND-KDFUNC                   
398300       CALL WZ01SEND          USING MRP-KIT-SEND-CONTROL-AREA             
398400       IF MRP-KIT-SEND-KDRC > 0                                           
398500         MOVE MRP-KIT-SEND-KDRC          TO KDRC-DISPLAY                  
398600         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
398700         DELIMITED BY SIZE     INTO ERROR-TEXT                            
398800         DISPLAY ERROR-TEXT                                               
398900         CALL FELLOG                                                      
399000       END-IF                                                             
399100     END-IF                                                               
399200     .                                                                    
399300     EJECT                                                                
399400                                                                          
399500*                                                                         
399600******************************************************                    
399700*                  IMS SEKTIONER                     *                    
399800******************************************************                    
399900                                                                          
400000 IMS-GU-MSG SECTION.                                                      
400100                                                                          
400200     MOVE '  QC' TO GODK-STATUSKODER                                      
400300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
400400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
400500     PERFORM IMS-STATUSKONTROLL                                           
400600     .                                                                    
400700     SKIP3                                                                
400800                                                                          
400900 IMS-ISRT-MSG-ALT1 SECTION.                                               
401000     MOVE 'IMS-ISRT-MSG-ALT1 '          TO WS-CURRENT-IMS-SECTION         
401100                                                                          
401200     MOVE SPACE TO GODK-STATUSKODER                                       
401300     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
401400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
401500     PERFORM IMS-STATUSKONTROLL                                           
401600     .                                                                    
401700     SKIP3                                                                
401800 IMS-GHU-4003-WL400301 SECTION.                                           
401900     MOVE 'IMS-GHU-4003-WL400301 '      TO WS-CURRENT-IMS-SECTION         
402000                                                                          
402100     STRING 'WL400301(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
402200          DELIMITED BY SIZE INTO SSA1                                     
402300     MOVE '    ' TO GODK-STATUSKODER                                      
402400     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-AREA1 SSA1                    
402500     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
402600     PERFORM IMS-STATUSKONTROLL                                           
402700     .                                                                    
402800                                                                          
402900 IMS-GHU-4003-WL400311 SECTION.                                           
403000     MOVE 'IMS-GHU-4003-WL400311 '      TO WS-CURRENT-IMS-SECTION         
403100                                                                          
403200     STRING 'WL400301(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
403300          DELIMITED BY SIZE INTO SSA1                                     
403400     MOVE 'WL400311 ' TO SSA2                                             
403500     MOVE '    ' TO GODK-STATUSKODER                                      
403600     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-AREA1 SSA1 SSA2               
403700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
403800     PERFORM IMS-STATUSKONTROLL                                           
403900     .                                                                    
404000                                                                          
404100 IMS-REPL-4003-WL400311 SECTION.                                          
404200     MOVE 'IMS-REPL-4003-WL400311'      TO WS-CURRENT-IMS-SECTION         
404300                                                                          
404400     MOVE '    ' TO GODK-STATUSKODER                                      
404500     CALL CBLTDLI USING REPL 4003-PCB DLI-IO-AREA1                        
404600     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
404700     PERFORM IMS-STATUSKONTROLL                                           
404800     .                                                                    
404900                                                                          
405000 IMS-DLET-4003-WL400301 SECTION.                                          
405100     MOVE 'IMS-DLET-4003-WL400301'      TO WS-CURRENT-IMS-SECTION         
405200                                                                          
405300     MOVE '    ' TO GODK-STATUSKODER                                      
405400     CALL CBLTDLI USING DLET 4003-PCB DLI-IO-AREA1                        
405500     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
405600     PERFORM IMS-STATUSKONTROLL                                           
405700     .                                                                    
405800     EJECT                                                                
405900 IMS-GNP-4003-WL400321-KVAL SECTION.                                      
406000     MOVE 'IMS-GNP-4003-WL400321-KVAL' TO WS-CURRENT-IMS-SECTION          
406100                                                                          
406200     STRING 'WL400321(WDGXKEY  =' W-4006-IDHTYP-X ')'                     
406300          DELIMITED BY SIZE INTO SSA1                                     
406400     MOVE '    ' TO GODK-STATUSKODER                                      
406500     CALL CBLTDLI USING GNP 4003-PCB DLI-IO-AREA2 SSA1                    
406600     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
406700     PERFORM IMS-STATUSKONTROLL                                           
406800     .                                                                    
406900                                                                          
407000 IMS-GNP-4003-WL400321-OKVAL SECTION.                                     
407100     MOVE 'IMS-GNP-4003-WL400321-OKVAL' TO WS-CURRENT-IMS-SECTION         
407200                                                                          
407300     STRING 'WL400321(KDPRT    <' W-4006-MAX-KDPRT ')'                    
407400          DELIMITED BY SIZE INTO SSA1                                     
407500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
407600     CALL CBLTDLI USING GNP 4003-PCB DLI-IO-AREA2 SSA1                    
407700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
407800     PERFORM IMS-STATUSKONTROLL                                           
407900     .                                                                    
408000                                                                          
408100 IMS-GU-WDB601    SECTION.                                                
408200     MOVE 'IMS-GU-WDB601    '           TO WS-CURRENT-IMS-SECTION         
408300                                                                          
408400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
408500          DELIMITED BY SIZE INTO SSA1                                     
408600     MOVE '  GE' TO GODK-STATUSKODER                                      
408700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
408800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
408900     PERFORM IMS-STATUSKONTROLL                                           
409000     IF SEGMENT-SAKNAS                                                    
409100        MOVE SPACE TO DCS-KDDC                                            
409200     END-IF                                                               
409300     .                                                                    
409400     EJECT                                                                
409500*THIS IMS-SECTION TO GET DESCRIPTION                                      
409600 IMS-GU-WDD311 SECTION.                                                   
409700     MOVE 'IMS-GU-WDD311 '              TO WS-CURRENT-IMS-SECTION         
409800                                                                          
409900     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
410000          DELIMITED BY SIZE INTO SSA1                                     
410100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
410200          DELIMITED BY SIZE INTO SSA2                                     
410300     MOVE '  GE' TO GODK-STATUSKODER                                      
410400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
410500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
410600     PERFORM IMS-STATUSKONTROLL                                           
410700     .                                                                    
410800     SKIP2                                                                
410900                                                                          
411000*THIS IMS-SECTION TO GET PRICE                                            
411100 IMS-GU-WDC311 SECTION.                                                   
411200     STRING 'WDC301  (IDARTNR  =' W-WDC301KY-X ')'                        
411300          DELIMITED BY SIZE INTO SSA1                                     
411400     STRING 'WDC311  (IDLANDX2 =' W-WDC311KY-X ')'                        
411500          DELIMITED BY SIZE INTO SSA2                                     
411600     MOVE '  GE' TO GODK-STATUSKODER                                      
411700     CALL CBLTDLI USING GU WDC3-PCB DLI-IO-WDC311 SSA1 SSA2               
411800     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
411900     PERFORM IMS-STATUSKONTROLL                                           
412000     .                                                                    
412100     EJECT                                                                
412200                                                                          
412300 IMS-GU-WDK601 SECTION.                                                   
412400     MOVE 'IMS-GU-WDK601 '              TO WS-CURRENT-IMS-SECTION         
412500                                                                          
412600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
412700     DELIMITED BY SIZE INTO SSA1                                          
412800     MOVE '  GE' TO GODK-STATUSKODER                                      
412900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
413000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
413100     PERFORM IMS-STATUSKONTROLL                                           
413200     .                                                                    
413300     SKIP3                                                                
413400                                                                          
413500 IMS-GNP-WDK611 SECTION.                                                  
413600     MOVE 'IMS-GNP-WDK611'              TO WS-CURRENT-IMS-SECTION         
413700                                                                          
413800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
413900     DELIMITED BY SIZE INTO SSA1                                          
414000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
414100     DELIMITED BY SIZE INTO SSA2                                          
414200     MOVE '  GE' TO GODK-STATUSKODER                                      
414300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
414400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
414500     PERFORM IMS-STATUSKONTROLL                                           
414600     .                                                                    
414700     SKIP3                                                                
414800                                                                          
414900 IMS-GU-WDJ101         SECTION.                                           
415000     MOVE 'IMS-GU-WDJ101       ' TO WS-CURRENT-IMS-SECTION                
415100                                                                          
415200     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
415300            DELIMITED BY SIZE INTO SSA1                                   
415400     MOVE '  GE' TO GODK-STATUSKODER                                      
415500     CALL CBLTDLI USING GU   WDJ1-PCB DLI-IO-WDJ101 SSA1                  
415600     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
415700     PERFORM IMS-STATUSKONTROLL                                           
415800     .                                                                    
415900     SKIP3                                                                
416000                                                                          
416100 IMS-GNP-WDJ111       SECTION.                                            
416200     MOVE 'IMS-GNP-WDJ111      ' TO WS-CURRENT-IMS-SECTION                
416300                                                                          
416400     MOVE 'WDJ111  ' TO SSA1                                              
416500     MOVE '  GE' TO GODK-STATUSKODER                                      
416600     CALL CBLTDLI USING GNP  WDJ1-PCB DLI-IO-WDJ111  SSA1                 
416700     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
416800     PERFORM IMS-STATUSKONTROLL                                           
416900     .                                                                    
417000     EJECT                                                                
417100                                                                          
417200*NEW                                                                      
417300 IMS-GU-WDJ401         SECTION.                                           
417400     MOVE 'IMS-GU-WDJ401       ' TO WS-CURRENT-IMS-SECTION                
417500                                                                          
417600     STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
417700            DELIMITED BY SIZE INTO SSA1                                   
417800     MOVE '  GE' TO GODK-STATUSKODER                                      
417900     CALL CBLTDLI USING GU   WDJ4-PCB DLI-IO-WDJ401 SSA1                  
418000     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
418100     PERFORM IMS-STATUSKONTROLL                                           
418200     .                                                                    
418300     SKIP3                                                                
418400                                                                          
418500 IMS-GNP-WDJ411       SECTION.                                            
418600     MOVE 'IMS-GNP-WDJ411      ' TO WS-CURRENT-IMS-SECTION                
418700                                                                          
418800*    STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
418900*         DELIMITED BY SIZE INTO SSA1                                     
419000*    STRING 'WDJ411  (IDARTNR  =' W-IDARTNR-X ')'                         
419100*         DELIMITED BY SIZE INTO SSA2                                     
419200     MOVE 'WDJ411  ' TO SSA1                                              
419300     MOVE '  GE' TO GODK-STATUSKODER                                      
419400     CALL CBLTDLI USING GNP  WDJ4-PCB DLI-IO-WDJ411  SSA1                 
419500     MOVE WDJ4-STATUS-CODE TO STATUS-WS                                   
419600     PERFORM IMS-STATUSKONTROLL                                           
419700     .                                                                    
419800     EJECT                                                                
419900                                                                          
420000*WDD3-ASEQ IMS-SECTION TO GET DESCRIPTION                                 
420100 IMS-GU-WDD301-ASEQ SECTION.                                              
420200     SKIP2                                                                
420300     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
420400                                  W-BEART-X ')'                           
420500          DELIMITED BY SIZE INTO SSA1                                     
420600     MOVE '  GE' TO GODK-STATUSKODER                                      
420700     CALL CBLTDLI USING GU WDD3A-PCB DLI-IO-WDD301 SSA1                   
420800     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
420900     PERFORM IMS-STATUSKONTROLL                                           
421000     .                                                                    
421100     EJECT                                                                
421200                                                                          
421300 IMS-GN-WDD301-ASEQ      SECTION.                                         
421400     SKIP2                                                                
421500     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
421600                                  W-BEART-X ')'                           
421700          DELIMITED BY SIZE INTO SSA1                                     
421800     MOVE '  GE' TO GODK-STATUSKODER                                      
421900     CALL CBLTDLI USING GN WDD3A-PCB DLI-IO-WDD301 SSA1                   
422000     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
422100     PERFORM IMS-STATUSKONTROLL                                           
422200     .                                                                    
422300     EJECT                                                                
422400                                                                          
422500 IMS-GNP-WDD311-ASEQ SECTION.                                             
422600     SKIP2                                                                
422700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
422800          DELIMITED BY SIZE INTO SSA1                                     
422900     MOVE '  GE' TO GODK-STATUSKODER                                      
423000     CALL CBLTDLI USING GNP WDD3A-PCB DLI-IO-WDD311 SSA1                  
423100     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
423200     PERFORM IMS-STATUSKONTROLL                                           
423300     .                                                                    
423400                                                                          
423500 IMS-GU-WDF502 SECTION.                                                   
423600     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
423700            DELIMITED BY SIZE INTO SSA1                                   
423800     MOVE 'WDF502  '       TO SSA2                                        
423900     MOVE '  GE'           TO GODK-STATUSKODER                            
424000     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
424100     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
424200     PERFORM IMS-STATUSKONTROLL                                           
424300     .                                                                    
424400                                                                          
424500 IMS-STATUSKONTROLL SECTION.                                              
424600                                                                          
424700     SET STATUS-IX TO 1                                                   
424800     SEARCH GODK-STATUS                                                   
424900       AT END CALL FELLOG                                                 
425000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
425100     END-SEARCH                                                           
425200     .                                                                    
425300*    -COPY WY2000P1                                                       
