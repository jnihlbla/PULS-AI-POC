000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W476SASO.                                                
000400 AUTHOR.         STINA MOGREN.                                            
000500 DATE-WRITTEN.   06/05/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PGM:ET SKRIVER FöR SAUDI ARABIEN                                 
001100*        - SASO-BILAGA                                                    
001200*        -                                                                
001300*        CERTIFICATE OF CONFOMITY TILL SAUDIARABIEN                       
001400*        DISTRICT: 4848,  CUSTOMER 21, 36, 41                             
001500*                                                                         
001600*        PGM:ET ANVÄNDER:                                                 
001700*            ALT-PCB     ANVÄNDS AV W006PRS1 (PCB FÖR PRINTER)            
001800*            WDE1                                                         
001900*            WDB2                                                         
002000*            WDB1                                                         
002100*            WDG7                                                         
002200*                                                                         
002300*                                                                         
002400*    FRAKTKOD FLYG: 17, 18 OCH 19                                         
002500*    FRAKTKOD BÅT:  41, 42 OCH 43                                         
002600*    FRAKTKOD DHL:  50                                                    
002700*                                                                         
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W476SASO'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  TAB-IX                      PIC S9(5)   VALUE +0   COMP SYNC.        
005000 77  INDX                        PIC S9(5)   VALUE +0   COMP SYNC.        
005100 77  MAX-IX                      PIC S9(5)   VALUE +100 COMP SYNC.        
005200 77  WS-SIDNR                    PIC S9(3)   VALUE +0   COMP-3.           
005300 77  WS-RADNR                    PIC S9(3)   VALUE +0   COMP-3.           
005400 77  WS-POST-RAEKNARE            PIC S9(5)   VALUE +0   COMP-3.           
005500 77  WS-ANTAL-RADER              PIC S9(3)   VALUE +0   COMP-3.           
005600 77  MAX-RADER                   PIC S9(3)   VALUE +40  COMP-3.           
005700 01  W-GRP                       PIC S9(5)   VALUE ZERO COMP-3.           
005800 01  X-GRP                       PIC X(2)    VALUE SPACE.                 
005900 01  W-IND                       PIC S9(5)   VALUE ZERO COMP-3.           
006000 01  W-IDFAKT                    PIC S9(7)   VALUE ZERO COMP-3.           
006100 01  W-BEART                     PIC X(25)   VALUE SPACE.                 
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600 01  WS-SEKTION                  PIC X(50)   VALUE SPACE.                 
006700*                                                                         
006800*      --- VALID IDDC CODES                                               
006900       EJECT                                                              
007000*                                                                         
007100 77  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
007200                                                                          
007300 01  WS-DATUM-AAMMDD.                                                     
007400     03  WS-DATUM-AA             PIC 9(2)          VALUE ZERO.            
007500     03  WS-DATUM-MM             PIC 9(2)          VALUE ZERO.            
007600     03  WS-DATUM-DD             PIC 9(2)          VALUE ZERO.            
007700                                                                          
007800 01 WS-DATUM-DDMMAA.                                                      
007900     03  WS-DD                   PIC 9(2)          VALUE ZERO.            
008000     03  WS-MM                   PIC 9(2)          VALUE ZERO.            
008100     03  WS-AA                   PIC 9(2)          VALUE ZERO.            
008200     EJECT                                                                
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800                                                                          
008900 01  WS-CENTURY-DATUM.                                                    
009000     03 WS-CENTURY               PIC 9(2).                                
009100     03 WS-AAMMDD.                                                        
009200       05 WS-AA2                 PIC 9(2).                                
009300       05 WS-MM2                 PIC 9(2).                                
009400       05 WS-DD2                 PIC 9(2).                                
009500     EJECT                                                                
009600 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
009700     SKIP3                                                                
009800*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL COPYTEXT WWOMVAND                                
010100*01  -COPY WWOMVAND                                                       
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL COPYTEXT W400ARTU                                
010400*01  -COPY W400ARTU                                                       
010500     EJECT                                                                
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700*                                                                         
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011100     03  INTSOR                  PIC X(8)    VALUE 'INTSOR  '.            
011200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011300     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
011500     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
011600     EJECT                                                                
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012000     SKIP2                                                                
012100 77  KDRC-DISPLAY                PIC Z(5).                                
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL POSTSUM                                          
012400*                                                                         
012500*01  -COPY W0005   -PRE  POSTSUM-                                         
012600     EJECT                                                                
012700*- - - - - - - - - - - - - -  PARAMETRAR TILL  INTSOR                     
012800 01  INTSOR-HJAELP-AREA.                                                  
012900     03  INTSOR-POST-ANTAL         PIC S9(3)  VALUE ZERO COMP-3.          
013000     03  INTSOR-POST-LAENGD        PIC S9(3)  VALUE ZERO COMP-3.          
013100     03  INTSOR-SORT-FAELT-LAENGD  PIC S9(3)  VALUE ZERO COMP-3.          
013200     SKIP3                                                                
013300 01  FILLER                      PIC X(16)   VALUE  'SEND-AREA'.          
013400 01  SEND-AREA.                                                           
013500*    03  -COPY WZ01SEND                                                   
013600                                                                          
013700 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
013800 01  WS-SKIP1                      PIC X      VALUE ' '.                  
013900 01  WS-SKIP2                      PIC X      VALUE '0'.                  
014000 01  WS-SKIP3                      PIC X      VALUE '-'.                  
014100                                                                          
014200 01  SEND-RAD-STYRTECKEN.                                                 
014300     03  STYRTECKEN-RAD          PIC X.                                   
014400     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
014500*                                                                         
014600*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
014700*                                                                         
014800*01  FILLER   -COPY W006PRAR                                              
014900     EJECT                                                                
015000                                                                          
015100 01  W-IDPRTLST                  PIC X(8).                                
015200 01  WS-PRT.                                                              
015300     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
015400     03 WS-PRT-IDLIST.                                                    
015500        05 WS-IDLIST             PIC X(4)  VALUE 'SASO'.                  
015600        05 WS-PRT-IDDISTR        PIC 9(4).                                
015700        05 FILLER                PIC X(2).                                
015800     03 WS-PRT-LISTRAD.                                                   
015900        05 WS-RAD                PIC X(115).                              
016000     03 WS-PRT-DUMMY             PIC X(1).                                
016100                                                                          
016200 01  WS-KDFRAKT                  PIC X(3)    VALUE SPACE.                 
016300 01  WS-KDFRAKT-AIR              PIC X(3)    VALUE 'AIR'.                 
016400 01  WS-KDFRAKT-DHL              PIC X(3)    VALUE 'DHL'.                 
016500 01  WS-KDFRAKT-SEA              PIC X(3)    VALUE 'SEA'.                 
016600                                                                          
016700 01  SASO-RUBRIK                 PIC X(50)   VALUE                        
016800       'SUMMED UP SASO-REGULATED PARTS                    '.              
016900                                                                          
017000 01  W-PRODUCTGROUP              PIC X(70)   VALUE SPACE.                 
017100 01  W-PRODUCTGROUP1             PIC X(70)                                
017200           VALUE 'PRODUCT GROUP III-03 ICCP REF. NO: R-203040'.           
017300 01  W-PRODUCTGROUP2             PIC X(70)                                
017400           VALUE 'PRODUCT GROUP IV-01  ICCP REF. NO: R-201062'.           
017500 01  W-PRODUCTGROUP3             PIC X(70)                                
017600           VALUE 'PRODUCT GROUP III-02 ICCP REF. NO: R-201045'.           
017700 01  W-PRODUCTGROUP4             PIC X(70)                                
017800           VALUE 'PRODUCT GROUP II-37  ICCP REF. NO: R-203406'.           
017900 01  W-INDEX     PIC X(82)       VALUE SPACE.                             
018000 01  W-INDEX-TAB.                                                         
018100 03  W-INDEX-01  PIC X(82) VALUE 'RADIATORS AND HOSES                     
018200-    '                                                  '.                
018300 03  W-INDEX-02  PIC X(82) VALUE 'BRAKE AND PARTS                         
018400-    '                                                  '.                
018500 03  W-INDEX-03  PIC X(82) VALUE 'LIGHTS                                  
018600-    '                                                  '.                
018700 03  W-INDEX-04  PIC X(82) VALUE 'FILTERS                                 
018800-    '                                                  '.                
018900 03  W-INDEX-05  PIC X(82) VALUE 'SILENCERS AND EXHAUST PIPES             
019000-    '                                                  '.                
019100 03  W-INDEX-06  PIC X(82) VALUE 'CLUTCHES AND PARTS                      
019200-    '                                                  '.                
019300 03  W-INDEX-07  PIC X(82) VALUE 'CHILD RESTRAIN SYSTEM                   
019400-    '                                                  '.                
019500 03  W-INDEX-08  PIC X(82) VALUE 'SPARK PLUGS                             
019600-    '                                                  '.                
019700 03  W-INDEX-09  PIC X(82) VALUE 'WIPER BLADES, MOTOR AND ACCESO          
019800-    'RIES                                              '.                
019900 03  W-INDEX-10  PIC X(82) VALUE 'SAFETY BELTS                            
020000-    '                                                  '.                
020100 03  W-INDEX-11  PIC X(82) VALUE 'FUEL TANKS                              
020200-    '                                                  '.                
020300 03  W-INDEX-12  PIC X(82) VALUE 'MIRRORS                                 
020400-    '                                                  '.                
020500 03  W-INDEX-13  PIC X(82) VALUE 'BUMPERS                                 
020600-    '                                                  '.                
020700 03  W-INDEX-14  PIC X(82) VALUE 'DOOR LOCKS, HINGES AND ACCESOR          
020800-    'IES                                               '.                
020900 03  W-INDEX-15  PIC X(82) VALUE 'TYRE TUBES                              
021000-    '                                                  '.                
021100 03  W-INDEX-16  PIC X(82) VALUE 'V-BELTS AND FLAT BELTS                  
021200-    '                                                  '.                
021300 03  W-INDEX-17  PIC X(82) VALUE 'GAUGES AND INDICATORS                   
021400-    '                                                  '.                
021500 03  W-INDEX-18  PIC X(82) VALUE 'ROAD WHEELS AND RIMS                    
021600-    '                                                  '.                
021700 03  W-INDEX-19  PIC X(82) VALUE 'STEERING SYSTEM                         
021800-    '                                                  '.                
021900 03  W-INDEX-20  PIC X(82) VALUE 'ENGINE, TRANSMISSION, HYDRAULIC,        
022000-    ' TURBINE OILS, BRAKE FLUID AND ANTI-FREEZE/COOLANT'.                
022100 03  W-INDEX-21  PIC X(82) VALUE 'WINDSCREEN AND GLASS                    
022200-    '                                                  '.                
022300 03  W-INDEX-22  PIC X(82) VALUE 'BATTERIES                               
022400-    '                                                  '.                
022500 03  W-INDEX-23  PIC X(82) VALUE 'OTHER DIVERSE PRODUCTS                  
022600-    '                                                  '.                
022700 03  W-INDEX-24  PIC X(82) VALUE '                                        
022800-    '                                                  '.                
022900 01  W-INDEX-TAB2    REDEFINES W-INDEX-TAB.                               
023000 03  W-IND-TEXT  PIC X(82)   OCCURS 24.                                   
023100 01  FILLER                      PIC X(20)   VALUE SPACE.                 
023200 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
023300*    --- SASO-TABELL                                                      
023400 01  W476SASOTAB.                                                         
023500*                                                                         
023600*           SASO     BILAGA  TABELL                                       
023700*                            TABELL GRÄNSER                               
023800*                                                                         
023900*    *** FÖR TOTAL AV TABELLEN                                            
024000*                                                                         
024100   03  SASOTOT.                                                           
024200       07  SASOTOT-KVLEVART         PIC S9(7)        COMP-3.              
024300       07  SASOTOT-PRARTNTO         PIC S9(9)V9(2)   COMP-3.              
024400       07  GRPTOT-KVLEVART          PIC S9(7)        COMP-3.              
024500       07  GRPTOT-PRARTNTO          PIC S9(9)V9(2)   COMP-3.              
024600*                                                                         
024700   03  FILLER.                                                            
024800*                                                                         
024900*    *** FÖR BEHANDLING AV TABELLEN                                       
025000*                                                                         
025100     05  SASOTAB-MAX-ANTAL-POST  PIC S9(4)  COMP VALUE +1500.             
025200     05  SASOTAB-ANTAL-POST      PIC S9(4)  COMP VALUE ZERO.              
025300*                                                                         
025400*    *** FÖR SORTERING  AV TABELLEN                                       
025500*                                                                         
025600     05  SASOTAB-POST-LAENGD     PIC S9(3) COMP-3 VALUE +60.              
025700     05  SASOTAB-IDARTNR-LAENGD  PIC S9(3) COMP-3 VALUE +9.               
025800*                                                                         
025900*                                                                         
026000     05  SASO-TABELL  OCCURS  1500 TIMES.                                 
026100*                                                                         
026200         07 SASO-GRUPP         PIC S9(3)       COMP-3.                    
026300*                      *** HUVUDGRUPP ***                                 
026400         07 SASO-GRP           PIC X(2).                                  
026500*                      *** FUNKTIONSGRUPP                                 
026600         07 SASO-IDARTNR       PIC S9(9)       COMP-3.                    
026700*                      *** ARTIKELNR                                      
026800         07 SASO-PRARTNTO      PIC S9(07)V9(2) COMP-3.                    
026900*                      *** NETTOPRIS                                      
027000         07 SASO-KVLEVART      PIC S9(09)      COMP-3.                    
027100*                      *** LEVERERAT ANTAL                                
027200         07 SASO-KDARTURS      PIC X(2).                                  
027300*                      *** URSPRUNG                                       
027400         07 SASO-IDSTAT        PIC S9(09)      COMP-3.                    
027500*                      *** STATISTIK NR                                   
027600         07 SASO-BEART         PIC X(25).                                 
027700*                      *** ARTIKEL BENÄMNING                              
027800         07 SASO-SHIP          PIC X(3).                                  
027900*                      *** SKEPPNINGSSÄTT                                 
028000         07 SASO-KDVALISO      PIC X(3).                                  
028100         07 SASO-FILL          PIC X(1).                                  
028200         07 SASO-IND           PIC S9(3)       COMP-3.                    
028300*                      *** VALUTAKOD                                      
028400     EJECT                                                                
028500 01  FILLER                    PIC X(10)  VALUE 'SPAR-AREA '.             
028600 01  SPAR-AREA.                                                           
028700     03 SPAR-IDKUNDRF          PIC X(10)  VALUE SPACE.                    
028800     03 SPAR-IDSHIPM           PIC 9(7)   VALUE ZERO.                     
028900     03 SPAR-IDFAKT            PIC S9(7)  VALUE ZERO.                     
029000                                                                          
029100 01  FILLER                    PIC X(16)  VALUE 'LIST-RADER'.             
029200 01  LIST-RADER.                                                          
029300                                                                          
029400     03 RUB-DATUM.                                                        
029500       05 FILLER               PIC X(90)  VALUE SPACE.                    
029600       05 DATUM.                                                          
029700         07 RUB-MM             PIC 99.                                    
029800         07 FILLER             PIC X     VALUE '/'.                       
029900         07 RUB-DD             PIC 99.                                    
030000         07 FILLER             PIC X     VALUE '/'.                       
030100         07 RUB-CC             PIC 99.                                    
030200         07 RUB-AA             PIC 99.                                    
030300                                                                          
030400     03 RAD-SIDNR.                                                        
030500       05 FILLER               PIC X(97)  VALUE SPACE.                    
030600       05 SIDNR                PIC Z(2)9.                                 
030700                                                                          
030800                                                                          
030900 01  RAD1.                                                                
031000     03  FILLER                    PIC X(30).                             
031100     03  RAD4H-IMPORTER            PIC X(35).                             
031200     03  FILLER                    PIC X(02).                             
031300     03  RAD1-TIAAMMDD             PIC 9(06).                             
031400     03  RAD1-IDDISTR              PIC Z(4)9.                             
031500     03  FILLER                    PIC X(01).                             
031600     03  RAD1-IDSHIPM              PIC Z(06)9.                            
031700     03  FILLER                    PIC X(04).                             
031800     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
031900     03  FILLER                    PIC X(01).                             
032000     03  RAD1-IDLBBET              PIC X(12).                             
032100     03  FILLER                    PIC X(02).                             
032200     03  RAD1-PAGE-NO              PIC Z(03).                             
032300                                                                          
032400 01  RAD-HEAD.                                                            
032500     03  FILLER                    PIC X(15).                             
032600     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
032700     03  FILLER                    PIC X(2).                              
032800     03  RAD1H-IMPORTER            PIC X(35).                             
032900     03  FILLER                    PIC X(2).                              
033000     03  RAD-TYP-IDSHIP            PIC X(50).                             
033100                                                                          
033200 01  RAD2-HEAD.                                                           
033300     03  FILLER                    PIC X(30).                             
033400     03  RAD2H-IMPORTER            PIC X(35).                             
033500                                                                          
033600 01  RAD3-HEAD.                                                           
033700     03  FILLER                    PIC X(30).                             
033800     03  RAD3H-IMPORTER            PIC X(35).                             
033900                                                                          
034000 01  RAD5-HEAD.                                                           
034100     03  FILLER                    PIC X(30).                             
034200     03  RAD5H-IMPORTER            PIC X(35).                             
034300     03  FILLER                    PIC X(02).                             
034400     03  RAD5-FILL                 PIC X(09).                             
034500     03  RAD5-IDFAKT               PIC 9(07).                             
034600                                                                          
034700 01  SASO-RUB1.                                                           
034800     03  FILLER                    PIC X(24)    VALUE SPACE.              
034900     03  SASO-RUB1-TEXT            PIC X(70)    VALUE SPACE.              
035000                                                                          
035100 01  SASO-RUB2.                                                           
035200     03  FILLER                    PIC X(25)    VALUE SPACE.              
035300     03  SASO-RUB2-TEXT            PIC X(82).                             
035400                                                                          
035500 01  SASO-RUB3.                                                           
035600     03  FILLER                  PIC X(2)    VALUE SPACE.                 
035700     03  RADS1                   PIC X(8)    VALUE 'PART NO.'.            
035800     03  FILLER                  PIC X(2)    VALUE SPACE.                 
035900     03  RADS2                   PIC X(25)   VALUE 'PART DESCRIPTI        
036000-                                            'ON        '.                
036100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
036200     03  RADS3                   PIC X(8)    VALUE 'STAT.NO.'.            
036300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
036400     03  RADS4                   PIC X(7)    VALUE 'Q. DEL.'.             
036500     03  FILLER                  PIC X(4)    VALUE SPACE.                 
036600     03  RADS5                   PIC X(10)   VALUE 'TOT. PRICE'.          
036700     03  FILLER                  PIC X(10)   VALUE '  CURRENCY'.          
036800     03  FILLER                  PIC X(08)   VALUE '  ORIGIN'.            
036900     03  FILLER                  PIC X(10)   VALUE '  SHIPMENT'.          
037000*                                                                         
037100*    FÖR UTSKRIFT AV SASO BILAGA                                          
037200*                                                                         
037300 01  ARBETS-RAD.                                                          
037400     03  FILLER                    PIC X(02).                             
037500     03  RAD-IDARTNR-X.                                                   
037600       05  RAD-IDARTNR             PIC Z(07)9.                            
037700     03  FILLER                    PIC X(02).                             
037800     03  RAD-BEART                 PIC X(25).                             
037900     03  FILLER                    PIC X(01).                             
038000     03  RAD-IDSTAT                PIC 9(8)    BLANK WHEN ZERO.           
038100     03  FILLER                    PIC X(02).                             
038200     03  RAD-KVLEVART              PIC Z(08).                             
038300     03  FILLER                    PIC X(06).                             
038400     03  RAD-PRARTNTO              PIC Z(04)9.99.                         
038500     03  FILLER                    PIC X(05).                             
038600     03  RAD-KDVALISO              PIC X(03).                             
038700     03  FILLER                    PIC X(08).                             
038800     03  RAD-KDARTURS              PIC X(02).                             
038900     03  FILLER                    PIC X(03).                             
039000     03  RAD-SHIP                  PIC X(03).                             
039100     03  FILLER                    PIC X(04).                             
039200                                                                          
039300*                                                                         
039400 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
039500     SKIP2                                                                
039600 01  KEYS-TO-DLI.                                                         
039700     03  W-WDB101KY-X.                                                    
039800        05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                  
039900        05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                   
040000                                                                          
040100     03  W-WDB201KY-X.                                                    
040200        05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
040300        05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
040400                                                                          
040500     03  W-IDSHIPM-X.                                                     
040600        05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                   
040700                                                                          
040800     03  W-WDE111KY-X.                                                    
040900        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
041000        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
041100                                                                          
041200     03  W-WDE111KY-MIN.                                                  
041300        05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.           
041400        05  FILLER               PIC X(04)   VALUE LOW-VALUES.            
041500                                                                          
041600     03  W-WDE111KY-MAX.                                                  
041700        05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.           
041800        05  FILLER               PIC X(04)   VALUE HIGH-VALUES.           
041900                                                                          
042000     03  W-WDE121KY-X.                                                    
042100        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
042200        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
042300                                                                          
042400     03  W-IDARTNR-X.                                                     
042500        05  W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
042600                                                                          
042700     03  W-IDDC-X.                                                        
042800        05  W-IDDC               PIC X(2)    VALUE SPACE.                 
042900                                                                          
043000     03  W-IDSKYLT-X.                                                     
043100        05  W-IDSKYLT            PIC X(3)    VALUE 'GB '.                 
043200                                                                          
043300*    --- STATUS-KOD FRÅN IMS                                              
043400 01  STATUS-WS                   PIC XX.                                  
043500     88  SEGMENT-FINNS                       VALUE '  '.                  
043600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
043700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
043800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
043900     88  IMS-EJ-OK                           VALUE 'XD'.                  
044000     SKIP2                                                                
044100 01  GOOD-STATUSCODES.                                                    
044200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044300     SKIP3                                                                
044400 01  SSA1                        PIC X(64).                               
044500 01  SSA2                        PIC X(64).                               
044600 01  SSA3                        PIC X(64).                               
044700 01  SSA4                        PIC X(64).                               
044800     EJECT                                                                
044900*    --- IMS FUNKTIONSKODER                                               
045000*01  -COPY W0003                                                          
045100     EJECT                                                                
045200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
045300 01  DLI-IO-WDE101.                                                       
045400*    03  -COPY WDE101                                                     
045500                                                                          
045600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
045700 01  DLI-IO-WDE111.                                                       
045800*    03  -COPY WDE111                                                     
045900                                                                          
046000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
046100 01  DLI-IO-WDE121.                                                       
046200*    03  -COPY WDE121                                                     
046300                                                                          
046400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
046500 01  DLI-IO-WDE131.                                                       
046600*    03  -COPY WDE131                                                     
046700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
046800 01  DLI-IO-WDB201.                                                       
046900*    03  -COPY WDB201                                                     
047000                                                                          
047100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
047200 01  DLI-IO-WDB101.                                                       
047300*    03  -COPY WDB101                                                     
047400                                                                          
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
047600 01  DLI-IO-WDD311.                                                       
047700*    03  -COPY WDD311                                                     
047800     EJECT                                                                
047900                                                                          
048000 LINKAGE SECTION.                                                         
048100*01  -COPY W476TRPD                                                       
048200                                                                          
048300*01  -COPY W0009   -PRE ALT-                                              
048400     EJECT                                                                
048500*01  -COPY W0008  -PRE WDE1-                                              
048600     05  FILLER                  PIC X.                                   
048700*01  -COPY W0008  -PRE WDB2-                                              
048800     05  FILLER                  PIC X.                                   
048900*01  -COPY W0008  -PRE WDB1-                                              
049000     05  FILLER                  PIC X.                                   
049100*01  -COPY W0008  -PRE WDD3-                                              
049200     05  FILLER                  PIC X.                                   
049300                                                                          
049400     EJECT                                                                
049500 PROCEDURE DIVISION  USING TRPD-W476TRPD                                  
049600                           ALT-PCB                                        
049700                           WDE1-PCB                                       
049800                           WDB2-PCB                                       
049900                           WDB1-PCB                                       
050000                           WDD3-PCB.                                      
050100 MAIN SECTION.                                                            
050200     ENTRY 'DLITCBL' USING TRPD-W476TRPD                                  
050300                           ALT-PCB                                        
050400                           WDE1-PCB                                       
050500                           WDB2-PCB                                       
050600                           WDB1-PCB                                       
050700                           WDD3-PCB.                                      
050800                                                                          
050900     PERFORM A-INIT                                                       
051000     PERFORM IMS-GU-WDE101                                                
051100                                                                          
051200     PERFORM B-SPARA-DATA                                                 
051300                                                                          
051400     PERFORM C-INIT-ALLM                                                  
051500                                                                          
051600     PERFORM IMS-GU-WDE101                                                
051700                                                                          
051800     IF SEGMENT-FINNS                                                     
051900       PERFORM IMS-GNP-WDE111                                             
052000                                                                          
052100       MOVE SGMT-IDDISTR                TO W-WDB201-IDDISTR               
052200       MOVE SGMT-IDKUNDNR               TO W-WDB201-IDKUNDNR              
052300       PERFORM IMS-GU-WDB201                                              
052400       MOVE GMT-IDPARTNR                TO W-WDB101-IDPARTNR              
052500       MOVE GMT-IDFTG                   TO W-WDB101-IDFTG                 
052600       PERFORM IMS-GU-WDB101                                              
052700                                                                          
052800       PERFORM F-SKAPA-SASO-BILAGA                                        
052900                                                                          
053000     END-IF                                                               
053100                                                                          
053200     PERFORM Z-FINIT                                                      
053300                                                                          
053400     MOVE ZERO TO RETURN-CODE                                             
053500     GOBACK                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 A-INIT SECTION.                                                          
053900                                                                          
054000     MOVE '**-- A-INIT**--'        TO WS-SEKTION                          
054100*                                                                         
054200     MOVE ZERO TO SASOTOT-KVLEVART                                        
054300                  SASOTOT-PRARTNTO                                        
054400                  GRPTOT-KVLEVART                                         
054500                  GRPTOT-PRARTNTO                                         
054600                  SASOTAB-ANTAL-POST                                      
054700     MOVE +1 TO INDX                                                      
054800     PERFORM UNTIL                                                        
054900      ( INDX > SASOTAB-MAX-ANTAL-POST )                                   
055000        MOVE ZERO      TO SASO-IDARTNR (INDX)                             
055100        ADD +1    TO INDX                                                 
055200     END-PERFORM                                                          
055300                                                                          
055400     MOVE ZERO     TO TAB-IX                                              
055500                                                                          
055600     ACCEPT DAGENS-DATUM  FROM DATE                                       
055700     MOVE   DAGENS-DATUM  TO WS-AAMMDD                                    
055800                                                                          
055900     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
056000                                                                          
056100     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
056200                              WS-PRT-IDPRTLST                             
056300     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
056400     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
056500     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
056600                              W-WDB201-IDDISTR                            
056700                              W-WDE111-IDDISTR-MIN                        
056800                              W-WDE111-IDDISTR-MAX                        
056900                              TEST-IDDISTR                                
057000     MOVE SPACES          TO RAD-HEAD                                     
057100                             RAD2-HEAD                                    
057200                             RAD3-HEAD                                    
057300                             RAD5-HEAD                                    
057400     .                                                                    
057500     EJECT                                                                
057600 B-SPARA-DATA SECTION.                                                    
057700     MOVE 'B-SPARA-DATA'      TO WS-SEKTION                               
057800                                                                          
057900     MOVE 1                   TO INDX                                     
058000     PERFORM IMS-GNP-WDE111                                               
058100     MOVE SGMT-IDKUNDNR       TO W-WDE111-IDKUNDNR                        
058200     MOVE SGMT-IDDISTR        TO W-WDE111-IDDISTR                         
058300     PERFORM UNTIL SEGMENT-SAKNAS                                         
058400       MOVE W-IDSHIPM         TO SPAR-IDSHIPM                             
058500                                                                          
058600       PERFORM IMS-GNP-WDE121                                             
058700       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
058800       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
058900       IF W-IDFAKT = ZERO                                                 
059000         MOVE SKOLLI-IDFAKT    TO W-IDFAKT                                
059100       END-IF                                                             
059200       PERFORM UNTIL SEGMENT-SAKNAS                                       
059300         PERFORM IMS-GNP-WDE131                                           
059400         PERFORM UNTIL SEGMENT-SAKNAS                                     
059500           MOVE SRAD-IDFKNGRP      TO W-GRP                               
059600           PERFORM S01-LAES-SASO                                          
059700*                               BEART, TEXT                               
059800           PERFORM S10-INDEX-NUMMER                                       
059900           MOVE W-IND              TO SASO-IND(INDX)                      
060000           MOVE W-INDEX(1:2)       TO SASO-GRP(INDX)                      
060100           MOVE SRAD-IDARTNR       TO SASO-IDARTNR(INDX)                  
060200           MOVE SRAD-KVLEVART      TO SASO-KVLEVART(INDX)                 
060300           MOVE SRAD-KDARTURS      TO SASO-KDARTURS(INDX)                 
060400           MOVE SRAD-PRARTNTO      TO SASO-PRARTNTO(INDX)                 
060500           IF DIST79-DEALER-PRICE                                         
060600             MOVE SRAD-PRARTNTO-LOC TO SASO-PRARTNTO(INDX)                
060700           END-IF                                                         
060800           COMPUTE SASO-PRARTNTO(INDX) = SASO-PRARTNTO(INDX) *            
060900                                         SASO-KVLEVART(INDX)              
061000           COMPUTE SASOTOT-PRARTNTO = SASOTOT-PRARTNTO +                  
061100                                      SASO-PRARTNTO(INDX)                 
061200           COMPUTE SASOTOT-KVLEVART = SASOTOT-KVLEVART +                  
061300                                      SASO-KVLEVART(INDX)                 
061400           MOVE SRAD-IDSTATNR      TO SASO-IDSTAT(INDX)                   
061500           MOVE SRAD-KDVALISO      TO SASO-KDVALISO(INDX)                 
061600           MOVE WS-KDFRAKT         TO SASO-SHIP(INDX)                     
061700           IF SKOLLI-KDFRAKT = 17 OR 18 OR 19                             
061800             MOVE WS-KDFRAKT-AIR   TO SASO-SHIP(INDX)                     
061900           ELSE                                                           
062000             IF SKOLLI-KDFRAKT = 41 OR 42 OR 43                           
062100               MOVE WS-KDFRAKT-SEA TO SASO-SHIP(INDX)                     
062200             ELSE                                                         
062300              IF SKOLLI-KDFRAKT = 50                                      
062400               MOVE WS-KDFRAKT-DHL TO SASO-SHIP(INDX)                     
062500              END-IF                                                      
062600             END-IF                                                       
062700           END-IF                                                         
062800           MOVE INDX               TO SASOTAB-ANTAL-POST                  
062900**         MOVE SASOTAB-MAX-ANTAL-POST TO INDX                            
063000           IF INDX = SASOTAB-MAX-ANTAL-POST                               
063100            MOVE 'SASO TABELL FÖR LITEN ' TO FELTEXT                      
063200            CALL FELLOG                                                   
063300           END-IF                                                         
063400          MOVE SRAD-IDARTNR        TO W-IDARTNR                           
063500          MOVE SHIP-IDDC           TO W-IDDC                              
063600                                                                          
063700          ADD +1                   TO INDX                                
063800          PERFORM IMS-GNP-WDE131                                          
063900         END-PERFORM                                                      
064000         PERFORM IMS-GNP-WDE121                                           
064100         MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                     
064200         MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                      
064300       END-PERFORM                                                        
064400                                                                          
064500       PERFORM IMS-GNP-WDE111                                             
064600       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
064700       MOVE SGMT-IDDISTR      TO W-WDE111-IDDISTR                         
064800     END-PERFORM                                                          
064900                                                                          
065000     .                                                                    
065100     EJECT                                                                
065200 C-INIT-ALLM SECTION.                                                     
065300     MOVE 'C-INIT-ALLM'           TO WS-SEKTION                           
065400                                                                          
065500     MOVE ZERO  TO WS-SIDNR                                               
065600                                                                          
065700     MOVE +1    TO TAB-IX                                                 
065800                                                                          
065900     MOVE +45   TO WS-RADNR                                               
066000                                                                          
066100     MOVE SPACE TO SPAR-IDKUNDRF                                          
066200                                                                          
066300     IF WS-AA < 50                                                        
066400       MOVE 20                        TO WS-CENTURY                       
066500     ELSE                                                                 
066600       MOVE 19                        TO WS-CENTURY                       
066700     END-IF                                                               
066800                                                                          
066900     MOVE WS-CENTURY                  TO RUB-CC                           
067000     MOVE DAGENS-DATUM-MAANAD         TO RUB-MM                           
067100     MOVE DAGENS-DATUM-DAG            TO RUB-DD                           
067200     MOVE DAGENS-DATUM-AAR            TO RUB-AA                           
067300                                                                          
067400     MOVE WS-SIDNR                    TO SIDNR                            
067500     .                                                                    
067600     EJECT                                                                
067700 F-SKAPA-SASO-BILAGA  SECTION.                                            
067800     MOVE 'F-SKAPA-SASO-BLIAGA'       TO WS-SEKTION                       
067900                                                                          
068000     IF SASOTAB-ANTAL-POST NOT = ZERO                                     
068100        IF SASOTAB-ANTAL-POST > +1                                        
068200          MOVE SASOTAB-ANTAL-POST     TO INTSOR-POST-ANTAL                
068300          MOVE SASOTAB-POST-LAENGD    TO INTSOR-POST-LAENGD               
068400          MOVE SASOTAB-IDARTNR-LAENGD TO INTSOR-SORT-FAELT-LAENGD         
068500          CALL INTSOR USING SASO-TABELL (+1)                              
068600                            INTSOR-POST-LAENGD                            
068700                            INTSOR-POST-ANTAL                             
068800                            SASO-IDARTNR (+1)                             
068900                            INTSOR-SORT-FAELT-LAENGD                      
069000        END-IF                                                            
069100        MOVE +99 TO WS-RADNR                                              
069200        MOVE +1  TO INDX                                                  
069300        MOVE SASO-IND(INDX)             TO W-GRP                          
069400        MOVE W-IND-TEXT(W-GRP)          TO SASO-RUB2-TEXT                 
069500        MOVE SASO-GRP(1)                TO X-GRP                          
069600        PERFORM UNTIL                                                     
069700        ( INDX > SASOTAB-ANTAL-POST )                                     
069800          IF WS-RADNR > MAX-RADER                                         
069900            PERFORM S02-HUVUD-DEL-1                                       
070000            PERFORM FA-SASO-RUBRIK                                        
070100          END-IF                                                          
070200          MOVE SPACES                   TO ARBETS-RAD                     
070300          MOVE SASO-IDARTNR    (INDX)   TO RAD-IDARTNR                    
070400          MOVE SASO-BEART      (INDX)   TO RAD-BEART                      
070500          MOVE SASO-IDSTAT     (INDX)   TO RAD-IDSTAT                     
070600          MOVE SASO-KVLEVART   (INDX)   TO RAD-KVLEVART                   
070700          MOVE SASO-PRARTNTO   (INDX)   TO RAD-PRARTNTO                   
070800          COMPUTE GRPTOT-KVLEVART = GRPTOT-KVLEVART                       
070900                             + SASO-KVLEVART (INDX)                       
071000          COMPUTE GRPTOT-PRARTNTO = GRPTOT-PRARTNTO                       
071100                             + SASO-PRARTNTO (INDX)                       
071200          MOVE SASO-KDVALISO   (INDX)   TO RAD-KDVALISO                   
071300          MOVE SASO-KDARTURS   (INDX)   TO RAD-KDARTURS                   
071400          MOVE SASO-SHIP       (INDX)   TO RAD-SHIP                       
071500          MOVE PRT-AFTER-1              TO PRT-RADSKIP                    
071600          MOVE WS-SKIP1                 TO STYRTECKEN-RAD                 
071700          MOVE ARBETS-RAD               TO WS-RAD                         
071800                                           SEND-RAD                       
071900          PERFORM S05-SKRIV-EN-RAD                                        
072000          ADD 1                         TO WS-RADNR                       
072100          MOVE SPACES                   TO ARBETS-RAD                     
072200          ADD +1 TO INDX                                                  
072300                                                                          
072400         IF X-GRP NOT = SASO-GRP(INDX) OR                                 
072500            INDX > SASOTAB-ANTAL-POST                                     
072600          IF WS-RADNR + +3 > MAX-RADER                                    
072700            PERFORM S02-HUVUD-DEL-1                                       
072800            PERFORM FA-SASO-RUBRIK                                        
072900          END-IF                                                          
073000          MOVE  SPACES                   TO ARBETS-RAD                    
073100          MOVE 'TOTAL QUANTITY OF SASO-REGULATED PARTS IN THIS FUN        
073200-              'CTION GROUP'             TO ARBETS-RAD                    
073300          MOVE PRT-AFTER-2               TO PRT-RADSKIP                   
073400          MOVE WS-SKIP2                  TO STYRTECKEN-RAD                
073500          MOVE ARBETS-RAD                TO WS-RAD                        
073600                                            SEND-RAD                      
073700          PERFORM S05-SKRIV-EN-RAD                                        
073800          ADD 2                          TO WS-RADNR                      
073900          MOVE  SPACES                   TO ARBETS-RAD                    
074000          MOVE GRPTOT-KVLEVART           TO RAD-KVLEVART                  
074100          MOVE GRPTOT-PRARTNTO           TO RAD-PRARTNTO                  
074200          MOVE PRT-AFTER-1               TO PRT-RADSKIP                   
074300          MOVE WS-SKIP1                  TO STYRTECKEN-RAD                
074400          MOVE ARBETS-RAD                TO WS-RAD                        
074500                                            SEND-RAD                      
074600          PERFORM S05-SKRIV-EN-RAD                                        
074700          ADD 1                          TO WS-RADNR                      
074800          MOVE  SPACES                   TO ARBETS-RAD                    
074900          MOVE ALL '_'                   TO ARBETS-RAD                    
075000          MOVE PRT-AFTER-2               TO PRT-RADSKIP                   
075100          MOVE WS-SKIP2                  TO STYRTECKEN-RAD                
075200          MOVE ARBETS-RAD                TO WS-RAD                        
075300                                            SEND-RAD                      
075400          PERFORM S05-SKRIV-EN-RAD                                        
075500          ADD 2                          TO WS-RADNR                      
075600          MOVE ZERO                      TO GRPTOT-KVLEVART               
075700                                            GRPTOT-PRARTNTO               
075800          IF INDX NOT > SASOTAB-ANTAL-POST                                
075900            MOVE SASO-IND(INDX)          TO W-GRP                         
076000            MOVE W-IND-TEXT(W-GRP)       TO SASO-RUB2-TEXT                
076100            MOVE SASO-GRP(INDX)          TO X-GRP                         
076200          END-IF                                                          
076300          IF INDX > SASOTAB-ANTAL-POST                                    
076400            CONTINUE                                                      
076500          ELSE                                                            
076600            MOVE +99                     TO WS-RADNR                      
076700          END-IF                                                          
076800         END-IF                                                           
076900        END-PERFORM                                                       
077000        IF WS-RADNR + +5 > MAX-RADER                                      
077100          PERFORM S02-HUVUD-DEL-1                                         
077200*         PERFORM FA-SASO-RUBRIK                                          
077300        END-IF                                                            
077400        MOVE  SPACES                   TO ARBETS-RAD                      
077500        MOVE ALL '_'                   TO ARBETS-RAD                      
077600        MOVE PRT-AFTER-2               TO PRT-RADSKIP                     
077700        MOVE WS-SKIP2                  TO STYRTECKEN-RAD                  
077800        MOVE ARBETS-RAD                TO WS-RAD                          
077900                                          SEND-RAD                        
078000        PERFORM S05-SKRIV-EN-RAD                                          
078100        MOVE  SPACES                   TO ARBETS-RAD                      
078200        MOVE 'TOTAL QUANTITY OF SASO-REGULATED PARTS IN THIS INVOI        
078300-            'CE        '              TO ARBETS-RAD                      
078400        MOVE PRT-AFTER-2               TO PRT-RADSKIP                     
078500        MOVE WS-SKIP2                  TO STYRTECKEN-RAD                  
078600        MOVE ARBETS-RAD                TO WS-RAD                          
078700                                          SEND-RAD                        
078800        PERFORM S05-SKRIV-EN-RAD                                          
078900        MOVE  SPACES                   TO ARBETS-RAD                      
079000        MOVE SASOTOT-KVLEVART          TO RAD-KVLEVART                    
079100        MOVE SASOTOT-PRARTNTO          TO RAD-PRARTNTO                    
079200        MOVE PRT-AFTER-1               TO PRT-RADSKIP                     
079300        MOVE WS-SKIP1                  TO STYRTECKEN-RAD                  
079400        MOVE ARBETS-RAD                TO WS-RAD                          
079500                                          SEND-RAD                        
079600        PERFORM S05-SKRIV-EN-RAD                                          
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 FA-SASO-RUBRIK   SECTION.                                                
080100     MOVE '***-- FA-SASO-RUBRIK ***---'  TO WS-SEKTION                    
080200                                                                          
080300     MOVE SPACES                         TO ARBETS-RAD                    
080400     MOVE W-PRODUCTGROUP1                TO SASO-RUB1-TEXT                
080500     IF W-GRP = 20                                                        
080600       MOVE W-PRODUCTGROUP2              TO SASO-RUB1-TEXT                
080700     END-IF                                                               
080800     IF W-GRP = 21                                                        
080900       MOVE W-PRODUCTGROUP3              TO SASO-RUB1-TEXT                
081000     END-IF                                                               
081100     IF W-GRP = 22                                                        
081200       MOVE W-PRODUCTGROUP4              TO SASO-RUB1-TEXT                
081300     END-IF                                                               
081400*         1,2,3 ELLER 4                                                   
081500**   MOVE SASO-RUB1                      TO ARBETS-RAD                    
081600     MOVE SPACE                          TO ARBETS-RAD                    
081700     MOVE ARBETS-RAD                     TO WS-RAD                        
081800                                            SEND-RAD                      
081900     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
082000     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
082100     PERFORM S05-SKRIV-EN-RAD                                             
082200     ADD 2                               TO WS-RADNR                      
082300     MOVE SPACES                         TO ARBETS-RAD                    
082400     MOVE SASO-RUB2                      TO ARBETS-RAD                    
082500     MOVE ARBETS-RAD                     TO WS-RAD                        
082600                                            SEND-RAD                      
082700     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
082800     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
082900     PERFORM S05-SKRIV-EN-RAD                                             
083000     ADD 2                               TO WS-RADNR                      
083100     MOVE SPACES                         TO ARBETS-RAD                    
083200     MOVE SASO-RUB3                      TO ARBETS-RAD                    
083300     MOVE ARBETS-RAD                     TO WS-RAD                        
083400                                            SEND-RAD                      
083500     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
083600     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
083700     PERFORM S05-SKRIV-EN-RAD                                             
083800     ADD 2                               TO WS-RADNR                      
083900     MOVE SPACES                         TO ARBETS-RAD                    
084000     MOVE ARBETS-RAD                     TO WS-RAD                        
084100                                            SEND-RAD                      
084200     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
084300     MOVE WS-SKIP1                       TO STYRTECKEN-RAD                
084400     PERFORM S05-SKRIV-EN-RAD                                             
084500     ADD 1                               TO WS-RADNR                      
084600     .                                                                    
084700     EJECT                                                                
084800 Z-FINIT SECTION.                                                         
084900                                                                          
085000     MOVE 'S' TO POSTSUM-OPKOD                                            
085100     CALL POSTSUM USING POSTSUM-PARM                                      
085200     .                                                                    
085300     EJECT                                                                
085400 S01-LAES-SASO SECTION.                                                   
085500                                                                          
085600*      BEHÖVER NÅGOT REGISTER LÄSAS ?                                     
085700     MOVE SRAD-IDARTNR          TO W-IDARTNR                              
085800     MOVE 'GB '                 TO W-IDSKYLT                              
085900     PERFORM IMS-GU-WDD311                                                
086000     IF SEGMENT-FINNS                                                     
086100       MOVE TEXT-BEART          TO SASO-BEART(INDX)                       
086200                                   W-BEART                                
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 S02-HUVUD-DEL-1     SECTION.                                             
086700     MOVE '*-- S02-HUVUD-DEL-1 *--'  TO WS-SEKTION                        
086800*                                                                         
086900*    MOVE SHIP-IDDC TO WS-IDDC                                            
087000                                                                          
087100     MOVE ZERO                       TO WS-RADNR                          
087200     MOVE SPACES                     TO ARBETS-RAD                        
087300                                        WS-RAD                            
087400                                        SEND-RAD                          
087500     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
087600     PERFORM S90-PUT-DOC-LINE                                             
087700*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
087800*    PERFORM S90-PUT-DOC-LINE                                             
087900     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
088000     PERFORM S90-PUT-DOC-LINE                                             
088100     MOVE BET-BEBETRAD-1             TO RAD1H-IMPORTER                    
088200     MOVE BET-BEBETRAD-2             TO RAD2H-IMPORTER                    
088300     MOVE BET-ADBETRAD-1             TO RAD3H-IMPORTER                    
088400     MOVE BET-ADBETRAD-2             TO RAD4H-IMPORTER                    
088500     MOVE BET-BELAND-SVE             TO RAD5H-IMPORTER                    
088600     MOVE 'INVOICE: '                TO RAD5-FILL                         
088700     MOVE W-IDFAKT                   TO RAD5-IDFAKT                       
088800     MOVE 'IMPORTER REF'             TO RAD1H-IMPORTER-TEXT               
088900     MOVE SASO-RUBRIK                TO RAD-TYP-IDSHIP                    
089000     MOVE RAD-HEAD                   TO WS-RAD                            
089100                                        SEND-RAD                          
089200     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
089300     ADD 7                           TO WS-RADNR                          
089400     PERFORM S05-SKRIV-EN-RAD                                             
089500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
089600     MOVE RAD2-HEAD                  TO ARBETS-RAD                        
089700                                        WS-RAD                            
089800                                        SEND-RAD                          
089900     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
090000     ADD 1                           TO WS-RADNR                          
090100     PERFORM S05-SKRIV-EN-RAD                                             
090200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
090300                                                                          
090400     MOVE RAD3-HEAD                  TO ARBETS-RAD                        
090500                                        WS-RAD                            
090600                                        SEND-RAD                          
090700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
090800     ADD 1                           TO WS-RADNR                          
090900     PERFORM S05-SKRIV-EN-RAD                                             
091000*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
091100                                                                          
091200     ADD 1                     TO WS-SIDNR                                
091300     MOVE SPACE                TO RAD1                                    
091400     MOVE BET-ADBETRAD-2       TO RAD4H-IMPORTER                          
091500     MOVE SHIP-TISKEPPN        TO RAD1-TIAAMMDD                           
091600     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
091700     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
091800     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
091900     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
092000     MOVE WS-SIDNR             TO RAD1-PAGE-NO                            
092100     MOVE RAD1                 TO ARBETS-RAD                              
092200                                  WS-RAD                                  
092300                                  SEND-RAD                                
092400     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
092500     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
092600     ADD 1                     TO WS-RADNR                                
092700     PERFORM S05-SKRIV-EN-RAD                                             
092800     MOVE RAD5-HEAD                  TO ARBETS-RAD                        
092900                                        WS-RAD                            
093000                                        SEND-RAD                          
093100     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
093200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
093300     ADD 1                           TO WS-RADNR                          
093400     PERFORM S05-SKRIV-EN-RAD                                             
093500     .                                                                    
093600     EJECT                                                                
093700 S05-SKRIV-EN-RAD  SECTION.                                               
093800     MOVE 'S05-SKRIV-EN-RAD'     TO WS-SEKTION                            
093900                                                                          
094000     PERFORM S90-PUT-DOC-LINE                                             
094100     IF TRPD-IDPGM = 'W4062200' OR                                        
094200        TRPD-IDPGM = 'W4063400'                                           
094300      CALL W006PRS1 USING PRT-SPOOL-OVR                                   
094400                          PRT-WRITE                                       
094500                          W-IDPRTLST                                      
094600                          ALT-PCB                                         
094700                          PRT-RADSKIP                                     
094800                          WS-RAD                                          
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 S10-INDEX-NUMMER  SECTION.                                               
095300     MOVE 'S10-INDEX-NUMMER'     TO WS-SEKTION                            
095400                                                                          
095500     MOVE W-INDEX-23             TO W-INDEX                               
095600     MOVE 23                     TO W-IND                                 
095700     MOVE SRAD-IDFKNGRP          TO W-GRP                                 
095800     IF W-GRP = 2611 AND W-BEART = 'RADIATOR, EXCH           ' OR         
095900        W-GRP = 2612 AND W-BEART = 'EXPANSION TANK           ' OR         
096000        W-GRP = 2613 AND W-BEART = 'FILLER CAP               ' OR         
096100        W-GRP = 2615 AND W-BEART = 'RADIATOR HOSE            ' OR         
096200        W-GRP = 2615 AND W-BEART = 'RADIATOR HOSE KIT        ' OR         
096300        W-GRP = 2615 AND W-BEART = 'RADIATOR HOSE, UPPER     ' OR         
096400        W-GRP = 2615 AND W-BEART = 'RADIATOR HOSE, LOWER     ' OR         
096500        W-GRP = 2615 AND W-BEART = 'FILLER HOSE              '            
096600        MOVE W-INDEX-01          TO W-INDEX                               
096700        MOVE 1                   TO W-IND                                 
096800     END-IF                                                               
096900*                                                                         
097000     IF W-GRP = 5112 AND W-BEART = 'BRAKE DISC               ' OR         
097100        W-GRP = 5112 AND W-BEART = 'BRAKE DISC KIT           ' OR         
097200        W-GRP = 5115 AND W-BEART = 'BRAKE PAD KIT            ' OR         
097300        W-GRP = 5121 AND W-BEART = 'BRAKE PIPE               ' OR         
097400        W-GRP = 5122 AND W-BEART = 'BRAKE DISC               ' OR         
097500        W-GRP = 5122 AND W-BEART = 'BRAKE DISC KIT           ' OR         
097600        W-GRP = 5125 AND W-BEART = 'BRAKE PAD KIT            ' OR         
097700        W-GRP = 5211 AND W-BEART = 'BRAKE CYLINDER, EXCH     ' OR         
097800        W-GRP = 5214 AND W-BEART = 'SERVO CYLINDER           ' OR         
097900        W-GRP = 5221 AND W-BEART = 'BRAKE PIPE               ' OR         
098000        W-GRP = 5222 AND W-BEART = 'BRAKE HOSE               ' OR         
098100        W-GRP = 5511 AND W-BEART = 'PARKING BRAKE LEVER      ' OR         
098200        W-GRP = 5513 AND W-BEART = 'HAND-BRAKE CABLE, FRONT  ' OR         
098300        W-GRP = 5513 AND W-BEART = 'HAND-BRAKE CABLE         ' OR         
098400        W-GRP = 5531 AND W-BEART = 'BRAKE SHOE KIT           '            
098500        MOVE W-INDEX-02          TO W-INDEX                               
098600        MOVE 2                   TO W-IND                                 
098700     END-IF                                                               
098800*                                                                         
098900     IF W-GRP = 3511 AND W-BEART = 'BULB                     ' OR         
099000        W-GRP = 3512 AND W-BEART = 'BULB                     ' OR         
099100        W-GRP = 3513 AND W-BEART = 'BULB                     ' OR         
099200        W-GRP = 3514 AND W-BEART = 'BULB                     ' OR         
099300        W-GRP = 3515 AND W-BEART = 'BULB KIT                 ' OR         
099400        W-GRP = 3519 AND W-BEART = 'BULB                     ' OR         
099500        W-GRP = 3521 AND W-BEART = 'HEADLAMP, L.H.           ' OR         
099600        W-GRP = 3521 AND W-BEART = 'HEADLAMP, R.H.           ' OR         
099700        W-GRP = 3522 AND W-BEART = 'DIRECTION INDICATOR, L.H.' OR         
099800        W-GRP = 3522 AND W-BEART = 'DIRECTION INDICATOR, R.H.' OR         
099900        W-GRP = 3522 AND W-BEART = 'DIRECTION INDICATOR      ' OR         
100000        W-GRP = 3522 AND W-BEART = 'LENS, L.H.               ' OR         
100100        W-GRP = 3522 AND W-BEART = 'LENS, R.H.               ' OR         
100200        W-GRP = 3522 AND W-BEART = 'HOUSING, L.H.            ' OR         
100300        W-GRP = 3522 AND W-BEART = 'HOUSING, R.H.            ' OR         
100400        W-GRP = 3526 AND W-BEART = 'DIRECTION INDICATOR, L.H.' OR         
100500        W-GRP = 3526 AND W-BEART = 'DIRECTION INDICATOR, R.H.' OR         
100600        W-GRP = 3526 AND W-BEART = 'COMBINED LAMP, L.H.      ' OR         
100700        W-GRP = 3526 AND W-BEART = 'COMBINED LAMP, R.H.      ' OR         
100800        W-GRP = 3527 AND W-BEART = 'LAMP BODY, L.H.          ' OR         
100900        W-GRP = 3527 AND W-BEART = 'LAMP BODY                ' OR         
101000        W-GRP = 3527 AND W-BEART = 'LENS, L.H.               ' OR         
101100        W-GRP = 3527 AND W-BEART = 'LENS, R.H.               ' OR         
101200        W-GRP = 3531 AND W-BEART = 'HOUSING, L.H.            ' OR         
101300        W-GRP = 3531 AND W-BEART = 'HOUSING, R.H.            ' OR         
101400        W-GRP = 3531 AND W-BEART = 'BRAKE LAMP               ' OR         
101500        W-GRP = 3531 AND W-BEART = 'TAIL LAMP, L.H.          ' OR         
101600        W-GRP = 3531 AND W-BEART = 'TAIL LAMP, R.H.          ' OR         
101700        W-GRP = 3532 AND W-BEART = 'HOUSING, L.H.            ' OR         
101800        W-GRP = 3532 AND W-BEART = 'HOUSING, R.H.            ' OR         
101900        W-GRP = 3541 AND W-BEART = 'INTERIOR LAMP            ' OR         
102000        W-GRP = 3549 AND W-BEART = 'WARNING LAMP             ' OR         
102100        W-GRP = 3551 AND W-BEART = 'LAMP BODY,L.H.           ' OR         
102200        W-GRP = 3551 AND W-BEART = 'LAMP BODY,R.H.           ' OR         
102300        W-GRP = 3551 AND W-BEART = 'POSITION LAMP            ' OR         
102400        W-GRP = 3552 AND W-BEART = 'DIRECTION INDICATOR, L.H.' OR         
102500        W-GRP = 3552 AND W-BEART = 'DIRECTION INDICATOR, R.H.' OR         
102600        W-GRP = 3552 AND W-BEART = 'DIRECTION INDICATOR      ' OR         
102700        W-GRP = 3561 AND W-BEART = 'FRONT FOG LAMP, LEFT     ' OR         
102800        W-GRP = 3561 AND W-BEART = 'FRONT FOG LAMP, RIGHT    ' OR         
102900        W-GRP = 3561 AND W-BEART = 'FRONT FOG LAMP           ' OR         
103000        W-GRP = 8469 AND W-BEART = 'BULB                     '            
103100        MOVE W-INDEX-03          TO W-INDEX                               
103200        MOVE 3                   TO W-IND                                 
103300     END-IF                                                               
103400*                                                                         
103500     IF W-GRP = 2349 AND W-BEART = 'AIR FILTER               ' OR         
103600        W-GRP = 2349 AND W-BEART = 'FILTER                   ' OR         
103700        W-GRP = 2561 AND W-BEART = 'AIR FILTER               ' OR         
103800        W-GRP = 2562 AND W-BEART = 'FILTER INSERT            ' OR         
103900        W-GRP = 2223 AND W-BEART = 'OIL FILTER               ' OR         
104000        W-GRP = 2223 AND W-BEART = 'FILTER INSERT            ' OR         
104100        W-GRP = 2334 AND W-BEART = 'FUEL FILTER              ' OR         
104200        W-GRP = 2334 AND W-BEART = 'FILTER                   ' OR         
104300        W-GRP = 2335 AND W-BEART = 'FILTER                   '            
104400        MOVE W-INDEX-04          TO W-INDEX                               
104500        MOVE 4                   TO W-IND                                 
104600     END-IF                                                               
104700*                                                                         
104800     IF W-GRP = 2519 AND W-BEART = 'CONVERSION KIT           ' OR         
104900        W-GRP = 2521 AND W-BEART = 'SILENCER                 ' OR         
105000        W-GRP = 2521 AND W-BEART = 'SILENCER, FRONT          ' OR         
105100        W-GRP = 2521 AND W-BEART = 'SILENCER, REAR           ' OR         
105200        W-GRP = 2522 AND W-BEART = 'EXHAUST PIPE             ' OR         
105300        W-GRP = 2522 AND W-BEART = 'EXHAUST SYSTEM           ' OR         
105400        W-GRP = 2524 AND W-BEART = 'END PIPE                 ' OR         
105500        W-GRP = 2524 AND W-BEART = 'SILENCER, REAR           ' OR         
105600        W-GRP = 2525 AND W-BEART = 'EXHAUST SYSTEM KIT       '            
105700        MOVE W-INDEX-05          TO W-INDEX                               
105800        MOVE 5                   TO W-IND                                 
105900     END-IF                                                               
106000*                                                                         
106100     IF W-GRP = 4111 AND W-BEART = 'CLUTCH KIT               ' OR         
106200        W-GRP = 4117 AND W-BEART = 'DRIVEN PLATE             ' OR         
106300        W-GRP = 4121 AND W-BEART = 'RELEASE BEARING          ' OR         
106400        W-GRP = 4134 AND W-BEART = 'CLUTCH CYLINDER, EXCH    ' OR         
106500        W-GRP = 4136 AND W-BEART = 'MASTER CYLINDER          ' OR         
106600        W-GRP = 6455 AND W-BEART = 'FLUID RESERVOIR          '            
106700        MOVE W-INDEX-06          TO W-INDEX                               
106800        MOVE 6                   TO W-IND                                 
106900     END-IF                                                               
107000*                                                                         
107100     IF W-GRP = 8541 AND W-BEART = 'CHILD SEAT               ' OR         
107200        W-GRP = 8571 AND W-BEART = 'HEAD CUSHION             ' OR         
107300        W-GRP = 8571 AND W-BEART = 'CHILD CUSHION            ' OR         
107400        W-GRP = 8571 AND W-BEART = 'CHILD SEAT               '            
107500        MOVE W-INDEX-07          TO W-INDEX                               
107600        MOVE 7                   TO W-IND                                 
107700     END-IF                                                               
107800*                                                                         
107900     IF W-GRP = 2812 AND W-BEART = 'SPARK PLUG KIT           '            
108000        MOVE W-INDEX-08          TO W-INDEX                               
108100        MOVE 8                   TO W-IND                                 
108200     END-IF                                                               
108300*                                                                         
108400     IF W-GRP = 3633 AND W-BEART = 'WIPER MOTOR, L.H.        ' OR         
108500        W-GRP = 3633 AND W-BEART = 'WIPER MOTOR, R.H.        ' OR         
108600        W-GRP = 3636 AND W-BEART = 'WIPER BLADE KIT          ' OR         
108700        W-GRP = 3636 AND W-BEART = 'WIPER BLADE              ' OR         
108800        W-GRP = 3637 AND W-BEART = 'HEADLAMP WIPER ARM       ' OR         
108900        W-GRP = 3637 AND W-BEART = 'WIPER ARM, L.H.          ' OR         
109000        W-GRP = 3637 AND W-BEART = 'WIPER ARM, R.H.          '            
109100        MOVE W-INDEX-09          TO W-INDEX                               
109200        MOVE 9                   TO W-IND                                 
109300     END-IF                                                               
109400*                                                                         
109500     IF W-GRP = 8841 AND W-BEART = 'SEAT BELT, L.H.          ' OR         
109600        W-GRP = 8841 AND W-BEART = 'SEAT BELT, R.H.          ' OR         
109700        W-GRP = 8842 AND W-BEART = 'SEAT BELT, L.H.          ' OR         
109800        W-GRP = 8842 AND W-BEART = 'SEAT BELT, R.H.          ' OR         
109900        W-GRP = 8842 AND W-BEART = 'SEAT BELT                ' OR         
110000        W-GRP = 8842 AND W-BEART = 'SEAT BELT, CENTRE        ' OR         
110100        W-GRP = 8843 AND W-BEART = 'BELT CATCH               ' OR         
110200        W-GRP = 8843 AND W-BEART = 'BELT CATCH, L.H.         ' OR         
110300        W-GRP = 8843 AND W-BEART = 'BELT CATCH, R.H.         '            
110400        MOVE W-INDEX-10          TO W-INDEX                               
110500        MOVE 10                  TO W-IND                                 
110600     END-IF                                                               
110700*                                                                         
110800     IF W-GRP = 2341 AND W-BEART = 'FUEL TANK                ' OR         
110900        W-GRP = 2341 AND W-BEART = 'TANK                     ' OR         
111000        W-GRP = 2342 AND W-BEART = 'FILLER CAP               ' OR         
111100        W-GRP = 2349 AND W-BEART = 'TANK                     ' OR         
111200        W-GRP = 2349 AND W-BEART = 'FUEL TANK                ' OR         
111300        W-GRP = 2441 AND W-BEART = 'TANK                     '            
111400        MOVE W-INDEX-11          TO W-INDEX                               
111500        MOVE 11                  TO W-IND                                 
111600     END-IF                                                               
111700*                                                                         
111800     IF W-GRP = 8461 AND W-BEART = 'REAR VIEW MIRROR, L.H.   ' OR         
111900        W-GRP = 8461 AND W-BEART = 'REAR VIEW MIRROR, R.H.   ' OR         
112000        W-GRP = 8462 AND W-BEART = 'REAR VIEW MIRROR, L.H.   ' OR         
112100        W-GRP = 8462 AND W-BEART = 'REAR VIEW MIRROR, R.H.   ' OR         
112200        W-GRP = 8462 AND W-BEART = 'REAR VIEW MIRROR KIT     ' OR         
112300        W-GRP = 8463 AND W-BEART = 'GLASS, L.H.              ' OR         
112400        W-GRP = 8463 AND W-BEART = 'GLASS, R.H.              ' OR         
112500        W-GRP = 8463 AND W-BEART = 'MIRROR GLASS, L.H.       ' OR         
112600        W-GRP = 8463 AND W-BEART = 'MIRROR GLASS, R.H.       ' OR         
112700        W-GRP = 8823 AND W-BEART = 'REAR VIEW MIRROR         '            
112800        MOVE W-INDEX-12          TO W-INDEX                               
112900        MOVE 12                  TO W-IND                                 
113000     END-IF                                                               
113100*                                                                         
113200     IF W-GRP = 8115 AND W-BEART = 'BUMPER RAIL              ' OR         
113300        W-GRP = 8611 AND W-BEART = 'BUMPER RAIL              ' OR         
113400        W-GRP = 8612 AND W-BEART = 'ABSORBER                 ' OR         
113500        W-GRP = 8612 AND W-BEART = 'MOULDING                 ' OR         
113600        W-GRP = 8612 AND W-BEART = 'BUMPER MEMBER            ' OR         
113700        W-GRP = 8612 AND W-BEART = 'REAR SECTION             ' OR         
113800        W-GRP = 8613 AND W-BEART = 'COVER PANEL, L.H.        ' OR         
113900        W-GRP = 8613 AND W-BEART = 'COVER PANEL, R.H.        ' OR         
114000        W-GRP = 8613 AND W-BEART = 'MOULDING                 ' OR         
114100        W-GRP = 8613 AND W-BEART = 'STRIP, L.H.              ' OR         
114200        W-GRP = 8613 AND W-BEART = 'STRIP, R.H.              ' OR         
114300        W-GRP = 8613 AND W-BEART = 'STRIP, CENTRE            ' OR         
114400        W-GRP = 8614 AND W-BEART = 'COVER                    ' OR         
114500        W-GRP = 8614 AND W-BEART = 'COVER PANEL              ' OR         
114600        W-GRP = 8614 AND W-BEART = 'COVER, FRONT             ' OR         
114700        W-GRP = 8614 AND W-BEART = 'COVER, REAR              ' OR         
114800        W-GRP = 8614 AND W-BEART = 'PANEL, L.H.              ' OR         
114900        W-GRP = 8614 AND W-BEART = 'PANEL, R.H.              ' OR         
115000        W-GRP = 8614 AND W-BEART = 'MOULDING                 ' OR         
115100        W-GRP = 8614 AND W-BEART = 'REINFORCEMENT            ' OR         
115200        W-GRP = 8614 AND W-BEART = 'SUPPORT, L.H.            ' OR         
115300        W-GRP = 8614 AND W-BEART = 'SUPPORT, R.H.            ' OR         
115400        W-GRP = 8615 AND W-BEART = 'SUPPORT, L.H.            ' OR         
115500        W-GRP = 8615 AND W-BEART = 'SUPPORT, R.H.            ' OR         
115600        W-GRP = 8615 AND W-BEART = 'ABSORBER                 ' OR         
115700        W-GRP = 8615 AND W-BEART = 'GUIDE, L.H.              ' OR         
115800        W-GRP = 8615 AND W-BEART = 'GUIDE, R.H.              ' OR         
115900        W-GRP = 8615 AND W-BEART = 'GUIDE                    ' OR         
116000        W-GRP = 8915 AND W-BEART = 'SPOILER                  ' OR         
116100        W-GRP = 8915 AND W-BEART = 'AIR GUIDE                '            
116200        MOVE W-INDEX-13          TO W-INDEX                               
116300        MOVE 13                  TO W-IND                                 
116400     END-IF                                                               
116500*                                                                         
116600     IF W-GRP = 3662 AND W-BEART = 'REMOTE CONTROL           ' OR         
116700        W-GRP = 3669 AND W-BEART = 'REMOTE CONTROL           ' OR         
116800        W-GRP = 3972 AND W-BEART = 'REMOTE CONTROL           ' OR         
116900        W-GRP = 6413 AND W-BEART = 'COLUMN LOCK              ' OR         
117000        W-GRP = 8313 AND W-BEART = 'HINGE                    ' OR         
117100        W-GRP = 8313 AND W-BEART = 'HINGE, L.H.              ' OR         
117200        W-GRP = 8313 AND W-BEART = 'HINGE, R.H.              ' OR         
117300        W-GRP = 8341 AND W-BEART = 'LOCK, L.H.               ' OR         
117400        W-GRP = 8341 AND W-BEART = 'LOCK, R.H.               ' OR         
117500        W-GRP = 8342 AND W-BEART = 'COLUMN LOCK              ' OR         
117600        W-GRP = 8342 AND W-BEART = 'LOCK CYLINDER            ' OR         
117700        W-GRP = 8342 AND W-BEART = 'LOCK CYLINDER, SET       ' OR         
117800        W-GRP = 8342 AND W-BEART = 'LOCK KIT                 ' OR         
117900        W-GRP = 8347 AND W-BEART = 'LOCK                     ' OR         
118000        W-GRP = 8349 AND W-BEART = 'LOCK BUTTON, R.H.        ' OR         
118100        W-GRP = 8349 AND W-BEART = 'REMOTE LOCK              '            
118200        MOVE W-INDEX-14          TO W-INDEX                               
118300        MOVE 14                  TO W-IND                                 
118400     END-IF                                                               
118500*                                                                         
118600     IF W-GRP = 7724 AND W-BEART = 'WINTER TYRE              ' OR         
118700        W-GRP = 7726 AND W-BEART = 'TYRE                     '            
118800        MOVE W-INDEX-15          TO W-INDEX                               
118900        MOVE 15                  TO W-IND                                 
119000     END-IF                                                               
119100*                                                                         
119200     IF W-GRP = 1223 AND W-BEART = 'BELT                     ' OR         
119300        W-GRP = 1223 AND W-BEART = 'V-BELT                   ' OR         
119400        W-GRP = 1223 AND W-BEART = 'DRIVE BELT               '            
119500        MOVE W-INDEX-16          TO W-INDEX                               
119600        MOVE 16                  TO W-IND                                 
119700     END-IF                                                               
119800*                                                                         
119900     IF W-GRP = 1912 AND W-BEART = 'WARNING TRIANGLE         ' OR         
120000        W-GRP = 3549 AND W-BEART = 'WARNING LAMP             ' OR         
120100        W-GRP = 3639 AND W-BEART = 'DIRECTION IND. SWITCH    ' OR         
120200        W-GRP = 3643 AND W-BEART = 'DIRECTION IND. SWITCH    ' OR         
120300        W-GRP = 3811 AND W-BEART = 'COMBINED INSTRUMENT, EXCH' OR         
120400        W-GRP = 3821 AND W-BEART = 'TEMPERATURE GAUGE        ' OR         
120500        W-GRP = 3822 AND W-BEART = 'FUEL GAUGE               ' OR         
120600        W-GRP = 3831 AND W-BEART = 'SPEEDOMETER              ' OR         
120700        W-GRP = 3837 AND W-BEART = 'ODOMETER                 '            
120800        MOVE W-INDEX-17          TO W-INDEX                               
120900        MOVE 17                  TO W-IND                                 
121000     END-IF                                                               
121100*                                                                         
121200     IF W-GRP = 7702 AND W-BEART = 'WINTER WHEEL             ' OR         
121300        W-GRP = 7704 AND W-BEART = 'SPARE WHEEL              ' OR         
121400        W-GRP = 7711 AND W-BEART = 'WHEEL                    ' OR         
121500        W-GRP = 7713 AND W-BEART = 'ALUMINIUM WHEEL          ' OR         
121600        W-GRP = 7713 AND W-BEART = 'ALUMINIUM RIM            ' OR         
121700        W-GRP = 7713 AND W-BEART = 'RIM                      ' OR         
121800        W-GRP = 7731 AND W-BEART = 'FRONT WHEEL              ' OR         
121900        W-GRP = 7731 AND W-BEART = 'HUB KIT                  ' OR         
122000        W-GRP = 7732 AND W-BEART = 'REAR WHEEL HUB           '            
122100        MOVE W-INDEX-18          TO W-INDEX                               
122200        MOVE 18                  TO W-IND                                 
122300     END-IF                                                               
122400*                                                                         
122500     IF W-GRP = 6411 AND W-BEART = 'STEERING WHEEL           ' OR         
122600        W-GRP = 6422 AND W-BEART = 'STEERING GEAR, EXCH      ' OR         
122700        W-GRP = 6432 AND W-BEART = 'BALL JOINT               ' OR         
122800        W-GRP = 6432 AND W-BEART = 'STEERING ROD             ' OR         
122900        W-GRP = 6432 AND W-BEART = 'BALL JOINT KIT           ' OR         
123000        W-GRP = 6435 AND W-BEART = 'STEERING ROD             '            
123100        MOVE W-INDEX-19          TO W-INDEX                               
123200        MOVE 19                  TO W-IND                                 
123300     END-IF                                                               
123400*                                                                         
123500     IF W-GRP = 1832 AND W-BEART = 'ENGINE                   ' OR         
123600        W-GRP = 1833 AND W-BEART = 'TRANSMISSION OIL         ' OR         
123700        W-GRP = 1834 AND W-BEART = 'REAR AXLE OIL            ' OR         
123800        W-GRP = 1839 AND W-BEART = 'POWER STEERING OIL       ' OR         
123900        W-GRP = 1841 AND W-BEART = 'ANTI-FREEZE              ' OR         
124000        W-GRP = 1841 AND W-BEART = 'ANTI-FREEZE READY MIXED  ' OR         
124100        W-GRP = 1851 AND W-BEART = 'BRAKE FLUID              ' OR         
124200        W-GRP = 1852 AND W-BEART = 'HYDRAULIC                '            
124300        MOVE W-INDEX-20          TO W-INDEX                               
124400        MOVE 20                  TO W-IND                                 
124500     END-IF                                                               
124600*                                                                         
124700     IF W-GRP = 3819 AND W-BEART = 'GLASS                    ' OR         
124800        W-GRP = 8431 AND W-BEART = 'WINDSCREEN               ' OR         
124900        W-GRP = 8433 AND W-BEART = 'REAR WINDOW              ' OR         
125000        W-GRP = 8441 AND W-BEART = 'GLASS, L.H.              ' OR         
125100        W-GRP = 8441 AND W-BEART = 'GLASS, R.H.              ' OR         
125200        W-GRP = 8443 AND W-BEART = 'GLASS, L.H.              ' OR         
125300        W-GRP = 8443 AND W-BEART = 'GLASS, R.H.              ' OR         
125400        W-GRP = 8445 AND W-BEART = 'GLASS, L.H.              ' OR         
125500        W-GRP = 8445 AND W-BEART = 'GLASS, R.H.              '            
125600        MOVE W-INDEX-21          TO W-INDEX                               
125700        MOVE 21                  TO W-IND                                 
125800     END-IF                                                               
125900*                                                                         
126000     IF W-GRP = 3111 AND W-BEART = 'BATTERY                  '            
126100        MOVE W-INDEX-22          TO W-INDEX                               
126200        MOVE 22                  TO W-IND                                 
126300     END-IF                                                               
126400*                                                                         
126500     .                                                                    
126600     EJECT                                                                
126700 S90-PUT-DOC-LINE SECTION.                                                
126800     MOVE 'S90-PUT-DOC-LINE'     TO WS-SEKTION                            
126900                                                                          
127000     IF TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1'                   
127100       IF TRPD-FLSKRIV-ONDEM = YES OR JA                                  
127200         MOVE +1                          TO SEND-IDCOM                   
127300         MOVE 'PUT'                       TO SEND-KDFUNC                  
127400         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
127500         CALL WZ01SEND USING SEND-CONTROL-AREA                            
127600                             SEND-KVDLEN                                  
127700                             SEND-RAD-STYRTECKEN                          
127800         IF SEND-KDRC > ZERO                                              
127900           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
128000           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
128100           DELIMITED BY SIZE INTO FELTEXT-STR                             
128200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
128300         END-IF                                                           
128400       END-IF                                                             
128500     END-IF                                                               
128600     .                                                                    
128700     EJECT                                                                
128800*****************IMS-SECTIONER*********************                       
128900                                                                          
129000 IMS-GU-WDE101  SECTION.                                                  
129100                                                                          
129200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
129300          DELIMITED BY SIZE INTO SSA1                                     
129400     MOVE '  GE' TO GOOD-STATUSCODES                                      
129500     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
129600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
129700     PERFORM IMS-STATUSCHECK                                              
129800     .                                                                    
129900     EJECT                                                                
130000 IMS-GNP-WDE111 SECTION.                                                  
130100                                                                          
130200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
130300          DELIMITED BY SIZE INTO SSA1                                     
130400     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
130500                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
130600          DELIMITED BY SIZE INTO SSA2                                     
130700     MOVE '  GE' TO GOOD-STATUSCODES                                      
130800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
130900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSCHECK                                              
131100     .                                                                    
131200     EJECT                                                                
131300 IMS-GNP-WDE121  SECTION.                                                 
131400                                                                          
131500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
131600          DELIMITED BY SIZE INTO SSA1                                     
131700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
131800          DELIMITED BY SIZE INTO SSA2                                     
131900     MOVE 'WDE121  '          TO SSA3                                     
132000     MOVE '  GE' TO GOOD-STATUSCODES                                      
132100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
132200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
132300     PERFORM IMS-STATUSCHECK                                              
132400     .                                                                    
132500     EJECT                                                                
132600 IMS-GNP-WDE131  SECTION.                                                 
132700                                                                          
132800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
132900          DELIMITED BY SIZE INTO SSA1                                     
133000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
133100          DELIMITED BY SIZE INTO SSA2                                     
133200     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
133300          DELIMITED BY SIZE INTO SSA3                                     
133400     MOVE 'WDE131  '          TO SSA4                                     
133500     MOVE '  GE' TO GOOD-STATUSCODES                                      
133600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
133700                                                   SSA3 SSA4              
133800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
133900     PERFORM IMS-STATUSCHECK                                              
134000     .                                                                    
134100     EJECT                                                                
134200 IMS-GU-WDB201 SECTION.                                                   
134300                                                                          
134400     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
134500          DELIMITED BY SIZE INTO SSA1                                     
134600     MOVE '  GE' TO GOOD-STATUSCODES                                      
134700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
134800     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSCHECK                                              
135000     .                                                                    
135100     EJECT                                                                
135200 IMS-GU-WDB101 SECTION.                                                   
135300                                                                          
135400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     MOVE '  GE' TO GOOD-STATUSCODES                                      
135700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
135800     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
135900     PERFORM IMS-STATUSCHECK                                              
136000     .                                                                    
136100     EJECT                                                                
136200 IMS-GU-WDD311    SECTION.                                                
136300                                                                          
136400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
136500            DELIMITED BY SIZE INTO SSA1                                   
136600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
136700            DELIMITED BY SIZE INTO SSA2                                   
136800     MOVE '  GE' TO GOOD-STATUSCODES                                      
136900     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
137000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSCHECK                                              
137200     .                                                                    
137300     EJECT                                                                
137400 IMS-STATUSCHECK SECTION.                                                 
137500                                                                          
137600     SET STATUS-IX TO 1                                                   
137700     SEARCH GOOD-STATUS                                                   
137800       AT END                                                             
137900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
138000           DELIMITED BY SIZE INTO FELTEXT                                 
138100         CALL FELLOG                                                      
138200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
138300         CONTINUE                                                         
138400     END-SEARCH                                                           
138500     .                                                                    
138600     EJECT                                                                
