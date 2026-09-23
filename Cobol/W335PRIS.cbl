000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.       W335PRIS.                                              
000400 AUTHOR.           RONNY STENHOLM                                         
000500 DATE-WRITTEN.     JULI 1996.                                             
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    KDCALL=1   NORMAL PRISTILLÄMPNING                                    
000900*    KDCALL=2   ANROP FÖR ATT ENBART HÄMTA VALUTAKOD                      
001000*                                                                         
001100*    VID KDCALL=1 UTFÖRS PRISTILLÄMPNING PÅ RS FAKTURERING,               
001200*    PER ARTIKEL. DÄRVID RÄKNAS KUNDENS NETTOPRIS FRAM:                   
001300*       1. BASPRIS BESTÄMMES. DET VÄLJES BEROENDE VILKET                  
001400*          MARKNADSBOLAG DET ÄR                                           
001500*       2. NETTOPRIS BERÄKNAS ENLIGT FÖLJANDE PRIORITET.                  
001600*          2.1   NETTOPRIS = RAB/ART * BASPRIS                            
001700*          2.2   NETTOPRIS = RAB/TRANSFER * BASPRIS                       
001800*          2.3   NETTOPRIS = RAB/NORMALKOD * BASPRIS                      
001900*          2.4   NETTOPRIS = 2 * SJÄLVKOST                                
002000*                                                                         
002100*    FÖR ÖVRIGT KOMPLETTERAS ARTIKELN MED INFO. FRÅN ARTREG,              
002200*    KUNDREGISTER LÄSES,                                                  
002300*                                                                         
002400*    REMARKS:    ETRACKER NR 1424741                                      
002500*                                                                         
002600*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  JA                  PIC X(1) VALUE 'J'.                              
003600 77  YES                 PIC X(1) VALUE 'Y'.                              
003700 77  NEJ                 PIC X(1) VALUE 'N'.                              
003800                                                                          
003900 77  FORTSATT-SW         PIC X     VALUE 'N'.                             
004000     88  FORTSATT-OK               VALUE 'J'.                             
004100     88  FORTSATT-EJ-OK            VALUE 'N'.                             
004200                                                                          
004300 77  SJK-SW              PIC X     VALUE 'N'.                             
004400     88  SJK-JA                    VALUE 'J'.                             
004500     88  SJK-NEJ                   VALUE 'N'.                             
004600                                                                          
004700 77  BOUNCE-FLOW-SW      PIC X     VALUE 'N'.                             
004800     88  BOUNCE-FLOW               VALUE 'J'.                             
004900     88  NO-BOUNCE-FLOW            VALUE 'N'.                             
005000                                                                          
005100 77  PRICE-SW            PIC X     VALUE 'N'.                             
005200     88  PRICE-JA                  VALUE 'J'.                             
005300     88  PRICE-NEJ                 VALUE 'N'.                             
005400*                                                                         
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800                                                                          
005900 01  BASPRIS             PIC S9(7)V9(2)  COMP-3.                          
006000 01  PRARTNTO-BER        PIC S9(7)V9(2)  COMP-3.                          
006100 01  SPAR-KDORDKL-DOG    PIC S9(1) VALUE ZERO COMP-3.                     
006200 01  SPAR-IDMARKBO       PIC  X(1) VALUE SPACE.                           
006300 01  W-IDLANDX2          PIC  X(2) VALUE SPACE.                           
006400 01  W-WDB1-KDVALISO     PIC  X(3) VALUE SPACE.                           
006500 01  W-WDB6-KDVALISO     PIC  X(3) VALUE SPACE.                           
006600 01  W-BOUNCE-KDVALISO   PIC  X(3) VALUE SPACE.                           
006700 01  W-WDB2-IDFTG        PIC  9(2) VALUE ZERO.                            
006800 01  W-WDB2-KDKUNDKAT    PIC  9(2) VALUE ZERO.                            
006900 01  W-WDB6-IDFTG        PIC  9(2) VALUE ZERO.                            
007000 01  W-BOUNCE-IDDC       PIC  X(2) VALUE SPACE.                           
007100 01  W-IDDC1-US          PIC  X(1) VALUE '4'.                             
007200 01  W-IDDC1-CN          PIC  X(1) VALUE '7'.                             
007300 01  SPAR-KDARTRAB       PIC  9(2) VALUE ZERO.                            
007400 01  RAB-INDX            PIC  9(2) VALUE ZERO.                            
007500 01  WS-IDDISTR          PIC  9(5) VALUE ZERO.                            
007600 01  WS-IDKUNDNR         PIC  9(7) VALUE ZERO.                            
007700 01  WS-IDARTNR          PIC  9(9) VALUE ZERO.                            
007800 01  W-DATE-AAMM         PIC  9(4) VALUE ZERO.                            
007900 01  WS-KDVALISO-HUV     PIC  X(3) VALUE 'SEK'.                           
008000 01  WS-KDVALISO         PIC  X(3) VALUE SPACE.                           
008100 01  WS-DAPRLIST         PIC  9(8) VALUE ZERO.                            
008200 01  WS-PRARTBEL-PR      PIC  S9(8)V9(5) COMP-3 VALUE ZERO.               
008300 01  WS-PRIS-PRARTNTO    PIC  S9(7)V9(2) COMP-3 VALUE ZERO.               
008400*                                                                         
008500*01  -COPY WWPRODSL                                                       
008600                                                                          
008700     SKIP2                                                                
008800 01  DAGENS-DATUM             PIC 9(8)  VALUE ZERO.                       
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-SEKEL   PIC 9(2).                                   
009100     03  DAGENS-DATUM-AAR     PIC 9(2).                                   
009200     03  DAGENS-DATUM-MAANAD  PIC 9(2).                                   
009300     03  DAGENS-DATUM-DAG     PIC 9(2).                                   
009400     EJECT                                                                
009500****************************************************************          
009600*           BYTESARTIKELTEST-COPYTEXT                                     
009700****************************************************************          
009800*                                                                         
009900 01  FILLER         PIC X(16)    VALUE 'BYTESART-AREA   '.                
010000 01  TEST-IDARTNR   PIC 9(9)    COMP-3.                                   
010100*01  FILLER        -COPY WWBYT02    -RED TEST-IDARTNR.                    
010200     SKIP2                                                                
010300*          GODKÄNDA FÖRETAG                                               
010400*01  -COPY WWIDFTG                                                        
010500*          GODKÄNDA DC                                                    
010600*01  -COPY WWDC99                                                         
010700*          KONSTANTER LANDKODER                                           
010800*01  -COPY WWLNDKON                                                       
010900                                                                          
011000 01  TEST-IDDISTR                PIC S9(5)   COMP-3 VALUE ZERO.           
011100*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
011200     EJECT                                                                
011300*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
011400     EJECT                                                                
011500*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
011600     EJECT                                                                
011700*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
011800     EJECT                                                                
011900                                                                          
012000 01  FILLER                    PIC X(16) VALUE 'ARTREG-INFO'.             
012100****    ARBETSAREA FÖR ARTIKELREGISTERINFO.                               
012200                                                                          
012300 01  ARTIKELREG-AREA.                                                     
012400     03 ARTREG-ARTIKEL-FINNS      PIC X.                                  
012500     03 ARTREG-KDPRODSL           PIC S9(3)        COMP-3.                
012600     03 ARTREG-PRARTSJK           PIC S9(7)V9(2)   COMP-3.                
012700     03 ARTREG-PRARTSTD           PIC S9(7)V9(2)   COMP-3.                
012800     03 ARTREG-KVROS              PIC S9(7)        COMP-3.                
012900     03 ARTREG-KDSORT             PIC X(2).                               
013000                                                                          
013100****    ARBETSAREA FÖR KUNDREGISTER                                       
013200                                                                          
013300 01  KUNDREG-AREA.                                                        
013400     03 KUND-SEGMENT-FINNS     PIC X.                                     
013500                                                                          
013600     EJECT                                                                
013700 01  NYCKLAR-TILL-DLI.                                                    
013800*   NYCKLAR TILL ARTREG             ************                          
013900     03  W-IDARTNR-X.                                                     
014000         05  W-IDARTNR   PIC S9(9)           COMP-3.                      
014100                                                                          
014200*    NYCKLAR TILL KUNDREG             ***********                         
014300                                                                          
014400     03  W-IDGMT-MAX-X.                                                   
014500         05  W-IDDISTR-B2-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
014600         05  W-IDKUNDNR-B2-MAX   PIC S9(7)                                
014700                                      VALUE 9999999  COMP-3.              
014800                                                                          
014900     03  W-IDGMT-MIN-X.                                                   
015000         05  W-IDDISTR-B2-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
015100         05  W-IDKUNDNR-B2-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
015200                                                                          
015300*    NYCKLAR TILL WDK7                ***********                         
015400                                                                          
015500     03  W-IDARTNR-WDK701-X.                                              
015600         05  W-IDARTNR-WDK701    PIC S9(9)   VALUE ZERO COMP-3.           
015700     03  W-IDDC-WDK711-X.                                                 
015800         05  W-IDDC-WDK711       PIC X(2)    VALUE SPACE.                 
015900     03  W-IDLANDX2-WDK712-X.                                             
016000         05  W-IDLANDX2-WDK712   PIC X(2)    VALUE SPACE.                 
016100     03  W-IDDC1-X.                                                       
016200         05 W-IDDC1              PIC X(1)    VALUE SPACE.                 
016300     03  W-PRAVCOST-X.                                                    
016400         05 W-PRAVCOST          PIC S9(7)V9(2) VALUE ZERO COMP-3.         
016500                                                                          
016600                                                                          
016700*    NYCKLAR TILL WDB1              *************                         
016800     03  W-WDB1-WDB101KY-X.                                               
016900         05  W-WDB1-IDPARTNR PIC X(9)        VALUE SPACE.                 
017000         05  W-WDB1-IDFTG    PIC 9(2)        VALUE ZERO.                  
017100                                                                          
017200*    NYCKLAR TILL WDB6              *************                         
017300     03  W-WDB6-WDB601KY-X.                                               
017400         05  W-WDB6-IDDC     PIC X(2)        VALUE SPACE.                 
017500                                                                          
017600*    NYCKLAR TILL WDC2             *************                          
017700     03  W-IDPROMR-X.                                                     
017800         05  W-IDPROMR       PIC X(3)         VALUE SPACE.                
017900                                                                          
018000     03  W-WDC211KY-MIN-X.                                                
018100         05  W-IDARTNR-211-MIN  PIC S9(9)     VALUE ZERO COMP-3.          
018200         05  W-DASTADAT-211-MIN PIC 9(8)      VALUE ZERO.                 
018300                                                                          
018400     03  W-WDC211KY-MAX-X.                                                
018500         05  W-IDARTNR-211-MAX  PIC S9(9)     VALUE ZERO COMP-3.          
018600         05  W-DASTADAT-211-MAX PIC 9(8)      VALUE ZERO.                 
018700                                                                          
018800     03  W-WDC212KY-MIN-X.                                                
018900         05  W-KDARTKAM-212-MIN PIC 9(5)     VALUE ZERO.                  
019000         05  W-DASTADAT-212-MIN PIC 9(8)     VALUE ZERO.                  
019100                                                                          
019200     03  W-WDC212KY-MAX-X.                                                
019300         05  W-KDARTKAM-212-MAX PIC 9(5)     VALUE ZERO.                  
019400         05  W-DASTADAT-212-MAX PIC 9(8)     VALUE ZERO.                  
019500                                                                          
019600     03  W-DASTADAT-X.                                                    
019700         05  W-DASTADAT     PIC 9(8)         VALUE ZERO.                  
019800                                                                          
019900*   NYCKLAR TILL WDC1              *************                          
020000     03  W-WDC101KY-X.                                                    
020100         05  W-IDARTNR-WDC  PIC S9(9)     VALUE ZERO COMP-3.              
020200         05  W-IDMARKBO-WDC PIC  X(1)     VALUE SPACE.                    
020300                                                                          
020400*    NYCKLAR TILL WDB6 LÄSNING MED IDFTG + FLMAINDC                       
020500     03  W-IDFTG-X.                                                       
020600         05 W-IDFTG              PIC 9(2)     VALUE ZERO.                 
020700                                                                          
020800     03  W-FLMAINDC-X.                                                    
020900         05 W-FLMAINDC           PIC X(1)     VALUE SPACE.                
021000                                                                          
021100*    ---- ARBETSAREOR FÖR IMS-SECTIONERNA                                 
021200                                                                          
021300 01  FILLER                PIC X(16)  VALUE 'IMS-WS'.                     
021400     SKIP2                                                                
021500*01  -COPY W0003.                                                         
021600     EJECT                                                                
021700 01  GENERELLA-SUBPROGRAM.                                                
021800     03  CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                  
021900     03  FELLOG            PIC X(8)    VALUE 'FELLOG  '.                  
022000     03  W335COST          PIC X(8)    VALUE 'W335COST'.                  
022100     03  W335CURR          PIC X(8)    VALUE 'W335CURR'.                  
022200     03  W510CURR          PIC X(8)    VALUE 'W510CURR'.                  
022300                                                                          
022400*    --- PARAMETRAR TILL SUBPROGRAM                                       
022500                                                                          
022600*01  -COPY W335COST                                                       
022700     EJECT                                                                
022800                                                                          
022900*01  -COPY W335CURR                                                       
023000     EJECT                                                                
023100                                                                          
023200*01  -COPY W510CURR                                                       
023300     EJECT                                                                
023400                                                                          
023500*    ---- STATUSKOD FRÅN IMS                                              
023600 01  STATUS-WS             PIC XX.                                        
023700     88  SEGMENT-FINNS                 VALUE '  '.                        
023800     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
023900     SKIP2                                                                
024000 01  GODK-STATUSKODER.                                                    
024100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024200     SKIP2                                                                
024300 01  SSA1                  PIC X(64).                                     
024400 01  SSA2                  PIC X(64).                                     
024500 01  SSA3                  PIC X(64).                                     
024600     EJECT                                                                
024700 01  FILLER              PIC X(16) VALUE 'DLI-IO-AREA'.                   
024800     SKIP2                                                                
024900 01  DLI-IO-AREA.                                                         
025000     03  IO-AREA         PIC X(900).                                      
025100                                                                          
025200*    03  WLARTC01  -COPY WDK601 -PRE ARTC01- -RED IO-AREA                 
025300     EJECT                                                                
025400*    03  WLARTC11  -COPY WDK611 -PRE ARTC11- -RED IO-AREA                 
025500     EJECT                                                                
025600*    03  WLARTC21  -COPY WDK621 -PRE ARTC21- -RED IO-AREA                 
025700     EJECT                                                                
025800*    03  WLPRIA01 -COPY WDC101  -PRE PRIA-   -RED IO-AREA                 
025900     EJECT                                                                
026000**   BETALARREGISTER                                                      
026100*    03  WDB101 -COPY WDB101    -PRE WDB1-   -RED IO-AREA                 
026200 01  FILLER              PIC X(16) VALUE 'DLI-IO-AREA-2'.                 
026300     SKIP2                                                                
026400 01  DLI-IO-AREA-2.                                                       
026500     03  IO-AREA-2       PIC X(850).                                      
026600*    PRIS-RABATT REGISTER         *****************                       
026700*    03  WLPRIB01 -COPY WDC201  -PRE PRIB01- -RED IO-AREA-2               
026800     EJECT                                                                
026900*    03  WLPRIB11 -COPY WDC211  -PRE PRIB11- -RED IO-AREA-2               
027000     EJECT                                                                
027100*    03  WLPRIB12 -COPY WDC212  -PRE PRIB12- -RED IO-AREA-2               
027200     EJECT                                                                
027300*    03  WLPRIB13 -COPY WDC213  -PRE PRIB13- -RED IO-AREA-2               
027400     EJECT                                                                
027500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
027600 01  DLI-IO-WDB601.                                                       
027700*    03  -COPY WDB601                                                     
027800     EJECT                                                                
027900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
028000 01  DLI-IO-WDK701.                                                       
028100*    03  -COPY WDK701                                                     
028200     EJECT                                                                
028300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
028400 01  DLI-IO-WDK711.                                                       
028500*    03  -COPY WDK711                                                     
028600     EJECT                                                                
028700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK712'.           
028800 01  DLI-IO-WDK712.                                                       
028900*    03  -COPY WDK712                                                     
029000     EJECT                                                                
029100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK724'.           
029200 01  DLI-IO-WDK724.                                                       
029300*    03  -COPY WDK724                                                     
029400**   KUNDREGISTER                                                         
029500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
029600 01  DLI-IO-WDB201.                                                       
029700*    03  -COPY WDB201                                                     
029800     EJECT                                                                
029900     EJECT                                                                
030000 LINKAGE SECTION.                                                         
030100     SKIP2                                                                
030200 01  FILLER              PIC X(16) VALUE 'W335PRIS-AREA'.                 
030300 01  PRISTILL-AREA.                                                       
030400*    03  -COPY W335PRIS                                                   
030500     EJECT                                                                
030600*01  -COPY W0008 -PRE  ARTC-.                                             
030700     05  FILLER        PIC X.                                             
030800     EJECT                                                                
030900*01  -COPY W0008  -PRE WDK7-                                              
031000     05  FILLER        PIC X.                                             
031100     EJECT                                                                
031200                                                                          
031300*01  -COPY W0008 -PRE  GMTA-.                                             
031400     05  FILLER        PIC X.                                             
031500     EJECT                                                                
031600                                                                          
031700*01  -COPY W0008 -PRE  WDB1-.                                             
031800     05  FILLER        PIC X.                                             
031900     EJECT                                                                
032000                                                                          
032100*01  -COPY W0008 -PRE  PRIA-.                                             
032200     05  FILLER        PIC X.                                             
032300     EJECT                                                                
032400*01  -COPY W0008 -PRE  PRIB-.                                             
032500     05  FILLER        PIC X.                                             
032600     EJECT                                                                
032700 01  COST-WDK6-PCB               PIC X.                                   
032800 01  COST-WDK7-PCB               PIC X.                                   
032900 01  COST-WDF1-PCB               PIC X.                                   
033000*01  -COPY W0008 -PRE  COST-9305-.                                        
033100     05  FILLER        PIC X.                                             
033200 01  COST-WDK72-PCB              PIC X.                                   
033300*01  -COPY W0008 -PRE  COST-WDB6-.                                        
033400     05  FILLER        PIC X.                                             
033500     EJECT                                                                
033600 PROCEDURE DIVISION USING PRISTILL-AREA ARTC-PCB WDK7-PCB GMTA-PCB        
033700                          WDB1-PCB PRIA-PCB PRIB-PCB                      
033800                          COST-WDK6-PCB                                   
033900                          COST-WDK7-PCB                                   
034000                          COST-WDF1-PCB                                   
034100                          COST-9305-PCB                                   
034200                          COST-WDK72-PCB                                  
034300                          COST-WDB6-PCB.                                  
034400                                                                          
034500                                                                          
034600     PERFORM A-INIT                                                       
034700     MOVE PRIS-IDDISTR         TO TEST-IDDISTR                            
034800     MOVE PRIS-IDDC            TO WS-IDDC                                 
034900                                                                          
035000     IF PRIS-KDCALL = 1                                                   
035100       MOVE FUNCTION CURRENT-DATE(1:8) TO  DAGENS-DATUM                   
035200                                                                          
035300       MOVE DAGENS-DATUM(3:2)  TO W-DATE-AAMM(1:2)                        
035400       MOVE DAGENS-DATUM(5:2)  TO W-DATE-AAMM(3:2)                        
035500       MOVE DAGENS-DATUM TO W-DASTADAT                                    
035600                            W-DASTADAT-211-MAX                            
035700                            W-DASTADAT-212-MAX                            
035800                                                                          
035900       MOVE W-DATE-AAMM        TO CURR-TIAAMM                             
036000       MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                       
036100       MOVE 'M'                TO CURR-KDVALTYP                           
036200                                                                          
036300       PERFORM B-LAS-ARTIKELREGISTER                                      
036400       IF ARTREG-ARTIKEL-FINNS = JA                                       
036500         PERFORM C-LAS-KUNDREGISTER                                       
036600         IF KUND-SEGMENT-FINNS = JA                                       
036700           MOVE ARTREG-KDPRODSL  TO TEST-KDPRODSL                         
036800           IF (W-IDLANDX2 = 'AU' OR 'JP' OR 'US' OR 'CA' )                
036900           AND KDPRODSL-LOCAL                                             
037000              PERFORM E-PRIS-LIKA-SJALVKOST                               
037100           ELSE                                                           
037600              PERFORM D-PRISTILLEMPA-ARTIKEL                              
037800           END-IF                                                         
037900                                                                          
038000           IF SJK-JA AND DIST79-DEALER-PRICE                              
038100              PERFORM F-RAKNA-OM-TILL-VALUTA                              
038200           END-IF                                                         
038800                                                                          
038900           IF (W-WDB6-IDFTG = WC-IDFTG-PV AND NO-BOUNCE-FLOW)             
039000           OR DIST35-CDC-RETURNS-NON-VCC                                  
039100           OR W-WDB2-KDKUNDKAT = 3                                        
039200             CONTINUE                                                     
039300           ELSE                                                           
039400             PERFORM G-PRIS-LIKA-AVCOST                                   
039500           END-IF                                                         
039600                                                                          
039700         ELSE                                                             
039800           MOVE '2'          TO PRIS-KDSVAR                               
039900         END-IF                                                           
040000       ELSE                                                               
040100         MOVE '1'            TO PRIS-KDSVAR                               
040200       END-IF                                                             
040300                                                                          
040400       IF PRIS-KDSVAR = '1' OR '2' OR '3' OR '4'                          
040500          MOVE SPACE           TO PRIS-FLPRTILL                           
040600                                  PRIS-KDPRTYP                            
040700                                  PRIS-IDMARKBO                           
040800                                  PRIS-KDVALISO                           
040900          MOVE ZERO            TO PRIS-KDARTRAB                           
041000                                  PRIS-PRARTBTO-MARK                      
041100                                  PRIS-PRBPRIS                            
041200                                  PRIS-PRARTSJK                           
041300                                  PRIS-PRARTSTD                           
041400                                  PRIS-PRARTNTO                           
041500                                  PRIS-PRAVCOST                           
041600*****ERRORS OCCURRING BECAUSE OF DUBIOUS PRICE CALCULATION METHODS        
041700*****IN USA.*****20000508****** ALL NET PRICES = TO 0.00SEK ARE           
041800*****TO BE ROUNDED UP TO 1 KRONA                                          
041900       ELSE                                                               
042000         IF PRIS-PRARTNTO = 0                                             
042100            MOVE 1 TO PRIS-PRARTNTO                                       
042200         END-IF                                                           
042300       END-IF                                                             
042400*****************20000508***** END OF FLINES*******************           
042500*970519                                                                   
042600       IF  PRIS-IDPGM NOT = 'W9020100'                                    
042700       AND PRIS-IDPGM NOT = 'W3039300'                                    
042800       AND PRIS-IDPGM NOT = 'W5020300'                                    
042900       AND PRIS-IDPGM NOT = 'W3301000'                                    
043000       AND PRIS-IDPGM NOT = 'W3355800'                                    
043100       AND PRIS-IDPGM NOT = 'W5020500'                                    
043200                                                                          
043300         IF PRIS-PRARTNTO = 0                                             
043400            MOVE PRIS-IDDISTR TO WS-IDDISTR                               
043500            MOVE PRIS-IDKUNDNR TO WS-IDKUNDNR                             
043600            MOVE PRIS-IDARTNR TO WS-IDARTNR                               
043700            STRING 'EJ GODKÄNT PRIS ' WS-IDDISTR ' DIST '                 
043800                    WS-IDKUNDNR ' KUND ' WS-IDARTNR ' ARTIKEL '           
043900            DELIMITED BY SIZE INTO FELTEXT-STR                            
044000            CALL FELLOG                                                   
044100         END-IF                                                           
044200       END-IF                                                             
044300     ELSE                                                                 
044400******* PRIS-KDCALL = 2 *******************************                   
044500       PERFORM H-HAMTA-KDVALISO                                           
044600       IF KUND-SEGMENT-FINNS = NEJ                                        
044700         MOVE '2'              TO PRIS-KDSVAR                             
044800         MOVE SPACE            TO PRIS-KDVALISO                           
044900       ELSE                                                               
045000         IF ARTREG-ARTIKEL-FINNS = NEJ                                    
045100           MOVE '1'              TO PRIS-KDSVAR                           
045200           MOVE SPACE            TO PRIS-KDVALISO                         
045300         END-IF                                                           
045400       END-IF                                                             
045500     END-IF                                                               
045600                                                                          
045700     MOVE ZERO TO RETURN-CODE                                             
045800     GOBACK                                                               
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200 A-INIT SECTION.                                                          
046300***  OUTPUT FÄLT FRÅN W335PRIS ***                                        
046400     MOVE SPACE    TO PRIS-KDSVAR                                         
046500                      PRIS-FLPRTILL                                       
046600                      PRIS-KDPRTYP                                        
046700                      PRIS-IDMARKBO                                       
046800                      PRIS-KDVALISO                                       
046900     MOVE ZERO     TO PRIS-KDARTRAB                                       
047000                      PRIS-PRARTBTO-MARK                                  
047100                      PRIS-PRBPRIS                                        
047200                      PRIS-PRARTSJK                                       
047300                      PRIS-PRARTSTD                                       
047400                      PRIS-PRARTNTO                                       
047500                      PRIS-PRAVCOST                                       
047600***  WS-FÄLT ***                                                          
047700     MOVE NEJ      TO FORTSATT-SW                                         
047800                      SJK-SW                                              
047900                      BOUNCE-FLOW-SW                                      
048000                      PRICE-SW                                            
048100                                                                          
048200     MOVE +0       TO PRARTNTO-BER                                        
048300                      BASPRIS                                             
048400                      SPAR-KDORDKL-DOG                                    
048500                                                                          
048600     MOVE SPACE    TO W-BOUNCE-KDVALISO                                   
048700                      W-BOUNCE-IDDC                                       
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 B-LAS-ARTIKELREGISTER SECTION.                                           
049200     MOVE PRIS-IDARTNR             TO W-IDARTNR                           
049300                                                                          
049400     PERFORM IMS-GET-WLARTC01                                             
049500     IF SEGMENT-FINNS                                                     
049600       MOVE ARTC01-ART-KDPRODSL    TO ARTREG-KDPRODSL                     
049700       MOVE ARTC01-ART-KDSORT      TO ARTREG-KDSORT                       
049800       PERFORM IMS-GET-WLARTC11                                           
049900       IF SEGMENT-FINNS                                                   
050000         MOVE JA                   TO ARTREG-ARTIKEL-FINNS                
050100         MOVE ARTC11-CLAG-PRARTSJK TO ARTREG-PRARTSJK                     
050200                                      PRIS-PRARTSJK                       
050300         MOVE ARTC11-CLAG-PRARTSTD TO ARTREG-PRARTSTD                     
050400                                      PRIS-PRARTSTD                       
050500         MOVE ARTC11-CLAG-KVROS    TO ARTREG-KVROS                        
050600         PERFORM IMS-GNP-WLARTC21                                         
050700         PERFORM UNTIL SEGMENT-SAKNAS OR PRICE-JA                         
050800           IF SEGMENT-FINNS                                               
050900             COMPUTE WS-DAPRLIST = 999999999 -                            
051000                                   ARTC21-PRL-DAPRLIST-9KOMPL             
051100             IF WS-DAPRLIST <= W-DASTADAT                                 
051200               MOVE ARTC21-PRL-PRARTBEL-PR TO WS-PRARTBEL-PR              
051300               MOVE ARTC21-PRL-KDVALISO    TO WS-KDVALISO                 
051400               MOVE JA TO PRICE-SW                                        
051500             ELSE                                                         
051600               PERFORM IMS-GNP-WLARTC21                                   
051700             END-IF                                                       
051800           ELSE                                                           
051900             MOVE ZEROS TO WS-PRARTBEL-PR                                 
052000           END-IF                                                         
052100         END-PERFORM                                                      
052200       ELSE                                                               
052300         MOVE NEJ                  TO ARTREG-ARTIKEL-FINNS                
052400         MOVE ZERO                 TO ARTREG-PRARTSJK                     
052500                                      ARTREG-PRARTSTD                     
052600                                      ARTREG-KVROS                        
052700                                      PRIS-PRARTSJK                       
052800                                      PRIS-PRARTSTD                       
052900       END-IF                                                             
053000     ELSE                                                                 
053100       MOVE NEJ              TO ARTREG-ARTIKEL-FINNS                      
053200       PERFORM BA-NOLLSTALL-ARTREG-AREA                                   
053300     END-IF                                                               
053400     .                                                                    
053500                                                                          
053600 BA-NOLLSTALL-ARTREG-AREA SECTION.                                        
053700     MOVE SPACE                       TO ARTREG-KDSORT                    
053800     MOVE ZERO                        TO ARTREG-PRARTSTD                  
053900                                         ARTREG-KDPRODSL                  
054000                                         ARTREG-PRARTSJK                  
054100                                         ARTREG-KVROS                     
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500 C-LAS-KUNDREGISTER SECTION.                                              
054600     MOVE PRIS-IDDISTR               TO W-IDDISTR-B2-MAX                  
054700                                        W-IDDISTR-B2-MIN                  
054800     MOVE PRIS-IDKUNDNR              TO W-IDKUNDNR-B2-MIN                 
054900     PERFORM IMS-GET-WLGMTA01                                             
055000     IF SEGMENT-FINNS                                                     
055100        MOVE GMT-IDPARTNR            TO W-WDB1-IDPARTNR                   
055200        MOVE GMT-IDFTG               TO W-WDB2-IDFTG                      
055300        MOVE GMT-KDKUNDKAT           TO W-WDB2-KDKUNDKAT                  
055400        MOVE PRIS-IDDC               TO W-WDB6-IDDC                       
055500        PERFORM IMS-GET-WDB601                                            
055600        IF SEGMENT-FINNS                                                  
055700           MOVE DCS-IDFTG            TO W-WDB1-IDFTG                      
055800                                        W-WDB6-IDFTG                      
055900           MOVE DCS-KDVALISO         TO W-WDB6-KDVALISO                   
056000**FIX START************************************************               
056100****** TEMPORÄR LÖSNING FÖR USA OCH CANADA.                               
056200****** NÄR DOM GÅR ÖVER FRÅN LAB TILL SAP TAS DETTA BORT.                 
056300***********************************************************               
056400           IF W-WDB6-IDFTG = WC-IDFTG-US OR WC-IDFTG-CA                   
056500              IF W-WDB6-IDFTG = WC-IDFTG-CA                               
056600                 MOVE WC-IDFTG-PV TO W-WDB1-IDFTG                         
056700                                     W-WDB6-IDFTG                         
056800              ELSE                                                        
056900                IF DIST35-NONVCC-REFILL                                   
057000                OR (W-WDB2-IDFTG NOT = W-WDB6-IDFTG                       
057100                AND GMT-KDKUNDKAT = 04)                                   
057200********** HÄR KAN DET BEHÖVAS LÄGGAS TILL DISTRIKT     ***               
057300********** I GLOBAL EXPORT FAS 2 (VOR/IMPORTER  STUDS)  ***               
057400********** NÄR SÄNDANDE DC = US.                        ***               
057500                   CONTINUE                                               
057600                ELSE                                                      
057700                   MOVE WC-IDFTG-PV TO W-WDB1-IDFTG                       
057800                                       W-WDB6-IDFTG                       
057900                END-IF                                                    
058000              END-IF                                                      
058100           END-IF                                                         
058200**FIX SLUT*************************************************               
058300           IF W-WDB2-IDFTG NOT = W-WDB6-IDFTG                             
058400           AND NOT DCS-DDC                                                
058500           AND ARTREG-KDSORT NOT = 'SW'                                   
058600           AND NOT DIST35-CDC-RETURNS-NON-VCC                             
058700           AND NOT DIST18-SCRAP-NDC-QUAL                                  
058800*******FIX START - TESTA OM EJ GILTIGT STUDS DISTRIKT ****                
058900**** LÄS OM WDB2                                                          
059000              MOVE PRIS-IDDISTR        TO W-IDDISTR-B2-MAX                
059100                                          W-IDDISTR-B2-MIN                
059200              MOVE PRIS-IDKUNDNR       TO W-IDKUNDNR-B2-MIN               
059300**** REREAD WDB2                                                          
059400              PERFORM IMS-GET-WLGMTA01                                    
059500**** NO BOUNCE FLOWS SHOULD BE TESTED                                     
059600              IF NOT DIST35-NONVCC-NONVCC-REFILL                          
059700              AND NOT DIST35-NONVCC-NONVCC-TRANSFER                       
059710              AND NOT DIST07-NON-VCC-OWNED                                
059800              AND NOT GMT-KDKUNDKAT = 04                                  
059900                 MOVE PRIS-IDDISTR TO WS-IDDISTR                          
060000                 MOVE PRIS-IDKUNDNR TO WS-IDKUNDNR                        
060100                 MOVE PRIS-IDARTNR TO WS-IDARTNR                          
060200                 STRING 'FELAKTIG STUDS ' WS-IDDISTR ' DIST '             
060300                    WS-IDKUNDNR ' KUND ' WS-IDARTNR ' ARTIKEL '           
060400                 DELIMITED BY SIZE INTO FELTEXT-STR                       
060500                 CALL FELLOG                                              
060600              END-IF                                                      
060700*******FIX SLUT ******************************************                
060800              MOVE JA TO BOUNCE-FLOW-SW                                   
060900              MOVE WC-IDFTG-PV       TO W-WDB1-IDFTG                      
061000              IF W-WDB6-IDFTG = WC-IDFTG-PV                               
061100                 MOVE W-WDB2-IDFTG TO W-IDFTG                             
061200                 MOVE JA             TO W-FLMAINDC                        
061300                 PERFORM IMS-GU-WDB601-EXP                                
061400                 MOVE DCS-IDDC       TO W-BOUNCE-IDDC                     
061500                 MOVE DCS-KDVALISO TO W-BOUNCE-KDVALISO                   
061600              END-IF                                                      
061700           END-IF                                                         
061800           PERFORM IMS-GET-WDB101                                         
061900           IF SEGMENT-FINNS                                               
062000              MOVE WDB1-BET-IDLANDX2 TO W-IDLANDX2                        
062100              MOVE WDB1-BET-IDPROMR  TO W-IDPROMR                         
062200              MOVE WDB1-BET-IDMARKBO TO SPAR-IDMARKBO                     
062300              MOVE WDB1-BET-KDVALISO TO W-WDB1-KDVALISO                   
062400              MOVE JA                TO KUND-SEGMENT-FINNS                
062500           ELSE                                                           
062600              MOVE NEJ               TO KUND-SEGMENT-FINNS                
062700           END-IF                                                         
062800        ELSE                                                              
062900           MOVE NEJ                  TO KUND-SEGMENT-FINNS                
063000        END-IF                                                            
063100     ELSE                                                                 
063200         MOVE NEJ                    TO KUND-SEGMENT-FINNS                
063300     END-IF                                                               
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700 D-PRISTILLEMPA-ARTIKEL SECTION.                                          
063800*                        *** FÖLJANDE BASPRIS FINNES:        ****         
063900*                        *** 1.  SUGGESTED-RETAIL            ****         
064000*                        *** 2.  2 * SJÄLVKOSTPRIS           ****         
064100*                        ***     (GÄLLER DÅ ÄVEN SOM NETTOPRIS **         
064200                                                                          
064300     PERFORM DA-BERAKNA-PRIS-RABATT                                       
064400     MOVE BASPRIS                TO PRIS-PRBPRIS                          
064500                                                                          
064600     IF PRIS-FLINVEST = JA                                                
064700       MOVE ARTREG-KDPRODSL      TO TEST-KDPRODSL                         
064800       IF KDPRODSL-VOLVO-BYTES                                            
064900         COMPUTE PRARTNTO-BER ROUNDED = PRARTNTO-BER * 1.33               
065000       END-IF                                                             
065100     END-IF                                                               
065200*****************************************************************         
065300     IF PRIS-KDORDKL >  SPAR-KDORDKL-DOG OR  ARTREG-KVROS > +0            
065400       MOVE NEJ TO PRIS-FLPRTILL                                          
065500     ELSE                                                                 
065600       MOVE JA  TO PRIS-FLPRTILL                                          
065700     END-IF                                                               
065800                                                                          
065900                                                                          
066000     MOVE PRARTNTO-BER           TO PRIS-PRARTNTO                         
066100     IF DIST79-DEALER-PRICE                                               
066200       MOVE W-WDB1-KDVALISO    TO PRIS-KDVALISO                           
066300                                  CURR-KDVALISO-ROW                       
066400     ELSE                                                                 
066500       MOVE 'SEK'              TO PRIS-KDVALISO                           
066600                                  CURR-KDVALISO-ROW                       
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000                                                                          
067100 DA-BERAKNA-PRIS-RABATT SECTION.                                          
067200                                                                          
067300     MOVE PRIS-IDARTNR  TO W-IDARTNR-211-MIN                              
067400                           W-IDARTNR-211-MAX                              
067500                           W-IDARTNR-WDC                                  
067600     MOVE SPAR-IDMARKBO TO W-IDMARKBO-WDC                                 
067700                                                                          
067800     PERFORM DAA-LAES-PRISREG                                             
067900                                                                          
068000     IF FORTSATT-OK                                                       
068100********   PRIS-RABATT REGISTER    PRISOMRÅDESINFO ***************        
068200         PERFORM IMS-GET-WLPRIB01                                         
068300         IF SEGMENT-FINNS                                                 
068400           MOVE PRIB01-PRO-KDORDKL-DOG TO SPAR-KDORDKL-DOG                
068500                                                                          
068600********   ARTIKELRABATTINFORMATION ******************************        
068700           PERFORM IMS-GET-WLPRIB11                                       
068800           IF SEGMENT-FINNS                                               
068900             IF  ( PRIS-KDORDKL <= SPAR-KDORDKL-DOG )                     
069000             AND ( ARTREG-KVROS = ZERO )                                  
069100               COMPUTE PRARTNTO-BER ROUNDED =                             
069200               (( 100 - PRIB11-ART-REARTRAB-DO )                          
069300               * PRIA-ART-PRARTBTO-MARK ) / 100                           
069400             ELSE                                                         
069500               COMPUTE PRARTNTO-BER ROUNDED =                             
069600               (( 100 - PRIB11-ART-REARTRAB-BULK )                        
069700               * PRIA-ART-PRARTBTO-MARK ) / 100                           
069800             END-IF                                                       
069900             MOVE 'S'          TO PRIS-KDPRTYP                            
070000           ELSE                                                           
070100                                                                          
070200********** KAMPANJRABATTINFORMATION ******************************        
070300             PERFORM IMS-GET-WLPRIB12                                     
070400             IF SEGMENT-FINNS                                             
070500               IF  ( PRIS-KDORDKL <= SPAR-KDORDKL-DOG )                   
070600               AND ( ARTREG-KVROS = ZERO )                                
070700                 COMPUTE PRARTNTO-BER ROUNDED =                           
070800                 (( 100 - PRIB12-KAM-REARTRAB-DO )                        
070900                 * PRIA-ART-PRARTBTO-MARK ) / 100                         
071000               ELSE                                                       
071100                 COMPUTE PRARTNTO-BER ROUNDED =                           
071200                 (( 100 - PRIB12-KAM-REARTRAB-BULK )                      
071300                 * PRIA-ART-PRARTBTO-MARK ) / 100                         
071400               END-IF                                                     
071500               MOVE 'F'        TO PRIS-KDPRTYP                            
071600             ELSE                                                         
071700                                                                          
071800************     RABATTINFORMATION *******************************        
071900               IF SPAR-KDARTRAB > ZERO                                    
072000                 PERFORM IMS-GET-WLPRIB13                                 
072100                 IF SEGMENT-FINNS                                         
072200                   MOVE SPAR-KDARTRAB TO RAB-INDX                         
072300                   IF  ( PRIS-KDORDKL <= SPAR-KDORDKL-DOG )               
072400                   AND ( ARTREG-KVROS = ZERO )                            
072500                     COMPUTE PRARTNTO-BER ROUNDED =                       
072600                     (( 100 - PRIB13-RAB-REARTRAB-DO (RAB-INDX))          
072700                     * PRIA-ART-PRARTBTO-MARK ) / 100                     
072800                   ELSE                                                   
072900                     COMPUTE PRARTNTO-BER ROUNDED =                       
073000                    (( 100 - PRIB13-RAB-REARTRAB-BULK(RAB-INDX))          
073100                     * PRIA-ART-PRARTBTO-MARK ) / 100                     
073200                   END-IF                                                 
073300                   MOVE 'O'        TO PRIS-KDPRTYP                        
073400                 END-IF                                                   
073500               ELSE                                                       
073600                 MOVE PRIA-ART-PRARTBTO-MARK TO PRARTNTO-BER              
073700               END-IF                                                     
073800             END-IF                                                       
073900           END-IF                                                         
074000         ELSE                                                             
074100           COMPUTE PRARTNTO-BER = 2 * ARTREG-PRARTSJK                     
074200           MOVE 'T'        TO PRIS-KDPRTYP                                
074300           MOVE JA         TO SJK-SW                                      
074400**********************************************************                
074500**** VARNING DENNA ARTIKEL SAKNAR SJÄLVKOST **************                
074600**********************************************************                
074700           IF ARTREG-PRARTSJK = 0                                         
074800              MOVE '3'                 TO PRIS-KDSVAR                     
074900           END-IF                                                         
075000         END-IF                                                           
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 DAA-LAES-PRISREG SECTION.                                                
075600     SKIP2                                                                
075700********** LÄSER ARTIKEL PRIS-INFO *******************************        
075800                                                                          
075900     PERFORM IMS-GET-WLPRIA01                                             
076000     IF SEGMENT-FINNS                                                     
076100       MOVE JA TO FORTSATT-SW                                             
076200                                                                          
076300       MOVE PRIA-ART-KDARTKAM       TO W-KDARTKAM-212-MIN                 
076400                                       W-KDARTKAM-212-MAX                 
076500       IF WDB1-BET-FLARTRAB = 'J'                                         
076600         MOVE PRIA-ART-KDARTRAB-ALT   TO SPAR-KDARTRAB                    
076700                                         PRIS-KDARTRAB                    
076800       ELSE                                                               
076900         MOVE PRIA-ART-KDARTRAB       TO SPAR-KDARTRAB                    
077000                                         PRIS-KDARTRAB                    
077100       END-IF                                                             
077200       MOVE PRIA-ART-PRARTBTO-MARK TO BASPRIS                             
077300                                       PRIS-PRARTBTO-MARK                 
077400       MOVE PRIA-ART-IDMARKBO       TO PRIS-IDMARKBO                      
077500                                                                          
077600     ELSE                                                                 
077700       MOVE NEJ TO FORTSATT-SW                                            
077800                                                                          
077900************** OM ARTIKEL SAKNAS PÅ PRISREG. WDC1 ****************        
078000       MOVE ARTREG-KDPRODSL      TO TEST-KDPRODSL                         
078100       IF KDPRODSL-VOLVO-EMB                                              
078200                                                                          
078300********* EMBALLAGEPRISER BERÄKNAS EJ I AMC **********************        
078400         COMPUTE PRARTNTO-BER ROUNDED = 1.15 * ARTREG-PRARTSJK            
078500         MOVE JA         TO SJK-SW                                        
078600       ELSE                                                               
078700         COMPUTE PRARTNTO-BER ROUNDED = 2 * ARTREG-PRARTSJK               
078800         MOVE 'T'        TO PRIS-KDPRTYP                                  
078900         MOVE JA         TO SJK-SW                                        
079000       END-IF                                                             
079100**********************************************************                
079200**** VARNING DENNA ARTIKEL SAKNAR SJÄLVKOST **************                
079300**********************************************************                
079400       IF ARTREG-PRARTSJK = 0                                             
079500          MOVE '3'                 TO PRIS-KDSVAR                         
079600       END-IF                                                             
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000                                                                          
080100 E-PRIS-LIKA-SJALVKOST SECTION.                                           
080200**********************************************************                
080300**** VARNING DENNA ARTIKEL SAKNAR SJÄLVKOST **************                
080400**** DETTA GÄLLER ENDAST JAPAN OCH AUSTRALIEN*************                
080500**** FRÅN 061120 GÄLLER DETTA ÄVEN FÖR USA OCH ***********                
080600**** CANADA                                    ***********                
080700**** DET ÄR EN LOKAL ARTIKEL                   ***********                
080800**********************************************************                
080900**** 030627/EÖ                                                            
081000**** RÄKNAR UT SJÄLVKOST MED MÅNADSKURS MHA SUBPGM W335COST               
081100**** ARTIKEL MED LOKAL LEVERANTÖR                                         
081200**** FINNS DET INGEN PRISRAD PÅ ARTIKELN SÅ TAS SOM TIDIGARE              
081300**** WDK611 SJÄLVKOST.                                                    
081400     IF ARTREG-PRARTSJK > 0                                               
081500        MOVE PRIS-IDDC           TO COST-IDDC                             
081600        MOVE PRIS-IDARTNR        TO COST-IDARTNR                          
081700        MOVE DAGENS-DATUM-MAANAD TO  COST-TIMM                            
081800                                                                          
081900        CALL W335COST USING COST-W335COST COST-WDK6-PCB                   
082000                                          COST-WDK7-PCB                   
082100                                          COST-WDF1-PCB                   
082200                                          COST-9305-PCB                   
082300                                          COST-WDK72-PCB                  
082400                                          COST-WDB6-PCB                   
082500                                                                          
082600        IF COST-PRARTSJK-MONLOC > 0                                       
082700          MOVE COST-PRARTSJK-MONLOC TO PRARTNTO-BER                       
082800          MOVE JA                   TO SJK-SW                             
082900        ELSE                                                              
083000          IF COST-PRARTSJK-MON > 0                                        
083100            MOVE COST-PRARTSJK-MON TO PRARTNTO-BER                        
083200            MOVE JA                TO SJK-SW                              
083300          ELSE                                                            
083400            MOVE ARTREG-PRARTSJK   TO PRARTNTO-BER                        
083500            MOVE JA                TO SJK-SW                              
083600          END-IF                                                          
083700        END-IF                                                            
083800                                                                          
083900        IF ARTREG-KVROS > +0                                              
084000           MOVE NEJ TO PRIS-FLPRTILL                                      
084100        ELSE                                                              
084200           MOVE JA  TO PRIS-FLPRTILL                                      
084300        END-IF                                                            
084400        MOVE PRARTNTO-BER        TO PRIS-PRARTNTO                         
084500                                    BASPRIS                               
084600        MOVE BASPRIS             TO PRIS-PRBPRIS                          
084700        IF DIST79-DEALER-PRICE                                            
084800          MOVE W-WDB1-KDVALISO   TO PRIS-KDVALISO                         
084900                                    CURR-KDVALISO-ROW                     
085000        ELSE                                                              
085100          MOVE 'SEK'             TO PRIS-KDVALISO                         
085200                                    CURR-KDVALISO-ROW                     
085300        END-IF                                                            
085400     ELSE                                                                 
085500        MOVE '4'                  TO PRIS-KDSVAR                          
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900                                                                          
086000 F-RAKNA-OM-TILL-VALUTA  SECTION.                                         
086100                                                                          
086200     IF CURR-KDVALISO-ROW NOT = 'SEK'                                     
086300       CALL W510CURR USING CURR-W510CURR COST-9305-PCB                    
086400       IF CURR-KDSVAR = ' '                                               
086500       AND CURR-PRKURS-NEW NOT = 1.0                                      
086600         MOVE ZERO                      TO CURR-SUORDV-IN                 
086700                                           CURR-PRARTVNA-IN               
086800                                           CURR-PRARTSTD-IN               
086900                                           CURR-PRKURS-02                 
087000         MOVE SPACE                     TO CURR-KDVALISO-02               
087100         MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                    
087200         MOVE PRIS-PRARTNTO             TO CURR-PRARTSJK-IN               
087300         MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01               
087400         MOVE +2                        TO CURR-KDCALL                    
087500         CALL W335CURR            USING CURR-W335CURR                     
087600         MOVE CURR-PRARTSJK-UT          TO PRIS-PRARTNTO                  
087700       END-IF                                                             
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100                                                                          
088200 G-PRIS-LIKA-AVCOST SECTION.                                              
088300     IF W-WDB6-IDFTG = WC-IDFTG-PV AND BOUNCE-FLOW                        
088400        MOVE W-BOUNCE-IDDC     TO W-IDDC-WDK711                           
088500        MOVE W-BOUNCE-KDVALISO TO PRIS-KDVALISO                           
088600                                  CURR-KDVALISO-ROW                       
088700     ELSE                                                                 
088800        MOVE PRIS-IDDC         TO W-IDDC-WDK711                           
088900        MOVE W-WDB6-KDVALISO   TO PRIS-KDVALISO                           
089000                                  CURR-KDVALISO-ROW                       
089100     END-IF                                                               
089200     MOVE PRIS-IDARTNR TO W-IDARTNR-WDK701                                
089300     PERFORM IMS-GU-WDK701                                                
089400     IF SEGMENT-FINNS                                                     
089500       PERFORM IMS-GNP-WDK711                                             
089600       IF SEGMENT-FINNS                                                   
089700         IF SLAG-PRAVCOST > ZERO                                          
089800           MOVE SLAG-PRAVCOST TO PRIS-PRAVCOST                            
089900         ELSE                                                             
090000           PERFORM GA-AVCOST-SAKNAS                                       
090100         END-IF                                                           
090200       ELSE                                                               
090300         IF PRARTNTO-BER = ZERO                                           
090400           COMPUTE PRARTNTO-BER = 2 * ARTREG-PRARTSJK                     
090500         END-IF                                                           
090600         PERFORM GAB-RAKNA-OM-PRARTSJK                                    
090700       END-IF                                                             
090800     ELSE                                                                 
090900       IF PRARTNTO-BER = ZERO                                             
091000         COMPUTE PRARTNTO-BER = 2 * ARTREG-PRARTSJK                       
091100       END-IF                                                             
091200       PERFORM GAB-RAKNA-OM-PRARTSJK                                      
091300     END-IF                                                               
091400     .                                                                    
091500     EJECT                                                                
091600                                                                          
091700 GA-AVCOST-SAKNAS SECTION.                                                
091800     MOVE W-IDDC-WDK711 TO WS-IDDC                                        
091900                           W-WDB6-IDDC                                    
092000     IF NDC-US OR XDC-NON-VCC-OWNED                                       
092100       PERFORM IMS-GET-WDB601                                             
092200       MOVE DCS-IDLANDX2  TO W-IDLANDX2-WDK712                            
092300***** PRIO 1 - HÄMTA MATRIALPRIS ***********                              
092400       PERFORM IMS-GNP-WDK712                                             
092500       IF SEGMENT-FINNS                                                   
092600         IF LART-PRMATRL > ZERO                                           
092700            PERFORM GAA-RAKNA-OM-MATRLPR                                  
092800         END-IF                                                           
092900       END-IF                                                             
093000                                                                          
093100***** ONLY US AND CN THAT HAS VALUES ON WDK724                            
093200       IF NDC-US OR NDC-CN                                                
093300         IF PRIS-PRAVCOST = ZERO                                          
093400*****   PRIO 2 - HÄMTA SENASTE INLEV RAD *****                            
093500           PERFORM IMS-GNP-WDK724                                         
093600           IF SEGMENT-FINNS                                               
093700             IF SPRL-PRARTBES-PR > ZERO                                   
093800                MOVE SPRL-PRARTBES-PR TO PRIS-PRAVCOST                    
093900             END-IF                                                       
094000           END-IF                                                         
094100         END-IF                                                           
094200                                                                          
094300***** ONLY US AND CN THAT HAS MORE THAN ONE DC                            
094400         IF PRIS-PRAVCOST = ZERO                                          
094500*****   PRIO 3 - KOLLA PÅ ÖVRIGA KINA/USA DC *                            
094600*****            OM AVERAGE COST FINNS.    *                              
094700           IF NDC-CN                                                      
094800              MOVE W-IDDC1-CN TO W-IDDC1                                  
094900           ELSE                                                           
095000             IF NDC-US                                                    
095100               MOVE W-IDDC1-US TO W-IDDC1                                 
095200             END-IF                                                       
095300           END-IF                                                         
095400           PERFORM IMS-GNP-WDK711-FIRST                                   
095500           IF SEGMENT-FINNS                                               
095600             IF SLAG-PRAVCOST > ZERO                                      
095700                MOVE SLAG-PRAVCOST TO PRIS-PRAVCOST                       
095800             END-IF                                                       
095900           END-IF                                                         
096000         END-IF                                                           
096100       END-IF                                                             
096200                                                                          
096300       IF PRIS-PRAVCOST = ZERO                                            
096400***** PRIO 4 - ANVÄND TRANSFERPRISET OMRÄKNAT TILL LOKAL VALUTA           
096500         IF PRARTNTO-BER = ZERO                                           
096600           COMPUTE PRARTNTO-BER = 2 * ARTREG-PRARTSJK                     
096700         END-IF                                                           
096800         PERFORM GAB-RAKNA-OM-PRARTSJK                                    
096900       END-IF                                                             
097000     ELSE                                                                 
097100       IF PRARTNTO-BER = ZERO                                             
097200         COMPUTE PRARTNTO-BER = 2 * ARTREG-PRARTSJK                       
097300       END-IF                                                             
097400       PERFORM GAB-RAKNA-OM-PRARTSJK                                      
097500     END-IF                                                               
097600     .                                                                    
097700     EJECT                                                                
097800                                                                          
097900 GAA-RAKNA-OM-MATRLPR  SECTION.                                           
098000                                                                          
098100     IF CURR-KDVALISO-ROW NOT = 'SEK'                                     
098200       CALL W510CURR USING CURR-W510CURR COST-9305-PCB                    
098300       IF CURR-KDSVAR = ' '                                               
098400       AND CURR-PRKURS-NEW NOT = 1.0                                      
098500         MOVE ZERO                      TO CURR-SUORDV-IN                 
098600                                           CURR-PRARTSJK-IN               
098700                                           CURR-PRARTSTD-IN               
098800                                           CURR-PRKURS-02                 
098900         MOVE SPACE                     TO CURR-KDVALISO-02               
099000         MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                    
099100         MOVE LART-PRMATRL              TO CURR-PRARTVNA-IN               
099200         MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01               
099300         MOVE +2                        TO CURR-KDCALL                    
099400         CALL W335CURR            USING CURR-W335CURR                     
099500         MOVE CURR-PRARTVNA-UT          TO PRIS-PRAVCOST                  
099600       END-IF                                                             
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 GAB-RAKNA-OM-PRARTSJK  SECTION.                                          
100200     IF CURR-KDVALISO-ROW NOT = 'SEK'                                     
100300       CALL W510CURR USING CURR-W510CURR COST-9305-PCB                    
100400       IF CURR-KDSVAR = ' '                                               
100500       AND CURR-PRKURS-NEW NOT = 1.0                                      
100600         MOVE ZERO                      TO CURR-SUORDV-IN                 
100700                                           CURR-PRARTSJK-IN               
100800                                           CURR-PRARTSTD-IN               
100900                                           CURR-PRKURS-02                 
101000         MOVE SPACE                     TO CURR-KDVALISO-02               
101100         MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                    
101200         MOVE PRARTNTO-BER              TO CURR-PRARTVNA-IN               
101300         MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01               
101400         MOVE +2                        TO CURR-KDCALL                    
101500         CALL W335CURR            USING CURR-W335CURR                     
101600         MOVE CURR-PRARTVNA-UT          TO PRIS-PRAVCOST                  
101700       END-IF                                                             
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100                                                                          
102200 H-HAMTA-KDVALISO SECTION.                                                
102300     MOVE 'SEK' TO PRIS-KDVALISO                                          
102400                                                                          
102500     MOVE PRIS-IDDISTR               TO W-IDDISTR-B2-MAX                  
102600                                        W-IDDISTR-B2-MIN                  
102700     MOVE PRIS-IDKUNDNR              TO W-IDKUNDNR-B2-MIN                 
102800     MOVE PRIS-IDDC                  TO W-WDB6-IDDC                       
102900*******  DNI DISTRIKT - HÄMTA VALUTAN FRÅN WDB1 **************            
103000*******  LYNK - RECEIVES CURRENCY FROM WDB1     **************            
103100     PERFORM IMS-GET-WLGMTA01                                             
103200     IF SEGMENT-FINNS                                                     
103300       MOVE GMT-IDPARTNR           TO W-WDB1-IDPARTNR                     
103400       MOVE GMT-KDKUNDKAT          TO W-WDB2-KDKUNDKAT                    
103500       IF DIST79-DEALER-PRICE                                             
103600       OR W-WDB2-KDKUNDKAT = 3                                            
103700         PERFORM IMS-GET-WDB601                                           
103800         IF SEGMENT-FINNS                                                 
103900           MOVE DCS-IDFTG            TO W-WDB1-IDFTG                      
104000           PERFORM IMS-GET-WDB101                                         
104100           IF SEGMENT-FINNS                                               
104200              MOVE WDB1-BET-KDVALISO TO PRIS-KDVALISO                     
104300              MOVE JA                TO KUND-SEGMENT-FINNS                
104400           ELSE                                                           
104500              MOVE NEJ               TO KUND-SEGMENT-FINNS                
104600           END-IF                                                         
104700         ELSE                                                             
104800           MOVE NEJ                  TO KUND-SEGMENT-FINNS                
104900         END-IF                                                           
105000       ELSE                                                               
105100*******  KOLLA OM AVERAGE COST - HÄMTA VALUTAN FRÅN WDB6 *****            
105200         PERFORM HA-HAMTA-AVCOST-VALUTA                                   
105300       END-IF                                                             
105400     ELSE                                                                 
105500        MOVE NEJ                  TO KUND-SEGMENT-FINNS                   
105600     END-IF                                                               
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000 HA-HAMTA-AVCOST-VALUTA SECTION.                                          
106100     PERFORM IMS-GET-WLGMTA01                                             
106200     IF SEGMENT-FINNS                                                     
106300        MOVE GMT-IDFTG               TO W-WDB2-IDFTG                      
106400        MOVE PRIS-IDDC               TO W-WDB6-IDDC                       
106500        PERFORM IMS-GET-WDB601                                            
106600        IF SEGMENT-FINNS                                                  
106700           MOVE JA                   TO KUND-SEGMENT-FINNS                
106800           MOVE DCS-IDFTG            TO W-WDB6-IDFTG                      
106900           MOVE DCS-KDVALISO         TO W-WDB6-KDVALISO                   
107000           PERFORM HAA-LAS-ARTIKELREGISTER                                
107100           IF ARTREG-ARTIKEL-FINNS = JA                                   
107200**FIX START************************************************               
107300****** TEMPORÄR LÖSNING FÖR USA OCH CANADA.                               
107400****** NÄR DOM GÅR ÖVER FRÅN LAB TILL SAP TAS DETTA BORT.                 
107500***********************************************************               
107600             IF W-WDB6-IDFTG = WC-IDFTG-US OR WC-IDFTG-CA                 
107700                IF W-WDB6-IDFTG = WC-IDFTG-CA                             
107800                   MOVE WC-IDFTG-PV TO W-WDB6-IDFTG                       
107900                ELSE                                                      
108000                  IF DIST35-NONVCC-REFILL                                 
108100                  OR (W-WDB2-IDFTG NOT = W-WDB6-IDFTG                     
108200                  AND GMT-KDKUNDKAT = 04)                                 
108300********** HÄR KAN DET BEHÖVAS LÄGGAS TILL DISTRIKT     ***               
108400********** I GLOBAL EXPORT FAS 2 (VOR/IMPORTER  STUDS)  ***               
108500********** NÄR SÄNDANDE DC = US.                        ***               
108600                     CONTINUE                                             
108700                  ELSE                                                    
108800                     MOVE WC-IDFTG-PV TO W-WDB6-IDFTG                     
108900                  END-IF                                                  
109000                END-IF                                                    
109100             END-IF                                                       
109200**FIX SLUT*************************************************               
109300             IF  W-WDB2-IDFTG NOT = W-WDB6-IDFTG                          
109400             AND NOT DCS-DDC                                              
109500             AND ARTREG-KDSORT NOT = 'SW'                                 
109600             AND NOT DIST35-CDC-RETURNS-NON-VCC                           
109700             AND NOT DIST18-SCRAP-NDC-QUAL                                
109800**FIX START - TESTA OM EJ GILTIGT STUDS DISTRIKT **********               
109900**** LÄS OM WDB2                                                          
110000                MOVE PRIS-IDDISTR        TO W-IDDISTR-B2-MAX              
110100                                            W-IDDISTR-B2-MIN              
110200                MOVE PRIS-IDKUNDNR       TO W-IDKUNDNR-B2-MIN             
110300**** REREAD WDB2                                                          
110400                PERFORM IMS-GET-WLGMTA01                                  
110500**** NO BOUNCE FLOWS SHOULD BE TESTED                                     
110600                IF NOT DIST35-NONVCC-NONVCC-REFILL                        
110610                AND NOT DIST35-NONVCC-NONVCC-TRANSFER                     
110700                AND NOT DIST07-NON-VCC-OWNED                              
110800                AND NOT GMT-KDKUNDKAT = 04                                
110900                    MOVE PRIS-IDDISTR TO WS-IDDISTR                       
111000                    MOVE PRIS-IDKUNDNR TO WS-IDKUNDNR                     
111100                    MOVE PRIS-IDARTNR TO WS-IDARTNR                       
111200                    STRING 'FELAKTIG STUDS ' WS-IDDISTR ' DIST '          
111300                    WS-IDKUNDNR ' KUND ' WS-IDARTNR ' ARTIKEL '           
111400                    DELIMITED BY SIZE INTO FELTEXT-STR                    
111500                    CALL FELLOG                                           
111600                END-IF                                                    
111700**FIX SLUT ************************************************               
111800                MOVE JA TO BOUNCE-FLOW-SW                                 
111900                IF W-WDB6-IDFTG = WC-IDFTG-PV                             
112000                   MOVE W-WDB2-IDFTG TO W-IDFTG                           
112100                   MOVE JA             TO W-FLMAINDC                      
112200                   PERFORM IMS-GU-WDB601-EXP                              
112300                   MOVE DCS-KDVALISO TO W-BOUNCE-KDVALISO                 
112400                END-IF                                                    
112500             END-IF                                                       
112600             IF (W-WDB6-IDFTG = WC-IDFTG-PV AND NO-BOUNCE-FLOW)           
112700             OR DIST35-CDC-RETURNS-NON-VCC                                
112800                CONTINUE                                                  
112900             ELSE                                                         
113000               IF W-WDB6-IDFTG = WC-IDFTG-PV AND BOUNCE-FLOW              
113100                  MOVE W-BOUNCE-KDVALISO TO PRIS-KDVALISO                 
113200               ELSE                                                       
113300                  MOVE W-WDB6-KDVALISO   TO PRIS-KDVALISO                 
113400               END-IF                                                     
113500             END-IF                                                       
113600           END-IF                                                         
113700        ELSE                                                              
113800           MOVE NEJ                  TO KUND-SEGMENT-FINNS                
113900        END-IF                                                            
114000     ELSE                                                                 
114100         MOVE NEJ                    TO KUND-SEGMENT-FINNS                
114200     END-IF                                                               
114300     .                                                                    
114400     EJECT                                                                
114500                                                                          
114600 HAA-LAS-ARTIKELREGISTER SECTION.                                         
114700     MOVE ZERO                     TO ARTREG-PRARTSTD                     
114800                                      ARTREG-KDPRODSL                     
114900                                      ARTREG-PRARTSJK                     
115000                                      ARTREG-KVROS                        
115100     MOVE PRIS-IDARTNR             TO W-IDARTNR                           
115200                                                                          
115300     PERFORM IMS-GET-WLARTC01                                             
115400     IF SEGMENT-FINNS                                                     
115500       MOVE ARTC01-ART-KDSORT      TO ARTREG-KDSORT                       
115600       PERFORM IMS-GET-WLARTC11                                           
115700       IF SEGMENT-FINNS                                                   
115800         MOVE JA                   TO ARTREG-ARTIKEL-FINNS                
115900       ELSE                                                               
116000         MOVE NEJ                  TO ARTREG-ARTIKEL-FINNS                
116100       END-IF                                                             
116200     ELSE                                                                 
116300       MOVE NEJ                    TO ARTREG-ARTIKEL-FINNS                
116400       MOVE SPACE                  TO ARTREG-KDSORT                       
116500     END-IF                                                               
116600     .                                                                    
116700     EJECT                                                                
116800                                                                          
117800* IMS-SECTIONER                                                           
117900                                                                          
118000 IMS-GET-WLARTC01 SECTION.                                                
118100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
118200             DELIMITED BY SIZE INTO SSA1                                  
118300     MOVE '  GE' TO GODK-STATUSKODER                                      
118400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
118500     MOVE ARTC-STATUS-CODE  TO STATUS-WS                                  
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 IMS-GET-WLARTC11  SECTION.                                               
119100     MOVE 'WLARTC11 ' TO SSA1                                             
119200     MOVE '  GE' TO GODK-STATUSKODER                                      
119300     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA SSA1                         
119400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
119500     PERFORM IMS-STATUSKONTROLL                                           
119600     .                                                                    
119700     SKIP2                                                                
119800                                                                          
119900 IMS-GNP-WLARTC21 SECTION.                                                
120000                                                                          
120100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
120200     MOVE 'WLARTC21 ' TO SSA2                                             
120300     MOVE '  GE' TO GODK-STATUSKODER                                      
120400     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA SSA1 SSA2                    
120500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
120600     PERFORM IMS-STATUSKONTROLL                                           
120700     .                                                                    
120800     SKIP3                                                                
120900                                                                          
121000 IMS-GU-WDK701 SECTION.                                                   
121100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-WDK701-X ')'                  
121200     DELIMITED BY SIZE INTO SSA1                                          
121300     MOVE '  GE' TO GODK-STATUSKODER                                      
121400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
121500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
121600     PERFORM IMS-STATUSKONTROLL                                           
121700     .                                                                    
121800     SKIP2                                                                
121900                                                                          
122000 IMS-GNP-WDK711 SECTION.                                                  
122100     STRING 'WDK711  (IDDC     =' W-IDDC-WDK711-X ')'                     
122200     DELIMITED BY SIZE INTO SSA1                                          
122300     MOVE '  GE' TO GODK-STATUSKODER                                      
122400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
122500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     .                                                                    
122800     SKIP2                                                                
122900                                                                          
123000 IMS-GNP-WDK711-FIRST SECTION.                                            
123100     STRING 'WDK711  *F(IDDC1    =' W-IDDC1-X                             
123200                    '&PRAVCOST >' W-PRAVCOST-X ')'                        
123300             DELIMITED BY SIZE INTO SSA1                                  
123400     MOVE '  GE' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
123600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP2                                                                
124000                                                                          
124100 IMS-GNP-WDK712 SECTION.                                                  
124200     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-WDK712-X ')'                 
124300     DELIMITED BY SIZE INTO SSA1                                          
124400     MOVE '  GE' TO GODK-STATUSKODER                                      
124500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712 SSA1                   
124600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
124700     PERFORM IMS-STATUSKONTROLL                                           
124800     .                                                                    
124900     SKIP2                                                                
125000                                                                          
125100 IMS-GNP-WDK724 SECTION.                                                  
125200     STRING 'WDK711  *F(IDDC     =' W-IDDC-WDK711-X ')'                   
125300     DELIMITED BY SIZE INTO SSA1                                          
125400     MOVE 'WDK724   '    TO SSA2                                          
125500     MOVE '  GE' TO GODK-STATUSKODER                                      
125600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1 SSA2              
125700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     .                                                                    
126000     EJECT                                                                
126100                                                                          
126200 IMS-GET-WLGMTA01 SECTION.                                                
126300     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
126400                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
126500            DELIMITED BY SIZE INTO SSA1                                   
126600     MOVE '  GE' TO GODK-STATUSKODER                                      
126700     CALL CBLTDLI USING GU  GMTA-PCB DLI-IO-WDB201 SSA1                   
126800     MOVE GMTA-STATUS-CODE  TO STATUS-WS                                  
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     SKIP2                                                                
127200                                                                          
127300 IMS-GET-WDB101 SECTION.                                                  
127400     STRING 'WLBETC01(WDB101KY =' W-WDB1-WDB101KY-X ')'                   
127500             DELIMITED BY SIZE INTO SSA1                                  
127600     MOVE '  GE' TO GODK-STATUSKODER                                      
127700     CALL CBLTDLI USING GU  WDB1-PCB DLI-IO-AREA SSA1                     
127800     MOVE WDB1-STATUS-CODE  TO STATUS-WS                                  
127900     PERFORM IMS-STATUSKONTROLL                                           
128000     .                                                                    
128100     SKIP2                                                                
128200                                                                          
128300 IMS-GET-WDB601 SECTION.                                                  
128400     STRING 'WDB601  (IDDC     =' W-WDB6-WDB601KY-X ')'                   
128500             DELIMITED BY SIZE INTO SSA1                                  
128600     MOVE '  GE' TO GODK-STATUSKODER                                      
128700     CALL CBLTDLI USING GU  COST-WDB6-PCB DLI-IO-WDB601 SSA1              
128800     MOVE COST-WDB6-STATUS-CODE  TO STATUS-WS                             
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     SKIP2                                                                
129200                                                                          
129300 IMS-GU-WDB601-EXP SECTION.                                               
129400     STRING 'WDB601  (IDFTG    =' W-IDFTG-X                               
129500                    '&FLMAINDC =' W-FLMAINDC ')'                          
129600             DELIMITED BY SIZE INTO SSA1                                  
129700     MOVE '    ' TO GODK-STATUSKODER                                      
129800     CALL CBLTDLI USING GU  COST-WDB6-PCB DLI-IO-WDB601 SSA1              
129900     MOVE COST-WDB6-STATUS-CODE  TO STATUS-WS                             
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 IMS-GET-WLPRIB01 SECTION.                                                
130500     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
130600             DELIMITED BY SIZE INTO SSA1                                  
130700     MOVE '  GE' TO GODK-STATUSKODER                                      
130800     CALL CBLTDLI USING GU  PRIB-PCB DLI-IO-AREA-2 SSA1                   
130900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     SKIP2                                                                
131300                                                                          
131400 IMS-GET-WLPRIB11 SECTION.                                                
131500     STRING 'WLPRIB11(WDC211KY>=' W-WDC211KY-MIN-X                        
131600                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
131700             DELIMITED BY SIZE INTO SSA1                                  
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA-2 SSA1                   
132000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300     SKIP2                                                                
132400                                                                          
132500 IMS-GET-WLPRIB12 SECTION.                                                
132600     STRING 'WLPRIB12(WDC212KY>=' W-WDC212KY-MIN-X                        
132700                    '&WDC212KY<=' W-WDC212KY-MAX-X ')'                    
132800             DELIMITED BY SIZE INTO SSA1                                  
132900     MOVE '  GE' TO GODK-STATUSKODER                                      
133000     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA-2 SSA1                   
133100     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400     EJECT                                                                
133500                                                                          
133600 IMS-GET-WLPRIB13 SECTION.                                                
133700     STRING 'WLPRIB13(DASTADAT<=' W-DASTADAT-X ')'                        
133800             DELIMITED BY SIZE INTO SSA1                                  
133900     MOVE '  GE' TO GODK-STATUSKODER                                      
134000     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA-2 SSA1                   
134100     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
134200     PERFORM IMS-STATUSKONTROLL                                           
134300     .                                                                    
134400                                                                          
134500 IMS-GET-WLPRIA01 SECTION.                                                
134600     STRING 'WLPRIA01(WDC101KY =' W-WDC101KY-X ')'                        
134700             DELIMITED BY SIZE INTO SSA1                                  
134800     MOVE '  GE' TO GODK-STATUSKODER                                      
134900     CALL CBLTDLI USING GU PRIA-PCB DLI-IO-AREA SSA1                      
135000     MOVE PRIA-STATUS-CODE   TO STATUS-WS                                 
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300                                                                          
135400 IMS-STATUSKONTROLL SECTION.                                              
135500     SET STATUS-IX TO 1                                                   
135600     SEARCH GODK-STATUS                                                   
135700       AT END                                                             
135800         CALL FELLOG                                                      
135900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
136000         CONTINUE                                                         
136100     END-SEARCH                                                           
136200     .                                                                    
