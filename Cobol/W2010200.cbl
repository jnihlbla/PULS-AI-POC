000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                W2010200.                                     
000400 AUTHOR.                    IDK, GÖTEBORG.                                
000500 DATE-WRITTEN.              APRIL -79.                                    
000600 DATE-COMPILED.                                                           
000700*    REMARKS.                                                             
000800*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000900*                'SALDO-INFO ANSKAFFNING'                                 
001000*                KINA NDC OCH LDC SKALL HAMNA I NDC-KOLUMNEN.             
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T102                                              
001400*        MID:         W2I10201                                            
001500*    UTDATA.                                                              
001600*        MOD:         W2O10201                                            
001700*    SUBPROGRAM.                                                          
001800*        FELLOG                                                           
001900*                                                                         
002000*                                                                         
002100*   ÄNDRINGAR:                                                            
002200*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002300*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002400*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002500*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002600*                                                                         
002700*      2007-12-03. E'TRACKER 4820410.                                     
002800*                  DO NOTE INCLUDE OVERSTOCK AT MICRO-LDC IN              
002900*                  DELIVERYPLAN CALCULATION OR IN OVERSTOCK               
003000*                  INFORMATION ON SCREEN 2102.                            
003100*                                                                         
003200*      2011-12-14  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1           
003300*                                                                         
003400*                                                                         
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004200*    -COPY WY2000W9                                                       
004300     SKIP3                                                                
004400 77  IDPGM               PIC X(8)    VALUE 'W2010200'.                    
004500 77  IDARTNR-WS          PIC X(9)    VALUE SPACE.                         
004600 77  JA                  PIC X       VALUE 'J'.                           
004700 77  NEJ                 PIC X       VALUE 'N'.                           
004800 77  STRECK              PIC X       VALUE '-'.                           
004900 77  WS-PRARTBES         PIC X       VALUE 'N'.                           
005000 77  SPIND               PIC S9(9)   VALUE +0   COMP SYNC.                
005100 77  MAX-MOD-LENGD       PIC S9(4)   VALUE +794  COMP SYNC.               
005200 77  VIP-ARTIKEL         PIC X(1)    VALUE 'V'.                           
005300 77  DEFINITIV           PIC S9      VALUE 1.                             
005400 77  SPAR-PRARTSTD       PIC S9(7)V9(2)  VALUE ZERO COMP-3.               
005500     SKIP3                                                                
005600 01  W-IDARTNR-X.                                                         
005700     03  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
005800 01  W-W6D1HSEQ-X.                                                        
005900     03  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
006000 01  W-KDCLAGER-X.                                                        
006100     03  W-KDCLAGER      PIC S9(1)   VALUE +1    COMP-3.                  
006200 01  W-IDSKYLT-X.                                                         
006300     03  W-IDSKYLT       PIC  X(3)   VALUE 'S  '.                         
006400 01  W-DABEHOV-MIN-X.                                                     
006500     03  W-DABEHOV-MIN   PIC   9(6)  VALUE ZERO.                          
006600 01  W-DABEHOV-MAX-X.                                                     
006700     03  W-DABEHOV-MAX   PIC   9(6)  VALUE ZERO.                          
006800 01  W-DAPRLIST-X.                                                        
006900     03  W-DAPRLIST      PIC   9(8)  VALUE ZERO.                          
007000 01  W-WDGXKEY-2231-X.                                                    
007100     03  FILLER          PIC  X(4)   VALUE '2231'.                        
007200     03  FILLER          PIC  X(26)  VALUE LOW-VALUE.                     
007300 01  W-WDGXKEY-2232-X.                                                    
007400     03  W-IDANSK-L      PIC  S9(3)  VALUE ZERO COMP-3.                   
007500     03  FILLER          PIC  X(3)   VALUE LOW-VALUE.                     
007600 01  W-WDGXKEY-2223-X.                                                    
007700     03  W-IDHTYP        PIC  X(4)   VALUE '2223'.                        
007800     03  W-IDANSK        PIC  S9(3)  VALUE ZERO COMP-3.                   
007900     03  FILLER          PIC  X(24)  VALUE LOW-VALUE.                     
008000 01  W-FLNYLARM-X.                                                        
008100     03  W-FLNYLARM      PIC  X      VALUE 'J'.                           
008200 01  W-IDDC-2224-X.                                                       
008300     03  W-IDDC-2224     PIC X(2)    VALUE SPACE.                         
008400 01  NYCKLAR-WDD8.                                                        
008500     03  W-IDDC-X.                                                        
008600         05  W-IDDC      PIC X(2)    VALUE SPACE.                         
008700     03  W-ADBUFFOMR-X.                                                   
008800         05  W-ADBUFFOMR PIC S9(3)   VALUE ZERO  COMP-3.                  
008900     03  W-ADBUFFGANG-X.                                                  
009000         05  W-ADBUFFGANG PIC S9(3)  VALUE ZERO  COMP-3.                  
009100 01  W-IDARTNR-WDA9-X.                                                    
009200     03  W-IDARTNR-WDA9  PIC S9(9)   VALUE ZERO  COMP-3.                  
009300 01  W-IDDISTR-WDA9-X.                                                    
009400     03  W-IDDISTR-WDA9  PIC S9(5)   VALUE ZERO  COMP-3.                  
009500 01  W-IDDC-B6-X.                                                         
009600     03  W-IDDC-B6       PIC X(2)    VALUE SPACE.                         
009700 01  W-WDD901KY-X.                                                        
009800     03  W-IDARTNR-WDD9  PIC S9(9)   VALUE ZERO  COMP-3.                  
009900     03  W-IDDC-WDD9     PIC X(2)    VALUE SPACE.                         
010000     SKIP3                                                                
010100 01      SWITCHAR.                                                        
010200   03    SW-NYCKLAR-OK   PIC X       VALUE 'N'.                           
010300   03    SW-ARTM-SEGM-FINNS PIC X    VALUE 'N'.                           
010400     88  ARTM-SEGMENT-FINNS          VALUE 'J'.                           
010500     SKIP3                                                                
010600 01  W.                                                                   
010700     03  IX                  PIC S9(9)               COMP SYNC.           
010800     SKIP1                                                                
010900     03  WS-KVLS-REM         PIC S9(7)    VALUE ZERO COMP-3.              
011000     03  W-KVAKS             PIC S9(7)    VALUE ZERO COMP-3.              
011100     03  WS-KVAVIS           PIC S9(7)    VALUE ZERO COMP-3.              
011200     03  WS-KVBUFF           PIC S9(7)    VALUE ZERO COMP-3.              
011300     03  WS-ADLAGOMR         PIC S9(3)    VALUE ZERO COMP-3.              
011400     03  WS-ADPLATS          PIC S9(5)    VALUE ZERO COMP-3.              
011500     03  W-KVBR-TOT          PIC S9(7)    VALUE ZERO COMP-3.              
011600     03  W-KVBR-OVR          PIC S9(7)    VALUE ZERO COMP-3.              
011700     03  W-TIINVDAT          PIC S9(5)    VALUE ZERO COMP-3.              
011800     03  W-HUVUDIDLEVNR      PIC X(5)     VALUE SPACE.                    
011900     03  W-TIAVIDAT-SEN      PIC S9(7)    VALUE ZERO COMP-3.              
012000     03  W-DISPONIBELT       PIC S9(7)    VALUE ZERO COMP-3.              
012100     03  W-ARB-SALDO         PIC S9(7)    VALUE ZERO COMP-3.              
012200     03  W-CDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
012300     03  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
012400     03  W-NDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
012500     03  W-SDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
012600     03  W-NDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
012700     03  W-NDC-KVROS         PIC S9(7)    VALUE ZERO COMP-3.              
012800     03  W-SDC-KVAKS-SDC     PIC S9(7)    VALUE ZERO COMP-3.              
012900     03  W-NDC-KVAKS-NDC     PIC S9(7)    VALUE ZERO COMP-3.              
013000     03  W-SDC-KVAKS-PAV     PIC S9(7)    VALUE ZERO COMP-3.              
013100     03  W-NDC-KVAKS-PAV     PIC S9(7)    VALUE ZERO COMP-3.              
013200     03  W-SDC-KVPB          PIC S9(6)V9  VALUE ZERO COMP-3.              
013300     03  W-NDC-KVPB          PIC S9(6)V9  VALUE ZERO COMP-3.              
013400     03  W-SDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.              
013500     03  W-NDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.              
013600     03  W-SDC-KVEFRS        PIC S9(7)    VALUE ZERO COMP-3.              
013700     03  W-NDC-KVEFRS        PIC S9(7)    VALUE ZERO COMP-3.              
013800     03  W-SDC-KVUTRS        PIC S9(7)    VALUE ZERO COMP-3.              
013900     03  W-NDC-KVUTRS        PIC S9(7)    VALUE ZERO COMP-3.              
014000     03  W-TILLG-SDC         PIC S9(7)               COMP-3.              
014100     03  W-OVERLAGER-SDC     PIC S9(7)               COMP-3.              
014200     03  W-KVART             PIC S9(7)    VALUE ZERO COMP-3.              
014300     03  W-KVART-TOT-C1      PIC S9(9)    VALUE ZERO COMP-3.              
014400     03  WS-IDARTNR          PIC  9(8)    VALUE ZERO.                     
014500     03  WS-FLGEMART         PIC  X(1)    VALUE 'N'.                      
014600     03  W-TIAAAAVV.                                                      
014700       05 W-TISEKEL          PIC  9(2)    VALUE ZERO.                     
014800       05 W-TIAA             PIC  9(2)    VALUE ZERO.                     
014900       05 W-TIVV             PIC  9(2)    VALUE ZERO.                     
015000     03  W-DADISPIN          PIC  9(6)    VALUE ZERO.                     
015100     03  WS-TPO-NAESTA-INLEV PIC S9(9)    VALUE ZERO COMP-3.              
015200     03  WS-KVOKS-TOT     PIC S9(9) OCCURS 3 VALUE ZERO COMP-3.           
015300     03  WS-SUTPO-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
015400     03  WS-IDLEVNR-8        PIC  X(8)    VALUE SPACE.                    
015500 01  DATUM-FAELT.                                                         
015600     03  DAGENS-AAAAMMDD     PIC 9(8)     VALUE ZERO.                     
015700                                                                          
015800     03  DAGENS-DATUM        PIC 9(6)     VALUE ZERO.                     
015900     03  DAGENS-AAVV         PIC 9(4)     VALUE ZERO.                     
016000     03  FILLER REDEFINES DAGENS-AAVV.                                    
016100         05  DAGENS-AA       PIC 9(2).                                    
016200         05  DAGENS-VV       PIC 9(2).                                    
016300     03  TIFINLV-AAVVD       PIC 9(5)     VALUE ZERO.                     
016400     03  FILLER REDEFINES TIFINLV-AAVVD.                                  
016500         05  TIFINLV-AA      PIC 9(2).                                    
016600         05  TIFINLV-VV      PIC 9(2).                                    
016700         05  FILLER          PIC 9(1).                                    
016800     03  VECKO-SKILLNAD      PIC S9(5) VALUE ZERO  COMP-3.                
016900     EJECT                                                                
017000 01 MESSAGE-CODES.                                                        
017100    03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                  
017200     EJECT                                                                
017300 01      MEDDELANDE.                                                      
017400                                                                          
017500* FEL-MEDDELANDEN                                                         
017600   03 W-FEL-1.                                                            
017700     05 FILLER  PIC X(26) VALUE 'ARTIKELNUMMER EJ NUMERISKT'.             
017800     05 FILLER  PIC X(26) VALUE 'PART NO. NOT NUMERIC      '.             
017900   03 FILLER REDEFINES W-FEL-1.                                           
018000     05 FEL-1   PIC X(26) OCCURS 2.                                       
018100                                                                          
018200   03 W-FEL-2.                                                            
018300     05 FILLER  PIC X(35) VALUE 'ARTIKEL SAKNAS I DATABAS'.               
018400     05 FILLER  PIC X(35) VALUE 'PART NO. MISSING IN DATA BASE'.          
018500   03 FILLER REDEFINES W-FEL-2.                                           
018600     05 FEL-2   PIC X(35) OCCURS 2.                                       
018700                                                                          
018800   03 W-FEL-3.                                                            
018900     05 FILLER  PIC X(15) VALUE 'ARTIKELN ERSATT'.                        
019000     05 FILLER  PIC X(15) VALUE 'PART REPLACED  '.                        
019100   03 FILLER REDEFINES W-FEL-3.                                           
019200     05 FEL-3   PIC X(15) OCCURS 2.                                       
019300                                                                          
019400   03 W-FEL-4.                                                            
019500     05 FILLER  PIC X(19) VALUE 'ARTIKELN ÄR DELETAD'.                    
019600     05 FILLER  PIC X(19) VALUE 'PART IS DELETED    '.                    
019700   03 FILLER REDEFINES W-FEL-4.                                           
019800     05 FEL-4   PIC X(19) OCCURS 2.                                       
019900                                                                          
020000   03 W-FEL-5.                                                            
020100     05 FILLER  PIC X(19) VALUE 'ERSÄTTANDE ARTIKEL'.                     
020200     05 FILLER  PIC X(19) VALUE 'REPLACING PART NO.'.                     
020300   03 FILLER REDEFINES W-FEL-5.                                           
020400     05 FEL-5   PIC X(19) OCCURS 2.                                       
020500                                                                          
020600   03 W-FEL-6.                                                            
020700     05 FILLER  PIC X(19) VALUE 'ARTIKELN UTGÅR'.                         
020800     05 FILLER  PIC X(19) VALUE 'PART EXPIRES  '.                         
020900   03 FILLER REDEFINES W-FEL-6.                                           
021000     05 FEL-6   PIC X(19) OCCURS 2.                                       
021100                                                                          
021200   03 W-FEL-7.                                                            
021300     05 FILLER  PIC X(19) VALUE 'ARTIKELN HAR UTGÅTT'.                    
021400     05 FILLER  PIC X(19) VALUE 'PART HAS EXPIRED   '.                    
021500   03 FILLER REDEFINES W-FEL-7.                                           
021600     05 FEL-7   PIC X(19) OCCURS 2.                                       
021700                                                                          
021800   03 W-FEL-8.                                                            
021900     05 FILLER  PIC X(19) VALUE 'OBEHÖRIG ANVÄNDARE '.                    
022000     05 FILLER  PIC X(19) VALUE 'USER NOT AUTHORIZED'.                    
022100   03 FILLER REDEFINES W-FEL-8.                                           
022200     05 FEL-8   PIC X(19) OCCURS 2.                                       
022300                                                                          
022400* INFO-MEDDELANDEN                                                        
022500   03 W-MED-1.                                                            
022600     05 FILLER  PIC X(20) VALUE 'GEMENSAM PV/LV'.                         
022700     05 FILLER  PIC X(20) VALUE 'COMMON VCC/VTC'.                         
022800   03 FILLER REDEFINES W-MED-1.                                           
022900     05 MED-1   PIC X(20) OCCURS 2.                                       
023000                                                                          
023100     EJECT                                                                
023200                                                                          
023300*01    -COPY WWDC99                                                       
023400                                                                          
023500 01  FILLER              PIC  X(16)  VALUE 'BYTES-DIST'.                  
023600 01  TEST-IDDISTR        PIC  9(5)   COMP-3.                              
023700*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER              PIC  X(16)  VALUE 'BYTES-ART '.                  
024100 01  TEST-IDARTNR        PIC  9(9)   COMP-3 VALUE ZERO.                   
024200*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
024300     EJECT                                                                
024400*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
024500     EJECT                                                                
024600*      --- VALID IDDC CODES                                               
024700*                                                                         
024800*01    -COPY WWDCKONS                                                     
024900       EJECT                                                              
025000*                        ****    DYNAMISKA SUBPROGRAM                     
025100 01      DYNAMISKA-SUBPROGRAM.                                            
025200   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
025300   03    WDATKONV        PIC X(8)    VALUE 'WDATKONV'.                    
025400   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
025500   03    W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
025600   03 WMEDKONV           PIC X(8)    VALUE 'WMEDKONV'.                    
025700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
025800*01 -COPY WMEDAREA                                                        
025900     SKIP3                                                                
026000*                        ****    PARAMETRAR TILL WDATKONV                 
026100*01      -COPY WDATAREA                                                   
026200*                        ****    PARAMETRAR TILL W005INIT                 
026300*01      -COPY WMSGINIT                                                   
026400*                        ****    TP-AREOR                                 
026500 01      TP-WS.                                                           
026600   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
026700     SKIP3                                                                
026800*01      MID -COPY W2I10201 -PRE MID-.                                    
026900     EJECT                                                                
027000*01      -COPY WMSGAREA                                                   
027100     EJECT                                                                
027200*  03    MOD -COPY W2O10201 -PRE MOD- -RED MSG-AREA.                      
027300     EJECT                                                                
027400*01  -COPY WMFSAREA.                                                      
027500     EJECT                                                                
027600******************************************************************        
027700*****                                                                     
027800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027900*****                                                                     
028000 01  IMS-WS.                                                              
028100   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
028200     SKIP3                                                                
028300*****                    **** STATUS-KOD FRÅN IMS                         
028400   03    STATUS-WS       PIC XX.                                          
028500         88  SEGMENT-FINNS       VALUE '  '.                              
028600         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
028700         88  SEGMENT-SLUT        VALUE 'GB'.                              
028800     SKIP3                                                                
028900   03    GODK-STATUSKODER.                                                
029000     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
029100     SKIP3                                                                
029200 01      SSA1            PIC X(64).                                       
029300 01      SSA2            PIC X(64).                                       
029400 01      SSA3            PIC X(64).                                       
029500     EJECT                                                                
029600 01  NYCKLAR-TILL-DB2.                                                    
029700     03  W-IDARTNR-BYART-X.                                               
029800         05  W-IDARTNR-BYART     PIC S9(9)   VALUE ZERO COMP-3.           
029900                                                                          
030000 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
030100*01  -COPY BYART -PRE BYART-                                              
030200     EJECT                                                                
030300 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
030400       EXEC SQL INCLUDE BYART END-EXEC.                                   
030500     SKIP3                                                                
030600 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
030700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
030800                                                                          
030900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
031000 01  DB2-WS.                                                              
031100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
031200         88  CURSOR-OK                       VALUE 000.                   
031300         88  RADER-FINNS                     VALUE 000.                   
031400         88  RADER-SAKNAS                    VALUE 100.                   
031500         88  ATKOMST-FEL                     VALUE 904.                   
031600     03  GODK-SQLCODEKODER.                                               
031700         05  GODK-SQLCODE OCCURS 5                                        
031800             INDEXED BY SQLCODE-IX PIC 9(3).                              
031900     EJECT                                                                
032000*                            IMS FUNKTIONSKODER                           
032100*01      -COPY W0003                                                      
032200     EJECT                                                                
032300*                            DLI INPUT-OUTPUT AREA                        
032400 01  DLI-IO-AREA-01.                                                      
032500*03  WLARTC01 -COPY WDK601                                                
032600     EJECT                                                                
032700 01  DLI-IO-AREA-11.                                                      
032800*03  WLARTC11 -COPY WDK611                                                
032900     EJECT                                                                
033000 01  DLI-IO-AREA-21.                                                      
033100*03  WLARTC21 -COPY WDK621                                                
033200     EJECT                                                                
033300 01  FILLER.                                                              
033400 03  DLI-IO-AREA     PIC X(300)  VALUE SPACE.                             
033500                                                                          
033600*03  WLARTS01 -COPY WDK701   -RED DLI-IO-AREA.                            
033700     EJECT                                                                
033800*03  WLARTS11 -COPY WDK711   -RED DLI-IO-AREA.                            
033900     EJECT                                                                
034000*03  WLBENA11 -COPY WDD311   -PRE BENA-   -RED DLI-IO-AREA.               
034100     EJECT                                                                
034200*03  WLINLB11 -COPY WDD902   -PRE LEVNR-  -RED DLI-IO-AREA.               
034300     EJECT                                                                
034400*03  WLXXBU01 -COPY WDGX2223 -PRE XXBU-   -RED DLI-IO-AREA.               
034500     EJECT                                                                
034600*03  WLXXBU11 -COPY WDGX2224 -PRE XXBU-   -RED DLI-IO-AREA.               
034700     EJECT                                                                
034800*03  WLXXBX01 -COPY WDGX01   -PRE XXBX-   -RED DLI-IO-AREA.               
034900     EJECT                                                                
035000*03  WLXXBX11 -COPY WDGX2232 -PRE XXBX-   -RED DLI-IO-AREA.               
035100     EJECT                                                                
035200*03  W6INLA11 -COPY W6D111   -PRE INLA-   -RED DLI-IO-AREA.               
035300     EJECT                                                                
035400*                            DLI INPUT-OUTPUT AREA2                       
035500 01  FILLER.                                                              
035600 03  DLI-IO-AREA2     PIC X(200)  VALUE SPACE.                            
035700     SKIP3                                                                
035800*03  WLARTM01 -COPY WDK901   -PRE ARTM-   -RED DLI-IO-AREA2.              
035900     EJECT                                                                
036000*03  WLARTM11 -COPY WDK911   -PRE ARTM-   -RED DLI-IO-AREA2.              
036100     EJECT                                                                
036200*                            DLI INPUT-OUTPUT AREA3                       
036300 01  FILLER.                                                              
036400 03  DLI-IO-AREA3     PIC X(200)  VALUE SPACE.                            
036500     SKIP3                                                                
036600*03  WLARTD01 -COPY WDD801   -PRE ARTD-   -RED DLI-IO-AREA3.              
036700     EJECT                                                                
036800*03  WLARTD11 -COPY WDD811   -PRE ARTD-   -RED DLI-IO-AREA3.              
036900     EJECT                                                                
037000                                                                          
037100 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDA901'.                    
037200 01  DLI-IO-WDA901.                                                       
037300*    03  -COPY WDA901                                                     
037400     EJECT                                                                
037500                                                                          
037600 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDA911'.                    
037700 01  DLI-IO-WDA911.                                                       
037800*    03  -COPY WDA911                                                     
037900     EJECT                                                                
038000                                                                          
038100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
038200 01   DLI-IO-AREA-B601.                                                   
038300*     03  -COPY WDB601                                                    
038400                                                                          
038500     EJECT                                                                
038600                                                                          
038700 LINKAGE SECTION.                                                         
038800*01  -COPY W0009     -PRE MSG-                                            
038900     EJECT                                                                
039000*01  -COPY W0008     -PRE USEA-                                           
039100         05  FILLER           PIC X.                                      
039200     EJECT                                                                
039300*01  -COPY W0008     -PRE BENA-                                           
039400         05  FILLER           PIC X.                                      
039500     EJECT                                                                
039600*01  -COPY W0008     -PRE ARTC-                                           
039700         05  FILLER            PIC X.                                     
039800     SKIP2                                                                
039900*01  -COPY W0008     -PRE ARTS-                                           
040000         05  FILLER           PIC X.                                      
040100     SKIP2                                                                
040200*01  -COPY W0008     -PRE ARTM-                                           
040300         05  FILLER           PIC X.                                      
040400     EJECT                                                                
040500*01  -COPY W0008     -PRE XXBU-                                           
040600         05  FILLER           PIC X.                                      
040700     EJECT                                                                
040800*01  -COPY W0008     -PRE XXBX-                                           
040900         05  FILLER           PIC X.                                      
041000     SKIP2                                                                
041100*01  -COPY W0008     -PRE INLA-                                           
041200         05  FILLER           PIC X.                                      
041300     EJECT                                                                
041400*01  -COPY W0008     -PRE INLB-                                           
041500         05  FILLER           PIC X.                                      
041600     EJECT                                                                
041700*01  -COPY W0008     -PRE WDD8-                                           
041800         05  FILLER           PIC X.                                      
041900     EJECT                                                                
042000*01  -COPY W0008     -PRE WDA9-                                           
042100         05  FILLER           PIC X.                                      
042200     EJECT                                                                
042300*01  -COPY W0008      -PRE WDB6-                                          
042400     05  FILLER                  PIC X.                                   
042500     EJECT                                                                
042600 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
042700                                    BENA-PCB ARTC-PCB ARTS-PCB            
042800                                    ARTM-PCB XXBU-PCB XXBX-PCB            
042900                                    INLA-PCB INLB-PCB WDD8-PCB            
043000                                    WDA9-PCB WDB6-PCB.                    
043100     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
043200                                    BENA-PCB ARTC-PCB ARTS-PCB            
043300                                    ARTM-PCB XXBU-PCB XXBX-PCB            
043400                                    INLA-PCB INLB-PCB WDD8-PCB            
043500                                    WDA9-PCB WDB6-PCB.                    
043600                                                                          
043700     PERFORM IMS-GET-MSG                                                  
043800                                                                          
043900     IF  SEGMENT-FINNS                                                    
044000         PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                              
044100                                                                          
044200         IF  SW-NYCKLAR-OK = JA                                           
044300                                                                          
044400             MOVE IDARTNR-WS TO W-IDARTNR                                 
044500                                TEST-IDARTNR                              
044600                                W-IDARTNR-WDD9                            
044700             PERFORM IMS-GET-ART-SEG                                      
044800                                                                          
044900             IF  SEGMENT-FINNS                                            
045000                MOVE ART-IDLEVNR TO WS-IDLEVNR-8                          
045100                IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                 
045200                OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE           
045300                   PERFORM B-RED-BILD-FRAN-WDK6-7-WDK9                    
045400                   PERFORM D-RED-BILD-FRAN-WDD3                           
045500                   PERFORM E-RED-BILD-FRAN-WDD8                           
045600                   PERFORM G-RED-BILD-FRAN-W6D1                           
045700                   PERFORM H-RED-BILD-FRAN-WDA9                           
045800                   PERFORM F-KOLLA-OM-LARM-FINNS                          
045900                ELSE                                                      
046000                   MOVE FEL-8 (SPIND) TO MOD-MESSAGE                      
046100                END-IF                                                    
046200             ELSE                                                         
046300                MOVE FEL-2 (SPIND) TO MOD-TEMFSINF                        
046400             END-IF                                                       
046500                                                                          
046600         END-IF                                                           
046700                                                                          
046800         PERFORM IMS-INSERT-MSG                                           
046900     END-IF                                                               
047000                                                                          
047100     MOVE ZERO TO RETURN-CODE                                             
047200     GOBACK                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
047600                                                                          
047700     MOVE JA TO SW-NYCKLAR-OK                                             
047800     IF MSG-DUBBLA-TRANSKODER                                             
047900         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
048000         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
048100         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10201               
048200     ELSE                                                                 
048300         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
048400         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
048500         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I10201               
048600     END-IF                                                               
048700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
048800     MOVE '001'             TO MSGI-KDCALL                                
048900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049100     MOVE '2102'            TO MSGI-IDTRANS                               
049200     IF MFS-IDTRANS(1:2) NOT = '42'                                       
049300       IF MFS-IDTRANS = '2102'                                            
049400       OR (MID-IDARTNR-IN NUMERIC                                         
049500       AND MID-IDARTNR-IN > ZERO)                                         
049600           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
049700       END-IF                                                             
049800     END-IF                                                               
049900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050000     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
050100     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
050200     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
050300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
050400                                                                          
050500     IF MSGI-IDLAND-SPR = SPACE                                           
050600       IF MSGI-IDSPRAK = SPACE                                            
050700         IF SWEDISH-TEXT                                                  
050800           MOVE +1 TO SPIND                                               
050900         ELSE                                                             
051000           MOVE +2 TO SPIND                                               
051100         END-IF                                                           
051200       ELSE                                                               
051300         IF MSGI-IDSPRAK = 'SV'                                           
051400           MOVE +1 TO SPIND                                               
051500         ELSE                                                             
051600           MOVE +2 TO SPIND                                               
051700         END-IF                                                           
051800       END-IF                                                             
051900     ELSE                                                                 
052000       IF MSGI-IDLAND-SPR = 'SE'                                          
052100         MOVE +1 TO SPIND                                                 
052200       ELSE                                                               
052300         MOVE +2 TO SPIND                                                 
052400       END-IF                                                             
052500     END-IF                                                               
052600                                                                          
052700                                                                          
052800     MOVE LOW-VALUE TO MOD-AREA-OUTPUT                                    
052900     MOVE 'W2O10201' TO MFS-IDMOD                                         
053000     MOVE '2102' TO MOD-TRANS-NUMMER                                      
053100     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
053200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
053300                      MOD-MESSAGE                                         
053400                      MOD-BEART-SVE                                       
053500                      MOD-LAGERPLATS                                      
053600                      MOD-TIINVDAT                                        
053700                      MOD-KVEFRS-CDC                                      
053800                      MOD-KVEFRS-SDC                                      
053900                      MOD-KVEFRS-NDC                                      
054000                      MOD-TEMFSINF                                        
054100     PERFORM MFS-RENSA-UTDATA-FAELT                                       
054200                                                                          
054300     ACCEPT DAGENS-DATUM FROM DATE                                        
054400     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
054500     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
054600     CALL WDATKONV USING DAT-KDDATFORM                                    
054700                         DAT-I-TIDATUM                                    
054800                         DAT-O-TIDATUM                                    
054900                         DAT-KDSVAR                                       
055000     MOVE DAT-TIAAVV-GRP TO DAGENS-AAVV                                   
055100                                                                          
055200     IF  IDARTNR-WS NOT NUMERIC                                           
055300         MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                
055400         MOVE NEJ TO SW-NYCKLAR-OK                                        
055500     END-IF                                                               
055600                                                                          
055700     MOVE ZERO             TO W-ARB-SALDO                                 
055800                              WS-KVOKS-TOT (1)                            
055900                              WS-KVOKS-TOT (2)                            
056000                              WS-KVOKS-TOT (3)                            
056100                              WS-SUTPO-TOT                                
056200                                                                          
056300     .                                                                    
056400     EJECT                                                                
056500 B-RED-BILD-FRAN-WDK6-7-WDK9 SECTION.                                     
056600******************************************************************        
056700*                                                                *        
056800*    REDIGERING AV BILD MED DATA FRÅN WDK6 , WDK7 OCH WDK9       *        
056900*                                                                *        
057000******************************************************************        
057100                                                                          
057200     MOVE ART-REKSIFFR TO MOD-REKSIFFR                                    
057300*    MOVE ART-IDLEVNR  TO MOD-IDLEVNR                                     
057400     MOVE ART-IDLEVNR  TO W-HUVUDIDLEVNR                                  
057300     MOVE ART-KDANSKSEG TO MOD-KDANSKSEG                                  
057500     MOVE STRECK TO MOD-STRECK-1                                          
057600     IF  ART-KDERS-UTG > ZERO                                             
057700       IF ART-KDERS-UTG = +29 OR +52                                      
057800         MOVE FEL-7 (SPIND) TO MOD-MESSAGE                                
057900       ELSE                                                               
058000         MOVE FEL-3 (SPIND) TO MOD-MESSAGE                                
058100       END-IF                                                             
058200       MOVE ART-KDERS-UTG TO MOD-KDERS (1)                                
058300     END-IF                                                               
058400     IF  ART-FLERS = JA                                                   
058500       MOVE FEL-5 (SPIND) TO MOD-TEMFSINF                                 
058600     END-IF                                                               
058700                                                                          
058800     PERFORM IMS-GET-ARTM01                                               
058900                                                                          
059000     IF SEGMENT-FINNS                                                     
059100       MOVE JA TO SW-ARTM-SEGM-FINNS                                      
059200         MOVE ARTM-ART-SUTPO-TOT     TO WS-SUTPO-TOT                      
059300                                        MOD-SUTPO-TOT (1)                 
059400         COMPUTE WS-KVOKS-TOT (1) =   ARTM-ART-KVOKS-BULK                 
059500                                    + ARTM-ART-KVOKS-DAG                  
059600                                    + ARTM-ART-KVOKS-VOR                  
059700         MOVE WS-KVOKS-TOT (1)       TO MOD-KVOKS (1)                     
059800     ELSE                                                                 
059900         MOVE ZERO TO WS-SUTPO-TOT                                        
060000                      MOD-SUTPO-TOT (1)                                   
060100                      WS-KVOKS-TOT (1)                                    
060200                      MOD-KVOKS (1)                                       
060300     END-IF                                                               
060400                                                                          
060500     PERFORM IMS-GET-CLAG-SEG                                             
060600                                                                          
060700     IF SEGMENT-FINNS                                                     
060800         MOVE CLAG-IDANSK       TO MOD-IDANSK                             
060900                                   W-IDANSK-L                             
061000         MOVE CLAG-KDAVT        TO MOD-KDAVT                              
061100         MOVE CLAG-KDKSP        TO MOD-KDKSP                              
061200         MOVE CLAG-KVSLUTKP     TO MOD-KVSLUTKP                           
061300         IF CLAG-FLGEMART = JA                                            
061400            MOVE JA TO WS-FLGEMART                                        
061500            MOVE MED-1 (SPIND) TO MOD-TEMFSINF                            
061600         END-IF                                                           
061700         MOVE CLAG-PRARTSTD    TO SPAR-PRARTSTD                           
061800     ELSE                                                                 
061900         MOVE SPACE TO W-HUVUDIDLEVNR                                     
062000         MOVE ZERO  TO SPAR-PRARTSTD                                      
062100     END-IF                                                               
062200     MOVE ZERO TO W-ARB-SALDO                                             
062300                                                                          
062400     PERFORM BA-RED-FRAN-CLAG-WDK611                                      
062500                                                                          
062600     PERFORM BB-RED-FRAN-PRL-WDK621                                       
062700                                                                          
062800     PERFORM BC-RED-FRAN-SDC-NDC                                          
062900                                                                          
063000     MOVE W-OVERLAGER-SDC   TO MOD-KVLS-SDC-OVER                          
063100     MOVE W-SDC-KVPB        TO MOD-KVPB-SEP    (2)                        
063200     MOVE W-NDC-KVPB        TO MOD-KVPB-SEP    (3)                        
063300     MOVE W-SDC-KVOKS       TO WS-KVOKS-TOT    (2)                        
063400                               MOD-KVOKS       (2)                        
063500     MOVE W-NDC-KVOKS       TO WS-KVOKS-TOT    (3)                        
063600                               MOD-KVOKS       (3)                        
063700                                                                          
063800     COMPUTE W-DISPONIBELT =  W-SDC-KVLS                                  
063900                            - W-SDC-KVRESS                                
064000                                                                          
064100     MOVE W-DISPONIBELT     TO MOD-DISP        (2)                        
064200                                                                          
064300     COMPUTE W-DISPONIBELT =  W-NDC-KVLS                                  
064400                            - W-NDC-KVRESS                                
064500                                                                          
064600     MOVE W-DISPONIBELT     TO MOD-DISP        (3)                        
064700                                                                          
064800     MOVE W-SDC-KVAKS-PAV   TO MOD-KVAKS-PAV   (2)                        
064900     MOVE W-NDC-KVAKS-PAV   TO MOD-KVAKS-PAV   (3)                        
065000     MOVE W-SDC-KVAKS-SDC   TO MOD-KVAKS-LAGER (2)                        
065100     MOVE W-NDC-KVAKS-NDC   TO MOD-KVAKS-LAGER (3)                        
065200     MOVE W-SDC-KVEFRS      TO MOD-KVEFRS-SDC                             
065300     MOVE W-NDC-KVEFRS      TO MOD-KVEFRS-NDC                             
065400     MOVE W-SDC-KVLS        TO MOD-KVLS        (2)                        
065500     MOVE W-NDC-KVLS        TO MOD-KVLS        (3)                        
065600     MOVE +0                TO MOD-KVRESS      (2)                        
065700     MOVE W-NDC-KVRESS      TO MOD-KVRESS      (3)                        
065800     MOVE +0                TO MOD-KVROS       (2)                        
065900     MOVE W-NDC-KVROS       TO MOD-KVROS       (3)                        
066000     MOVE W-SDC-KVUTRS      TO MOD-KVUTRS      (2)                        
066100     MOVE W-NDC-KVUTRS      TO MOD-KVUTRS      (3)                        
066200                                                                          
066300     IF  W-TIINVDAT > ZERO                                                
066400         MOVE W-TIINVDAT TO MOD-TIINVDAT                                  
066500     END-IF                                                               
066600                                                                          
066700     MOVE MSGI-IDDC         TO W-IDDC-WDD9                                
066800                               WS-IDDC                                    
066900     PERFORM IMS-GET-INLB01                                               
067000     MOVE +0 TO W-KVBR-TOT                                                
067100                W-KVBR-OVR                                                
067200     IF SEGMENT-FINNS                                                     
067300         PERFORM IMS-GET-LEVERANTOER-SEG                                  
067400         PERFORM UNTIL SEGMENT-SAKNAS                                     
067500              ADD LEVNR-KVBR   TO W-KVBR-TOT                              
067600              IF LEVNR-IDLEVNR NOT = W-HUVUDIDLEVNR                       
067700                  ADD LEVNR-KVBR TO W-KVBR-OVR                            
067800              END-IF                                                      
067900              PERFORM IMS-GET-LEVERANTOER-SEG                             
068000         END-PERFORM                                                      
068100     END-IF                                                               
068200     MOVE W-KVBR-OVR TO MOD-KVBR-OVR                                      
068300     MOVE W-KVBR-TOT TO MOD-KVBR-TOT                                      
068400     .                                                                    
068500     EJECT                                                                
068600 BA-RED-FRAN-CLAG-WDK611 SECTION.                                         
068700                                                                          
068800     IF  SEGMENT-FINNS                                                    
068900        MOVE CLAG-IDLEVNR-SHIP   TO MOD-IDLEVNR-SHIP                      
069000        MOVE CLAG-KDERS          TO MOD-KDERS       (1)                   
069100        MOVE CLAG-IDLEVNR-SEN    TO MOD-IDLEVNR-SEN                       
069200        MOVE CLAG-IDFS-SEN       TO MOD-IDFS-SEN                          
069300        MOVE CLAG-KVAVIS-SEN     TO MOD-KVAVIS-SEN                        
069400        MOVE CLAG-TIAVIDAT-SEN   TO MOD-TIAVIDAT-SEN                      
069500        MOVE CLAG-PRARTSTD       TO MOD-PRARTSTD                          
069600        MOVE CLAG-KVPB-SEP       TO MOD-KVPB-SEP    (1)                   
069700        MOVE CLAG-KVPB-SATS      TO MOD-KVPB-SATS   (1)                   
069800        IF  CLAG-TISLJUST NOT = +0                                        
069900            MOVE CLAG-RESLJUST   TO MOD-RESLJUST    (1)                   
070000        END-IF                                                            
070100        MOVE CLAG-KVMP           TO MOD-KVMP        (1)                   
070200        MOVE CLAG-TIINVDAT       TO W-TIINVDAT                            
070300        MOVE CLAG-KDGK           TO MOD-KDGK                              
070400        MOVE CLAG-KDLTK          TO MOD-KDLTK                             
070500        MOVE MFS-RENSA-FAELT     TO MOD-KDUART                            
070600                                                                          
070700        MOVE CLAG-KVAKS-T        TO MOD-KVAKS-T     (1)                   
070800        MOVE CLAG-KVAKS-PAV      TO MOD-KVAKS-PAV   (1)                   
070900        MOVE CLAG-KVAKS-CDC      TO MOD-KVAKS-LAGER (1)                   
071000        MOVE CLAG-KVRESS         TO MOD-KVRESS      (1)                   
071100        MOVE CLAG-KVROS          TO MOD-KVROS       (1)                   
071200        MOVE CLAG-KVSLAGER       TO MOD-KVSLAGER    (1)                   
071300        MOVE CLAG-KVSPANT        TO MOD-KVSPANT     (1)                   
071400        MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR  WS-ADLAGOMR             
071500        MOVE CLAG-ADGANG         TO MOD-ADGANG                            
071600        MOVE CLAG-ADPLATS        TO MOD-ADPLATS   WS-ADPLATS              
071700        MOVE CLAG-KVEFRS         TO MOD-KVEFRS-CDC                        
071800        MOVE CLAG-KVUTRS         TO MOD-KVUTRS      (1)                   
071900        MOVE CLAG-FLCDART        TO MOD-FLCDART                           
072000                                                                          
072100        MOVE CLAG-KVVORKO        TO MOD-KVVORKO                           
072200                                                                          
072300        IF CLAG-KDERS > ZERO                                              
072400          IF CLAG-KDERS = +09 OR +19                                      
072500            MOVE FEL-6 (SPIND) TO MOD-MESSAGE                             
072600          ELSE                                                            
072700            IF CLAG-KDERS = +29 OR +52                                    
072800              MOVE FEL-7 (SPIND) TO MOD-MESSAGE                           
072900            ELSE                                                          
073000              MOVE FEL-3 (SPIND) TO MOD-MESSAGE                           
073100            END-IF                                                        
073200          END-IF                                                          
073300        ELSE                                                              
073400          IF CLAG-IDDC-REF NOT = SPACE                                    
073500            MOVE INF-REFILL-PART TO MED-IDMFSFEL                          
073600            CALL WMEDKONV     USING MED-WMEDAREA                          
073700            MOVE MED-MFSFEL      TO MOD-MESSAGE                           
073800          END-IF                                                          
073900        END-IF                                                            
074000                                                                          
074100        PERFORM BAA-RED-FRAN-ARTM11                                       
074200                                                                          
074300        COMPUTE W-KVAKS = CLAG-KVAKS-CDC                                  
074400                        + CLAG-KVAKS-PAV                                  
074500                        + CLAG-KVAKS-T                                    
074600                                                                          
074700        COMPUTE W-CDC-KVLS = CLAG-KVLS                                    
074800        MOVE W-CDC-KVLS          TO MOD-KVLS     (1)                      
074900                                                                          
075000        COMPUTE W-DISPONIBELT =  W-CDC-KVLS                               
075100                               - CLAG-KVRESS                              
075200                                                                          
075300        MOVE W-DISPONIBELT       TO MOD-DISP  (1)                         
075400                                                                          
075500        COMPUTE W-ARB-SALDO      = W-ARB-SALDO                            
075600                                   + W-DISPONIBELT                        
075700                                   - CLAG-KVROS                           
075800                                   - WS-TPO-NAESTA-INLEV                  
075900                                   - WS-KVOKS-TOT (1)                     
076000                                   + W-KVAKS                              
076100        MOVE W-ARB-SALDO         TO MOD-ARB-SALDO (1)                     
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 BAA-RED-FRAN-ARTM11 SECTION.                                             
076600                                                                          
076700     MOVE ZERO TO WS-TPO-NAESTA-INLEV                                     
076800                                                                          
076900     IF CLAG-TIDISPIN = ZERO                                              
077000       MOVE WS-SUTPO-TOT     TO WS-TPO-NAESTA-INLEV                       
077100     ELSE                                                                 
077200       IF ARTM-SEGMENT-FINNS                                              
077300         MOVE ZERO       TO W-DABEHOV-MIN                                 
077400         PERFORM S01-KONV-TIDISPIN                                        
077500         MOVE W-DADISPIN TO W-DABEHOV-MAX                                 
077600         PERFORM IMS-GET-ARTM11                                           
077700         PERFORM UNTIL SEGMENT-SAKNAS                                     
077800           COMPUTE WS-TPO-NAESTA-INLEV =  WS-TPO-NAESTA-INLEV             
077900                                        + ARTM-ANT-SUTPO-PB               
078000                                        + ARTM-ANT-SUTPO-EJPB             
078100           PERFORM IMS-GET-ARTM11                                         
078200         END-PERFORM                                                      
078300       END-IF                                                             
078400     END-IF                                                               
078500     .                                                                    
078600     EJECT                                                                
078700                                                                          
078800 BB-RED-FRAN-PRL-WDK621 SECTION.                                          
078900                                                                          
079000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
079100     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
079200     PERFORM IMS-GET-PRL-SEG                                              
079300     IF SEGMENT-SAKNAS                                                    
079400       MOVE SPAR-PRARTSTD       TO MOD-PRARTBES                           
079500     ELSE                                                                 
079600       MOVE NEJ                 TO WS-PRARTBES                            
079700       PERFORM UNTIL  SEGMENT-SAKNAS                                      
079800         IF PRL-SUINLEV-PR > ZERO                                         
079900           MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                          
080000           SET SEGMENT-SAKNAS TO TRUE                                     
080100         ELSE                                                             
080200           IF WS-PRARTBES = NEJ                                           
080300             MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                         
080400             MOVE JA              TO WS-PRARTBES                          
080500           END-IF                                                         
080600           PERFORM IMS-GET-PRL-SEG                                        
080700         END-IF                                                           
080800       END-PERFORM                                                        
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 BC-RED-FRAN-SDC-NDC SECTION.                                             
081300                                                                          
081400     MOVE ZERO TO W-OVERLAGER-SDC                                         
081500                                                                          
081600     PERFORM IMS-GET-SART-SEG                                             
081700     IF SEGMENT-FINNS                                                     
081800        PERFORM IMS-GET-SLAG-SEG                                          
081900        PERFORM UNTIL SEGMENT-SAKNAS                                      
082000          MOVE SLAG-IDDC     TO W-IDDC-B6                                 
082100          PERFORM IMS-GU-WDB601                                           
082200          IF DCS-SDC                                                      
082300            ADD SLAG-KVLS            TO   W-SDC-KVLS                      
082400            ADD SLAG-KVAKS-SDC       TO   W-SDC-KVAKS-SDC                 
082500            ADD SLAG-KVAKS-PAV       TO   W-SDC-KVAKS-PAV                 
082600            ADD SLAG-KVPB-REF        TO   W-SDC-KVPB                      
082700            ADD SLAG-KVOKS-BULK      TO   W-SDC-KVOKS                     
082800            ADD SLAG-KVOKS-DAG       TO   W-SDC-KVOKS                     
082900                                                                          
083000            ADD SLAG-KVEFRS          TO   W-SDC-KVEFRS                    
083100            ADD SLAG-KVUTRS          TO   W-SDC-KVUTRS                    
083200                                                                          
083300            IF DCS-FLOVRLAGBER = NEJ                                      
083400              CONTINUE                                                    
083500            ELSE                                                          
083600              MOVE ZERO              TO   W-TILLG-SDC                     
083700              ADD SLAG-KVLS          TO   W-TILLG-SDC                     
083800              ADD SLAG-KVBEART       TO   W-TILLG-SDC                     
083900              ADD SLAG-KVAKS-SDC     TO   W-TILLG-SDC                     
084000              ADD SLAG-KVAKS-PAV     TO   W-TILLG-SDC                     
084100              SUBTRACT SLAG-KVOKS-BULK FROM W-TILLG-SDC                   
084200              SUBTRACT SLAG-KVOKS-DAG FROM W-TILLG-SDC                    
084300              IF SLAG-KVREFOVL < W-TILLG-SDC                              
084400                 COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC                
084500                                         + W-TILLG-SDC                    
084600                                         - SLAG-KVREFOVL                  
084700              END-IF                                                      
084800            END-IF                                                        
084900            MOVE ART-TIFINLV   TO TIFINLV-AAVVD                           
085000            MOVE DAGENS-AA     TO TMP1-YY                                 
085100            MOVE TIFINLV-AA    TO TMP2-YY                                 
085200            PERFORM WY2000P9                                              
085300            COMPUTE VECKO-SKILLNAD = (TMP1-YY - TMP2-YY) * 52             
085400                                   + DAGENS-VV - TIFINLV-VV               
085500            IF VECKO-SKILLNAD < 52                                        
085600               MOVE ZERO TO W-OVERLAGER-SDC                               
085700            END-IF                                                        
085800          ELSE                                                            
086120            IF DCS-NDC                                                    
086300              ADD SLAG-KVLS          TO W-NDC-KVLS                        
086400              ADD SLAG-KVRESS        TO W-NDC-KVRESS                      
086500              ADD SLAG-KVAKS-SDC     TO W-NDC-KVAKS-NDC                   
086600              ADD SLAG-KVAKS-PAV     TO W-NDC-KVAKS-PAV                   
086700              IF SLAG-IDLEVNR = '1441'                                    
086800                 OR SLAG-IDLEVNR = 'BP2TW'                                
086900                  COMPUTE W-NDC-KVPB = W-NDC-KVPB    +                    
087000                                       SLAG-KVPB-REF +                    
087100                                       SLAG-KVPBREOI                      
087200              END-IF                                                      
087300              ADD SLAG-KVOKS-BULK    TO W-NDC-KVOKS                       
087400              ADD SLAG-KVOKS-DAG     TO W-NDC-KVOKS                       
087500              ADD SLAG-KVROS-BULK    TO W-NDC-KVROS                       
087600              ADD SLAG-KVROS-DAG     TO W-NDC-KVROS                       
087700              ADD SLAG-KVEFRS        TO W-NDC-KVEFRS                      
087800              ADD SLAG-KVUTRS        TO W-NDC-KVUTRS                      
087900            END-IF                                                        
088000          END-IF                                                          
088100          PERFORM IMS-GET-SLAG-SEG                                        
088200        END-PERFORM                                                       
088300        COMPUTE W-SDC-KVPB ROUNDED =  W-SDC-KVPB                          
088400                                                                          
088500        COMPUTE W-NDC-KVPB ROUNDED =  W-NDC-KVPB                          
088600                                                                          
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 D-RED-BILD-FRAN-WDD3 SECTION.                                            
089100                                                                          
089200     IF MSGI-IDLAND-SPR = 'GB'                                            
089300        MOVE 'GB' TO W-IDSKYLT                                            
089400     ELSE                                                                 
089500        MOVE 'S'  TO W-IDSKYLT                                            
089600     END-IF                                                               
089700                                                                          
089800     PERFORM IMS-GET-BENA11-BSEQ                                          
089900     MOVE BENA-TEXT-BEART     TO MOD-BEART-SVE                            
090000     .                                                                    
090100     EJECT                                                                
090200 E-RED-BILD-FRAN-WDD8 SECTION.                                            
090300                                                                          
090400     MOVE ZERO                      TO MOD-KVBUFF                         
090500                                       WS-KVBUFF                          
090600     PERFORM IMS-GET-WDD801                                               
090700     IF SEGMENT-FINNS                                                     
090800        MOVE '11'                   TO W-IDDC                             
090900        MOVE 17                     TO W-ADBUFFOMR                        
091000        PERFORM IMS-GET-WDD811                                            
091100        PERFORM UNTIL SEGMENT-SAKNAS                                      
091200           ADD ARTD-SALDO-KVBUFF-F  TO WS-KVBUFF                          
091300           ADD ARTD-SALDO-KVBUFF-OF TO WS-KVBUFF                          
091400           PERFORM IMS-GET-WDD811                                         
091500        END-PERFORM                                                       
091600     END-IF                                                               
091700                                                                          
091800     IF WS-ADLAGOMR = 71 AND WS-ADPLATS = 7000                            
091900        ADD W-CDC-KVLS              TO WS-KVBUFF                          
092000     END-IF                                                               
092100                                                                          
092200     MOVE WS-KVBUFF                 TO MOD-KVBUFF                         
092300     .                                                                    
092400     EJECT                                                                
092500 F-KOLLA-OM-LARM-FINNS SECTION.                                           
092600                                                                          
092700     IF W-IDANSK-L > ZERO                                                 
092800       PERFORM IMS-GET-XXBX-2231                                          
092900       PERFORM IMS-GET-XXBX-2232                                          
093000       IF SEGMENT-FINNS                                                   
093100         MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK                           
093200         PERFORM IMS-GET-XXBU-2223                                        
093300         IF SEGMENT-FINNS                                                 
093400           MOVE WC-CDC-SE          TO W-IDDC-2224                         
093500           PERFORM IMS-GET-XXBU-2224-FLNYLARM                             
093600           IF SEGMENT-FINNS                                               
093700             IF WS-FLGEMART = JA                                          
093800                MOVE MED-1 (SPIND) TO MOD-TEMFSINF                        
093900             END-IF                                                       
094000           END-IF                                                         
094100         END-IF                                                           
094200       END-IF                                                             
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 G-RED-BILD-FRAN-W6D1 SECTION.                                            
094700                                                                          
094800     MOVE IDARTNR-WS TO WS-IDARTNR                                        
094900     MOVE WS-IDARTNR TO W-IDARTNR-HSEQ                                    
095000     MOVE ZERO       TO W-KVART-TOT-C1                                    
095100     PERFORM IMS-GN-INLA11-W6D1SEQ                                        
095200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
095300       MOVE INLA-ART-IDDC TO W-IDDC-B6                                    
095400       PERFORM IMS-GU-WDB601                                              
095500       IF (DCS-CDC                                                        
095600       OR  DCS-CDC-TR)                                                    
095700       AND INLA-ART-IDLOPNRM  = ZERO                                      
095800         MOVE INLA-ART-KVAVIS TO WS-KVAVIS                                
095900         IF INLA-ART-FLFEL = NEJ                                          
096000           ADD WS-KVAVIS TO W-KVART-TOT-C1                                
096100         END-IF                                                           
096200       END-IF                                                             
096300       PERFORM IMS-GN-INLA11-W6D1SEQ                                      
096400     END-PERFORM                                                          
096500     MOVE W-KVART-TOT-C1 TO MOD-KVART-FORAVIS (1)                         
096600     .                                                                    
096700     EJECT                                                                
096800 H-RED-BILD-FRAN-WDA9 SECTION.                                            
096900                                                                          
097000     IF BYT02-RENOV                                                       
097100       IF BYT16-BYTES                                                     
097200         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
097300                                  6000                                    
097400         END-COMPUTE                                                      
097500       ELSE                                                               
097600         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
097700                                  1000                                    
097800         END-COMPUTE                                                      
097900       END-IF                                                             
098000       PERFORM IMS-GU-WDA901                                              
098100       IF SEGMENT-FINNS                                                   
098200         MOVE W-IDARTNR TO W-IDARTNR-BYART                                
098300         PERFORM DB2-SELECT-BYART                                         
098400         IF RADER-FINNS                                                   
098500           MOVE BYART-IDDISTR-RENOV TO W-IDDISTR-WDA9                     
098600           PERFORM IMS-GNP-WDA911                                         
098700           IF SEGMENT-FINNS                                               
098800* FIX SOM UTÖKAS FÖR VARJE RENOVÖR SOM ANSLUTS TILL WEB:EN                
098900             MOVE UPD-IDDISTR   TO TEST-IDDISTR                           
099000             IF DIS134-BYTESREN-WEB                                       
099100               ADD UPD-KVLS-REM TO WS-KVLS-REM                            
099200             END-IF                                                       
099300           END-IF                                                         
099400         END-IF                                                           
099500       END-IF                                                             
099600     END-IF                                                               
099700     MOVE WS-KVLS-REM           TO MOD-KVLS-REM                           
099800     .                                                                    
099900     EJECT                                                                
100000 S01-KONV-TIDISPIN SECTION.                                               
100100                                                                          
100200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
100300     MOVE CLAG-TIDISPIN TO DAT-I-TIDATUM                                  
100400     CALL WDATKONV USING DAT-KDDATFORM                                    
100500                         DAT-I-TIDATUM                                    
100600                         DAT-O-TIDATUM                                    
100700                         DAT-KDSVAR                                       
100800     IF DAT-KDSVAR-OK                                                     
100900       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
101000       MOVE DAT-TIVV       TO W-TIVV                                      
101100       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
101200       MOVE W-TIAAAAVV     TO W-DADISPIN                                  
101300     ELSE                                                                 
101400       MOVE ZERO           TO W-DADISPIN                                  
101500     END-IF                                                               
101600     .                                                                    
101700     EJECT                                                                
101800 MFS-RENSA-UTDATA-FAELT SECTION.                                          
101900                                                                          
102000     MOVE 1 TO IX                                                         
102100     PERFORM UNTIL IX > 3                                                 
102200                                                                          
102300         MOVE MFS-RENSA-FAELT TO  MOD-KVLS      (IX)                      
102400                              MOD-KVRESS        (IX)                      
102500                              MOD-KVOKS         (IX)                      
102600                              MOD-DISP          (IX)                      
102700                              MOD-KVART-FORAVIS (IX)                      
102800                              MOD-KVAKS-LAGER   (IX)                      
102900                              MOD-KVAKS-PAV     (IX)                      
103000                              MOD-KVAKS-T       (IX)                      
103100                              MOD-SUTPO-TOT     (IX)                      
103200                              MOD-KVROS         (IX)                      
103300                              MOD-KDERS         (IX)                      
103400                              MOD-KVMP          (IX)                      
103500                              MOD-KVPB-SEP      (IX)                      
103600                              MOD-KVPB-SATS     (IX)                      
103700                              MOD-KVSLAGER      (IX)                      
103800                              MOD-KVSPANT       (IX)                      
103900                              MOD-RESLJUST      (IX)                      
104000                              MOD-ARB-SALDO     (IX)                      
104100                              MOD-KVUTRS        (IX)                      
104200          ADD 1 TO IX                                                     
104300     END-PERFORM                                                          
104400     .                                                                    
104500     EJECT                                                                
104600* IMS SEKTIONER                                                           
104700                                                                          
104800 IMS-GET-MSG SECTION.                                                     
104900     MOVE '  QC' TO GODK-STATUSKODER                                      
105000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     SKIP3                                                                
105500 IMS-INSERT-MSG SECTION.                                                  
105600     IF MSGI-IDLAND-SPR = 'GB'                                            
105700        MOVE 'N' TO MFS-KDHUVOMR                                          
105800     END-IF                                                               
105900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
106000     MOVE SPACE TO GODK-STATUSKODER                                       
106100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106300     PERFORM IMS-STATUSKONTROLL                                           
106400     .                                                                    
106500     EJECT                                                                
106600 IMS-GET-BENA11-BSEQ SECTION.                                             
106700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
106800*    --FYS = WDD301                                                       
106900            DELIMITED BY SIZE INTO SSA1                                   
107000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
107100*    --FYS = WDD311                                                       
107200            DELIMITED BY SIZE INTO SSA2                                   
107300     MOVE '  ' TO GODK-STATUSKODER                                        
107400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
107500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     EJECT                                                                
107900 IMS-GET-ART-SEG SECTION.                                                 
108000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
108100*    --FYS = WDK601                                                       
108200            DELIMITED BY SIZE INTO SSA1                                   
108300     MOVE '  GE' TO GODK-STATUSKODER                                      
108400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
108500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     .                                                                    
108800     SKIP3                                                                
108900 IMS-GET-CLAG-SEG SECTION.                                                
109000     MOVE   'WLARTC11' TO SSA1                                            
109100*    --FYS = WDK611                                                       
109200     MOVE '  GE' TO GODK-STATUSKODER                                      
109300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
109400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     .                                                                    
109700     EJECT                                                                
109800 IMS-GET-PRL-SEG  SECTION.                                                
109900     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
110000*    --FYS = WDK621                                                       
110100            DELIMITED BY SIZE INTO SSA1                                   
110200     MOVE '  GE' TO GODK-STATUSKODER                                      
110300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-21 SSA1                  
110400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     .                                                                    
110700     EJECT                                                                
110800 IMS-GET-SART-SEG SECTION.                                                
110900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
111000*    --FYS = WDK701                                                       
111100            DELIMITED BY SIZE INTO SSA1                                   
111200     MOVE '  GE' TO GODK-STATUSKODER                                      
111300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
111400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
111500     PERFORM IMS-STATUSKONTROLL                                           
111600     .                                                                    
111700     SKIP3                                                                
111800 IMS-GET-SLAG-SEG SECTION.                                                
111900     MOVE   'WLARTS11' TO SSA1                                            
112000*    --FYS = WDK711                                                       
112100     MOVE '  GE' TO GODK-STATUSKODER                                      
112200     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
112300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     EJECT                                                                
112700 IMS-GET-INLB01 SECTION.                                                  
112800     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
112900*    --FYS = WDD901                                                       
113000            DELIMITED BY SIZE INTO SSA1                                   
113100     MOVE '  GE' TO GODK-STATUSKODER                                      
113200     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
113300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
113400     PERFORM IMS-STATUSKONTROLL                                           
113500     .                                                                    
113600     SKIP3                                                                
113700 IMS-GET-LEVERANTOER-SEG SECTION.                                         
113800     MOVE   'WLINLB11' TO SSA1                                            
113900*    --FYS = WDD902                                                       
114000     MOVE '  GE' TO GODK-STATUSKODER                                      
114100     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
114200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     .                                                                    
114500     EJECT                                                                
114600 IMS-GET-ARTM01         SECTION.                                          
114700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
114800*    --FYS = WDK901                                                       
114900            DELIMITED BY SIZE INTO SSA1                                   
115000     MOVE '  GE' TO GODK-STATUSKODER                                      
115100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
115200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500     SKIP3                                                                
115600 IMS-GET-ARTM11         SECTION.                                          
115700     STRING 'WLARTM11(DABEHOV >=' W-DABEHOV-MIN-X                         
115800*    --FYS = WDK911                                                       
115900                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
116000            DELIMITED BY SIZE INTO SSA1                                   
116100     MOVE '  GE' TO GODK-STATUSKODER                                      
116200     CALL CBLTDLI USING GNP ARTM-PCB DLI-IO-AREA2 SSA1                    
116300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600     EJECT                                                                
116700 IMS-GET-WDD801         SECTION.                                          
116800     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
116900            DELIMITED BY SIZE INTO SSA1                                   
117000     MOVE '  GE' TO GODK-STATUSKODER                                      
117100     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-AREA3 SSA1                     
117200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     SKIP3                                                                
117600 IMS-GET-WDD811         SECTION.                                          
117700     STRING 'WDD811  (IDDC     =' W-IDDC-X                                
117800                    '&ADBUFFOM =' W-ADBUFFOMR-X                           
117900                    '&ADBUFGAN =' W-ADBUFFGANG-X   ')'                    
118000            DELIMITED BY SIZE INTO SSA1                                   
118100     MOVE '  GE' TO GODK-STATUSKODER                                      
118200     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-AREA3 SSA1                    
118300     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
118400     PERFORM IMS-STATUSKONTROLL                                           
118500     .                                                                    
118600     EJECT                                                                
118700 IMS-GET-XXBX-2231      SECTION.                                          
118800     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
118900*    --FYS = WDR201                                                       
119000            DELIMITED BY SIZE INTO SSA1                                   
119100     MOVE '  ' TO GODK-STATUSKODER                                        
119200     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA SSA1                      
119300     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
119400     PERFORM IMS-STATUSKONTROLL                                           
119500     .                                                                    
119600     SKIP3                                                                
119700 IMS-GET-XXBX-2232      SECTION.                                          
119800     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
119900*    --FYS = WDR220                                                       
120000            DELIMITED BY SIZE INTO SSA1                                   
120100     MOVE '  GE' TO GODK-STATUSKODER                                      
120200     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA SSA1                     
120300     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
120400     PERFORM IMS-STATUSKONTROLL                                           
120500     .                                                                    
120600     EJECT                                                                
120700 IMS-GET-XXBU-2223      SECTION.                                          
120800     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
120900*    --FYS = WDR501                                                       
121000            DELIMITED BY SIZE INTO SSA1                                   
121100     MOVE '  GE' TO GODK-STATUSKODER                                      
121200     CALL CBLTDLI USING GU  XXBU-PCB DLI-IO-AREA SSA1                     
121300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     .                                                                    
121600     SKIP3                                                                
121700 IMS-GET-XXBU-2224-FLNYLARM SECTION.                                      
121800     STRING 'WLXXBU11(FLNYLARM =' W-FLNYLARM-X                            
121900                    '&IDDC     =' W-IDDC-2224-X ')'                       
122000*    --FYS = WDR550                                                       
122100            DELIMITED BY SIZE INTO SSA1                                   
122200     MOVE '  GE' TO GODK-STATUSKODER                                      
122300     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
122400     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     EJECT                                                                
122800 IMS-GN-INLA11-W6D1SEQ SECTION.                                           
122900     STRING 'W6INLA11(W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
123000*    --FYS = W6D111                                                       
123100            DELIMITED BY SIZE INTO SSA1                                   
123200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123300     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA SSA1                      
123400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
123500     PERFORM IMS-STATUSKONTROLL                                           
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 IMS-GU-WDA901 SECTION.                                                   
124000     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
124100          DELIMITED BY SIZE INTO SSA1                                     
124200     MOVE '  GE'           TO GODK-STATUSKODER                            
124300     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
124400     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     .                                                                    
124700                                                                          
124800 IMS-GNP-WDA911 SECTION.                                                  
124900     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-WDA9-X ')'                    
125000          DELIMITED BY SIZE INTO SSA1                                     
125100     MOVE '  GE'           TO GODK-STATUSKODER                            
125200     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
125300     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     .                                                                    
125600     EJECT                                                                
125700                                                                          
125800 IMS-GU-WDB601    SECTION.                                                
125900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
126000          DELIMITED BY SIZE INTO SSA1                                     
126100     MOVE '  ' TO GODK-STATUSKODER                                        
126200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
126300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
126400     PERFORM IMS-STATUSKONTROLL                                           
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800 IMS-STATUSKONTROLL SECTION.                                              
126900     SET STATUS-IX TO 1                                                   
127000     SEARCH GODK-STATUS                                                   
127100       AT END                                                             
127200         CALL FELLOG                                                      
127300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127400         CONTINUE                                                         
127500     END-SEARCH                                                           
127600     .                                                                    
127700     EJECT                                                                
127800*    -COPY WY2000P9                                                       
127900     EJECT                                                                
128000                                                                          
128100 DB2-SELECT-BYART SECTION.                                                
128200                                                                          
128300     MOVE 000100           TO GODK-SQLCODEKODER                           
128400     EXEC SQL                                                             
128500         SELECT IDDISTR_RENOV                                             
128600         INTO :BYART-IDDISTR-RENOV                                        
128700         FROM BYART                                                       
128800         WHERE IDARTNR_BYT = :W-IDARTNR-BYART                             
128900     END-EXEC                                                             
129000     MOVE SQLCODE          TO SQLCODE-WS                                  
129100     PERFORM DB2-STATUSKONTROLL                                           
129200     .                                                                    
129300     EJECT                                                                
129400 DB2-STATUSKONTROLL  SECTION.                                             
129500                                                                          
129600     SET SQLCODE-IX TO 1                                                  
129700     SEARCH GODK-SQLCODE                                                  
129800       AT END CALL FELLOG                                                 
129900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
130000     END-SEARCH                                                           
130100     .                                                                    
