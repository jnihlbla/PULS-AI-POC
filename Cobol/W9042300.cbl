000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                W9042300.                                     
000300 AUTHOR.                    IDK, GÖTEBORG.                                
000400 DATE-WRITTEN.              APRIL -79.                                    
000500 DATE-COMPILED.                                                           
000600*    REMARKS.                                                             
000700*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000800*                'SALDO-INFO ANSKAFFNING'                                 
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W90423T                                             
001200*        MID:         W90423I1                                            
001300*    UTDATA.                                                              
001400*        MOD:         W90423O1                                            
001500*    SUBPROGRAM.                                                          
001600*        FELLOG                                                           
001700*                                                                         
001800*                                                                         
001900*   ÄNDRINGAR:                                                            
002000*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002100*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002200*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002300*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002301*                                                                         
002310*        07-10-11. TILLAGT MOD-KDAVT ENL. ETRACKER 3408639 /C.E.          
002400*                                                                         
002401*      2008-01-03. E'TRACKER 4820410.                                     
002402*                  DO NOTE INCLUDE OVERSTOCK AT MICRO-LDC                 
002403*                  (FÖRBEREDD KOD LIKA 2102) /C.E.                        
002410*                                                                         
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200*    -COPY WY2000W9                                                       
003300     SKIP3                                                                
003400 77  IDPGM               PIC X(8)    VALUE 'W9042300'.                    
003500 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003600 77      JA              PIC X       VALUE 'J'.                           
003700 77      NEJ             PIC X       VALUE 'N'.                           
003800 77      STRECK          PIC X       VALUE '-'.                           
003900 77      SPIND           PIC S9(9)   VALUE +0   COMP SYNC.                
004000 77      VIP-ARTIKEL     PIC X(1)    VALUE 'V'.                           
004100     SKIP3                                                                
004200 01  W-IDARTNR-X.                                                         
004300     03  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
004400 01  W-W6D1HSEQ-X.                                                        
004500     03  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
004600 01  W-KDCLAGER-X.                                                        
004700     03  W-KDCLAGER      PIC S9(1)   VALUE +1    COMP-3.                  
004800 01  W-IDSKYLT-X.                                                         
004900     03  W-IDSKYLT       PIC  X(3)   VALUE 'S  '.                         
005000 01  W-DABEHOV-MIN-X.                                                     
005100     03  W-DABEHOV-MIN   PIC   9(6)  VALUE ZERO.                          
005200 01  W-DABEHOV-MAX-X.                                                     
005300     03  W-DABEHOV-MAX   PIC   9(6)  VALUE ZERO.                          
005400 01  W-WDGXKEY-2231-X.                                                    
005500     03  FILLER          PIC  X(4)   VALUE '2231'.                        
005600     03  FILLER          PIC  X(26)  VALUE LOW-VALUE.                     
005700 01  W-WDGXKEY-2232-X.                                                    
005800     03  W-IDANSK-L      PIC  S9(3)  VALUE ZERO COMP-3.                   
005900     03  FILLER          PIC  X(3)   VALUE LOW-VALUE.                     
006000 01  W-WDGXKEY-2223-X.                                                    
006100     03  W-IDHTYP        PIC  X(4)   VALUE '2223'.                        
006200     03  W-IDANSK        PIC  S9(3)  VALUE ZERO COMP-3.                   
006300     03  FILLER          PIC  X(24)  VALUE LOW-VALUE.                     
006400 01  W-FLNYLARM-X.                                                        
006500     03  W-FLNYLARM      PIC  X      VALUE 'J'.                           
006600 01  NYCKLAR-WDD8.                                                        
006700     03  W-IDDC-X.                                                        
006800         05  W-IDDC      PIC X(2)    VALUE SPACE.                         
006900     03  W-ADBUFFOMR-X.                                                   
007000         05  W-ADBUFFOMR PIC S9(3)   VALUE ZERO  COMP-3.                  
007100     03  W-ADBUFFGANG-X.                                                  
007200         05  W-ADBUFFGANG PIC S9(3)  VALUE ZERO  COMP-3.                  
007300 01  W-IDDC-B6-X.                                                         
007400     03 W-IDDC-B6         PIC X(2).                                       
007500 01  W-IDARTNR-WDA9-X.                                                    
007600     03  W-IDARTNR-WDA9  PIC S9(9)   VALUE ZERO  COMP-3.                  
007800     SKIP3                                                                
007900 01      SWITCHAR.                                                        
008000   03    SW-NYCKLAR-OK   PIC X       VALUE 'N'.                           
008100   03    SW-ARTM-SEGM-FINNS PIC X    VALUE 'N'.                           
008200     88  ARTM-SEGMENT-FINNS          VALUE 'J'.                           
008300     SKIP3                                                                
008400 01  W.                                                                   
008500     03  IX                  PIC S9(9)               COMP SYNC.           
008600     SKIP1                                                                
008700     03  WS-KVLS-REM         PIC S9(7)    VALUE ZERO COMP-3.              
008800     03  W-KVAKS             PIC S9(7)    VALUE ZERO COMP-3.              
008900     03  WS-KVAVIS           PIC S9(7)    VALUE ZERO COMP-3.              
009000     03  WS-KVBUFF           PIC S9(7)    VALUE ZERO COMP-3.              
009100     03  WS-ADLAGOMR         PIC S9(3)    VALUE ZERO COMP-3.              
009200     03  WS-ADPLATS          PIC S9(5)    VALUE ZERO COMP-3.              
009300     03  W-KVBR-TOT          PIC S9(7)    VALUE ZERO COMP-3.              
009400     03  W-KVBR-OVR          PIC S9(7)    VALUE ZERO COMP-3.              
009500     03  W-TIINVDAT          PIC S9(5)    VALUE ZERO COMP-3.              
009600     03  W-HUVUDIDLEVNR      PIC X(5)     VALUE SPACE.                    
009700     03  W-TIAVIDAT-SEN      PIC S9(7)    VALUE ZERO COMP-3.              
009800     03  W-DISPONIBELT       PIC S9(7)    VALUE ZERO COMP-3.              
009900     03  W-ARB-SALDO         PIC S9(7)    VALUE ZERO COMP-3.              
010000     03  W-CDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
010100     03  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
010200     03  W-NDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
010300     03  W-SDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
010400     03  W-NDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
010500     03  W-NDC-KVROS         PIC S9(7)    VALUE ZERO COMP-3.              
010600     03  W-SDC-KVAKS-SDC     PIC S9(7)    VALUE ZERO COMP-3.              
010700     03  W-NDC-KVAKS-NDC     PIC S9(7)    VALUE ZERO COMP-3.              
010800     03  W-SDC-KVAKS-PAV     PIC S9(7)    VALUE ZERO COMP-3.              
010900     03  W-NDC-KVAKS-PAV     PIC S9(7)    VALUE ZERO COMP-3.              
011000     03  W-SDC-KVPB          PIC S9(6)V9  VALUE ZERO COMP-3.              
011100     03  W-NDC-KVPB          PIC S9(6)V9  VALUE ZERO COMP-3.              
011200     03  W-SDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.              
011300     03  W-NDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.              
011400     03  W-SDC-KVEFRS        PIC S9(7)    VALUE ZERO COMP-3.              
011500     03  W-NDC-KVEFRS        PIC S9(7)    VALUE ZERO COMP-3.              
011600     03  W-SDC-KVUTRS        PIC S9(7)    VALUE ZERO COMP-3.              
011700     03  W-NDC-KVUTRS        PIC S9(7)    VALUE ZERO COMP-3.              
011800     03  W-TILLG-SDC         PIC S9(7)               COMP-3.              
011900     03  W-OVERLAGER-SDC     PIC S9(7)               COMP-3.              
012000     03  W-KVART             PIC S9(7)    VALUE ZERO COMP-3.              
012100     03  W-KVART-TOT-C1      PIC S9(9)    VALUE ZERO COMP-3.              
012200     03  WS-IDARTNR          PIC  9(8)    VALUE ZERO.                     
012300     03  WS-FLGEMART         PIC  X(1)    VALUE 'N'.                      
012400     03  W-TIAAAAVV.                                                      
012500       05 W-TISEKEL          PIC  9(2)    VALUE ZERO.                     
012600       05 W-TIAA             PIC  9(2)    VALUE ZERO.                     
012700       05 W-TIVV             PIC  9(2)    VALUE ZERO.                     
012800     03  W-DADISPIN          PIC  9(6)    VALUE ZERO.                     
012900     03  WS-TPO-NAESTA-INLEV PIC S9(9)    VALUE ZERO COMP-3.              
013400     03  WS-KVOKS-TOT     PIC S9(9) OCCURS 3 VALUE ZERO COMP-3.           
013500     03  WS-SUTPO-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
013600     03  WS-IDLEVNR-8        PIC  X(8)    VALUE SPACE.                    
013700 01  DATUM-FAELT.                                                         
013800     03  DAGENS-DATUM        PIC 9(6)     VALUE ZERO.                     
013900     03  DAGENS-AAVV         PIC 9(4)     VALUE ZERO.                     
014000     03  FILLER REDEFINES DAGENS-AAVV.                                    
014100         05  DAGENS-AA       PIC 9(2).                                    
014200         05  DAGENS-VV       PIC 9(2).                                    
014300     03  TIFINLV-AAVVD       PIC 9(5)     VALUE ZERO.                     
014400     03  FILLER REDEFINES TIFINLV-AAVVD.                                  
014500         05  TIFINLV-AA      PIC 9(2).                                    
014600         05  TIFINLV-VV      PIC 9(2).                                    
014700         05  FILLER          PIC 9(1).                                    
014800     03  VECKO-SKILLNAD      PIC S9(5) VALUE ZERO  COMP-3.                
014900     EJECT                                                                
015000                                                                          
015100 01      MEDDELANDE.                                                      
015200                                                                          
015300* FEL-MEDDELANDEN                                                         
015400   03 W-FEL-1.                                                            
015500     05 FILLER  PIC X(26) VALUE 'ARTIKELNUMMER EJ NUMERISKT'.             
015600     05 FILLER  PIC X(26) VALUE 'PART NO. NOT NUMERIC      '.             
015700   03 FILLER REDEFINES W-FEL-1.                                           
015800     05 FEL-1   PIC X(26) OCCURS 2.                                       
015900                                                                          
016000   03 W-FEL-2.                                                            
016100     05 FILLER  PIC X(35) VALUE 'ARTIKEL SAKNAS I DATABAS'.               
016200     05 FILLER  PIC X(35) VALUE 'PART NO. MISSING IN DATA BASE'.          
016300   03 FILLER REDEFINES W-FEL-2.                                           
016400     05 FEL-2   PIC X(35) OCCURS 2.                                       
016500                                                                          
016600   03 W-FEL-3.                                                            
016700     05 FILLER  PIC X(15) VALUE 'ARTIKELN ERSATT'.                        
016800     05 FILLER  PIC X(15) VALUE 'PART REPLACED  '.                        
016900   03 FILLER REDEFINES W-FEL-3.                                           
017000     05 FEL-3   PIC X(15) OCCURS 2.                                       
017100                                                                          
017200   03 W-FEL-4.                                                            
017300     05 FILLER  PIC X(19) VALUE 'ARTIKELN ÄR DELETAD'.                    
017400     05 FILLER  PIC X(19) VALUE 'PART IS DELETED    '.                    
017500   03 FILLER REDEFINES W-FEL-4.                                           
017600     05 FEL-4   PIC X(19) OCCURS 2.                                       
017700                                                                          
017800   03 W-FEL-5.                                                            
017900     05 FILLER  PIC X(19) VALUE 'ERSÄTTANDE ARTIKEL'.                     
018000     05 FILLER  PIC X(19) VALUE 'REPLACING PART NO.'.                     
018100   03 FILLER REDEFINES W-FEL-5.                                           
018200     05 FEL-5   PIC X(19) OCCURS 2.                                       
018300                                                                          
018400   03 W-FEL-6.                                                            
018500     05 FILLER  PIC X(19) VALUE 'ARTIKELN UTGÅR'.                         
018600     05 FILLER  PIC X(19) VALUE 'PART EXPIRES  '.                         
018700   03 FILLER REDEFINES W-FEL-6.                                           
018800     05 FEL-6   PIC X(19) OCCURS 2.                                       
018900                                                                          
019000   03 W-FEL-7.                                                            
019100     05 FILLER  PIC X(19) VALUE 'ARTIKELN HAR UTGÅTT'.                    
019200     05 FILLER  PIC X(19) VALUE 'PART HAS EXPIRED   '.                    
019300   03 FILLER REDEFINES W-FEL-7.                                           
019400     05 FEL-7   PIC X(19) OCCURS 2.                                       
019500                                                                          
019600   03 W-FEL-8.                                                            
019700     05 FILLER  PIC X(19) VALUE 'OBEHÖRIG ANVÄNDARE '.                    
019800     05 FILLER  PIC X(19) VALUE 'USER NOT AUTHORIZED'.                    
019900   03 FILLER REDEFINES W-FEL-8.                                           
020000     05 FEL-8   PIC X(19) OCCURS 2.                                       
020100                                                                          
020200* INFO-MEDDELANDEN                                                        
020300   03 W-MED-1.                                                            
020400     05 FILLER  PIC X(20) VALUE 'SE LARM          '.                      
020500     05 FILLER  PIC X(20) VALUE 'INSPECT ALARM CUE'.                      
020600   03 FILLER REDEFINES W-MED-1.                                           
020700     05 MED-1   PIC X(20) OCCURS 2.                                       
020800                                                                          
020900   03 W-MED-2.                                                            
021000     05 FILLER  PIC X(20) VALUE 'GEMENSAM PV/LV'.                         
021100     05 FILLER  PIC X(20) VALUE 'COMMON VCC/VTC'.                         
021200   03 FILLER REDEFINES W-MED-2.                                           
021300     05 MED-2   PIC X(20) OCCURS 2.                                       
021400                                                                          
021500   03 W-MED-3.                                                            
021600     05 FILLER  PIC X(31) VALUE 'SE LARM      GEMENSAM PV/LV    '.        
021700     05 FILLER  PIC X(31) VALUE 'INSP. ALARM CUE, COMMON VCC/VTC'.        
021800   03 FILLER REDEFINES W-MED-3.                                           
021900     05 MED-3   PIC X(31) OCCURS 2.                                       
022000                                                                          
022100     EJECT                                                                
022200*      --- VALID IDDC CODES                                               
022300*                                                                         
022400*01    -COPY WWDCKONS                                                     
022500     EJECT                                                                
022600                                                                          
022700 01  FILLER              PIC  X(16)  VALUE 'BYTES-DIST'.                  
022800 01  TEST-IDDISTR        PIC  9(5)   COMP-3.                              
022900*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
023000     EJECT                                                                
023100                                                                          
023200 01  FILLER              PIC  X(16)  VALUE 'BYTES-ART '.                  
023300 01  TEST-IDARTNR        PIC  9(9)   COMP-3 VALUE ZERO.                   
023400*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
023500     EJECT                                                                
023600*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
023700     EJECT                                                                
023800*                        ****    DYNAMISKA SUBPROGRAM                     
023900 01      DYNAMISKA-SUBPROGRAM.                                            
024000   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
024100   03    WDATKONV        PIC X(8)    VALUE 'WDATKONV'.                    
024200   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
024300   03    W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
024400*                        ****    PARAMETRAR TILL WDATKONV                 
024500*01      -COPY WDATAREA                                                   
024600*                        ****    PARAMETRAR TILL W005INIT                 
024700*01      -COPY WMSGINIT                                                   
024800*                        ****    TP-AREOR                                 
024900 01      TP-WS.                                                           
025000   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
025100     SKIP3                                                                
025200*01      MID -COPY W90423I1 -PRE MID-.                                    
025300     EJECT                                                                
025400*01      -COPY WMSGAREA                                                   
025500     EJECT                                                                
025600*  03    MOD -COPY W90423O1 -PRE MOD- -RED MSG-AREA.                      
025700     EJECT                                                                
025800*01  -COPY WMFSAREA.                                                      
025900     EJECT                                                                
026000******************************************************************        
026100*****                                                                     
026200*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026300*****                                                                     
026400 01  IMS-WS.                                                              
026500   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
026600     SKIP3                                                                
026700*****                    **** STATUS-KOD FRÅN IMS                         
026800   03    STATUS-WS       PIC XX.                                          
026900         88  SEGMENT-FINNS       VALUE '  '.                              
027000         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
027100         88  SEGMENT-SLUT        VALUE 'GB'.                              
027200     SKIP3                                                                
027300   03    GODK-STATUSKODER.                                                
027400     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
027500     SKIP3                                                                
027600 01      SSA1            PIC X(64).                                       
027700 01      SSA2            PIC X(64).                                       
027800 01      SSA3            PIC X(64).                                       
027900     EJECT                                                                
028000*                            IMS FUNKTIONSKODER                           
028100*01      -COPY W0003                                                      
028200     EJECT                                                                
028300*                            DLI INPUT-OUTPUT AREA                        
028400 01  DLI-IO-AREA-01.                                                      
028500*03  WLARTC01 -COPY WDK601                                                
028600     EJECT                                                                
028700 01  DLI-IO-AREA-11.                                                      
028800*03  WLARTC11 -COPY WDK611                                                
028900     EJECT                                                                
029000 01  FILLER.                                                              
029100 03  DLI-IO-AREA     PIC X(300)  VALUE SPACE.                             
029200                                                                          
029300*03  WLARTS01 -COPY WDK701   -RED DLI-IO-AREA.                            
029400     EJECT                                                                
029500*03  WLARTS11 -COPY WDK711   -RED DLI-IO-AREA.                            
029600     EJECT                                                                
029900*03  WLXXBU01 -COPY WDGX2223 -PRE XXBU-   -RED DLI-IO-AREA.               
030000     EJECT                                                                
030100*03  WLXXBU11 -COPY WDGX2224 -PRE XXBU-   -RED DLI-IO-AREA.               
030200     EJECT                                                                
030300*03  WLXXBX01 -COPY WDGX01   -PRE XXBX-   -RED DLI-IO-AREA.               
030400     EJECT                                                                
030500*03  WLXXBX11 -COPY WDGX2232 -PRE XXBX-   -RED DLI-IO-AREA.               
030600     EJECT                                                                
030700*03  W6INLA11 -COPY W6D111   -PRE INLA-   -RED DLI-IO-AREA.               
030800     EJECT                                                                
030900*                            DLI INPUT-OUTPUT AREA2                       
031000 01  FILLER.                                                              
031100 03  DLI-IO-AREA2     PIC X(200)  VALUE SPACE.                            
031200     SKIP3                                                                
031300*03  WLARTM01 -COPY WDK901   -PRE ARTM-   -RED DLI-IO-AREA2.              
031400     EJECT                                                                
031500*03  WLARTM11 -COPY WDK911   -PRE ARTM-   -RED DLI-IO-AREA2.              
031600     EJECT                                                                
031700*                            DLI INPUT-OUTPUT AREA3                       
031800 01  FILLER.                                                              
031900 03  DLI-IO-AREA3     PIC X(200)  VALUE SPACE.                            
032000     SKIP3                                                                
032100*03  WLARTD01 -COPY WDD801   -PRE ARTD-   -RED DLI-IO-AREA3.              
032200     EJECT                                                                
032300*03  WLARTD11 -COPY WDD811   -PRE ARTD-   -RED DLI-IO-AREA3.              
032400     EJECT                                                                
032500                                                                          
032600 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDA901'.                    
032700 01  DLI-IO-WDA901.                                                       
032800*    03  -COPY WDA901                                                     
032900     EJECT                                                                
033000                                                                          
033100 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDA911'.                    
033200 01  DLI-IO-WDA911.                                                       
033300*    03  -COPY WDA911                                                     
033400                                                                          
033500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033600 01   DLI-IO-AREA-B601.                                                   
033700*     03  -COPY WDB601                                                    
033800     EJECT                                                                
033900                                                                          
034000 LINKAGE SECTION.                                                         
034100*01  -COPY W0009     -PRE MSG-                                            
034200     EJECT                                                                
034300*01  -COPY W0008     -PRE USEA-                                           
034400         05  FILLER           PIC X.                                      
034500     EJECT                                                                
034600*01  -COPY W0008     -PRE ARTC-                                           
034700         05  FILLER            PIC X.                                     
034800     SKIP2                                                                
034900*01  -COPY W0008     -PRE ARTS-                                           
035000         05  FILLER           PIC X.                                      
035100     SKIP2                                                                
035200*01  -COPY W0008     -PRE ARTM-                                           
035300         05  FILLER           PIC X.                                      
035400     EJECT                                                                
035500*01  -COPY W0008     -PRE XXBU-                                           
035600         05  FILLER           PIC X.                                      
035700     EJECT                                                                
035800*01  -COPY W0008     -PRE XXBX-                                           
035900         05  FILLER           PIC X.                                      
036000     SKIP2                                                                
036100*01  -COPY W0008     -PRE INLA-                                           
036200         05  FILLER           PIC X.                                      
036300     EJECT                                                                
036700*01  -COPY W0008     -PRE WDD8-                                           
036800         05  FILLER           PIC X.                                      
036900     EJECT                                                                
037000*01  -COPY W0008     -PRE WDA9-                                           
037100         05  FILLER           PIC X.                                      
037200     EJECT                                                                
037300*01  -COPY W0008     -PRE WDB6-                                           
037400         05  FILLER           PIC X.                                      
037500     EJECT                                                                
037600 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
037700                                    ARTC-PCB ARTS-PCB                     
037800                                    ARTM-PCB XXBU-PCB XXBX-PCB            
037900                                    INLA-PCB WDD8-PCB                     
038000                                    WDA9-PCB WDB6-PCB.                    
038100     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
038200                                    ARTC-PCB ARTS-PCB                     
038300                                    ARTM-PCB XXBU-PCB XXBX-PCB            
038400                                    INLA-PCB WDD8-PCB                     
038500                                    WDA9-PCB WDB6-PCB.                    
038600                                                                          
038700     PERFORM IMS-GET-MSG                                                  
038800                                                                          
038900     IF  SEGMENT-FINNS                                                    
039000         PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                              
039100                                                                          
039200         IF  SW-NYCKLAR-OK = JA                                           
039300                                                                          
039400             MOVE IDARTNR-WS TO W-IDARTNR                                 
039500                                TEST-IDARTNR                              
039600             PERFORM IMS-GET-ART-SEG                                      
039700                                                                          
039800             IF  SEGMENT-FINNS                                            
039900                MOVE ART-IDLEVNR TO WS-IDLEVNR-8                          
040000                IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                 
040100                OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE           
040200                   PERFORM B-RED-BILD-FRAN-WDK6-7-WDK9                    
040300                   PERFORM D-RED-BILD-FRAN-WDD3                           
040400                   PERFORM E-RED-BILD-FRAN-WDD8                           
040500                   PERFORM G-RED-BILD-FRAN-W6D1                           
040600                   PERFORM H-RED-BILD-FRAN-WDA9                           
040700                   PERFORM F-KOLLA-OM-LARM-FINNS                          
040800                ELSE                                                      
040900                   MOVE FEL-8 (SPIND) TO MOD-MESSAGE                      
041000                END-IF                                                    
041100             ELSE                                                         
041200                MOVE FEL-2 (SPIND) TO MOD-TEMFSINF                        
041300             END-IF                                                       
041400                                                                          
041500         END-IF                                                           
041600                                                                          
041700         PERFORM IMS-INSERT-MSG                                           
041800     END-IF                                                               
041900                                                                          
042000     MOVE ZERO TO RETURN-CODE                                             
042100     GOBACK                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
042500                                                                          
042600     MOVE JA TO SW-NYCKLAR-OK                                             
042700     IF MSG-DUBBLA-TRANSKODER                                             
042800         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
042900         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
043000         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90423I1-CTX           
043100     ELSE                                                                 
043200         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
043300         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
043400         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90423I1-CTX           
043500     END-IF                                                               
043600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
043700     MOVE '001'             TO MSGI-KDCALL                                
043800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043900                               MSGI-IDLTERM-USER                          
044000     MOVE '9423'            TO MSGI-IDTRANS                               
044100     IF MFS-IDTRANS(1:2) NOT = '42'                                       
044200       IF MFS-IDTRANS = '9423'                                            
044300       OR (MID-IDARTNR-IN NUMERIC                                         
044400       AND MID-IDARTNR-IN > ZERO)                                         
044500           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
044600       END-IF                                                             
044700     END-IF                                                               
044800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044900     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
045000     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
045100                                                                          
045200     IF MSGI-IDLAND-SPR = SPACE                                           
045300       IF MSGI-IDSPRAK = SPACE                                            
045400         IF SWEDISH-TEXT                                                  
045500           MOVE +1 TO SPIND                                               
045600         ELSE                                                             
045700           MOVE +2 TO SPIND                                               
045800         END-IF                                                           
045900       ELSE                                                               
046000         IF MSGI-IDSPRAK = 'SV'                                           
046100           MOVE +1 TO SPIND                                               
046200         ELSE                                                             
046300           MOVE +2 TO SPIND                                               
046400         END-IF                                                           
046500       END-IF                                                             
046600     ELSE                                                                 
046700       IF MSGI-IDLAND-SPR = 'SE'                                          
046800         MOVE +1 TO SPIND                                                 
046900       ELSE                                                               
047000         MOVE +2 TO SPIND                                                 
047100       END-IF                                                             
047200     END-IF                                                               
047300                                                                          
047400                                                                          
047500     MOVE LOW-VALUE TO MOD-AREA-OUTPUT                                    
047600     MOVE 'W90423O1' TO MFS-IDMOD                                         
047700     COMPUTE MSG-KVLL = LENGTH OF MOD-W90423O1 + 4                        
047800     MOVE MFS-RENSA-FAELT TO                                              
047900                      MOD-MESSAGE                                         
048000                      MOD-LAGERPLATS                                      
048100                      MOD-TEMFSINF                                        
048200     PERFORM MFS-RENSA-UTDATA-FAELT                                       
048300                                                                          
048400     ACCEPT DAGENS-DATUM FROM DATE                                        
048500     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
048600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
048700     CALL WDATKONV USING DAT-KDDATFORM                                    
048800                         DAT-I-TIDATUM                                    
048900                         DAT-O-TIDATUM                                    
049000                         DAT-KDSVAR                                       
049100     MOVE DAT-TIAAVV-GRP TO DAGENS-AAVV                                   
049200                                                                          
049300     IF  IDARTNR-WS NOT NUMERIC                                           
049400         MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                
049500         MOVE NEJ TO SW-NYCKLAR-OK                                        
049600     END-IF                                                               
049700                                                                          
049800     MOVE ZERO             TO W-ARB-SALDO                                 
049900                              WS-KVOKS-TOT (1)                            
050000                              WS-KVOKS-TOT (2)                            
050100                              WS-KVOKS-TOT (3)                            
050200                              WS-SUTPO-TOT                                
050300                                                                          
050400     .                                                                    
050500     EJECT                                                                
050600 B-RED-BILD-FRAN-WDK6-7-WDK9 SECTION.                                     
050700******************************************************************        
050800*                                                                *        
050900*    REDIGERING AV BILD MED DATA FRÅN WDK6 , WDK7 OCH WDK9       *        
051000*                                                                *        
051100******************************************************************        
051200                                                                          
051300     MOVE ART-IDLEVNR  TO W-HUVUDIDLEVNR                                  
051400     IF  ART-KDERS-UTG > ZERO                                             
051500       IF ART-KDERS-UTG = +29 OR +52                                      
051600         MOVE FEL-7 (SPIND) TO MOD-MESSAGE                                
051700       ELSE                                                               
051800         MOVE FEL-3 (SPIND) TO MOD-MESSAGE                                
051900       END-IF                                                             
052000     END-IF                                                               
052100     IF  ART-FLERS = JA                                                   
052200       MOVE FEL-5 (SPIND) TO MOD-TEMFSINF                                 
052300     END-IF                                                               
052400                                                                          
052500     PERFORM IMS-GET-ARTM01                                               
052600                                                                          
052700     IF SEGMENT-FINNS                                                     
052800       MOVE JA TO SW-ARTM-SEGM-FINNS                                      
052900         MOVE ARTM-ART-SUTPO-TOT     TO WS-SUTPO-TOT                      
053000         COMPUTE WS-KVOKS-TOT (1) =   ARTM-ART-KVOKS-BULK                 
053100                                    + ARTM-ART-KVOKS-DAG                  
053200                                    + ARTM-ART-KVOKS-VOR                  
053300     ELSE                                                                 
053400         MOVE ZERO TO WS-SUTPO-TOT                                        
053500                      WS-KVOKS-TOT (1)                                    
053600     END-IF                                                               
053700                                                                          
053800     PERFORM IMS-GET-CLAG-SEG                                             
053900                                                                          
054000     IF  SEGMENT-FINNS                                                    
054100         MOVE CLAG-IDANSK       TO W-IDANSK-L                             
054200         MOVE CLAG-KDAVT        TO MOD-KDAVT                              
054300         IF CLAG-FLGEMART = JA                                            
054400            MOVE JA TO WS-FLGEMART                                        
054500            MOVE MED-2 (SPIND) TO MOD-TEMFSINF                            
054600         END-IF                                                           
054700     ELSE                                                                 
054800         MOVE SPACE TO W-HUVUDIDLEVNR                                     
054900     END-IF                                                               
055000     MOVE ZERO TO W-ARB-SALDO                                             
055100                                                                          
055200     PERFORM BA-RED-FRAN-CLAG-WDK611                                      
055300                                                                          
055400     PERFORM BC-RED-FRAN-SDC-NDC                                          
055500                                                                          
055510***  MOVE W-OVERLAGER-SDC        ( FRÅN BC-RED- )                         
055511***                         TO MOD-KVLS-SDC-OVER                          
055520                                                                          
055600     MOVE W-SDC-KVOKS       TO WS-KVOKS-TOT    (2)                        
055700     MOVE W-NDC-KVOKS       TO WS-KVOKS-TOT    (3)                        
055800                                                                          
055900     COMPUTE W-DISPONIBELT =  W-SDC-KVLS                                  
056000                            - W-SDC-KVRESS                                
056100                                                                          
056200     MOVE W-DISPONIBELT     TO MOD-DISP        (2)                        
056300                                                                          
056400     COMPUTE W-DISPONIBELT =  W-NDC-KVLS                                  
056500                            - W-NDC-KVRESS                                
056600                                                                          
056700     MOVE W-DISPONIBELT     TO MOD-DISP        (3)                        
056800                                                                          
056900     MOVE W-SDC-KVAKS-SDC   TO MOD-KVAKS-LAGER (2)                        
057000     MOVE W-NDC-KVAKS-NDC   TO MOD-KVAKS-LAGER (3)                        
057100     MOVE W-SDC-KVLS        TO MOD-KVLS        (2)                        
057200     MOVE W-NDC-KVLS        TO MOD-KVLS        (3)                        
058800     .                                                                    
058900     EJECT                                                                
059000 BA-RED-FRAN-CLAG-WDK611 SECTION.                                         
059100                                                                          
059200     IF  SEGMENT-FINNS                                                    
059300        MOVE CLAG-TIINVDAT       TO W-TIINVDAT                            
059400                                                                          
059500        MOVE CLAG-KVAKS-CDC      TO MOD-KVAKS-LAGER (1)                   
059600        MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR  WS-ADLAGOMR             
059700        MOVE CLAG-ADGANG         TO MOD-ADGANG                            
059800        MOVE CLAG-ADPLATS        TO MOD-ADPLATS   WS-ADPLATS              
059900                                                                          
060000        IF CLAG-KDERS > ZERO                                              
060100          IF CLAG-KDERS = +09 OR +19                                      
060200            MOVE FEL-6 (SPIND) TO MOD-MESSAGE                             
060300          ELSE                                                            
060400            IF CLAG-KDERS = +29 OR +52                                    
060500              MOVE FEL-7 (SPIND) TO MOD-MESSAGE                           
060600            ELSE                                                          
060700              MOVE FEL-3 (SPIND) TO MOD-MESSAGE                           
060800            END-IF                                                        
060900          END-IF                                                          
061000        END-IF                                                            
061100                                                                          
061200        PERFORM BAA-RED-FRAN-ARTM11                                       
061300                                                                          
061400        COMPUTE W-KVAKS = CLAG-KVAKS-CDC                                  
061500                        + CLAG-KVAKS-PAV                                  
061600                        + CLAG-KVAKS-T                                    
061700                                                                          
061800        COMPUTE W-CDC-KVLS = CLAG-KVLS                                    
061900        MOVE W-CDC-KVLS          TO MOD-KVLS     (1)                      
062000                                                                          
062100        COMPUTE W-DISPONIBELT =  W-CDC-KVLS                               
062200                               - CLAG-KVRESS                              
062300                                                                          
062400        MOVE W-DISPONIBELT       TO MOD-DISP  (1)                         
062500                                                                          
062600        COMPUTE W-ARB-SALDO      = W-ARB-SALDO                            
062700                                   + W-DISPONIBELT                        
062800                                   - CLAG-KVROS                           
062900                                   - WS-TPO-NAESTA-INLEV                  
063000                                   - WS-KVOKS-TOT (1)                     
063100                                   + W-KVAKS                              
063200        MOVE W-ARB-SALDO         TO MOD-ARB-SALDO (1)                     
063300     END-IF                                                               
063400     .                                                                    
063500     EJECT                                                                
063600 BAA-RED-FRAN-ARTM11 SECTION.                                             
063700                                                                          
063800     MOVE ZERO TO WS-TPO-NAESTA-INLEV                                     
063900                                                                          
064000     IF CLAG-TIDISPIN = ZERO                                              
064100       MOVE WS-SUTPO-TOT     TO WS-TPO-NAESTA-INLEV                       
064200     ELSE                                                                 
064300       IF ARTM-SEGMENT-FINNS                                              
064400         MOVE ZERO       TO W-DABEHOV-MIN                                 
064500         PERFORM S01-KONV-TIDISPIN                                        
064600         MOVE W-DADISPIN TO W-DABEHOV-MAX                                 
064700         PERFORM IMS-GET-ARTM11                                           
064800         PERFORM UNTIL SEGMENT-SAKNAS                                     
064900           COMPUTE WS-TPO-NAESTA-INLEV =  WS-TPO-NAESTA-INLEV             
065000                                        + ARTM-ANT-SUTPO-PB               
065100                                        + ARTM-ANT-SUTPO-EJPB             
065200           PERFORM IMS-GET-ARTM11                                         
065300         END-PERFORM                                                      
065400       END-IF                                                             
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800 BC-RED-FRAN-SDC-NDC SECTION.                                             
065900                                                                          
066000     MOVE ZERO TO W-OVERLAGER-SDC                                         
066100                                                                          
066200     PERFORM IMS-GET-SART-SEG                                             
066300     IF SEGMENT-FINNS                                                     
066400        PERFORM IMS-GET-SLAG-SEG                                          
066500        PERFORM UNTIL SEGMENT-SAKNAS                                      
066600          IF DCS-IDDC NOT = SLAG-IDDC                                     
066700             MOVE SLAG-IDDC TO W-IDDC-B6                                  
066800             PERFORM IMS-GU-WDB601                                        
066900          END-IF                                                          
067000          IF SEGMENT-FINNS AND                                            
067100             DCS-SDC                                                      
067200            ADD SLAG-KVLS            TO   W-SDC-KVLS                      
067300            ADD SLAG-KVAKS-SDC       TO   W-SDC-KVAKS-SDC                 
067400            ADD SLAG-KVAKS-PAV       TO   W-SDC-KVAKS-PAV                 
067500            ADD SLAG-KVPB-REF        TO   W-SDC-KVPB                      
067600            ADD SLAG-KVOKS-BULK      TO   W-SDC-KVOKS                     
067700            ADD SLAG-KVOKS-DAG       TO   W-SDC-KVOKS                     
067800                                                                          
067900            ADD SLAG-KVEFRS          TO   W-SDC-KVEFRS                    
068000            ADD SLAG-KVUTRS          TO   W-SDC-KVUTRS                    
068100                                                                          
068110            IF DCS-FLOVRLAGBER = NEJ                                      
068120              CONTINUE                                                    
068130            ELSE                                                          
068200              MOVE ZERO              TO   W-TILLG-SDC                     
068300              ADD SLAG-KVLS          TO   W-TILLG-SDC                     
068400              ADD SLAG-KVBEART       TO   W-TILLG-SDC                     
068500              ADD SLAG-KVAKS-SDC     TO   W-TILLG-SDC                     
068600              ADD SLAG-KVAKS-PAV     TO   W-TILLG-SDC                     
068700              SUBTRACT SLAG-KVOKS-BULK FROM W-TILLG-SDC                   
068800              SUBTRACT SLAG-KVOKS-DAG FROM W-TILLG-SDC                    
068900              IF SLAG-KVREFOVL    <    W-TILLG-SDC                        
069000                 COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC                
069100                                         + W-TILLG-SDC                    
069200                                         - SLAG-KVREFOVL                  
069300              END-IF                                                      
069310            END-IF                                                        
069320                                                                          
069400            MOVE ART-TIFINLV   TO TIFINLV-AAVVD                           
069500            MOVE DAGENS-AA     TO TMP1-YY                                 
069600            MOVE TIFINLV-AA    TO TMP2-YY                                 
069700            PERFORM WY2000P9                                              
069800            COMPUTE VECKO-SKILLNAD = (TMP1-YY - TMP2-YY) * 52             
069900                                   + DAGENS-VV - TIFINLV-VV               
070000            IF VECKO-SKILLNAD < 52                                        
070100               MOVE ZERO TO W-OVERLAGER-SDC                               
070200            END-IF                                                        
070300          ELSE                                                            
070410            IF DCS-NDC AND NOT DCS-NDC-CN                                 
070500              ADD SLAG-KVLS          TO W-NDC-KVLS                        
070600              ADD SLAG-KVRESS        TO W-NDC-KVRESS                      
070700              ADD SLAG-KVAKS-SDC     TO W-NDC-KVAKS-NDC                   
070800              ADD SLAG-KVAKS-PAV     TO W-NDC-KVAKS-PAV                   
070900              IF SLAG-IDLEVNR = '1441'                                    
071000                 OR SLAG-IDLEVNR = 'BP2TW'                                
071100                  ADD SLAG-KVPB-REF      TO W-NDC-KVPB                    
071200              END-IF                                                      
071300              ADD SLAG-KVOKS-BULK    TO W-NDC-KVOKS                       
071400              ADD SLAG-KVOKS-DAG     TO W-NDC-KVOKS                       
071500              ADD SLAG-KVROS-BULK    TO W-NDC-KVROS                       
071600              ADD SLAG-KVROS-DAG     TO W-NDC-KVROS                       
071700              ADD SLAG-KVEFRS        TO W-NDC-KVEFRS                      
071800              ADD SLAG-KVUTRS        TO W-NDC-KVUTRS                      
071900            END-IF                                                        
072000          END-IF                                                          
072100          PERFORM IMS-GET-SLAG-SEG                                        
072200        END-PERFORM                                                       
072300        COMPUTE W-SDC-KVPB ROUNDED =  W-SDC-KVPB                          
072400                                                                          
072500        COMPUTE W-NDC-KVPB ROUNDED =  W-NDC-KVPB                          
072600                                                                          
072700     END-IF                                                               
072800     .                                                                    
072900     EJECT                                                                
073000 D-RED-BILD-FRAN-WDD3 SECTION.                                            
073100                                                                          
073200     IF MSGI-IDLAND-SPR = 'GB'                                            
073300        MOVE 'GB' TO W-IDSKYLT                                            
073400     ELSE                                                                 
073500        MOVE 'S'  TO W-IDSKYLT                                            
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 E-RED-BILD-FRAN-WDD8 SECTION.                                            
074000                                                                          
074100     MOVE ZERO                      TO WS-KVBUFF                          
074200     PERFORM IMS-GET-WDD801                                               
074300     IF SEGMENT-FINNS                                                     
074400        MOVE WC-CDC-SE              TO W-IDDC                             
074500        MOVE 17                     TO W-ADBUFFOMR                        
074600        PERFORM IMS-GET-WDD811                                            
074700        PERFORM UNTIL SEGMENT-SAKNAS                                      
074800           ADD ARTD-SALDO-KVBUFF-F  TO WS-KVBUFF                          
074900           ADD ARTD-SALDO-KVBUFF-OF TO WS-KVBUFF                          
075000           PERFORM IMS-GET-WDD811                                         
075100        END-PERFORM                                                       
075200     END-IF                                                               
075300                                                                          
075400     IF WS-ADLAGOMR = 71 AND WS-ADPLATS = 7000                            
075500        ADD W-CDC-KVLS              TO WS-KVBUFF                          
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 F-KOLLA-OM-LARM-FINNS SECTION.                                           
076000                                                                          
076100     IF W-IDANSK-L > ZERO                                                 
076200       PERFORM IMS-GET-XXBX-2231                                          
076300       PERFORM IMS-GET-XXBX-2232                                          
076400       IF SEGMENT-FINNS                                                   
076500         MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK                           
076600         PERFORM IMS-GET-XXBU-2223                                        
076700         IF SEGMENT-FINNS                                                 
076800           PERFORM IMS-GET-XXBU-2224-FLNYLARM                             
076900           IF SEGMENT-FINNS                                               
077000             IF WS-FLGEMART = JA                                          
077100                MOVE MED-3 (SPIND) TO MOD-TEMFSINF                        
077200             ELSE                                                         
077300                MOVE MED-1 (SPIND) TO MOD-TEMFSINF                        
077400             END-IF                                                       
077500           END-IF                                                         
077600         END-IF                                                           
077700       END-IF                                                             
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 G-RED-BILD-FRAN-W6D1 SECTION.                                            
078200                                                                          
078300     MOVE IDARTNR-WS TO WS-IDARTNR                                        
078400     MOVE WS-IDARTNR TO W-IDARTNR-HSEQ                                    
078500     MOVE ZERO       TO W-KVART-TOT-C1                                    
078600     PERFORM IMS-GN-INLA11-W6D1SEQ                                        
078700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
078800       IF DCS-IDDC NOT = INLA-ART-IDDC                                    
078900          MOVE INLA-ART-IDDC TO W-IDDC-B6                                 
079000          PERFORM IMS-GU-WDB601                                           
079100       END-IF                                                             
079200       IF (DCS-CDC OR DCS-CDC-TR) AND                                     
079300           INLA-ART-IDLOPNRM  = ZERO                                      
079400         MOVE INLA-ART-KVAVIS TO WS-KVAVIS                                
079500         IF INLA-ART-FLFEL = NEJ                                          
079600           ADD WS-KVAVIS TO W-KVART-TOT-C1                                
079700         END-IF                                                           
079800       END-IF                                                             
079900       PERFORM IMS-GN-INLA11-W6D1SEQ                                      
080000     END-PERFORM                                                          
080100     .                                                                    
080200     EJECT                                                                
080300 H-RED-BILD-FRAN-WDA9 SECTION.                                            
080400                                                                          
080500     IF BYT02-RENOV                                                       
080600       IF BYT16-BYTES                                                     
080700         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
080800                                  6000                                    
080900         END-COMPUTE                                                      
081000       ELSE                                                               
081100         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
081200                                  1000                                    
081300         END-COMPUTE                                                      
081400       END-IF                                                             
081500       PERFORM IMS-GU-WDA901                                              
081600       IF SEGMENT-FINNS                                                   
081700         PERFORM UNTIL SEGMENT-SAKNAS                                     
081800           PERFORM IMS-GNP-WDA911                                         
081900           IF SEGMENT-FINNS                                               
082000* FIX SOM UTÖKAS FÖR VARJE RENOVÖR SOM ANSLUTS TILL WEB:EN                
082100             MOVE UPD-IDDISTR   TO TEST-IDDISTR                           
082200             IF DIS134-BYTESREN-WEB                                       
082300               ADD UPD-KVLS-REM TO WS-KVLS-REM                            
082400             END-IF                                                       
082500             PERFORM IMS-GNP-WDA911                                       
082600           END-IF                                                         
082700         END-PERFORM                                                      
082800       END-IF                                                             
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 S01-KONV-TIDISPIN SECTION.                                               
083300                                                                          
083400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
083500     MOVE CLAG-TIDISPIN TO DAT-I-TIDATUM                                  
083600     CALL WDATKONV USING DAT-KDDATFORM                                    
083700                         DAT-I-TIDATUM                                    
083800                         DAT-O-TIDATUM                                    
083900                         DAT-KDSVAR                                       
084000     IF DAT-KDSVAR-OK                                                     
084100       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
084200       MOVE DAT-TIVV       TO W-TIVV                                      
084300       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
084400       MOVE W-TIAAAAVV     TO W-DADISPIN                                  
084500     ELSE                                                                 
084600       MOVE ZERO           TO W-DADISPIN                                  
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 MFS-RENSA-UTDATA-FAELT SECTION.                                          
085100                                                                          
085200     MOVE 1 TO IX                                                         
085300     PERFORM UNTIL IX > 3                                                 
085400                                                                          
085500         MOVE MFS-RENSA-FAELT TO  MOD-KVLS      (IX)                      
085600                              MOD-DISP          (IX)                      
085700                              MOD-KVAKS-LAGER   (IX)                      
085800                              MOD-ARB-SALDO     (IX)                      
085900          ADD 1 TO IX                                                     
086000     END-PERFORM                                                          
086100     .                                                                    
086200     EJECT                                                                
086300* IMS SEKTIONER                                                           
086400                                                                          
086500 IMS-GET-MSG SECTION.                                                     
086600     MOVE '  QC' TO GODK-STATUSKODER                                      
086700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
086800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086900     PERFORM IMS-STATUSKONTROLL                                           
087000     .                                                                    
087100     SKIP3                                                                
087200 IMS-INSERT-MSG SECTION.                                                  
087300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
087400     MOVE SPACE TO GODK-STATUSKODER                                       
087500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
087600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     .                                                                    
087900     EJECT                                                                
088000 IMS-GET-ART-SEG SECTION.                                                 
088100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
088200            DELIMITED BY SIZE INTO SSA1                                   
088300     MOVE '  GE' TO GODK-STATUSKODER                                      
088400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
088500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSKONTROLL                                           
088700     .                                                                    
088800     SKIP3                                                                
088900 IMS-GET-CLAG-SEG SECTION.                                                
089000     MOVE 'WLARTC11' TO SSA1                                              
089100     MOVE '  GE' TO GODK-STATUSKODER                                      
089200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
089300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     EJECT                                                                
089700 IMS-GET-SART-SEG SECTION.                                                
089800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
089900            DELIMITED BY SIZE INTO SSA1                                   
090000     MOVE '  GE' TO GODK-STATUSKODER                                      
090100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
090200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
090300     PERFORM IMS-STATUSKONTROLL                                           
090400     .                                                                    
090500     SKIP3                                                                
090600 IMS-GET-SLAG-SEG SECTION.                                                
090700     MOVE 'WLARTS11' TO SSA1                                              
090800     MOVE '  GE' TO GODK-STATUSKODER                                      
090900     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
091000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
091100     PERFORM IMS-STATUSKONTROLL                                           
091200     .                                                                    
091300     EJECT                                                                
093100 IMS-GET-ARTM01         SECTION.                                          
093200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
093300            DELIMITED BY SIZE INTO SSA1                                   
093400     MOVE '  GE' TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
093600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     SKIP3                                                                
094000 IMS-GET-ARTM11         SECTION.                                          
094100     STRING 'WLARTM11(DABEHOV >=' W-DABEHOV-MIN-X                         
094200                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
094300            DELIMITED BY SIZE INTO SSA1                                   
094400     MOVE '  GE' TO GODK-STATUSKODER                                      
094500     CALL CBLTDLI USING GNP ARTM-PCB DLI-IO-AREA2 SSA1                    
094600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-GET-WDD801         SECTION.                                          
095100     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
095200            DELIMITED BY SIZE INTO SSA1                                   
095300     MOVE '  GE' TO GODK-STATUSKODER                                      
095400     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-AREA3 SSA1                     
095500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800     SKIP3                                                                
095900 IMS-GET-WDD811         SECTION.                                          
096000     STRING 'WDD811  (IDDC     =' W-IDDC-X                                
096100                    '&ADBUFFOM =' W-ADBUFFOMR-X                           
096200                    '&ADBUFGAN =' W-ADBUFFGANG-X   ')'                    
096300            DELIMITED BY SIZE INTO SSA1                                   
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-AREA3 SSA1                    
096600     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900     EJECT                                                                
097000 IMS-GET-XXBX-2231      SECTION.                                          
097100     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
097200            DELIMITED BY SIZE INTO SSA1                                   
097300     MOVE '  ' TO GODK-STATUSKODER                                        
097400     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA SSA1                      
097500     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
097600     PERFORM IMS-STATUSKONTROLL                                           
097700     .                                                                    
097800     SKIP3                                                                
097900 IMS-GET-XXBX-2232      SECTION.                                          
098000     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
098100            DELIMITED BY SIZE INTO SSA1                                   
098200     MOVE '  GE' TO GODK-STATUSKODER                                      
098300     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA SSA1                     
098400     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
098500     PERFORM IMS-STATUSKONTROLL                                           
098600     .                                                                    
098700     EJECT                                                                
098800 IMS-GET-XXBU-2223      SECTION.                                          
098900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
099000            DELIMITED BY SIZE INTO SSA1                                   
099100     MOVE '  GE' TO GODK-STATUSKODER                                      
099200     CALL CBLTDLI USING GU  XXBU-PCB DLI-IO-AREA SSA1                     
099300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
099400     PERFORM IMS-STATUSKONTROLL                                           
099500     .                                                                    
099600     SKIP3                                                                
099700 IMS-GET-XXBU-2224-FLNYLARM SECTION.                                      
099800     STRING 'WLXXBU11(FLNYLARM =' W-FLNYLARM-X ')'                        
099900            DELIMITED BY SIZE INTO SSA1                                   
100000     MOVE '  GE' TO GODK-STATUSKODER                                      
100100     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1                     
100200     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GN-INLA11-W6D1SEQ SECTION.                                           
100700     STRING 'W6INLA11(W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
100800            DELIMITED BY SIZE INTO SSA1                                   
100900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
101000     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA SSA1                      
101100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
101500                                                                          
101600 IMS-GU-WDA901 SECTION.                                                   
101700     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
101800          DELIMITED BY SIZE INTO SSA1                                     
101900     MOVE '  GE'           TO GODK-STATUSKODER                            
102000     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
102100     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
102200     PERFORM IMS-STATUSKONTROLL                                           
102300     .                                                                    
102400                                                                          
102500 IMS-GNP-WDA911 SECTION.                                                  
102600     MOVE 'WDA911   '      TO SSA1                                        
102700     MOVE '  GE'           TO GODK-STATUSKODER                            
102800     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
102900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     .                                                                    
103200                                                                          
103300 IMS-GU-WDB601    SECTION.                                                
103400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
103500          DELIMITED BY SIZE INTO SSA1                                     
103600     MOVE '  GE' TO GODK-STATUSKODER                                      
103700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
103800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
103900     PERFORM IMS-STATUSKONTROLL                                           
104000     .                                                                    
104100     EJECT                                                                
104200                                                                          
104300 IMS-STATUSKONTROLL SECTION.                                              
104400     SET STATUS-IX TO 1                                                   
104500     SEARCH GODK-STATUS                                                   
104600       AT END                                                             
104700         CALL FELLOG                                                      
104800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104900         CONTINUE                                                         
105000     END-SEARCH                                                           
105100     .                                                                    
105200     EJECT                                                                
105300*    -COPY WY2000P9                                                       
105400     EJECT                                                                
