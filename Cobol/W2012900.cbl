000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2012900.                                                
000300 AUTHOR.         IDK, BS.                                                 
000400 DATE-WRITTEN.   MARS 1979.                                               
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION.   TP-PROGRAM FÖR SKROTNING, F R Å G E D E L,               
000800*                                   -OCH   U P P D A T E R I N G          
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W2T129                                              
001200*                     W2T129U                                             
001210*                     W2T129V                                             
001300*        MID:         W2I12901                                            
001400*    UTDATA.                                                              
001500*        MOD:         W2O12901                                            
001600*    SUBPROGRAM.                                                          
001700*        FELLOG                                                           
001800*                                                                         
001900*   ÄNDRINGAR:                                                            
002000*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002100*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002200*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002300*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002400*                                                                         
002500*        16-03-04  E'TRACKER 10243132 CHINA EXPORT 2015                   
002600*                                                                         
002700*                                                                         
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM               PIC X(8)    VALUE 'W2012900'.                    
003900 77  JA                  PIC X       VALUE 'J'.                           
004000 77  NEJ                 PIC X       VALUE 'N'.                           
004100 77  CURRENT-SECTION     PIC X(30) VALUE SPACE.                           
004200 77  DBS-SECTION         PIC X(30) VALUE SPACE.                           
004300                                                                          
004400 77  WS-DAGENS-AAMMDD    PIC 9(7).                                        
004500 77  IDARTNR-WS          PIC X(9).                                        
004600 77  KVSLUTKP-WS         PIC 9(7).                                        
004700 77  TISLUTKP-WS         PIC 9(7).                                        
004800 77  WS-IDTRANS          PIC X(4).                                        
004900     88 EGEN-BILD                    VALUE '2129'.                        
005000 77  WS-FLERS            PIC X(1)    VALUE SPACE.                         
005100 77  PROGSW-IX           PIC 9(2)    VALUE ZERO.                          
005200 77  IX                  PIC 9(2)    VALUE ZERO.                          
005300 77  WS-SUTPO-TOT-C2     PIC 9(7)    VALUE ZERO.                          
005400 77  WS-KVOKS-C1         PIC S9(6)   VALUE ZERO.                          
005500 77  DAGENS-DATUM-Y2K    PIC 9(8)    VALUE ZERO.                          
005600 77  WS-IDLEVNR-8        PIC X(8)    VALUE SPACE.                         
005700 77  WS-SPAR-BEANST-GODK PIC X(25)   VALUE SPACE.                         
005800 77  WS-IDDC-REF         PIC X(1)    VALUE SPACE.                         
005900                                                                          
006000 01  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.                  
006100 01  W-SDC-KVAKS         PIC S9(7)    VALUE ZERO COMP-3.                  
006200 01  W-SDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.                  
006300                                                                          
006400     EJECT                                                                
006500 01  SWITCHAR.                                                            
006600   03  INDATA-SW         PIC X.                                           
006700     88  INDATA-OK       VALUE 'J'.                                       
006800   03  INGAR-SATS-SW     PIC X.                                           
006900     88  INGAR-I-SATS    VALUE 'J'.                                       
007000*      --- VALID IDDC CODES                                               
007100*                                                                         
007200*01    -COPY WWDCKONS                                                     
007300       EJECT                                                              
007400                                                                          
007500*01    -COPY WWPRODSL                                                     
007600       EJECT                                                              
007700                                                                          
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900   03 CBLTDLI            PIC X(8)    VALUE 'CBLTDLI '.                    
008000   03 FELLOG             PIC X(8)    VALUE 'FELLOG  '.                    
008100   03 W005INIT           PIC X(8)    VALUE 'W005INIT'.                    
008200   03  W411SAP           PIC X(8)    VALUE 'W411SAP'.                     
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*01 -COPY WMSGINIT                                                        
008500                                                                          
008600 01  W-DATUM.                                                             
008700     05  W-DATUM-DATE    PIC X(6).                                        
008800                                                                          
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'W411SAP '.            
009100     SKIP3                                                                
009200*01 -COPY W411SAP                                                         
009300     EJECT                                                                
009400 01  W-IDARTNR-X.                                                         
009500   03 W-IDARTNR          PIC S9(9)   VALUE ZERO  COMP-3.                  
009600                                                                          
009700 01  W-WDJ1C1KY-MIN.                                                      
009800     03  FILLER               PIC X(5)  VALUE SPACE.                      
009900     03  FILLER               PIC X(30) VALUE SPACE.                      
010000     03  W-RAD-IDARTNR-MIN    PIC S9(9) VALUE ZERO    COMP-3.             
010100     03  FILLER               PIC X(9)  VALUE LOW-VALUE.                  
010200 01  W-WDJ1C1KY-MAX.                                                      
010300     03  FILLER               PIC X(5)  VALUE SPACE.                      
010400     03  FILLER               PIC X(30) VALUE SPACE.                      
010500     03  W-RAD-IDARTNR-MAX    PIC S9(9) VALUE ZERO    COMP-3.             
010600     03  FILLER               PIC X(9)  VALUE HIGH-VALUE.                 
010700 01  W-WDJ111KY-X.                                                        
010800     03  W-KDSTRRAD           PIC X(1).                                   
010900     03  W-IDRADNR            PIC S9(5) COMP-3 VALUE ZERO.                
011000 01  W-IDARTNR-STR-X.                                                     
011100     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
011200                                                                          
011300 01  W-IDSKYLT-KEY-X.                                                     
011400   03  W-IDSKYLT-KEY     PIC X(3)    VALUE SPACE.                         
011500                                                                          
011600 01  NYCKLAR-6321.                                                        
011700     03  W-WDGX6321-ROT-X.                                                
011800         05  FILLER              PIC X(04)   VALUE '6321'.                
011900         05  W-KDARBTYP          PIC X(08)   VALUE 'ANSK    '.            
012000         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
012100     03  W-WDGX6322-KEY-X.                                                
012200         05  W-DASKROT9-BEORD    PIC 9(08)   VALUE ZERO.                  
012300     03  W-WDGX6324-KEY-X.                                                
012400         05  W-IDARTNR-6324      PIC S9(9)  COMP-3 VALUE ZERO.            
012500         05  W-IDDC-6324         PIC X(02)   VALUE '11'.                  
012600         05  W-KDSTASKR-6324     PIC S9     COMP-3 VALUE ZERO.            
012700                                                                          
012800     03  W-WDGXKEY-X.                                                     
012900         05  FILLER             PIC X(4)    VALUE '6327'.                 
013000         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
013100         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
013200         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
013300     03  W-IDUSER-GODK-X.                                                 
013400         05  W-IDUSER-GODK      PIC X(8)       VALUE SPACE.               
013500     EJECT                                                                
013600 01  W.                                                                   
013700    03  WCLAG-FLSKROT-BEORD-CDC   PIC X.                                  
013800    03  WARTC91-KVUTRS-C1         PIC S9(7)   COMP-3.                     
013900    03  WARTC91-KVUTRS-C2         PIC S9(7)   COMP-3.                     
014000    03  WARTC91-KVAKS-C1          PIC S9(7)   COMP-3.                     
014100    03  WARTC91-KDERS-C1          PIC S9(3)   COMP-3.                     
014200    03  WARTC91-KDERS-C2          PIC S9(3)   COMP-3.                     
014300    03  WARTC91-KVLS-C1           PIC S9(7)   COMP-3.                     
014400    03  WARTC91-KVLS-C2           PIC S9(7)   COMP-3.                     
014500    03  WARTC91-KVRESS-C1         PIC S9(7)   COMP-3.                     
014600    03  WARTC91-KVRESS-C2         PIC S9(7)   COMP-3.                     
014700    03  WARTC91-IDANSK            PIC S9(3)   COMP-3.                     
014800    03  WKVSKRANT                 PIC  9(7).                              
014900    03  WKVKVAR.                                                          
015000        05  WKVKVAR-N             PIC  9(7).                              
015100    03  WMESSAGE.                                                         
015200        05  WMESSAGE-1            PIC  X(20).                             
015300        05  WMESSAGE-2            PIC  X(20).                             
015400 01  WS-IDKONTO                   PIC 9(10)   VALUE ZERO.                 
015500 01  WS-BELAGINS-DEL.                                                     
015600    03  FILLER                    PIC  X(20)                              
015700        VALUE  'KVARVARANDE ANTAL = '.                                    
015800    03  WS-BELAGINS-KVSKROT-KVAR  PIC  Z(6)9   VALUE ZERO.                
015900    03  FILLER                    PIC  X(03)   VALUE SPACE.               
016000    03  WS-BELAGINS-DEL2          PIC  X(30)   VALUE SPACE.               
016100     EJECT                                                                
016200                                                                          
016300 01      MEDDELANDEN.                                                     
016400   03    MEDDELANDE-1    PIC X(31)                                        
016500                    VALUE 'ARTIKELNUMRET ÄR INTE NUMERISKT'.              
016600                                                                          
016700   03    MEDDELANDE-2    PIC X(28)                                        
016800                    VALUE 'ARTIKELN SAKNAS PÅ DATABASEN'.                 
016900                                                                          
017000   03    MEDDELANDE-3.                                                    
017100     05  FILLER          PIC X(30)                                        
017200                    VALUE 'SKROTNING BEORDRAD PÅ C-LAGER '.               
017300     05  MED3-CLAGER     PIC X(7).                                        
017400                                                                          
017500   03    MEDDELANDE-4.                                                    
017600     05  FILLER          PIC X(33)                                        
017700                    VALUE 'UTREDNING PÅGÅR - AVVAKTA SKROTN.'.            
017800     05  MED4-CLAGER     PIC X(7).                                        
017900                                                                          
018000   03    MEDDELANDE-5    PIC X(26)                                        
018100                    VALUE 'FÖR UPPDATERING TRYCK PF11'.                   
018200                                                                          
018300   03    MEDDELANDE-6    PIC X(17)                                        
018400                    VALUE 'UPPLYSTA FÄLT FEL'.                            
018500                                                                          
018510   03    MEDDELANDE-7    PIC X(17)                                        
018520                    VALUE 'LYNK & CO PART'.                               
018530                                                                          
018600   03    MEDDELANDE-10   PIC X(20)                                        
018700                    VALUE 'SKROTORDER GENERERAD'.                         
018800                                                                          
018900   03    MEDDELANDE-12   PIC X(20)                                        
019000                    VALUE ' ERS-KOD ÄNDR GENAD '.                         
019100                                                                          
019200   03    MEDDELANDE-13.                                                   
019300       05  FILLER        PIC X(23)                                        
019400                    VALUE 'SKROTSP SATT TILL N PÅ '.                      
019500       05  MED-13A-CLAGER PIC X(4)  VALUE SPACE.                          
019600       05  FILLER         PIC X(1)  VALUE SPACE.                          
019700       05  MED-13B-CLAGER PIC X(4)  VALUE SPACE.                          
019800                                                                          
019900   03    MEDDELANDE-14.                                                   
020000       05  FILLER        PIC X(13)                                        
020100                    VALUE 'ARTIKEL UTGÅR'.                                
020200                                                                          
020300   03    MEDDELANDE-15.                                                   
020400       05  FILLER        PIC X(20)                                        
020500                    VALUE 'UPPDATERING GJORD'.                            
020510   03    MEDDELANDE-16   PIC X(26)                                        
020520                    VALUE 'FÖR UPPDATERING TRYCK PF23'.                   
020521   03    MEDDELANDE-17   PIC X(37)                                        
020522                    VALUE 'ENDAST SKROTSPÄRR FÅR ÄNDRAS VID PF23'.        
020530                                                                          
020600   03    W-FEL-6.                                                         
020700     05  FILLER        PIC X(19)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
020800     05  FILLER        PIC X(19)   VALUE 'USER NOT AUTHORIZED'.           
020900   03   FILLER REDEFINES W-FEL-6.                                         
021000     05  FEL-6         PIC X(19) OCCURS 2.                                
021100                                                                          
021200   03    W-FEL-7.                                                         
021300     05  FILLER     PIC X(23)   VALUE 'EJ BEHÖRIG ATT SKROTA'.            
021400     05  FILLER     PIC X(23)   VALUE 'NOT AUTHORIZED TO SCRAP'.          
021500   03   FILLER REDEFINES W-FEL-7.                                         
021600     05  FEL-7         PIC X(23) OCCURS 2.                                
021700     EJECT                                                                
021800*                        ****    TP-AREOR                                 
021900 01  FILLER  PIC X(16)   VALUE '    TP-AREAOR   '.                        
022000     SKIP2                                                                
022100*01      MID -COPY W2I12901 -PRE MID-.                                    
022200     EJECT                                                                
022300*        -COPY WMSGAREA                                                   
022400     EJECT                                                                
022500*    03  MOD -COPY W2O12901 -PRE MOD-  -RED MSG-AREA.                     
022600     EJECT                                                                
022700 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
022800                                                                          
022900 01  W-PROG-TO-PROG-SW.                                                   
023000     05  P-WS-LL         PIC S9(4)  VALUE +469 COMP SYNC.                 
023100     05  P-WS-Z1-Z2      PIC  X(2)  VALUE LOW-VALUE.                      
023200     05  KDTRANS-WS      PIC  X(8)  VALUE 'W1T113U '.                     
023300     05  FILLER          PIC  X(5)  VALUE '21293'.                        
023400*    05  MID    -COPY W1I11301     -PRE PROGSW-                           
023500     EJECT                                                                
023600*01  -COPY WMFSAREA.                                                      
023700     EJECT                                                                
023800******************************************************************        
023900*****                                                                     
024000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024100*****                                                                     
024200 01  IMS-WS.                                                              
024300   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
024400     SKIP3                                                                
024500*****                    **** STATUS-KOD FRÅN IMS                         
024600   03    STATUS-WS       PIC XX.                                          
024700         88  SEGMENT-FINNS       VALUE '  '.                              
024800         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
024900     SKIP3                                                                
025000   03    GODK-STATUSKODER.                                                
025100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025200     SKIP3                                                                
025300 01      SSA1            PIC X(160).                                      
025400 01      SSA2            PIC X(64).                                       
025500 01      SSA3            PIC X(64).                                       
025600     EJECT                                                                
025700*                            IMS FUNKTIONSKODER                           
025800*01      -COPY W0003                                                      
025900     EJECT                                                                
026000 01  FILLER              PIC X(16)   VALUE 'IO-AREA  IO-AREA'.            
026100 01  DLI-IO-AREA-01.                                                      
026200     03  IO-AREA-01      PIC X(200)  VALUE SPACE.                         
026300     SKIP3                                                                
026400*    03  WLARTC01   -COPY WDK601 -RED IO-AREA-01.                         
026500     EJECT                                                                
026600 01  DLI-IO-AREA-11.                                                      
026700     03  IO-AREA-11      PIC X(1000)  VALUE SPACE.                        
026800     SKIP3                                                                
026900*    03  WLARTC11   -COPY WDK611 -RED IO-AREA-11.                         
027000     EJECT                                                                
027100 01  FILLER              PIC X(16) VALUE 'DLI-IO-WDK627'.                 
027200 01  DLI-IO-WDK627.                                                       
027300*    03  -COPY WDK627                                                     
027400     EJECT                                                                
027500 01  FILLER              PIC X(16) VALUE 'DLI-IO-WDK629'.                 
027600 01  DLI-IO-WDK629.                                                       
027700*    03  -COPY WDK629                                                     
027800     EJECT                                                                
027900                                                                          
028000 01  DLI-IO-AREA.                                                         
028100     03  IO-AREA         PIC X(200)  VALUE SPACE.                         
028200     EJECT                                                                
028300*    03  WLBENA     -COPY WDD301 -PRE BENA-    -RED IO-AREA.              
028400     EJECT                                                                
028500*    03  WLBENA     -COPY WDD311 -PRE BENA-    -RED IO-AREA.              
028600     EJECT                                                                
028700 01  FILLER              PIC X(17)   VALUE 'IO-AREA2 IO-AREA2'.           
028800 01  DLI-IO-AREA2.                                                        
028900     03  IO-AREA2         PIC X(100)  VALUE SPACE.                        
029000     SKIP3                                                                
029100*    03  WLARTM01   -COPY WDK901 -PRE ARTM-    -RED IO-AREA2.             
029200     EJECT                                                                
029300 01  FILLER                   PIC X(16) VALUE 'DLI-IO-AREA3    '.         
029400     SKIP2                                                                
029500 01  DLI-IO-AREA3.                                                        
029600    03  IO-AREA3              PIC X(250)  VALUE SPACE.                    
029700     SKIP2                                                                
029800*    03 WLSATE -COPY WDJ1C1      -PRE SATE01-  -RED IO-AREA3.             
029900     EJECT                                                                
030000*    03 WLSATB -COPY WDJ101      -PRE SATB01-  -RED IO-AREA3.             
030100     EJECT                                                                
030200*    03 WLSATB -COPY WDJ111      -PRE SATB11-  -RED IO-AREA3.             
030300     EJECT                                                                
030400                                                                          
030500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
030600 01  DLI-IO-WDR501-6321.                                                  
030700*    03  -COPY WDGX6321                                                   
030800     EJECT                                                                
030900                                                                          
031000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6322'.                    
031100 01  DLI-IO-WDGX6322.                                                     
031200*    03  -COPY WDGX6322                                                   
031300     EJECT                                                                
031400                                                                          
031500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6324'.                    
031600 01  DLI-IO-WDGX6324.                                                     
031700*    03  -COPY WDGX6324                                                   
031800     EJECT                                                                
031900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501'.                      
032000 01  DLI-IO-WDR501-6327.                                                  
032100*    03  -COPY WDGX6327                                                   
032200     EJECT                                                                
032300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6328'.                    
032400 01  DLI-IO-WDGX6328.                                                     
032500*    03  -COPY WDGX6328                                                   
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
032900 01  DLI-IO-WDK701.                                                       
033000*    03  -COPY WDK701                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
033400 01  DLI-IO-WDK711.                                                       
033500*    03  -COPY WDK711                                                     
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
033900 01  DLI-IO-WDN601.                                                       
034000*    03  -COPY WDN601                                                     
034100     EJECT                                                                
034200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
034300 01  DLI-IO-WDN611.                                                       
034400*    03  -COPY WDN611                                                     
034500     EJECT                                                                
034600 LINKAGE SECTION.                                                         
034700*01  -COPY W0009     -PRE MSG-                                            
034800                                                                          
034900*01  -COPY W0009     -PRE ALT-                                            
035000     EJECT                                                                
035100*01  -COPY W0008     -PRE USEA-.                                          
035200         05  FILLER           PIC X.                                      
035300                                                                          
035400*01  -COPY W0008     -PRE WDK6-                                           
035500        05  FILLER            PIC X(6).                                   
035600     EJECT                                                                
035700*01  -COPY W0008     -PRE ARTC2-                                          
035800        05  FILLER            PIC X(6).                                   
035900                                                                          
036000*01  -COPY W0008     -PRE BENA-                                           
036100         05  FILLER           PIC X.                                      
036200     EJECT                                                                
036300*01  -COPY W0008     -PRE ARTM-                                           
036400         05  FILLER           PIC X.                                      
036500     EJECT                                                                
036600*01  -COPY W0008     -PRE SATE-                                           
036700         05  FILLER           PIC X.                                      
036800                                                                          
036900*    -COPY W0008       -PRE SATB-.                                        
037000      05    FILLER           PIC X.                                       
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE 6321-                                              
037300     05  FILLER                  PIC X.                                   
037400     SKIP3                                                                
037500 01  SAPC-PCB                    PIC X.                                   
037600     SKIP3                                                                
037700*01  -COPY W0008  -PRE WDR5-                                              
037800     05  FILLER                  PIC X.                                   
037900     EJECT                                                                
038000*01  -COPY W0008  -PRE WDK7-                                              
038100     05  FILLER                  PIC X.                                   
038200     EJECT                                                                
038300*01  -COPY W0008  -PRE WDN6-                                              
038400     05  FILLER                  PIC X.                                   
038500     EJECT                                                                
038600 PROCEDURE DIVISION USING MSG-PCB ALT-PCB USEA-PCB                        
038700           WDK6-PCB ARTC2-PCB  BENA-PCB ARTM-PCB                          
038800           SATE-PCB  SATB-PCB 6321-PCB SAPC-PCB WDR5-PCB                  
038900           WDK7-PCB WDN6-PCB.                                             
039000 MAIN SECTION.                                                            
039100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
039200           WDK6-PCB ARTC2-PCB  BENA-PCB ARTM-PCB                          
039300           SATE-PCB  SATB-PCB 6321-PCB SAPC-PCB WDR5-PCB                  
039400           WDK7-PCB WDN6-PCB.                                             
039500                                                                          
039600     PERFORM IMS-GET-MSG                                                  
039700     IF SEGMENT-FINNS                                                     
039800        PERFORM A-INIT-SPARA-INPUT                                        
039900        IF IDARTNR-WS NUMERIC                                             
040000           MOVE IDARTNR-WS               TO W-IDARTNR                     
040100           PERFORM IMS-GU-WDK601                                          
040200           IF SEGMENT-FINNS                                               
040201             MOVE ART-KDPRODSL           TO TEST-KDPRODSL                 
040300             MOVE ART-IDLEVNR            TO WS-IDLEVNR-8                  
040400             IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                    
040500             OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE              
040600*              --- OK, USER HAS NO RESTRICTIONS                           
040800               IF ART-KDERS-UTG > ZERO                                    
040900                  MOVE MEDDELANDE-14     TO MOD-TEMFSFEL                  
041000               ELSE                                                       
041100                  IF ART-FLIART = 'J'                                     
041200                      MOVE JA            TO INGAR-SATS-SW                 
041300                  ELSE                                                    
041400                      MOVE NEJ           TO INGAR-SATS-SW                 
041500                  END-IF                                                  
041600                  IF (MFS-UPDATE OR MFS-UPD-V)                            
041610                  AND EGEN-BILD                                           
041700                     MOVE ART-FLERS   TO WS-FLERS                         
041800                     PERFORM B-LAES-SKROTINFO                             
041900                     PERFORM C-KOLLA-INDATA                               
042000                     IF INDATA-OK                                         
042100                        PERFORM D-UPPDATERA                               
042200                     ELSE                                                 
042201                        IF MOD-TEMFSFEL <= SPACE                          
042300                           MOVE MEDDELANDE-6 TO MOD-TEMFSFEL              
042400                        END-IF                                            
042410                     END-IF                                               
042500                  ELSE                                                    
042600                     PERFORM E-VISA-BILD                                  
042610                     IF KDPRODSL-LYNK                                     
042620                       MOVE MEDDELANDE-7 TO MOD-TEMFSINF                  
042630                     END-IF                                               
042700                  END-IF                                                  
042800               END-IF                                                     
042900             ELSE                                                         
043000*               --- USER RESTRICTIONS IN EFFECT                           
043100                MOVE FEL-6 (1) TO MOD-TEMFSFEL                            
043200             END-IF                                                       
043300           ELSE                                                           
043400              MOVE MEDDELANDE-2          TO MOD-TEMFSFEL                  
043500           END-IF                                                         
043600        ELSE                                                              
043700           MOVE MEDDELANDE-1          TO MOD-TEMFSFEL                     
043800        END-IF                                                            
043900        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O12901 + 4                     
044000        PERFORM IMS-INSERT-MSG                                            
044100     END-IF                                                               
044200     MOVE ZERO TO RETURN-CODE                                             
044300     GOBACK                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 A-INIT-SPARA-INPUT SECTION.                                              
044700*                                                                         
044800     IF MSG-DUBBLA-TRANSKODER                                             
044900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12901                
045000        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
045100                                              WS-IDTRANS                  
045200        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
045300        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
045400     ELSE                                                                 
045500        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I12901                
045600        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
045700                                              WS-IDTRANS                  
045800        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
045900     END-IF                                                               
046000                                                                          
046100     MOVE LOW-VALUE                        TO MOD-W2O12901                
046200     MOVE '2129'                           TO MOD-IDTRANS                 
046300     MOVE 'W2O12901'                       TO MFS-IDMOD                   
046400                                                                          
046500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046600     MOVE '001'             TO MSGI-KDCALL                                
046700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046800     MOVE '2129'               TO MSGI-IDTRANS                            
046900     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
047000     MOVE '11'                 TO MSGI-IDDC-KEY                           
047100     IF MFS-IDTRANS = '2129'                                              
047200     OR (MID-IDARTNR-IN NUMERIC                                           
047300     AND MID-IDARTNR-IN > ZERO)                                           
047400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
047500     END-IF                                                               
047600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047700     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
047800     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
047900                                                                          
048000     MOVE IDARTNR-WS                       TO MOD-IDARTNR-UT              
048100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
048200                                                                          
048300                                                                          
048400     MOVE MID-C2-FAELT-SW                  TO MOD-C2FAELT-SW              
048500                                                                          
048600     MOVE MFS-RENSA-FAELT                  TO MOD-IDARTNR-IN              
048700                                              MOD-TEMFSFEL                
048800                                              MOD-TEMFSINF                
048900     ACCEPT WS-DAGENS-AAMMDD FROM DATE                                    
049000                                                                          
049100     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
049200     .                                                                    
049300     EJECT                                                                
049400 B-LAES-SKROTINFO SECTION.                                                
049500     MOVE 'B-LAES-SKROTINFO '  TO CURRENT-SECTION                         
049600                                                                          
049700     PERFORM IMS-GNP-WDK611                                               
049800     MOVE CLAG-FLSKROT-BEORD       TO WCLAG-FLSKROT-BEORD-CDC             
049900                                                                          
050000         MOVE CLAG-KDERS           TO WARTC91-KDERS-C1                    
050100         MOVE CLAG-KVLS            TO WARTC91-KVLS-C1                     
050200         MOVE CLAG-KVRESS          TO WARTC91-KVRESS-C1                   
050300         MOVE CLAG-IDANSK          TO WARTC91-IDANSK                      
050400                                                                          
050500         MOVE ZERO                 TO WARTC91-KVLS-C2                     
050600                                      WARTC91-KVRESS-C2                   
050700                                      WARTC91-KDERS-C2                    
050800                                                                          
050900         PERFORM IMS-GU-ARTM01                                            
051000         IF SEGMENT-FINNS                                                 
051100                                                                          
051200            COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                 
051300                                  ARTM-ART-KVOKS-DAG    +                 
051400                                  ARTM-ART-KVOKS-VOR                      
051500                                                                          
051600         ELSE                                                             
051700            MOVE ZERO              TO WS-KVOKS-C1                         
051800         END-IF                                                           
051900     .                                                                    
052000     EJECT                                                                
052100 C-KOLLA-INDATA SECTION.                                                  
052200     MOVE 'C-KOLLA-INDATA '  TO CURRENT-SECTION                           
052300                                                                          
052400     MOVE JA                      TO INDATA-SW                            
052500                                                                          
052600     MOVE MFS-NUM-FAELT-RAETT     TO MOD-KDERS-ATTR                       
052700                                     MOD-KVKVAR-ATTR                      
052800                                     MOD-IDKONTO-ATTR                     
052900                                     MOD-TISKROT-AUTO-IN-ATTR             
053000     MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDANALYS-ATTR                    
053100                                     MOD-FLSKROT-CLASS-ATTR               
053200                                     MOD-BELAGINS30-ATTR                  
053210                                     MOD-FLSKRSP-C1-VISN-ATTR             
053300                                                                          
053400     MOVE MFS-ROER-EJ-FAELT       TO MOD-KDERS                            
053500                                     MOD-KVKVAR                           
053600                                     MOD-IDKONTO                          
053700                                     MOD-IDANALYS                         
053800                                     MOD-FLSKROT-CLASS                    
053900                                     MOD-BELAGINS30                       
054000                                     MOD-TISKROT-AUTO-IN                  
054010                                     MOD-FLSKRSP-C1-VISN                  
054100     MOVE MFS-STAENG-FAELT        TO                                      
054200                                     MOD-KDCLAGER-ATTR                    
054300     MOVE MFS-RENSA-FAELT         TO                                      
054400                                     MOD-KDCLAGER                         
054401*    IF KDPRODSL-LYNK                                                     
054402*      MOVE MFS-STAENG-FAELT        TO MOD-FLSKROT-CLASS-ATTR             
054403*    END-IF                                                               
054410                                                                          
054430     IF MFS-UPD-V                                                         
054431        IF  MID-KDERS            = ALL '+'                                
054432        AND MID-TISKROT-AUTO-IN  = ALL '+'                                
054433        AND MID-KVKVAR           = ALL '+'                                
054434        AND MID-IDKONTO          = ALL '+'                                
054435        AND MID-FLSKROT-CLASS    = ALL '+'                                
054437        AND MID-IDANALYS         = ALL '+'                                
054440          IF MID-FLSKRSP-C1-VISN = JA                                     
054450          OR MID-FLSKRSP-C1-VISN = NEJ                                    
054460             CONTINUE                                                     
054500          ELSE                                                            
054501             MOVE NEJ           TO INDATA-SW                              
054502             MOVE MFS-ALFA-FAELT-FEL                                      
054503                                TO MOD-FLSKRSP-C1-VISN-ATTR               
054504          END-IF                                                          
054505        ELSE                                                              
054506           IF MID-KDERS            NOT = ALL '+'                          
054507              MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR                  
054508           END-IF                                                         
054509           IF MID-TISKROT-AUTO-IN  NOT = ALL '+'                          
054510              MOVE MFS-NUM-FAELT-FEL                                      
054511                                  TO MOD-TISKROT-AUTO-IN-ATTR             
054512           END-IF                                                         
054513           IF MID-KVKVAR           NOT = ALL '+'                          
054514              MOVE MFS-NUM-FAELT-FEL                                      
054515                                  TO MOD-KVKVAR-ATTR                      
054516           END-IF                                                         
054517           IF MID-IDKONTO          NOT = ALL '+'                          
054518              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                
054519           END-IF                                                         
054520           IF MID-FLSKROT-CLASS    NOT = ALL '+'                          
054521              MOVE MFS-ALFA-FAELT-FEL                                     
054522                                  TO MOD-FLSKROT-CLASS-ATTR               
054523           END-IF                                                         
054524           IF MID-IDANALYS         NOT = ALL '+'                          
054525              MOVE MFS-ALFA-FAELT-FEL                                     
054526                                  TO MOD-IDANALYS-ATTR                    
054527           END-IF                                                         
054528           MOVE NEJ             TO INDATA-SW                              
054529           MOVE MEDDELANDE-17   TO MOD-TEMFSFEL                           
054530        END-IF                                                            
054535     ELSE                                                                 
054540                                                                          
054550      IF MID-FLSKRSP-C1-VISN NOT = WCLAG-FLSKROT-BEORD-CDC                
054580         MOVE NEJ                TO INDATA-SW                             
054590         MOVE MEDDELANDE-16      TO MOD-TEMFSFEL                          
054591      END-IF                                                              
054592                                                                          
054600      IF MID-KDERS NOT = ALL '+'                                          
054700         IF MID-KDERS NOT = '09'                                          
054800            MOVE NEJ                 TO INDATA-SW                         
054900            MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR                    
055000         ELSE                                                             
055100            IF INGAR-I-SATS                                               
055200               MOVE IDARTNR-WS         TO W-RAD-IDARTNR-MIN               
055300                                           W-RAD-IDARTNR-MAX              
055400               PERFORM IMS-GU-SATE01                                      
055500               PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-SW = NEJ            
055600                 IF SATE01-SEQC-IDARTNR-STR < +100000000                  
055700                    MOVE SATE01-SEQC-IDARTNR-STR                          
055800                                              TO W-IDARTNR-STR            
055900                                                 W-IDARTNR                
056000                    MOVE SATE01-SEQC-IDRADNR                              
056100                                              TO W-IDRADNR                
056200                    MOVE SATE01-SEQC-KDSTRRAD                             
056300                                              TO W-KDSTRRAD               
056400                    PERFORM IMS-GU-SATB01                                 
056500                    IF SATB01-STR-TIBORT  = ZERO                          
056600                    AND SATB01-STR-IDLEVNR = '1002'                       
056700                       PERFORM IMS-GU-SATB11                              
056800                       MOVE SATB11-RAD-TISTODAT  TO TMP1-YYMMDD           
056900                       MOVE WS-DAGENS-AAMMDD     TO TMP2-YYMMDD           
057000                       PERFORM WY2000P1                                   
057100                       IF TMP1-YYMMDD > TMP2-YYMMDD                       
057200                           PERFORM IMS-GU-ARTC01-PCB2                     
057300                           PERFORM IMS-GNP-ARTC11-PCB2                    
057400                           IF CLAG-KDERS < 09                             
057500*********************   MID-ARTIKEL INGÅR I SATS-ARTIKEL SOM INTE         
057600********************    ÄR ERSÄTTNINGSMÄRKT, DÅ FÅR MAN EJ 09-MÄRK        
057700********************    MID-ARTIKEL                                       
057800                              MOVE NEJ    TO INDATA-SW                    
057900                              MOVE MFS-NUM-FAELT-FEL                      
058000                                               TO MOD-KDERS-ATTR          
058100                           END-IF                                         
058200                       END-IF                                             
058300                    END-IF                                                
058400                 END-IF                                                   
058500                 PERFORM IMS-GN-SATE01                                    
058600               END-PERFORM                                                
058700            ELSE                                                          
058800                IF WARTC91-KDERS-C1 NOT = 0                               
058900                   MOVE NEJ              TO INDATA-SW                     
059000                   MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-ATTR               
059100                END-IF                                                    
059200            END-IF                                                        
059300         END-IF                                                           
059400      ELSE                                                                
059500         MOVE MFS-RENSA-FAELT      TO MOD-KDERS                           
059600      END-IF                                                              
059700                                                                          
059800      IF MID-TISKROT-AUTO-IN NOT = ALL '+'                                
059900         IF MID-TISKROT-AUTO-IN NOT NUMERIC                               
060000            MOVE NEJ                TO INDATA-SW                          
060100            MOVE MFS-NUM-FAELT-FEL  TO MOD-TISKROT-AUTO-IN-ATTR           
060200         ELSE                                                             
060300            IF MID-TISKROT-AUTO-IN < WS-DAGENS-AAMMDD                     
060400              MOVE NEJ              TO INDATA-SW                          
060500              MOVE MFS-NUM-FAELT-FEL TO MOD-TISKROT-AUTO-IN-ATTR          
060600            END-IF                                                        
060700         END-IF                                                           
060800      ELSE                                                                
060900         MOVE MFS-RENSA-FAELT        TO MOD-TISKROT-AUTO-IN               
061000      END-IF                                                              
061100                                                                          
061200      IF MID-KDERS = ALL '+'                                              
061300         CONTINUE                                                         
061400      ELSE                                                                
061500         CONTINUE                                                         
061600***      IF INDATA-OK                                                     
061700***         PERFORM CA-KOLLA-IDUSER                                       
061800***         IF INDATA-OK                                                  
061900***            CONTINUE                                                   
062000***         ELSE                                                          
062100***            MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-ATTR                   
062200***            MOVE FEL-7(1) TO MOD-TEMFSINF                              
062300***         END-IF                                                        
062400***      END-IF                                                           
062500      END-IF                                                              
062510                                                                          
062600      IF MID-KVKVAR NOT = ALL '+'                                         
062700        IF MID-KVKVAR NUMERIC                                             
062800          IF (WCLAG-FLSKROT-BEORD-CDC = JA )                              
062900              MOVE NEJ  TO INDATA-SW                                      
063000              MOVE MFS-NUM-FAELT-FEL TO                                   
063100                                    MOD-KVKVAR-ATTR                       
063200          END-IF                                                          
063300                                                                          
063400          MOVE MID-KVKVAR TO WKVKVAR                                      
063500          IF (WARTC91-KVLS-C1 -                                           
063600              WS-KVOKS-C1     -                                           
063700              WARTC91-KVRESS-C1) NOT > WKVKVAR-N                          
063800              MOVE MFS-NUM-FAELT-FEL TO                                   
063900                                     MOD-KVKVAR-ATTR                      
064000              MOVE NEJ               TO INDATA-SW                         
064100          END-IF                                                          
064200          IF INDATA-OK                                                    
064300             PERFORM CA-KOLLA-IDUSER                                      
064400             IF INDATA-OK                                                 
064500                CONTINUE                                                  
064600             ELSE                                                         
064700                MOVE MFS-NUM-FAELT-FEL                                    
064800                             TO MOD-KVKVAR-ATTR                           
064900                MOVE FEL-7(1) TO MOD-TEMFSINF                             
065000             END-IF                                                       
065100          END-IF                                                          
065200          IF MID-FLSKROT-CLASS = JA                                       
065300             IF MID-KVKVAR = ZERO                                         
065400                CONTINUE                                                  
065500             ELSE                                                         
065600               MOVE NEJ TO INDATA-SW                                      
065700               MOVE MFS-NUM-FAELT-FEL TO                                  
065800                           MOD-KVKVAR-ATTR                                
065900               MOVE MFS-ALFA-FAELT-FEL TO                                 
066000                           MOD-FLSKROT-CLASS-ATTR                         
066100             END-IF                                                       
066200          END-IF                                                          
066300        ELSE                                                              
066400          MOVE NEJ                  TO INDATA-SW                          
066500          MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKVAR-ATTR                    
066600        END-IF                                                            
066700      ELSE                                                                
066800        MOVE MFS-RENSA-FAELT        TO MOD-KVKVAR                         
066900      END-IF                                                              
067000                                                                          
067100      IF MID-IDKONTO NOT = ALL '+'                                        
067200        IF MID-IDKONTO NUMERIC                                            
067300            MOVE MID-IDKONTO         TO MOD-IDKONTO                       
067400        ELSE                                                              
067500            MOVE NEJ                 TO INDATA-SW                         
067600            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                  
067700        END-IF                                                            
067800      ELSE                                                                
067900        MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO                        
068000      END-IF                                                              
068100                                                                          
068200      IF MID-IDANALYS NOT = ALL '+'                                       
068300        MOVE MID-IDANALYS           TO MOD-IDANALYS                       
068400      ELSE                                                                
068500        MOVE MFS-RENSA-FAELT        TO MOD-IDANALYS                       
068600      END-IF                                                              
068700                                                                          
068800      IF MID-FLSKROT-CLASS NOT = ALL '+' AND                              
068900        MID-FLSKROT-CLASS NOT = SPACE                                     
069000        IF MID-FLSKROT-CLASS = JA                                         
069100           MOVE MID-FLSKROT-CLASS   TO MOD-FLSKROT-CLASS                  
069200        ELSE                                                              
069300           MOVE NEJ                 TO INDATA-SW                          
069400           MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSKROT-CLASS-ATTR             
069500        END-IF                                                            
069600      ELSE                                                                
069700        MOVE MFS-RENSA-FAELT        TO MOD-FLSKROT-CLASS                  
069800      END-IF                                                              
069900                                                                          
070000      IF MID-IDKONTO  NOT = ALL '+' AND MID-IDKONTO NUMERIC               
070100        MOVE 'SEPV'               TO SAP-KDTRADP                          
070200        MOVE MID-IDKONTO          TO SAP-IDKONTO                          
070300        MOVE SPACE                TO SAP-IDKST                            
070400        IF MID-IDANALYS = ALL '+'                                         
070500          MOVE SPACE              TO SAP-IDANALYS                         
070600        ELSE                                                              
070700          MOVE MID-IDANALYS       TO SAP-IDANALYS                         
070800        END-IF                                                            
070900        MOVE ZERO                 TO SAP-IDDISTR                          
071000        MOVE SPACE                TO SAP-KDFAKTYP                         
071100        MOVE ZERO                 TO SAP-IDFTG                            
071200        MOVE SPACE                TO SAP-IDPROFIT                         
071300        MOVE +2                   TO SAP-KDCALL                           
071400                                                                          
071500        CALL W411SAP USING SAP-W411SAP SAPC-PCB                           
071600                                                                          
071700        IF SAP-BEFEL NOT = SPACE                                          
071800          MOVE SAP-BEFEL            TO MOD-TEMFSFEL                       
071900          MOVE NEJ                  TO INDATA-SW                          
072000          IF SAP-IDKONTO-OK = NEJ                                         
072100            MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-ATTR                   
072200          ELSE                                                            
072300            IF SAP-IDANALYS-OK = NEJ                                      
072400              MOVE MFS-ALFA-FAELT-FEL                                     
072500                                    TO MOD-IDANALYS-ATTR                  
072600            END-IF                                                        
072700          END-IF                                                          
072800        END-IF                                                            
072900      ELSE                                                                
073000        IF MID-IDANALYS NOT = ALL '+' OR                                  
073100           MID-IDKONTO  NOT = ALL '+'                                     
073200           MOVE NEJ                 TO INDATA-SW                          
073300           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                   
073400           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDANALYS-ATTR                  
073500        END-IF                                                            
073600      END-IF                                                              
073700                                                                          
073800      IF MID-FLSKROT-CLASS = JA AND                                       
073900       (MID-IDKONTO  NOT = ALL '+' OR                                     
074000        MID-IDANALYS NOT = ALL '+')                                       
074100        MOVE NEJ                 TO INDATA-SW                             
074200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
074300        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDANALYS-ATTR                     
074400        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSKROT-CLASS-ATTR                
074500      END-IF                                                              
074600                                                                          
074700      IF MID-FLSKROT-CLASS = JA                                           
074800        IF KDPRODSL-VCBV                                                  
074900          MOVE NEJ                 TO INDATA-SW                           
075000          MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSKROT-CLASS-ATTR              
075100        END-IF                                                            
075200      END-IF                                                              
075300                                                                          
075400      IF MID-BELAGINS30 NOT = ALL '+'                                     
075500        MOVE MID-BELAGINS30         TO MOD-BELAGINS30                     
075600      ELSE                                                                
075700        MOVE MFS-RENSA-FAELT        TO MOD-BELAGINS30                     
075800      END-IF                                                              
075810     END-IF                                                               
075900                                                                          
076000     IF INDATA-OK                                                         
076100        CONTINUE                                                          
076200     ELSE                                                                 
076300        PERFORM S01-ROER-EJ-FAELT                                         
076400     END-IF                                                               
076500                                                                          
076600     .                                                                    
076700     EJECT                                                                
076800 CA-KOLLA-IDUSER SECTION.                                                 
076900     MOVE 'CA-KOLLA-IDUSER '  TO CURRENT-SECTION                          
077000                                                                          
077100     MOVE WC-CDC-SE TO W-IDDC-6327                                        
077200     MOVE 'ANSK'    TO W-KDARBTYP-6327                                    
077300     MOVE SPACE     TO WS-SPAR-BEANST-GODK                                
077400     PERFORM IMS-GU-WDR501-6327                                           
077500     IF SEGMENT-FINNS                                                     
077600        MOVE MSG-SIGNON-USERID TO W-IDUSER-GODK                           
077700        PERFORM IMS-GHNP-WDGX6328                                         
077800        IF SEGMENT-FINNS                                                  
077900           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
078000        ELSE                                                              
078100           MOVE NEJ TO INDATA-SW                                          
078200        END-IF                                                            
078300     ELSE                                                                 
078400        MOVE NEJ TO INDATA-SW                                             
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 D-UPPDATERA SECTION.                                                     
078900     MOVE 'D-UPPDATERA '   TO CURRENT-SECTION                             
079000                                                                          
079100     PERFORM S01-ROER-EJ-FAELT                                            
079200     MOVE SPACE                     TO WMESSAGE                           
079300     MOVE IDARTNR-WS TO W-IDARTNR                                         
079400     IF  MID-TISKROT-AUTO-IN NOT = ALL '+'                                
079500         PERFORM DE-UPPD-TISKROT-AUTO                                     
079600     END-IF                                                               
079700     IF  MID-KVKVAR NOT = ALL '+'                                         
079800         PERFORM DA-SKAPA-KVSKRANT                                        
079900         PERFORM DB-UPPD-SKROTSPARR                                       
080000         PERFORM DC-SKAPA-HANDELSETR-6321                                 
080100         MOVE MEDDELANDE-10         TO WMESSAGE-1                         
080200     END-IF                                                               
080300                                                                          
080400     IF  MID-KDERS NOT = ALL '+'                                          
080500         PERFORM DD-SKAPA-TRANS-TILL-1113                                 
080600         MOVE MEDDELANDE-12         TO WMESSAGE-2                         
080700     END-IF                                                               
080710                                                                          
080711     IF MID-FLSKRSP-C1-VISN NOT = WCLAG-FLSKROT-BEORD-CDC                 
080712       PERFORM IMS-GU-WDK601                                              
080713       PERFORM IMS-GHNP-WDK611                                            
080714       IF SEGMENT-FINNS                                                   
080715         MOVE MID-FLSKRSP-C1-VISN   TO CLAG-FLSKROT-BEORD                 
080716                                       MOD-FLSKRSP-C1-VISN                
080717         PERFORM IMS-REPL-WDK611                                          
080718         MOVE MEDDELANDE-15         TO WMESSAGE-1                         
080730       END-IF                                                             
080740     END-IF                                                               
080800                                                                          
080900     MOVE WMESSAGE                  TO MOD-TEMFSINF                       
081000                                                                          
081100     MOVE  MFS-RENSA-FAELT          TO MOD-KDERS                          
081200                                       MOD-KDCLAGER                       
081300                                       MOD-KVKVAR                         
081400                                       MOD-IDKONTO                        
081500                                       MOD-IDANALYS                       
081600                                       MOD-FLSKROT-CLASS                  
081700                                       MOD-BELAGINS30                     
081800                                       MOD-TISKROT-AUTO-IN                
081900                                                                          
082000     MOVE MFS-FORMATETS-ATTR        TO MOD-KDERS-ATTR                     
082100                                       MOD-KVKVAR-ATTR                    
082200                                       MOD-IDKONTO-ATTR                   
082300                                       MOD-IDANALYS-ATTR                  
082400                                       MOD-FLSKROT-CLASS-ATTR             
082500                                       MOD-BELAGINS30-ATTR                
082600                                       MOD-TISKROT-AUTO-IN-ATTR           
082700                                                                          
082800     MOVE MFS-STAENG-FAELT          TO MOD-KDCLAGER-ATTR                  
082900                                                                          
083000                                                                          
083100     .                                                                    
083200     EJECT                                                                
083300 DA-SKAPA-KVSKRANT SECTION.                                               
083400     MOVE 'DA-SKAPA-KVSKRANT '  TO CURRENT-SECTION                        
083500                                                                          
083600     MOVE MID-KVKVAR TO WKVKVAR                                           
083700     COMPUTE WKVSKRANT =                                                  
083800               WARTC91-KVLS-C1 - WARTC91-KVRESS-C1                        
083900                               - WS-KVOKS-C1                              
084000                               - WKVKVAR-N                                
084100     MOVE WKVSKRANT TO MOD-KVSKRANT                                       
084200     .                                                                    
084300     EJECT                                                                
084400 DB-UPPD-SKROTSPARR SECTION.                                              
084500     MOVE 'DB-UPPD-SKROTSPARR '  TO CURRENT-SECTION                       
084600                                                                          
084700*-- RÖRELSEINDIKATORN SKALL SÄTTAS TILL NEJ PÅ WDK629 FÖR                 
084800*-- REFILL-ARTIKEL PÅ CDC.                                                
084900                                                                          
085000     MOVE JA TO MOD-FLSKRSP-C1-VISN                                       
085100     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSKRSP-C1-VISN-ATTR               
085200                                                                          
085300     PERFORM IMS-GU-WDK601                                                
085400     PERFORM IMS-GHNP-WDK611                                              
085500     IF SEGMENT-FINNS                                                     
085600       MOVE JA TO CLAG-FLSKROT-BEORD                                      
085700       PERFORM IMS-REPL-WDK611                                            
085800                                                                          
085900       MOVE CLAG-IDDC-REF    TO WS-IDDC-REF                               
086000                                                                          
086100       PERFORM IMS-GHNP-WDK627                                            
086200                                                                          
086300       IF SEGMENT-FINNS                                                   
086400         ACCEPT W-DATUM-DATE FROM DATE                                    
086500         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
086600                                                                          
086700             MOVE SKROT-TISKROT-BEORD TO MOD-TISKROT-BEORD-C1             
086800             MOVE SKROT-DASKROT (3:6)  TO MOD-TISKROT-C1                  
086900             MOVE SKROT-KVSKROT        TO MOD-KVSKROT-C1                  
087000             MOVE MFS-RENSA-FAELT      TO MOD-TISKROT-BEORD-C2            
087100                                          MOD-TISKROT-C2                  
087200                                          MOD-KVSKROT-C2                  
087300         PERFORM IMS-REPL-SKROT                                           
087400       ELSE                                                               
087500         MOVE ZERO                    TO SKROT-KVSKROT                    
087600         MOVE ZERO                    TO SKROT-DASKROT                    
087700         ACCEPT W-DATUM-DATE FROM DATE                                    
087800         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
087900                                                                          
088000             MOVE SKROT-TISKROT-BEORD  TO MOD-TISKROT-BEORD-C1            
088100             MOVE MFS-ROER-EJ-FAELT    TO MOD-TISKROT-C1                  
088200                                          MOD-KVSKROT-C1                  
088300             MOVE MFS-RENSA-FAELT      TO MOD-TISKROT-BEORD-C2            
088400                                          MOD-TISKROT-C2                  
088500                                          MOD-KVSKROT-C2                  
088600                                                                          
088700         PERFORM IMS-ISRT-WDK627                                          
088800       END-IF                                                             
088900                                                                          
089000       IF WS-IDDC-REF NOT = SPACE                                         
089100         PERFORM IMS-GHNP-WDK629                                          
089200         IF SEGMENT-FINNS                                                 
089300           IF CREF-FLREFNYO = JA                                          
089400             MOVE NEJ   TO CREF-FLREFNYO                                  
089500             PERFORM IMS-REPL-WDK629                                      
089600           END-IF                                                         
089700         END-IF                                                           
089800       END-IF                                                             
089900     END-IF                                                               
090000                                                                          
090100     .                                                                    
090200     EJECT                                                                
090300 DC-SKAPA-HANDELSETR-6321 SECTION.                                        
090400     MOVE 'DC-SKAPA-HANDELSETR-6321 ' TO CURRENT-SECTION                  
090500                                                                          
090600     MOVE WC-CDC-SE         TO W-IDDC-6324                                
090700     MOVE 'ANSK'            TO W-KDARBTYP                                 
090800     PERFORM IMS-GU-WDR501-6321                                           
090900     IF SEGMENT-SAKNAS                                                    
091000        MOVE '6321'         TO 6321-IDHTYP                                
091100        MOVE 'ANSK'         TO 6321-KDARBTYP                              
091200        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
091300        PERFORM IMS-ISRT-WDR501-6321                                      
091400     END-IF                                                               
091500                                                                          
091600     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
091700     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
091800     PERFORM IMS-ISRT-WDGX6322                                            
091900                                                                          
092000     MOVE IDARTNR-WS        TO 6324-IDARTNR                               
092100     MOVE WC-CDC-SE         TO 6324-IDDC                                  
092200     MOVE 1                 TO 6324-KDSTASKR                              
092300     MOVE NEJ               TO 6324-FLSKROT-GODK                          
092400     MOVE WARTC91-IDANSK    TO 6324-IDPERSON                              
092500     MOVE 81                TO 6324-IDDISTR                               
092600     MOVE 11                TO 6324-IDKUNDNR                              
092700     MOVE MSG-SIGNON-USERID TO 6324-IDUSER                                
092800     MOVE ZERO              TO 6324-KDFRAKT                               
092900     MOVE 1                 TO 6324-KDORDKL                               
093000     MOVE WKVSKRANT         TO 6324-KVSKROT-BEORD                         
093100     MOVE SPACE             TO 6324-IDKST                                 
093200     MOVE SPACE             TO 6324-IDANALYS                              
093300                               6324-FLJUSTBUFF                            
093400     MOVE ZERO              TO 6324-IDKONTO                               
093500                               6324-KVSKROT-KVAR                          
093600     MOVE WKVKVAR-N           TO 6324-KVSKROT-ONDEM                       
093700     MOVE WS-SPAR-BEANST-GODK TO 6324-BEANST                              
093800                                                                          
093900     IF MID-IDKONTO NOT = ALL '+'                                         
094000        MOVE MID-IDKONTO    TO WS-IDKONTO                                 
094100        MOVE WS-IDKONTO     TO 6324-IDKONTO                               
094200     END-IF                                                               
094300     IF MID-IDANALYS NOT = ALL '+'                                        
094400        MOVE MID-IDANALYS   TO 6324-IDANALYS                              
094500     END-IF                                                               
094600     MOVE WKVKVAR-N         TO WS-BELAGINS-KVSKROT-KVAR                   
094700     IF MID-BELAGINS30 NOT = ALL '+'                                      
094800        MOVE MID-BELAGINS30 TO WS-BELAGINS-DEL2                           
094900     END-IF                                                               
095000     MOVE WS-BELAGINS-DEL   TO 6324-BELAGINS-DEL                          
095100     IF MID-FLSKROT-CLASS = JA                                            
095200        MOVE 482319         TO 6324-IDKONTO                               
095300        MOVE '158600000635' TO 6324-IDANALYS                              
095400        MOVE 111            TO 6324-IDKUNDNR                              
095500     END-IF                                                               
095600********************************************                              
095700     MOVE CLAG-KDERS        TO 6324-KDERS-UTG                             
095800     PERFORM DCA-LAS-FLYTTA-WDK7                                          
095900     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
096000            CLAG-KVLS     - CLAG-KVRESS                                   
096100                          - CLAG-KVROS                                    
096200                          - WS-KVOKS-C1                                   
096300                                                                          
096400     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
096500            W-SDC-KVLS - W-SDC-KVOKS                                      
096600                                                                          
096700     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
096800          CLAG-KVAKS-CDC  + CLAG-KVAKS-PAV                                
096900                          + CLAG-KVAKS-T                                  
097000                                                                          
097100     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
097200          W-SDC-KVAKS                                                     
097300                                                                          
097400     PERFORM IMS-GU-ARTM01                                                
097500     IF SEGMENT-FINNS                                                     
097600       COMPUTE 6324-SUTPO-TOT =                                           
097700                ARTM-ART-SUTPO-TOT                                        
097800     ELSE                                                                 
097900       MOVE ZERO              TO 6324-SUTPO-TOT                           
098000     END-IF                                                               
098100                                                                          
098200     PERFORM DCB-LAS-FLYTTA-WDN6                                          
098300********************************************                              
098400                                                                          
098500     PERFORM IMS-ISRT-WDGX6324                                            
098600     .                                                                    
098700     EJECT                                                                
098800********************************************                              
098900 DCA-LAS-FLYTTA-WDK7 SECTION.                                             
099000     MOVE 'DCA-LAS-FLYTTA-WDK7 '  TO CURRENT-SECTION                      
099100                                                                          
099200     PERFORM IMS-GU-WDK701                                                
099300     IF SEGMENT-FINNS                                                     
099400        MOVE ZERO             TO W-SDC-KVLS                               
099500                                 W-SDC-KVAKS                              
099600                                 W-SDC-KVOKS                              
099700        PERFORM IMS-GNP-WDK711                                            
099800        PERFORM UNTIL SEGMENT-SAKNAS                                      
099900          IF WC-CDC-SE = SLAG-IDDC                                        
100000             ADD SLAG-KVLS      TO W-SDC-KVLS                             
100100             ADD SLAG-KVAKS-SDC TO W-SDC-KVAKS                            
100200             ADD SLAG-KVAKS-PAV TO W-SDC-KVAKS                            
100300             ADD SLAG-KVOKS-DAG TO W-SDC-KVOKS                            
100400             ADD SLAG-KVOKS-BULK TO W-SDC-KVOKS                           
100500          END-IF                                                          
100600          PERFORM IMS-GNP-WDK711                                          
100700        END-PERFORM                                                       
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100********************************************                              
101200                                                                          
101300 DCB-LAS-FLYTTA-WDN6 SECTION.                                             
101400     MOVE 'DCB-LAS-FLYTTA-WDN6 '  TO CURRENT-SECTION                      
101500                                                                          
101600     MOVE +1 TO IX                                                        
101700     PERFORM UNTIL IX > 20                                                
101800        MOVE SPACE        TO 6324-BEEMBLEM (IX)                           
101900        ADD +1        TO IX                                               
102000     END-PERFORM                                                          
102100     PERFORM IMS-GU-WDN601                                                
102200     IF SEGMENT-FINNS                                                     
102300        PERFORM IMS-GNP-WDN611                                            
102400        MOVE +1 TO IX                                                     
102500        PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
102600           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (IX)                        
102700           ADD +1        TO IX                                            
102800           PERFORM IMS-GNP-WDN611                                         
102900        END-PERFORM                                                       
103000        IF SEGMENT-FINNS                                                  
103100           MOVE 'MORE' TO 6324-BEEMBLEM (20)                              
103200        END-IF                                                            
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600 DD-SKAPA-TRANS-TILL-1113 SECTION.                                        
103700     MOVE 'DD-SKAPA-TRANS-TILL-1113 ' TO CURRENT-SECTION                  
103800                                                                          
103900     MOVE IDARTNR-WS              TO PROGSW-MID-IDARTNR-UT                
104000     MOVE 1                       TO PROGSW-MID-DIERS-ERS                 
104100     MOVE 09                      TO PROGSW-MID-KDERS                     
104200     MOVE JA                      TO PROGSW-MID-FLKLAR                    
104300                                                                          
104400     MOVE ALL '+'                 TO PROGSW-MID-IDARTNR-IN                
104500                                     PROGSW-MID-IDAO                      
104600                                     PROGSW-MID-TIERSDAT-PREL             
104700                                     PROGSW-MID-TEARTNOT                  
104800                                                                          
104900     MOVE  1                      TO PROGSW-IX                            
105000     PERFORM UNTIL PROGSW-IX > 09                                         
105100        MOVE ALL '+'              TO PROGSW-MID-IDKORTNR                  
105200                                                     (PROGSW-IX)          
105300                                     PROGSW-MID-IDARTNR-TILLK             
105400                                                     (PROGSW-IX)          
105500                                     PROGSW-MID-DIERS-TILLK               
105600                                                     (PROGSW-IX)          
105700                                     PROGSW-MID-BEERS                     
105800                                                     (PROGSW-IX)          
105900        ADD 1                     TO PROGSW-IX                            
106000     END-PERFORM                                                          
106100                                                                          
106200     PERFORM IMS-ISRT-ALT-PCB                                             
106300     .                                                                    
106400     EJECT                                                                
106500 DE-UPPD-TISKROT-AUTO SECTION.                                            
106600     MOVE 'DE-UPPD-TISKROT-AUTO '  TO CURRENT-SECTION                     
106700                                                                          
106800     PERFORM IMS-GU-WDK601                                                
106900     PERFORM IMS-GHNP-WDK611                                              
107000     MOVE MID-TISKROT-AUTO-IN TO CLAG-TISKROT-AUTO                        
107100                                 MOD-TISKROT-AUTO-UT                      
107200     PERFORM IMS-REPL-WDK611                                              
107300                                                                          
107400     MOVE MEDDELANDE-15         TO WMESSAGE-1                             
107500     .                                                                    
107600     EJECT                                                                
107700 E-VISA-BILD  SECTION.                                                    
107800     MOVE 'E-VISA-BILD  '  TO CURRENT-SECTION                             
107900                                                                          
108000     PERFORM IMS-GNP-WDK611                                               
108100                                                                          
108200     IF SEGMENT-FINNS                                                     
108300       PERFORM EB-FLYTTA-MOD-DATA                                         
108400                                                                          
108500       PERFORM IMS-GNP-WDK627                                             
108600*-- MAN LÄSER BARA FÖRSTA POSTEN AV WDK627 I ALLA SKROT-PGM.              
108700       IF SEGMENT-FINNS                                                   
108800          MOVE SKROT-TISKROT-BEORD  TO MOD-TISKROT-BEORD-C1               
108900          MOVE SKROT-KVSKROT        TO MOD-KVSKROT-C1                     
109000          MOVE SKROT-DASKROT (3:6)  TO MOD-TISKROT-C1                     
109100          MOVE MFS-RENSA-FAELT      TO MOD-TISKROT-BEORD-C2               
109200                                       MOD-KVSKROT-C2                     
109300                                       MOD-TISKROT-C2                     
109400       END-IF                                                             
109500     ELSE                                                                 
109600       MOVE ZERO                    TO WARTC91-KVUTRS-C2                  
109700                                       WARTC91-KVUTRS-C1                  
109800                                       WARTC91-KVAKS-C1                   
109900     END-IF                                                               
110000                                                                          
110100     COMPUTE MOD-KVAK-C1 = WARTC91-KVAKS-C1                               
110200                                                                          
110300     MOVE IDARTNR-WS              TO W-IDARTNR                            
110400     PERFORM IMS-GU-WDK601                                                
110500     PERFORM IMS-GNP-WDK611                                               
110600     IF SEGMENT-FINNS                                                     
110700        MOVE CLAG-TISLUTKP       TO TISLUTKP-WS                           
110800                                                                          
110900        IF CLAG-TISLUTKP = ALL ZERO                                       
111000           MOVE ZERO            TO MOD-TISLUTKP-VISN                      
111100           MOVE CLAG-KVSLUTKP TO MOD-KVSLUTKP-VISN                        
111200        ELSE                                                              
111300           MOVE TISLUTKP-WS        TO TMP1-YYMMDD                         
111400           MOVE WS-DAGENS-AAMMDD   TO TMP2-YYMMDD                         
111500           PERFORM WY2000P1                                               
111600           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
111700              MOVE CLAG-KVSLUTKP     TO KVSLUTKP-WS                       
111800              MOVE KVSLUTKP-WS       TO MOD-KVSLUTKP-VISN                 
111900              MOVE TISLUTKP-WS       TO MOD-TISLUTKP-VISN                 
112000           ELSE                                                           
112100              MOVE CLAG-KVSLUTKP     TO KVSLUTKP-WS                       
112200              MOVE KVSLUTKP-WS       TO MOD-KVSLUTKP-VISN                 
112300              MOVE ZERO              TO MOD-TISLUTKP-VISN                 
112400           END-IF                                                         
112500        END-IF                                                            
112600     END-IF                                                               
112700                                                                          
112800     PERFORM IMS-GU-BENA01                                                
112900     IF SEGMENT-FINNS                                                     
113000       MOVE 'S  '                 TO W-IDSKYLT-KEY                        
113100       PERFORM IMS-GNP-BENA11                                             
113200       IF SEGMENT-FINNS                                                   
113300          MOVE BENA-TEXT-BEART    TO MOD-BEN                              
113400       END-IF                                                             
113500     END-IF                                                               
113600                                                                          
113700     PERFORM EA-KOLLA-REDIGERING                                          
113800                                                                          
113900     IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                          
114000        MOVE MEDDELANDE-5           TO MOD-TEMFSINF                       
114100        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDERS-ATTR                     
114200                                       MOD-KVKVAR-ATTR                    
114300                                       MOD-IDKONTO-ATTR                   
114400                                       MOD-IDANALYS-ATTR                  
114500                                       MOD-FLSKROT-CLASS-ATTR             
114600                                       MOD-BELAGINS30-ATTR                
114700                                                                          
114800        MOVE MFS-ROER-EJ-FAELT      TO MOD-KDERS                          
114900                                       MOD-KVKVAR                         
115000                                       MOD-IDKONTO                        
115100                                       MOD-IDANALYS                       
115200                                       MOD-FLSKROT-CLASS                  
115300                                       MOD-BELAGINS30                     
115310                                       MOD-TISKROT-AUTO-IN                
115400        MOVE MFS-STAENG-FAELT       TO                                    
115500                                       MOD-KDCLAGER-ATTR                  
115600        MOVE MFS-RENSA-FAELT        TO                                    
115700                                        MOD-KDCLAGER                      
115800     ELSE                                                                 
115900        PERFORM S02-RENSA-MOD-INMATNINGSFAELT                             
116000     END-IF                                                               
116010*    IF KDPRODSL-LYNK                                                     
116020*      MOVE MFS-STAENG-FAELT        TO MOD-FLSKROT-CLASS-ATTR             
116030*    END-IF                                                               
116100     MOVE MFS-ADD-SAETT-CURSOR  TO MOD-KDERS-ATTR                         
116200     .                                                                    
116300     EJECT                                                                
116400 EA-KOLLA-REDIGERING SECTION.                                             
116500     MOVE 'EA-KOLLA-REDIGERING  '  TO CURRENT-SECTION                     
116600                                                                          
116700     IF  WARTC91-KVUTRS-C1 > 0                                            
116800         MOVE ' CDC   '         TO MED4-CLAGER                            
116900         MOVE MEDDELANDE-4      TO MOD-TEMFSFEL                           
117000     END-IF                                                               
117100     IF  WCLAG-FLSKROT-BEORD-CDC = JA                                     
117200         MOVE ' CDC   '         TO MED3-CLAGER                            
117300         MOVE MEDDELANDE-3      TO MOD-TEMFSFEL                           
117400     END-IF                                                               
117500     .                                                                    
117600     EJECT                                                                
117700 EB-FLYTTA-MOD-DATA  SECTION.                                             
117800     MOVE 'EB-FLYTTA-MOD-DATA  '  TO CURRENT-SECTION                      
117900                                                                          
118000     MOVE CLAG-FLSKROT-BEORD  TO MOD-FLSKRSP-C1-VISN                      
118100                                 WCLAG-FLSKROT-BEORD-CDC                  
118200                                                                          
118300     MOVE CLAG-KDLTK          TO MOD-KDLTK                                
118400                                                                          
118500     MOVE MFS-RENSA-FAELT     TO MOD-KDUART                               
118600                                                                          
118700     MOVE CLAG-KVUTRS         TO WARTC91-KVUTRS-C1                        
118800     MOVE CLAG-KVAKS-CDC      TO WARTC91-KVAKS-C1                         
118900     ADD  CLAG-KVAKS-PAV      TO WARTC91-KVAKS-C1                         
119000     ADD  CLAG-KVAKS-T        TO WARTC91-KVAKS-C1                         
119100     MOVE CLAG-FLSKROT-AUTO   TO MOD-FLSKROT-AUTO                         
119200     MOVE CLAG-TISKROT-AUTO   TO MOD-TISKROT-AUTO-UT                      
119300     MOVE CLAG-KDERS          TO MOD-KDERS-C1                             
119400     MOVE CLAG-KVSPANT        TO MOD-KV-SPANT-C1                          
119500                                                                          
119600     PERFORM IMS-GU-ARTM01                                                
119700     IF SEGMENT-FINNS                                                     
119800        MOVE ARTM-ART-SUTPO-TOT    TO MOD-SUTPO-TOT-C1                    
119900        MOVE ARTM-ART-KVOFFERT     TO MOD-KVOFFERT-C1                     
120000                                                                          
120100        COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK    +                    
120200                              ARTM-ART-KVOKS-DAG    +                     
120300                              ARTM-ART-KVOKS-VOR                          
120400                                                                          
120500     ELSE                                                                 
120600        MOVE ZERO                  TO MOD-SUTPO-TOT-C1                    
120700                                      WS-SUTPO-TOT-C2                     
120800     END-IF                                                               
120900                                                                          
121000     MOVE WS-KVOKS-C1              TO MOD-KVOKS-C1                        
121100     SUBTRACT CLAG-KVRESS WS-KVOKS-C1 FROM CLAG-KVLS                      
121200              GIVING MOD-KVDISP-C1                                        
121300     MOVE ZERO                     TO WARTC91-KVUTRS-C2                   
121400     .                                                                    
121500     EJECT                                                                
121600 S01-ROER-EJ-FAELT SECTION.                                               
121700                                                                          
121800     MOVE MFS-ROER-EJ-FAELT    TO MOD-BEN                                 
121900                                  MOD-KVOKS-C1                            
122000                                  MOD-KVOKS-C2                            
122100                                  MOD-KVDISP-C1                           
122200                                  MOD-KVDISP-C2                           
122300                                  MOD-KVAK-C1                             
122400                                  MOD-KVAK-C2                             
122500                                  MOD-SUTPO-TOT-C1                        
122600                                  MOD-SUTPO-TOT-C2                        
122700                                  MOD-KV-SPANT-C1                         
122800                                  MOD-KV-SPANT-C2                         
122900                                  MOD-KDLTK                               
123000                                  MOD-KDUART                              
123100                                  MOD-KDERS-C1                            
123200                                  MOD-KDERS-C2                            
123300                                  MOD-FLSKRSP-C1-VISN                     
123400                                  MOD-FLSKRSP-C2-VISN                     
123500                                  MOD-TISKROT-BEORD-C1                    
123600                                  MOD-TISKROT-BEORD-C2                    
123700                                  MOD-KVSLUTKP-VISN                       
123800                                  MOD-TISLUTKP-VISN                       
123900                                  MOD-KVOFFERT-C1                         
124000                                  MOD-KVOFFERT-C2                         
124100                                  MOD-KVSKROT-C1                          
124200                                  MOD-KVSKROT-C2                          
124300                                  MOD-TISKROT-C1                          
124400                                  MOD-TISKROT-C2                          
124500                                  MOD-FLSKROT-AUTO                        
124600                                  MOD-TISKROT-AUTO-UT                     
124700     .                                                                    
124800     EJECT                                                                
124900 S02-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
125000                                                                          
125100     MOVE MFS-RENSA-FAELT         TO MOD-KDERS                            
125200                                     MOD-KDCLAGER                         
125300                                     MOD-KVKVAR                           
125400                                     MOD-IDKONTO                          
125500                                     MOD-IDANALYS                         
125600                                     MOD-FLSKROT-CLASS                    
125700                                     MOD-BELAGINS30                       
125800     .                                                                    
125900     EJECT                                                                
126000* IMS SEKTIONER                                                           
126100                                                                          
126200 IMS-GET-MSG SECTION.                                                     
126300     MOVE '  QC'                 TO GODK-STATUSKODER                      
126400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126500     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     SKIP3                                                                
126900 IMS-INSERT-MSG SECTION.                                                  
127000     IF MSGI-IDLAND-SPR = 'GB'                                            
127100        MOVE 'N' TO MFS-KDHUVOMR                                          
127200     END-IF                                                               
127300     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
127400     MOVE SPACE                  TO GODK-STATUSKODER                      
127500     CALL CBLTDLI USING ISRT MSG-PCB                                      
127600                          MSG-IO-AREA MFS-IDMOD                           
127700     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     EJECT                                                                
128100 IMS-GU-WDK601 SECTION.                                                   
128200     MOVE 'IMS-GU-WDK601 '  TO DBS-SECTION                                
128300                                                                          
128400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
128500            DELIMITED BY SIZE INTO SSA1                                   
128600     MOVE '  GE'                 TO GODK-STATUSKODER                      
128700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-01 SSA1                   
128800     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     SKIP3                                                                
129200 IMS-GU-ARTC01-PCB2    SECTION.                                           
129300     MOVE 'IMS-GU-ARTC01-PCB2 '   TO DBS-SECTION                          
129400                                                                          
129500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
129600            DELIMITED BY SIZE INTO SSA1                                   
129700     MOVE '  GE'                 TO GODK-STATUSKODER                      
129800     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA-01 SSA1                  
129900     MOVE ARTC2-STATUS-CODE       TO STATUS-WS                            
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     SKIP3                                                                
130300 IMS-GHNP-WDK611      SECTION.                                            
130400     MOVE 'IMS-GHNP-WDK611 '  TO DBS-SECTION                              
130500                                                                          
130600     MOVE 'WDK611  '             TO SSA1                                  
130700     MOVE '  GE'                 TO GODK-STATUSKODER                      
130800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA-11 SSA1                 
130900     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     EJECT                                                                
131300                                                                          
131400 IMS-GNP-WDK611       SECTION.                                            
131500     MOVE 'IMS-GNP-WDK611 '  TO DBS-SECTION                               
131600                                                                          
131700     MOVE 'WDK611  '             TO SSA1                                  
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-11 SSA1                  
132000     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300     SKIP3                                                                
132400 IMS-GNP-WDK611-INFO SECTION.                                             
132500     MOVE 'IMS-GNP-WDK611-INFO '  TO DBS-SECTION                          
132600                                                                          
132700     MOVE 'WDK611*F  '           TO SSA1                                  
132800     MOVE '  GE'                 TO GODK-STATUSKODER                      
132900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-11 SSA1                  
133000     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     EJECT                                                                
133400 IMS-GNP-ARTC11-PCB2 SECTION.                                             
133500     MOVE 'IMS-GNP-ARTC11-PCB2 '  TO DBS-SECTION                          
133600                                                                          
133700     MOVE 'WLARTC11'             TO SSA1                                  
133800     MOVE '  GE'                 TO GODK-STATUSKODER                      
133900     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-AREA-11 SSA1                 
134000     MOVE ARTC2-STATUS-CODE       TO STATUS-WS                            
134100     PERFORM IMS-STATUSKONTROLL                                           
134200     .                                                                    
134300     SKIP3                                                                
134400 IMS-GNP-WDK627   SECTION.                                                
134500     MOVE 'IMS-GNP-WDK627 '      TO DBS-SECTION                           
134600                                                                          
134700     MOVE 'WDK627  '             TO SSA1                                  
134800     MOVE '  GE'                 TO GODK-STATUSKODER                      
134900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK627 SSA1                   
135000     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300     EJECT                                                                
135400 IMS-GU-BENA01     SECTION.                                               
135500     MOVE 'IMS-GU-BENA01 ' TO DBS-SECTION                                 
135600                                                                          
135700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
135800            DELIMITED BY SIZE INTO SSA1                                   
135900     MOVE '  GE'                 TO GODK-STATUSKODER                      
136000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
136100     MOVE BENA-STATUS-CODE       TO STATUS-WS                             
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400     SKIP3                                                                
136500 IMS-GNP-BENA11    SECTION.                                               
136600     MOVE 'IMS-GNP-BENA11 '  TO DBS-SECTION                               
136700                                                                          
136800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
136900            DELIMITED BY SIZE INTO SSA1                                   
137000     MOVE '  GE'                 TO GODK-STATUSKODER                      
137100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
137200     MOVE BENA-STATUS-CODE       TO STATUS-WS                             
137300     PERFORM IMS-STATUSKONTROLL                                           
137400     SKIP3                                                                
137500     .                                                                    
137600     EJECT                                                                
137700* NYTT 840911 ****************************************************        
137800 IMS-GHNP-WDK627 SECTION.                                                 
137900     MOVE 'IMS-GHNP-WDK627 '  TO DBS-SECTION                              
138000*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
138100                                                                          
138200     MOVE   'WDK627   '          TO SSA1                                  
138300     MOVE   '  GE'               TO GODK-STATUSKODER                      
138400     SKIP2                                                                
138500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1                  
138600     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
138700     PERFORM IMS-STATUSKONTROLL                                           
138800     SKIP3                                                                
138900     .                                                                    
139000     EJECT                                                                
139100 IMS-ISRT-WDK627 SECTION.                                                 
139200     MOVE 'IMS-ISRT-WDK627 '  TO DBS-SECTION                              
139300*                        INSERT AV SKROTNINGSSEGMENT WDK627               
139400                                                                          
139500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
139600             DELIMITED BY SIZE INTO SSA1                                  
139700     MOVE   'WDK611   '          TO SSA2                                  
139800     MOVE   'WDK627   '          TO SSA3                                  
139900     MOVE   '  '  TO GODK-STATUSKODER                                     
140000                                                                          
140100     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK627 SSA1                  
140200                                          SSA2 SSA3                       
140300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     EJECT                                                                
140700*-----------------------------------------------------------------        
140800 IMS-REPL-SKROT   SECTION.                                                
140900     MOVE 'IMS-REPL-SKROT '  TO DBS-SECTION                               
141000                                                                          
141100     MOVE '  ' TO GODK-STATUSKODER                                        
141200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK627                       
141300     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
141400     PERFORM IMS-STATUSKONTROLL                                           
141500     .                                                                    
141600     SKIP3                                                                
141700 IMS-REPL-WDK611  SECTION.                                                
141800     MOVE 'IMS-REPL-WDK611 '  TO DBS-SECTION                              
141900                                                                          
142000     MOVE '  ' TO GODK-STATUSKODER                                        
142100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-11                      
142200     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP3                                                                
142600 IMS-GHNP-WDK629 SECTION.                                                 
142700     MOVE 'IMS-GHNP-WDK629 '  TO DBS-SECTION                              
142800                                                                          
142900     MOVE   'WDK629   '         TO SSA1                                   
143000     MOVE   '  GE'              TO GODK-STATUSKODER                       
143100     SKIP2                                                                
143200     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
143300     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
143400     PERFORM IMS-STATUSKONTROLL                                           
143500     .                                                                    
143600     EJECT                                                                
143700 IMS-REPL-WDK629  SECTION.                                                
143800     MOVE 'IMS-REPL-WDK629 '  TO DBS-SECTION                              
143900                                                                          
144000     MOVE '  ' TO GODK-STATUSKODER                                        
144100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
144200     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-ISRT-ALT-PCB SECTION.                                                
144700     MOVE 'IMS-ISRT-ALT-PCB '  TO DBS-SECTION                             
144800                                                                          
144900     MOVE '  ' TO GODK-STATUSKODER                                        
145000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
145100     MOVE ALT-STATUS-CODE      TO STATUS-WS                               
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     SKIP3                                                                
145500 IMS-GU-ARTM01 SECTION.                                                   
145600     MOVE 'IMS-GU-ARTM01 '   TO DBS-SECTION                               
145700                                                                          
145800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
145900            DELIMITED BY SIZE INTO SSA1                                   
146000     MOVE '  GE'                 TO GODK-STATUSKODER                      
146100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
146200     MOVE ARTM-STATUS-CODE       TO STATUS-WS                             
146300     PERFORM IMS-STATUSKONTROLL                                           
146400     SKIP3                                                                
146500     .                                                                    
146600 IMS-GU-SATE01 SECTION.                                                   
146700     MOVE 'IMS-GU-SATE01 '  TO DBS-SECTION                                
146800                                                                          
146900     STRING 'WLSATE01(WDJ1C1KY>=' W-WDJ1C1KY-MIN                          
147000                    '&WDJ1C1KY<=' W-WDJ1C1KY-MAX ')'                      
147100            DELIMITED BY SIZE INTO SSA1                                   
147200     MOVE '  GE'              TO GODK-STATUSKODER                         
147300     CALL  CBLTDLI  USING GU   SATE-PCB DLI-IO-AREA3 SSA1                 
147400     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
147500     PERFORM IMS-STATUSKONTROLL                                           
147600     .                                                                    
147700     SKIP3                                                                
147800 IMS-GN-SATE01 SECTION.                                                   
147900     MOVE 'IMS-GN-SATE01 '  TO DBS-SECTION                                
148000                                                                          
148100     STRING 'WLSATE01(WDJ1C1KY>=' W-WDJ1C1KY-MIN                          
148200                    '&WDJ1C1KY<=' W-WDJ1C1KY-MAX ')'                      
148300            DELIMITED BY SIZE INTO SSA1                                   
148400     MOVE '  GE'              TO GODK-STATUSKODER                         
148500     CALL  CBLTDLI  USING GN   SATE-PCB DLI-IO-AREA3 SSA1                 
148600     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     SKIP3                                                                
149000 IMS-GU-SATB01 SECTION.                                                   
149100     MOVE 'IMS-GU-SATB01 '  TO DBS-SECTION                                
149200                                                                          
149300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-STR-X ')'                     
149400            DELIMITED BY SIZE INTO SSA1                                   
149500     MOVE '  GE'                 TO GODK-STATUSKODER                      
149600     CALL  CBLTDLI  USING GU SATB-PCB DLI-IO-AREA3 SSA1                   
149700     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     SKIP3                                                                
150100 IMS-GU-SATB11 SECTION.                                                   
150200     MOVE 'IMS-GU-SATB11 '  TO DBS-SECTION                                
150300                                                                          
150400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-STR-X ')'                     
150500            DELIMITED BY SIZE INTO SSA1                                   
150600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
150700            DELIMITED BY SIZE INTO SSA2                                   
150800     MOVE '  GE'                 TO GODK-STATUSKODER                      
150900     CALL  CBLTDLI  USING GU SATB-PCB DLI-IO-AREA3 SSA1 SSA2              
151000     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400 IMS-GU-WDR501-6321 SECTION.                                              
151500     MOVE 'IMS-GU-WDR501-6321 '  TO DBS-SECTION                           
151600                                                                          
151700     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
151800            DELIMITED BY SIZE INTO SSA1                                   
151900     MOVE 'GE  '                TO GODK-STATUSKODER                       
152000     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
152100     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     .                                                                    
152400     SKIP3                                                                
152500 IMS-ISRT-WDR501-6321 SECTION.                                            
152600     MOVE 'IMS-ISRT-WDR501-6321'  TO DBS-SECTION                          
152700                                                                          
152800     STRING 'WDR501     '                                                 
152900            DELIMITED BY SIZE INTO SSA1                                   
153000     MOVE '  '                  TO GODK-STATUSKODER                       
153100     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
153200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
153300     PERFORM IMS-STATUSKONTROLL                                           
153400     .                                                                    
153500     EJECT                                                                
153600 IMS-ISRT-WDGX6322 SECTION.                                               
153700     MOVE 'IMS-ISRT-WDGX6322  '  TO DBS-SECTION                           
153800                                                                          
153900     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
154000            DELIMITED BY SIZE INTO SSA1                                   
154100     MOVE 'WDGX6322'            TO SSA2                                   
154200     MOVE '  II'                TO GODK-STATUSKODER                       
154300     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
154400     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
154500     PERFORM IMS-STATUSKONTROLL                                           
154600     .                                                                    
154700     SKIP3                                                                
154800 IMS-ISRT-WDGX6324 SECTION.                                               
154900     MOVE 'IMS-ISRT-WDGX6324 '  TO DBS-SECTION                            
155000                                                                          
155100     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
155200            DELIMITED BY SIZE INTO SSA1                                   
155300     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
155400            DELIMITED BY SIZE INTO SSA2                                   
155500     MOVE 'WDGX6324'            TO SSA3                                   
155600     MOVE '  '                  TO GODK-STATUSKODER                       
155700     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
155800                                      SSA1 SSA2 SSA3                      
155900     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
156000     PERFORM IMS-STATUSKONTROLL                                           
156100     SKIP3                                                                
156200     .                                                                    
156300     EJECT                                                                
156400 IMS-GU-WDR501-6327 SECTION.                                              
156500     MOVE 'IMS-GU-WDR501-6327 '   TO DBS-SECTION                          
156600                                                                          
156700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501-6327 SSA1               
157100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400     SKIP3                                                                
157500 IMS-GHNP-WDGX6328 SECTION.                                               
157600     MOVE 'IMS-GHNP-WDGX6328 '  TO DBS-SECTION                            
157700                                                                          
157800     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
157900          DELIMITED BY SIZE INTO SSA1                                     
158000     MOVE '  GE' TO GODK-STATUSKODER                                      
158100     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX6328 SSA1                
158200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     .                                                                    
158500     EJECT                                                                
158600 IMS-GU-WDK701 SECTION.                                                   
158700     MOVE 'IMS-GU-WDK701 '  TO DBS-SECTION                                
158800                                                                          
158900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
159000            DELIMITED BY SIZE INTO SSA1                                   
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
159300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     SKIP3                                                                
159700 IMS-GNP-WDK711 SECTION.                                                  
159800     MOVE 'IMS-GNP-WDK711 '  TO DBS-SECTION                               
159900                                                                          
160000     MOVE 'WDK711  ' TO SSA1                                              
160100     MOVE '  GE' TO GODK-STATUSKODER                                      
160200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
160300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
160400     PERFORM IMS-STATUSKONTROLL                                           
160500     .                                                                    
160600     EJECT                                                                
160700 IMS-GU-WDN601 SECTION.                                                   
160800     MOVE 'IMS-GU-WDN601 '  TO DBS-SECTION                                
160900                                                                          
161000     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
161100          DELIMITED BY SIZE INTO SSA1                                     
161200     MOVE '  GE' TO GODK-STATUSKODER                                      
161300     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
161400     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
161500     PERFORM IMS-STATUSKONTROLL                                           
161600     .                                                                    
161700     EJECT                                                                
161800 IMS-GNP-WDN611 SECTION.                                                  
161900     MOVE 'IMS-GNP-WDN611 '  TO DBS-SECTION                               
162000                                                                          
162100     STRING 'WDN611   '                                                   
162200          DELIMITED BY SIZE INTO SSA1                                     
162300     MOVE '  GE' TO GODK-STATUSKODER                                      
162400     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
162500     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
162600     PERFORM IMS-STATUSKONTROLL                                           
162700     .                                                                    
162800     EJECT                                                                
162900 IMS-STATUSKONTROLL SECTION.                                              
163000     SET STATUS-IX TO 1                                                   
163100     SEARCH GODK-STATUS AT END CALL FELLOG                                
163200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
163300     END-SEARCH                                                           
163400     .                                                                    
163500     EJECT                                                                
163600*    -COPY WY2000P1                                                       
