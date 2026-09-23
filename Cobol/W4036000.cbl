000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4036000.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   99/03/11.                                                
000600                                                                          
000700*   *FUNKTION:                                                            
000800*        UPPDATERINGSPROGRAM FÖR ORDERBEKRÄFTELSE M.M TILL                
000900*        DIREKTLEVERANSRADER.                                             
001000*                                                                         
001100*        UPPDATERING SKER GENOM TRANS W4T360X FRÅN W46385.                
001200*                                                                         
001300*        OM UPPDATERINGEN GICK BRA SKICKAS OK-MEDDELANDE                  
001400*        TILL DISPATCHER ANNARS SKICKAS FELMEDDELANDE.                    
001500*                                                                         
001600*        LARM-MAIL OM RADEN SAKNAS PÅ WDF4 VID UPPDATERING                
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T360X                                             
002000*        MID:         W4I36001 + WMSGKOM                                  
002100*                                                                         
002200*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002300*    E-TRACKER: 10129446 2013-HÖST  DIREKTLEVERANSLARM                    
002400*    E-TRACKER: 10254592 2015       DECOMISSON VOHF                       
002500                                                                          
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4036000'.            
003500 01  FELTEXT.                                                             
003600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003800                                                                          
003900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004100 77  CURRENT-DAP-SECTION         PIC X(16)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  RAETT                       PIC X       VALUE 'R'.                   
004600 77  FEL                         PIC X       VALUE 'F'.                   
004700 77  IX                          PIC S9(3)   VALUE +0   COMP-3.           
004800 77  INDX                        PIC S9(3)   VALUE +0   COMP-3.           
004900 77  MAX-INDX                    PIC S9(3)   VALUE +10  COMP-3.           
005000 77  2109-IX                     PIC S9(3)   VALUE +0   COMP-3.           
005100 77  2109-IX-MAX                 PIC S9(3)   VALUE +18  COMP-3.           
005200 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
005300 77  WS-IDRADNR                  PIC 9(4)    VALUE ZERO.                  
005400 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
005500 77  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
005600 77  WS-IDORDNR5                 PIC 9(5)    VALUE ZERO.                  
005700 77  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
005800 77  WS-ANTAL-ANNULL-RADER       PIC S9(3)   VALUE ZERO  COMP-3.          
005900 77  W-KVKOLLI                   PIC S9(7)   VALUE ZERO  COMP-3.          
006000 77  W-KVKOLLI-FAKT              PIC S9(7)   VALUE ZERO  COMP-3.          
006100 77  W-KVKOLLI-LAST              PIC S9(7)   VALUE ZERO  COMP-3.          
006200 77  WS-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
006300 77  WS-ANTAL-PA-RAD             PIC S9(7)   VALUE ZERO  COMP-3.          
006400 77  WS-ANTAL-ATT-BACKA          PIC S9(7)   VALUE ZERO  COMP-3.          
006500 77  W-SPAR-IDPRODNR             PIC S9(7)   VALUE ZERO  COMP-3.          
006600 77  W-SPAR-IDDC                 PIC X(2).                                
006700 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
006800 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
006900 77  WS-SAVE-IDCOM-TACD          PIC S9(9)  COMP.                         
007000 77  WS-SAVE-IDCOM-LARM          PIC S9(9)  COMP.                         
007100 77  WS-SAVE-IDCOM-DELAY         PIC S9(9)  COMP.                         
007200 77  WS-DATUM-9KOMPL             PIC 9(8).                                
007300 77  WS-DATUM-8                  PIC 9(8).                                
007400 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
007410 77  4245-MOD-LAENGD             PIC S9(4)  VALUE +836  COMP SYNC.        
007500 01    WS-HHMMSSDD               PIC 9(8)   VALUE ZERO.                   
007600 01    FILLER REDEFINES  WS-HHMMSSDD.                                     
007700   03   WS-HHMMSS                PIC  9(6).                               
007800   03   WS-DD                    PIC  9(2).                               
007900                                                                          
008000 77  SW-MAIL-HDR-SW              PIC X(1)    VALUE 'N'.                   
008100     88  SW-MAIL-HDR-JA                      VALUE 'J'.                   
008200     88  SW-MAIL-HDR-NEJ                     VALUE 'N'.                   
008300                                                                          
008400*  ---PARAMETRAR TILL IDDISTR                                             
008500                                                                          
008600 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
008700*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'REFILLTABDC'.         
009000*   -COPY WWDIST57                                                        
009100                                                                          
009200 01  FILLER                      PIC X(16)  VALUE 'BYTESARTIKLAR'.        
009300*   -COPY WWBYT03                                                         
009400                                                                          
009500*    --- ÖVRIGA ARBETSFÄLT                                                
009600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009700     88  INDATA-OK                           VALUE 'J'.                   
009800     88  INDATA-FEL                          VALUE 'N'.                   
009900                                                                          
010000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010100     88  NYCKLAR-OK                          VALUE 'J'.                   
010200     88  NYCKLAR-FEL                         VALUE 'N'.                   
010300                                                                          
010400 77  WS-INDATA-TEST              PIC X(01).                               
010500     88  WS-INDATA-RATT                      VALUE 'R'.                   
010600                                                                          
010700 77  WS-IDTRANS                  PIC X(04).                               
010800     88  GODKAND-TRANS                       VALUE '4360'                 
010810                                                   '4245'.                
010900                                                                          
011000 77  GODK-KDORDBEK               PIC X(2).                                
011100     88 GODK-KOD                             VALUE '20'                   
011200                                                   '22'                   
011300                                                   '67'                   
011400                                                   '83'                   
011500                                                   '96'.                  
011600                                                                          
011700 77    KDORDSTA-SW               PIC X(01).                               
011800   88  KDORDSTA-KLAR                         VALUE 'J'.                   
011900   88  KDORDSTA-EJ-KLAR                      VALUE 'N'.                   
012000                                                                          
012500 01    WS-IDUSER                 PIC X(8).                                
012600                                                                          
012700 01    FILLER REDEFINES  WS-IDUSER.                                       
012800   03  FILLER                    PIC X(2).                                
012900   03  WS-IDANSTNR               PIC 9(5).                                
013000   03  FILLER                    PIC X(1).                                
013100*                                                                         
013200 01     FILLER                   PIC X(10)   VALUE 'SPAR-AREOR'.          
013300 01     SPAR-AREOR.                                                       
013400   03   SPAR-AREA.                                                        
013500     05 SPAR-VKORDNTO            PIC  9(6)V9(4)    VALUE ZERO.            
013600     05 SPAR-VLORDNTO            PIC  9(4)V9(7)    VALUE ZERO.            
013700     05 SPAR-SUORDV-LEVPL        PIC  S9(9)V9(2)   VALUE ZERO.            
013800     05 SPAR-SUORDV-LEVPL-LOC    PIC  S9(9)V9(2)   VALUE ZERO.            
013900     05 SPAR-SUORDV-LEVPL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.           
014000                                                                          
014100                                                                          
014200                                                                          
014300 01     WS-IDKUNDRF-OLD.                                                  
014400   03   WS-IDORDNR5-OLD          PIC 9(5).                                
014500   03   FILLER                   PIC X(5)    VALUE SPACE.                 
014600                                                                          
014700 01     WS-IDKUNDRF-NEW.                                                  
014800   03   WS-IDORDNR7-NEW          PIC 9(7).                                
014900   03   FILLER                   PIC X(3)    VALUE SPACE.                 
015000 01 DB2-LASNING.                                                          
015100     03 FILLER                   PIC X(16)   VALUE                        
015200                                             'WS-DB2-SEKTION'.            
015300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
015400                                                                          
015500                                                                          
015600*                                                                         
015700 77  WS-IDKUNDNR6                PIC 9(6)    VALUE ZERO.                  
015800 77  WS-IDORDNR7                 PIC 9(7).                                
015900 77  WS-IDKUNDNR-X               PIC Z(7)    VALUE SPACE.                 
016000*                                                                         
016100 01  KDRC-DISPLAY                PIC Z(5).                                
016200 01  WS-TIAAAAMMDD               PIC  9(8).                               
016300 01  FILLER REDEFINES WS-TIAAAAMMDD.                                      
016400     03 WS-TIAA                  PIC  9(2).                               
016500     03 WS-TIAAMMDD              PIC  9(6).                               
016600 01  FILLER                      PIC X(24)   VALUE 'W402TACD'.            
016700*01  -COPY W402TACD                                                       
016800                                                                          
016900 01  HDR-AREA.                                                            
017000*    03  -COPY WZ01REQU                                                   
017100*    03  -COPY WZ04HDR                                                    
017200 01  WS-KV402                    PIC 9(4)    VALUE ZERO.                  
017300                                                                          
017400                                                                          
017500 01 NYCKLAR-TP4TRAN.                                                      
017600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
017700                                                                          
017800                                                                          
017900*    --- DDGS DELAY MAIL REPORT ---                                       
018000 01  WS-REPORT                   PIC X(80) VALUE SPACES.                  
018100 01  WS-HEADING-1.                                                        
018200     03  FILLER                  PIC X(10) VALUE SPACES.                  
018300     03  FILLER                  PIC X(70) VALUE 'FOLLOWING LINES         
018400-    'HAVE ORDER CONF CODE 96 - DELAY FROM SUPPLIER'.                     
018500 01  WS-HEADING-2.                                                        
018600     03  FILLER                  PIC X(08) VALUE 'DISTRICT'.              
018700     03  FILLER                  PIC X(02) VALUE SPACES.                  
018800     03  FILLER                  PIC X(08) VALUE 'CUSTOMER'.              
018900     03  FILLER                  PIC X(02) VALUE SPACES.                  
019000     03  FILLER                  PIC X(07) VALUE '  ORDER'.               
019100     03  FILLER                  PIC X(02) VALUE SPACES.                  
019200     03  FILLER                  PIC X(11) VALUE 'PART NUMBER'.           
019300     03  FILLER                  PIC X(02) VALUE SPACES.                  
019400     03  FILLER                  PIC X(08) VALUE 'QUANTITY'.              
019500     03  FILLER                  PIC X(02) VALUE SPACES.                  
019510     03  FILLER                  PIC X(14) VALUE 'AVAILABLE DATE'.        
019600 01  WS-LINE1.                                                            
020620     03  WS-LINE1-IDDISTR        PIC Z(08) VALUE SPACES.                  
020630     03  FILLER                  PIC X(04) VALUE SPACES.                  
020640     03  WS-LINE1-IDKUNDNR       PIC Z(08) VALUE SPACES.                  
020650     03  FILLER                  PIC X(09) VALUE SPACES.                  
020660     03  WS-LINE1-IDORDNR7       PIC Z(07) VALUE SPACES.                  
020670     03  FILLER                  PIC X(02) VALUE SPACES.                  
020680     03  WS-LINE1-IDARTNR        PIC Z(11) VALUE SPACES.                  
020690     03  FILLER                  PIC X(05) VALUE SPACES.                  
020691     03  WS-LINE1-KVBEART        PIC Z(08) VALUE SPACES.                  
020692     03  FILLER                  PIC X(08) VALUE SPACES.                  
020693     03  WS-LINE1-AVAILDT        PIC Z(06) VALUE SPACES.                  
020700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
020800 01  GENERELLA-SUBPROGRAM.                                                
020900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
021000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
021100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
021400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
021500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
021510     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021600                                                                          
021700 01    ABENDKODER.                                                        
021800     03 FILLER                   PIC X(16) VALUE 'ABENDKODER'.            
021900     03 RKOD-ABEND-UTAN-DUMP     PIC S9(4) COMP SYNC VALUE +16.           
022000     03 RKOD-ABEND-MED-DUMP      PIC S9(4) COMP SYNC VALUE +33.           
022100     03 RKOD-FELTEXT             PIC X(32) VALUE SPACE.                   
022200                                                                          
022300                                                                          
022400 01  MESSAGE-CODES.                                                       
022500     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
022600     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
022700     03  ERR-FEL-KOD             PIC X(3)    VALUE '025'.                 
022800     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
022900     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '054'.                 
023000     03  ERR-ORDER-HEAD-MISSING  PIC X(3)    VALUE '701'.                 
023100     03  INF-OK-BEHANDLAD        PIC X(3)    VALUE '101'.                 
023110*    -COPY WMEDAREA                                                       
023200                                                                          
023300*01  -COPY W009CIA                                                        
023400*                            IMS FUNKTIONSKODER                           
023500 01  FILLER                      PIC X(16)   VALUE 'W005INIT '.           
023600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023700*01 -COPY WMSGINIT                                                        
023800                                                                          
023900 01  FILLER                      PIC X(16)   VALUE '*WZ01SEND**'.         
024000*01  -COPY WZ01SEND                                                       
024100*                            DB2 FUNKTIONSKODER                           
024200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
024300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024400                                                                          
024500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
024600 01  DB2-WS.                                                              
024700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
024800         88  CURSOR-OK                       VALUE 000.                   
024900         88  RADER-FINNS                     VALUE 000.                   
025000         88  RADER-SAKNAS                    VALUE 100.                   
025100         88  ATKOMST-FEL                     VALUE 904.                   
025200     03  GODK-SQLCODEKODER.                                               
025300         05  GODK-SQLCODE OCCURS 5                                        
025400             INDEXED BY SQLCODE-IX PIC 9(3).                              
025500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
025600                                                                          
025700*                            IMS FUNKTIONSKODER                           
025800*01    -COPY W0003                                                        
025900   03    ROLB                    PIC X(4)    VALUE 'ROLB'.                
026000                                                                          
026100*                            DLI INPUT-OUTPUT AREA                        
026200                                                                          
026300                                                                          
026400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
026500*                                                                         
026600 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
026700                                                                          
026800*01  -COPY W4I36001                                                       
026900                                                                          
027000 01  FILLER                  PIC X(16)   VALUE 'MSG/MOD-AREA'.            
027100                                                                          
027200*01  -COPY WMSGAREA                                                       
027210     EJECT                                                                
027220*    03 MOD -COPY W4O24501  -RED MSG-AREA.                                
027300                                                                          
027400 01  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.                
027500                                                                          
027600*01  -COPY WMFSAREA                                                       
027700                                                                          
027800*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
027900*                                                                         
028000 01  FILLER                  PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.        
028100                                                                          
028200 01  KOM-IO-AREA.                                                         
028300*  03  -COPY WMSGKOM                                                      
028400                                                                          
028500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028600                                                                          
028700 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
028800                                                                          
028900 01    NYCKLAR-TILL-DLI.                                                  
029000*                                                                         
029100*                                                                         
029200     03  W-WDK601-IDARTNR-X.                                              
029300         05  W-WDK601-IDARTNR-N   PIC S9(9) COMP-3   VALUE ZERO.          
029400*                                                                         
029500     03  W-WDK701-IDARTNR-X.                                              
029600         05  W-WDK701-IDARTNR-N   PIC S9(9) COMP-3   VALUE ZERO.          
029700*                                                                         
029800     03  W-WDK711-IDDC-X.                                                 
029900         05  W-WDK711-IDDC        PIC X(2)           VALUE ZERO.          
030000*                                                                         
030100   03    W-WDE401-X.                                                      
030200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
030300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
030400     05    W-401-IDKUNDRF.                                                
030500       07  W-401-IDORDNR         PIC 9(5)    VALUE ZERO.                  
030600       07  FILLER                PIC X(5)    VALUE SPACE.                 
030700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
030800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
030900*                                                                         
031000   03    W-WDE411-X.                                                      
031100     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
031200*                                                                         
031300   03    W-WDE601-X.                                                      
031400     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
031500*                                                                         
031600     03  W-IDORDER-X.                                                     
031700         05  W-IDORDER           PIC S9(7)   COMP-3.                      
031800*                                                                         
031900   03    W-WDQ211-X.                                                      
032000     05    W-211-IDDC            PIC  X(2).                               
032100     05    W-211-IDLEVNR         PIC  X(5)   VALUE SPACE.                 
032200*                                                                         
032300   03    W-Q301-KEY-X.                                                    
032400     05    W-Q301-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.          
032500     05    W-Q301-IDDC           PIC X(2).                                
032600     05    W-Q301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
032700     05    W-Q301-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
032800                                                                          
032900   03    W-WDA601KY-MIN-X.                                                
033000     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
033100     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
033200     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
033300     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
033400     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
033500     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
033600     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
033700     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
033800                                                                          
033900   03    W-WDA601KY-MAX-X.                                                
034000     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
034100     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
034200     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
034300     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
034400     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
034500     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
034600     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
034700     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
034800                                                                          
034900   03 W-IDLEVNR-F4-X.                                                     
035000     05 W-IDLEVNR-F4                 PIC X(5)  VALUE SPACE.               
035100                                                                          
035200   03 W-WDF411KY-MIN-X.                                                   
035300     05 W-IDPRODNR-F4-MIN     PIC S9(7) COMP-3  VALUE ZERO.               
035400     05 W-IDPURAD-F4-MIN      PIC S9(5) COMP-3  VALUE ZERO.               
035500     05 W-TIUTSKR-F4-MIN      PIC S9(7) COMP-3  VALUE ZERO.               
035600   03 W-WDF411KY-MAX-X.                                                   
035700     05 W-IDPRODNR-F4-MAX     PIC S9(7) COMP-3  VALUE ZERO.               
035800     05 W-IDPURAD-F4-MAX      PIC S9(5) COMP-3  VALUE ZERO.               
035900     05 W-TIUTSKR-F4-MAX      PIC S9(7) COMP-3  VALUE 9999999.            
036000                                                                          
036100*                                                                         
036200*--------------------WDB2                                                 
036300   03    W-IDGMT-X.                                                       
036400     05  W-IDDISTR                 PIC S9(5)   VALUE ZERO  COMP-3.        
036500     05  W-IDKUNDNR                PIC S9(7)   VALUE ZERO  COMP-3.        
036600                                                                          
036700 01    IMS-WS.                                                            
036800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
036900                                                                          
037000*                        **** STATUS-KOD FRÅN IMS                         
037100   03    STATUS-WS               PIC XX.                                  
037200     88    SEGMENT-FINNS                     VALUE '  '.                  
037300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
037400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
037500     88    SEGMENT-SLUT                      VALUE 'GB'.                  
037600     88    BASEN-SLUT                        VALUE 'GB'.                  
037700                                                                          
037800   03    GODK-STATUSKODER.                                                
037900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
038000                                                                          
038100 01  ALL-SSA.                                                             
038200     03  SSA1                    PIC X(150).                              
038300     03  SSA2                    PIC X(64).                               
038400                                                                          
038500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ART '.             
038600 01  DLI-IO-AREA-ART.                                                     
038700*    03  WLARTS11   -COPY WDK711                                          
038800                                                                          
038900                                                                          
039000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OBKR'.             
039100 01  DLI-IO-AREA-OBKR.                                                    
039200*    03  WLORQM01   -COPY WDQ101                                          
039300                                                                          
039400                                                                          
039500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
039600 01  DLI-IO-AREA-OHUV.                                                    
039700*    03  WLORQI11   -COPY WDQ201                                          
039800                                                                          
039900                                                                          
040000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-DIRL'.             
040100 01  DLI-IO-AREA-DIRL.                                                    
040200*    03  WLORQI11   -COPY WDQ211                                          
040300                                                                          
040400                                                                          
040500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQA'.             
040600 01  DLI-IO-AREA-ORQA.                                                    
040700*    03  WLORQA01   -COPY WDQ301                                          
040800                                                                          
040900 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
041000 01  DLI-IO-AREA-WDA6.                                                    
041100*  03    -COPY WDA601                                                     
041200                                                                          
041300                                                                          
041400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E601'.             
041500 01  DLI-IO-E601.                                                         
041600*    03  -COPY WDE601                                                     
041700                                                                          
041800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E401'.             
041900 01  DLI-IO-E401.                                                         
042000*    03  -COPY WDE401                                                     
042100                                                                          
042200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-K611'.             
042300 01  DLI-IO-K611.                                                         
042400*    03  -COPY WDK611                                                     
042500                                                                          
042600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E411'.             
042700 01  DLI-IO-E411.                                                         
042800*    03  -COPY WDE411                                                     
042900                                                                          
043000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-LOGG'.             
043100 01    DLI-IO-WDGZ01.                                                     
043200   03    IO-WDGZ01               PIC X(200)  VALUE SPACE.                 
043300                                                                          
043400*  03    WDGZ01   -COPY WDGZ01  -PRE LOGG- -RED IO-WDGZ01.                
043500                                                                          
043600 01  FILLER                      PIC X(16)   VALUE 'IO-WDB201'.           
043700 01  DLI-IO-AREA-WDB201.                                                  
043800     03  WDB201.                                                          
043900*        05  -COPY WDB201                                                 
044000 01    FILLER                PIC X(17)  VALUE 'ANNULLATIONSTRANS'.        
044100                                                                          
044200*01      WDGZRY5  -COPY WDGZRY5.                                          
044300*01      WDGZRY5  -COPY WDGZRY5S.                                         
044400                                                                          
044500                                                                          
044600 01  FILLER                      PIC X(16)   VALUE 'IO-WDF411'.           
044700 01  DLI-IO-F411.                                                         
044800*    03  -COPY WDF411                                                     
044900                                                                          
045000                                                                          
045100*    MSG-AREA FÖR HOPP TILL W20109                                        
045200 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
045300 01  W-PROG-TO-PROG-SW-1.                                                 
045400     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
045500     03  2109-Z1                   PIC X.                                 
045600     03  2109-Z2                   PIC X.                                 
045700     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
045800     03  2109-IDTRANS              PIC X(4)  VALUE '4360'.                
045900     03  2109-KDMFSFOR             PIC X.                                 
046000*    03  -COPY W2I10902    -PRE 2109-                                     
046100                                                                          
046200 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
046300                                                                          
046400*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
046500                                                                          
046600     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
046700                                                                          
046800 01  SEND-RAD                    PIC X(120)  VALUE SPACE.                 
046900                                                                          
047000 01  FILLER                      PIC X(16)   VALUE 'MAILAREA'.            
047100                                                                          
047200*    --- LISTLAYOUT                                                       
047300 01  F4-LARM.                                                             
047400     03  F4-RUBRIK1.                                                      
047500         05  FILLER              PIC X(26)   VALUE                        
047600            'VOLVO CAR CORP., WDF4 LARM'.                                 
047700         05  FILLER              PIC X(2)    VALUE SPACE.                 
047800         05  F4R1-DATUM          PIC 9(6).                                
047900                                                                          
048000     03  F4-RUBRIK2.                                                      
048100         05  FILLER              PIC X(31)   VALUE                        
048200            'FÖLJANDE RAD(ER) SAKNAS PÅ WDF4'.                            
048300                                                                          
048400     03  F4-KOMMENTAR-RAD.                                                
048500         05 F4-IDPGM             PIC X(8)    VALUE SPACE.                 
048600         05 FILLER               PIC X(2)    VALUE ': '.                  
048700         05 F4-KOMMENTAR         PIC X(25)   VALUE SPACE.                 
048800                                                                          
048900     03  F4-RAD.                                                          
049000         05  FILLER              PIC X(6)    VALUE 'LEVNR '.              
049100         05  FILLER              PIC X(1)    VALUE SPACE.                 
049200         05  F4-RAD-IDLEVNR      PIC X(5).                                
049300         05  FILLER              PIC X(9)    VALUE '  PRODNR '.           
049400         05  FILLER              PIC X(1)    VALUE SPACE.                 
049500         05  F4-RAD-IDPRODNR     PIC 9(7).                                
049600         05  FILLER              PIC X(6)    VALUE '  RAD '.              
049700         05  FILLER              PIC X(1)    VALUE SPACE.                 
049800         05  F4-RAD-IDPURAD      PIC 9(6).                                
049900         05  FILLER              PIC X(7)    VALUE '  UTSKR'.             
050000         05  FILLER              PIC X(1)    VALUE SPACE.                 
050100         05  F4-RAD-TIUTSKR      PIC 9(6).                                
050200                                                                          
050300                                                                          
050400 LINKAGE SECTION.                                                         
050500*01    -COPY W0009     -PRE MSG-                                          
050600                                                                          
050700*01    -COPY W0009     -PRE DISTRDOC-                                     
050800                                                                          
050900*01    -COPY W0009     -PRE DISP-                                         
051000                                                                          
051100*01    -COPY W0009     -PRE 2109-                                         
051200                                                                          
051300*01    -COPY W0009     -PRE USEA-                                         
051400                                                                          
051500*01    -COPY W0008     -PRE ARTS-                                         
051600     05  FILLER                  PIC X.                                   
051700                                                                          
051800*01    -COPY W0008     -PRE ORQA-                                         
051900     05  FILLER                  PIC X.                                   
052000                                                                          
052100*01    -COPY W0008     -PRE ORQI-                                         
052200     05  FILLER                  PIC X.                                   
052300                                                                          
052400*01    -COPY W0008     -PRE ORQM-                                         
052500     05  FILLER                  PIC X.                                   
052600                                                                          
052700*01    -COPY W0008     -PRE WDE4-                                         
052800     05  FILLER                  PIC X.                                   
052900                                                                          
053000*01    -COPY W0008     -PRE WDE6-                                         
053100     05  FILLER                  PIC X.                                   
053200                                                                          
053300*01    -COPY W0008     -PRE ZZAC-                                         
053400     05  FILLER                  PIC X.                                   
053500                                                                          
053600*01    -COPY W0008     -PRE WDA6B-                                        
053700     05  FILLER                  PIC X.                                   
053800                                                                          
053900*01    -COPY W0008     -PRE WDB2-                                         
054000     05  FILLER                  PIC X.                                   
054100                                                                          
054200*01    -COPY W0008     -PRE WDF4-                                         
054300     05  FILLER                  PIC X.                                   
054400                                                                          
054500*01    -COPY W0008     -PRE WDK6-                                         
054600     05  FILLER                  PIC X.                                   
054700                                                                          
054800 PROCEDURE DIVISION USING  MSG-PCB  DISTRDOC-PCB                          
054900                           DISP-PCB 2109-PCB  USEA-PCB                    
055000                           ARTS-PCB                                       
055100                           ORQA-PCB  ORQI-PCB ORQM-PCB  WDE4-PCB          
055200                           WDE6-PCB  ZZAC-PCB WDA6B-PCB WDB2-PCB          
055300                           WDF4-PCB  WDK6-PCB.                            
055400                                                                          
055500     ENTRY 'DLITCBL' USING MSG-PCB  DISTRDOC-PCB                          
055600                           DISP-PCB 2109-PCB  USEA-PCB                    
055700                           ARTS-PCB                                       
055800                           ORQA-PCB  ORQI-PCB ORQM-PCB  WDE4-PCB          
055900                           WDE6-PCB  ZZAC-PCB WDA6B-PCB WDB2-PCB          
056000                           WDF4-PCB  WDK6-PCB.                            
056110                                                                          
056130                                                                          
056200     PERFORM IMS-GET-MSG                                                  
056300     IF SEGMENT-FINNS                                                     
056400       PERFORM IMS-GN-KOM-AREA                                            
056500       PERFORM A-INIT                                                     
056600       PERFORM B-KOLLA-INDATA                                             
056700                                                                          
056800       IF WS-INDATA-RATT AND GODKAND-TRANS                                
056900          PERFORM D-BEHANDLA-RADER                                        
057000       END-IF                                                             
057100                                                                          
057200       PERFORM Z-DISPATCH-AVSLUT                                          
057300                                                                          
057400     END-IF                                                               
057500                                                                          
057600     MOVE ZERO TO RETURN-CODE                                             
057700     GOBACK                                                               
057800     .                                                                    
057900                                                                          
058000                                                                          
058100 A-INIT SECTION.                                                          
058200     MOVE 'STA A-SEC'                     TO   CURRENT-SECTION            
058300                                                                          
058400     IF MSG-DUBBLA-TRANSKODER                                             
058500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I36001               
058600       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
058700       MOVE MSG-KDMFSFOR-2                TO   2109-KDMFSFOR              
058900     ELSE                                                                 
059000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I36001               
059100       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
059200       MOVE MSG-KDMFSFOR-1                TO   2109-KDMFSFOR              
059400     END-IF                                                               
059500                                                                          
059600     PERFORM AA-FLYTTA-INDATA-MID                                         
059700                                                                          
059800     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
059900     ACCEPT WS-HHMMSSDD                   FROM TIME                       
060000                                                                          
060100     ACCEPT WS-TINUDAT FROM DATE                                          
060200     ACCEPT WS-TINUTID FROM TIME                                          
060300                                                                          
060400     MOVE SPACE                        TO MSG-KOM-IDMFSMED                
060500                                                                          
060600     MOVE ZERO                        TO SPAR-VKORDNTO                    
060700                                         SPAR-VLORDNTO                    
060800                                         SPAR-SUORDV-LEVPL                
060900                                         SPAR-SUORDV-LEVPL-LOC            
061000                                         SPAR-SUORDV-LEVPL-LOCPREL        
061100                                         WS-ANTAL-ANNULL-RADER            
061200                                                                          
061300     MOVE 'SLUTA-SEC'                  TO CURRENT-SECTION                 
061400     .                                                                    
061500                                                                          
061600                                                                          
061700 AA-FLYTTA-INDATA-MID SECTION.                                            
061800     MOVE 'AA-FLYTTA-INDATA'           TO CURRENT-SECTION                 
061900                                                                          
062000     MOVE MID-IDDISTR                  TO WS-IDDISTR                      
062100                                          TEST-IDDISTR                    
062200     MOVE MID-IDKUNDNR                 TO WS-IDKUNDNR                     
062300     MOVE MID-IDORDNR7(3:5)            TO WS-IDORDNR5                     
062400     MOVE MID-IDPRODNR                 TO WS-IDPRODNR                     
062500     .                                                                    
062600                                                                          
062700                                                                          
062800 B-KOLLA-INDATA SECTION.                                                  
062900     MOVE 'STA B-SEC'                  TO CURRENT-SECTION                 
063000                                                                          
063100     MOVE RAETT                        TO WS-INDATA-TEST                  
063200     MOVE +1 TO INDX                                                      
063300     PERFORM UNTIL INDX > MAX-INDX OR                                     
063400                   MID-IDRADNR (INDX) = SPACE                             
063500                                                                          
063600         PERFORM BA-KONTROLL-AV-RAD                                       
063700         ADD +1                TO INDX                                    
063800     END-PERFORM                                                          
063900                                                                          
064000     MOVE 'SLUT B-SEC'                  TO CURRENT-SECTION                
064100     .                                                                    
064200                                                                          
064300                                                                          
064400 BA-KONTROLL-AV-RAD SECTION.                                              
064500     MOVE 'STA BA-SEC '             TO CURRENT-SECTION                    
064600                                                                          
064700     MOVE WS-IDDISTR                TO W-401-IDDISTR                      
064800     MOVE WS-IDKUNDNR               TO W-401-IDKUNDNR                     
064900     MOVE WS-IDORDNR5               TO W-401-IDORDNR                      
065000     MOVE WS-IDPRODNR               TO W-401-IDPRODNR                     
065100                                       W-601-IDPRODNR                     
065200     MOVE +1                        TO W-401-IDPLKLST                     
065300                                                                          
065400     PERFORM IMS-GU-WDE601                                                
065500     IF SEGMENT-FINNS                                                     
065600       IF VORD-KDORDSTA < +3 OR                                           
065700         (MID-KDORDBEK(INDX) = '20' OR '96')                              
065800         PERFORM IMS-GU-WDE401                                            
065900                                                                          
066000         PERFORM UNTIL SEGMENT-FINNS                                      
066100              OR W-401-IDPLKLST > 15                                      
066200           ADD +1              TO W-401-IDPLKLST                          
066300           PERFORM IMS-GU-WDE401                                          
066400         END-PERFORM                                                      
066500                                                                          
066600         IF SEGMENT-FINNS                                                 
066700           MOVE KORD-IDORDER        TO W-IDORDER                          
066800           MOVE KORD-IDUSER         TO WS-IDUSER                          
066900           MOVE KORD-IDDC           TO W-SPAR-IDDC                        
067000                                                                          
067100           MOVE MID-IDRADNR(INDX)   TO WS-IDRADNR                         
067110                                                                          
067200           MOVE WS-IDRADNR          TO W-411-IDPURAD                      
067300           PERFORM IMS-GNP-WDE411                                         
067400           IF SEGMENT-FINNS                                               
067500              PERFORM BAA-KONTROLLERA-RADEN                               
067600           ELSE                                                           
067700             MOVE ERR-LINES-MISSING TO MSG-KOM-IDMFSMED                   
067800             MOVE '4'               TO MSG-KOM-KDSVAR                     
067900             MOVE FEL               TO WS-INDATA-TEST                     
068000           END-IF                                                         
068100         ELSE                                                             
068200           MOVE ERR-ORDER-MISSING   TO MSG-KOM-IDMFSMED                   
068300           MOVE '4'                 TO MSG-KOM-KDSVAR                     
068400           MOVE FEL                 TO WS-INDATA-TEST                     
068500         END-IF                                                           
068600       ELSE                                                               
068700         MOVE ERR-UPD-NOT-ALLOWED   TO MSG-KOM-IDMFSMED                   
068800         MOVE '4'                   TO MSG-KOM-KDSVAR                     
068900         MOVE FEL                   TO WS-INDATA-TEST                     
069000       END-IF                                                             
069100     ELSE                                                                 
069200       MOVE ERR-ORDER-MISSING       TO MSG-KOM-IDMFSMED                   
069300       MOVE '4'                     TO MSG-KOM-KDSVAR                     
069400       MOVE FEL                     TO WS-INDATA-TEST                     
069500     END-IF                                                               
069600                                                                          
069700     MOVE 'SLUT BA-SEC'             TO CURRENT-SECTION                    
069800     .                                                                    
069900                                                                          
070000                                                                          
070100 BAA-KONTROLLERA-RADEN SECTION.                                           
070200     MOVE 'STA BAA-SEC '            TO CURRENT-SECTION                    
070300                                                                          
070400     MOVE MID-KDORDBEK(INDX)        TO GODK-KDORDBEK                      
070500     IF GODK-KOD                                                          
070600       MOVE MID-IDARTPRE(INDX)      TO CIA-IDARTPRE-IN                    
070700       MOVE MID-IDARTBET(INDX)      TO CIA-IDARTBET-IN                    
070800                                                                          
070900       CALL W009CIA USING CIA-W009CIA                                     
071000       IF CIA-KDSVAR = 'F'                                                
071100          MOVE ERR-PART-MISSING     TO MSG-KOM-IDMFSMED                   
071200          MOVE '4'                  TO MSG-KOM-KDSVAR                     
071300          MOVE FEL                  TO WS-INDATA-TEST                     
071400       ELSE                                                               
071500          MOVE CIA-IDARTNR TO WS-IDARTNR                                  
071600          IF ORAD-IDARTNR = CIA-IDARTNR                                   
071700            IF ORAD-KDRADSTA > +3 AND                                     
071800              (MID-KDORDBEK(INDX) = '22' OR '67' OR '83')                 
071900              MOVE ERR-UPD-NOT-ALLOWED                                    
072000                                    TO MSG-KOM-IDMFSMED                   
072100              MOVE '4'              TO MSG-KOM-KDSVAR                     
072200              MOVE FEL              TO WS-INDATA-TEST                     
072300            END-IF                                                        
072400          ELSE                                                            
072500            MOVE ERR-PART-MISSING   TO MSG-KOM-IDMFSMED                   
072600            MOVE '4'                TO MSG-KOM-KDSVAR                     
072700            MOVE FEL                TO WS-INDATA-TEST                     
072800          END-IF                                                          
072900       END-IF                                                             
073000     ELSE                                                                 
073100       MOVE ERR-FEL-KOD             TO MSG-KOM-IDMFSMED                   
073200       MOVE '4'                     TO MSG-KOM-KDSVAR                     
073300       MOVE FEL                     TO WS-INDATA-TEST                     
073400     END-IF                                                               
073500                                                                          
073600     MOVE 'SLUT BAA-SEC'            TO CURRENT-SECTION                    
073700     .                                                                    
073800                                                                          
073900                                                                          
074000 D-BEHANDLA-RADER SECTION.                                                
074100     MOVE 'STA D-SEC '              TO CURRENT-SECTION                    
074200                                                                          
074300     PERFORM IMS-GHU-WDE401                                               
074400     PERFORM IMS-GHU-WDQ201                                               
074500     IF SEGMENT-FINNS                                                     
074600                                                                          
074700       MOVE +1 TO INDX                                                    
074800                                                                          
074900       PERFORM UNTIL INDX > MAX-INDX OR                                   
075000         MID-IDRADNR(INDX) = SPACE                                        
075100         MOVE MID-IDRADNR (INDX)    TO WS-IDRADNR                         
075200         MOVE WS-IDRADNR            TO W-411-IDPURAD                      
075300         PERFORM IMS-GHU-WDE411                                           
075400         PERFORM DA-UPPDATERA-E4-RAD                                      
075500*        OKÄND ARTIKEL/ANNULLATION                                        
075600         IF MID-KDORDBEK(INDX) = '22' OR '67' OR '83'                     
075700           PERFORM DB-SKAPA-TRANSAR                                       
075800           PERFORM DH-EV-UPPDATERA-K7                                     
075900         END-IF                                                           
076000*        ORDERBEKRÄFTELSE SKAPAS FÖR SAMTLIGA ORSAKSKODER                 
076100         PERFORM DG-SKAPA-OBKR-Q1                                         
076200         MOVE OBKR-IDDISTR        TO   W-IDDISTR                          
076300         MOVE OBKR-IDKUNDNR       TO   W-IDKUNDNR                         
076400         PERFORM IMS-GU-WDB201                                            
076500         IF SEGMENT-SAKNAS                                                
076600            MOVE NEJ              TO GMT-FLOBKR-TACD                      
076700         END-IF                                                           
076800         IF  (OBKR-IDSYSTEM = 'LDC' OR 'TACD')                            
076900         AND OBKR-KDORDBEK = '96'                                         
077000           IF GMT-FLOBKR-TACD = JA                                        
077100             PERFORM DI-SKAPA-TACD-402                                    
077200           ELSE                                                           
077300             PERFORM DJ-SEND-MAIL-KOD96                                   
077400           END-IF                                                         
077500         END-IF                                                           
077600*        OKÄND ARTIKEL                                                    
077700         IF MID-KDORDBEK(INDX) = '22' OR '67'                             
077800           MOVE '83'                TO MID-KDORDBEK(INDX)                 
077900           PERFORM DG-SKAPA-OBKR-Q1                                       
078000         END-IF                                                           
078100         ADD +1                     TO INDX                               
078200       END-PERFORM                                                        
078300                                                                          
078400       IF 2109-IX > ZERO                                                  
078500         COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17            
078600         PERFORM IMS-PURG-MSG-2109                                        
078700       END-IF                                                             
078800                                                                          
078900*      OKÄND ARTIKEL/ANNULLATION                                          
079000       IF WS-ANTAL-ANNULL-RADER > ZERO                                    
079100         PERFORM DC-UPPDATERA-E6                                          
079200         PERFORM DD-UPPDATERA-E4                                          
079300                                                                          
079400         IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                            
079500           PERFORM DE-UPPDATERA-Q3                                        
079600           PERFORM DF-UPPDATERA-Q2                                        
079700         END-IF                                                           
079800       END-IF                                                             
079900     ELSE                                                                 
080000* ORDERHUVUD RENSAD                                                       
080100       MOVE ERR-ORDER-HEAD-MISSING  TO MSG-KOM-IDMFSMED                   
080200       MOVE '4'                     TO MSG-KOM-KDSVAR                     
080300       MOVE FEL                     TO WS-INDATA-TEST                     
080400     END-IF                                                               
080500                                                                          
080600     PERFORM S29-SEND-CLOSE                                               
080700     PERFORM S39-SEND-CLOSE                                               
080800     MOVE 'SLUT D-SEC '        TO CURRENT-SECTION                         
080900     .                                                                    
081000                                                                          
081100                                                                          
081200 DA-UPPDATERA-E4-RAD SECTION.                                             
081300     MOVE 'STA DA-SEC '        TO CURRENT-SECTION                         
081400                                                                          
081500*    ANNULLATION EJ MÖJLIG                                                
081600     IF MID-KDORDBEK(INDX) = '20'                                         
081700        IF ORAD-KDRADSTA < 4                                              
081900           MOVE '2'            TO ORAD-KDANNULL                           
082000           PERFORM DAB-RENSA-ANNULLATION-F4                               
082100        END-IF                                                            
082200     END-IF                                                               
082300                                                                          
082400*    OKÄND ARTIKEL/ANNULLATION                                            
082500     IF MID-KDORDBEK(INDX) = '22' OR '67' OR '83'                         
082600       IF MID-KDORDBEK(INDX) = '83'                                       
082700         MOVE ORAD-KVAVBART     TO WS-ANTAL-PA-RAD                        
082800         MOVE MID-KVBEART(INDX) TO WS-ANTAL-ATT-BACKA                     
082900         COMPUTE ORAD-KVANNANT = ORAD-KVANNANT +                          
083000                                 MID-KVBEART(INDX)                        
083100         COMPUTE ORAD-KVAVBART = ORAD-KVAVBART -                          
083200                                 MID-KVBEART(INDX)                        
083300         IF OHUV-KDORDKL = 0                                              
083400           PERFORM S04B-BACKA-NYVORKO                                     
083500         END-IF                                                           
083600       ELSE                                                               
083700         MOVE ORAD-KVAVBART    TO WS-ANTAL-PA-RAD                         
083800         MOVE ORAD-KVBEART     TO WS-ANTAL-ATT-BACKA                      
083900         MOVE ORAD-KVBEART     TO ORAD-KVANNANT                           
084000         MOVE ZERO             TO ORAD-KVAVBART                           
084100         IF OHUV-KDORDKL = 0                                              
084200           PERFORM S04B-BACKA-NYVORKO                                     
084300         END-IF                                                           
084400       END-IF                                                             
084500       MOVE +4                 TO ORAD-KDRADSTA                           
084600       MOVE '3'                TO ORAD-KDANNULL                           
084700       ADD +1                  TO WS-ANTAL-ANNULL-RADER                   
084800                                                                          
084900       PERFORM DAA-SPAR-UPPGIFTER                                         
085000       PERFORM DAC-RENSA-DIRLEV-F4                                        
085100     END-IF                                                               
085200                                                                          
085300*    FÖRSENAD LEVERANS                                                    
085400     IF MID-KDORDBEK(INDX) = '96'                                         
085500       MOVE MID-DALEVDAT(INDX) TO WS-DATUM-8                              
085600       MOVE WS-DATUM-8 (3:6)   TO ORAD-TISLULEV                           
085700       PERFORM DAD-NY-LEVERANSDATUM-F4                                    
085800     END-IF                                                               
085900                                                                          
086000     PERFORM IMS-REPL-WDE411                                              
086100                                                                          
086200     MOVE 'SLUT DA-SEC '       TO CURRENT-SECTION                         
086300     .                                                                    
086400                                                                          
086500                                                                          
086600 DAA-SPAR-UPPGIFTER SECTION.                                              
086700     MOVE 'DAA-SPAR-UPPG   '           TO CURRENT-SECTION                 
086800                                                                          
086900     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
087000                 (ORAD-VKARTNTO * ORAD-KVANNANT)                          
087100                                                                          
087200     COMPUTE SPAR-VLORDNTO ROUNDED = SPAR-VLORDNTO +                      
087300                 (ORAD-VLARTNTO * ORAD-KVANNANT / 1000000)                
087400                                                                          
087500     COMPUTE SPAR-SUORDV-LEVPL ROUNDED = SPAR-SUORDV-LEVPL +              
087600                 (ORAD-PRARTNTO * ORAD-KVANNANT)                          
087700                                                                          
087800     COMPUTE SPAR-SUORDV-LEVPL-LOC ROUNDED =                              
087900     SPAR-SUORDV-LEVPL-LOC + (ORAD-PRARTNTO-LOC * ORAD-KVANNANT)          
088000                                                                          
088100     COMPUTE SPAR-SUORDV-LEVPL-LOCPREL ROUNDED =                          
088200                 SPAR-SUORDV-LEVPL-LOCPREL +                              
088300                 (ORAD-PRARTNTO-LOCPREL * ORAD-KVANNANT)                  
088400     .                                                                    
088500                                                                          
088600                                                                          
088700 DAB-RENSA-ANNULLATION-F4 SECTION.                                        
088800     MOVE 'DAB-RENSA ANNULL'        TO CURRENT-SECTION                    
088900                                                                          
089000     MOVE ORAD-IDLEVNR       TO W-IDLEVNR-F4                              
089100     MOVE ORAD-IDPRODNR      TO W-IDPRODNR-F4-MIN                         
089200                                W-IDPRODNR-F4-MAX                         
089300     MOVE ORAD-IDPURAD       TO W-IDPURAD-F4-MIN                          
089400                                W-IDPURAD-F4-MAX                          
089500*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
089600*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
089700*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
089800*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
089900                                                                          
090000     PERFORM IMS-GHU-WDF411                                               
090100     IF SEGMENT-FINNS                                                     
090110        IF DLOR-TIPACKN > 0                                               
090111          MOVE 'ANNULLATION AVSLAGEN'                                     
090112                               TO F4-KOMMENTAR                            
090113          PERFORM S10-GENERERA-LARM-MAIL                                  
090120        ELSE                                                              
090121          MOVE '2'             TO DLOR-KDANNULL                           
090122          PERFORM IMS-REPL-WDF411                                         
090130        END-IF                                                            
090400     ELSE                                                                 
090500        MOVE 'ANNULLATION AVSLAGEN'                                       
090600                             TO F4-KOMMENTAR                              
090700        PERFORM S10-GENERERA-LARM-MAIL                                    
090800     END-IF                                                               
090900     .                                                                    
091000                                                                          
091100                                                                          
091200 DAC-RENSA-DIRLEV-F4 SECTION.                                             
091300     MOVE 'DAC-RENSA-DIRLEV'        TO CURRENT-SECTION                    
091400                                                                          
091500     MOVE ORAD-IDLEVNR       TO W-IDLEVNR-F4                              
091600     MOVE ORAD-IDPRODNR      TO W-IDPRODNR-F4-MIN                         
091700                                W-IDPRODNR-F4-MAX                         
091800     MOVE ORAD-IDPURAD       TO W-IDPURAD-F4-MIN                          
091900                                W-IDPURAD-F4-MAX                          
092000*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
092100*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
092200*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
092300*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
092400                                                                          
092500     PERFORM IMS-GHU-WDF411                                               
092600     IF SEGMENT-FINNS                                                     
092610        IF DLOR-TIPACKN > 0                                               
092611          MOVE 'SKEPPAD RAD'   TO F4-KOMMENTAR                            
092612          PERFORM S10-GENERERA-LARM-MAIL                                  
092620        ELSE                                                              
092700          PERFORM IMS-DLET-WDF411                                         
092710        END-IF                                                            
092800     ELSE                                                                 
092900        MOVE 'SKEPPAD RAD'   TO F4-KOMMENTAR                              
093000        PERFORM S10-GENERERA-LARM-MAIL                                    
093100     END-IF                                                               
093200     .                                                                    
093300                                                                          
093400                                                                          
093500 DAD-NY-LEVERANSDATUM-F4  SECTION.                                        
093600     MOVE 'DAD-NY-LEVDAT   '        TO CURRENT-SECTION                    
093700                                                                          
093800     MOVE ORAD-IDLEVNR       TO W-IDLEVNR-F4                              
093900     MOVE ORAD-IDPRODNR      TO W-IDPRODNR-F4-MIN                         
094000                                W-IDPRODNR-F4-MAX                         
094100     MOVE ORAD-IDPURAD       TO W-IDPURAD-F4-MIN                          
094200                                W-IDPURAD-F4-MAX                          
094300*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
094400*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
094500*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
094600*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
094700                                                                          
094800     PERFORM IMS-GHU-WDF411                                               
094900     IF SEGMENT-FINNS                                                     
094910        IF DLOR-TIPACKN > 0                                               
094920          MOVE 'NY LEVERANSDATUM' TO F4-KOMMENTAR                         
094930          PERFORM S10-GENERERA-LARM-MAIL                                  
094940        ELSE                                                              
094950          MOVE ORAD-TISLULEV   TO DLOR-TISLULEV                           
094951          ADD 1                TO DLOR-KVSLULEV                           
094952          PERFORM IMS-REPL-WDF411                                         
094960        END-IF                                                            
095300     ELSE                                                                 
095400        MOVE 'NY LEVERANSDATUM' TO F4-KOMMENTAR                           
095500        PERFORM S10-GENERERA-LARM-MAIL                                    
095600     END-IF                                                               
095700     .                                                                    
095800                                                                          
095900                                                                          
096000 DB-SKAPA-TRANSAR SECTION.                                                
096100     MOVE 'STA DB-SEC '             TO CURRENT-SECTION                    
096200                                                                          
096300     PERFORM DBA-GENERERA-ANNULL-TRANS                                    
096400                                                                          
096500     IF ORAD-KDOI NOT = SPACE                                             
096600       PERFORM DBB-GENERERA-2109-TRANS                                    
096700     END-IF                                                               
096800                                                                          
096900     MOVE 'SLUT DB-SEC '            TO CURRENT-SECTION                    
097000     .                                                                    
097100                                                                          
097200                                                                          
097300 DBA-GENERERA-ANNULL-TRANS SECTION.                                       
097400     MOVE 'STA DBA-SEC '        TO CURRENT-SECTION                        
097500                                                                          
097600     ACCEPT LOGG-TIAAMMDD       FROM DATE                                 
097700     ACCEPT LOGG-TIKLOCK        FROM TIME                                 
097800     ADD +1                     TO  LOGG-IDLOGLOP                         
097900     MOVE 'RY5'                 TO  RY5-IDPTYP                            
098000                                    LOGG-IDPTYP                           
098100     MOVE ORAD-BERADREF         TO  RY5-BERADREF                          
098200     MOVE ORAD-BEVOLREF         TO  RY5-BEVOLREF                          
098300     MOVE KORD-IDKUNDRF         TO  RY5-IDKUNDRF                          
098400     MOVE ORAD-IDARTNR          TO  RY5-IDARTNR                           
098500     MOVE ORAD-FLRESTN          TO  RY5-FLRESTN                           
098600     MOVE ORAD-FLDIRLEV         TO  RY5-FLDIRLEV                          
098700     MOVE KORD-FLLSBOK          TO  RY5-FLLSBOK                           
098800     MOVE KORD-FLORDSPE         TO  RY5-FLORDSPE                          
098900     MOVE ORAD-IDKUNDRF-RO      TO  RY5-IDKUNDRF-RO                       
099000                                                                          
099100     IF ORAD-FLTILLK = JA                                                 
099200         MOVE 1                 TO  RY5-KDARTERS                          
099300     ELSE                                                                 
099400         MOVE ZERO              TO  RY5-KDARTERS                          
099500     END-IF                                                               
099600                                                                          
099700     MOVE KORD-IDDC             TO  RY5-IDDC                              
099800     MOVE ORAD-KDDSP            TO  RY5-KDDSP                             
099900     MOVE KORD-KDFAKTYP         TO  RY5-KDFAKTYP                          
100000     MOVE ORAD-KDFRAKT          TO  RY5-KDFRAKT                           
100100     MOVE ORAD-KDORDING         TO  RY5-KDORDING                          
100200     MOVE ORAD-KDORDKL          TO  RY5-KDORDKL                           
100300                                    RY5-KDORDKL-URS                       
100400     MOVE ORAD-KDORDTYP         TO  RY5-KDORDTYP                          
100500     MOVE ORAD-KDKVBRYT         TO  RY5-KDKVBRYT                          
100600     MOVE ORAD-KDVRINFO         TO  RY5-KDVRINFO                          
100700     MOVE ORAD-KVBEART          TO  RY5-KVBEART                           
100800     MOVE ORAD-KVAVBART         TO  RY5-KVAVBART                          
100900     MOVE ORAD-KVANNANT         TO  RY5-KVANNANT                          
101000                                    RY5-KVAVART                           
101100     MOVE ORAD-REKSIFFR         TO  RY5-REKSIFFR                          
101200     MOVE ORAD-TIUTSKR          TO  RY5-TIORDREG                          
101300     MOVE ORAD-TIRODAT          TO  RY5-TIRODAT                           
101400     MOVE RY5-WDGZRY5           TO  LOGG-LOGGPOST                         
101500     MOVE KORD-IDDISTR          TO  RY5S-IDDISTR                          
101600     MOVE KORD-IDKUNDNR         TO  RY5S-IDKUNDNR                         
101700     IF  OHUV-FLVORKO = JA                                                
101800     OR  OHUV-FLVORKO = YES                                               
101900         MOVE JA                TO  RY5S-FLVORKO                          
102000     ELSE                                                                 
102100         MOVE OHUV-FLVORKO      TO  RY5S-FLVORKO                          
102200     END-IF                                                               
102300     MOVE OHUV-FLFORBI          TO  RY5S-FLFORBI                          
102400     MOVE OHUV-FLOVRLEV         TO  RY5S-FLOVRLEV                         
102500     MOVE ORAD-IDSYSTEM         TO  RY5S-IDSYSTEM                         
102600     MOVE ORAD-KVSLATT          TO  RY5S-KVSLATT                          
102700     MOVE ORAD-KDPRODSL         TO  RY5S-KDPRODSL                         
102800     MOVE SPACE                 TO  RY5S-FILLERX5                         
102900                                    RY5S-FILLERX10                        
103000     MOVE ZERO                  TO  RY5S-KDTPOTYP                         
103100     MOVE RY5S-WDGZRY5S-CTX     TO  LOGG-SORTPOST                         
103200                                                                          
103300     PERFORM IMS-ISRT-ZZAC01                                              
103400     PERFORM UNTIL SEGMENT-FINNS                                          
103500       IF LOGG-IDLOGLOP = 9                                               
103600         MOVE ZERO              TO LOGG-IDLOGLOP                          
103700         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
103800       END-IF                                                             
103900       ADD +1                   TO LOGG-IDLOGLOP                          
104000       PERFORM IMS-ISRT-ZZAC01                                            
104100     END-PERFORM                                                          
104200                                                                          
104300     MOVE 'SLUT DBA-SEC '       TO CURRENT-SECTION                        
104400     .                                                                    
104500                                                                          
104600                                                                          
104700 DBB-GENERERA-2109-TRANS SECTION.                                         
104800     MOVE 'STA  DBB-SEC '    TO CURRENT-SECTION                           
104900                                                                          
105000*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
105100     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
105200     IF NOT BYT03-OBJEKT                                                  
105300                                                                          
105400        ADD +1                  TO 2109-IX                                
105500        MOVE 2109-IX            TO 2109-MID2-KVANTART                     
105600        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-IX)            
105700        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC (2109-IX)               
105800        MOVE ORAD-KDOI          TO 2109-MID2-KDOI (2109-IX)               
105900        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-IX)          
106000        MOVE '-'                TO 2109-MID2-KDTECKEN (2109-IX)           
106100        MOVE ORAD-KVANNANT      TO 2109-MID2-KVOI (2109-IX)               
106200        MOVE KORD-TIORDREG      TO 2109-MID2-TIUPPDAT (2109-IX)           
106300                                                                          
106400        IF 2109-IX = 2109-IX-MAX                                          
106500          PERFORM DBBA-STARTA-2109                                        
106600        END-IF                                                            
106700     END-IF                                                               
106800                                                                          
106900     MOVE 'SLUT DBB-SEC '    TO CURRENT-SECTION                           
107000     .                                                                    
107100                                                                          
107200                                                                          
107300 DBBA-STARTA-2109 SECTION.                                                
107400     MOVE 'DBBA-STARTA-2109'           TO CURRENT-SECTION                 
107500                                                                          
107600     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
107700                                                                          
107800     PERFORM IMS-PURG-MSG-2109                                            
107900                                                                          
108000     MOVE SPACE              TO 2109-MID2-W2I10902                        
108100     MOVE +0                 TO 2109-IX                                   
108200     .                                                                    
108300                                                                          
108400                                                                          
108500 DC-UPPDATERA-E6 SECTION.                                                 
108600     MOVE 'STA  DC-SEC '         TO CURRENT-SECTION                       
108700                                                                          
108800     PERFORM IMS-GHU-WDE601                                               
108900                                                                          
109000     ADD WS-ANTAL-ANNULL-RADER   TO VORD-KVORDRAD-PACK                    
109100     SUBTRACT SPAR-SUORDV-LEVPL  FROM VORD-SUORDV                         
109200     SUBTRACT SPAR-SUORDV-LEVPL-LOC      FROM VORD-SUORDV-LOC             
109300     SUBTRACT SPAR-SUORDV-LEVPL-LOCPREL  FROM VORD-SUORDV-LOCPREL         
109400     SUBTRACT SPAR-VKORDNTO      FROM VORD-VKORDNTO                       
109500     SUBTRACT SPAR-VLORDNTO      FROM VORD-VLORDNTO                       
109600                                                                          
109700     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
109800       IF VORD-KVORDRAD = +1                                              
109900         MOVE WS-DAGENS-DATUM    TO VORD-TIPACKN-SK                       
110000         MOVE +5                 TO VORD-KDORDSTA                         
110100       ELSE                                                               
110200         IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT AND                          
110300            VORD-KVKOLLI = VORD-KVKOLLI-LAST                              
110400           MOVE +5               TO VORD-KDORDSTA                         
110500         ELSE                                                             
110600           IF VORD-KVKOLLI-FL = VORD-KVKOLLI                              
110700             MOVE +4               TO VORD-KDORDSTA                       
110800           ELSE                                                           
110900             MOVE +3               TO VORD-KDORDSTA                       
111000           END-IF                                                         
111100         END-IF                                                           
111200       END-IF                                                             
111300     END-IF                                                               
111400                                                                          
111500     PERFORM IMS-REPL-WDE601                                              
111600                                                                          
111700     MOVE 'SLUT DC-SEC '            TO CURRENT-SECTION                    
111800     .                                                                    
111900                                                                          
112000                                                                          
112100 DD-UPPDATERA-E4 SECTION.                                                 
112200     MOVE 'STA  DD-SEC '            TO CURRENT-SECTION                    
112300                                                                          
112400     PERFORM IMS-GHU-WDE401                                               
112500     MOVE VORD-VKORDNTO             TO  KORD-VKORDNTO                     
112600     MOVE VORD-VLORDNTO             TO  KORD-VLORDNTO                     
112700     MOVE VORD-SUORDV               TO  KORD-SUORDV-LEVPL                 
112800     MOVE VORD-SUORDV-LOC           TO  KORD-SUORDV-LEVPL-LOC             
112900     MOVE VORD-SUORDV-LOCPREL       TO  KORD-SUORDV-LEVPL-LOCPREL         
113000     MOVE VORD-KVORDRAD-PACK        TO  KORD-KVORDRAD-PACK                
113100                                                                          
113200     PERFORM IMS-REPL-WDE401                                              
113300                                                                          
113400     MOVE 'SLUT DD-SEC '            TO CURRENT-SECTION                    
113500     .                                                                    
113600                                                                          
113700                                                                          
113800 DE-UPPDATERA-Q3 SECTION.                                                 
113900     MOVE 'STA  DE-SEC '             TO CURRENT-SECTION                   
114000                                                                          
114100     MOVE KORD-IDORDER               TO W-Q301-IDORDER                    
114200     MOVE KORD-IDDC                  TO W-Q301-IDDC                       
114300     MOVE KORD-IDPRODNR              TO W-Q301-IDPRODNR                   
114400     MOVE KORD-IDPLKLST              TO W-Q301-IDPLKLST                   
114500     PERFORM IMS-GHU-WDQ301                                               
114600                                                                          
114700     MOVE KORD-KVORDRAD-PACK         TO ODEL-KVPACKRAD-OD                 
114800     MOVE 'P'                        TO ODEL-KDODELSTA                    
114900     MOVE WS-DAGENS-DATUM            TO ODEL-TIPACKN                      
115000     MOVE WS-HHMMSS                  TO ODEL-TIPACTID                     
115100     PERFORM IMS-REPL-WDQ301                                              
115200                                                                          
115300     MOVE 'SLUT DE-SEC '             TO CURRENT-SECTION                   
115400     .                                                                    
115500                                                                          
115600                                                                          
115700 DF-UPPDATERA-Q2 SECTION.                                                 
115800     MOVE 'STA  DF-SEC '            TO CURRENT-SECTION                    
115900                                                                          
116000     MOVE ORAD-IDLEVNR              TO W-211-IDLEVNR                      
116100     MOVE KORD-IDDC                 TO W-211-IDDC                         
116200     PERFORM IMS-GHNP-WDQ211                                              
116300                                                                          
116400     MOVE VORD-VKORDNTO             TO DIRL-VKORDNTO                      
116500     MOVE VORD-VLORDNTO             TO DIRL-VLORDNTO                      
116600     MOVE VORD-SUORDV               TO DIRL-SUORDV                        
116700     MOVE VORD-SUORDV-LOC           TO DIRL-SUORDV-LOC                    
116800     MOVE VORD-SUORDV-LOCPREL       TO DIRL-SUORDV-LOCPREL                
116900*    MOVE VORD-KVORDRAD-PACK        TO DIRL-KVRADER                       
117000     MOVE 'P'                       TO DIRL-KDORDSTA                      
117100     PERFORM IMS-REPL-ORQI                                                
117200                                                                          
117300     MOVE 'SLUT DF-SEC '            TO CURRENT-SECTION                    
117400     .                                                                    
117500                                                                          
117600                                                                          
117700 DG-SKAPA-OBKR-Q1 SECTION.                                                
117800     MOVE 'STA  DG-SEC  '                 TO CURRENT-SECTION              
117900                                                                          
118000     MOVE KORD-IDORDER                    TO OBKR-IDORDER                 
118100     MOVE ORAD-IDARTNR                    TO OBKR-IDARTNR                 
118200     MOVE 1                               TO OBKR-IDLOPNR                 
118300     MOVE 1                               TO OBKR-IDSEKVNR                
118400     MOVE KORD-IDDC                       TO OBKR-IDDC                    
118500     MOVE MID-KDORDBEK(INDX)              TO OBKR-KDORDBEK                
118600     MOVE SPACE                           TO OBKR-BEERS                   
118700     MOVE OHUV-BEKUNDRF                   TO OBKR-BEKUNDRF                
118800     MOVE ORAD-BERADREF                   TO OBKR-BERADREF                
118900     MOVE ORAD-BEVOLREF                   TO OBKR-BEVOLREF                
119000     MOVE ORAD-IDKAMPRF                   TO OBKR-IDKAMPRF                
119100     MOVE 0                               TO OBKR-DIERS-KVOT              
119200     MOVE NEJ                             TO OBKR-FLAKPLOC                
119300     MOVE NEJ                             TO OBKR-FLSLATT                 
119400     MOVE ORAD-FLINVEST                   TO OBKR-FLINVEST                
119500     MOVE JA                              TO OBKR-FLOBOK                  
119600     MOVE NEJ                             TO OBKR-FLOBTRAN                
119700     MOVE NEJ                             TO OBKR-FLOBPRT                 
119800     MOVE ORAD-FLPRTILL                   TO OBKR-FLPRTILL                
119900     MOVE ORAD-FLRESTN                    TO OBKR-FLRESTN                 
120000     MOVE NEJ                             TO OBKR-FLTILLK                 
120100     MOVE 0                               TO OBKR-IDARTNR-TILLK           
120200     MOVE ORAD-IDDC-RO                    TO OBKR-IDDC-RO                 
120300     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
120400     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
120500                                                                          
120600     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
120700     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
120800     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
120900                                                                          
121000     MOVE ORAD-IDKUNDRF-RO                TO WS-IDKUNDRF-OLD              
121100     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
121200     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
121300                                                                          
121400     MOVE ORAD-IDLEVNR                    TO OBKR-IDLEVNR                 
121500     MOVE ORAD-IDLOPNR-RO                 TO OBKR-IDLOPNR-RO              
121510     IF WS-IDTRANS = '4245'                                               
121520       MOVE 'W4024500'                    TO OBKR-IDPGM                   
121530     ELSE                                                                 
121600       MOVE IDPGM                         TO OBKR-IDPGM                   
121610     END-IF                                                               
121700     MOVE ORAD-IDSYSTEM                   TO OBKR-IDSYSTEM                
121800     MOVE ORAD-KDDSP                      TO OBKR-KDDSP                   
121900     MOVE 0                               TO OBKR-KDERS                   
122000     MOVE ORAD-KDOI                       TO OBKR-KDOI                    
122100     MOVE ORAD-CLEARGROUP                 TO OBKR-CLEARGROUP              
122200     MOVE ORAD-KDKVBRYT                   TO OBKR-KDKVBRYT                
122300     MOVE ORAD-KDPRTYP                    TO OBKR-KDPRTYP                 
122400     MOVE 0                               TO OBKR-KDTPOTYP                
122500     MOVE ORAD-KDVRINFO                   TO OBKR-KDVRINFO                
122600                                                                          
122700     MOVE ORAD-KVANNANT                   TO OBKR-KVANNANT                
122800     MOVE ORAD-KVAVBART                   TO OBKR-KVAVBART                
122900     IF MID-KDORDBEK(INDX) = '83' OR '96'                                 
123000       MOVE MID-KVBEART(INDX)             TO OBKR-KVBEART                 
123100                                             OBKR-KVBEART-Q               
123200     ELSE                                                                 
123300       MOVE ORAD-KVBEART                  TO OBKR-KVBEART                 
123400                                             OBKR-KVBEART-Q               
123500     END-IF                                                               
123600     MOVE 0                               TO OBKR-KVBEART-TILLK           
123700     MOVE 0                               TO OBKR-KVPREAVB                
123800     MOVE 0                               TO OBKR-KVPRERO                 
123900     MOVE ZERO                            TO OBKR-KVQPACK                 
124000     MOVE 0                               TO OBKR-KVRO                    
124100     MOVE ZERO                            TO OBKR-TIRODAT                 
124200     MOVE ORAD-KVSLATT                    TO OBKR-KVSLATT                 
124300     MOVE ORAD-PRARTNTO                   TO OBKR-PRARTNTO                
124400     MOVE ORAD-DEAL-PR-LINE               TO OBKR-DEAL-PR-LINE            
124500     MOVE 0                               TO OBKR-PRBPRIS                 
124600     MOVE ORAD-REKSIFFR                   TO OBKR-REKSIFFR                
124700     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
124800     MOVE 0                               TO OBKR-RERF-RAD                
124900     IF MID-KDORDBEK(INDX) = '96'                                         
125000       MOVE ORAD-TISLULEV                 TO OBKR-TIDISPIN                
125100     ELSE                                                                 
125200       MOVE +0                            TO OBKR-TIDISPIN                
125300     END-IF                                                               
125400     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
125500                                             WS-DATUM-9KOMPL              
125600     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
125700     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
125800     END-COMPUTE                                                          
125900     MOVE ORAD-TIPRIS                     TO OBKR-TIPRIS                  
126000                                                                          
126100     MOVE WS-DAGENS-DATUM                 TO OBKR-TIREGDAT                
126200     MOVE WS-HHMMSS                       TO OBKR-TIREGTID                
126300     MOVE KORD-TIORDREG                   TO WS-DATUM-9KOMPL              
126400     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
126500                                                                          
126600     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
126700     END-COMPUTE                                                          
126800     MOVE 0                               TO OBKR-TITPO                   
126900     MOVE ORAD-KDFRAKT                    TO OBKR-KDFRAKT                 
127000     MOVE ORAD-KDORDKL                    TO OBKR-KDORDKL                 
127100     MOVE ORAD-IDBIL                      TO OBKR-IDBIL                   
127200                                                                          
127300     MOVE OHUV-KDORDTYP-LDC               TO OBKR-KDORDTYP-LDC            
127400     MOVE OHUV-TIREPDAT                   TO OBKR-TIREPDAT                
127500     MOVE ORAD-IDKUNDRF-WIP               TO OBKR-IDKUNDRF-WIP            
127610     MOVE OBKR-TIDISPIN                   TO OBKR-TIDLEVDAT               
127620     MOVE ORAD-PRAVCOST                   TO OBKR-PRAVCOST                
127630     MOVE ORAD-KDVALISO                   TO OBKR-KDVALISO                
127700                                                                          
127800     PERFORM IMS-ISRT-WDQ101                                              
127900     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
128000        ADD +1                            TO OBKR-IDLOPNR                 
128100        PERFORM IMS-ISRT-WDQ101                                           
128200     END-PERFORM                                                          
128300                                                                          
128400     MOVE 'SLUT DG-SEC  '                 TO CURRENT-SECTION              
128500     .                                                                    
128600                                                                          
128700                                                                          
128800 DH-EV-UPPDATERA-K7 SECTION.                                              
128900     MOVE 'DH-EV-UPPD-K7   '           TO CURRENT-SECTION                 
129000                                                                          
129100******************************************************************        
129200*                                                                         
129300*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
129400*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
129500*                                                                         
129600******************************************************************        
129700                                                                          
129800     MOVE TEST-IDDISTR         TO W-TP4TRAN-IDDISTR                       
129900                                                                          
130000     PERFORM DB2-SELECT-TP4TRAN                                           
130100                                                                          
130200     IF  DIST35-REFILL                                                    
130300     OR  DIST35-REFILL-INOM-NDC                                           
130310     OR  DIST35-NONVCC-NONVCC-REFILL                                      
132800     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
130400     OR  DIST35-NA-TRANSFER                                               
130500     OR  DIST35-PACIFIC-TRANSFER                                          
130510     OR  DIST35-REFILL-INOM-JP                                            
130600     OR  DIST35-CN-TRANSFER                                               
130700     OR  DIST35-NA-NDC-RETURNS                                            
130710     OR  DIST35-NONVCC-VCC-REFILL                                         
133500     OR  DIST35-NONVCC-VCC-TRANSFER                                       
130800     OR  RADER-FINNS                                                      
130900                                                                          
131000       IF RADER-FINNS                                                     
131100         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
131200       ELSE                                                               
131300         SEARCH ALL DIST57-REFILL-DC                                      
131400            AT END                                                        
131500               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
131600                                TO FELTEXT-STR                            
131700               CALL FELLOG                                                
131800            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
131900               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
132000                                TO W-WDK711-IDDC                          
132100         END-SEARCH                                                       
132200       END-IF                                                             
132300                                                                          
132400       MOVE ORAD-IDARTNR      TO W-WDK701-IDARTNR-N                       
132500       PERFORM IMS-GHU-WDK711                                             
132600       SUBTRACT ORAD-KVANNANT FROM SLAG-KVBEART                           
132700                                                                          
132800       PERFORM IMS-REPL-WDK711                                            
132900     ELSE                                                                 
133000        IF DIST35-NONVCC-CDC-REFILL                                       
133200                                                                          
133300         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
133400         SEARCH ALL DIST57-REFILL-DC                                      
133500            AT END                                                        
133600               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
133700                                TO FELTEXT-STR                            
133800               CALL FELLOG                                                
133900            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
134000               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
134100                                TO W-WDK711-IDDC                          
134200         END-SEARCH                                                       
134300                                                                          
134400         MOVE ORAD-IDARTNR      TO W-WDK601-IDARTNR-N                     
134500         PERFORM IMS-GHU-WDK711                                           
134600         SUBTRACT ORAD-KVANNANT FROM CLAG-KVBEART                         
134700                                                                          
134800         PERFORM IMS-REPL-WDK711                                          
134900        END-IF                                                            
135000     END-IF                                                               
135100     .                                                                    
135200                                                                          
135300                                                                          
135400 DI-SKAPA-TACD-402          SECTION.                                      
135500     MOVE 'DI-SK-TACD-4024 '      TO CURRENT-SECTION                      
135600                                                                          
135700     MOVE 'PU1'                   TO 402-IDPTYP                           
135800     MOVE 01                      TO 402-IDVTYP-TACDIS                    
135900     MOVE 20                      TO WS-TIAA                              
136000     MOVE WS-DAGENS-DATUM         TO WS-TIAAMMDD                          
136100     MOVE WS-TIAAAAMMDD           TO 402-DAREGDAT                         
136200     MOVE OBKR-IDDISTR            TO 402-IDDISTR                          
136300     MOVE OBKR-IDKUNDNR           TO 402-IDKUNDNR                         
136400     MOVE OBKR-IDORDNR7           TO 402-IDORDNR7                         
136500                                                                          
136600     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
136700     MOVE OBKR-IDARTNR            TO CIA-IDARTBET-IN                      
136800     CALL W009CIA              USING CIA-W009CIA                          
136900     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET                         
137000                                                                          
137100     MOVE OBKR-KDORDBEK           TO 402-KDORDBEK                         
137200     MOVE OBKR-KVBEART            TO 402-KVBEART                          
137300     MOVE OBKR-IDSEKVNR           TO 402-IDSEKVNR                         
137400                                                                          
137500     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
137600     MOVE OBKR-IDARTNR-TILLK      TO CIA-IDARTBET-IN                      
137700     CALL W009CIA              USING CIA-W009CIA                          
137800     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET-TILLK                   
137900                                                                          
138000     MOVE OBKR-KVBEART-TILLK      TO 402-KVLEVART                         
138100     MOVE OBKR-IDDC               TO 402-IDDC                             
138110     MOVE OBKR-TIDLEVDAT          TO 402-DADLEVDAT                        
138111     IF 402-DADLEVDAT > ZERO                                              
138120        ADD 20000000              TO 402-DADLEVDAT                        
138130     END-IF                                                               
138200                                                                          
138300     IF WS-KV402 = 0                                                      
138400       PERFORM S21-SEND-OPEN                                              
138500       MOVE OBKR-IDKUNDNR         TO WS-IDKUNDNR6                         
138600       MOVE OBKR-IDORDNR7         TO WS-IDORDNR7                          
138700       PERFORM S22-PUT-HEADER                                             
138800     END-IF                                                               
138900     ADD +1                       TO WS-KV402                             
139000     PERFORM S25-PUT-LINE                                                 
139100     .                                                                    
139200                                                                          
139300                                                                          
139400 DJ-SEND-MAIL-KOD96         SECTION.                                      
139500     MOVE 'DJ-SEND-MAIL-KOD96'   TO CURRENT-SECTION                       
139600                                                                          
139700     IF SW-MAIL-HDR-NEJ                                                   
139800       PERFORM S31-SEND-OPEN                                              
139900       PERFORM S32-PUT-HEADER                                             
140000       MOVE WS-HEADING-1         TO WS-REPORT                             
140100       PERFORM S35-PUT-LINE                                               
140200       MOVE ALL '-'              TO WS-REPORT                             
140300       PERFORM S35-PUT-LINE                                               
140400       MOVE WS-HEADING-2         TO WS-REPORT                             
140500       PERFORM S35-PUT-LINE                                               
140600       MOVE ALL '-'              TO WS-REPORT                             
140700       PERFORM S35-PUT-LINE                                               
140800       SET SW-MAIL-HDR-JA        TO TRUE                                  
140900     END-IF                                                               
141000                                                                          
141100     MOVE OBKR-IDDISTR           TO WS-LINE1-IDDISTR                      
141200     MOVE OBKR-IDKUNDNR          TO WS-LINE1-IDKUNDNR                     
141300     MOVE OBKR-IDORDNR7          TO WS-LINE1-IDORDNR7                     
141400     MOVE OBKR-IDARTNR           TO WS-LINE1-IDARTNR                      
141500     MOVE OBKR-KVBEART           TO WS-LINE1-KVBEART                      
141510     MOVE OBKR-TIDISPIN          TO WS-LINE1-AVAILDT                      
141600     MOVE WS-LINE1               TO WS-REPORT                             
141700                                                                          
141800     PERFORM S35-PUT-LINE                                                 
141900     .                                                                    
142000                                                                          
142100                                                                          
142200 S10-GENERERA-LARM-MAIL SECTION.                                          
142300     MOVE 'S10-LARM-MAIL   '      TO CURRENT-SECTION                      
142400                                                                          
142500     PERFORM S90-SEND-OPEN                                                
142600     PERFORM S90-PUT-DAP-START                                            
142700     PERFORM S11-REDIGERA-RUBRIK                                          
142800     PERFORM S12-REDIGERA-KOMMENTAR                                       
142900     PERFORM S13-REDIGERA-RAD                                             
143000     PERFORM S90-SEND-CLOSE                                               
143100     .                                                                    
143200                                                                          
143300                                                                          
143400 S11-REDIGERA-RUBRIK    SECTION.                                          
143500     MOVE 'S11-REIGERA-RUBR' TO CURRENT-SECTION                           
143600                                                                          
143700                                                                          
143800     MOVE WS-DAGENS-DATUM   TO F4R1-DATUM                                 
143900     MOVE F4-RUBRIK1        TO SEND-RAD                                   
144000     PERFORM S90-PUT-DOC-LINE                                             
144100                                                                          
144200     MOVE SPACE             TO SEND-RAD                                   
144300     PERFORM S90-PUT-DOC-LINE                                             
144400                                                                          
144500     MOVE SPACE             TO SEND-RAD                                   
144600     PERFORM S90-PUT-DOC-LINE                                             
144700                                                                          
144800     MOVE F4-RUBRIK2        TO SEND-RAD                                   
144900     PERFORM S90-PUT-DOC-LINE                                             
145000                                                                          
145100     MOVE SPACE             TO SEND-RAD                                   
145200     PERFORM S90-PUT-DOC-LINE                                             
145300                                                                          
145400     .                                                                    
145500                                                                          
145600                                                                          
145700 S12-REDIGERA-KOMMENTAR SECTION.                                          
145800     MOVE 'S12-RED-KOMMENT ' TO CURRENT-SECTION                           
145900                                                                          
146000     MOVE IDPGM                    TO F4-IDPGM                            
146100                                                                          
146200     MOVE F4-KOMMENTAR-RAD         TO SEND-RAD                            
146300     PERFORM S90-PUT-DOC-LINE                                             
146400     .                                                                    
146500                                                                          
146600                                                                          
146700 S13-REDIGERA-RAD       SECTION.                                          
146800     MOVE 'S12-REIGERA-RAD ' TO CURRENT-SECTION                           
146900                                                                          
147000     MOVE W-IDLEVNR-F4             TO F4-RAD-IDLEVNR                      
147100     MOVE W-IDPRODNR-F4-MIN        TO F4-RAD-IDPRODNR                     
147200     MOVE W-IDPURAD-F4-MIN         TO F4-RAD-IDPURAD                      
147300     MOVE ORAD-TIUTSKR             TO F4-RAD-TIUTSKR                      
147400                                                                          
147500     MOVE F4-RAD                   TO SEND-RAD                            
147600     PERFORM S90-PUT-DOC-LINE                                             
147700     .                                                                    
147800                                                                          
147900                                                                          
148000 S21-SEND-OPEN SECTION.                                                   
148100     MOVE 'S21-SEND-OPEN   '      TO CURRENT-SECTION                      
148200                                                                          
148300     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
148400     MOVE 'OPEN'                  TO SEND-KDFUNC                          
148500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
148600                                     SEND-OPEN-AREA                       
148700     IF SEND-KDRC > ZERO                                                  
148800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
148900       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
149000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
149100       DISPLAY FELTEXT                                                    
149200       CALL FELLOG                                                        
149300     END-IF                                                               
149400     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-TACD                    
149500     .                                                                    
149600                                                                          
149700                                                                          
149800 S22-PUT-HEADER SECTION.                                                  
149900     MOVE 'S22-PUT-HEADER  '      TO CURRENT-SECTION                      
150000                                                                          
150100     MOVE 1                       TO REQU-IDMSGVER                        
150200     MOVE 'R'                     TO REQU-KDPGMACT                        
150300     MOVE IDPGM                   TO REQU-IDUSER                          
150400     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
150500     MOVE WS-IDKUNDNR6            TO HDR-IDOUTREC                         
150600     MOVE WS-IDORDNR7             TO HDR-IDLIST                           
150700     MOVE 'PUT'                   TO SEND-KDFUNC                          
150800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
150900     MOVE WS-SAVE-IDCOM-TACD      TO SEND-IDCOM                           
151000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
151100                                     SEND-KVDLEN                          
151200                                     HDR-AREA                             
151300     IF SEND-KDRC > ZERO                                                  
151400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
151500       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
151600       DELIMITED BY SIZE       INTO FELTEXT-STR                           
151700       DISPLAY FELTEXT                                                    
151800       CALL FELLOG                                                        
151900     END-IF                                                               
152000     .                                                                    
152100                                                                          
152200                                                                          
152300 S25-PUT-LINE SECTION.                                                    
152400     MOVE 'S25-PUT-LINE    '      TO CURRENT-SECTION                      
152500                                                                          
152600     MOVE 'PUT'                   TO SEND-KDFUNC                          
152700     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
152800     MOVE WS-SAVE-IDCOM-TACD      TO SEND-IDCOM                           
152900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
153000                                     SEND-KVDLEN                          
153100                                     402-W402TACD                         
153200     IF SEND-KDRC > ZERO                                                  
153300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
153400       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
153500       DELIMITED BY SIZE       INTO FELTEXT-STR                           
153600       DISPLAY FELTEXT                                                    
153700       CALL FELLOG                                                        
153800     END-IF                                                               
153900     .                                                                    
154000                                                                          
154100                                                                          
154200 S29-SEND-CLOSE SECTION.                                                  
154300     MOVE 'S29-SEND-CLOSE  '      TO CURRENT-SECTION                      
154400                                                                          
154500     IF WS-KV402 > 0                                                      
154600       MOVE 'CLOSE'               TO SEND-KDFUNC                          
154700       MOVE WS-SAVE-IDCOM-TACD    TO SEND-IDCOM                           
154800       CALL WZ01SEND           USING SEND-CONTROL-AREA                    
154900       IF SEND-KDRC > 0                                                   
155000         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
155100         STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                  
155200         DELIMITED BY SIZE     INTO FELTEXT-STR                           
155300         DISPLAY FELTEXT                                                  
155400         CALL FELLOG                                                      
155500       END-IF                                                             
155600     END-IF                                                               
155700     .                                                                    
155800                                                                          
155900                                                                          
156000 S31-SEND-OPEN SECTION.                                                   
156100     MOVE 'S31-SEND-OPEN   '      TO CURRENT-SECTION                      
156200                                                                          
156300     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
156400     MOVE 'OPEN'                  TO SEND-KDFUNC                          
156500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
156600                                     SEND-OPEN-AREA                       
156700     IF SEND-KDRC > ZERO                                                  
156800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
156900       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
157000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
157100       DISPLAY FELTEXT                                                    
157200       CALL FELLOG                                                        
157300     END-IF                                                               
157400     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-DELAY                   
157500     .                                                                    
157600                                                                          
157700                                                                          
157800 S32-PUT-HEADER SECTION.                                                  
157900     MOVE 'S32-PUT-HEADER  '      TO CURRENT-SECTION                      
158000                                                                          
158100     MOVE 1                       TO REQU-IDMSGVER                        
158200     MOVE 'R'                     TO REQU-KDPGMACT                        
158300     MOVE IDPGM                   TO REQU-IDUSER                          
158400     MOVE 'DDGSDELAY'             TO HDR-IDOUTTYPE                        
158500     MOVE OBKR-IDDISTR            TO HDR-IDOUTREC                         
158600     MOVE OBKR-IDKUNDNR           TO WS-IDKUNDNR-X                        
158700     MOVE WS-IDKUNDNR-X           TO HDR-IDLIST                           
158800     MOVE 'PUT'                   TO SEND-KDFUNC                          
158900     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
159000     MOVE WS-SAVE-IDCOM-DELAY     TO SEND-IDCOM                           
159100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
159200                                     SEND-KVDLEN                          
159300                                     HDR-AREA                             
159400     IF SEND-KDRC > ZERO                                                  
159500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
159600       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
159700       DELIMITED BY SIZE       INTO FELTEXT-STR                           
159800       DISPLAY FELTEXT                                                    
159900       CALL FELLOG                                                        
160000     END-IF                                                               
160100     .                                                                    
160200                                                                          
160300 S35-PUT-LINE SECTION.                                                    
160400     MOVE 'S35-PUT-LINE    '      TO CURRENT-SECTION                      
160500                                                                          
160600     MOVE 'PUT'                   TO SEND-KDFUNC                          
160700     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
160800     MOVE WS-SAVE-IDCOM-DELAY     TO SEND-IDCOM                           
160900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
161000                                     SEND-KVDLEN                          
161100                                     WS-REPORT                            
161200     IF SEND-KDRC > ZERO                                                  
161300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
161400       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
161500       DELIMITED BY SIZE       INTO FELTEXT-STR                           
161600       DISPLAY FELTEXT                                                    
161700       CALL FELLOG                                                        
161800     END-IF                                                               
161900     .                                                                    
162000                                                                          
162100 S39-SEND-CLOSE SECTION.                                                  
162200     MOVE 'S39-SEND-CLOSE  '      TO CURRENT-SECTION                      
162300                                                                          
162400     IF SW-MAIL-HDR-JA                                                    
162500       MOVE 'CLOSE'               TO SEND-KDFUNC                          
162600       MOVE WS-SAVE-IDCOM-DELAY     TO SEND-IDCOM                         
162700       CALL WZ01SEND           USING SEND-CONTROL-AREA                    
162800       IF SEND-KDRC > 0                                                   
162900         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
163000         STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                  
163100         DELIMITED BY SIZE     INTO FELTEXT-STR                           
163200         DISPLAY FELTEXT                                                  
163300         CALL FELLOG                                                      
163400       END-IF                                                             
163500     END-IF                                                               
163600     .                                                                    
163700                                                                          
163800                                                                          
163900 S04B-BACKA-NYVORKO SECTION.                                              
164000     MOVE 'S04B-BA-NYVORKO '      TO CURRENT-SECTION                      
164100                                                                          
164200     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
164300     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
164400     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
164500                                    W-A601KY-MAX-IDDISTR                  
164600     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
164700                                    W-A601KY-MAX-IDKUNDNR                 
164800     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
164900                                    W-A601KY-MAX-IDKUNDRF                 
165000     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
165100                                    W-A601KY-MAX-TIREGDAT                 
165200     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
165300                                    W-A601KY-MAX-IDARTNR                  
165400                                                                          
165500     PERFORM IMS-GHN-WDA6B                                                
165600     PERFORM UNTIL SEGMENT-SAKNAS                                         
165700                OR BASEN-SLUT                                             
165800                                                                          
165900         IF  VOR-KDVORATG > '1'                                           
166000         AND VOR-KDVORATG < '6'                                           
166100         AND VOR-KVPREAVB        = WS-ANTAL-PA-RAD                        
166200             SUBTRACT WS-ANTAL-ATT-BACKA FROM VOR-KVPREAVB                
166300             IF  VOR-KVPREAVB = 0                                         
166400               MOVE '8'          TO VOR-KDVORATG                          
166500               MOVE 83           TO VOR-KDORDBEK                          
166600               MOVE WS-TINUDAT   TO VOR-TIKLAR                            
166700               COMPUTE VOR-TIKLATID  = WS-TINUTID                         
166800                                       / 100                              
166900               END-COMPUTE                                                
167000             END-IF                                                       
167100             PERFORM IMS-REPL-WDA6B                                       
167200         END-IF                                                           
167300                                                                          
167400         PERFORM IMS-GHN-WDA6B                                            
167500     END-PERFORM                                                          
167600     .                                                                    
167700                                                                          
167800                                                                          
167900 S90-SEND-OPEN SECTION.                                                   
168000     MOVE 'S90-SEND-OPEN'           TO CURRENT-SECTION.                   
168100                                                                          
168200     MOVE 'OPEN'                    TO SEND-KDFUNC                        
168300     MOVE 'CARPARTS.DAP.DISTRDOC'   TO SEND-ADDISPABS                     
168400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
168500                         SEND-OPEN-AREA                                   
168600     IF SEND-KDRC > 0                                                     
168700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
168800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
168900       DELIMITED BY SIZE INTO FELTEXT                                     
169000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
169100     END-IF                                                               
169200     MOVE SEND-IDCOM               TO WS-SAVE-IDCOM-LARM                  
169300     .                                                                    
169400                                                                          
169500                                                                          
169600 S90-PUT-DAP-START SECTION.                                               
169700     MOVE 'S90-PUT-DAP-START'     TO CURRENT-SECTION.                     
169800                                                                          
169900     MOVE 1                       TO REQU-IDMSGVER                        
170000     MOVE 'R'                     TO REQU-KDPGMACT                        
170100     MOVE IDPGM                   TO REQU-IDUSER                          
170200     MOVE 'WDF4'                  TO HDR-IDOUTTYPE                        
170300     MOVE SPACE                   TO HDR-IDOUTREC                         
170400                                     HDR-IDLIST                           
170500     MOVE 'LARM'                  TO HDR-IDOUTREC (1:4)                   
170600                                     HDR-IDLIST                           
170700     MOVE 'PUT'                   TO SEND-KDFUNC                          
170800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
170900     MOVE WS-SAVE-IDCOM-LARM      TO SEND-IDCOM                           
171000                                                                          
171100                                                                          
171200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
171300                         SEND-KVDLEN                                      
171400                         HDR-AREA                                         
171500     IF SEND-KDRC > ZERO                                                  
171600       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
171700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
171800       DELIMITED BY SIZE          INTO FELTEXT-STR                        
171900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
172000     END-IF                                                               
172100     .                                                                    
172200                                                                          
172300                                                                          
172400 S90-PUT-DOC-LINE SECTION.                                                
172500     MOVE 'S90-PUT-DOC-LINE' TO CURRENT-SECTION.                          
172600                                                                          
172700     MOVE 'PUT'                           TO SEND-KDFUNC                  
172800     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
172900     MOVE WS-SAVE-IDCOM-LARM              TO SEND-IDCOM                   
173000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
173100                         SEND-KVDLEN                                      
173200                         SEND-RAD                                         
173300     IF SEND-KDRC > ZERO                                                  
173400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
173500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
173600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
173700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
173800     END-IF                                                               
173900     .                                                                    
174000                                                                          
174100                                                                          
174200 S90-SEND-CLOSE SECTION.                                                  
174300     MOVE 'S90-SEND-CLOSE' TO CURRENT-SECTION.                            
174400                                                                          
174500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
174600     MOVE WS-SAVE-IDCOM-LARM         TO SEND-IDCOM                        
174700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
174800                                                                          
174900     IF SEND-KDRC > 0                                                     
175000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
175100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
175200       DELIMITED BY SIZE INTO FELTEXT                                     
175300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
175400     END-IF                                                               
175500     .                                                                    
175600 Z-DISPATCH-AVSLUT SECTION.                                               
175700     MOVE 'Z-DISPATCH-AVSL '      TO CURRENT-SECTION                      
175800                                                                          
175900*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
176000     IF MSG-KOM-IDMFSMED = SPACE                                          
176100        MOVE INF-OK-BEHANDLAD  TO MSG-KOM-IDMFSMED                        
176200     END-IF                                                               
176210     IF WS-IDTRANS = '4245'                                               
176220        CONTINUE                                                          
176230     ELSE                                                                 
176300        PERFORM IMS-INSERT-DISP-MSG                                       
176310     END-IF                                                               
176400     .                                                                    
176500                                                                          
176600                                                                          
176700* IMS SEKTIONER                                                           
176800*                                                                         
176900 IMS-GET-MSG SECTION.                                                     
177000                                                                          
177100     MOVE '  QC' TO GODK-STATUSKODER                                      
177200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
177300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
177400     PERFORM IMS-STATUSKONTROLL                                           
177500     .                                                                    
177600                                                                          
177700                                                                          
177800 IMS-GN-KOM-AREA SECTION.                                                 
177900     MOVE '  QD'   TO GODK-STATUSKODER                                    
178000     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
178100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178200     PERFORM IMS-STATUSKONTROLL                                           
178300     .                                                                    
178400                                                                          
178500                                                                          
178600 IMS-INSERT-DISP-MSG SECTION.                                             
178700     MOVE SPACE TO GODK-STATUSKODER                                       
178800     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
178900     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
179000     PERFORM IMS-STATUSKONTROLL                                           
179100     .                                                                    
179200                                                                          
179400 IMS-PURG-MSG-2109 SECTION.                                               
179500     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
179600     MOVE '  '  TO GODK-STATUSKODER                                       
179700     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
179800     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
179900     PERFORM IMS-STATUSKONTROLL                                           
180000     .                                                                    
180100                                                                          
180200                                                                          
180300 IMS-GHU-WDK711 SECTION.                                                  
180400     MOVE 'IMS-GHU-WDK711  '      TO CURRENT-IMS-SECTION                  
180500                                                                          
180600     MOVE SPACE               TO ALL-SSA                                  
180700     STRING 'WLARTS01(IDARTNR  =' W-WDK701-IDARTNR-X ')'                  
180800          DELIMITED BY SIZE INTO SSA1                                     
180900     STRING 'WLARTS11(IDDC     =' W-WDK711-IDDC-X ')'                     
181000          DELIMITED BY SIZE INTO SSA2                                     
181100     MOVE '  GE'              TO GODK-STATUSKODER                         
181200     CALL CBLTDLI USING GHU ARTS-PCB SLAG-WDK711 SSA1 SSA2                
181300     MOVE ARTS-STATUS-CODE    TO STATUS-WS                                
181400     PERFORM IMS-STATUSKONTROLL                                           
181500     .                                                                    
181600                                                                          
181700                                                                          
181800 IMS-REPL-WDK711 SECTION.                                                 
181900     MOVE 'IMS-REPL-WDK711 '      TO CURRENT-IMS-SECTION                  
182000                                                                          
182100     MOVE SPACE            TO ALL-SSA                                     
182200     MOVE '  '             TO GODK-STATUSKODER                            
182300     CALL CBLTDLI USING REPL ARTS-PCB SLAG-WDK711                         
182400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
182500     PERFORM IMS-STATUSKONTROLL                                           
182600     .                                                                    
182700                                                                          
182800                                                                          
182900 IMS-GHU-WDK611 SECTION.                                                  
183000     MOVE 'IMS-GHU-WDK611  '      TO CURRENT-IMS-SECTION                  
183100                                                                          
183200     MOVE SPACE               TO ALL-SSA                                  
183300     STRING 'WDK601  (IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
183400          DELIMITED BY SIZE INTO SSA1                                     
183500     MOVE   'WDK611 '         TO SSA2                                     
183600     MOVE '    '              TO GODK-STATUSKODER                         
183700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-K611 SSA1 SSA2                
183800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
183900     PERFORM IMS-STATUSKONTROLL                                           
184000     .                                                                    
184100                                                                          
184200                                                                          
184300 IMS-REPL-WDK611 SECTION.                                                 
184400     MOVE 'IMS-REPL-WDK611 '      TO CURRENT-IMS-SECTION                  
184500                                                                          
184600     MOVE SPACE            TO ALL-SSA                                     
184700     MOVE '  '             TO GODK-STATUSKODER                            
184800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-K611                         
184900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
185000     PERFORM IMS-STATUSKONTROLL                                           
185100     .                                                                    
185200                                                                          
185300                                                                          
185400 IMS-GHU-WDE401 SECTION.                                                  
185500     MOVE 'IMS-GHU-WDE401  '      TO CURRENT-IMS-SECTION                  
185600                                                                          
185700     MOVE SPACE                 TO ALL-SSA                                
185800     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
185900            DELIMITED BY SIZE INTO SSA1                                   
186000     MOVE '    '                TO GODK-STATUSKODER                       
186100     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E401 SSA1                  
186200     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
186300     PERFORM IMS-STATUSKONTROLL                                           
186400     .                                                                    
186500                                                                          
186600                                                                          
186700 IMS-GU-WDE401 SECTION.                                                   
186800     MOVE 'IMS-GU-WDE401   '      TO CURRENT-IMS-SECTION                  
186900                                                                          
187000     MOVE SPACE                 TO ALL-SSA                                
187100     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
187200            DELIMITED BY SIZE INTO SSA1                                   
187300     MOVE '  GE'                TO GODK-STATUSKODER                       
187400     CALL CBLTDLI USING GU    WDE4-PCB DLI-IO-E401 SSA1                   
187500     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     .                                                                    
187800                                                                          
187900                                                                          
188000 IMS-REPL-WDE401 SECTION.                                                 
188100     MOVE 'IMS-REPL-WDE401 '      TO CURRENT-IMS-SECTION                  
188200                                                                          
188300     MOVE SPACE            TO ALL-SSA                                     
188400     MOVE '  '             TO GODK-STATUSKODER                            
188500     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
188600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900                                                                          
189000                                                                          
189100 IMS-GNP-WDE411 SECTION.                                                  
189200     MOVE 'IMS-GNP-WDE411  '      TO CURRENT-IMS-SECTION                  
189300                                                                          
189400     MOVE SPACE                 TO ALL-SSA                                
189500     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
189600            DELIMITED BY SIZE INTO SSA1                                   
189700     MOVE '  GE'                TO GODK-STATUSKODER                       
189800     CALL CBLTDLI USING GNP    WDE4-PCB DLI-IO-E411 SSA1                  
189900     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200                                                                          
190300                                                                          
190400 IMS-GHU-WDE411 SECTION.                                                  
190500     MOVE 'IMS-GHU-WDE411  '      TO CURRENT-IMS-SECTION                  
190600                                                                          
190700     MOVE SPACE                 TO ALL-SSA                                
190800     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
190900            DELIMITED BY SIZE INTO SSA1                                   
191000     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
191100            DELIMITED BY SIZE INTO SSA2                                   
191200     MOVE '    '                TO GODK-STATUSKODER                       
191300     CALL CBLTDLI USING GHU   WDE4-PCB DLI-IO-E411 SSA1 SSA2              
191400     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
191500     PERFORM IMS-STATUSKONTROLL                                           
191600     .                                                                    
191700                                                                          
191800                                                                          
191900 IMS-REPL-WDE411 SECTION.                                                 
192000     MOVE 'IMS-REPL-WDE411 '      TO CURRENT-IMS-SECTION                  
192100                                                                          
192200     MOVE SPACE            TO ALL-SSA                                     
192300     MOVE '    '           TO GODK-STATUSKODER                            
192400     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
192500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
192600     PERFORM IMS-STATUSKONTROLL                                           
192700     .                                                                    
192800                                                                          
192900                                                                          
193000 IMS-GHU-WDQ301 SECTION.                                                  
193100     MOVE 'IMS-GHU-WDQ301  '      TO CURRENT-IMS-SECTION                  
193200                                                                          
193300     MOVE SPACE                 TO ALL-SSA                                
193400     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-X ')'                        
193500            DELIMITED BY SIZE INTO SSA1                                   
193600     MOVE '  '                  TO GODK-STATUSKODER                       
193700     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
193800     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
193900     PERFORM IMS-STATUSKONTROLL                                           
194000     .                                                                    
194100                                                                          
194200                                                                          
194300 IMS-REPL-WDQ301 SECTION.                                                 
194400     MOVE 'IMS-REPL-WDQ301 '      TO CURRENT-IMS-SECTION                  
194500                                                                          
194600     MOVE SPACE            TO ALL-SSA                                     
194700     MOVE '    '           TO GODK-STATUSKODER                            
194800     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
194900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
195000     PERFORM IMS-STATUSKONTROLL                                           
195100     .                                                                    
195200                                                                          
195300                                                                          
195400 IMS-ISRT-WDQ101 SECTION.                                                 
195500     MOVE 'IMS-ISRT-WDQ101 '      TO CURRENT-IMS-SECTION                  
195600                                                                          
195700     MOVE SPACE            TO ALL-SSA                                     
195800     MOVE 'WLORQM01 '      TO SSA1                                        
195900     MOVE '  II'           TO GODK-STATUSKODER                            
196000     CALL CBLTDLI USING ISRT ORQM-PCB OBKR-WDQ101 SSA1                    
196100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400                                                                          
196500                                                                          
196600 IMS-GU-WDE601 SECTION.                                                   
196700     MOVE 'IMS-GU-WDE601   '      TO CURRENT-IMS-SECTION                  
196800                                                                          
196900     MOVE SPACE                 TO ALL-SSA                                
197000     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
197100            DELIMITED BY SIZE INTO SSA1                                   
197200     MOVE '  GE'                TO GODK-STATUSKODER                       
197300     CALL CBLTDLI USING GU     WDE6-PCB DLI-IO-E601 SSA1                  
197400     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700                                                                          
197800                                                                          
197900 IMS-GHU-WDE601 SECTION.                                                  
198000     MOVE 'IMS-GHU-WDE601  '      TO CURRENT-IMS-SECTION                  
198100                                                                          
198200     MOVE SPACE                 TO ALL-SSA                                
198300     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
198400            DELIMITED BY SIZE INTO SSA1                                   
198500     MOVE '  GE'                TO GODK-STATUSKODER                       
198600     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-E601 SSA1                  
198700     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
198800     PERFORM IMS-STATUSKONTROLL                                           
198900     .                                                                    
199000                                                                          
199100                                                                          
199200 IMS-REPL-WDE601 SECTION.                                                 
199300     MOVE 'IMS-REPL-WDE601 '      TO CURRENT-IMS-SECTION                  
199400                                                                          
199500     MOVE SPACE            TO ALL-SSA                                     
199600     MOVE '    '           TO GODK-STATUSKODER                            
199700     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
199800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
200100                                                                          
200200                                                                          
200300 IMS-GHU-WDQ201 SECTION.                                                  
200400     MOVE 'IMS-GHU-WDQ201  '      TO CURRENT-IMS-SECTION                  
200500                                                                          
200600     MOVE SPACE                 TO ALL-SSA                                
200700     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
200800            DELIMITED BY SIZE INTO SSA1                                   
200900     MOVE '  GE'                TO GODK-STATUSKODER                       
201000     CALL CBLTDLI USING GHU     ORQI-PCB OHUV-WDQ201 SSA1                 
201100     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
201200     PERFORM IMS-STATUSKONTROLL                                           
201300     .                                                                    
201400                                                                          
201500                                                                          
201600 IMS-GHNP-WDQ211 SECTION.                                                 
201700     MOVE 'IMS-GHNP-WDQ211 '      TO CURRENT-IMS-SECTION                  
201800                                                                          
201900     MOVE SPACE                 TO ALL-SSA                                
202000     STRING 'WLORQI11(WDQ211KY =' W-WDQ211-X ')'                          
202100            DELIMITED BY SIZE INTO SSA1                                   
202200     MOVE '  '                  TO GODK-STATUSKODER                       
202300     CALL CBLTDLI USING GHNP    ORQI-PCB DIRL-WDQ211 SSA1                 
202400     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
202500     PERFORM IMS-STATUSKONTROLL                                           
202600     .                                                                    
202700                                                                          
202800                                                                          
202900 IMS-REPL-ORQI SECTION.                                                   
203000     MOVE 'IMS-REPL-ORQI   '      TO CURRENT-IMS-SECTION                  
203100                                                                          
203200     MOVE SPACE            TO ALL-SSA                                     
203300     MOVE '    '           TO GODK-STATUSKODER                            
203400     CALL CBLTDLI USING REPL ORQI-PCB DIRL-WDQ211                         
203500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800                                                                          
203900                                                                          
204000 IMS-ISRT-ZZAC01 SECTION.                                                 
204100*    WDG601                                                               
204200     MOVE 'IMS-ISRT-ZZAC01 '      TO CURRENT-IMS-SECTION                  
204300                                                                          
204400     MOVE SPACE            TO ALL-SSA                                     
204500     MOVE 'WLZZAC01'       TO SSA1                                        
204600     MOVE '  II'           TO GODK-STATUSKODER                            
204700     CALL CBLTDLI USING ISRT ZZAC-PCB IO-WDGZ01 SSA1                      
204800     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     .                                                                    
205100                                                                          
205200                                                                          
205300 IMS-GHN-WDA6B SECTION.                                                   
205400     MOVE 'IMS-GHN-WDA6B   '      TO CURRENT-IMS-SECTION                  
205500                                                                          
205600     MOVE SPACE                 TO ALL-SSA                                
205700     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
205800                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
205900            DELIMITED BY SIZE INTO SSA1                                   
206000     MOVE '  GEGB'              TO GODK-STATUSKODER                       
206100     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
206200     MOVE WDA6B-STATUS-CODE     TO STATUS-WS                              
206300     PERFORM IMS-STATUSKONTROLL                                           
206400     .                                                                    
206500                                                                          
206600                                                                          
206700 IMS-REPL-WDA6B SECTION.                                                  
206800     MOVE 'IMS-REPL-WDA6B  '      TO CURRENT-IMS-SECTION                  
206900                                                                          
207000     MOVE SPACE                TO ALL-SSA                                 
207100     MOVE 'WDA601  '           TO SSA1                                    
207200     MOVE '    '               TO GODK-STATUSKODER                        
207300     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
207400     MOVE WDA6B-STATUS-CODE    TO STATUS-WS                               
207500     PERFORM IMS-STATUSKONTROLL                                           
207600     .                                                                    
207700                                                                          
207800                                                                          
207900 IMS-GU-WDB201 SECTION.                                                   
208000     MOVE 'IMS-GU-WDB201   '      TO CURRENT-IMS-SECTION                  
208100                                                                          
208200     MOVE SPACE               TO ALL-SSA                                  
208300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
208400          DELIMITED BY SIZE INTO SSA1                                     
208500     MOVE '  GE'              TO GODK-STATUSKODER                         
208600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
208700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000                                                                          
209100                                                                          
209200 IMS-GHU-WDF411 SECTION.                                                  
209300     MOVE 'IMS-GHU-WDF411  ' TO CURRENT-IMS-SECTION                       
209400                                                                          
209500     MOVE SPACE               TO ALL-SSA                                  
209600     STRING 'WDF401  (IDLEVNR  =' W-IDLEVNR-F4-X ')'                      
209700          DELIMITED BY SIZE INTO SSA1                                     
209800     STRING 'WDF411  (WDF411KY>=' W-WDF411KY-MIN-X                        
209900                    '&WDF411KY<=' W-WDF411KY-MAX-X ')'                    
210000          DELIMITED BY SIZE INTO SSA2                                     
210100     MOVE '  GE'              TO GODK-STATUSKODER                         
210200     CALL CBLTDLI USING GHU WDF4-PCB DLI-IO-F411 SSA1 SSA2                
210300     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
210400     PERFORM IMS-STATUSKONTROLL                                           
210500     .                                                                    
210600                                                                          
210700                                                                          
210800 IMS-REPL-WDF411 SECTION.                                                 
210900     MOVE 'IMS-REPL-WDF411 ' TO CURRENT-IMS-SECTION                       
211000                                                                          
211100     MOVE SPACE              TO ALL-SSA                                   
211200     MOVE '  '               TO GODK-STATUSKODER                          
211300     CALL CBLTDLI USING REPL WDF4-PCB DLI-IO-F411                         
211400     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
211500     PERFORM IMS-STATUSKONTROLL                                           
211600     .                                                                    
211700                                                                          
211800                                                                          
211900 IMS-DLET-WDF411 SECTION.                                                 
212000     MOVE 'IMS-DLET-WDF411 ' TO CURRENT-IMS-SECTION                       
212100                                                                          
212200     MOVE SPACE              TO ALL-SSA                                   
212300     MOVE '  '               TO GODK-STATUSKODER                          
212400     CALL CBLTDLI USING DLET WDF4-PCB DLI-IO-F411                         
212500     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
212600     PERFORM IMS-STATUSKONTROLL                                           
212700     .                                                                    
212800                                                                          
212900                                                                          
213000 DB2-SELECT-TP4TRAN     SECTION.                                          
213100     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
213200                                                                          
213300     MOVE 000100 TO GODK-SQLCODEKODER                                     
213400                                                                          
213500     EXEC SQL                                                             
213600           SELECT  DISTINCT                                               
213700                   IDDC_REC                                               
213800                                                                          
213900           INTO   :TP4TRAN-IDDC-REC                                       
214000                                                                          
214100           FROM    TP4TRAN                                                
214200                                                                          
214300           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
214400     END-EXEC                                                             
214500                                                                          
214600     MOVE SQLCODE TO SQLCODE-WS                                           
214700     PERFORM DB2-STATUSKONTROLL                                           
214800     .                                                                    
214900                                                                          
215000                                                                          
215100 IMS-STATUSKONTROLL SECTION.                                              
215200     SET STATUS-IX TO 1                                                   
215300     SEARCH GODK-STATUS AT END CALL FELLOG                                
215400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
215500     END-SEARCH                                                           
215600     .                                                                    
215700                                                                          
215800                                                                          
215900 DB2-STATUSKONTROLL  SECTION.                                             
216000                                                                          
216100     SET SQLCODE-IX TO 1                                                  
216200     SEARCH GODK-SQLCODE                                                  
216300       AT END                                                             
216400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
216500          DELIMITED BY SIZE INTO FELTEXT-STR                              
216600          CALL ABEND USING RKOD-ABEND-DB2                                 
216700       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
216800     END-SEARCH                                                           
216900     .                                                                    
