000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035900.                                                
000400 AUTHOR.         EVA LUNDELL.                                             
000500 DATE-WRITTEN.   92/09/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800**   FUNKTION:                                                            
000900*        PROGRAMMET ANVÄNDS FÖR TRANSFERS MELLAN NDC:ER OCH/ELLER         
001000*        NDC/CDC SAMT FÖR ATT BEORDRA SKROTNING HOS NDC:ERNA              
001100*                                                                         
001200*  FÖR TRANSFER GÄLLER:                                                   
001300** VANLIGA (NORMALA ORDRAR) MED ORDERKLASS 3 SKA LÄGGAS UPP SOM EN        
001400** REFILL-ORDER PÅ WDE3-BASEN, FAST MED EGNA KUND- OCH DISTRIKTS-         
001500** NUMMER (DE MAN TIDIGARE I PROGRAMMET LETAT FRAM)                       
001600** DET FÖRUTSÄTTS ATT ANVÄNDARNA VET VAD DE HÅLLER PÅ MED SÅ              
001700** OM MAN UNDER DAGEN SKAPAT EN ORDER MED ARTIKEL, LEV KUND OCH           
001800** DISTRIKT OCH MAN SENARE UNDER DAGEN GÖR EN UPPDATERING AV SAMMA        
001900** ARTIKEL DITRIKT KUND SÅ FÖRUTSÄTTS DET ATT DET SOM MAN ANGER PÅ        
002000** BILDEN ÄR DET KORREKTA.                                                
002100** BORTTAG ÄR INTE MÖJLIGT, MAN LÄGGER NOLL I ANTAL I STÄLLET.            
002200**                                                                        
002300*                                                                         
002400*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002500*                              WLARTS (WDK7)                              
002600*                              WLOIGA (WDL7)                              
002700*                              WL6321 (WDR5)                              
002800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002900*                              WL4109 (WDGX)                              
003000*                                      WDK9                               
003100*                                      WDN6                               
003200*        PROGRAMMET ANROPAR    W411SAP FÖR KONTROLL AV                    
003300*                              KONTERINGSINFO, WLSAPC (WDH3)              
003400*                                                                         
003500*    INDATA.                                                              
003600*        TRANSAKTION: W2T359                                              
003700*        MID:         W2I35901                                            
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W2O35901                                            
004100*                                                                         
004200*    E'TRACKER 4823800 DAT. 20071204 NEW LDC                              
004300*                                                                         
004400*                                                                         
004500                                                                          
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000 WORKING-STORAGE SECTION.                                                 
005100*    --- CHECKED BY WY2000                                                
005200 77  IDPGM                       PIC X(08)   VALUE 'W2035900'.            
005300                                                                          
005400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005600                                                                          
005700 77  JA                          PIC X     VALUE 'J'.                     
005800 77  YES                         PIC X     VALUE 'Y'.                     
005900 77  NEJ                         PIC X     VALUE 'N'.                     
006000 77  SPR                         PIC S9    VALUE +1 COMP-3.               
006100 77  W-ANTAL-SKROT               PIC S9(7) COMP-3 VALUE ZERO.             
006200 77  W-KVSKROT-KVAR-NUM          PIC S9(7) COMP-3 VALUE ZERO.             
006300 77  W-KVRETUR-TRANSFER          PIC S9(7) COMP-3 VALUE ZERO.             
006400 01  WS-IDKONTO-X.                                                        
006500  03 WS-IDKONTO                  PIC 9(10) VALUE ZERO.                    
006600 77  WS-KDARBTYP                 PIC X(8)  VALUE SPACE.                   
006700 77  WS-KDARBTYP-X3              PIC X(3)  VALUE 'ESC'.                   
006800 77  WS-KDARBTYP-SEC             PIC X(8)  VALUE SPACE.                   
006810 77  W-IDLEVNR-SEND              PIC X(5)  VALUE SPACE.                   
006900 77  WS-OSPARRAT-ANTAL           PIC S9(7) COMP-3 VALUE ZERO.             
007000 77  WS-SPARA-IDPERSON-BUY       PIC 9(3)         VALUE ZERO.             
007100 77  WS-RET-IDDISTR              PIC 9(4)    VALUE ZERO.                  
007200 77  WS-SKROT-IDDISTR            PIC 9(4)    VALUE ZERO.                  
007300 01  WS-RED-IDKONTO              PIC Z(9)9   VALUE ZERO.                  
007400 77  W-KVRETUR-TRANSFER-TRUNK    PIC 9(6)  VALUE ZERO.                    
007500 77  W-SCRAPPED-VALUE            PIC S9(9) COMP-3 VALUE ZERO.             
007600 77  W-KVSKROT                   PIC S9(9) COMP-3 VALUE ZERO.             
007700 77  W-KVSKROT-DISPLAY           PIC 9(6)  VALUE ZERO.                    
007800 77  W-KVSTOCK                   PIC 9(7)  VALUE ZERO.                    
007900 77  IX                          PIC 9(3)    VALUE ZERO.                  
008000 77  IX2                         PIC 9(3)    VALUE ZERO.                  
008100 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
008200 77  IX-VV                       PIC 9(2)    VALUE ZERO.                  
008300 77  INDX                        PIC S9(3)   VALUE ZERO.                  
008400 77  INDX-2                      PIC S9(4)  VALUE +0    COMP SYNC.        
008500 77  INDX-3                      PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  BEEMB-IX                    PIC 9(3)    VALUE ZERO.                  
008700 77  FILLER                      PIC X     VALUE '-'.                     
008800 77  NOLLOR                      PIC 9     VALUE ZERO.                    
008900 77  W-IDDC-FROM                 PIC X(2)  VALUE SPACE.                   
009000 77  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
009100 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
009200 77  FILLER                      PIC X     VALUE '+'.                     
009300 77  WS-SPAR-BEANST-GODK         PIC X(25) VALUE SPACE.                   
009400 77  DAGENS-TID                  PIC 9(8)  VALUE ZERO.                    
009500 77  WS-KVOKS-CDC                PIC S9(6) VALUE ZERO.                    
009600                                                                          
009700*01  -COPY WWPRODSL                                                       
009800*                                                                         
009900*01  -COPY WWDCKONS                                                       
010000*                                                                         
010100*01    -COPY WWDC99                                                       
010200*                                                                         
010300*01    -COPY WWDC99     -PRE FROM-                                        
010400*                                                                         
010500*01    -COPY WWDC99     -PRE SEND-                                        
010600                                                                          
010700 01  W-SDC-KVLS                  PIC S9(7) VALUE ZERO COMP-3.             
010800 01  W-SDC-KVAKS                 PIC S9(7) VALUE ZERO COMP-3.             
010900 01  W-SDC-KVOKS                 PIC S9(7) VALUE ZERO COMP-3.             
011000                                                                          
011100 01 NYCKLAR-TP4TRAN.                                                      
011200     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
011300     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
011400                                                                          
011500 01  DAGENS-TID-I-DELAR.                                                  
011600     03  FILLER                  PIC 9(1).                                
011700     03  DAGENS-TIT              PIC 9(1).                                
011800     03  DAGENS-TIMM             PIC 9(2).                                
011900     03  FILLER                  PIC 9(4).                                
012000 01  DAGENS-AAVVD.                                                        
012100     03  FILLER                  PIC 9(3).                                
012200     03  DAGENS-VECKA            PIC 9(1).                                
012300     03  DAGENS-DAG              PIC 9(1).                                
012400                                                                          
012500 01  W-TRANSFER-TEXT.                                                     
012600     03  W-TRANSFER-TEXT-1       PIC X(30) VALUE SPACE.                   
012700     03  W-TRANSFER-TEXT-2       PIC X(30) VALUE SPACE.                   
012800                                                                          
012900 01  W-TRANSFER-DEFAULT-TEXT     PIC X(40) VALUE                          
013000     'PLEASE INFORM VCAS OF ANY DISCREPANCIES'.                           
013100                                                                          
013200 01  W-TRANSFER-DEFAULT-TEXT-RAD PIC X(10) VALUE                          
013300     'PICK TOTAL'.                                                        
013400                                                                          
013500 01  W-SKROT-TEXT-DEFAULT.                                                
013600     03 FILLER                      PIC X(40) VALUE                       
013700     'REMAINING PIECES; SEE LINE REFERENCE.'.                             
013800     03 W-SKROT-TEXT                PIC X(20) VALUE SPACE.                
013900                                                                          
014000 01  W-SKROT-TEXT-ORAD.                                                   
014100     03 W-KVSKROT-KVAR           PIC X(7)  VALUE SPACE.                   
014200     03 FILLER                   PIC X(3)  VALUE 'PCS'.                   
014300                                                                          
014400 01  MEDDELANDEN.                                                         
014500     03 FEL-1-AREA.                                                       
014600        05 FILLER                PIC X(49)                                
014700           VALUE 'ARTIKEL SAKNAS PÅ DETTA DC                  '.          
014800        05 FILLER                PIC X(49)                                
014900           VALUE 'PART MISSING ON THIS DC                     '.          
015000     03 FILLER REDEFINES FEL-1-AREA.                                      
015100        05 FEL-1 OCCURS 2        PIC X(49).                               
015200                                                                          
015300     03 MED-1-AREA.                                                       
015400        05 FILLER                PIC X(49)                                
015500           VALUE 'ANALYSNUMMER SAKNAS.  GÅ TILL BILD 4702     '.          
015600        05 FILLER                PIC X(49)                                
015700           VALUE 'ANALYSIS NUMBER MISSING.  GO TO SCREEN 4702 '.          
015800     03 FILLER REDEFINES MED-1-AREA.                                      
015900        05 MED-1 OCCURS 2        PIC X(49).                               
016000                                                                          
016100     03 MED-2-AREA.                                                       
016200        05 FILLER                PIC X(25)                                
016300           VALUE 'FEL ORDERKLASS      '.                                  
016400        05 FILLER                PIC X(25)                                
016500           VALUE 'WRONG ORDER CLASS   '.                                  
016600     03 FILLER REDEFINES MED-2-AREA.                                      
016700        05 MED-2 OCCURS 2        PIC X(25).                               
016800                                                                          
016900     03 MED-3-AREA.                                                       
017000        05 FILLER                PIC X(25)                                
017100           VALUE 'FEL FRAKTKOD        '.                                  
017200        05 FILLER                PIC X(25)                                
017300           VALUE 'WRONG FREIGHT CODE  '.                                  
017400     03 FILLER REDEFINES MED-3-AREA.                                      
017500        05 MED-3 OCCURS 2        PIC X(25).                               
017600                                                                          
017700     03 MED-4-AREA.                                                       
017800        05 FILLER                PIC X(35)                                
017900           VALUE 'CDC ARTIKEL, LOCAL BUYER      '.                        
018000        05 FILLER                PIC X(35)                                
018100           VALUE 'CDC PART, LOCAL BUYER         '.                        
018200     03 FILLER REDEFINES MED-4-AREA.                                      
018300        05 MED-4 OCCURS 2        PIC X(35).                               
018400                                                                          
018500     03 MED-5-AREA.                                                       
018600        05 FILLER                PIC X(35)                                
018700           VALUE 'FORTFARANDE KVALITETSSPÄRRAD   '.                       
018800        05 FILLER                PIC X(35)                                
018900           VALUE 'QUALITY BLOCKED QTY STILL EXIST'.                       
019000     03 FILLER REDEFINES MED-5-AREA.                                      
019100        05 MED-5 OCCURS 2        PIC X(35).                               
019200                                                                          
019300     03 MED-6-AREA.                                                       
019400        05 FILLER                PIC X(25)                                
019500           VALUE '*** KVALITETSPÄRR ANTAL ='.                             
019600        05 FILLER                PIC X(25)                                
019700           VALUE '*** QUALITY BLOCKED QTY ='.                             
019800     03 FILLER REDEFINES MED-6-AREA.                                      
019900        05 MED-6 OCCURS 2        PIC X(25).                               
020000                                                                          
020100     03 MED-7-AREA.                                                       
020200        05 FILLER                PIC X(27)                                
020300           VALUE '*** TOTALT KVALITETSSPÄRRAD'.                           
020400        05 FILLER                PIC X(27)                                
020500           VALUE '*** TOTALY QUALITY BLOCKED '.                           
020600     03 FILLER REDEFINES MED-7-AREA.                                      
020700        05 MED-7 OCCURS 2        PIC X(27).                               
020800                                                                          
020900     03 MED-8-AREA.                                                       
021000        05 FILLER                PIC X(27)                                
021100           VALUE '*** ORDERSPÄRRAD           '.                           
021200        05 FILLER                PIC X(27)                                
021300           VALUE '*** ORDER BLOCKED          '.                           
021400     03 FILLER REDEFINES MED-8-AREA.                                      
021500        05 MED-8 OCCURS 2        PIC X(27).                               
021600                                                                          
021700     03 MED-9-AREA.                                                       
021800        05 FILLER                PIC X(35)                                
021900           VALUE 'USA ARTIKEL, CDC BUYER     '.                           
022000        05 FILLER                PIC X(35)                                
022100           VALUE 'LOCAL PART, CDC BUYER      '.                           
022200     03 FILLER REDEFINES MED-9-AREA.                                      
022300        05 MED-9 OCCURS 2        PIC X(35).                               
022400                                                                          
022500     03 MED-10-AREA.                                                      
022600        05 FILLER                PIC X(35)                                
022700           VALUE 'LOKAL PART, RETUR EJ TILLÅTEN'.                         
022800        05 FILLER                PIC X(35)                                
022900           VALUE 'LOCAL PART, NOT TO BE RETURNED'.                        
023000     03 FILLER REDEFINES MED-10-AREA.                                     
023100        05 MED-10 OCCURS 2        PIC X(35).                              
023200     03 MED-11-AREA.                                                      
023300        05 FILLER                PIC X(35)                                
023400           VALUE 'SKROTNING EJ TILLÅTEN        '.                         
023500        05 FILLER                PIC X(35)                                
023600           VALUE 'SCRAPPING NOT ALLOWED         '.                        
023700     03 FILLER REDEFINES MED-11-AREA.                                     
023800        05 MED-11 OCCURS 2        PIC X(35).                              
023900     03 MED-12-AREA.                                                      
024000        05 FILLER                PIC X(35)                                
024100           VALUE 'TRANSFER EJ TILLÅTEN         '.                         
024200        05 FILLER                PIC X(35)                                
024300           VALUE 'TRANSFER NOT ALLOWED          '.                        
024400     03 FILLER REDEFINES MED-12-AREA.                                     
024500        05 MED-12 OCCURS 2        PIC X(35).                              
024600     03 MED-13-AREA.                                                      
024700        05 FILLER                PIC X(35)                                
024800           VALUE 'ENDAST LOKAL ARTIKEL TILLÅTEN'.                         
024900        05 FILLER                PIC X(35)                                
025000           VALUE 'ONLY LOCAL PART ALLOWED       '.                        
025100     03 FILLER REDEFINES MED-13-AREA.                                     
025200        05 MED-13 OCCURS 2        PIC X(35).                              
025300     03 MED-14-AREA.                                                      
025400        05 FILLER                PIC X(35)                                
025500           VALUE 'KONFLIKT SNABBORDER/ TOTAL    '.                        
025600        05 FILLER                PIC X(35)                                
025700           VALUE 'CONFLICT FAST ORDER/ TOTAL   '.                         
025800     03 FILLER REDEFINES MED-14-AREA.                                     
025900        05 MED-14 OCCURS 2        PIC X(35).                              
026000     03 MED-15-AREA.                                                      
026100        05 FILLER                PIC X(35)                                
026200           VALUE 'INGEN REFILL ARTIKEL          '.                        
026300        05 FILLER                PIC X(35)                                
026400           VALUE 'NOT A REFILL PART            '.                         
026500     03 FILLER REDEFINES MED-15-AREA.                                     
026600        05 MED-15 OCCURS 2        PIC X(35).                              
026700     03 MED-16-AREA.                                                      
026800        05 FILLER                PIC X(35)                                
026900           VALUE 'INGET MATERIALPRIS            '.                        
027000        05 FILLER                PIC X(35)                                
027100           VALUE 'NO MATERIALPRICE             '.                         
027200     03 FILLER REDEFINES MED-16-AREA.                                     
027300        05 MED-16 OCCURS 2        PIC X(35).                              
027400       EJECT                                                              
027500                                                                          
027600 77  SPARA-IDDISTR               PIC X(4)  VALUE SPACE.                   
027700 77  SPARA-IDKUNDNR              PIC X(6)  VALUE SPACE.                   
027800 77  SPARA-IDORDNR               PIC X(7)  VALUE SPACE.                   
027900 77  SPARA-IDDISTR-NUM           PIC 9(4)  VALUE ZERO.                    
028000 77  SPARA-IDKUNDNR-NUM          PIC 9(6)  VALUE ZERO.                    
028100                                                                          
028200*    ARBETSFÄLT FÖR KONTROLL AV MULTIPEL AV KVQPACK                       
028300 77  WS-KVQPACK-ANT              PIC S9(9)   VALUE ZERO.                  
028400 77  WS-KVQPACK-REST             PIC S9V9(9) VALUE ZERO.                  
028500*    ARBETSFÄLT FÖR INFORMATION OM KVALITETSSPÄRR                         
028600 77  WS-KVSPARR-KVAL             PIC Z(6)9 VALUE ZERO.                    
028700                                                                          
028800 01  FILLER       PIC X(24)         VALUE  'W-ORDERNUMMER'.               
028900 01  W-ORDERNR.                                                           
029000     03  W-ORDER-TIMM            PIC 9(2)  VALUE ZERO.                    
029100     03  W-ORDER-TIT             PIC 9(1)  VALUE ZERO.                    
029200     03  W-ORDER-DAG             PIC 9(1)  VALUE ZERO.                    
029300     03  W-ORDER-V               PIC 9(1)  VALUE ZERO.                    
029400                                                                          
029500 01  FILLER       PIC X(24)         VALUE  'W-ORDERNUMMER-7POS'.          
029600 01  W-ORDERNR-7POS.                                                      
029700     03  FILLER                  PIC 9(2)  VALUE ZERO.                    
029800     03  W-ORDERNR-NUM           PIC 9(5)  VALUE ZERO.                    
029900                                                                          
030000 01  FILLER       PIC X(24)      VALUE  'MOTTAGANDE-LAGER-ADRESS'.        
030100 01  MOTTAGANDE-LAGER-ADRESS.                                             
030200     03 WS-ADART-X.                                                       
030300        05 WS-ADLAGOMR-X    PIC  X(2)     VALUE SPACE.                    
030400        05 WS-ADGANG-X      PIC  X(2)     VALUE SPACE.                    
030500        05 WS-ADPLATS-X     PIC  X(5)     VALUE SPACE.                    
030600        05 FILLER           PIC  X(1)     VALUE SPACE.                    
030700*                                                                         
030800     03 WS-ADDRESS.                                                       
030900        05 WS-ADLAGOMR-N    PIC  9(2)     VALUE ZERO.                     
031000        05 WS-ADGANG-N      PIC  9(2)     VALUE ZERO.                     
031100        05 WS-ADPLATS-N     PIC  9(5)     VALUE ZERO.                     
031200     EJECT                                                                
031300 01 DB2-LASNING.                                                          
031400     03 FILLER                   PIC X(16)   VALUE                        
031500                                             'WS-DB2-SEKTION'.            
031600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
031700                                                                          
031800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
031900                                                                          
032000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
032100     88  INDATA-OK                           VALUE 'J'.                   
032200     88  INDATA-FEL                          VALUE 'N'.                   
032300                                                                          
032400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
032500     88  NYCKLAR-OK                          VALUE 'J'.                   
032600     88  NYCKLAR-FEL                         VALUE 'N'.                   
032700                                                                          
032800 77  TRANSFER-IFYLLT-SW          PIC X       VALUE 'N'.                   
032900     88  TRANSFER-IFYLLT                     VALUE 'J'.                   
033000                                                                          
033100 77  RETUR-IAF-SW                PIC X       VALUE 'N'.                   
033200     88  RETUR-IAF                           VALUE 'J'.                   
033300                                                                          
033400 77  SKROT-IFYLLT-SW             PIC X       VALUE 'N'.                   
033500     88  SKROT-IFYLLT                        VALUE 'J'.                   
033600                                                                          
033700 77  SKROT-AUTO-SW               PIC X       VALUE 'N'.                   
033800     88  SKROT-AUTO                          VALUE 'J'.                   
033900                                                                          
034000 77  ORDER-FINNS-SW              PIC X       VALUE 'N'.                   
034100     88  ORDER-FINNS                         VALUE 'J'.                   
034200                                                                          
034300 77  ORDERSPARRAD-SW             PIC X       VALUE 'N'.                   
034400     88  ORDERSPARR-ARTIKEL                  VALUE 'J'.                   
034500                                                                          
034600 77  DIREKTLEV-SW                PIC X       VALUE 'N'.                   
034700     88  DIREKTLEV-ARTIKEL                   VALUE 'J'.                   
034800                                                                          
034900 77  NOSPLITBULK-SW              PIC X       VALUE 'N'.                   
035000     88  NOSPLITBULK                         VALUE 'J'.                   
035100                                                                          
035200 77  ERR-TISKROT-SW              PIC X       VALUE 'N'.                   
035300     88  ERR-TISKROT                         VALUE 'J'.                   
035400                                                                          
035500 77  NO-TRANS-SW                 PIC X       VALUE 'N'.                   
035600     88  NO-TRANS                            VALUE 'J'.                   
035700                                                                          
035800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
035900     88  EGEN-MID                            VALUE '2359'.                
036000     88  GODK-MID                            VALUE '2351' '2352'          
036100                                                   '2353' '2354'          
036200                                                   '2355' '2356'          
036300                                                   '2357' '2358'          
036400                                                   '2359'.                
036500     88  HELP-MID                            VALUE '0551'.                
036600     EJECT                                                                
036700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
036800 01  GENERELLA-SUBPROGRAM.                                                
036900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
037000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
037100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
037200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037300   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
037400   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
037500   03  W411ORDN                  PIC X(8)    VALUE 'W411ORDN'.            
037600   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
037700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
037800   03  W411SAP                   PIC X(8)    VALUE 'W411SAP'.             
037900     EJECT                                                                
038000 01  RETURKOD-ABEND.                                                      
038100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16 COMP SYNC.         
038200 01  KONTROLL-SIFFRA.                                                     
038300   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
038400   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
038500   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
038600     EJECT                                                                
038700 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
038800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
038900*01 -COPY WMEDAREA                                                        
039000     EJECT                                                                
039100 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
039200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
039300*01 -COPY WDATAREA                                                        
039400     EJECT                                                                
039500 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
039600*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
039700*01 -COPY W411ORDN                                                        
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
040000*    --- PARAMETRAR TILL SUBPROGRAM WMSGINIT                              
040100*01 -COPY WMSGINIT                                                        
040200     EJECT                                                                
040300 01  FILLER                      PIC X(16)   VALUE 'W411SAP '.            
040400*    --- PARAMETRAR TILL SUBPROGRAM W411SAP                               
040500*01 -COPY W411SAP                                                         
040600     SKIP3                                                                
040700*01  -COPY WWIDFTG                                                        
040800     EJECT                                                                
040900 01  MESSAGE-CODES.                                                       
041000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
041100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
041200     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
041300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
041400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
041500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
041600     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
041700     03  ERR-WRONG-FREIGHT-CODE  PIC X(3)    VALUE '086'.                 
041800     03  ERR-ORDER-NOT-AVAILABLE PIC X(3)    VALUE '248'.                 
041900     03  ERR-NO-SPLIT-BULK-PACK  PIC X(3)    VALUE '419'.                 
042000     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
042100     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
042200     03  DIRECTLY-DELIVERED-PART PIC X(3)    VALUE '306'.                 
042300     03  NOT-AUTHORIZED-TO-SCRAP PIC X(3)    VALUE '601'.                 
042400     03  ERR-INVALID-DATE        PIC X(3)    VALUE '492'.                 
042500     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
042600     EJECT                                                                
042700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
042800*                                                                         
042900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
043000     SKIP3                                                                
043100*01  MID -COPY W2I35901                                                   
043200     EJECT                                                                
043300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
043400     SKIP3                                                                
043500*01  -COPY WMSGAREA                                                       
043600     EJECT                                                                
043700     03  MOD REDEFINES MSG-AREA.                                          
043800*      05  -COPY W2O35901                                                 
043900     EJECT                                                                
044000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
044100     SKIP3                                                                
044200*01  -COPY WMFSAREA                                                       
044300     EJECT                                                                
044400 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
044500*01  -COPY WMSGKOM                                                        
044600     EJECT                                                                
044700                                                                          
044800 01  P-TO-P-SW.                                                           
044900   03  P-TO-P-KVLL           PIC S9(4)   COMP SYNC.                       
045000   03  P-TO-P-KDZ1           PIC X(1)    VALUE LOW-VALUE.                 
045100   03  P-TO-P-KDZ2           PIC X(1)    VALUE LOW-VALUE.                 
045200   03  P-TO-P-KDTRANS        PIC X(8).                                    
045300   03  P-TO-P-IDTRANS        PIC X(4).                                    
045400   03  P-TO-P-KDMFSFOR       PIC X(1).                                    
045500   03  P-TO-P-DATA           PIC X(1000).                                 
045600     EJECT                                                                
045700*                                                                         
045800*    --- AREOR FÖR W006KOM SUBMODUL                                       
045900*                                                                         
046000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
046100 01  KOM-IO-AREA.                                                         
046200   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
046300*03  FILLER  -COPY W4I25101 -PRE OHUV-  -RED KOM-AREA.                    
046400     EJECT                                                                
046500*03  FILLER  -COPY W4I25201 -PRE ORAD-  -RED KOM-AREA.                    
046600     EJECT                                                                
046700                                                                          
046800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046900*                                                                         
047000     EJECT                                                                
047100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
047200     SKIP3                                                                
047300 01  NYCKLAR-TILL-DLI.                                                    
047400     03  W-IDARTNR-X.                                                     
047500         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
047600     03  W-KDSEGKEY-X.                                                    
047700         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
047800     03  W-IDDC-X.                                                        
047900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
048000     03  W-IDDC-TO-X.                                                     
048100         05  W-IDDC-TO           PIC X(2)    VALUE SPACE.                 
048200     03  W-IDDC-B6-X.                                                     
048300         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
048400     03  W-IDLAND-X.                                                      
048500         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
048600     03  W-WDGXKEY-ROT-X.                                                 
048700         05  FILLER              PIC X(04)   VALUE '4109'.                
048800         05  W-IDFTG-4109        PIC 9(02)   VALUE ZERO.                  
048900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
049000     03  W-WDGXKEY-ROT-X-MIN.                                             
049100         05  FILLER              PIC X(04)   VALUE '4109'.                
049200         05  W-IDFTG-4109-MIN    PIC 9(02)   VALUE ZERO.                  
049300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
049400     03  W-WDGXKEY-ROT-X-MAX.                                             
049500         05  FILLER              PIC X(04)   VALUE '4109'.                
049600         05  W-IDFTG-4109-MAX    PIC 9(02)   VALUE 99.                    
049700         05  FILLER              PIC X(24)   VALUE HIGH-VALUE.            
049800     03  W-KEY4110-MIN-X.                                                 
049900         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
050000         05  FILLER              PIC X(10)  VALUE LOW-VALUE.              
050100     03  W-KEY4110-MAX-X.                                                 
050200         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
050300         05  FILLER              PIC X(10)  VALUE LOW-VALUE.              
050400                                                                          
050500     03  W-IDSKYLT-X.                                                     
050600         05  W-IDSKYLT           PIC X(03)    VALUE SPACE.                
050700                                                                          
050800     03 W-WDE301KY-X.                                                     
050900         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
051000         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
051100         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
051200         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
051300         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
051400                                                                          
051500     03  W-WDGX6321-ROT-X.                                                
051600         05  FILLER              PIC X(04) VALUE '6321'.                  
051700         05  W-KDARBTYP          PIC X(08) VALUE SPACE.                   
051800         05  FILLER              PIC X(18) VALUE LOW-VALUE.               
051900     03  W-WDGX6322-KEY-X.                                                
052000         05  W-DASKROT9-BEORD    PIC 9(08) VALUE ZERO.                    
052100     03  W-WDGX6324-KEY-X.                                                
052200         05  W-IDARTNR-6324      PIC S9(9)  COMP-3 VALUE ZERO.            
052300         05  W-IDDC-6324         PIC X(02)  VALUE SPACE.                  
052400         05  W-KDSTASKR-6324     PIC S9     COMP-3 VALUE ZERO.            
052500                                                                          
052600     03  W-WDGXKEY-X.                                                     
052700         05  FILLER             PIC X(4)    VALUE '6327'.                 
052800         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
052900         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
053000         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
053100     03  W-IDUSER-GODK-X.                                                 
053200         05  W-IDUSER-GODK      PIC X(8)    VALUE SPACE.                  
053300                                                                          
053400                                                                          
053500     03  W-IDDC-B6-FROM-X.                                                
053600         05 W-IDDC-B6-FROM             PIC X(2).                          
053700                                                                          
053800     03  W-IDDC-B6-TO-X.                                                  
053900         05 W-IDDC-B6-TO               PIC X(2).                          
054000                                                                          
054100     03  W-IDDC-B6-MSGI-X.                                                
054200         05 W-IDDC-B6-MSGI             PIC X(2).                          
054300                                                                          
054400     03  W-IDDC-B6-KEY-X.                                                 
054500         05 W-IDDC-B6-KEY              PIC X(2).                          
054600                                                                          
054700     03  W-IDDC-B601-X.                                                   
054800         05 W-IDDC-B601                PIC X(2).                          
054900                                                                          
055000     03  W-IDDC-B616-X.                                                   
055100         05 W-IDDC-B616                PIC X(2).                          
055200     SKIP2                                                                
055300*    --- STATUS-KOD FRÅN IMS                                              
055400 01  STATUS-WS                   PIC XX.                                  
055500     88  SEGMENT-FINNS                       VALUE '  '.                  
055600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
055700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
055800     SKIP2                                                                
055900 01  GODK-STATUSKODER.                                                    
056000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
056100     SKIP3                                                                
056200 01  SSA1                        PIC X(128).                              
056300 01  SSA2                        PIC X(64).                               
056400 01  SSA3                        PIC X(64).                               
056500     EJECT                                                                
056600*    --- IMS FUNKTIONSKODER                                               
056700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
056800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
056900                                                                          
057000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
057100 01  DB2-WS.                                                              
057200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
057300         88  CURSOR-OK                       VALUE 000.                   
057400         88  RADER-FINNS                     VALUE 000.                   
057500         88  RADER-SAKNAS                    VALUE 100.                   
057600         88  ATKOMST-FEL                     VALUE 904.                   
057700     03  GODK-SQLCODEKODER.                                               
057800         05  GODK-SQLCODE OCCURS 5                                        
057900             INDEXED BY SQLCODE-IX PIC 9(3).                              
058000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
058100     EJECT                                                                
058200*01  -COPY W0003                                                          
058300     EJECT                                                                
058400*    ---  DLI INPUT-OUTPUT AREA                                           
058500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
058600 01  DLI-IO-ARTC01.                                                       
058700*    03  -COPY WDK601     -PRE ARTC-                                      
058800     EJECT                                                                
058900                                                                          
059000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
059100 01  DLI-IO-ARTC11.                                                       
059200*    03  -COPY WDK611     -PRE ARTC-                                      
059300     EJECT                                                                
059400                                                                          
059500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
059600 01  DLI-IO-ARTS01.                                                       
059700*    03  -COPY WDK701                                                     
059800     EJECT                                                                
059900                                                                          
060000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
060100 01  DLI-IO-ARTS11.                                                       
060200*    03  -COPY WDK711                                                     
060300     EJECT                                                                
060400                                                                          
060500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
060600 01  DLI-IO-WDK711.                                                       
060700*    03  -COPY WDK711 -PRE TO-                                            
060800     EJECT                                                                
060900                                                                          
061000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
061100 01  DLI-IO-WDK712.                                                       
061200*    03  -COPY WDK712                                                     
061300     EJECT                                                                
061400                                                                          
061500 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
061600 01  DLI-IO-BENA11.                                                       
061700*    03  -COPY WDD311                                                     
061800     EJECT                                                                
061900                                                                          
062000 01  FILLER         PIC X(24) VALUE 'DLI-IO-410901'.                      
062100 01  DLI-IO-411001.                                                       
062200*    03  -COPY WDGX01                                                     
062300     EJECT                                                                
062400                                                                          
062500 01  FILLER         PIC X(24) VALUE 'DLI-IO-410911'.                      
062600 01  DLI-IO-411011.                                                       
062700*    03  -COPY WDGX4110                                                   
062800     EJECT                                                                
062900                                                                          
063000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORDL01'.                      
063100 01  DLI-IO-ORDL01.                                                       
063200*    03  -COPY WDE301                                                     
063300     EJECT                                                                
063400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
063500 01  DLI-IO-WDR501-6321.                                                  
063600*    03  -COPY WDGX6321                                                   
063700     EJECT                                                                
063800                                                                          
063900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6322'.                    
064000 01  DLI-IO-WDGX6322.                                                     
064100*    03  -COPY WDGX6322                                                   
064200     EJECT                                                                
064300                                                                          
064400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6324'.                    
064500 01  DLI-IO-WDGX6324.                                                     
064600*    03  -COPY WDGX6324                                                   
064700     EJECT                                                                
064800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6327'.                 
064900 01  DLI-IO-WDR501-6327.                                                  
065000*    03  -COPY WDGX6327                                                   
065100     EJECT                                                                
065200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6328'.                    
065300 01  DLI-IO-WDGX6328.                                                     
065400*    03  -COPY WDGX6328                                                   
065500     EJECT                                                                
065600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-OIGA11'.             
065700     SKIP3                                                                
065800 01  DLI-IO-AREA-OIGA11.                                                  
065900*        05  -COPY WDL711                                                 
066000     EJECT                                                                
066100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
066200 01   DLI-IO-AREA-B601.                                                   
066300*     03  -COPY WDB601                                                    
066400     EJECT                                                                
066500                                                                          
066600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
066700 01  DLI-IO-WDN601.                                                       
066800*    03  -COPY WDN601                                                     
066900     EJECT                                                                
067000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
067100 01  DLI-IO-WDN611.                                                       
067200*    03  -COPY WDN611                                                     
067300     EJECT                                                                
067400 01  FILLER         PIC X(16) Value 'DLI-IO-WDK901'.                      
067500 01  DLI-IO-WDK901.                                                       
067600*    03  -COPY WDK901                                                     
067700     EJECT                                                                
067800                                                                          
067900 01  FILLER               PIC X(16)   VALUE 'WDB601 FROM'.                
068000 01   DLI-IO-AREA-B601-FROM.                                              
068100*     03  -COPY WDB601   -PRE FROM-                                       
068200                                                                          
068300 01  FILLER               PIC X(16)   VALUE 'WDB601 TO  '.                
068400 01   DLI-IO-AREA-B601-TO.                                                
068500*     03  -COPY WDB601   -PRE TO-                                         
068600                                                                          
068700 01  FILLER               PIC X(16)   VALUE 'WDB601 MSGI'.                
068800 01   DLI-IO-AREA-B601-MSGI.                                              
068900*     03  -COPY WDB601   -PRE MSGI-                                       
069000                                                                          
069100 01  FILLER               PIC X(16)   VALUE 'WDB601 KEY '.                
069200 01   DLI-IO-AREA-B601-KEY.                                               
069300*     03  -COPY WDB601   -PRE KEY-                                        
069400                                                                          
069500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
069600 01  DLI-IO-WDB616.                                                       
069700*    03  -COPY WDB616    -PRE B616-                                       
069800     EJECT                                                                
069900                                                                          
070000                                                                          
070100 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
070200                                                                          
070300*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
070400     EJECT                                                                
070500     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
070600     EJECT                                                                
070700                                                                          
070800                                                                          
070900 LINKAGE SECTION.                                                         
071000*01  -COPY W0009   -PRE MSG-                                              
071100*01  -COPY W0009   -PRE ALT-                                              
071200*01  -COPY W0009   -PRE KOMA-                                             
071300*01  -COPY W0008   -PRE USEA-                                             
071400     05  FILLER                  PIC X.                                   
071500     EJECT                                                                
071600*01  -COPY W0008  -PRE ARTC-                                              
071700     05  FILLER                  PIC X.                                   
071800     EJECT                                                                
071900*01  -COPY W0008  -PRE ARTS-                                              
072000     05  FILLER                  PIC X.                                   
072100     EJECT                                                                
072200*01  -COPY W0008  -PRE BENA-                                              
072300     05  FILLER                  PIC X.                                   
072400     EJECT                                                                
072500*01  -COPY W0008  -PRE ORDL-                                              
072600     05  FILLER                  PIC X.                                   
072700     EJECT                                                                
072800*01  -COPY W0008  -PRE OIGA-                                              
072900     05  FILLER                  PIC X.                                   
073000     EJECT                                                                
073100*01  -COPY W0008  -PRE 6321-                                              
073200     05  FILLER                  PIC X.                                   
073300     EJECT                                                                
073400 01  ORDN-XXKP-PCB               PIC X.                                   
073500 01  ORDN-ORQL-PCB               PIC X.                                   
073600 01  ORDN-PROC-PCB               PIC X.                                   
073700 01  ORDN-ORQI-PCB               PIC X.                                   
073800 01  SAPC-PCB                    PIC X.                                   
073900 01  WDE6-ORQI-PCB               PIC X.                                   
074000     EJECT                                                                
074100*01  -COPY W0008      -PRE WDB6-                                          
074200     05  FILLER                  PIC X.                                   
074300     EJECT                                                                
074400*01  -COPY W0008      -PRE 6327-                                          
074500     05  FILLER                  PIC X.                                   
074600     EJECT                                                                
074700*01  -COPY W0008      -PRE WDN6-                                          
074800     05  FILLER                  PIC X.                                   
074900     EJECT                                                                
075000*01  -COPY W0008      -PRE WDK9-                                          
075100     05  FILLER                  PIC X.                                   
075200     EJECT                                                                
075300*01  -COPY W0008      -PRE WDB62-                                         
075400     05  FILLER                  PIC X.                                   
075500     EJECT                                                                
075600*01  -COPY W0008      -PRE WDK7-                                          
075700     05  FILLER                  PIC X.                                   
075800     EJECT                                                                
075900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB                       
076000                           USEA-PCB ARTC-PCB ARTS-PCB                     
076100                           BENA-PCB ORDL-PCB                              
076200                           OIGA-PCB 6321-PCB                              
076300                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
076400                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
076500                           SAPC-PCB WDB6-PCB 6327-PCB                     
076600                           WDN6-PCB WDK9-PCB WDB62-PCB WDK7-PCB.          
076700 MAIN SECTION.                                                            
076800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB                       
076900                           USEA-PCB ARTC-PCB ARTS-PCB                     
077000                           BENA-PCB ORDL-PCB                              
077100                           OIGA-PCB 6321-PCB                              
077200                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
077300                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
077400                           SAPC-PCB WDB6-PCB 6327-PCB                     
077500                           WDN6-PCB WDK9-PCB WDB62-PCB WDK7-PCB.          
077600     PERFORM IMS-GET-MSG                                                  
077700     IF SEGMENT-FINNS                                                     
077800       PERFORM A-INIT                                                     
077900       PERFORM B-KOLLA-NYCKLAR                                            
078000       IF NYCKLAR-OK                                                      
078100         IF MFS-UPDATE OR MFS-UPD-V                                       
078200           PERFORM G-KOLLA-INPUT                                          
078300           IF INDATA-OK                                                   
078400             PERFORM H-UPPDATERA                                          
078500           END-IF                                                         
078600         ELSE                                                             
078700           IF MFS-FIRST                                                   
078800             PERFORM C-FOERSTA-SIDA                                       
078900           ELSE                                                           
079000             PERFORM E-SAMMA-SIDA                                         
079100           END-IF                                                         
079200         END-IF                                                           
079300         PERFORM F-LAES-VISA-INFO                                         
079400       END-IF                                                             
079500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35901-CTX + 4                  
079600       PERFORM IMS-INSERT-MSG                                             
079700     END-IF                                                               
079800                                                                          
079900     MOVE ZERO TO RETURN-CODE                                             
080000     GOBACK                                                               
080100     .                                                                    
080200     EJECT                                                                
080300 A-INIT SECTION.                                                          
080400                                                                          
080500     IF MSG-DUBBLA-TRANSKODER                                             
080600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35901-CTX             
080700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
080800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
080900     ELSE                                                                 
081000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35901-CTX              
081100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
081200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
081300     END-IF                                                               
081400                                                                          
081500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
081600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
081700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
081800                                                                          
081900     MOVE LOW-VALUE TO MSG-AREA                                           
082000     MOVE 'W2O359N1' TO MFS-IDMOD                                         
082100     MOVE '2359' TO MOD-IDTRANS                                           
082200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
082300                                                                          
082400     IF EGEN-MID OR HELP-MID                                              
082500       CONTINUE                                                           
082600     ELSE                                                                 
082700       MOVE SPACE TO MFS-KDTRTYP                                          
082800       MOVE '7' TO MFS-IDPFK                                              
082900     END-IF                                                               
083000                                                                          
083100     ACCEPT DAGENS-DATUM FROM DATE                                        
083200     ACCEPT DAGENS-TID FROM TIME                                          
083300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
083400     .                                                                    
083500     EJECT                                                                
083600 B-KOLLA-NYCKLAR SECTION.                                                 
083700                                                                          
083800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
083900     MOVE '001'             TO MSGI-KDCALL                                
084000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
084100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
084200     MOVE '2359'            TO MSGI-IDTRANS                               
084300     IF EGEN-MID                                                          
084400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
084500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
084600     ELSE                                                                 
084700       IF  MID-IDARTNR-IN NUMERIC                                         
084800       AND MID-IDARTNR-IN > ZERO                                          
084900         MOVE MID-IDARTNR-IN                                              
085000                            TO MSGI-IDARTNR                               
085100       END-IF                                                             
085200     END-IF                                                               
085300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
085400                                                                          
085500*    -- KONTROLL AV SPRÅKKOD                                              
085600     IF MSGI-IDLAND-SPR = 'SE'                                            
085700       MOVE +1    TO SPR                                                  
085800       MOVE 'S  ' TO MED-IDSKYLT                                          
085900     ELSE                                                                 
086000       MOVE +2    TO SPR                                                  
086100       MOVE 'GB ' TO MED-IDSKYLT                                          
086200     END-IF                                                               
086300                                                                          
086400     MOVE JA TO NYCKLAR-SW                                                
086500                                                                          
086600*    -- KONTROLL AV IDARTNR                                               
086700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
086800                                                                          
086900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
087000       MOVE '7'         TO MFS-IDPFK                                      
087100       MOVE SPACE       TO MFS-KDTRTYP                                    
087200     END-IF                                                               
087300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
087400     IF MSGI-IDARTNR NUMERIC                                              
087500       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
087600                            W-IDARTNR-K7                                  
087700                            W-IDARTNR-MAX                                 
087800     ELSE                                                                 
087900       MOVE NEJ TO NYCKLAR-SW                                             
088000     END-IF                                                               
088100                                                                          
088200*    -- KONTROLL AV IDDC                                                  
088300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
088400                                                                          
088500     IF MID-IDDC-IN NOT = ALL '+'                                         
088600       MOVE '7'         TO MFS-IDPFK                                      
088700       MOVE SPACE       TO MFS-KDTRTYP                                    
088800     END-IF                                                               
088900                                                                          
089000     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
089100                                                                          
089200     INSPECT W-IDDC REPLACING ALL  '+'  BY ZEROS                          
089300                              ALL SPACE BY ZEROS                          
089400                                                                          
089500     IF DCS-IDDC NOT = W-IDDC                                             
089600        MOVE W-IDDC TO W-IDDC-B6                                          
089700        PERFORM IMS-GU-WDB601                                             
089710        MOVE DCS-IDLEVNR-DC     TO W-IDLEVNR-SEND                         
089800     END-IF                                                               
089900                                                                          
090000     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC                            
090100        MOVE NEJ    TO NYCKLAR-SW                                         
090200     ELSE                                                                 
090300       MOVE W-IDDC  TO   W-IDDC-FROM                                      
090400                         WS-IDDC-SEND                                     
090500     END-IF                                                               
090600*--  USED FOR CHECKING IF DC IS IN CHINA FOR TRANSFERS                    
090700     MOVE W-IDDC-FROM          TO FROM-WS-IDDC                            
090800     IF MID-IDDC-SEND-TO    NOT = ALL '+'                                 
090900        MOVE MID-IDDC-SEND-TO  TO SEND-WS-IDDC                            
091000     END-IF                                                               
091100*--                                                                       
091200                                                                          
091300     MOVE W-IDDC          TO MOD-IDDC-UT                                  
091400*--  KONTROLL AV KDARBTYP                                                 
091500     IF MSGI-KDARBTYP-SEC(1:3) = 'ESC'                                    
091600       MOVE 'ESC' TO WS-KDARBTYP-SEC                                      
091700     ELSE                                                                 
091800       IF MSGI-KDARBTYP-SEC(1:3) = 'VOR'                                  
091900         MOVE 'VOR' TO WS-KDARBTYP-SEC                                    
092000       ELSE                                                               
092100         IF MSGI-KDARBTYP-SEC(1:3) = 'LOC'                                
092200           MOVE 'LOC' TO WS-KDARBTYP-SEC                                  
092300         ELSE                                                             
092400           MOVE 'FEL' TO WS-KDARBTYP-SEC                                  
092500         END-IF                                                           
092600       END-IF                                                             
092700     END-IF                                                               
092800     MOVE MSGI-KDARBTYP-SEC TO WS-KDARBTYP                                
092900     MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                             
093000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
093100     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
093200     IF NYCKLAR-FEL                                                       
093300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
093400       CALL WMEDKONV USING MED-WMEDAREA                                   
093500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
093600       PERFORM MFS-RENSA-FAELT-UT                                         
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 C-FOERSTA-SIDA SECTION.                                                  
094100                                                                          
094200     PERFORM MFS-RENSA-FAELT-IN                                           
094300     .                                                                    
094400     EJECT                                                                
094500 E-SAMMA-SIDA SECTION.                                                    
094600     SKIP2                                                                
094700     IF EGEN-MID OR HELP-MID                                              
094800       IF MID-W2I35901-001-GRP  = ALL '+'                                 
094900       AND MID-W2I35901-002-GRP = ALL '+'                                 
095000       AND MID-W2I35901-004-GRP = ALL '+'                                 
095010       AND MID-FLSKROT-BEORD-IN = ALL '+'                                 
095100         PERFORM MFS-RENSA-FAELT-IN                                       
095200       ELSE                                                               
095300         IF SEND-WS-IDDC IS NUMERIC                                       
095400             CONTINUE                                                     
095500         ELSE                                                             
095510            IF MID-FLSKROT-BEORD-IN = ALL '+'                             
095600              MOVE INF-PRESS-PF11    TO MED-IDMFSINF                      
095610            ELSE                                                          
095611              MOVE INF-PRESS-PF23    TO MED-IDMFSINF                      
095620            END-IF                                                        
095700         END-IF                                                           
095800         CALL WMEDKONV USING MED-WMEDAREA                                 
095900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
096000         PERFORM EA-MID-INDATA-TILL-MOD                                   
096100       END-IF                                                             
096200     ELSE                                                                 
096300       PERFORM MFS-RENSA-FAELT-IN                                         
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700 EA-MID-INDATA-TILL-MOD SECTION.                                          
096800     SKIP2                                                                
096900     IF MID-IDDC-SEND-TO NOT = ALL '+'                                    
097000         MOVE MID-IDDC-SEND-TO TO MOD-IDDC-SEND-TO                        
097100         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-SEND-TO-ATTR             
097200     ELSE                                                                 
097300         MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-TO                         
097400     END-IF                                                               
097500                                                                          
097600     IF MID-KDORDKL NOT = ALL '+' AND SPACE                               
097700         MOVE MID-KDORDKL          TO MOD-KDORDKL                         
097800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDORDKL-ATTR                  
097900     ELSE                                                                 
098000         MOVE MFS-RENSA-FAELT TO MOD-KDORDKL                              
098100     END-IF                                                               
098200                                                                          
098300     IF MID-FLTOT   NOT = ALL '+'                                         
098400         MOVE MID-FLTOT            TO MOD-FLTOT                           
098500         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLTOT-ATTR                    
098600     ELSE                                                                 
098700         MOVE MFS-RENSA-FAELT TO MOD-FLTOT                                
098800     END-IF                                                               
098900                                                                          
099000     IF MID-TRANSFER-TEXT-1 NOT = ALL '+' AND SPACE                       
099100         MOVE MID-TRANSFER-TEXT-1  TO MOD-TRANSFER-TEXT-1                 
099200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TRANSFER-TEXT-1-ATTR          
099300     ELSE                                                                 
099400         MOVE MFS-RENSA-FAELT TO MOD-TRANSFER-TEXT-1                      
099500     END-IF                                                               
099600                                                                          
099700     IF MID-TRANSFER-TEXT-2 NOT = ALL '+' AND SPACE                       
099800         MOVE MID-TRANSFER-TEXT-2  TO MOD-TRANSFER-TEXT-2                 
099900         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TRANSFER-TEXT-2-ATTR          
100000     ELSE                                                                 
100100         MOVE MFS-RENSA-FAELT TO MOD-TRANSFER-TEXT-2                      
100200     END-IF                                                               
100300                                                                          
100400     IF MID-KVSKROT-KVAR    NOT = ALL '+' AND SPACE                       
100500         MOVE MID-KVSKROT-KVAR     TO MOD-KVSKROT-KVAR                    
100600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVSKROT-KVAR-ATTR             
100700     ELSE                                                                 
100800         MOVE MFS-RENSA-FAELT TO MOD-KVSKROT-KVAR                         
100900     END-IF                                                               
101000                                                                          
101100     IF MID-IDKONTO         NOT = ALL '+' AND SPACE                       
101200         INSPECT MID-IDKONTO                                              
101300                          REPLACING LEADING SPACE BY ZERO                 
101400         MOVE MID-IDKONTO    TO WS-RED-IDKONTO                            
101500         MOVE WS-RED-IDKONTO TO MOD-IDKONTO                               
101600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKONTO-ATTR                  
101700     ELSE                                                                 
101800         MOVE MFS-RENSA-FAELT TO MOD-IDKONTO                              
101900     END-IF                                                               
102000                                                                          
102100     IF MID-IDANALYS        NOT = ALL '+' AND SPACE                       
102200         MOVE MID-IDANALYS         TO MOD-IDANALYS                        
102300         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-ATTR                 
102400     ELSE                                                                 
102500         MOVE MFS-RENSA-FAELT TO MOD-IDANALYS                             
102600     END-IF                                                               
102700                                                                          
102800     IF MID-TISKROT-AUTO-IN NOT = ALL '+' AND SPACE                       
102900         MOVE MID-TISKROT-AUTO-IN    TO MOD-TISKROT-AUTO-IN               
103000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TISKROT-AUTO-IN-ATTR          
103100     ELSE                                                                 
103200         MOVE MFS-RENSA-FAELT        TO MOD-TISKROT-AUTO-IN               
103300     END-IF                                                               
103301                                                                          
103370     IF MID-FLSKROT-BEORD-IN NOT = ALL '+'                                
103380         MOVE MID-FLSKROT-BEORD-IN   TO MOD-FLSKROT-BEORD                 
103390         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLSKROT-BEORD-ATTR            
103391     ELSE                                                                 
103392         MOVE MFS-RENSA-FAELT        TO MOD-FLSKROT-BEORD                 
103394     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600 F-LAES-VISA-INFO SECTION.                                                
103700                                                                          
103800     PERFORM FA-LAES-GRUNDDATA                                            
103900                                                                          
104000     IF SEGMENT-SAKNAS                                                    
104100        MOVE PART-MISSING TO MED-IDMFSFEL                                 
104200        MOVE 'GB '         TO MED-IDSKYLT                                 
104300        CALL WMEDKONV USING MED-WMEDAREA                                  
104400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
104500        PERFORM MFS-RENSA-FAELT-UT                                        
104600     ELSE                                                                 
104700        IF ARTC-ART-KDERS-UTG > +0                                        
104800           MOVE PART-SUPERSEDED TO MED-IDMFSFEL                           
104900           MOVE 'GB '         TO MED-IDSKYLT                              
105000           CALL WMEDKONV USING MED-WMEDAREA                               
105100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
105200           PERFORM MFS-RENSA-FAELT-UT                                     
105300        ELSE                                                              
105400           PERFORM FB-VISA                                                
105500        END-IF                                                            
105600     END-IF                                                               
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000                                                                          
106100 FA-LAES-GRUNDDATA SECTION.                                               
106200                                                                          
106300     PERFORM IMS-GU-ARTC-ARTC                                             
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700                                                                          
106800 FB-VISA SECTION.                                                         
106900                                                                          
107000     MOVE W-IDDC-FROM TO W-IDDC                                           
107100                                                                          
107200     PERFORM IMS-GET-ARTC-CLAG-INFO                                       
107300     IF SEGMENT-FINNS                                                     
107400       MOVE ARTC-CLAG-KDERS    TO MOD-KDERS                               
107500       MOVE ARTC-CLAG-KVQPACK-1                                           
107600                               TO MOD-KVQPACK-1                           
107700     ELSE                                                                 
107800       MOVE MFS-RENSA-FAELT    TO MOD-KDERS                               
107900                                  MOD-KVQPACK-1                           
108000     END-IF                                                               
108100     PERFORM IMS-GET-ARTS-WLARTS11                                        
108200     IF SEGMENT-SAKNAS                                                    
108300        MOVE FEL-1(SPR)        TO MED-IDMFSFEL                            
108400        MOVE NEJ TO INDATA-SW                                             
108500        PERFORM MFS-RENSA-FAELT-UT                                        
108600     ELSE                                                                 
108700        MOVE SLAG-IDDC    TO WS-IDDC                                      
108800        IF SLAG-IDDC-REF = SPACE AND (NDC-CN OR NDC-US)                   
108900          MOVE MED-15(SPR)      TO MOD-TEMFSINF                           
109000          MOVE NEJ TO INDATA-SW                                           
109100          PERFORM MFS-RENSA-FAELT-UT                                      
109200        ELSE                                                              
109300          IF MFS-UPDATE OR MFS-UPD-V                                      
109400            CONTINUE                                                      
109500          ELSE                                                            
109600            IF MFS-FIRST OR MFS-ENTER                                     
109700              PERFORM MFS-RENSA-FAELT-UT                                  
109800              MOVE NEJ TO MOD-FLTOT                                       
109900                          MOD-KDORDKL                                     
110000            END-IF                                                        
110100          END-IF                                                          
110200          MOVE 'GB ' TO W-IDSKYLT                                         
110300          PERFORM IMS-GU-BENA-TEXT                                        
110400                                                                          
110500          IF SEGMENT-FINNS                                                
110600              MOVE TEXT-BEART TO MOD-BEART                                
110700          ELSE                                                            
110800              MOVE SPACE TO MOD-BEART                                     
110900          END-IF                                                          
111000                                                                          
111100          MOVE SLAG-TIRETUR-BEORD TO MOD-TIRETUR-BEORD                    
111200          MOVE SLAG-KVRETUR-BEORD TO MOD-KVRETUR-BEORD                    
111300          COMPUTE W-KVSTOCK       = SLAG-KVLS - 0                         
111400          END-COMPUTE                                                     
111500          MOVE W-KVSTOCK       TO MOD-KVSTOCK                             
111600          MOVE SLAG-KVAKS-PAV  TO MOD-KVAKS-PAV                           
111700          MOVE SLAG-KVAKS-SDC  TO MOD-KVAKS-SDC                           
111800          MOVE SLAG-KVBEART    TO MOD-KVBEART                             
111900          MOVE SLAG-KVSKROT    TO MOD-KVSKROT                             
112000          MOVE SLAG-TISKROT    TO MOD-TISKROT                             
112100          MOVE SLAG-TISKROT-BEORD TO MOD-TISKROT-BEORD                    
112200          MOVE SLAG-FLSKROT-AUTO TO MOD-FLSKROT-AUTO                      
112210          MOVE SLAG-TISKROT-AUTO TO MOD-TISKROT-AUTO-UT                   
112211                                                                          
112215          IF INDATA-OK                                                    
112219            MOVE SLAG-FLSKROT-BEORD TO MOD-FLSKROT-BEORD                  
112225          END-IF                                                          
112240                                                                          
112241          IF MFS-UPDATE OR MFS-UPD-V                                      
112242             CONTINUE                                                     
112250          ELSE                                                            
112260             IF SLAG-FLORDSP = JA                                         
112270                MOVE MED-8(SPR)TO MOD-TEMFSINF                            
112280             ELSE                                                         
112290                IF SLAG-KVSPARR-KVAL > ZERO                               
112291                  MOVE SLAG-KVSPARR-KVAL TO WS-KVSPARR-KVAL               
112292                  STRING MED-6(SPR) WS-KVSPARR-KVAL                       
112293                  DELIMITED BY SIZE INTO MOD-TEMFSINF                     
112294                ELSE                                                      
112295                  IF SLAG-KDLEVSP > ZERO                                  
112296                    MOVE MED-7(SPR) TO MOD-TEMFSINF                       
112297                  END-IF                                                  
112298                END-IF                                                    
112299             END-IF                                                       
112300          END-IF                                                          
114200                                                                          
114300          IF MFS-UPDATE AND  SKROT-IFYLLT                                 
114400            COMPUTE W-ANTAL-SKROT = SLAG-KVLS - W-KVSKROT-KVAR-NUM        
114500                      END-COMPUTE                                         
114600              IF DCS-NDC-NA                                               
114700              OR DCS-NDC-CN                                               
114900                 COMPUTE W-SCRAPPED-VALUE =                               
115000                         W-ANTAL-SKROT * SLAG-PRAVCOST                    
115100              ELSE                                                        
115200                 COMPUTE W-SCRAPPED-VALUE =                               
115300                         W-ANTAL-SKROT * ARTC-CLAG-PRARTSTD               
115400                 END-COMPUTE                                              
115500              END-IF                                                      
115600              MOVE W-SCRAPPED-VALUE TO MOD-VALUE-OF-SCRAP-QTY             
115700          END-IF                                                          
115800        END-IF                                                            
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200                                                                          
116300                                                                          
116400 G-KOLLA-INPUT SECTION.                                                   
116500     SKIP2                                                                
116600     MOVE JA  TO INDATA-SW                                                
116700     IF MID-W2I35901-001-GRP  = ALL '+'                                   
116800     AND MID-W2I35901-002-GRP = ALL '+'                                   
116900     AND MID-W2I35901-004-GRP = ALL '+'                                   
116910     AND MID-FLSKROT-BEORD-IN = ALL '+'                                   
117000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
117100       CALL WMEDKONV USING MED-WMEDAREA                                   
117200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
117300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
117400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
117500       MOVE NEJ TO INDATA-SW                                              
117600     ELSE                                                                 
117700       IF MFS-UPDATE OR MFS-UPD-V                                         
117701          IF MFS-UPDATE                                                   
117710         AND MID-FLSKROT-BEORD-IN NOT = ALL '+'                           
117730             MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                  
117731             CALL WMEDKONV      USING MED-WMEDAREA                        
117732             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
117740             PERFORM MFS-ROER-EJ-FAELT-IN                                 
117750             PERFORM MFS-RENSA-FAELT-UT                                   
117760             MOVE NEJ              TO INDATA-SW                           
117800          END-IF                                                          
117900       ELSE                                                               
117910          IF MID-FLSKROT-BEORD-IN = ALL '+'                               
117911           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
117920          ELSE                                                            
117921           MOVE INF-PRESS-PF23 TO MED-IDMFSINF                            
117930          END-IF                                                          
118100          CALL WMEDKONV      USING MED-WMEDAREA                           
118200          MOVE MED-MFSINF       TO MOD-TEMFSINF                           
118300          PERFORM MFS-ROER-EJ-FAELT-IN                                    
118400          PERFORM MFS-RENSA-FAELT-UT                                      
118500          MOVE NEJ              TO INDATA-SW                              
118600       END-IF                                                             
118700     END-IF                                                               
118800                                                                          
118900     IF INDATA-OK                                                         
118910       IF MFS-UPD-V                                                       
118912         PERFORM GD-KOLLA-SKROT-BEORD                                     
118920       ELSE                                                               
119000         IF MID-W2I35901-001-GRP NOT = ALL '+'                            
119100             MOVE JA TO TRANSFER-IFYLLT-SW                                
119200         END-IF                                                           
119300         IF MID-W2I35901-002-GRP NOT = ALL '+'                            
119400             MOVE JA TO SKROT-IFYLLT-SW                                   
119500         END-IF                                                           
119600         IF MID-W2I35901-004-GRP NOT = ALL '+'                            
119700             MOVE JA TO SKROT-AUTO-SW                                     
119701         END-IF                                                           
119900         IF (TRANSFER-IFYLLT AND  SKROT-IFYLLT) OR                        
120000            (TRANSFER-IFYLLT AND  SKROT-AUTO)   OR                        
120100            (SKROT-IFYLLT    AND  SKROT-AUTO)                             
120200             MOVE NEJ TO INDATA-SW                                        
120300         ELSE                                                             
120400           IF TRANSFER-IFYLLT                                             
120500               PERFORM GA-KOLLA-TRANSFER                                  
120600           ELSE                                                           
120700               IF SKROT-IFYLLT                                            
120800                   PERFORM GB-KOLLA-SKROT                                 
120900               ELSE                                                       
121000                 IF SKROT-AUTO                                            
121100                     PERFORM GC-KOLLA-SKROT-AUTO                          
121204                 END-IF                                                   
121300               END-IF                                                     
121400           END-IF                                                         
121500         END-IF                                                           
121510       END-IF                                                             
121520                                                                          
121700       IF INDATA-FEL                                                      
121710         IF MFS-UPDATE                                                    
121800           IF ORDERSPARR-ARTIKEL                                          
121900             MOVE ERR-ORDER-NOT-AVAILABLE TO MED-IDMFSFEL                 
122000           ELSE                                                           
122100             IF NOSPLITBULK                                               
122200               MOVE ERR-NO-SPLIT-BULK-PACK TO MED-IDMFSFEL                
122300             ELSE                                                         
122400                IF DIREKTLEV-ARTIKEL                                      
122500                   MOVE DIRECTLY-DELIVERED-PART                           
122600                               TO MED-IDMFSFEL                            
122700                ELSE                                                      
122800                   IF ERR-TISKROT                                         
122900                     MOVE ERR-INVALID-DATE TO MED-IDMFSFEL                
123000                   ELSE                                                   
123100                     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL            
123200                   END-IF                                                 
123300                END-IF                                                    
123400             END-IF                                                       
123500           END-IF                                                         
123510         END-IF                                                           
123600         CALL WMEDKONV USING MED-WMEDAREA                                 
123700         IF NO-TRANS                                                      
123800           MOVE 'THIS IS A REFILLCOMBINATION' TO MOD-TEMFSFEL             
123900         ELSE                                                             
124000           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
124100         END-IF                                                           
124200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
124300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
124400       END-IF                                                             
124500     END-IF                                                               
124600     .                                                                    
124700     EJECT                                                                
124800 GA-KOLLA-TRANSFER SECTION.                                               
124900     SKIP2                                                                
125000     PERFORM GAA-KOLLA-TRANSFER-INDATA                                    
125100     PERFORM S01-KONTROLLERA-KDARBTYP                                     
125200     IF INDATA-OK                                                         
125300       PERFORM GAB-FORMELLA-KONTROLLER                                    
125400     END-IF                                                               
125500     .                                                                    
125600     EJECT                                                                
125700                                                                          
125800 GAA-KOLLA-TRANSFER-INDATA SECTION.                                       
125900     SKIP2                                                                
126000      IF MID-IDDC-SEND-TO NOT = ALL '+'                                   
126100         MOVE MID-IDDC-SEND-TO TO WS-IDDC-REC                             
126200         IF TO-DCS-IDDC NOT = MID-IDDC-SEND-TO                            
126300            MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                         
126400            PERFORM IMS-GU-WDB601-TO                                      
126500         END-IF                                                           
126600                                                                          
126700         MOVE W-IDDC-FROM     TO W-IDDC                                   
126800         PERFORM IMS-GET-ARTS-WLARTS11                                    
126900         MOVE NEJ TO RETUR-IAF-SW                                         
127000         IF SEGMENT-FINNS                                                 
127100           IF SLAG-IDDC-REF = WS-IDDC-REC                                 
127200             IF ((FROM-NDC-CN AND SEND-NDC-US)                            
127300             OR ( FROM-NDC-US AND SEND-NDC-CN))                           
127400               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-SEND-TO-ATTR            
127500               MOVE NEJ TO INDATA-SW                                      
127600             ELSE                                                         
127700               MOVE SLAG-IDDC       TO W-IDDC-B601                        
127800               MOVE SLAG-IDDC-REF   TO W-IDDC-B616                        
127900               PERFORM IMS-GU-WDB616                                      
128000               IF SEGMENT-FINNS                                           
128100                  MOVE JA TO RETUR-IAF-SW                                 
128200               ELSE                                                       
128300                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-SEND-TO-ATTR         
128400                  MOVE NEJ TO INDATA-SW                                   
128500               END-IF                                                     
128600             END-IF                                                       
128700           END-IF                                                         
128800         END-IF                                                           
128900         PERFORM DB2-SELECT-TP4TRAN                                       
128910** to-dcs-cdc is removed due to as of now it should                       
128920** not be able to rerefill to cdc when refilling dc is other              
128930** then CDC. 2024-08-21 JN                                                
129000*        IF TO-DCS-CDC OR RADER-FINNS OR RETUR-IAF                        
129010         IF RADER-FINNS OR RETUR-IAF                                      
129100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC-SEND-TO-ATTR              
129200         ELSE                                                             
129300           MOVE NEJ TO INDATA-SW                                          
129400         END-IF                                                           
129500      ELSE                                                                
129600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-SEND-TO-ATTR                  
129700         MOVE NEJ TO INDATA-SW                                            
129800      END-IF                                                              
129900                                                                          
130000      INSPECT MID-KVRETUR-TRANSFER REPLACING ALL SPACE BY  '+'            
130100                                                                          
130200      IF MID-KVRETUR-TRANSFER = ALL '+'                                   
130300        IF MID-FLTOT = '+' OR SPACE                                       
130400           MOVE MFS-NUM-FAELT-FEL TO MOD-KVRETUR-TRANSFER-ATTR            
130500           MOVE NEJ TO INDATA-SW                                          
130600        END-IF                                                            
130700      ELSE                                                                
130800        IF MID-KVRETUR-TRANSFER NUMERIC                                   
130900            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVRETUR-TRANSFER-ATTR         
131000        ELSE                                                              
131100            MOVE MFS-NUM-FAELT-FEL TO MOD-KVRETUR-TRANSFER-ATTR           
131200            MOVE NEJ TO INDATA-SW                                         
131300        END-IF                                                            
131400      END-IF                                                              
131500                                                                          
131600*     --- MAN FÅR BARA NOLLA EN TRANSFER SOM GÅR MED NORMALFART           
131700*     --- MAN FÅR INTE NOLLA EN RETUR TILL DC11 (DISPATCHER)              
131800      IF TO-DCS-IDDC NOT = MID-IDDC-SEND-TO                               
131900         MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                            
132000         PERFORM IMS-GU-WDB601-TO                                         
132100      END-IF                                                              
132200      IF ( ( MID-KDORDKL = YES OR JA )                                    
132300            AND MID-KVRETUR-TRANSFER = ZERO )                             
132400      OR                                                                  
132500         ( TO-DCS-CDC                                                     
132600            AND MID-KVRETUR-TRANSFER = ZERO )                             
132700      OR                                                                  
132800         ( RETUR-IAF                                                      
132900            AND MID-KVRETUR-TRANSFER = ZERO )                             
133000        MOVE MFS-NUM-FAELT-FEL TO MOD-KVRETUR-TRANSFER-ATTR               
133100        MOVE NEJ TO INDATA-SW                                             
133200      END-IF                                                              
133300                                                                          
133400*     --- FAST ORDER YES OR NO                                            
133500      IF MID-KDORDKL  = ALL '+'                                           
133600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDORDKL-ATTR                    
133700      ELSE                                                                
133800         IF MID-KDORDKL = YES OR JA  OR NEJ                               
133900             MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDORDKL-ATTR               
134000           IF MID-KDORDKL = YES OR JA                                     
134100             IF MID-FLTOT = YES OR JA                                     
134200               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDORDKL-ATTR                
134300               MOVE NEJ TO INDATA-SW                                      
134400               MOVE MED-14 (SPR)      TO MOD-TEMFSINF                     
134500             END-IF                                                       
134600           END-IF                                                         
134700         ELSE                                                             
134800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDORDKL-ATTR                  
134900             MOVE NEJ TO INDATA-SW                                        
135000             MOVE MED-2 (SPR)      TO MOD-TEMFSINF                        
135100         END-IF                                                           
135200      END-IF                                                              
135300                                                                          
135400                                                                          
135500      IF MID-KDFRAKT  = ALL '+'                                           
135600      OR MID-KDFRAKT  = SPACE                                             
135700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFRAKT-ATTR                    
135800      ELSE                                                                
135900         IF MID-IDDC-SEND-TO NOT = TO-DCS-IDDC                            
136000            MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                         
136100            PERFORM IMS-GU-WDB601-TO                                      
136200         END-IF                                                           
136300         IF  MID-KDFRAKT = '12'                                           
136400         AND (TO-DCS-NDC-NA OR TO-DCS-NDC-CN)                             
136600             MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDFRAKT-ATTR               
136700         ELSE                                                             
136800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFRAKT-ATTR                  
136900             MOVE NEJ TO INDATA-SW                                        
137000             MOVE MED-3 (SPR)       TO MOD-TEMFSINF                       
137100         END-IF                                                           
137200      END-IF                                                              
137300                                                                          
137400                                                                          
137500      IF MID-FLTOT    = ALL '+'                                           
137600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDORDKL-ATTR                    
137700      ELSE                                                                
137800         IF MID-FLTOT = JA  OR YES OR NEJ                                 
137900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTOT-ATTR                  
138000         ELSE                                                             
138100             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTOT-ATTR                    
138200             MOVE NEJ TO INDATA-SW                                        
138300         END-IF                                                           
138400      END-IF                                                              
138500                                                                          
138600                                                                          
138700                                                                          
138800      IF MID-TRANSFER-TEXT-1 = ALL '+'                                    
138900         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TRANSFER-TEXT-1-ATTR           
139000         MOVE MFS-RENSA-FAELT TO MOD-TRANSFER-TEXT-1                      
139100      ELSE                                                                
139200         INSPECT MID-TRANSFER-TEXT-1                                      
139300             REPLACING ALL '+' BY SPACE                                   
139400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TRANSFER-TEXT-1-ATTR            
139500      END-IF                                                              
139600                                                                          
139700      IF MID-TRANSFER-TEXT-2 = ALL '+'                                    
139800         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TRANSFER-TEXT-2-ATTR           
139900         MOVE MFS-RENSA-FAELT TO MOD-TRANSFER-TEXT-2                      
140000      ELSE                                                                
140100         INSPECT MID-TRANSFER-TEXT-2                                      
140200             REPLACING ALL '+' BY SPACE                                   
140300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TRANSFER-TEXT-2-ATTR            
140400      END-IF                                                              
140500                                                                          
140600     .                                                                    
140700     EJECT                                                                
140800 GAB-FORMELLA-KONTROLLER SECTION.                                         
140900     SKIP2                                                                
141000     PERFORM GABC-KOLLA-ORDER-FRAKT                                       
141100     IF INDATA-OK                                                         
141200       PERFORM GABB-KOLLA-ARTNR-FINNS                                     
141300     END-IF                                                               
141400     .                                                                    
141500     EJECT                                                                
141600 GABB-KOLLA-ARTNR-FINNS SECTION.                                          
141700     SKIP2                                                                
141800     MOVE W-IDDC-FROM     TO W-IDDC                                       
141900     IF MID-IDDC-SEND-TO NOT = TO-DCS-IDDC                                
142000        MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                             
142100        PERFORM IMS-GU-WDB601-TO                                          
142200     END-IF                                                               
142300     PERFORM IMS-GET-ARTS-WLARTS11                                        
142400     IF SEGMENT-SAKNAS                                                    
142500        MOVE NEJ TO INDATA-SW                                             
142600        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                    
142700     ELSE                                                                 
142800        IF TO-DCS-CDC OR RETUR-IAF                                        
142900*          RETUR                                                          
143000            IF SLAG-KDLEVSP > 0                                           
143100              MOVE NEJ TO INDATA-SW                                       
143200              MOVE JA TO ORDERSPARRAD-SW                                  
143300            END-IF                                                        
143400        ELSE                                                              
143500*          TRANSFER                                                       
143600           IF SLAG-KDLEVSP > 0                                            
143700             MOVE NEJ TO INDATA-SW                                        
143800             MOVE JA TO ORDERSPARRAD-SW                                   
143900           END-IF                                                         
144000        END-IF                                                            
144100     END-IF                                                               
144200                                                                          
144300     IF INDATA-OK                                                         
144400*      --- BEHANDLA TRANSFER-KVANTITET                                    
144500       IF MID-KVRETUR-TRANSFER = ALL '+'                                  
144600         IF MID-FLTOT = JA OR YES                                         
144700*          --- FLYTTA ALLT !                                              
144800           MOVE SLAG-KVLS TO W-KVRETUR-TRANSFER                           
144900         ELSE                                                             
145000           MOVE ZERO      TO W-KVRETUR-TRANSFER                           
145100         END-IF                                                           
145200       ELSE                                                               
145300         MOVE MID-KVRETUR-TRANSFER TO W-KVRETUR-TRANSFER                  
145400         IF SLAG-KVSPARR-KVAL > ZERO                                      
145500           COMPUTE WS-OSPARRAT-ANTAL =                                    
145600                   SLAG-KVLS - SLAG-KVSPARR-KVAL                          
145700           IF W-KVRETUR-TRANSFER > WS-OSPARRAT-ANTAL                      
145800              MOVE NEJ TO INDATA-SW                                       
145900              MOVE MFS-ALFA-FAELT-FEL TO                                  
146000                                  MOD-KVRETUR-TRANSFER-ATTR               
146100           END-IF                                                         
146200         ELSE                                                             
146300           IF W-KVRETUR-TRANSFER > SLAG-KVLS                              
146400             MOVE NEJ TO INDATA-SW                                        
146500             MOVE MFS-ALFA-FAELT-FEL TO                                   
146600                               MOD-KVRETUR-TRANSFER-ATTR                  
146700           END-IF                                                         
146800         END-IF                                                           
146900       END-IF                                                             
147000                                                                          
147100       IF INDATA-OK                                                       
147200         PERFORM IMS-GU-ARTC-ARTC                                         
147300*        --- KOLLA OM BESTÄLLD KVANT BERÖRS AV KVQPACK                    
147400         PERFORM IMS-GNP-CLAG                                             
147500         IF ARTC-CLAG-KVQPACK-1 > 0                                       
147600           DIVIDE W-KVRETUR-TRANSFER BY ARTC-CLAG-KVQPACK-1               
147700             GIVING WS-KVQPACK-ANT ROUNDED                                
147800             REMAINDER WS-KVQPACK-REST                                    
147900           END-DIVIDE                                                     
148000                                                                          
148100           IF WS-KVQPACK-REST NOT = ZERO                                  
148200             IF MID-FLTOT NOT = JA AND YES                                
148300                MOVE NEJ TO INDATA-SW                                     
148400                MOVE JA TO NOSPLITBULK-SW                                 
148500                IF MID-FLTOT = YES OR JA                                  
148600                  MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTOT-ATTR               
148700                                       MOD-KVRETUR-TRANSFER-ATTR          
148800                ELSE                                                      
148900                  MOVE MFS-ALFA-FAELT-FEL                                 
149000                                    TO MOD-KVRETUR-TRANSFER-ATTR          
149100                END-IF                                                    
149200             END-IF                                                       
149300           END-IF                                                         
149400         END-IF                                                           
149500***DIREKTLEVERANS GÄLLER BARA FÖR CDC EJ FÖR KINA RETUR                   
149600         IF TO-DCS-CDC                                                    
149700         AND ARTC-CLAG-REDIRLEV = 1.00                                    
149800           MOVE JA           TO DIREKTLEV-SW                              
149900           MOVE NEJ          TO INDATA-SW                                 
150000         END-IF                                                           
150100       END-IF                                                             
150200     END-IF                                                               
150300*    --- KOLLA SÅ ATT MOTTAGANDE LAGER HAR ARTIKELN                       
150400     IF INDATA-OK                                                         
150500       IF TO-DCS-CDC OR RETUR-IAF                                         
150600*      --- RETUR                                                          
150700*      --- LOKAL USA ARTIKEL FÅR INTE RETURNERAS                          
150800         MOVE ARTC-ART-KDPRODSL  TO TEST-KDPRODSL                         
150900         IF KDPRODSL-LOCAL                                                
151000         AND TO-DCS-CDC                                                   
151100           MOVE NEJ TO INDATA-SW                                          
151200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                 
151300                                      MOD-IDDC-SEND-TO-ATTR               
151400           MOVE MED-10 (SPR)        TO MOD-TEMFSINF                       
151500         END-IF                                                           
151600       ELSE                                                               
151700*----      TRANSFER                                                       
151800         MOVE MID-IDDC-SEND-TO  TO W-IDDC                                 
151900         PERFORM IMS-GET-ARTS-WLARTS11                                    
152000         IF SEGMENT-FINNS                                                 
152100*          --- OK, SPARA LAGERADRESSEN FÖR ORDERRADENS BERADREF           
152200           IF SLAG-IDDC-REF = W-IDDC-FROM                                 
152300             MOVE NEJ TO INDATA-SW                                        
152400             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UT-ATTR                  
152500                                        MOD-IDDC-SEND-TO-ATTR             
152600             MOVE JA TO NO-TRANS-SW                                       
152700           END-IF                                                         
152800           MOVE SLAG-ADLAGOMR TO WS-ADLAGOMR-N                            
152900           MOVE SLAG-ADGANG   TO WS-ADGANG-N                              
153000           MOVE SLAG-ADPLATS  TO WS-ADPLATS-N                             
153100                                                                          
153200         ELSE                                                             
153300           MOVE NEJ TO INDATA-SW                                          
153400           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDARTNR-UT-ATTR                
153500                                       MOD-IDDC-SEND-TO-ATTR              
153600         END-IF                                                           
153700       END-IF                                                             
153800     END-IF                                                               
153900     .                                                                    
154000     EJECT                                                                
154100 GABC-KOLLA-ORDER-FRAKT SECTION.                                          
154200     SKIP2                                                                
154300*    OM MAN VILL HA EN SNABB-ORDER  SKA MAN HA MÖJLIGHET ATT              
154400*    ANGE EN FRAKTKOD, SOM FÖR TILLFÄLLET ENDAST KAN VARA                 
154500*    12. ANGER MAN INGEN FRAKTKOD LÄGGS DET BLANKT I ORDER-               
154600*    HUVUDET OCH INFO HÄMTAS DÅ I STÄLLET FRÅN KUNDREGISTRET              
154700*                                                                         
154800     IF MID-KDORDKL = YES OR JA                                           
154900       IF MID-KDFRAKT = ALL '+' OR '12' OR SPACE                          
155000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFRAKT-ATTR                  
155100       ELSE                                                               
155200           MOVE NEJ TO INDATA-SW                                          
155300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFRAKT-ATTR                    
155400       END-IF                                                             
155500     ELSE                                                                 
155600       IF MID-KDFRAKT = ALL '+' OR SPACE                                  
155700         CONTINUE                                                         
155800       ELSE                                                               
155900         MOVE NEJ TO INDATA-SW                                            
156000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFRAKT-ATTR                      
156100       END-IF                                                             
156200     END-IF                                                               
156300                                                                          
156400     .                                                                    
156500     EJECT                                                                
156600 GB-KOLLA-SKROT SECTION.                                                  
156700     SKIP2                                                                
156800     PERFORM GBA-KOLLA-SKROT-INDATA                                       
156900     PERFORM S01-KONTROLLERA-KDARBTYP                                     
157000     IF INDATA-OK                                                         
157100       PERFORM GBB-FORMELLA-KONTROLLER                                    
157200     END-IF                                                               
157300     IF INDATA-OK                                                         
157400        PERFORM GBC-KOLLA-IDUSER                                          
157500     END-IF                                                               
157600     .                                                                    
157700     EJECT                                                                
157800                                                                          
157900 GBA-KOLLA-SKROT-INDATA SECTION.                                          
158000     SKIP2                                                                
158100      IF MID-KVSKROT-KVAR   = ALL '+'                                     
158200         MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-KVAR-ATTR                  
158300         MOVE NEJ TO INDATA-SW                                            
158400      ELSE                                                                
158500         IF MID-KVSKROT-KVAR     NUMERIC                                  
158600         AND MID-KVSKROT-KVAR    > +0                                     
158700             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKROT-KVAR-ATTR            
158800         ELSE                                                             
158900             MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-KVAR-ATTR              
159000             MOVE NEJ TO INDATA-SW                                        
159100         END-IF                                                           
159200      END-IF                                                              
159300                                                                          
159400      IF MID-SKROT-TEXT      = ALL '+'                                    
159500         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-SKROT-TEXT-ATTR                
159600         MOVE MFS-RENSA-FAELT TO MOD-SKROT-TEXT                           
159700      ELSE                                                                
159800         INSPECT MID-SKROT-TEXT                                           
159900             REPLACING ALL '+' BY SPACE                                   
160000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SKROT-TEXT-ATTR                 
160100      END-IF                                                              
160200                                                                          
160300      IF MID-IDKONTO        = ALL '+'                                     
160400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-ATTR                     
160500      ELSE                                                                
160600         IF MID-IDKONTO          NUMERIC                                  
160700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-ATTR                 
160800         ELSE                                                             
160900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                   
161000             MOVE NEJ TO INDATA-SW                                        
161100         END-IF                                                           
161200      END-IF                                                              
161300                                                                          
161400      IF MID-IDANALYS        = ALL '+'                                    
161500         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDANALYS-ATTR                  
161600         MOVE MFS-RENSA-FAELT TO MOD-IDANALYS                             
161700      ELSE                                                                
161800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDANALYS-ATTR                   
161900      END-IF                                                              
162000                                                                          
162100     IF (MID-IDKONTO      NOT = ALL '+' AND                               
162200         MID-IDANALYS     NOT = ALL '+')                                  
162300         MOVE MSGI-IDFTG TO WS-IDFTG                                      
162400*        IF IDFTG-PV                                                      
162500*          MOVE 'SEPV'           TO SAP-KDTRADP                           
162600*        ELSE                                                             
162700*           IF IDFTG-CN                                                   
162800*              MOVE 'CN05'       TO SAP-KDTRADP                           
162900*           ELSE                                                          
163000*             IF IDFTG-IN                                                 
163100*                MOVE 'IN07'     TO SAP-KDTRADP                           
163200*             ELSE                                                        
163300*                MOVE 'SEPV'     TO SAP-KDTRADP                           
163400*             END-IF                                                      
163500*           END-IF                                                        
163501*        END-IF                                                           
163502         IF IDFTG-NON-VCC                                                 
163510           MOVE MSGI-DCS-KDTRADP TO SAP-KDTRADP                           
163520         ELSE                                                             
163521           MOVE 'SEPV'           TO SAP-KDTRADP                           
163530         END-IF                                                           
163700         MOVE SPACE              TO SAP-IDKST                             
163800         INSPECT MID-IDKONTO                                              
163900                          REPLACING LEADING SPACE BY ZERO                 
164000         MOVE MID-IDKONTO        TO SAP-IDKONTO                           
164100         MOVE MID-IDANALYS       TO SAP-IDANALYS                          
164200         MOVE ZERO               TO SAP-IDDISTR                           
164300         MOVE SPACE              TO SAP-KDFAKTYP                          
164400         MOVE ZERO               TO SAP-IDFTG                             
164500         MOVE SPACE              TO SAP-IDPROFIT                          
164600         MOVE +2                 TO SAP-KDCALL                            
164700         CALL W411SAP USING SAP-W411SAP SAPC-PCB                          
164800                                                                          
164900         IF SAP-BEFEL NOT = SPACE                                         
165000           MOVE SAP-BEFEL TO MOD-TEMFSFEL                                 
165100           MOVE NEJ TO INDATA-SW                                          
165200           IF SAP-IDKONTO-OK = NEJ                                        
165300             MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKONTO                       
165400             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKONTO-ATTR                  
165500           ELSE                                                           
165600             IF SAP-IDANALYS-OK = NEJ                                     
165700               MOVE MFS-ROER-EJ-FAELT                                     
165800                                     TO MOD-IDKONTO                       
165900               MOVE MFS-ALFA-FAELT-FEL                                    
166000                                     TO MOD-IDANALYS-ATTR                 
166100             END-IF                                                       
166200           END-IF                                                         
166300         END-IF                                                           
166400     ELSE                                                                 
166500        IF (MID-IDKONTO       = ALL '+' AND                               
166600            MID-IDANALYS      = ALL '+')                                  
166700            CONTINUE                                                      
166800        ELSE                                                              
166900             MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-ATTR                  
167000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANALYS-ATTR                 
167100             MOVE NEJ TO INDATA-SW                                        
167200        END-IF                                                            
167300     END-IF                                                               
167400     EJECT                                                                
167500     .                                                                    
167600 GBB-FORMELLA-KONTROLLER SECTION.                                         
167700     SKIP2                                                                
167800     MOVE MSGI-IDARTNR TO W-IDARTNR                                       
167900     MOVE W-IDDC-FROM  TO W-IDDC                                          
168000     PERFORM IMS-GET-ARTS-WLARTS11                                        
168100     IF SEGMENT-SAKNAS                                                    
168200       MOVE NEJ TO INDATA-SW                                              
168300       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                     
168400     ELSE                                                                 
168500       PERFORM GBBA-KOLLA-SKROT-SALDO                                     
168600     END-IF                                                               
168700     .                                                                    
168800     EJECT                                                                
168900 GBBA-KOLLA-SKROT-SALDO SECTION.                                          
169000     SKIP2                                                                
169100     IF SLAG-FLSKROT-BEORD = JA  OR YES                                   
169200       MOVE NEJ TO INDATA-SW                                              
169300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISKROT-ATTR                     
169400                                     MOD-KVSKROT-ATTR                     
169500       MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVSKROT-KVAR-ATTR                
169600     END-IF                                                               
169700                                                                          
169800     MOVE MID-KVSKROT-KVAR TO W-KVSKROT-KVAR-NUM                          
169900     IF SLAG-KDLEVSP > ZERO                                               
170000        MOVE NEJ TO INDATA-SW                                             
170100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-KVAR-ATTR                  
170200     ELSE                                                                 
170300        IF SLAG-KVSPARR-KVAL > ZERO                                       
170400           COMPUTE W-KVSKROT = SLAG-KVLS - W-KVSKROT-KVAR-NUM             
170500           COMPUTE WS-OSPARRAT-ANTAL =                                    
170600                   SLAG-KVLS - SLAG-KVSPARR-KVAL                          
170700           IF W-KVSKROT > WS-OSPARRAT-ANTAL                               
170800             MOVE NEJ TO INDATA-SW                                        
170900             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-KVAR-ATTR             
171000           END-IF                                                         
171100        ELSE                                                              
171200          IF W-KVSKROT-KVAR-NUM  >= SLAG-KVLS                             
171300              MOVE NEJ TO INDATA-SW                                       
171400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-KVAR-ATTR            
171500          END-IF                                                          
171600        END-IF                                                            
171700        COMPUTE W-KVSKROT = SLAG-KVLS - W-KVSKROT-KVAR-NUM                
171800     END-IF                                                               
171900                                                                          
172000     .                                                                    
172100     EJECT                                                                
172200 GBC-KOLLA-IDUSER SECTION.                                                
172300                                                                          
172400     MOVE W-IDDC-FROM      TO W-IDDC-6327                                 
172500     MOVE WS-KDARBTYP(1:3) TO W-KDARBTYP-6327                             
172600     MOVE SPACE TO WS-SPAR-BEANST-GODK                                    
172700                                                                          
172800     PERFORM IMS-GET-WDR501-6327                                          
172900     IF SEGMENT-FINNS                                                     
173000        MOVE MSG-SIGNON-USERID TO W-IDUSER-GODK                           
173100        PERFORM IMS-GET-WDGX6328                                          
173200        IF SEGMENT-FINNS                                                  
173300           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
173400        ELSE                                                              
173500           MOVE NEJ TO INDATA-SW                                          
173600           MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                   
173700           CALL WMEDKONV USING MED-WMEDAREA                               
173800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
173900        END-IF                                                            
174000     ELSE                                                                 
174100        MOVE NEJ TO INDATA-SW                                             
174200        MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                      
174300        CALL WMEDKONV USING MED-WMEDAREA                                  
174400        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
174500     END-IF                                                               
174600     .                                                                    
174700     EJECT                                                                
174800 GC-KOLLA-SKROT-AUTO SECTION.                                             
174900     SKIP2                                                                
175010     IF MID-TISKROT-AUTO-IN  NUMERIC                                      
175100       MOVE MFS-NUM-FIELD-OK    TO MOD-TISKROT-AUTO-IN-ATTR               
175200     ELSE                                                                 
175300       MOVE MID-TISKROT-AUTO-IN TO MOD-TISKROT-AUTO-IN                    
175400       MOVE MFS-NUM-FIELD-WRONG TO MOD-TISKROT-AUTO-IN-ATTR               
175500       MOVE NEJ TO INDATA-SW                                              
175600     END-IF                                                               
175780                                                                          
175800**   VALIDATION FOR INPUT EJ SKRAP AUTO DATE                              
175900     IF INDATA-OK                                                         
176000       MOVE 'AAMMDD'            TO DAT-KDDATFORM                          
176100       MOVE MID-TISKROT-AUTO-IN TO DAT-I-TIDATUM                          
176200                                                                          
176300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
176400                           DAT-O-TIDATUM DAT-KDSVAR                       
176500                                                                          
176600       IF DAT-KDSVAR-OK                                                   
176700         IF DAT-TIAAMMDD > DAGENS-DATUM                                   
176800           MOVE DAT-TIAAMMDD     TO SLAG-TISKROT-AUTO                     
176900         ELSE                                                             
177000           MOVE JA TO ERR-TISKROT-SW                                      
177100           MOVE MFS-NUM-FIELD-WRONG TO MOD-TISKROT-AUTO-IN-ATTR           
177200           MOVE NEJ TO INDATA-SW                                          
177300         END-IF                                                           
177400       ELSE                                                               
177500         MOVE JA TO ERR-TISKROT-SW                                        
177600         MOVE MFS-NUM-FIELD-WRONG   TO MOD-TISKROT-AUTO-IN-ATTR           
177700         MOVE NEJ TO INDATA-SW                                            
177800       END-IF                                                             
177900     END-IF                                                               
178000     .                                                                    
178100     EJECT                                                                
178110 GD-KOLLA-SKROT-BEORD SECTION.                                            
178123                                                                          
178124     IF MID-FLSKROT-BEORD-IN NOT = ALL '+'                                
178125       IF MID-FLSKROT-BEORD-IN = YES OR JA OR NEJ                         
178128         MOVE MFS-ALPHA-FIELD-OK   TO MOD-FLSKROT-BEORD-ATTR              
178130       ELSE                                                               
178136         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSKROT-BEORD-ATTR             
178137         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
178138         MOVE NEJ                  TO INDATA-SW                           
178140       END-IF                                                             
178142     ELSE                                                                 
178143       MOVE MFS-ALPHA-FIELD-OK     TO MOD-FLSKROT-BEORD-ATTR              
178145     END-IF                                                               
178146     .                                                                    
178150     EJECT                                                                
178213                                                                          
178220                                                                          
178300 H-UPPDATERA SECTION.                                                     
178400     SKIP2                                                                
178410     IF MFS-UPD-V                                                         
178420       PERFORM HH-UPPDATERA-FLSKROT-BEORD                                 
178430     ELSE                                                                 
178500       IF TRANSFER-IFYLLT                                                 
178600         PERFORM HA-SKAPA-ORDER                                           
178700       ELSE                                                               
178800         IF SKROT-IFYLLT                                                  
178900            PERFORM HD-SKAPA-HANDELSE-6321                                
179000            PERFORM HE-UPPDATERA-WDK7                                     
179100         ELSE                                                             
179200            IF SKROT-AUTO                                                 
179300               PERFORM HF-UPDATE-WDK7                                     
179400            END-IF                                                        
179500         END-IF                                                           
179600       END-IF                                                             
179700       PERFORM HG-SKAPA-UPPDATERA-FLREFNYO                                
179710     END-IF                                                               
179800                                                                          
179900*    MOVE MFS-RENSA-FAELT TO MOD                                          
180000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
180100     CALL WMEDKONV USING MED-WMEDAREA                                     
180200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
180300                                                                          
180400     PERFORM MFS-FORM-ATTR                                                
180500     PERFORM MFS-RENSA-FAELT-IN                                           
180600* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
180700     .                                                                    
180800     EJECT                                                                
180900 HA-SKAPA-ORDER SECTION.                                                  
181000     SKIP2                                                                
181100*    OM MID-IDDC-SEND-TO = 11 ÄR DET EN RETUR TILL CDC OCH SKA            
181200*    DÄRMED HA SPECIELLA DISTRIKTSNUMMER OCH KUNDNUMMER.                  
181300*    I ANDRA FALL ÄR DET EN TRANSFER MELLAN DE OLIKA NDC:ERNA             
181400*    OCH DÅ FINNS DET SPECIELLA DISTRIKT- OCH KUNDNUMMER FÖR              
181500*    DETTA                                                                
181600                                                                          
181700     IF MID-IDDC-SEND-TO NOT = TO-DCS-IDDC                                
181800        MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                             
181900        PERFORM IMS-GU-WDB601-TO                                          
182000     END-IF                                                               
182100     IF TO-DCS-CDC OR RETUR-IAF                                           
182200       PERFORM HAA-SKAPA-RETUR-DISTRIKT-KUND                              
182300       IF MID-KDORDKL = YES OR JA                                         
182400*--                                            SNABB RETUR                
182500         PERFORM S03-SKAPA-MSG-KOM-AREA                                   
182600         PERFORM S12-SKAPA-ORDERNR-W411ORDN                               
182700         PERFORM S13-SKAPA-ORDERHUVUD                                     
182800         PERFORM S14-SKAPA-ORDERRADER                                     
182900         PERFORM S16-UPPDAT-RETBEO-O-SPARRKVAL                            
183000       ELSE                                                               
183100*--                                            NORMAL RETUR               
183200         PERFORM HAD-SKAPA-ORDER-WDE3                                     
183300       END-IF                                                             
183400     ELSE                                                                 
183500       PERFORM HAB-SKAPA-TRANSFER-DISTR-KUND                              
183600       IF MID-KDORDKL = YES OR JA                                         
183700*--                                            SNABB-ORDER                
183800         PERFORM S03-SKAPA-MSG-KOM-AREA                                   
183900         PERFORM S12-SKAPA-ORDERNR-W411ORDN                               
184000         PERFORM S13-SKAPA-ORDERHUVUD                                     
184100         PERFORM S14-SKAPA-ORDERRADER                                     
184200         PERFORM S15-UPPDAT-KVBEART                                       
184300       ELSE                                                               
184400*--                                            NORMAL ORDER               
184500         PERFORM HAC-SKAPA-ORDER-WDE3                                     
184600       END-IF                                                             
184700     END-IF                                                               
184800     .                                                                    
184900     EJECT                                                                
185000 HAA-SKAPA-RETUR-DISTRIKT-KUND SECTION.                                   
185100     SKIP2                                                                
185200                                                                          
185300     IF MID-IDDC-SEND-TO = '11'                                           
185400       IF DCS-SDC OR DCS-NDC-CN                                           
185500         MOVE DCS-IDDISTR-RETUR                                           
185600                               TO SPARA-IDDISTR-NUM                       
185700         MOVE SPARA-IDDISTR-NUM                                           
185800                               TO SPARA-IDDISTR                           
185900         IF MID-FLTOT = JA OR YES                                         
186000           MOVE DCS-IDKUNDNR-TRETUR                                       
186100                               TO SPARA-IDKUNDNR-NUM                      
186200           MOVE SPARA-IDKUNDNR-NUM                                        
186300                               TO SPARA-IDKUNDNR                          
186400         ELSE                                                             
186500           IF MID-KDORDKL = JA OR YES                                     
186600             MOVE DCS-IDKUNDNR-SRETUR                                     
186700                               TO SPARA-IDKUNDNR-NUM                      
186800             MOVE SPARA-IDKUNDNR-NUM                                      
186900                               TO SPARA-IDKUNDNR                          
187000           ELSE                                                           
187100             MOVE DCS-IDKUNDNR-RETUR                                      
187200                               TO SPARA-IDKUNDNR-NUM                      
187300             MOVE SPARA-IDKUNDNR-NUM                                      
187400                               TO SPARA-IDKUNDNR                          
187500           END-IF                                                         
187600         END-IF                                                           
187700       ELSE                                                               
187800         MOVE DCS-IDDISTR-RETUR                                           
187900                               TO SPARA-IDDISTR-NUM                       
188000         MOVE SPARA-IDDISTR-NUM                                           
188100                               TO SPARA-IDDISTR                           
188200         MOVE DCS-IDKUNDNR-RETUR                                          
188300                               TO SPARA-IDKUNDNR-NUM                      
188400         MOVE SPARA-IDKUNDNR-NUM                                          
188500                               TO SPARA-IDKUNDNR                          
188600       END-IF                                                             
188700     ELSE                                                                 
188800       MOVE B616-REF-IDDISTR-RETUR                                        
188900                             TO SPARA-IDDISTR-NUM                         
189000       MOVE SPARA-IDDISTR-NUM                                             
189100                             TO SPARA-IDDISTR                             
189200       IF MID-FLTOT = JA OR YES                                           
189300         MOVE B616-REF-IDKUNDNR-TRETUR                                    
189400                             TO SPARA-IDKUNDNR-NUM                        
189500         MOVE SPARA-IDKUNDNR-NUM                                          
189600                             TO SPARA-IDKUNDNR                            
189700       ELSE                                                               
189800         IF MID-KDORDKL = JA OR YES                                       
189900           MOVE B616-REF-IDKUNDNR-SRETUR                                  
190000                             TO SPARA-IDKUNDNR-NUM                        
190100           MOVE SPARA-IDKUNDNR-NUM                                        
190200                             TO SPARA-IDKUNDNR                            
190300         ELSE                                                             
190400           MOVE B616-REF-IDKUNDNR-RETUR                                   
190500                             TO SPARA-IDKUNDNR-NUM                        
190600           MOVE SPARA-IDKUNDNR-NUM                                        
190700                             TO SPARA-IDKUNDNR                            
190800         END-IF                                                           
190900       END-IF                                                             
191000     END-IF                                                               
191100     .                                                                    
191200     EJECT                                                                
191300                                                                          
191400                                                                          
191500 HAB-SKAPA-TRANSFER-DISTR-KUND SECTION.                                   
191600     SKIP2                                                                
191700*--  VID TRANSFERS SÄTTS "FRÅN"-IDDC SOM KUNDNUMMER                       
191800*--  DESSUTOM FINNS DET ETT SPECIELLT DISTRIKTS-NUMMER                    
191900*--  FÖR VARJE MOTTAGARE. DETTA DISTRIKTSNUMMER KAN VARA                  
192000*--  OLIKA BEROENDE PÅ VILKET IDDC SOM ÄR AVSÄNDARE.                      
192100                                                                          
192200     MOVE TP4TRAN-IDDISTR   TO SPARA-IDDISTR-NUM                          
192300     MOVE SPARA-IDDISTR-NUM TO SPARA-IDDISTR                              
192400     MOVE TP4TRAN-IDKUNDNR  TO SPARA-IDKUNDNR-NUM                         
192500     MOVE SPARA-IDKUNDNR-NUM                                              
192600                            TO SPARA-IDKUNDNR                             
192700     .                                                                    
192800 HAC-SKAPA-ORDER-WDE3 SECTION.                                            
192900     SKIP2                                                                
193000     PERFORM IMS-GET-ARTC-CLAG                                            
193100                                                                          
193200     MOVE MID-IDDC-SEND-TO   TO W-IDDC                                    
193300                                                                          
193400     IF MID-IDDC-SEND-TO not = TO-DCS-IDDC                                
193500        move MID-IDDC-SEND-TO TO W-IDDC-B6-TO                             
193600        PERFORM IMS-GU-WDB601-TO                                          
193700     END-IF                                                               
193800     PERFORM IMS-GET-ARTS-WLARTS01                                        
193900     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
194000                                                                          
194100     MOVE SLAG-IDPERSON-BUY                                               
194200                             TO WS-SPARA-IDPERSON-BUY                     
194300                                                                          
194400     MOVE  MID-IDDC-SEND-TO        TO W-IDDC-301                          
194500     MOVE  WS-SPARA-IDPERSON-BUY   TO W-IDPERSON-BUY                      
194600     MOVE 'T'                      TO W-KDREFTYP                          
194700     MOVE  W-IDARTNR               TO W-IDARTNR-301                       
194800     MOVE SPARA-IDDISTR            TO W-IDDISTR                           
194900     PERFORM IMS-GHU-ORDL-WLORDL01                                        
195000                                                                          
195100     IF SEGMENT-FINNS                                                     
195200       COMPUTE SLAG-KVBEART = SLAG-KVBEART - REF-KVBEART                  
195300       END-COMPUTE                                                        
195400                                                                          
195500       COMPUTE SLAG-KVBEART = SLAG-KVBEART + W-KVRETUR-TRANSFER           
195600       END-COMPUTE                                                        
195700                                                                          
195800       MOVE   W-KVRETUR-TRANSFER TO REF-KVBEART                           
195900       MOVE 'O'                  TO REF-KDREFORS                          
195910       MOVE MID-IDDC-SEND-TO     TO REF-IDDC-REF                          
196000                                                                          
196100       PERFORM IMS-REPL-ORDL-WLORDL01                                     
196200                                                                          
196300     ELSE                                                                 
196400       PERFORM HACA-INIT-WDE3-ORDER                                       
196500       PERFORM IMS-ISRT-ORDL-WLORDL01                                     
196600                                                                          
196700       COMPUTE SLAG-KVBEART = SLAG-KVBEART + W-KVRETUR-TRANSFER           
196800                                                                          
196900     END-IF                                                               
197000     PERFORM IMS-REPL-WLARTS11                                            
197100                                                                          
197200     .                                                                    
197300     EJECT                                                                
197400 HACA-INIT-WDE3-ORDER SECTION.                                            
197500     SKIP2                                                                
197600                                                                          
197700     MOVE MID-IDDC-SEND-TO          TO REF-IDDC                           
197800     MOVE WS-SPARA-IDPERSON-BUY     TO REF-IDPERSON-BUY                   
197900     MOVE 'T'                       TO REF-KDREFTYP                       
198000     MOVE W-IDARTNR                 TO REF-IDARTNR                        
198100     MOVE SPARA-IDDISTR             TO SPARA-IDDISTR-NUM                  
198200     MOVE SPARA-IDDISTR-NUM         TO REF-IDDISTR                        
198300     MOVE SLAG-ADLAGOMR             TO REF-ADLAGOMR-SDC                   
198400     MOVE SLAG-ADGANG               TO REF-ADGANG-SDC                     
198500     MOVE SLAG-ADPLATS              TO REF-ADPLATS-SDC                    
198600     MOVE ARTC-CLAG-ADLAGOMR        TO REF-ADLAGOMR-CDC                   
198700     MOVE ARTC-CLAG-ADGANG          TO REF-ADGANG-CDC                     
198800     MOVE ARTC-CLAG-ADPLATS         TO REF-ADPLATS-CDC                    
198900     MOVE SPARA-IDKUNDNR            TO SPARA-IDKUNDNR-NUM                 
199000     MOVE SPARA-IDKUNDNR-NUM        TO REF-IDKUNDNR                       
199100     MOVE W-KVRETUR-TRANSFER        TO REF-KVBEART                        
199200     MOVE 'O'                       TO REF-KDREFORS                       
199300     MOVE W-IDLEVNR-SEND            TO REF-IDLEVNR                        
199400     MOVE ZERO                      TO REF-KDREFTXT                       
199500                                       REF-KDFRAKT                        
199600                                       REF-KVBEART-CD                     
199700                                       REF-ADLAGOMR-CD                    
199800                                       REF-ADGANG-CD                      
199900                                       REF-ADPLATS-CD                     
200000     MOVE TP4TRAN-IDDC-SEND         TO REF-IDDC-REF                       
200100                                                                          
200200     .                                                                    
200300     EJECT                                                                
200400 HAD-SKAPA-ORDER-WDE3 SECTION.                                            
200500     SKIP2                                                                
200600     PERFORM IMS-GET-ARTC-CLAG                                            
200700                                                                          
200800     MOVE W-IDDC-FROM        TO W-IDDC                                    
200900                                                                          
201000     IF W-IDDC-FROM NOT = FROM-DCS-IDDC                                   
201100        MOVE W-IDDC-FROM TO W-IDDC-B6-FROM                                
201200        PERFORM IMS-GU-WDB601-FROM                                        
201300     END-IF                                                               
201400     PERFORM IMS-GET-ARTS-WLARTS01                                        
201500     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
201600                                                                          
201700     MOVE SLAG-IDPERSON-BUY                                               
201800                             TO WS-SPARA-IDPERSON-BUY                     
201900                                                                          
202000     MOVE W-IDDC-FROM              TO W-IDDC-301                          
202100     MOVE  WS-SPARA-IDPERSON-BUY   TO W-IDPERSON-BUY                      
202200     MOVE 'R'                      TO W-KDREFTYP                          
202300     MOVE  W-IDARTNR               TO W-IDARTNR-301                       
202400     MOVE SPARA-IDDISTR            TO W-IDDISTR                           
202500     PERFORM IMS-GHU-ORDL-WLORDL01                                        
202600                                                                          
202700     IF SEGMENT-FINNS                                                     
202800       MOVE   W-KVRETUR-TRANSFER TO REF-KVBEART                           
202900       MOVE 'O'                  TO REF-KDREFORS                          
202910       MOVE W-IDDC-FROM          TO REF-IDDC-REF                          
203000                                                                          
203100       PERFORM IMS-REPL-ORDL-WLORDL01                                     
203200                                                                          
203300     ELSE                                                                 
203400       PERFORM HADA-INIT-WDE3-ORDER                                       
203500       PERFORM IMS-ISRT-ORDL-WLORDL01                                     
203600                                                                          
203700     END-IF                                                               
203800     MOVE W-KVRETUR-TRANSFER   TO SLAG-KVRETUR-BEORD                      
203900     MOVE DAGENS-DATUM         TO SLAG-TIRETUR-BEORD                      
204000     PERFORM IMS-REPL-WLARTS11                                            
204100                                                                          
204200     .                                                                    
204300     EJECT                                                                
204400 HADA-INIT-WDE3-ORDER SECTION.                                            
204500     SKIP2                                                                
204600                                                                          
204700     MOVE W-IDDC-FROM               TO REF-IDDC                           
204800     MOVE WS-SPARA-IDPERSON-BUY     TO REF-IDPERSON-BUY                   
204900     MOVE 'R'                       TO REF-KDREFTYP                       
205000     MOVE W-IDARTNR                 TO REF-IDARTNR                        
205100     MOVE SPARA-IDDISTR             TO SPARA-IDDISTR-NUM                  
205200     MOVE SPARA-IDDISTR-NUM         TO REF-IDDISTR                        
205300     MOVE SLAG-ADLAGOMR             TO REF-ADLAGOMR-SDC                   
205400     MOVE SLAG-ADGANG               TO REF-ADGANG-SDC                     
205500     MOVE SLAG-ADPLATS              TO REF-ADPLATS-SDC                    
205600     IF SLAG-IDDC-REF = '11'                                              
205700       MOVE ARTC-CLAG-ADLAGOMR      TO REF-ADLAGOMR-CDC                   
205800       MOVE ARTC-CLAG-ADGANG        TO REF-ADGANG-CDC                     
205900       MOVE ARTC-CLAG-ADPLATS       TO REF-ADPLATS-CDC                    
206000     ELSE                                                                 
206100       MOVE SLAG-IDDC-REF   TO W-IDDC-TO                                  
206200       PERFORM IMS-GU-WDK7                                                
206300       IF SEGMENT-FINNS                                                   
206400         MOVE TO-SLAG-ADLAGOMR       TO REF-ADLAGOMR-CDC                  
206500         MOVE TO-SLAG-ADGANG         TO REF-ADGANG-CDC                    
206600         MOVE TO-SLAG-ADPLATS        TO REF-ADPLATS-CDC                   
206700       ELSE                                                               
206800         MOVE ZERO                   TO REF-ADLAGOMR-CDC                  
206900         MOVE ZERO                   TO REF-ADGANG-CDC                    
207000         MOVE ZERO                   TO REF-ADPLATS-CDC                   
207100       END-IF                                                             
207200     END-IF                                                               
207300     MOVE SPARA-IDKUNDNR            TO SPARA-IDKUNDNR-NUM                 
207400     MOVE SPARA-IDKUNDNR-NUM        TO REF-IDKUNDNR                       
207500     MOVE W-KVRETUR-TRANSFER        TO REF-KVBEART                        
207600     MOVE 'N'                       TO REF-KDREFORS                       
207700     MOVE SLAG-IDLEVNR              TO REF-IDLEVNR                        
207800     MOVE ZERO                      TO REF-KDREFTXT                       
207900                                       REF-KDFRAKT                        
208000                                       REF-KVBEART-CD                     
208100                                       REF-ADLAGOMR-CD                    
208200                                       REF-ADGANG-CD                      
208300                                       REF-ADPLATS-CD                     
208310     MOVE SLAG-IDDC-REF             TO REF-IDDC-REF                       
208400                                                                          
208500     .                                                                    
208600     EJECT                                                                
208700 HD-SKAPA-HANDELSE-6321 SECTION.                                          
208800                                                                          
208900     PERFORM IMS-GET-ARTC-CLAG                                            
209000                                                                          
209100* -- WDGX6321 -- WDGX6322                                                 
209200     MOVE '6321'            TO 6321-IDHTYP                                
209300     MOVE WS-KDARBTYP(1:3)  TO 6321-KDARBTYP                              
209400                               W-KDARBTYP                                 
209500     MOVE LOW-VALUE         TO 6321-LOW-VALUE                             
209600     PERFORM IMS-ISRT-WDR501-6321                                         
209700                                                                          
209800     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
209900     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
210000     PERFORM IMS-ISRT-WDGX6322                                            
210100                                                                          
210200* -- WDGX6324                                                             
210300     MOVE SPACE             TO 6324-WDGX6324                              
210400     MOVE W-IDARTNR-K7      TO 6324-IDARTNR                               
210500     MOVE W-IDDC-FROM       TO 6324-IDDC                                  
210600     MOVE 1                 TO 6324-KDSTASKR                              
210700     IF MID-SKROT-TEXT NOT = ALL '+'                                      
210800       MOVE MID-SKROT-TEXT  TO 6324-BELAGINS-DEL                          
210900     END-IF                                                               
211000     MOVE NEJ               TO 6324-FLSKROT-GODK                          
211100     IF (MID-IDANALYS NOT = ALL '+'                                       
211200     AND MID-IDANALYS > SPACE)                                            
211300        MOVE MID-IDANALYS   TO 6324-IDANALYS                              
211400        MOVE MID-IDKONTO    TO 6324-IDKONTO                               
211500        MOVE SPACE          TO 6324-IDKST                                 
211600     ELSE                                                                 
211700        MOVE ZERO           TO 6324-IDKONTO                               
211800        MOVE SPACE          TO 6324-IDANALYS                              
211900                               6324-IDKST                                 
212000     END-IF                                                               
212100                                                                          
212200     MOVE DCS-IDDISTR-SKROT TO 6324-IDDISTR                               
212300     MOVE DCS-IDKUNDNR-SKROT                                              
212400                            TO 6324-IDKUNDNR                              
212500     MOVE WS-KDARBTYP(4:3)  TO 6324-IDPERSON                              
212600     MOVE MSG-SIGNON-USERID TO 6324-IDUSER                                
212700     MOVE ZERO              TO 6324-KDFRAKT                               
212800     MOVE 1                 TO 6324-KDORDKL                               
212900     MOVE W-KVSKROT         TO 6324-KVSKROT-BEORD                         
213000     MOVE ZERO              TO 6324-KVSKROT-KVAR                          
213100     MOVE W-KVSKROT-KVAR-NUM  TO 6324-KVSKROT-ONDEM                       
213200     MOVE WS-SPAR-BEANST-GODK TO 6324-BEANST                              
213300     MOVE ARTC-CLAG-KDERS        TO 6324-KDERS-UTG                        
213400     PERFORM HDA-LAS-FLYTTA-WDK7                                          
213500     PERFORM IMS-GU-WDK901                                                
213600     IF SEGMENT-FINNS                                                     
213700        MOVE ART-SUTPO-TOT        TO 6324-SUTPO-TOT                       
213800                                                                          
213900        COMPUTE WS-KVOKS-CDC = ART-KVOKS-BULK       +                     
214000                               ART-KVOKS-DAG        +                     
214100                               ART-KVOKS-VOR                              
214200                                                                          
214300     ELSE                                                                 
214400        MOVE ZERO                      TO 6324-SUTPO-TOT                  
214500                                          WS-KVOKS-CDC                    
214600     END-IF                                                               
214700     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
214800       ARTC-CLAG-KVLS     - ARTC-CLAG-KVRESS                              
214900                          - ARTC-CLAG-KVROS                               
215000                          - WS-KVOKS-CDC                                  
215100                                                                          
215200     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
215300            W-SDC-KVLS - W-SDC-KVOKS                                      
215400                                                                          
215500     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
215600     ARTC-CLAG-KVAKS-CDC  + ARTC-CLAG-KVAKS-PAV                           
215700                          + ARTC-CLAG-KVAKS-T                             
215800                                                                          
215900     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
216000          W-SDC-KVAKS                                                     
216100     PERFORM HDB-LAS-FLYTTA-WDN6                                          
216200     PERFORM IMS-ISRT-WDGX6324                                            
216300                                                                          
216400     PERFORM IMS-GET-ARTS-WLARTS01                                        
216500     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
216600                                                                          
216700     MOVE NEJ               TO SLAG-FLSKROT-AUTO                          
216800     MOVE JA                TO SLAG-FLSKROT-BEORD                         
216900     MOVE DAGENS-DATUM      TO SLAG-TISKROT-BEORD                         
217000     PERFORM IMS-REPL-WLARTS11                                            
217100     .                                                                    
217200     EJECT                                                                
217301 HDA-LAS-FLYTTA-WDK7 SECTION.                                             
217400                                                                          
217500     PERFORM IMS-GET-ARTS-WLARTS01                                        
217600     IF SEGMENT-FINNS                                                     
217700        MOVE ZERO             TO W-SDC-KVLS                               
217800                                 W-SDC-KVAKS                              
217900                                 W-SDC-KVOKS                              
218000        PERFORM IMS-GNP-ARTS-WLARTS11                                     
218100        PERFORM UNTIL SEGMENT-SAKNAS                                      
218200           ADD SLAG-KVLS        TO W-SDC-KVLS                             
218300           ADD SLAG-KVAKS-SDC TO W-SDC-KVAKS                              
218400           ADD SLAG-KVAKS-PAV TO W-SDC-KVAKS                              
218500           ADD SLAG-KVOKS-DAG TO W-SDC-KVOKS                              
218600           ADD SLAG-KVOKS-BULK TO W-SDC-KVOKS                             
218700          PERFORM IMS-GNP-ARTS-WLARTS11                                   
218800        END-PERFORM                                                       
218900     END-IF                                                               
219000     .                                                                    
219100     EJECT                                                                
219200                                                                          
219300 HDB-LAS-FLYTTA-WDN6 SECTION.                                             
219400     MOVE +1 TO BEEMB-IX                                                  
219500     PERFORM UNTIL BEEMB-IX > 20                                          
219600        MOVE SPACE        TO 6324-BEEMBLEM (BEEMB-IX)                     
219700        ADD +1        TO BEEMB-IX                                         
219800     END-PERFORM                                                          
219900     PERFORM IMS-GU-WDN601                                                
220000     IF SEGMENT-FINNS                                                     
220100        PERFORM IMS-GNP-WDN611                                            
220200        MOVE +1 TO BEEMB-IX                                               
220300        PERFORM UNTIL BEEMB-IX > 20 OR SEGMENT-SAKNAS                     
220400           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (BEEMB-IX)                  
220500           ADD +1        TO BEEMB-IX                                      
220600           PERFORM IMS-GNP-WDN611                                         
220700        END-PERFORM                                                       
220800        IF SEGMENT-FINNS                                                  
220900           MOVE 'MORE' TO 6324-BEEMBLEM (20)                              
221000        END-IF                                                            
221100     END-IF                                                               
221200      .                                                                   
221300      EJECT                                                               
221400                                                                          
221500 HE-UPPDATERA-WDK7 SECTION.                                               
221600                                                                          
221700     PERFORM IMS-GET-ARTS-WLARTS01                                        
221800     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
221900     MOVE NEJ                    TO SLAG-FLSKROT-AUTO                     
222000     MOVE JA                     TO SLAG-FLSKROT-BEORD                    
222100     MOVE DAGENS-DATUM           TO SLAG-TISKROT-BEORD                    
222200                                                                          
222300     COMPUTE W-ANTAL-SKROT = SLAG-KVLS - W-KVSKROT-KVAR-NUM               
222400     END-COMPUTE                                                          
222500                                                                          
222600     PERFORM IMS-REPL-WLARTS11                                            
222700     .                                                                    
222800     EJECT                                                                
222900 HF-UPDATE-WDK7 SECTION.                                                  
223000                                                                          
223100     PERFORM IMS-GET-ARTS-WLARTS01                                        
223200     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
223300     IF SEGMENT-FINNS                                                     
223400       IF MID-TISKROT-AUTO-IN NOT = ALL '+'                               
223401         MOVE MID-TISKROT-AUTO-IN    TO SLAG-TISKROT-AUTO                 
223402       END-IF                                                             
223500       PERFORM IMS-REPL-WLARTS11                                          
223600     END-IF                                                               
223700     .                                                                    
223800     EJECT                                                                
223900 HG-SKAPA-UPPDATERA-FLREFNYO SECTION.                                     
224000                                                                          
224100* -- UPPDATERING AV FLREFNYO PGA LAGERSALDOFÖRÄNDRING                     
224200     MOVE W-IDDC-FROM TO W-IDDC                                           
224300     PERFORM IMS-GET-ARTS-WLARTS01                                        
224400     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
224500     IF SEGMENT-FINNS                                                     
224600        MOVE NEJ           TO SLAG-FLREFNYO                               
224700        PERFORM IMS-REPL-WLARTS11                                         
224800     END-IF                                                               
224900     .                                                                    
225000     EJECT                                                                
225100                                                                          
225110 HH-UPPDATERA-FLSKROT-BEORD SECTION.                                      
225112     IF MID-FLSKROT-BEORD-IN NOT = '+'                                    
225113       PERFORM IMS-GET-ARTS-WLARTS01                                      
225114       IF SEGMENT-FINNS                                                   
225115         PERFORM IMS-GHNP-ARTS-WLARTS11                                   
225116         IF SEGMENT-FINNS                                                 
225117           MOVE MID-FLSKROT-BEORD-IN TO SLAG-FLSKROT-BEORD                
225118           PERFORM IMS-REPL-WLARTS11                                      
225119         END-IF                                                           
225120       END-IF                                                             
225123     END-IF                                                               
225124     .                                                                    
225130     EJECT                                                                
225200 S01-KONTROLLERA-KDARBTYP SECTION.                                        
225300                                                                          
225400     IF TRANSFER-IFYLLT                                                   
225500* -- OM INGEN KDARBTYP FINNS FÅR INGEN UPPDATERING GÖRAS                  
225600       IF WS-KDARBTYP-SEC(1:3) = 'FEL'                                    
225700         MOVE MED-12 (SPR)        TO MOD-TEMFSINF                         
225800         MOVE NEJ                 TO INDATA-SW                            
225900       END-IF                                                             
226000* -- VOR GRUPPEN                                                          
226100* -- FÅR BARA RETURNERA IFRÅN SDC:ER                                      
226200       IF WS-KDARBTYP-SEC(1:3) = 'VOR'                                    
226300         IF MSGI-IDDC-KEY = WC-SDC-NL OR                                  
226500                            WC-SDC-ES OR                                  
226600                            WC-SDC-IT OR                                  
226700                            WC-SDC-AT OR                                  
226800                            WC-LDC-GB-3A                                  
226900           CONTINUE                                                       
227000         ELSE                                                             
227100           MOVE MED-12 (SPR)        TO MOD-TEMFSINF                       
227200           MOVE NEJ                 TO INDATA-SW                          
227300         END-IF                                                           
227400       END-IF                                                             
227500* -- LOKALA LAGERSTYRARE                                                  
227600       IF WS-KDARBTYP-SEC(1:3) = 'LOC'                                    
227700         IF MSGI-IDDC NOT = MSGI-DCS-IDDC                                 
227800            MOVE MSGI-IDDC TO W-IDDC-B6-MSGI                              
227900            PERFORM IMS-GU-WDB601-MSGI                                    
228000         END-IF                                                           
228100         IF MID-IDDC-SEND-TO NOT = TO-DCS-IDDC                            
228200            MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                         
228300            PERFORM IMS-GU-WDB601-TO                                      
228400         END-IF                                                           
228500         IF (MSGI-DCS-NDC-NA OR MSGI-DCS-NDC-CN)                          
228700           IF (TO-DCS-NDC-NA OR TO-DCS-NDC-CN)                            
228900             CONTINUE                                                     
229000           ELSE                                                           
229100             MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-SEND-TO-ATTR            
229200             MOVE MED-12 (SPR)        TO MOD-TEMFSINF                     
229300             MOVE NEJ                 TO INDATA-SW                        
229400           END-IF                                                         
229500         ELSE                                                             
229600* -- TRANSFER MELLAN DC61 & DC62 & RETURER FÅR BARA GÖRAS AV              
229700* -- LAGERSTYRARE I CDC                                                   
229800           IF MSGI-DCS-NDC-PF                                             
229810           OR MSGI-DCS-NDC-OTHERS                                         
229820           OR MSGI-DCS-NDC-SA                                             
229900             MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-SEND-TO-ATTR            
230000             MOVE MED-12 (SPR)        TO MOD-TEMFSINF                     
230100             MOVE NEJ                 TO INDATA-SW                        
230200           END-IF                                                         
230300         END-IF                                                           
230400       END-IF                                                             
230500     END-IF                                                               
230600                                                                          
230700     IF SKROT-IFYLLT                                                      
230800* -- OM INGEN KDARBTYP FINNS FÅR INGEN UPPDATERING GÖRAS                  
230900       IF WS-KDARBTYP-SEC(1:3) = 'FEL'                                    
231000         MOVE MED-11 (SPR)        TO MOD-TEMFSINF                         
231100         MOVE NEJ                 TO INDATA-SW                            
231200       END-IF                                                             
231300* -- VOR GRUPPEN FÅR INTE SKROTA NÅGONTING                                
231400       IF WS-KDARBTYP-SEC(1:3) = 'VOR'                                    
231500         MOVE MED-11 (SPR)        TO MOD-TEMFSINF                         
231600         MOVE NEJ                 TO INDATA-SW                            
231700       END-IF                                                             
231800* -- LOKALA LAGERSTYRARE                                                  
231900       IF WS-KDARBTYP-SEC(1:3) = 'LOC'                                    
232000         PERFORM IMS-GET-ARTS-WLARTS11                                    
232100         IF SLAG-IDDC-REF NOT = '11'                                      
232200           IF MSGI-IDDC NOT = MSGI-DCS-IDDC                               
232300              MOVE MSGI-IDDC TO W-IDDC-B6-MSGI                            
232400              PERFORM IMS-GU-WDB601-MSGI                                  
232500           END-IF                                                         
232600           IF (MSGI-DCS-NDC-NA OR MSGI-DCS-NDC-CN)                        
232800             IF MSGI-IDDC-KEY NOT = MSGI-DCS-IDDC                         
232900                MOVE MSGI-IDDC-KEY TO W-IDDC-B6-MSGI                      
233000                PERFORM IMS-GU-WDB601-MSGI                                
233100             END-IF                                                       
233200             IF (MSGI-DCS-NDC-NA OR MSGI-DCS-NDC-CN)                      
233400               CONTINUE                                                   
233500             ELSE                                                         
233600               MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-UT-ATTR               
233700               MOVE MED-11 (SPR)        TO MOD-TEMFSINF                   
233800               MOVE NEJ                 TO INDATA-SW                      
233900             END-IF                                                       
234000           END-IF                                                         
234100           IF MSGI-IDDC NOT = MSGI-DCS-IDDC                               
234200              MOVE MSGI-IDDC TO W-IDDC-B6-MSGI                            
234300              PERFORM IMS-GU-WDB601-MSGI                                  
234400           END-IF                                                         
234500           IF MSGI-DCS-NDC-PF                                             
234510           OR MSGI-DCS-NDC-OTHERS                                         
234520           OR MSGI-DCS-NDC-SA                                             
234600             IF MSGI-IDDC-KEY NOT = MSGI-DCS-IDDC                         
234700                MOVE MSGI-IDDC-KEY TO W-IDDC-B6-MSGI                      
234800                PERFORM IMS-GU-WDB601-MSGI                                
234900             END-IF                                                       
235000             IF MSGI-DCS-NDC-PF                                           
235010             OR MSGI-DCS-NDC-OTHERS                                       
235020             OR MSGI-DCS-NDC-SA                                           
235100               IF MSGI-IDDC = MSGI-IDDC-KEY                               
235200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR           
235300                 MOVE MED-13 (SPR)      TO MOD-TEMFSINF                   
235400                 MOVE NEJ               TO INDATA-SW                      
235500               ELSE                                                       
235600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UT-ATTR              
235700                 MOVE MED-11 (SPR)      TO MOD-TEMFSINF                   
235800                 MOVE NEJ               TO INDATA-SW                      
235900               END-IF                                                     
236000             ELSE                                                         
236100               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UT-ATTR              
236200               MOVE MED-11 (SPR)        TO MOD-TEMFSINF                   
236300               MOVE NEJ                 TO INDATA-SW                      
236400             END-IF                                                       
236500           END-IF                                                         
236600         ELSE                                                             
236700           IF MSGI-IDDC NOT = MSGI-DCS-IDDC                               
236800              MOVE MSGI-IDDC TO W-IDDC-B6-MSGI                            
236900              PERFORM IMS-GU-WDB601-MSGI                                  
237000           END-IF                                                         
237100           IF MSGI-IDDC-KEY NOT = KEY-DCS-IDDC                            
237200              MOVE MSGI-IDDC-KEY TO W-IDDC-B6-KEY                         
237300              PERFORM IMS-GU-WDB601-KEY                                   
237400           END-IF                                                         
237500           IF ((MSGI-DCS-NDC-PF AND MSGI-DCS-JAPAN) AND                   
237600           NOT (KEY-DCS-NDC-PF AND KEY-DCS-JAPAN))                        
237610                                                                          
237700           OR ((MSGI-DCS-NDC-PF AND MSGI-DCS-AUSTRALIA) AND               
237800           NOT (KEY-DCS-NDC-PF AND KEY-DCS-AUSTRALIA))                    
237810                                                                          
237900           OR ((MSGI-DCS-NDC-PF AND MSGI-DCS-INDIA) AND                   
238000           NOT (KEY-DCS-NDC-PF AND KEY-DCS-INDIA))                        
238001                                                                          
238002           OR ((MSGI-DCS-NDC-OTHERS AND MSGI-DCS-EMIRATES) AND            
238003           NOT (KEY-DCS-NDC-OTHERS AND KEY-DCS-EMIRATES))                 
238004                                                                          
238010           OR ((MSGI-DCS-NDC-PF AND MSGI-DCS-KOREA) AND                   
238020           NOT (KEY-DCS-NDC-PF AND KEY-DCS-KOREA))                        
238030                                                                          
238031           OR ((MSGI-DCS-NDC-SA AND MSGI-DCS-MEXICO) AND                  
238032           NOT (KEY-DCS-NDC-SA AND KEY-DCS-MEXICO))                       
238033                                                                          
238034           OR ((MSGI-DCS-NDC-SA AND MSGI-DCS-BRASIL) AND                  
238035           NOT (KEY-DCS-NDC-SA AND KEY-DCS-BRASIL))                       
238036                                                                          
238040           OR ((MSGI-DCS-NDC-PF AND MSGI-DCS-TURKEY) AND                  
238050           NOT (KEY-DCS-NDC-PF AND KEY-DCS-TURKEY))                       
238100             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-SEND-TO-ATTR           
238200             MOVE MED-11 (SPR)        TO MOD-TEMFSINF                     
238300             MOVE NEJ                 TO INDATA-SW                        
238400           END-IF                                                         
238500           IF SLAG-IDDC (1:1) = '4' OR '5'                                
238600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-SEND-TO-ATTR           
238700             MOVE MED-11 (SPR)        TO MOD-TEMFSINF                     
238800             MOVE NEJ                 TO INDATA-SW                        
238900           END-IF                                                         
239000         END-IF                                                           
239100       END-IF                                                             
239200     END-IF                                                               
239300     .                                                                    
239400     EJECT                                                                
239500                                                                          
239600 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
239700     SKIP2                                                                
239800     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
239900     MOVE +54                    TO MSG-KOM-KVLL                          
240000     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
240100     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
240200     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
240300     MOVE 'TRANSFER'             TO MSG-KOM-IDSNDNOD                      
240400     MOVE 'W2035900'             TO MSG-KOM-IDSNDJOB                      
240500     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
240600     MOVE DAGENS-TID             TO MSG-KOM-TIKLOCK                       
240700     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
240800     .                                                                    
240900     EJECT                                                                
241000 S11-W009KSIF  SECTION.                                                   
241100     SKIP2                                                                
241200*    BERÄKNA KONTROLLSIFFRA FÖR ARTIKELNR                                 
241300     IF ORAD-MID-REKSIFFR (1) = SPACE OR                                  
241400        ORAD-MID-REKSIFFR (1) = '0'                                       
241500        IF ORAD-MID-IDARTNR (1) NUMERIC                                   
241600           MOVE ORAD-MID-IDARTNR (1) TO REK-IDARTNR                       
241700           MOVE 0                TO REK-REKSIFFR                          
241800           MOVE 9                TO REK-LNGD                              
241900           CALL W009KSIF USING REK-IDARTNR                                
242000                               REK-LNGD                                   
242100                               REK-REKSIFFR                               
242200           MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR (1)                 
242300        END-IF                                                            
242400     END-IF                                                               
242500     .                                                                    
242600     EJECT                                                                
242700                                                                          
242800                                                                          
242900 S12-SKAPA-ORDERNR-W411ORDN  SECTION.                                     
243000                                                                          
243100     MOVE 'W203'               TO ORDN-IDSYSTEM                           
243200                                                                          
243300     MOVE SPARA-IDDISTR        TO ORDN-IDDISTR                            
243400     MOVE SPARA-IDKUNDNR       TO ORDN-IDKUNDNR                           
243500                                                                          
243600     MOVE ZERO                 TO ORDN-IDORDNR-IN                         
243700                                                                          
243800     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
243900                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
244000                                                                          
244100     MOVE ORDN-IDORDNR-UT      TO SPARA-IDORDNR                           
244200     .                                                                    
244300     EJECT                                                                
244400                                                                          
244500                                                                          
244600 S13-SKAPA-ORDERHUVUD SECTION.                                            
244700     SKIP2                                                                
244800     MOVE SPACE TO OHUV-MID-W4I25101                                      
244900     MOVE SPACE            TO KOM-AREA                                    
245000     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
245100     MOVE 'REFT'           TO OHUV-MID-IDSYSTEM                           
245200     MOVE W-IDDC-FROM      TO OHUV-MID-IDDC                               
245300     MOVE SPARA-IDDISTR    TO OHUV-MID-IDDISTR                            
245400     MOVE SPARA-IDKUNDNR   TO OHUV-MID-IDKUNDNR                           
245500     MOVE SPARA-IDORDNR    TO OHUV-MID-IDORDNR                            
245600     IF (MID-KDORDKL = NEJ OR '+')                                        
245800         MOVE '3'          TO OHUV-MID-KDORDKL                            
245900     ELSE                                                                 
246000         MOVE '1'          TO OHUV-MID-KDORDKL                            
246100         IF MID-KDFRAKT = '12'                                            
246200            MOVE MID-KDFRAKT TO OHUV-MID-KDFRAKT                          
246300         ELSE                                                             
246400            MOVE SPACE       TO OHUV-MID-KDFRAKT                          
246500         END-IF                                                           
246600     END-IF                                                               
246700                                                                          
246800     IF MID-FLTOT = JA OR YES                                             
246900*--     FLYTTAR DEFAULTTEXT TILL LAGERINSTRUKTION OM ATT MAN SKA          
247000*--     MEDDELA VCAS OM AVVIKELSER                                        
247100*--     KONCATENERAR TEXT TILL LAGERINSTRUKTION OM ATT MAN SKA            
247200*--     PLOCKA ALLT PÅ HYLLAN  "PICK TOTAL"                               
247300                                                                          
247400        STRING W-TRANSFER-DEFAULT-TEXT ', '                               
247500               W-TRANSFER-DEFAULT-TEXT-RAD                                
247600        DELIMITED BY SIZE INTO  OHUV-MID-BELAGINS                         
247700     ELSE                                                                 
247800*       FLYTTA BILDENS TRANSFERTEXTER TILL OHUV-LAGERINSTRUKT.            
247900        MOVE SPACE TO W-TRANSFER-TEXT                                     
248000                                                                          
248100        IF MID-TRANSFER-TEXT-1 = ALL '+'                                  
248200          IF MID-TRANSFER-TEXT-2 NOT = ALL '+'                            
248300            MOVE MID-TRANSFER-TEXT-2 TO W-TRANSFER-TEXT-1                 
248400          END-IF                                                          
248500        ELSE                                                              
248600          MOVE MID-TRANSFER-TEXT-1 TO W-TRANSFER-TEXT-1                   
248700          IF MID-TRANSFER-TEXT-2 NOT = ALL '+'                            
248800            MOVE MID-TRANSFER-TEXT-2 TO W-TRANSFER-TEXT-2                 
248900          END-IF                                                          
249000        END-IF                                                            
249100                                                                          
249200        MOVE W-TRANSFER-TEXT TO OHUV-MID-BELAGINS                         
249300     END-IF                                                               
249400                                                                          
249500     MOVE NEJ                  TO OHUV-MID-FLAUTFAK                       
249600     MOVE NEJ                  TO OHUV-MID-FLAUTPAC                       
249700                                  OHUV-MID-FLEMBORD                       
249800                                  OHUV-MID-FLOVRLEV                       
249900                                  OHUV-MID-FLFORBI                        
250000                                  OHUV-MID-FLORDTIL                       
250100     MOVE SPACE                TO OHUV-MID-KDORDTYP-LDC                   
250200     MOVE ZERO                 TO OHUV-MID-TIREPDAT                       
250400                                  OHUV-MID-IDGROSS                        
250500                                                                          
250600     COMPUTE P-TO-P-KVLL = LENGTH OF OHUV-MID-W4I25101 + 17               
250700     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
250800     MOVE '4251'           TO P-TO-P-IDTRANS                              
250900     MOVE '1'              TO P-TO-P-KDMFSFOR                             
251000                                                                          
251100     MOVE KOM-AREA                TO P-TO-P-DATA                          
251200     CALL W006KOM USING MSG-PCB                                           
251300                        ALT-PCB                                           
251400                        KOMA-PCB                                          
251500                        MSG-KOM-WMSGKOM                                   
251600                        P-TO-P-SW                                         
251700     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
251800*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
251900*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
252000        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
252100        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
252200     END-IF                                                               
252300                                                                          
252400     MOVE SPACE TO KOM-AREA                                               
252500     .                                                                    
252600     EJECT                                                                
252700 S14-SKAPA-ORDERRADER SECTION.                                            
252800     SKIP2                                                                
252900                                                                          
253000     MOVE SPACE TO ORAD-MID-W4I25201                                      
253100     MOVE SPACE            TO KOM-AREA                                    
253200     MOVE 'W4I25201'       TO MSG-KOM-IDCPYTXT                            
253300                                                                          
253400     MOVE 'REFT'           TO ORAD-MID-IDSYSTEM                           
253500     MOVE SPARA-IDDISTR    TO ORAD-MID-IDDISTR                            
253600     MOVE SPARA-IDKUNDNR   TO ORAD-MID-IDKUNDNR                           
253700     MOVE SPARA-IDORDNR    TO ORAD-MID-IDORDNR                            
253800     MOVE MSGI-IDARTNR     TO ORAD-MID-IDARTNR (1)                        
253900     PERFORM S11-W009KSIF                                                 
254000*--  HÖGERJUSTERA OCH NOLLFYLL KVRETUR-TRANSFER                           
254100     MOVE W-KVRETUR-TRANSFER TO W-KVRETUR-TRANSFER-TRUNK                  
254200     MOVE W-KVRETUR-TRANSFER-TRUNK TO ORAD-MID-KVBEART (1)                
254300     INSPECT ORAD-MID-KVBEART (1) REPLACING ALL SPACE BY ZERO             
254400     MOVE WS-ADLAGOMR-N    TO WS-ADLAGOMR-X                               
254500     MOVE WS-ADGANG-N      TO WS-ADGANG-X                                 
254600     MOVE WS-ADPLATS-N     TO WS-ADPLATS-X                                
254700     MOVE WS-ADART-X       TO ORAD-MID-BERADREF (1)                       
254800     MOVE JA               TO ORAD-MID-FLSLUT                             
254900                                                                          
255000     COMPUTE P-TO-P-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17               
255100     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
255200     MOVE '4252'           TO P-TO-P-IDTRANS                              
255300     MOVE '1'              TO P-TO-P-KDMFSFOR                             
255400                                                                          
255500     MOVE KOM-AREA         TO P-TO-P-DATA                                 
255600     CALL W006KOM USING MSG-PCB                                           
255700                        ALT-PCB                                           
255800                        KOMA-PCB                                          
255900                        MSG-KOM-WMSGKOM                                   
256000                        P-TO-P-SW                                         
256100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
256200*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
256300*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
256400        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
256500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
256600     END-IF                                                               
256700                                                                          
256800     MOVE SPACE TO KOM-AREA                                               
256900     .                                                                    
257000     EJECT                                                                
257100 S15-UPPDAT-KVBEART SECTION.                                              
257200     SKIP2                                                                
257300     IF MID-IDDC-SEND-TO NOT = TO-DCS-IDDC                                
257400        MOVE MID-IDDC-SEND-TO TO W-IDDC-B6-TO                             
257500        PERFORM IMS-GU-WDB601-TO                                          
257600     END-IF                                                               
257700     IF TO-DCS-CDC OR RETUR-IAF                                           
257800       CONTINUE                                                           
257900     ELSE                                                                 
258000       MOVE MID-IDDC-SEND-TO TO W-IDDC                                    
258100       PERFORM IMS-GET-ARTS-WLARTS01                                      
258200       PERFORM IMS-GHNP-ARTS-WLARTS11                                     
258300       IF SEGMENT-FINNS                                                   
258400           COMPUTE SLAG-KVBEART =                                         
258500                   SLAG-KVBEART + W-KVRETUR-TRANSFER                      
258600           END-COMPUTE                                                    
258700           PERFORM IMS-REPL-WLARTS11                                      
258800       END-IF                                                             
258900     END-IF                                                               
259000     .                                                                    
259100     EJECT                                                                
259200 S16-UPPDAT-RETBEO-O-SPARRKVAL   SECTION.                                 
259300     SKIP2                                                                
259400*VID RETURER SKA MAN UPPDATERA RETURNERAT ANTAL SAMT TIDPUNKT             
259500*                                                                         
259600     MOVE W-IDDC-FROM  TO W-IDDC                                          
259700     PERFORM IMS-GET-ARTS-WLARTS01                                        
259800     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
259900     MOVE W-KVRETUR-TRANSFER   TO SLAG-KVRETUR-BEORD                      
260000     MOVE DAGENS-DATUM         TO SLAG-TIRETUR-BEORD                      
260100                                                                          
260200     PERFORM IMS-REPL-WLARTS11                                            
260300     IF RETUR-IAF                                                         
260400       MOVE MID-IDDC-SEND-TO TO W-IDDC                                    
260500       PERFORM IMS-GET-ARTS-WLARTS01                                      
260600       PERFORM IMS-GHNP-ARTS-WLARTS11                                     
260700       IF SEGMENT-FINNS                                                   
260800           COMPUTE SLAG-KVBEART =                                         
260900                   SLAG-KVBEART + W-KVRETUR-TRANSFER                      
261000           END-COMPUTE                                                    
261100           PERFORM IMS-REPL-WLARTS11                                      
261200       END-IF                                                             
261300     END-IF                                                               
261400     .                                                                    
261500     EJECT                                                                
261600                                                                          
261700 MFS-RENSA-FAELT-UT SECTION.                                              
261800                                                                          
261900*    --- ALLA UTDATA-FÄLT                                                 
262000     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
262100                             MOD-IDDC-SEND-TO                             
262200                             MOD-KVRETUR-TRANSFER                         
262300                             MOD-KDORDKL                                  
262400                             MOD-FLTOT                                    
262500                             MOD-TRANSFER-TEXT-1                          
262600                             MOD-TRANSFER-TEXT-2                          
262700                             MOD-KVSKROT-KVAR                             
262800                             MOD-VALUE-OF-SCRAP-QTY                       
262900                             MOD-TISKROT                                  
263000                             MOD-KVSKROT                                  
263100                             MOD-TIRETUR-BEORD                            
263200                             MOD-KVRETUR-BEORD                            
263300                             MOD-KVSTOCK                                  
263400                             MOD-KVAKS-PAV                                
263500                             MOD-KVAKS-SDC                                
263600                             MOD-KVBEART                                  
263700                             MOD-IDKONTO                                  
263800                             MOD-IDANALYS                                 
263900                             MOD-TISKROT-AUTO-UT                          
264000                             MOD-FLSKROT-AUTO                             
264010                             MOD-FLSKROT-BEORD                            
264100     .                                                                    
264200     EJECT                                                                
264300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
264400                                                                          
264500*    --- ALLA UTDATA-FÄLT                                                 
264600     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
264700                              MOD-IDDC-SEND-TO                            
264800                              MOD-IDKONTO                                 
264900                              MOD-IDANALYS                                
265000                              MOD-KVRETUR-TRANSFER                        
265100                              MOD-KDORDKL                                 
265200                              MOD-FLTOT                                   
265300                              MOD-TRANSFER-TEXT-1                         
265400                              MOD-TRANSFER-TEXT-2                         
265500                              MOD-KVSKROT-KVAR                            
265600                              MOD-VALUE-OF-SCRAP-QTY                      
265700                              MOD-TISKROT                                 
265800                              MOD-KVSKROT                                 
265900                              MOD-TIRETUR-BEORD                           
266000                              MOD-KVRETUR-BEORD                           
266100                              MOD-KVSTOCK                                 
266200                              MOD-KVAKS-PAV                               
266300                              MOD-KVAKS-SDC                               
266400                              MOD-KVBEART                                 
266500                              MOD-IDKONTO                                 
266600                              MOD-IDANALYS                                
266700                              MOD-TISKROT-AUTO-UT                         
266800                              MOD-FLSKROT-AUTO                            
266810                              MOD-FLSKROT-BEORD                           
266900     .                                                                    
267000     SKIP3                                                                
267100 MFS-RENSA-FAELT-IN SECTION.                                              
267200                                                                          
267300*    --- ALLA INDATA-FÄLT                                                 
267400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
267500                             MOD-IDDC-IN                                  
267600                             MOD-IDKONTO                                  
267700                             MOD-IDANALYS                                 
267800                             MOD-TISKROT-AUTO-IN                          
267810                             MOD-FLSKROT-BEORD                            
267900     .                                                                    
268000     EJECT                                                                
268100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
268200                                                                          
268300*    --- ALLA INDATA-FÄLT                                                 
268400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
268500                               MOD-IDDC-IN                                
268600                               MOD-TISKROT-AUTO-IN                        
268610                               MOD-FLSKROT-BEORD                          
268700     .                                                                    
268800     EJECT                                                                
268900 MFS-FORM-ATTR SECTION.                                                   
269000                                                                          
269100*    --- ALLA INDATA-FÄLT                                                 
269200     MOVE MFS-FORMATETS-ATTR TO MOD-BEART-ATTR                            
269300                                MOD-IDDC-SEND-TO-ATTR                     
269400                                MOD-KVRETUR-TRANSFER-ATTR                 
269500                                MOD-KDORDKL-ATTR                          
269600                                MOD-FLTOT-ATTR                            
269700                                MOD-TRANSFER-TEXT-1-ATTR                  
269800                                MOD-TRANSFER-TEXT-2-ATTR                  
269900                                MOD-TRANSFER-TEXT-2-ATTR                  
270000                                MOD-KVSKROT-KVAR-ATTR                     
270100                                MOD-VALUE-OF-SCRAP-QTY-ATTR               
270200                                MOD-TISKROT-ATTR                          
270300                                MOD-KVSKROT-ATTR                          
270400                                MOD-TISKROT-AUTO-IN-ATTR                  
270410                                MOD-FLSKROT-BEORD-ATTR                    
270500     .                                                                    
270600     SKIP2                                                                
270700                                                                          
270800* --- IMS SEKTIONER ---                                                   
270900     SKIP3                                                                
271000 IMS-GET-MSG SECTION.                                                     
271100                                                                          
271200     MOVE '  QC' TO GODK-STATUSKODER                                      
271300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
271400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
271500     PERFORM IMS-STATUSKONTROLL                                           
271600     .                                                                    
271700     SKIP3                                                                
271800 IMS-INSERT-MSG SECTION.                                                  
271900                                                                          
272000     IF ENGLISH-TEXT                                                      
272100       MOVE 'N' TO MFS-KDHUVOMR                                           
272200     END-IF                                                               
272300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
272400     MOVE SPACE TO GODK-STATUSKODER                                       
272500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
272600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
272700     PERFORM IMS-STATUSKONTROLL                                           
272800     .                                                                    
272900     EJECT                                                                
273000 IMS-GU-ARTC-ARTC SECTION.                                                
273100                                                                          
273200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
273300          DELIMITED BY SIZE INTO SSA1                                     
273400     MOVE '  GE' TO GODK-STATUSKODER                                      
273500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
273600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
273700     PERFORM IMS-STATUSKONTROLL                                           
273800     .                                                                    
273900     EJECT                                                                
274000 IMS-GET-ARTC-CLAG SECTION.                                               
274100                                                                          
274200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
274300          DELIMITED BY SIZE INTO SSA1                                     
274400     STRING 'WLARTC11 '                                                   
274500          DELIMITED BY SIZE INTO SSA2                                     
274600     MOVE '    ' TO GODK-STATUSKODER                                      
274700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
274800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
274900     PERFORM IMS-STATUSKONTROLL                                           
275000     .                                                                    
275100     SKIP3                                                                
275200 IMS-GNP-CLAG SECTION.                                                    
275300                                                                          
275400     MOVE 'WLARTC11 '  TO SSA1                                            
275500     MOVE '    ' TO GODK-STATUSKODER                                      
275600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
275700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
275800     PERFORM IMS-STATUSKONTROLL                                           
275900     .                                                                    
276000     SKIP3                                                                
276100 IMS-GET-ARTC-CLAG-INFO SECTION.                                          
276200     MOVE 'GET-ARTC-CLAG-INFO ' TO FELTEXT                                
276300                                                                          
276400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
276500          DELIMITED BY SIZE INTO SSA1                                     
276600     STRING 'WLARTC11 '                                                   
276700          DELIMITED BY SIZE INTO SSA2                                     
276800     MOVE '  ' TO GODK-STATUSKODER                                        
276900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
277000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
277100     PERFORM IMS-STATUSKONTROLL                                           
277200     .                                                                    
277300     SKIP3                                                                
277400 IMS-GET-ARTS-WLARTS01 SECTION.                                           
277500                                                                          
277600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
277700          DELIMITED BY SIZE INTO SSA1                                     
277800     MOVE '  GE' TO GODK-STATUSKODER                                      
277900     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
278000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
278100     PERFORM IMS-STATUSKONTROLL                                           
278200     .                                                                    
278300     EJECT                                                                
278400 IMS-GET-ARTS-WLARTS11 SECTION.                                           
278500                                                                          
278600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
278700          DELIMITED BY SIZE INTO SSA1                                     
278800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
278900          DELIMITED BY SIZE INTO SSA2                                     
279000     MOVE '  GE' TO GODK-STATUSKODER                                      
279100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
279200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
279300     PERFORM IMS-STATUSKONTROLL                                           
279400     .                                                                    
279500     EJECT                                                                
279600 IMS-GHNP-ARTS-WLARTS11 SECTION.                                          
279700                                                                          
279800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
279900          DELIMITED BY SIZE INTO SSA1                                     
280000     MOVE '  GE' TO GODK-STATUSKODER                                      
280100     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-ARTS11 SSA1                  
280200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
280300     PERFORM IMS-STATUSKONTROLL                                           
280400     .                                                                    
280500     SKIP3                                                                
280600 IMS-GNP-ARTS-WLARTS11 SECTION.                                           
280700                                                                          
280800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
280900          DELIMITED BY SIZE INTO SSA1                                     
281000     MOVE '  GE' TO GODK-STATUSKODER                                      
281100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
281200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
281300     PERFORM IMS-STATUSKONTROLL                                           
281400     .                                                                    
281500     SKIP3                                                                
281600 IMS-REPL-WLARTS11 SECTION.                                               
281700                                                                          
281800     MOVE '  ' TO GODK-STATUSKODER                                        
281900     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
282000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
282100     PERFORM IMS-STATUSKONTROLL                                           
282200     .                                                                    
282300     EJECT                                                                
282400 IMS-GU-WDK7 SECTION.                                                     
282500                                                                          
282600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
282700          DELIMITED BY SIZE INTO SSA1                                     
282800     STRING 'WDK711  (IDDC     =' W-IDDC-TO-X ')'                         
282900          DELIMITED BY SIZE INTO SSA2                                     
283000     MOVE '  GE' TO GODK-STATUSKODER                                      
283100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
283200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
283300     PERFORM IMS-STATUSKONTROLL                                           
283400     .                                                                    
283500     EJECT                                                                
283600                                                                          
283700 IMS-GU-WDK712 SECTION.                                                   
283800                                                                          
283900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
284000          DELIMITED BY SIZE INTO SSA1                                     
284100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
284200          DELIMITED BY SIZE INTO SSA2                                     
284300     MOVE '  GE' TO GODK-STATUSKODER                                      
284400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
284500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
284600     PERFORM IMS-STATUSKONTROLL                                           
284700     .                                                                    
284800     EJECT                                                                
284900 IMS-GU-BENA-TEXT SECTION.                                                
285000     SKIP2                                                                
285100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
285200            DELIMITED BY SIZE INTO SSA1                                   
285300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
285400            DELIMITED BY SIZE INTO SSA2                                   
285500     MOVE '  GE'                TO GODK-STATUSKODER                       
285600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
285700     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
285800     PERFORM IMS-STATUSKONTROLL                                           
285900     .                                                                    
286000     EJECT                                                                
286100 IMS-GHU-ORDL-WLORDL01 SECTION.                                           
286200                                                                          
286300     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
286400          DELIMITED BY SIZE INTO SSA1                                     
286500     MOVE '  GE' TO GODK-STATUSKODER                                      
286600     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-ORDL01 SSA1                   
286700     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
286800     PERFORM IMS-STATUSKONTROLL                                           
286900     .                                                                    
287000     EJECT                                                                
287100 IMS-ISRT-ORDL-WLORDL01 SECTION.                                          
287200                                                                          
287300     MOVE 'WLORDL01 ' TO SSA1                                             
287400     MOVE '  ' TO GODK-STATUSKODER                                        
287500     CALL CBLTDLI USING ISRT ORDL-PCB DLI-IO-ORDL01 SSA1                  
287600     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
287700     PERFORM IMS-STATUSKONTROLL                                           
287800     .                                                                    
287900     SKIP3                                                                
288000 IMS-REPL-ORDL-WLORDL01 SECTION.                                          
288100                                                                          
288200     MOVE '  ' TO GODK-STATUSKODER                                        
288300     CALL CBLTDLI USING REPL ORDL-PCB DLI-IO-ORDL01                       
288400     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
288500     PERFORM IMS-STATUSKONTROLL                                           
288600     .                                                                    
288700     EJECT                                                                
288800 IMS-ISRT-WDR501-6321 SECTION.                                            
288900                                                                          
289000     MOVE 'WDR501   '           TO SSA1                                   
289100     MOVE '  II'                TO GODK-STATUSKODER                       
289200     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
289300     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
289400     PERFORM IMS-STATUSKONTROLL                                           
289500     SKIP3                                                                
289600     .                                                                    
289700 IMS-ISRT-WDGX6322 SECTION.                                               
289800                                                                          
289900     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
290000            DELIMITED BY SIZE INTO SSA1                                   
290100     MOVE 'WDGX6322 '           TO SSA2                                   
290200     MOVE '  II'                TO GODK-STATUSKODER                       
290300     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
290400     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
290500     PERFORM IMS-STATUSKONTROLL                                           
290600     SKIP3                                                                
290700     .                                                                    
290800 IMS-ISRT-WDGX6324 SECTION.                                               
290900                                                                          
291000     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
291100            DELIMITED BY SIZE INTO SSA1                                   
291200     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
291300            DELIMITED BY SIZE INTO SSA2                                   
291400     MOVE 'WDGX6324'            TO SSA3                                   
291500     MOVE '  '                  TO GODK-STATUSKODER                       
291600     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
291700                                      SSA1 SSA2 SSA3                      
291800     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
291900     PERFORM IMS-STATUSKONTROLL                                           
292000     SKIP3                                                                
292100     .                                                                    
292200     EJECT                                                                
292300 IMS-GHU-WDL7-OIGA11 SECTION.                                             
292400                                                                          
292500     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
292600          DELIMITED BY SIZE INTO SSA1                                     
292700     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
292800          DELIMITED BY SIZE INTO SSA2                                     
292900     MOVE '  GE' TO GODK-STATUSKODER                                      
293000     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-AREA-OIGA11 SSA1 SSA2         
293100     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
293200     PERFORM IMS-STATUSKONTROLL                                           
293300     .                                                                    
293400     SKIP3                                                                
293500 IMS-REPL-WDL7-OIGA11 SECTION.                                            
293600                                                                          
293700     MOVE '  ' TO GODK-STATUSKODER                                        
293800     CALL CBLTDLI USING REPL OIGA-PCB DLI-IO-AREA-OIGA11                  
293900     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
294000     PERFORM IMS-STATUSKONTROLL                                           
294100     .                                                                    
294200     EJECT                                                                
294300                                                                          
294400 IMS-GU-WDB601    SECTION.                                                
294500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
294600          DELIMITED BY SIZE INTO SSA1                                     
294700     MOVE '  GE' TO GODK-STATUSKODER                                      
294800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
294900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
295000     PERFORM IMS-STATUSKONTROLL                                           
295100     IF SEGMENT-SAKNAS                                                    
295200         MOVE SPACE TO DCS-KDDC                                           
295300     END-IF                                                               
295400     .                                                                    
295500     EJECT                                                                
295600 IMS-GET-WDR501-6327 SECTION.                                             
295700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
295800          DELIMITED BY SIZE INTO SSA1                                     
295900     MOVE '  GE' TO GODK-STATUSKODER                                      
296000     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
296100     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
296200     PERFORM IMS-STATUSKONTROLL                                           
296300     .                                                                    
296400     SKIP3                                                                
296500 IMS-GET-WDGX6328 SECTION.                                                
296600     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
296700          DELIMITED BY SIZE INTO SSA1                                     
296800     MOVE '  GE' TO GODK-STATUSKODER                                      
296900     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
297000     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
297100     PERFORM IMS-STATUSKONTROLL                                           
297200     .                                                                    
297300     EJECT                                                                
297400 IMS-GU-WDN601 SECTION.                                                   
297500     SKIP2                                                                
297600     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
297700          DELIMITED BY SIZE INTO SSA1                                     
297800     MOVE '  GE' TO GODK-STATUSKODER                                      
297900     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
298000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
298100     PERFORM IMS-STATUSKONTROLL                                           
298200     .                                                                    
298300     EJECT                                                                
298400 IMS-GNP-WDN611 SECTION.                                                  
298500     SKIP2                                                                
298600     STRING 'WDN611   '                                                   
298700          DELIMITED BY SIZE INTO SSA1                                     
298800     MOVE '  GE' TO GODK-STATUSKODER                                      
298900     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
299000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
299100     PERFORM IMS-STATUSKONTROLL                                           
299200     .                                                                    
299300     EJECT                                                                
299400 IMS-GU-WDK901 SECTION.                                                   
299500                                                                          
299600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
299700          DELIMITED BY SIZE INTO SSA1                                     
299800     MOVE '  GE' TO GODK-STATUSKODER                                      
299900     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
300000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
300100     PERFORM IMS-STATUSKONTROLL                                           
300200     .                                                                    
300300     EJECT                                                                
300400 DB2-SELECT-TP4TRAN     SECTION.                                          
300500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
300600                                                                          
300700     MOVE 000100 TO GODK-SQLCODEKODER                                     
300800                                                                          
300900     EXEC SQL                                                             
301000           SELECT  KDARBTYP                                               
301100                  ,IDDC_SEND                                              
301200                  ,IDDC_REC                                               
301300                  ,IDDISTR                                                
301400                  ,IDKUNDNR                                               
301500                                                                          
301600           INTO   :TP4TRAN-KDARBTYP                                       
301700                 ,:TP4TRAN-IDDC-SEND                                      
301800                 ,:TP4TRAN-IDDC-REC                                       
301900                 ,:TP4TRAN-IDDISTR                                        
302000                 ,:TP4TRAN-IDKUNDNR                                       
302100                                                                          
302200           FROM    TP4TRAN                                                
302300                                                                          
302400           WHERE KDARBTYP  = :WS-KDARBTYP-X3                              
302500           AND   IDDC_SEND = :WS-IDDC-SEND                                
302600           AND   IDDC_REC  = :WS-IDDC-REC                                 
302700     END-EXEC                                                             
302800                                                                          
302900     MOVE SQLCODE TO SQLCODE-WS                                           
303000     PERFORM DB2-STATUSKONTROLL                                           
303100     .                                                                    
303200     EJECT                                                                
303300 IMS-GU-WDB601-FROM    SECTION.                                           
303400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-FROM-X ')'                    
303500          DELIMITED BY SIZE INTO SSA1                                     
303600     MOVE '  GE' TO GODK-STATUSKODER                                      
303700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-FROM SSA1            
303800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
303900     PERFORM IMS-STATUSKONTROLL                                           
304000     IF SEGMENT-SAKNAS                                                    
304100         MOVE SPACE TO FROM-DCS-KDDC                                      
304200     END-IF                                                               
304300     .                                                                    
304400                                                                          
304500 IMS-GU-WDB601-TO      SECTION.                                           
304600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-TO-X ')'                      
304700          DELIMITED BY SIZE INTO SSA1                                     
304800     MOVE '  GE' TO GODK-STATUSKODER                                      
304900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-TO SSA1              
305000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
305100     PERFORM IMS-STATUSKONTROLL                                           
305200     IF SEGMENT-SAKNAS                                                    
305300         MOVE SPACE TO TO-DCS-KDDC                                        
305400     END-IF                                                               
305500     .                                                                    
305600                                                                          
305700 IMS-GU-WDB601-MSGI    SECTION.                                           
305800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-MSGI-X ')'                    
305900          DELIMITED BY SIZE INTO SSA1                                     
306000     MOVE '  GE' TO GODK-STATUSKODER                                      
306100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-MSGI SSA1            
306200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
306300     PERFORM IMS-STATUSKONTROLL                                           
306400     IF SEGMENT-SAKNAS                                                    
306500         MOVE SPACE TO MSGI-DCS-KDDC                                      
306600     END-IF                                                               
306700     .                                                                    
306800                                                                          
306900 IMS-GU-WDB601-KEY     SECTION.                                           
307000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-KEY-X ')'                     
307100          DELIMITED BY SIZE INTO SSA1                                     
307200     MOVE '  GE' TO GODK-STATUSKODER                                      
307300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-KEY SSA1             
307400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
307500     PERFORM IMS-STATUSKONTROLL                                           
307600     IF SEGMENT-SAKNAS                                                    
307700         MOVE SPACE TO KEY-DCS-KDDC                                       
307800     END-IF                                                               
307900     .                                                                    
308000                                                                          
308100 IMS-GU-WDB616 SECTION.                                                   
308200                                                                          
308300     STRING 'WDB601  (IDDC     =' W-IDDC-B601-X ')'                       
308400          DELIMITED BY SIZE INTO SSA1                                     
308500     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
308600          DELIMITED BY SIZE INTO SSA2                                     
308700     MOVE '  GE' TO GODK-STATUSKODER                                      
308800     CALL CBLTDLI USING GU WDB62-PCB DLI-IO-WDB616 SSA1 SSA2              
308900     MOVE WDB62-STATUS-CODE TO STATUS-WS                                  
309000     PERFORM IMS-STATUSKONTROLL                                           
309100     .                                                                    
309200     SKIP3                                                                
309300 IMS-STATUSKONTROLL SECTION.                                              
309400                                                                          
309500     SET STATUS-IX TO 1                                                   
309600     SEARCH GODK-STATUS                                                   
309700       AT END                                                             
309800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
309900         DELIMITED BY SIZE INTO FELTEXT                                   
310000         CALL FELLOG                                                      
310100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
310200         CONTINUE                                                         
310300     END-SEARCH                                                           
310400     .                                                                    
310500 DB2-STATUSKONTROLL  SECTION.                                             
310600                                                                          
310700     SET SQLCODE-IX TO 1                                                  
310800     SEARCH GODK-SQLCODE                                                  
310900       AT END                                                             
311000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
311100          DELIMITED BY SIZE INTO FELTEXT                                  
311200          CALL ABEND USING RKOD-ABEND-DB2                                 
311300       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
311400     END-SEARCH                                                           
311500     .                                                                    
