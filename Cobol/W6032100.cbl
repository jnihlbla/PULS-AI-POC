000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6032100.                                                
000400 AUTHOR.         PAH.                                                     
000500 DATE-WRITTEN.   98/09/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                      (DELVIS KOPIA AV W20359)              
000900*        PROGRAMMET ANVÄNDS                                               
001000*        FÖR ATT BEORDRA SKROTNING HOS SDC/NDC:ERNA                       
001100*                                                                         
001200** BORTTAG ÄR INTE MÖJLIGT, MAN LÄGGER NOLL I ANTAL I STÄLLET.            
001300**                                                                        
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001600*                              WLARTS (WDK7)                              
001700*                              WL6321 (WDR5)                              
001800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001900*                              WL4109 (WDGX)                              
002000*                                      WDN6                               
002100*                                      WDK9                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T321                                              
002500*        MID:         W6I32101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O32101                                            
002900*                                                                         
003000*                                                                         
003100*    2011-07-12  SO   e'TRACKER 9399577 Destocking - scrap 2              
003200*                                                                         
003300*    2011-10-25  e'TRACKER 10143271 CHINA  WAREHOUSE PROJECT-1            
003400*                                                                         
003500*    2018-03-29  JIRA 2282 UPDATE NOLLOR VALIDATION FOR COBOL 5           
003600*                                                                         
003700*                                                                         
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400*    --- CHECKED BY WY2000                                                
004500 77  IDPGM                       PIC X(08)   VALUE 'W6032100'.            
004600                                                                          
004700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004900                                                                          
005000 77  JA                          PIC X     VALUE 'J'.                     
005100 77  YES                         PIC X     VALUE 'Y'.                     
005200 77  NEJ                         PIC X     VALUE 'N'.                     
005300 77  W-IDFTG-B6                  PIC 9(2)  VALUE ZERO.                    
005310 77  SPR                         PIC S9    VALUE +1 COMP-3.               
005400 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
005500 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
005600                                                                          
005700 77  INDX                        PIC S9(3) VALUE ZERO.                    
005800 77  IX-DC                       PIC S9(3) COMP-3 VALUE ZERO.             
005900 77  IX-DISTR                    PIC S9(3) COMP-3 VALUE ZERO.             
006000 77  IX                          PIC 9(3)  VALUE ZERO.                    
006100 77  IX2                         PIC 9(3)  VALUE ZERO.                    
006200 77  IX-VV                       PIC 9(2)  VALUE ZERO.                    
006300 77  BEEMB-IX                    PIC 9(3)  VALUE ZERO.                    
006400 77  W-ANTAL-SKROT               PIC S9(7) COMP-3 VALUE ZERO.             
006500 77  W-KVRETUR-BEORD             PIC S9(7) COMP-3 VALUE ZERO.             
006600 77  WS-OSPARRAT-ANTAL           PIC S9(7) COMP-3 VALUE ZERO.             
006700 77  WS-SPARA-IDPERSON-BUY       PIC 9(3)         VALUE ZERO.             
006800 77  W-KVRETUR-BEORD-TRUNK       PIC 9(6)  VALUE ZERO.                    
006900 77  W-SCRAPPED-VALUE            PIC S9(7)V99 COMP-3 VALUE ZERO.          
007000 77  W-PRARTSTD                  PIC S9(7)V99 COMP-3 VALUE ZERO.          
007100 77  W-KVSKROT                   PIC S9(7) COMP-3 VALUE ZERO.             
007200 77  W-KVSKROT-DISPLAY           PIC 9(7)  VALUE ZERO.                    
007300 77  W-KVSKROT-KVAR              PIC 9(7)  VALUE ZERO.                    
007400 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
007500 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
007600 77  W-KVSKROT-TRUNK             PIC 9(6)  VALUE ZERO.                    
007700 77  W-KVSTOCK                   PIC 9(7)  VALUE ZERO.                    
007800 77  FILLER                      PIC X     VALUE '-'.                     
007900 77  NOLLOR                      PIC 9     VALUE ZERO.                    
008000 77  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
008100 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
008200 77  FILLER                      PIC X     VALUE '+'.                     
008300 77  DAGENS-TID                  PIC 9(8)  VALUE ZERO.                    
008400 77  WS-SPAR-BEANST-GODK         PIC X(25) VALUE SPACE.                   
008500 77  W-IDDC-FROM                 PIC X(2)  VALUE SPACE.                   
008600 77  WS-KVOKS-CDC                PIC S9(6) VALUE ZERO.                    
008700 77  SW-LOKAL-ART                PIC X     VALUE 'N'.                     
008800                                                                          
008900 01  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.                  
009000 01  W-SDC-KVAKS         PIC S9(7)    VALUE ZERO COMP-3.                  
009100 01  W-SDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.                  
009200                                                                          
009300 01  DAGENS-TID-I-DELAR.                                                  
009400     03  FILLER                  PIC 9(1).                                
009500     03  DAGENS-TIT              PIC 9(1).                                
009600     03  DAGENS-TIMM             PIC 9(2).                                
009700     03  FILLER                  PIC 9(4).                                
009800 01  DAGENS-AAVVD.                                                        
009900     03  FILLER                  PIC 9(3).                                
010000     03  DAGENS-VECKA            PIC 9(1).                                
010100     03  DAGENS-DAG              PIC 9(1).                                
010200                                                                          
010300 01 NYCKLAR-TP4TRAN.                                                      
010400     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
010500     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
010600                                                                          
010700 01  WS-IDPERSON                 PIC S9(3) COMP-3 VALUE ZERO.             
010800 01  WS-IDPERSON-BUY             PIC S9(3) COMP-3 VALUE ZERO.             
010900 01  W-IDLEVNR                   PIC X(5)  VALUE SPACE.                   
011000 01  W-KDERS                     PIC S9(3) COMP-3 VALUE ZERO.             
011100 01  W-IDANSK                    PIC S9(3) COMP-3 VALUE ZERO.             
011200 01  WS-KDARBTYP                 PIC X(8)  VALUE SPACE.                   
011300                                                                          
011400 01  W-RETUR-TEXT.                                                        
011500     03  W-RETUR-TEXT-1          PIC X(30) VALUE SPACE.                   
011600     03  W-RETUR-TEXT-2          PIC X(30) VALUE SPACE.                   
011700                                                                          
011800 01  W-TRANSFER-DEFAULT-TEXT     PIC X(40) VALUE                          
011900     'PLEASE INFORM VCAS OF ANY DISCREPANCIES'.                           
012000                                                                          
012100 01  W-TRANSFER-DEFAULT-TEXT-RAD PIC X(10) VALUE                          
012200     'PICK TOTAL'.                                                        
012300                                                                          
012400 01  W-SKROT-TEXT-DEFAULT.                                                
012500     03 FILLER                      PIC X(40) VALUE                       
012600     'REMAINING PIECES; SEE LINE REFERENCE.'.                             
012700     03 W-SKROT-TEXT                PIC X(20) VALUE SPACE.                
012800                                                                          
012900 01  W-SKROT-TEXT-ORAD.                                                   
013000     03 W-KVSKROT-ORAD           PIC X(7)  VALUE SPACE.                   
013100     03 FILLER                   PIC X(3)  VALUE 'PCS'.                   
013200                                                                          
013300 01  MEDDELANDEN.                                                         
013400     03 FEL-1-AREA.                                                       
013500        05 FILLER                PIC X(49)                                
013600           VALUE 'ARTIKEL SAKNAS PÅ DETTA DC                  '.          
013700        05 FILLER                PIC X(49)                                
013800           VALUE 'PART MISSING ON THIS DC                     '.          
013900     03 FILLER REDEFINES FEL-1-AREA.                                      
014000        05 FEL-1 OCCURS 2        PIC X(49).                               
014100                                                                          
014200     03 MED-1-AREA.                                                       
014300        05 FILLER                PIC X(49)                                
014400           VALUE 'ANALYSNUMMER SAKNAS.  GÅ TILL BILD 4702     '.          
014500        05 FILLER                PIC X(49)                                
014600           VALUE 'ANALYSIS NUMBER MISSING.  GO TO SCREEN 4702 '.          
014700     03 FILLER REDEFINES MED-1-AREA.                                      
014800        05 MED-1 OCCURS 2        PIC X(49).                               
014900                                                                          
015000     03 MED-2-AREA.                                                       
015100        05 FILLER                PIC X(25)                                
015200           VALUE 'FEL ORDERKLASS      '.                                  
015300        05 FILLER                PIC X(25)                                
015400           VALUE 'WRONG ORDER CLASS   '.                                  
015500     03 FILLER REDEFINES MED-2-AREA.                                      
015600        05 MED-2 OCCURS 2        PIC X(25).                               
015700                                                                          
015800     03 MED-3-AREA.                                                       
015900        05 FILLER                PIC X(25)                                
016000           VALUE 'FEL FRAKTKOD        '.                                  
016100        05 FILLER                PIC X(25)                                
016200           VALUE 'WRONG FREIGHT CODE  '.                                  
016300     03 FILLER REDEFINES MED-3-AREA.                                      
016400        05 MED-3 OCCURS 2        PIC X(25).                               
016500                                                                          
016600     03 MED-4-AREA.                                                       
016700        05 FILLER                PIC X(35)                                
016800           VALUE 'CDC ARTIKEL, LOCAL BUYER      '.                        
016900        05 FILLER                PIC X(35)                                
017000           VALUE 'CDC PART, LOCAL BUYER         '.                        
017100     03 FILLER REDEFINES MED-4-AREA.                                      
017200        05 MED-4 OCCURS 2        PIC X(35).                               
017300                                                                          
017400     03 MED-5-AREA.                                                       
017500        05 FILLER                PIC X(35)                                
017600           VALUE 'FORTFARANDE KVALITETSSPÄRRAD   '.                       
017700        05 FILLER                PIC X(35)                                
017800           VALUE 'QUALITY BLOCKED QTY STILL EXIST'.                       
017900     03 FILLER REDEFINES MED-5-AREA.                                      
018000        05 MED-5 OCCURS 2        PIC X(35).                               
018100                                                                          
018200     03 MED-6-AREA.                                                       
018300        05 FILLER                PIC X(25)                                
018400           VALUE '*** KVALITETSPÄRR ANTAL ='.                             
018500        05 FILLER                PIC X(25)                                
018600           VALUE '*** QUALITY BLOCKED QTY ='.                             
018700     03 FILLER REDEFINES MED-6-AREA.                                      
018800        05 MED-6 OCCURS 2        PIC X(25).                               
018900                                                                          
019000     03 MED-7-AREA.                                                       
019100        05 FILLER                PIC X(27)                                
019200           VALUE '*** TOTALT KVALITETSSPÄRRAD'.                           
019300        05 FILLER                PIC X(27)                                
019400           VALUE '*** TOTALY QUALITY BLOCKED '.                           
019500     03 FILLER REDEFINES MED-7-AREA.                                      
019600        05 MED-7 OCCURS 2        PIC X(27).                               
019700                                                                          
019800     03 MED-8-AREA.                                                       
019900        05 FILLER                PIC X(27)                                
020000           VALUE '*** ORDERSPÄRRAD           '.                           
020100        05 FILLER                PIC X(27)                                
020200           VALUE '*** ORDER BLOCKED          '.                           
020300     03 FILLER REDEFINES MED-8-AREA.                                      
020400        05 MED-8 OCCURS 2        PIC X(27).                               
020500                                                                          
020600     03 MED-9-AREA.                                                       
020700        05 FILLER                PIC X(35)                                
020800           VALUE 'USA ARTIKEL, CDC BUYER     '.                           
020900        05 FILLER                PIC X(35)                                
021000           VALUE 'LOCAL PART, CDC BUYER      '.                           
021100     03 FILLER REDEFINES MED-9-AREA.                                      
021200        05 MED-9 OCCURS 2        PIC X(35).                               
021300                                                                          
021400     03 MED-10-AREA.                                                      
021500        05 FILLER                PIC X(35)                                
021600           VALUE 'LOKAL PART, RETUR EJ TILLÅTEN'.                         
021700        05 FILLER                PIC X(35)                                
021800           VALUE 'LOCAL PART, NOT TO BE RETURNED'.                        
021900     03 FILLER REDEFINES MED-10-AREA.                                     
022000        05 MED-10 OCCURS 2        PIC X(35).                              
022100                                                                          
022200     03 MED-11-AREA.                                                      
022300        05 FILLER                PIC X(50)                                
022400           VALUE '        KONTO ANALYSNR, FÅR EJ UPPDATERAS HÄR'.         
022500        05 FILLER                PIC X(50)                                
022600           VALUE '           ACCOUNT ANALYSNO, MUST NOT BE USED'.         
022700     03 FILLER REDEFINES MED-11-AREA.                                     
022800        05 MED-11 OCCURS 2        PIC X(50).                              
022900                                                                          
023000     03 MED-12-AREA.                                                      
023100        05 FILLER                PIC X(50)                                
023200           VALUE 'EJ TILLÅTET ATT UPPD BÅDE SKROT OCH RETUR    '.         
023300        05 FILLER                PIC X(50)                                
023400           VALUE 'SCRAP AND RETURN AT THE SAME TIME NOT PERM.  '.         
023500     03 FILLER REDEFINES MED-12-AREA.                                     
023600        05 MED-12 OCCURS 2        PIC X(50).                              
023700                                                                          
023800     03 MED-13-AREA.                                                      
023900        05 FILLER                PIC X(50)                                
024000           VALUE 'KVANTITET EJ NUMERISKT                       '.         
024100        05 FILLER                PIC X(50)                                
024200           VALUE 'QUANTITY NOT NUMERIC                         '.         
024300     03 FILLER REDEFINES MED-13-AREA.                                     
024400        05 MED-13 OCCURS 2        PIC X(50).                              
024500                                                                          
024600     03 MED-14-AREA.                                                      
024700        05 FILLER                PIC X(50)                                
024800           VALUE 'ARTNR + DC SAKNAS PÅ SDC/NDC-BAS (K7)        '.         
024900        05 FILLER                PIC X(50)                                
025000           VALUE 'PART NO + DC MISSING                         '.         
025100     03 FILLER REDEFINES MED-14-AREA.                                     
025200        05 MED-14 OCCURS 2        PIC X(50).                              
025300                                                                          
025400     03 MED-15-AREA.                                                      
025500        05 FILLER                PIC X(50)                                
025600           VALUE 'KVANT RETUR > LAGERSALDO                     '.         
025700        05 FILLER                PIC X(50)                                
025800           VALUE 'RETURN QTY > STOCK BALANCE                   '.         
025900     03 FILLER REDEFINES MED-15-AREA.                                     
026000        05 MED-15 OCCURS 2        PIC X(50).                              
026100                                                                          
026200     03 MED-16-AREA.                                                      
026300        05 FILLER                PIC X(50)                                
026400           VALUE 'QUAL ELLER ESC MÅSTE VARA IFYLLD (NUMERISKT) '.         
026500        05 FILLER                PIC X(50)                                
026600           VALUE 'QUAL OR ESC  MUST EXIST (NUMERIC)            '.         
026700     03 FILLER REDEFINES MED-16-AREA.                                     
026800        05 MED-16 OCCURS 2        PIC X(50).                              
026900                                                                          
027000     03 MED-18-AREA.                                                      
027100        05 FILLER                PIC X(50)                                
027200           VALUE 'KONTONUMMER    SKALL VARA NUMERISKT          '.         
027300        05 FILLER                PIC X(50)                                
027400           VALUE 'ACCOUNT NO   MUST BE NUMERIC                 '.         
027500     03 FILLER REDEFINES MED-18-AREA.                                     
027600        05 MED-18 OCCURS 2        PIC X(50).                              
027700                                                                          
027800     03 MED-19-AREA.                                                      
027900        05 FILLER                PIC X(50)                                
028000           VALUE 'KONTONR, ANALYSNR       BÄGGE ELLER  INGET   '.         
028100        05 FILLER                PIC X(50)                                
028200           VALUE 'ACCUUNT, ANALYSIS NO    BOTH OR NONE         '.         
028300     03 FILLER REDEFINES MED-19-AREA.                                     
028400        05 MED-19 OCCURS 2        PIC X(50).                              
028500                                                                          
028600     03 MED-20-AREA.                                                      
028700        05 FILLER                PIC X(50)                                
028800           VALUE 'SKROTNING PÅGÅR                              '.         
028900        05 FILLER                PIC X(50)                                
029000           VALUE 'SCRAP ALREADY IN USE                         '.         
029100     03 FILLER REDEFINES MED-20-AREA.                                     
029200        05 MED-20 OCCURS 2        PIC X(50).                              
029300                                                                          
029400     03 MED-21-AREA.                                                      
029500        05 FILLER                PIC X(50)                                
029600           VALUE 'KVANT SKROT > LAGERSALDO                     '.         
029700        05 FILLER                PIC X(50)                                
029800           VALUE 'SCRAP  QTY > STOCK BALANCE                   '.         
029900     03 FILLER REDEFINES MED-21-AREA.                                     
030000        05 MED-21 OCCURS 2        PIC X(50).                              
030100                                                                          
030200     03 MED-22-AREA.                                                      
030300        05 FILLER                PIC X(50)                                
030400           VALUE 'QUAL / ESC FINNS EJ                          '.         
030500        05 FILLER                PIC X(50)                                
030600           VALUE 'QUAL / ESC   DO NOT EXIST                    '.         
030700     03 FILLER REDEFINES MED-22-AREA.                                     
030800        05 MED-22 OCCURS 2        PIC X(50).                              
030900*      --- VALID IDDC CODES                                               
031000*                                                                         
031100*01    -COPY WWDC99 -PRE TO-                                              
031200     EJECT                                                                
031300                                                                          
031400*01    -COPY WWDC99 -PRE FROM-                                            
031500     EJECT                                                                
031600                                                                          
031700*01  -COPY WWIDFTG                                                        
031800     EJECT                                                                
031900                                                                          
032000*01  -COPY WWPRODSL                                                       
032100     EJECT                                                                
032200                                                                          
032300 77  SPARA-IDDISTR               PIC X(4)  VALUE SPACE.                   
032400 77  SPARA-IDKUNDNR              PIC X(6)  VALUE SPACE.                   
032500 77  SPARA-IDORDNR               PIC X(7)  VALUE SPACE.                   
032600 77  SPARA-IDDISTR-NUM           PIC 9(4)  VALUE ZERO.                    
032700 77  SPARA-IDKUNDNR-NUM          PIC 9(6)  VALUE ZERO.                    
032800 77  WS-IDKUNDNR                 PIC 9(6)  VALUE ZERO.                    
032900                                                                          
033000*    ARBETSFÄLT FÖR KONTROLL AV MULTIPEL AV KVQPACK                       
033100 77  WS-KVQPACK-ANT              PIC S9(9)   VALUE ZERO.                  
033200 77  WS-KVQPACK-REST             PIC S9V9(9) VALUE ZERO.                  
033300*    ARBETSFÄLT FÖR INFORMATION OM KVALITETSSPÄRR                         
033400 77  WS-KVSPARR-KVAL             PIC Z(6)9 VALUE ZERO.                    
033500                                                                          
033600 01  FILLER       PIC X(24)         VALUE  'W-ORDERNUMMER'.               
033700 01  W-ORDERNR.                                                           
033800     03  W-ORDER-TIMM            PIC 9(2)  VALUE ZERO.                    
033900     03  W-ORDER-TIT             PIC 9(1)  VALUE ZERO.                    
034000     03  W-ORDER-DAG             PIC 9(1)  VALUE ZERO.                    
034100     03  W-ORDER-V               PIC 9(1)  VALUE ZERO.                    
034200                                                                          
034300 01  FILLER       PIC X(24)         VALUE  'W-ORDERNUMMER-7POS'.          
034400 01  W-ORDERNR-7POS.                                                      
034500     03  FILLER                  PIC 9(2)  VALUE ZERO.                    
034600     03  W-ORDERNR-NUM           PIC 9(5)  VALUE ZERO.                    
034700                                                                          
034800 01  FILLER       PIC X(24)      VALUE  'MOTTAGANDE-LAGER-ADRESS'.        
034900 01  MOTTAGANDE-LAGER-ADRESS.                                             
035000     03 WS-ADART-X.                                                       
035100        05 WS-ADLAGOMR-X    PIC  X(2)     VALUE SPACE.                    
035200        05 WS-ADGANG-X      PIC  X(2)     VALUE SPACE.                    
035300        05 WS-ADPLATS-X     PIC  X(5)     VALUE SPACE.                    
035400        05 FILLER           PIC  X(1)     VALUE SPACE.                    
035500       EJECT                                                              
035600                                                                          
035700 01 DB2-LASNING.                                                          
035800     03 FILLER                   PIC X(16)   VALUE                        
035900                                             'WS-DB2-SEKTION'.            
036000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
036100                                                                          
036200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
036300                                                                          
036400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
036500     88  INDATA-OK                           VALUE 'J'.                   
036600     88  INDATA-FEL                          VALUE 'N'.                   
036700                                                                          
036800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
036900     88  NYCKLAR-OK                          VALUE 'J'.                   
037000     88  NYCKLAR-FEL                         VALUE 'N'.                   
037100                                                                          
037200 77  UPDKEY-SW                   PIC X       VALUE 'J'.                   
037300     88  UPDKEY-OK                           VALUE 'J'.                   
037400     88  UPDKEY-FEL                          VALUE 'N'.                   
037500                                                                          
037600 77  RETUR-IFYLLT-SW             PIC X       VALUE 'N'.                   
037700     88  RETUR-IFYLLT                        VALUE 'J'.                   
037800                                                                          
037900 77  SKROT-IFYLLT-SW             PIC X       VALUE 'N'.                   
038000     88  SKROT-IFYLLT                        VALUE 'J'.                   
038100                                                                          
038200 77  IFYLLD-KONTORAD-SW          PIC X       VALUE 'J'.                   
038300     88  IFYLLD-KONTORAD                     VALUE 'J'.                   
038400     88  TOM-KONTORAD                        VALUE 'N'.                   
038500                                                                          
038600 77  GILTIGT-ANALYSNR-SW         PIC X       VALUE 'J'.                   
038700     88  GILTIGT-ANALYSNR                    VALUE 'J'.                   
038800     88  OGILTIGT-ANALYSNR                   VALUE 'N'.                   
038900                                                                          
039000 77  ORDER-FINNS-SW              PIC X       VALUE 'N'.                   
039100     88  ORDER-FINNS                         VALUE 'J'.                   
039200                                                                          
039300 77  SPARR-ANT-SW                PIC X       VALUE 'N'.                   
039400     88  SPARR-ANT-KVAR                      VALUE 'J'.                   
039500                                                                          
039600 77  ORDERSPARRAD-SW             PIC X       VALUE 'N'.                   
039700     88  ORDERSPARR-ARTIKEL                  VALUE 'J'.                   
039800                                                                          
039900 77  NOSPLITBULK-SW              PIC X       VALUE 'N'.                   
040000     88  NOSPLITBULK                         VALUE 'J'.                   
040100                                                                          
040200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
040300     88  EGEN-MID                            VALUE '6321'.                
040400     88  GODK-MID                            VALUE '2351' '2352'          
040500                                                   '2353' '2354'          
040600                                                   '2355' '2356'          
040700                                                   '2357' '2358'          
040800                                                   '2359' '6321'          
040900                                                   '6322' '6324'.         
041000     88  HELP-MID                            VALUE '0551'.                
041100     EJECT                                                                
041200 01  FILLER                    PIC X(16) VALUE 'TEST-IDDISTRIKT '.        
041300                                                                          
041400 01  TEST-IDDISTR              PIC 9(5)   COMP-3.                         
041500*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
041600                                                                          
041700     EJECT                                                                
041800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
041900 01  GENERELLA-SUBPROGRAM.                                                
042000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
042100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
042200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
042300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
042400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
042500     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
042600     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
042700     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
042800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
042900     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
043000     EJECT                                                                
043100 01  RETURKOD-ABEND.                                                      
043200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16 COMP SYNC.         
043300 01  KONTROLL-SIFFRA.                                                     
043400   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
043500   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
043600   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
043700     EJECT                                                                
043800 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
043900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
044000*01 -COPY WMEDAREA                                                        
044100     EJECT                                                                
044200 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
044300*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
044400*01 -COPY WDATAREA                                                        
044500     EJECT                                                                
044600 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
044700*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
044800*01 -COPY W411ORDN                                                        
044900     EJECT                                                                
045000 01  FILLER                      PIC X(16)   VALUE 'W411SAP '.            
045100     SKIP3                                                                
045200*01 -COPY W411SAP                                                         
045300     EJECT                                                                
045400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
045500*    --- PARAMETRAR TILL SUBPROGRAM WMSGINIT                              
045600*01 -COPY WMSGINIT                                                        
045700     EJECT                                                                
045800 01  MESSAGE-CODES.                                                       
045900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
046000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
046100     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
046200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
046300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
046400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
046500     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
046600     03  ERR-WRONG-FREIGHT-CODE  PIC X(3)    VALUE '086'.                 
046700     03  ERR-ORDER-NOT-AVAILABLE PIC X(3)    VALUE '248'.                 
046800     03  ERR-NO-SPLIT-BULK-PACK  PIC X(3)    VALUE '419'.                 
046900     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
047000     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
047100     03  PART-DIR-DEL            PIC X(3)    VALUE '306'.                 
047200     03  NOT-AUTHORIZED-TO-SCRAP PIC X(3)    VALUE '601'.                 
047300     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
047400     03  ERR-NO-REFILL-PART      PIC X(3)    VALUE '957'.                 
047500     03  ERR-KUNDUPPG-SAKNAS     PIC X(3)    VALUE '063'.                 
047600     EJECT                                                                
047700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
047800*                                                                         
047900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
048000     SKIP3                                                                
048100*01  MID -COPY W6I32101                                                   
048200     EJECT                                                                
048300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
048400     SKIP3                                                                
048500*01  -COPY WMSGAREA                                                       
048600     EJECT                                                                
048700     03  MOD REDEFINES MSG-AREA.                                          
048800*      05  -COPY W6O32101                                                 
048900     EJECT                                                                
049000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
049100     SKIP3                                                                
049200*01  -COPY WMFSAREA                                                       
049300     EJECT                                                                
049400 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
049500*01  -COPY WMSGKOM                                                        
049600     EJECT                                                                
049700                                                                          
049800 01  P-TO-P-SW.                                                           
049900   03  P-TO-P-KVLL           PIC S9(4)   COMP SYNC.                       
050000   03  P-TO-P-KDZ1           PIC X(1)    VALUE LOW-VALUE.                 
050100   03  P-TO-P-KDZ2           PIC X(1)    VALUE LOW-VALUE.                 
050200   03  P-TO-P-KDTRANS        PIC X(8).                                    
050300   03  P-TO-P-IDTRANS        PIC X(4).                                    
050400   03  P-TO-P-KDMFSFOR       PIC X(1).                                    
050500   03  P-TO-P-DATA           PIC X(1000).                                 
050600     EJECT                                                                
050700*                                                                         
050800*    --- AREOR FÖR W006KOM SUBMODUL                                       
050900*                                                                         
051000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
051100 01  KOM-IO-AREA.                                                         
051200   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
051300*03  FILLER  -COPY W4I25101 -PRE OHUV-  -RED KOM-AREA.                    
051400     EJECT                                                                
051500*03  FILLER  -COPY W4I25201 -PRE ORAD-  -RED KOM-AREA.                    
051600     EJECT                                                                
051700                                                                          
051800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
051900*                                                                         
052000                                                                          
052100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
052200     SKIP2                                                                
052300 01  NYCKLAR-TILL-DLI.                                                    
052400     03  W-IDARTNR-X.                                                     
052500         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
052600     03  W-KDSEGKEY-X.                                                    
052700         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
052800     03  W-IDDC-X.                                                        
052900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
053000     03  W-IDDC-B6-X.                                                     
053100         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
053300     03  W-IDDC-MIN-X.                                                    
053400         05  W-IDDC-MIN          PIC X(2)    VALUE LOW-VALUE.             
053500     03  W-IDDC-MAX-X.                                                    
053600         05  W-IDDC-MAX          PIC X(2)    VALUE HIGH-VALUE.            
053700                                                                          
053800     03  W-IDDC-REF-X.                                                    
053900         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
054000                                                                          
054100     03  W-WDGXKEY-ROT-X.                                                 
054200         05  FILLER              PIC X(04)   VALUE '4109'.                
054300         05  W-IDFTG-4109        PIC 9(02)   VALUE ZERO.                  
054400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
054500     03  W-KEY4110-MIN-X.                                                 
054600         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
054700         05  FILLER              PIC X(10)  VALUE LOW-VALUE.              
054800     03  W-KEY4110-MAX-X.                                                 
054900         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
055000         05  FILLER              PIC X(10)  VALUE HIGH-VALUE.             
055100     03  W-WDGX6321-ROT-X.                                                
055200         05  FILLER              PIC X(04)   VALUE '6321'.                
055300         05  W-KDARBTYP-6321     PIC X(08)   VALUE '        '.            
055400         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
055500     03  W-WDGX6322-KEY-X.                                                
055600         05  W-DASKROT9-BEORD    PIC 9(08)   VALUE ZERO.                  
055700     03  W-WDGX6324-KEY-X.                                                
055800         05  W-IDARTNR-6324      PIC S9(9)  COMP-3 VALUE ZERO.            
055900         05  W-IDDC-6324         PIC X(2)   VALUE SPACE.                  
056000         05  W-KDSTASKR-6324     PIC S9     COMP-3 VALUE ZERO.            
056100                                                                          
056200     03  W-IDSKYLT-X.                                                     
056300         05  W-IDSKYLT           PIC X(03)    VALUE SPACE.                
056400     03  W-WDE301KY-X.                                                    
056500         05  W-IDDC-301          PIC X(2)          VALUE SPACE.           
056600         05  W-IDPERSON-BUY      PIC S9(3)  COMP-3 VALUE +0.              
056700         05  W-KDREFTYP          PIC X             VALUE 'R'.             
056800         05  W-IDARTNR-301       PIC S9(9)  COMP-3 VALUE +0.              
056900         05  W-IDDISTR-301       PIC S9(5)  COMP-3 VALUE +0.              
057000     03  W-KDARBTYP-X.                                                    
057100         05  W-KDARBTYP          PIC X(8)    VALUE 'XXXX'.                
057200     03  W-IDPERSON-X.                                                    
057300         05  W-IDPERSON          PIC S9(3)   COMP-3 VALUE +0.             
057400     03  W-WDGXKEY-X.                                                     
057500         05  FILLER             PIC X(4)    VALUE '6327'.                 
057600         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
057700         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
057800         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
057900     03  W-IDUSER-GODK-X.                                                 
058000         05  W-IDUSER-GODK      PIC X(8)       VALUE SPACE.               
058100     SKIP2                                                                
058200*    --- STATUS-KOD FRÅN IMS                                              
058300 01  STATUS-WS                   PIC XX.                                  
058400     88  SEGMENT-FINNS                       VALUE '  '.                  
058500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
058600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
058700     SKIP2                                                                
058800 01  GODK-STATUSKODER.                                                    
058900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
059000     SKIP3                                                                
059100 01  SSA1                        PIC X(128).                              
059200 01  SSA2                        PIC X(64).                               
059300 01  SSA3                        PIC X(64).                               
059400     EJECT                                                                
059500                                                                          
059600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
059700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
059800                                                                          
059900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
060000 01  DB2-WS.                                                              
060100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
060200         88  CURSOR-OK                       VALUE 000.                   
060300         88  RADER-FINNS                     VALUE 000.                   
060400         88  RADER-SAKNAS                    VALUE 100.                   
060500         88  ATKOMST-FEL                     VALUE 904.                   
060600     03  GODK-SQLCODEKODER.                                               
060700         05  GODK-SQLCODE OCCURS 5                                        
060800             INDEXED BY SQLCODE-IX PIC 9(3).                              
060900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
061000     EJECT                                                                
061100*    --- IMS FUNKTIONSKODER                                               
061200*01  -COPY W0003                                                          
061300     EJECT                                                                
061400*    ---  DLI INPUT-OUTPUT AREA                                           
061500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
061600 01  DLI-IO-ARTC01.                                                       
061700*    03  -COPY WDK601     -PRE ARTC-                                      
061800     EJECT                                                                
061900                                                                          
062000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
062100 01  DLI-IO-ARTC11.                                                       
062200*    03  -COPY WDK611                                                     
062300     EJECT                                                                
062400                                                                          
062500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
062600 01  DLI-IO-WDK629.                                                       
062700*    03  -COPY WDK629                                                     
062800     EJECT                                                                
062900                                                                          
063000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
063100 01  DLI-IO-ARTS01.                                                       
063200*    03  -COPY WDK701                                                     
063300     EJECT                                                                
063400                                                                          
063500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
063600 01  DLI-IO-ARTS11.                                                       
063700*    03  -COPY WDK711                                                     
063800     EJECT                                                                
063900 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
064000 01  DLI-IO-BENA11.                                                       
064100*    03  -COPY WDD311                                                     
064200     EJECT                                                                
064300                                                                          
064400 01  FILLER         PIC X(24) VALUE 'DLI-IO-410901'.                      
064500 01  DLI-IO-411001.                                                       
064600*    03  -COPY WDGX01                                                     
064700     EJECT                                                                
064800                                                                          
064900 01  FILLER         PIC X(24) VALUE 'DLI-IO-410911'.                      
065000 01  DLI-IO-411011.                                                       
065100*    03  -COPY WDGX4110                                                   
065200     EJECT                                                                
065300                                                                          
065400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORDL01'.                      
065500 01  DLI-IO-ORDL01.                                                       
065600*    03  -COPY WDE301                                                     
065700     EJECT                                                                
065800                                                                          
065900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
066000 01  DLI-IO-WDR501-6321.                                                  
066100     03  -COPY WDGX6321                                                   
066200     EJECT                                                                
066300                                                                          
066400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6322'.                    
066500 01  DLI-IO-WDGX6322.                                                     
066600     03  -COPY WDGX6322                                                   
066700     EJECT                                                                
066800                                                                          
066900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6324'.                    
067000 01  DLI-IO-WDGX6324.                                                     
067100     03  -COPY WDGX6324                                                   
067200     EJECT                                                                
067300                                                                          
067400 01  FILLER         PIC X(24) VALUE 'DLI-IO-P311'.                        
067500 01  DLI-IO-P311.                                                         
067600*   03   -COPY WDP311                                                     
067700     EJECT                                                                
067800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
067900 01   DLI-IO-AREA-B601.                                                   
068000*     03  -COPY WDB601                                                    
068100     EJECT                                                                
068200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR5-6327'.           
068300 01   DLI-IO-WDR501-6327.                                                 
068400*     03  -COPY WDGX6327.                                                 
068500     EJECT                                                                
068600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDGX6328'.            
068700 01   DLI-IO-WDGX6328.                                                    
068800*     03  -COPY WDGX6328.                                                 
068900     EJECT                                                                
069000                                                                          
069100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
069200 01  DLI-IO-WDN601.                                                       
069300*    03  -COPY WDN601                                                     
069400     EJECT                                                                
069500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
069600 01  DLI-IO-WDN611.                                                       
069700*    03  -COPY WDN611                                                     
069800     EJECT                                                                
069900 01  FILLER         PIC X(16) Value 'DLI-IO-WDK901'.                      
070000 01  DLI-IO-WDK901.                                                       
070100*    03  -COPY WDK901                                                     
070200     EJECT                                                                
070300 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
070400                                                                          
070500*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
070600     EJECT                                                                
070700     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
070800     EJECT                                                                
070900                                                                          
071000 LINKAGE SECTION.                                                         
071100*01  -COPY W0009   -PRE MSG-                                              
071200*01  -COPY W0009   -PRE ALT-                                              
071300*01  -COPY W0009   -PRE KOMA-                                             
071400*01  -COPY W0008   -PRE USEA-                                             
071500     05  FILLER                  PIC X.                                   
071600     EJECT                                                                
071700*01  -COPY W0008  -PRE ARTC-                                              
071800     05  FILLER                  PIC X.                                   
071900     EJECT                                                                
072000*01  -COPY W0008  -PRE ARTS-                                              
072100     05  FILLER                  PIC X.                                   
072200     EJECT                                                                
072300*01  -COPY W0008  -PRE BENA-                                              
072400     05  FILLER                  PIC X.                                   
072500     EJECT                                                                
072600*01  -COPY W0008  -PRE 4109-                                              
072700     05  FILLER                  PIC X.                                   
072800     EJECT                                                                
072900*01  -COPY W0008  -PRE ORDL-                                              
073000     05  FILLER                  PIC X.                                   
073100     EJECT                                                                
073200*01  -COPY W0008  -PRE 6321-                                              
073300     05  FILLER                  PIC X.                                   
073400     EJECT                                                                
073500 01  ORDN-XXKP-PCB               PIC X.                                   
073600 01  ORDN-ORQL-PCB               PIC X.                                   
073700 01  ORDN-PROC-PCB               PIC X.                                   
073800 01  ORDN-ORQI-PCB               PIC X.                                   
073900 01  SAPC-PCB                    PIC X.                                   
074000     EJECT                                                                
074100*01    -COPY W0008     -PRE WDP3-                                         
074200     05  FILLER                  PIC X.                                   
074300     EJECT                                                                
074400*01  -COPY W0008      -PRE WDB6-                                          
074500     05  FILLER                  PIC X.                                   
074600     EJECT                                                                
074700*01  -COPY W0008      -PRE WDR5-                                          
074800     05  FILLER                  PIC X.                                   
074900     EJECT                                                                
075000*01  -COPY W0008      -PRE WDN6-                                          
075100     05  FILLER                  PIC X.                                   
075200     EJECT                                                                
075300*01  -COPY W0008      -PRE WDK9-                                          
075400     05  FILLER                  PIC X.                                   
075500     EJECT                                                                
075600*01  -COPY W0008      -PRE WDK6-                                          
075700     05  FILLER                  PIC X.                                   
075800     EJECT                                                                
075900                                                                          
076000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB                       
076100                           USEA-PCB ARTC-PCB ARTS-PCB                     
076200                           BENA-PCB 4109-PCB ORDL-PCB                     
076300                           6321-PCB                                       
076400                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
076500                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
076600                           SAPC-PCB WDP3-PCB WDB6-PCB WDR5-PCB            
076700                           WDN6-PCB WDK9-PCB WDK6-PCB.                    
076800 MAIN SECTION.                                                            
076900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB                       
077000                           USEA-PCB ARTC-PCB ARTS-PCB                     
077100                           BENA-PCB 4109-PCB ORDL-PCB                     
077200                           6321-PCB                                       
077300                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
077400                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
077500                           SAPC-PCB WDP3-PCB WDB6-PCB WDR5-PCB            
077600                           WDN6-PCB WDK9-PCB WDK6-PCB.                    
077700     PERFORM IMS-GET-MSG                                                  
077800     IF SEGMENT-FINNS                                                     
077900       PERFORM A-INIT                                                     
078000       PERFORM B-KOLLA-NYCKLAR                                            
078100       IF NYCKLAR-OK                                                      
078200         IF MFS-UPDATE OR MFS-UPD-V                                       
078300           PERFORM G-KOLLA-INPUT                                          
078400           IF INDATA-OK                                                   
078500             PERFORM H-UPPDATERA                                          
078600           END-IF                                                         
078700         ELSE                                                             
078800           IF MFS-FIRST                                                   
078900             PERFORM C-FOERSTA-SIDA                                       
079000           ELSE                                                           
079100             PERFORM E-SAMMA-SIDA                                         
079200           END-IF                                                         
079300         END-IF                                                           
079400         IF INDATA-OK                                                     
079500           PERFORM F-LAES-VISA-INFO                                       
079600         END-IF                                                           
079700       END-IF                                                             
079800       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O32101 + 4                      
079900       PERFORM IMS-INSERT-MSG                                             
080000     END-IF                                                               
080100                                                                          
080200     MOVE ZERO TO RETURN-CODE                                             
080300     GOBACK                                                               
080400     .                                                                    
080500     EJECT                                                                
080600 A-INIT SECTION.                                                          
080700                                                                          
080800     IF MSG-DUBBLA-TRANSKODER                                             
080900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I32101                 
081000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
081100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
081200     ELSE                                                                 
081300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I32101                  
081400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
081500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
081600     END-IF                                                               
081700                                                                          
081800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
081900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
082000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
082100                                                                          
082200     MOVE LOW-VALUE TO MSG-AREA                                           
082300     MOVE 'W6O321N1' TO MFS-IDMOD                                         
082400     MOVE '6321' TO MOD-IDTRANS                                           
082500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
082600                                                                          
082700     IF EGEN-MID OR HELP-MID                                              
082800       CONTINUE                                                           
082900     ELSE                                                                 
083000       MOVE SPACE TO MFS-KDTRTYP                                          
083100       MOVE '7' TO MFS-IDPFK                                              
083200     END-IF                                                               
083300                                                                          
083400     ACCEPT DAGENS-DATUM FROM DATE                                        
083500     ACCEPT DAGENS-TID FROM TIME                                          
083600     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
083700     .                                                                    
083800     EJECT                                                                
083900 B-KOLLA-NYCKLAR SECTION.                                                 
084000                                                                          
084100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
084200     MOVE '001'             TO MSGI-KDCALL                                
084300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
084400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
084500     MOVE '6321'            TO MSGI-IDTRANS                               
084600     IF EGEN-MID                                                          
084700       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
084800       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
084900     ELSE                                                                 
085000       IF  MID-IDARTNR-IN NUMERIC                                         
085100       AND MID-IDARTNR-IN > ZERO                                          
085200         MOVE MID-IDARTNR-IN                                              
085300                            TO MSGI-IDARTNR                               
085400       END-IF                                                             
085500     END-IF                                                               
085600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
085700                                                                          
085800*    -- KONTROLL AV SPRÅKKOD                                              
085900     IF MSGI-IDLAND-SPR = 'SE'                                            
086000       MOVE +1    TO SPR                                                  
086100       MOVE 'S  ' TO MED-IDSKYLT                                          
086200     ELSE                                                                 
086300       MOVE +2    TO SPR                                                  
086400       MOVE 'GB ' TO MED-IDSKYLT                                          
086500     END-IF                                                               
086600                                                                          
086700     MOVE JA TO NYCKLAR-SW                                                
086800                                                                          
086900*    -- KONTROLL AV IDARTNR                                               
087000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
087100                                                                          
087200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
087300       MOVE '7'         TO MFS-IDPFK                                      
087400       MOVE SPACE       TO MFS-KDTRTYP                                    
087500     END-IF                                                               
087600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
087700     IF MSGI-IDARTNR NUMERIC                                              
087800       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
087900                            W-IDARTNR-K7                                  
088000                            W-IDARTNR-MAX                                 
088100     ELSE                                                                 
088200       MOVE NEJ TO NYCKLAR-SW                                             
088300     END-IF                                                               
088400                                                                          
088500*    -- KONTROLL AV IDDC                                                  
088600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
088700                                                                          
088800     IF MID-IDDC-IN NOT = ALL '+'                                         
088900       MOVE '7'         TO MFS-IDPFK                                      
089000       MOVE SPACE       TO MFS-KDTRTYP                                    
089100     END-IF                                                               
089200                                                                          
089300     MOVE MSGI-IDDC-KEY  TO W-IDDC-B6                                     
089400                            W-IDDC-FROM                                   
089500     PERFORM IMS-GU-WDB601                                                
089600                                                                          
089700     IF SEGMENT-FINNS                                                     
089800     AND (DCS-CDC                                                         
089900     OR   DCS-CDC-TR                                                      
090000     OR   DCS-SDC                                                         
090100     OR   DCS-NDC-NA                                                      
090200     OR   DCS-NDC-CN                                                      
090300     OR   DCS-NDC-PF                                                      
090310     OR   DCS-NDC-OTHERS                                                  
090320     OR   DCS-NDC-SA)                                                     
090400       MOVE W-IDDC-B6        TO W-IDDC                                    
090500     ELSE                                                                 
090600       MOVE NEJ              TO NYCKLAR-SW                                
090700     END-IF                                                               
090800                                                                          
090900     MOVE W-IDDC          TO MOD-IDDC-UT                                  
091000                             WS-IDDC-SEND                                 
091100     MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                               
091200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
091300     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
091400     IF DCS-CDC                                                           
091500     OR DCS-CDC-TR                                                        
091600        MOVE MFS-STAENG-FAELT TO MOD-KVRETUR-BEORD-ATTR                   
091700                                 MOD-KDORDKL-ATTR                         
091800                                 MOD-RETUR-TEXT-ATTR                      
091900     END-IF                                                               
092000                                                                          
092100*--  USED FOR CHECKING IF DC IS IN CHINA FOR RETURNS                      
092200     MOVE W-IDDC-FROM          TO FROM-WS-IDDC                            
092300     IF MID-IDDC-SEND-TO    NOT = ALL '+'                                 
092400        MOVE MID-IDDC-SEND-TO  TO TO-WS-IDDC                              
092500     END-IF                                                               
092600*--                                                                       
092700     IF NYCKLAR-FEL                                                       
092800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
092900       CALL WMEDKONV USING MED-WMEDAREA                                   
093000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
093100       PERFORM MFS-RENSA-FAELT-UT                                         
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500 C-FOERSTA-SIDA SECTION.                                                  
093600                                                                          
093700     PERFORM MFS-RENSA-FAELT-IN                                           
093800     CONTINUE                                                             
093900     .                                                                    
094000     EJECT                                                                
094100 E-SAMMA-SIDA SECTION.                                                    
094200     SKIP2                                                                
094300     IF EGEN-MID OR HELP-MID                                              
094400       IF MID-INPUT-RETUR = ALL '+'                                       
094500       AND MID-INPUT-SKROT = ALL '+'                                      
094600         PERFORM MFS-RENSA-FAELT-IN                                       
094700         CONTINUE                                                         
094800       ELSE                                                               
094900         IF  TO-WS-IDDC IS NUMERIC                                        
095000         AND(FROM-NDC-CN OR FROM-LDC-CN)                                  
095100         AND NOT  TO-NDC-CN                                               
095200         AND NOT  TO-LDC-CN                                               
095300             MOVE INF-PRESS-PF23    TO MED-IDMFSINF                       
095400         ELSE                                                             
095500             MOVE INF-PRESS-PF11    TO MED-IDMFSINF                       
095600         END-IF                                                           
095700         CALL WMEDKONV USING MED-WMEDAREA                                 
095800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
095900                                                                          
096000         PERFORM EA-MID-INDATA-TILL-MOD                                   
096100                                                                          
096200       END-IF                                                             
096300     ELSE                                                                 
096400       PERFORM MFS-RENSA-FAELT-IN                                         
096500       CONTINUE                                                           
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 EA-MID-INDATA-TILL-MOD SECTION.                                          
097000     SKIP2                                                                
097100     IF MID-IDDC-SEND-TO NOT = ALL '+'                                    
097200         MOVE MID-IDDC-SEND-TO TO MOD-IDDC-SEND-TO                        
097300         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-SEND-TO-ATTR             
097400     ELSE                                                                 
097500         MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-TO                         
097600     END-IF                                                               
097700                                                                          
097800     IF MID-KVRETUR-BEORD NOT = ALL '+' AND SPACE                         
097900         MOVE MID-KVRETUR-BEORD      TO MOD-KVRETUR-BEORD                 
098000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVRETUR-BEORD-ATTR            
098100     ELSE                                                                 
098200         MOVE MFS-RENSA-FAELT TO MOD-KVRETUR-BEORD                        
098300     END-IF                                                               
098400                                                                          
098500     IF MID-KDORDKL NOT = ALL '+' AND SPACE                               
098600         MOVE MID-KDORDKL            TO MOD-KDORDKL                       
098700         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDORDKL-ATTR                  
098800     ELSE                                                                 
098900         MOVE MFS-RENSA-FAELT TO MOD-KDORDKL                              
099000     END-IF                                                               
099100                                                                          
099200     IF MID-RETUR-TEXT   NOT = ALL '+' AND SPACE                          
099300         MOVE MID-RETUR-TEXT         TO MOD-RETUR-TEXT                    
099400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-RETUR-TEXT-ATTR               
099500     ELSE                                                                 
099600         MOVE MFS-RENSA-FAELT TO MOD-RETUR-TEXT                           
099700     END-IF                                                               
099800                                                                          
099900     IF MID-KVSKROT         NOT = ALL '+' AND SPACE                       
100000         MOVE MID-KVSKROT            TO MOD-KVSKROT                       
100100         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVSKROT-ATTR                  
100200     ELSE                                                                 
100300         MOVE MFS-RENSA-FAELT TO MOD-KVSKROT                              
100400     END-IF                                                               
100500                                                                          
100600     IF MID-SKROT-TEXT   NOT = ALL '+' AND SPACE                          
100700         MOVE MID-SKROT-TEXT         TO MOD-SKROT-TEXT                    
100800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-SKROT-TEXT-ATTR               
100900     ELSE                                                                 
101000         MOVE MFS-RENSA-FAELT TO MOD-SKROT-TEXT                           
101100     END-IF                                                               
101200                                                                          
101300     IF MID-IDPERSON-QUAL   NOT = ALL '+' AND SPACE                       
101400         MOVE MID-IDPERSON-QUAL      TO MOD-IDPERSON-QUAL                 
101500         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDPERSON-QUAL-ATTR            
101600     ELSE                                                                 
101700         MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL                        
101800     END-IF                                                               
101900                                                                          
102000     IF MID-IDPERSON-ESC    NOT = ALL '+' AND SPACE                       
102100         MOVE MID-IDPERSON-ESC       TO MOD-IDPERSON-ESC                  
102200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDPERSON-ESC-ATTR             
102300     ELSE                                                                 
102400         MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-ESC                         
102500     END-IF                                                               
102600                                                                          
102700     IF MID-IDKONTO         NOT = ALL '+' AND SPACE                       
102800         MOVE MID-IDKONTO            TO MOD-IDKONTO                       
102900         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKONTO-ATTR                  
103000     ELSE                                                                 
103100         MOVE MFS-RENSA-FAELT TO MOD-IDKONTO                              
103200     END-IF                                                               
103300                                                                          
103400     IF MID-IDANALYS        NOT = ALL '+' AND SPACE                       
103500         MOVE MID-IDANALYS           TO MOD-IDANALYS                      
103600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-ATTR                 
103700     ELSE                                                                 
103800         MOVE MFS-RENSA-FAELT TO MOD-IDANALYS                             
103900     END-IF                                                               
104000                                                                          
104100     PERFORM MFS-ROER-EJ-FAELT-UT                                         
104200                                                                          
104300     MOVE NEJ TO INDATA-SW                                                
104400     .                                                                    
104500     EJECT                                                                
104600 F-LAES-VISA-INFO SECTION.                                                
104700                                                                          
104800     PERFORM FA-LAES-GRUNDDATA                                            
104900                                                                          
105000     IF SEGMENT-SAKNAS                                                    
105100        MOVE PART-MISSING TO MED-IDMFSFEL                                 
105200        MOVE 'GB '         TO MED-IDSKYLT                                 
105300        CALL WMEDKONV USING MED-WMEDAREA                                  
105400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
105500        PERFORM MFS-RENSA-FAELT-UT                                        
105600     ELSE                                                                 
105700        IF ARTC-ART-KDERS-UTG > +0                                        
105800           MOVE PART-SUPERSEDED TO MED-IDMFSFEL                           
105900           MOVE 'GB '         TO MED-IDSKYLT                              
106000           CALL WMEDKONV USING MED-WMEDAREA                               
106100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
106200           PERFORM MFS-RENSA-FAELT-UT                                     
106300        ELSE                                                              
106400           PERFORM FB-VISA                                                
106500        END-IF                                                            
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900                                                                          
107000                                                                          
107100 FA-LAES-GRUNDDATA SECTION.                                               
107200                                                                          
107300     PERFORM IMS-GU-ARTC-ARTC                                             
107400                                                                          
107500     IF SEGMENT-FINNS                                                     
107600        MOVE ARTC-ART-IDLEVNR       TO W-IDLEVNR                          
107700                                                                          
107800        PERFORM IMS-GNP-CLAG                                              
107900                                                                          
108000        IF SEGMENT-FINNS                                                  
108100           MOVE CLAG-KDERS          TO W-KDERS                            
108200           MOVE CLAG-IDANSK         TO W-IDANSK                           
108300           MOVE CLAG-PRARTSTD       TO W-PRARTSTD                         
108400           IF CLAG-REDIRLEV = 1.00                                        
108500              MOVE PART-DIR-DEL   TO MED-IDMFSINF                         
108600              CALL WMEDKONV USING MED-WMEDAREA                            
108700              MOVE MED-MFSINF TO MOD-TEMFSINF                             
108800           END-IF                                                         
108900        ELSE                                                              
109000          MOVE FEL-1(SPR)      TO MOD-TEMFSFEL                            
109100          PERFORM MFS-RENSA-FAELT-UT                                      
109200        END-IF                                                            
109300     END-IF                                                               
109400     .                                                                    
109500     EJECT                                                                
109600                                                                          
109700 FB-VISA SECTION.                                                         
109800                                                                          
109900     IF DCS-CDC                                                           
110000     OR DCS-CDC-TR                                                        
110100        PERFORM FBA-VISA-CDC                                              
110200     ELSE                                                                 
110300        PERFORM FBB-VISA-ANDRA                                            
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700                                                                          
110800 FBA-VISA-CDC SECTION.                                                    
110900                                                                          
111000     PERFORM IMS-GET-ARTC-CLAG                                            
111100     IF SEGMENT-SAKNAS                                                    
111200        MOVE FEL-1(SPR)        TO MOD-TEMFSFEL                            
111300        PERFORM MFS-RENSA-FAELT-UT                                        
111400     ELSE                                                                 
111500        IF MFS-UPDATE OR MFS-UPD-V                                        
111600          CONTINUE                                                        
111700        END-IF                                                            
111800        MOVE 'GB ' TO W-IDSKYLT                                           
111900        PERFORM IMS-GU-BENA-TEXT                                          
112000                                                                          
112100        IF SEGMENT-FINNS                                                  
112200            MOVE TEXT-BEART TO MOD-BEART                                  
112300        ELSE                                                              
112400            MOVE SPACE TO MOD-BEART                                       
112500        END-IF                                                            
112600                                                                          
112700        MOVE W-IDLEVNR           TO MOD-IDLEVNR                           
112800        MOVE W-KDERS             TO MOD-KDERS                             
112900        MOVE W-IDANSK            TO MOD-IDANSK                            
113000                                                                          
113100        MOVE MFS-RENSA-FAELT     TO MOD-KDORDKL                           
113200        MOVE CLAG-KVLS           TO MOD-KVSTOCK                           
113300        MOVE CLAG-KVAKS-PAV      TO MOD-KVAKS-PAV                         
113400        MOVE CLAG-KVAKS-CDC      TO MOD-KVAKS-SDC                         
113500        MOVE ZEROES              TO MOD-KVBEART                           
113600        MOVE CLAG-TISKROT        TO MOD-TISKROT                           
113700        MOVE CLAG-KDLEVSP        TO MOD-KDLEVSP                           
113800        MOVE CLAG-KVSPARR-KVAL   TO MOD-KVSPARR-KVAL                      
113900        MOVE MFS-RENSA-FAELT     TO MOD-KVSKROT                           
114000        MOVE MFS-RENSA-FAELT     TO MOD-RETUR-TEXT                        
114100        MOVE MFS-RENSA-FAELT     TO MOD-SKROT-TEXT                        
114200        MOVE MFS-RENSA-FAELT     TO MOD-IDPERSON-QUAL                     
114300        MOVE MFS-RENSA-FAELT     TO MOD-IDPERSON-ESC                      
114400        MOVE MFS-RENSA-FAELT     TO MOD-IDKONTO                           
114500        MOVE MFS-RENSA-FAELT   TO MOD-IDANALYS                            
114600                                                                          
114700        IF MFS-UPDATE OR MFS-UPD-V                                        
114800           CONTINUE                                                       
114900        ELSE                                                              
115000           IF CLAG-KVSPARR-KVAL > ZERO                                    
115100             MOVE CLAG-KVSPARR-KVAL TO WS-KVSPARR-KVAL                    
115200             STRING MED-6(SPR)     WS-KVSPARR-KVAL                        
115300             DELIMITED BY SIZE INTO MOD-TEMFSINF                          
115400           ELSE                                                           
115500             IF CLAG-KDLEVSP > ZERO                                       
115600               MOVE MED-7(SPR)     TO MOD-TEMFSINF                        
115700             END-IF                                                       
115800           END-IF                                                         
115900        END-IF                                                            
116000                                                                          
116100        IF ((MFS-UPDATE OR MFS-UPD-V)                                     
116200        AND  INDATA-OK                                                    
116300        AND  SKROT-IFYLLT  )                                              
116400            COMPUTE W-ANTAL-SKROT =  W-KVSKROT                            
116500                    END-COMPUTE                                           
116600            COMPUTE W-SCRAPPED-VALUE =                                    
116700                    W-ANTAL-SKROT * W-PRARTSTD                            
116800                    END-COMPUTE                                           
116900            MOVE W-SCRAPPED-VALUE TO MOD-VALUE-OF-SCRAP-QTY               
117000        END-IF                                                            
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400                                                                          
117500 FBB-VISA-ANDRA SECTION.                                                  
117600                                                                          
117700     PERFORM IMS-GET-ARTS-WLARTS11                                        
117800     IF SEGMENT-SAKNAS                                                    
117900        MOVE FEL-1(SPR)        TO MOD-TEMFSFEL                            
118000        PERFORM MFS-RENSA-FAELT-UT                                        
118100     ELSE                                                                 
118200        IF MFS-UPDATE OR MFS-UPD-V                                        
118300          CONTINUE                                                        
118400        END-IF                                                            
118500        MOVE 'GB ' TO W-IDSKYLT                                           
118600        PERFORM IMS-GU-BENA-TEXT                                          
118700                                                                          
118800        IF SEGMENT-FINNS                                                  
118900            MOVE TEXT-BEART TO MOD-BEART                                  
119000        ELSE                                                              
119100            MOVE SPACE TO MOD-BEART                                       
119200        END-IF                                                            
119300                                                                          
119400        MOVE W-IDLEVNR           TO MOD-IDLEVNR                           
119500        MOVE W-KDERS             TO MOD-KDERS                             
119600        MOVE W-IDANSK            TO MOD-IDANSK                            
119700                                                                          
119800        MOVE SLAG-TIRETUR-BEORD  TO MOD-TIRETUR-BEORD                     
119900        MOVE MFS-RENSA-FAELT     TO MOD-KVRETUR-BEORD                     
120000        MOVE MFS-RENSA-FAELT     TO MOD-KDORDKL                           
120100        COMPUTE W-KVSTOCK         = SLAG-KVLS - 0                         
120200***                                 SLAG-KVRESS                           
120300        END-COMPUTE                                                       
120400        MOVE W-KVSTOCK         TO MOD-KVSTOCK                             
120500        MOVE SLAG-KVAKS-PAV    TO MOD-KVAKS-PAV                           
120600        MOVE SLAG-KVAKS-SDC    TO MOD-KVAKS-SDC                           
120700        MOVE SLAG-KVBEART      TO MOD-KVBEART                             
120800        MOVE MFS-RENSA-FAELT   TO MOD-KVSKROT                             
120900        MOVE SLAG-TISKROT      TO MOD-TISKROT                             
121000        MOVE SLAG-KDLEVSP      TO MOD-KDLEVSP                             
121100        MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL                        
121200        MOVE MFS-RENSA-FAELT   TO MOD-RETUR-TEXT                          
121300        MOVE MFS-RENSA-FAELT   TO MOD-SKROT-TEXT                          
121400        MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-QUAL                       
121500        MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-ESC                        
121600        MOVE MFS-RENSA-FAELT   TO MOD-IDKONTO                             
121700        MOVE MFS-RENSA-FAELT   TO MOD-IDANALYS                            
121800        IF NOT (SLAG-IDLEVNR = '1441 ' OR 'BP2TW') AND                    
121900           SLAG-IDLEVNR     > SPACE                                       
122000           MOVE SLAG-IDLEVNR   TO MOD-IDLEVNR                             
122100        END-IF                                                            
122200                                                                          
122300        IF MFS-UPDATE OR MFS-UPD-V                                        
122400           CONTINUE                                                       
122500        ELSE                                                              
122600           IF SLAG-FLORDSP = JA                                           
122700              MOVE MED-8(SPR)TO MOD-TEMFSINF                              
122800           ELSE                                                           
122900              IF SLAG-KVSPARR-KVAL > ZERO                                 
123000                MOVE SLAG-KVSPARR-KVAL TO WS-KVSPARR-KVAL                 
123100                STRING MED-6(SPR)  WS-KVSPARR-KVAL                        
123200                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
123300              ELSE                                                        
123400                IF SLAG-KDLEVSP > ZERO                                    
123500                  MOVE MED-7(SPR)  TO MOD-TEMFSINF                        
123600                END-IF                                                    
123700              END-IF                                                      
123800           END-IF                                                         
123900        END-IF                                                            
124000                                                                          
124100        IF ((MFS-UPDATE OR MFS-UPD-V)                                     
124200        AND  INDATA-OK                                                    
124300        AND  SKROT-IFYLLT  )                                              
124400          COMPUTE W-ANTAL-SKROT =  W-KVSKROT                              
124500                    END-COMPUTE                                           
124600          IF DCS-NDC-NA OR                                                
124700             DCS-NDC-CN OR                                                
124800            (DCS-SDC AND DCS-CHINA)                                       
124900            COMPUTE W-SCRAPPED-VALUE =                                    
125000                    W-ANTAL-SKROT * SLAG-PRAVCOST                         
125100                    END-COMPUTE                                           
125200            MOVE W-SCRAPPED-VALUE TO MOD-VALUE-OF-SCRAP-QTY               
125300                                                                          
125400          ELSE                                                            
125500            COMPUTE W-SCRAPPED-VALUE =                                    
125600                    W-ANTAL-SKROT * W-PRARTSTD                            
125700                    END-COMPUTE                                           
125800            MOVE W-SCRAPPED-VALUE TO MOD-VALUE-OF-SCRAP-QTY               
125900          END-IF                                                          
126000        END-IF                                                            
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500                                                                          
126600                                                                          
126700 G-KOLLA-INPUT SECTION.                                                   
126800     SKIP2                                                                
126900     MOVE JA  TO INDATA-SW                                                
127000     IF MID-INPUT-RETUR       = ALL '+'                                   
127100     AND MID-INPUT-SKROT = ALL '+'                                        
127200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
127300       CALL WMEDKONV USING MED-WMEDAREA                                   
127400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
127500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
127600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
127700       MOVE NEJ TO INDATA-SW                                              
127800     ELSE                                                                 
127900       IF  MID-INPUT-RETUR  NOT  = ALL '+'                                
128000       AND (TO-CDC-SE OR TO-CDC-TR)                                       
128100       AND (FROM-NDC-CN OR FROM-LDC-CN OR FROM-NDC-US)                    
128200         IF  MFS-UPD-V                                                    
128300             CONTINUE                                                     
128400         ELSE                                                             
128500             MOVE INF-PRESS-PF23 TO MED-IDMFSINF                          
128600             CALL WMEDKONV    USING MED-WMEDAREA                          
128700             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
128800             PERFORM MFS-ROER-EJ-FAELT-IN                                 
128900             PERFORM EA-MID-INDATA-TILL-MOD                               
129000             MOVE NEJ            TO INDATA-SW                             
129100         END-IF                                                           
129200       ELSE                                                               
129300         IF  MFS-UPDATE                                                   
129400             CONTINUE                                                     
129500         ELSE                                                             
129600             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
129700             CALL WMEDKONV    USING MED-WMEDAREA                          
129800             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
129900             PERFORM MFS-ROER-EJ-FAELT-IN                                 
130000             PERFORM EA-MID-INDATA-TILL-MOD                               
130100             MOVE NEJ            TO INDATA-SW                             
130200         END-IF                                                           
130300       END-IF                                                             
130400     END-IF                                                               
130500                                                                          
130600     IF INDATA-OK                                                         
130700       IF MID-INPUT-RETUR NOT = ALL '+'                                   
130800           MOVE JA TO RETUR-IFYLLT-SW                                     
130900       END-IF                                                             
131000       IF MID-INPUT-SKROT NOT = ALL '+'                                   
131100           MOVE JA TO SKROT-IFYLLT-SW                                     
131200       END-IF                                                             
131300       IF RETUR-IFYLLT AND SKROT-IFYLLT                                   
131400           MOVE MED-12 (SPR)   TO MOD-TEMFSINF                            
131500           MOVE NEJ            TO INDATA-SW                               
131600       ELSE                                                               
131700          IF RETUR-IFYLLT                                                 
131800             PERFORM GA-KOLLA-RETUR                                       
131900          ELSE                                                            
132000            IF SKROT-IFYLLT                                               
132100               PERFORM GB-KOLLA-SKROT                                     
132200            END-IF                                                        
132300          END-IF                                                          
132400       END-IF                                                             
132500                                                                          
132600       IF INDATA-FEL                                                      
132700***      IF ORDERSPARR-ARTIKEL                                            
132800***        MOVE ERR-ORDER-NOT-AVAILABLE TO MED-IDMFSFEL                   
132900***      ELSE                                                             
133000           IF NOSPLITBULK                                                 
133100             MOVE ERR-NO-SPLIT-BULK-PACK TO MED-IDMFSFEL                  
133200           ELSE                                                           
133300             IF OGILTIGT-ANALYSNR                                         
133400               MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                
133500             ELSE                                                         
133600               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
133700             END-IF                                                       
133800           END-IF                                                         
133900***      END-IF                                                           
134000         CALL WMEDKONV USING MED-WMEDAREA                                 
134100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
134200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
134300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
134400         PERFORM MFS-LAES-IN-IGEN                                         
134500       END-IF                                                             
134600     END-IF                                                               
134700     .                                                                    
134800     EJECT                                                                
134900 GA-KOLLA-RETUR    SECTION.                                               
135000     MOVE 'GA-KOLLA-RETUR '  TO CURRENT-SECTION                           
135100     SKIP2                                                                
135200     PERFORM GAA-KOLLA-RETUR-INDATA                                       
135300                                                                          
135400     IF INDATA-OK                                                         
135500       PERFORM S02-KOLLA-USER-DC                                          
135600     END-IF                                                               
135700                                                                          
135800     IF INDATA-OK                                                         
135900******      ANALYSNUMMER Kollas bara vid retur till CDC för Kina.         
136000******      för transfers inom Kina skall ingen koll göras.               
136100       IF DCS-NDC-NA OR                                                   
136200         (DCS-NDC-CN AND TO-CDC-SE) OR                                    
136300         (DCS-SDC AND DCS-FTG-CN AND TO-CDC-SE)                           
136400                                                                          
136500          PERFORM S01-KONTROLLERA-ANALYSNR                                
136600          IF OGILTIGT-ANALYSNR                                            
136700            MOVE NEJ TO INDATA-SW                                         
136800            MOVE MED-1 (SPR) TO MOD-TEMFSINF MOD-TEMFSFEL                 
136900******      ANALYSNUMMER UPPDATERAS PÅ 4702                               
137000          END-IF                                                          
137100       END-IF                                                             
137200       IF INDATA-OK                                                       
137300         PERFORM GAB-FORMELLA-KONTROLLER                                  
137400       END-IF                                                             
137500     END-IF                                                               
137600     .                                                                    
137700     EJECT                                                                
137800                                                                          
137900 GAA-KOLLA-RETUR-INDATA SECTION.                                          
138000     MOVE 'GAA-KOLLA-RETUR-INDATA ' TO CURRENT-SECTION                    
138100     SKIP2                                                                
138200                                                                          
138300      IF MID-IDDC-SEND-TO NOT = ALL '+'                                   
138400         MOVE MID-IDDC-SEND-TO TO TO-WS-IDDC                              
138500                                  WS-IDDC-REC                             
138600                                                                          
138700         IF TO-CDC-SE                                                     
138800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-SEND-TO-ATTR             
138900         ELSE                                                             
139000           MOVE 'QUAL'    TO WS-KDARBTYP                                  
139100           PERFORM DB2-SELECT-TP4TRAN                                     
139200           IF RADER-FINNS                                                 
139300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-SEND-TO-ATTR           
139400           ELSE                                                           
139500             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-SEND-TO-ATTR             
139600             MOVE NEJ TO INDATA-SW                                        
139700           END-IF                                                         
139800         END-IF                                                           
139900      ELSE                                                                
140000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-SEND-TO-ATTR                 
140100         MOVE NEJ TO INDATA-SW                                            
140200      END-IF                                                              
140300                                                                          
140400***   INSPECT MID-KVRETUR-BEORD REPLACING ALL SPACE BY     '+'            
140500      INSPECT MID-KVRETUR-BEORD REPLACING ALL SPACE BY '0'                
140600                                                                          
140700***   IF MID-KVRETUR-BEORD = ALL '+'                                      
140800***     CONTINUE                                                          
140900***   ELSE                                                                
141000        IF MID-KVRETUR-BEORD NUMERIC                                      
141100        AND MID-KVRETUR-BEORD > +0                                        
141200            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVRETUR-BEORD-ATTR            
141300        ELSE                                                              
141400            MOVE MFS-NUM-FAELT-FEL   TO MOD-KVRETUR-BEORD-ATTR            
141500            MOVE MED-13 (SPR)        TO MOD-TEMFSINF                      
141600            MOVE NEJ                 TO INDATA-SW                         
141700        END-IF                                                            
141800***   END-IF                                                              
141900                                                                          
142000                                                                          
142100*     --- FAST ORDER YES OR NO                                            
142200      IF MID-KDORDKL  = ALL '+'                                           
142300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDORDKL-ATTR                    
142400      ELSE                                                                
142500         IF MID-KDORDKL = YES OR JA  OR NEJ                               
142600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDORDKL-ATTR                
142700         ELSE                                                             
142800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDORDKL-ATTR                
142900             MOVE NEJ                  TO INDATA-SW                       
143000             MOVE MED-2 (SPR)          TO MOD-TEMFSINF                    
143100         END-IF                                                           
143200      END-IF                                                              
143300                                                                          
143400                                                                          
143500      IF MID-RETUR-TEXT   = ALL '+'                                       
143600         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-RETUR-TEXT-ATTR                
143700         MOVE MFS-RENSA-FAELT TO MOD-RETUR-TEXT                           
143800      ELSE                                                                
143900         INSPECT MID-RETUR-TEXT                                           
144000             REPLACING ALL '+' BY SPACE                                   
144100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-RETUR-TEXT-ATTR                 
144200      END-IF                                                              
144300     .                                                                    
144400     EJECT                                                                
144500 GAB-FORMELLA-KONTROLLER SECTION.                                         
144600     MOVE 'GAB-FORMELLA-KONTROLLER '  TO CURRENT-SECTION                  
144700     SKIP2                                                                
144800     IF INDATA-OK                                                         
144900       IF TO-CDC-SE                                                       
145000         IF (DCS-NDC-NA AND DCS-USA) OR                                   
145100             DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                        
145200                                                                          
145300           PERFORM S10-KOLLA-LOKAL-ART-NDC                                
145400         END-IF                                                           
145500       END-IF                                                             
145600                                                                          
145700       PERFORM GABB-KOLLA-ARTNR-FINNS                                     
145800     END-IF                                                               
145900     .                                                                    
146000     EJECT                                                                
146100 GABB-KOLLA-ARTNR-FINNS SECTION.                                          
146200     MOVE 'GABB-KOLLA-ARTNR-FINNS ' TO CURRENT-SECTION                    
146300     SKIP2                                                                
146400     PERFORM IMS-GET-ARTS-WLARTS11                                        
146500     IF SEGMENT-SAKNAS                                                    
146600        MOVE NEJ                TO INDATA-SW                              
146700        MOVE MED-14 (SPR)       TO MOD-TEMFSINF                           
146800        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                    
146900     ELSE                                                                 
147000       IF TO-CDC-SE                                                       
147100         IF DCS-NDC-NA OR DCS-NDC-CN OR                                   
147200           (DCS-SDC AND DCS-FTG-CN)                                       
147300           IF SLAG-IDDC-REF = SPACE OR SW-LOKAL-ART = JA                  
147400*      --- LOKALT ANSKAFFAD USA/CN ARTIKEL FÅR EJ RETURNERAS,CDC          
147500                                                                          
147600             MOVE NEJ                TO INDATA-SW                         
147700             MOVE MED-10 (SPR)       TO MOD-TEMFSINF                      
147800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR               
147900           END-IF                                                         
148000         END-IF                                                           
148100       END-IF                                                             
148200     END-IF                                                               
148300                                                                          
148400     IF INDATA-OK                                                         
148500*      --- BEHANDLA RETUR-KVANTITET                                       
148600       IF MID-KVRETUR-BEORD = ALL '+'                                     
148700           MOVE ZERO      TO W-KVRETUR-BEORD                              
148800       ELSE                                                               
148900         MOVE MID-KVRETUR-BEORD TO W-KVRETUR-BEORD                        
149000       END-IF                                                             
149100                                                                          
149200       IF W-KVRETUR-BEORD     >  SLAG-KVLS                                
149300           MOVE NEJ                TO INDATA-SW                           
149400           MOVE MED-15 (SPR)       TO MOD-TEMFSINF                        
149500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVRETUR-BEORD-ATTR              
149600       END-IF                                                             
149700                                                                          
149800                                                                          
149900       IF INDATA-OK                                                       
150000         PERFORM IMS-GU-ARTC-ARTC                                         
150100*        --- KOLLA OM BESTÄLLD KVANT BERÖRS AV KVQPACK                    
150200         PERFORM IMS-GNP-CLAG                                             
150300         IF      CLAG-KVQPACK-1 > 0                                       
150400           DIVIDE W-KVRETUR-BEORD BY      CLAG-KVQPACK-1                  
150500             GIVING WS-KVQPACK-ANT ROUNDED                                
150600             REMAINDER WS-KVQPACK-REST                                    
150700           END-DIVIDE                                                     
150800                                                                          
150900           IF WS-KVQPACK-REST NOT = ZERO                                  
151000                MOVE NEJ TO INDATA-SW                                     
151100                MOVE JA TO NOSPLITBULK-SW                                 
151200                MOVE MFS-ALFA-FAELT-FEL                                   
151300                                    TO MOD-KVRETUR-BEORD-ATTR             
151400           END-IF                                                         
151500         END-IF                                                           
151600       END-IF                                                             
151700     END-IF                                                               
151800*    ---                                                                  
151900     IF INDATA-OK                                                         
152000       IF  NOT TO-CDC-SE                                                  
152100         MOVE MID-IDDC-SEND-TO  TO W-IDDC                                 
152200         PERFORM IMS-GET-ARTS-WLARTS11                                    
152300         IF SEGMENT-FINNS                                                 
152400*          --- OK, SPARA LAGERADRESSEN FÖR ORDERRADENS BERADREF           
152500           MOVE SLAG-ADLAGOMR TO WS-ADLAGOMR-X                            
152600           MOVE SLAG-ADGANG   TO WS-ADGANG-X                              
152700           MOVE SLAG-ADPLATS  TO WS-ADPLATS-X                             
152800         ELSE                                                             
152900           MOVE NEJ TO INDATA-SW                                          
153000           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDARTNR-UT-ATTR                
153100                                       MOD-IDDC-SEND-TO-ATTR              
153200         END-IF                                                           
153300       ELSE                                                               
153400*      --- RETUR                                                          
153500*      --- LOKAL USA ARTIKEL FÅR INTE RETURNERAS                          
153600*      --- LOKAL KINA ARTIKEL FÅR INTE RETURNERAS ENL. PATRIK L.          
153700         MOVE ARTC-ART-KDPRODSL  TO TEST-KDPRODSL                         
153800         IF KDPRODSL-LOCAL                                                
153900           MOVE NEJ TO INDATA-SW                                          
154000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                 
154100           MOVE MED-10 (SPR)        TO MOD-TEMFSINF                       
154200                                                                          
154300         END-IF                                                           
154400       END-IF                                                             
154500     END-IF                                                               
154600     .                                                                    
154700     EJECT                                                                
154800*GABC-KOLLA-ORDER-FRAKT SECTION.                                          
154900*    SKIP2                                                                
155000*    OM MAN VILL HA EN SNABB-ORDER  SKA MAN HA MÖJLIGHET ATT              
155100*    ANGE EN FRAKTKOD, SOM FÖR TILLFÄLLET ENDAST KAN VARA                 
155200*    12. ANGER MAN INGEN FRAKTKOD LÄGGS DET BLANKT I ORDER-               
155300*    HUVUDET OCH INFO HÄMTAS DÅ I STÄLLET FRÅN KUNDREGISTRET              
155400*                                                                         
155500*                                                                         
155600*    .                                                                    
155700     EJECT                                                                
155800 GB-KOLLA-SKROT SECTION.                                                  
155900     MOVE 'GB-KOLLA-SKROT '  TO CURRENT-SECTION                           
156000     SKIP2                                                                
156100     PERFORM GBA-KOLLA-SKROT-INDATA                                       
156200                                                                          
156300     IF INDATA-OK                                                         
156400       PERFORM S02-KOLLA-USER-DC                                          
156500     END-IF                                                               
156600                                                                          
156700     IF INDATA-OK                                                         
156800        MOVE MSGI-IDFTG    TO WS-IDFTG                                    
156900        IF (DCS-NDC-NA AND IDFTG-PV) OR                                   
157000           (DCS-NDC-CN AND IDFTG-PV) OR                                   
157100           (DCS-SDC AND DCS-FTG-CN AND IDFTG-PV)                          
157200                                                                          
157300           PERFORM S10-KOLLA-LOKAL-ART-NDC                                
157400                                                                          
157500           IF MID-IDKONTO  NOT = ALL '+' OR                               
157600              MID-IDANALYS NOT = ALL '+'                                  
157700              MOVE NEJ TO INDATA-SW                                       
157800              MOVE MED-11 (SPR)  TO MOD-TEMFSINF   MOD-TEMFSFEL           
157900*            *IDKONTO IDANALYS FÅR EJ UPPDATERAS HÄR                      
158000*            *DÄREMOT SKALL IDANALYS KOLLAS                               
158100              PERFORM S01-KONTROLLERA-ANALYSNR                            
158200              IF OGILTIGT-ANALYSNR                                        
158300                 MOVE NEJ TO INDATA-SW                                    
158400                 MOVE MED-1 (SPR)  TO MOD-TEMFSINF  MOD-TEMFSFEL          
158500*                ANALYSNUMMER UPPDATERAS PÅ 4702                          
158600              END-IF                                                      
158700           END-IF                                                         
158800        ELSE                                                              
158900          IF IFYLLD-KONTORAD                                              
159000           PERFORM S01-KONTROLLERA-ANALYSNR                               
159100           IF OGILTIGT-ANALYSNR                                           
159200              MOVE NEJ TO INDATA-SW                                       
159300              MOVE MED-1 (SPR)  TO MOD-TEMFSINF   MOD-TEMFSFEL            
159400*             ANALYSNUMMER UPPDATERAS PÅ 4702                             
159500           END-IF                                                         
159600                                                                          
159700           MOVE MSGI-IDFTG           TO W-IDFTG-B6                        
159710*                                                                         
159800*          IF IDFTG-PV                                                    
159900*             MOVE 'SEPV'            TO SAP-KDTRADP                       
160000*          ELSE                                                           
160100*            IF IDFTG-CN                                                  
160200*               MOVE 'CN05'          TO SAP-KDTRADP                       
160300*            ELSE                                                         
160400*              IF IDFTG-US                                                
160500*                 MOVE 'US01'        TO SAP-KDTRADP                       
160600*              ELSE                                                       
160700*                IF IDFTG-IN                                              
160800*                   MOVE 'IN07'      TO SAP-KDTRADP                       
160900*                ELSE                                                     
161000*                   MOVE 'SEPV'      TO SAP-KDTRADP                       
161100*                END-IF                                                   
161200*              END-IF                                                     
161300*            END-IF                                                       
161400*          END-IF                                                         
161401           IF IDFTG-NON-VCC                                               
161410             PERFORM IMS-GU-WDB601-FTG                                    
161420             IF SEGMENT-FINNS                                             
161430               MOVE DCS-KDTRADP        TO SAP-KDTRADP                     
161440             END-IF                                                       
161450           ELSE                                                           
161451             IF IDFTG-US                                                  
161452               MOVE 'US01'        TO SAP-KDTRADP                          
161453             ELSE                                                         
161454               MOVE 'SEPV'      TO SAP-KDTRADP                            
161455             END-IF                                                       
161460           END-IF                                                         
161500           MOVE MID-IDKONTO          TO SAP-IDKONTO                       
161600           MOVE space                TO SAP-IDKST                         
161700           MOVE MID-IDANALYS         TO SAP-IDANALYS                      
161800           MOVE ZERO                 TO SAP-IDDISTR                       
161900           MOVE SPACE                TO SAP-KDFAKTYP                      
162000           MOVE ZERO                 TO SAP-IDFTG                         
162100           MOVE SPACE                TO SAP-IDPROFIT                      
162200           MOVE +2                   TO SAP-KDCALL                        
162300                                                                          
162400           CALL W411SAP USING SAP-W411SAP SAPC-PCB                        
162500                                                                          
162600           IF SAP-BEFEL NOT = SPACE                                       
162700             MOVE SAP-BEFEL            TO MOD-TEMFSFEL                    
162800             MOVE NEJ                  TO INDATA-SW                       
162900             IF SAP-IDKONTO-OK = NEJ                                      
163000               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKONTO-ATTR                
163100             ELSE                                                         
163200               IF SAP-IDANALYS-OK = NEJ                                   
163300                 MOVE MFS-ALFA-FAELT-FEL                                  
163400                                       TO MOD-IDANALYS-ATTR               
163500               END-IF                                                     
163600             END-IF                                                       
163700           END-IF                                                         
163800          END-IF                                                          
163900        END-IF                                                            
164000        IF INDATA-OK                                                      
164100           PERFORM GBB-FORMELLA-KONTROLLER                                
164200        END-IF                                                            
164300        IF INDATA-OK                                                      
164400          IF (DCS-NDC-NA AND IDFTG-PV)                                    
164500          OR DCS-NDC-PF OR DCS-NDC-OTHERS OR DCS-NDC-SA                   
164600             CONTINUE                                                     
164700          ELSE                                                            
164800             PERFORM GBC-KOLLA-IDUSER                                     
164900          END-IF                                                          
165000        END-IF                                                            
165100     END-IF                                                               
165200     .                                                                    
165300     EJECT                                                                
165400 GBA-KOLLA-SKROT-INDATA SECTION.                                          
165500     MOVE 'GBA-KOLLA-SKROT-INDATA '  TO CURRENT-SECTION                   
165600     SKIP2                                                                
165700      INSPECT MID-KVSKROT  REPLACING ALL SPACE BY '0'                     
165800      IF MID-KVSKROT        = ALL '+'                                     
165900         MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-ATTR                       
166000         MOVE MED-13 (SPR)      TO MOD-TEMFSINF                           
166100         MOVE NEJ               TO INDATA-SW                              
166200      ELSE                                                                
166300         IF MID-KVSKROT          NUMERIC                                  
166400         AND MID-KVSKROT         > +0                                     
166500             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKROT-ATTR                 
166600         ELSE                                                             
166700             MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-ATTR                   
166800             MOVE MED-13 (SPR)      TO MOD-TEMFSINF                       
166900             MOVE NEJ               TO INDATA-SW                          
167000         END-IF                                                           
167100      END-IF                                                              
167200                                                                          
167300      IF MID-SKROT-TEXT      = ALL '+'                                    
167400         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-SKROT-TEXT-ATTR                
167500         MOVE MFS-RENSA-FAELT TO MOD-SKROT-TEXT                           
167600         MOVE SPACE           TO MID-SKROT-TEXT                           
167700      ELSE                                                                
167800         INSPECT MID-SKROT-TEXT                                           
167900             REPLACING ALL '+' BY SPACE                                   
168000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SKROT-TEXT-ATTR                 
168100      END-IF                                                              
168200                                                                          
168300      INSPECT MID-IDPERSON-QUAL REPLACING LEADING SPACE BY ZERO           
168400      INSPECT MID-IDPERSON-ESC  REPLACING LEADING SPACE BY ZERO           
168500      IF MID-IDPERSON-QUAL  = ALL '+' AND                                 
168600         MID-IDPERSON-ESC   = ALL '+'                                     
168700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR                 
168800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-ESC-ATTR                  
168900         MOVE MED-16 (SPR)      TO MOD-TEMFSINF                           
169000         MOVE NEJ               TO INDATA-SW                              
169100      ELSE                                                                
169200         IF (MID-IDPERSON-QUAL    NUMERIC                                 
169300             AND MID-IDPERSON-QUAL   > +0) OR                             
169400            (MID-IDPERSON-ESC     NUMERIC                                 
169500             AND MID-IDPERSON-ESC    > +0)                                
169600            IF (MID-IDPERSON-QUAL    NUMERIC                              
169700                AND MID-IDPERSON-QUAL   > +0) AND                         
169800               (MID-IDPERSON-ESC     NUMERIC                              
169900                AND MID-IDPERSON-ESC    > +0)                             
170000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR             
170100             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-ESC-ATTR              
170200             MOVE MED-16 (SPR)      TO MOD-TEMFSINF                       
170300             MOVE NEJ               TO INDATA-SW                          
170400            ELSE                                                          
170500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-QUAL-ATTR           
170600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-ESC-ATTR            
170700            END-IF                                                        
170800         ELSE                                                             
170900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR             
171000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-ESC-ATTR              
171100             MOVE MED-16 (SPR)      TO MOD-TEMFSINF                       
171200             MOVE NEJ               TO INDATA-SW                          
171300         END-IF                                                           
171400      END-IF                                                              
171500      IF MID-IDPERSON-QUAL NUMERIC AND                                    
171600         MID-IDPERSON-QUAL > ZERO                                         
171700         MOVE MID-IDPERSON-QUAL TO WS-IDPERSON W-IDPERSON                 
171800         MOVE 'QUAL'            TO WS-KDARBTYP W-KDARBTYP                 
171900      END-IF                                                              
172000      IF MID-IDPERSON-ESC  NUMERIC AND                                    
172100         MID-IDPERSON-ESC  > ZERO                                         
172200         MOVE MID-IDPERSON-ESC  TO WS-IDPERSON W-IDPERSON                 
172300         MOVE 'ESC '            TO WS-KDARBTYP W-KDARBTYP                 
172400      END-IF                                                              
172500                                                                          
172600      PERFORM IMS-LAS-IDPERSON                                            
172700      IF SEGMENT-FINNS                                                    
172800         CONTINUE                                                         
172900      ELSE                                                                
173000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR                 
173100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-ESC-ATTR                  
173200         MOVE MED-22 (SPR)      TO MOD-TEMFSINF                           
173300         MOVE NEJ               TO INDATA-SW                              
173400      END-IF                                                              
173500                                                                          
173600      IF MID-IDKONTO        = ALL '+'                                     
173700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-ATTR                     
173800      ELSE                                                                
173900         IF MID-IDKONTO          NUMERIC                                  
174000             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-ATTR                 
174100         ELSE                                                             
174200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                   
174300             MOVE MED-18 (SPR)      TO MOD-TEMFSINF                       
174400             MOVE NEJ               TO INDATA-SW                          
174500         END-IF                                                           
174600      END-IF                                                              
174700                                                                          
174800      IF MID-IDANALYS        = ALL '+'                                    
174900         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDANALYS-ATTR                  
175000         MOVE MFS-RENSA-FAELT TO MOD-IDANALYS                             
175100      ELSE                                                                
175200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDANALYS-ATTR                   
175300      END-IF                                                              
175400                                                                          
175500     IF (MID-IDKONTO      NOT = ALL '+' AND                               
175600         MID-IDANALYS     NOT = ALL '+')                                  
175700         MOVE JA  TO IFYLLD-KONTORAD-SW                                   
175800     ELSE                                                                 
175900        IF (MID-IDKONTO       = ALL '+' AND                               
176000            MID-IDANALYS      = ALL '+')                                  
176100            MOVE NEJ TO IFYLLD-KONTORAD-SW                                
176200        ELSE                                                              
176300             MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-ATTR                  
176400             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANALYS-ATTR                 
176500             MOVE MED-19 (SPR)       TO MOD-TEMFSINF                      
176600             MOVE NEJ                TO INDATA-SW                         
176700        END-IF                                                            
176800     END-IF                                                               
176900     .                                                                    
177000     EJECT                                                                
177100 GBB-FORMELLA-KONTROLLER SECTION.                                         
177200     MOVE 'GBB-FORMELLA-KONTROLLER '  TO CURRENT-SECTION                  
177300     SKIP2                                                                
177400     MOVE MSGI-IDARTNR TO W-IDARTNR W-IDARTNR-K7 W-IDARTNR-MAX            
177500     PERFORM IMS-GET-ARTC-CLAG-GE                                         
177600     IF DCS-CDC                                                           
177700     OR DCS-CDC-TR                                                        
177800*       PERFORM IMS-GET-ARTC-CLAG-GE                                      
177900        CONTINUE                                                          
178000     ELSE                                                                 
178100        PERFORM IMS-GET-ARTS-WLARTS11                                     
178200     END-IF                                                               
178300     IF SEGMENT-SAKNAS                                                    
178400       MOVE NEJ                TO INDATA-SW                               
178500       MOVE MED-14 (SPR)       TO MOD-TEMFSINF                            
178600       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UT-ATTR                     
178700     ELSE                                                                 
178800       IF DCS-CDC                                                         
178900       OR DCS-CDC-TR                                                      
179000          PERFORM GBBA-KOLLA-SKROT-SALDO                                  
179100       ELSE                                                               
179200*-   FTG 57 SKALL BARA KUNNA SKROTA REFILLARTIKLAR I USA/KINA             
179300         MOVE MSGI-IDFTG   TO WS-IDFTG                                    
179400         IF IDFTG-PV                                                      
179500           IF (DCS-NDC-NA AND DCS-USA) OR                                 
179600               DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                      
179700             IF SLAG-IDDC-REF = SPACE OR                                  
179800                SW-LOKAL-ART = JA                                         
179900                                                                          
180000               MOVE NEJ TO INDATA-SW                                      
180100               MOVE ERR-NO-REFILL-PART      TO MED-IDMFSINF               
180200               CALL WMEDKONV USING MED-WMEDAREA                           
180300               MOVE MED-MFSINF TO MOD-TEMFSINF                            
180400             END-IF                                                       
180500           END-IF                                                         
180600         END-IF                                                           
180700                                                                          
180800         IF INDATA-OK                                                     
180900           PERFORM GBBB-KOLLA-SKROT-SALDO                                 
181000         END-IF                                                           
181100         IF INDATA-OK                                                     
181200           IF (IDFTG-CN AND DCS-NDC-CN) OR                                
181300              (IDFTG-US AND DCS-NDC-NA)                                   
181400              IF DCS-IDDISTR-RSKROT = ZERO OR                             
181500                 DCS-IDKUNDNR-RSKROT = ZERO                               
181600                 MOVE NEJ TO INDATA-SW                                    
181700                 MOVE ERR-KUNDUPPG-SAKNAS   TO MED-IDMFSINF               
181800                 CALL WMEDKONV USING MED-WMEDAREA                         
181900                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
182000             END-IF                                                       
182100           END-IF                                                         
182200         END-IF                                                           
182300       END-IF                                                             
182400     END-IF                                                               
182500     .                                                                    
182600     EJECT                                                                
182700 GBBA-KOLLA-SKROT-SALDO SECTION.                                          
182800     MOVE 'GBBA-KOLLA-SKROT-SALDO '  TO CURRENT-SECTION                   
182900     SKIP2                                                                
183000     IF CLAG-FLSKROT-BEORD = JA  OR YES                                   
183100       MOVE NEJ                   TO INDATA-SW                            
183200       MOVE MED-20 (SPR)          TO MOD-TEMFSINF                         
183300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVSKROT-ATTR                     
183400       MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVSKROT-ATTR                     
183500     END-IF                                                               
183600                                                                          
183700     MOVE MID-KVSKROT             TO W-KVSKROT-DISPLAY                    
183800     MOVE W-KVSKROT-DISPLAY       TO W-KVSKROT                            
183900                                                                          
184000     IF W-KVSKROT-DISPLAY   >  CLAG-KVLS                                  
184100         MOVE NEJ                 TO INDATA-SW                            
184200         MOVE MED-21 (SPR)        TO MOD-TEMFSINF                         
184300         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVSKROT-ATTR                     
184400     END-IF                                                               
184500                                                                          
184600     .                                                                    
184700     EJECT                                                                
184800 GBBB-KOLLA-SKROT-SALDO SECTION.                                          
184900     MOVE 'GBBB-KOLLA-SKROT-SALDO '  TO CURRENT-SECTION                   
185000     SKIP2                                                                
185100     IF SLAG-FLSKROT-BEORD = JA  OR YES                                   
185200       MOVE NEJ                   TO INDATA-SW                            
185300       MOVE MED-20 (SPR)          TO MOD-TEMFSINF                         
185400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVSKROT-ATTR                     
185500       MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVSKROT-ATTR                     
185600     END-IF                                                               
185700                                                                          
185800     MOVE MID-KVSKROT             TO W-KVSKROT-DISPLAY                    
185900     MOVE W-KVSKROT-DISPLAY       TO W-KVSKROT                            
186000                                                                          
186100     IF W-KVSKROT-DISPLAY   >  SLAG-KVLS                                  
186200         MOVE NEJ                 TO INDATA-SW                            
186300         MOVE MED-21 (SPR)        TO MOD-TEMFSINF                         
186400         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVSKROT-ATTR                     
186500     END-IF                                                               
186600                                                                          
186700     .                                                                    
186800     EJECT                                                                
186900 GBC-KOLLA-IDUSER SECTION.                                                
187000     MOVE 'GBC-KOLLA-IDUSER '  TO CURRENT-SECTION                         
187100                                                                          
187200     MOVE W-IDDC-FROM TO W-IDDC-6327                                      
187300     MOVE WS-KDARBTYP TO W-KDARBTYP-6327                                  
187400     MOVE SPACE       TO WS-SPAR-BEANST-GODK                              
187500     PERFORM IMS-GET-WDR501-6327                                          
187600     IF SEGMENT-FINNS                                                     
187700        MOVE MSG-SIGNON-USERID TO W-IDUSER-GODK                           
187800        PERFORM IMS-GET-WDGX6328                                          
187900        IF SEGMENT-FINNS                                                  
188000           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
188100        ELSE                                                              
188200           MOVE NEJ TO INDATA-SW                                          
188300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-ATTR                    
188400           MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                   
188500           CALL WMEDKONV USING MED-WMEDAREA                               
188600           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
188700        END-IF                                                            
188800     ELSE                                                                 
188900        MOVE NEJ TO INDATA-SW                                             
189000        MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-ATTR                       
189100        MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                      
189200        CALL WMEDKONV USING MED-WMEDAREA                                  
189300        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
189400     END-IF                                                               
189500     .                                                                    
189600     EJECT                                                                
189700 H-UPPDATERA SECTION.                                                     
189800     SKIP2                                                                
189900     IF RETUR-IFYLLT                                                      
190000       PERFORM HA-SKAPA-RETUR-ORDER                                       
190100     ELSE                                                                 
190200       IF SKROT-IFYLLT                                                    
190300         PERFORM HC-SKAPA-SKROT-ORDER                                     
190400       END-IF                                                             
190500     END-IF                                                               
190600     IF (DCS-CDC                                                          
190700     AND CLAG-IDDC-REF NOT = SPACE)                                       
190800       PERFORM HE-UPPDATERA-WDK629                                        
190900     ELSE                                                                 
191000       IF DCS-CDC                                                         
191100         CONTINUE                                                         
191200       ELSE                                                               
191300         PERFORM HD-SKAPA-UPPDATERA-WDK7-POST                             
191400       END-IF                                                             
191500     END-IF                                                               
191600                                                                          
191700*    MOVE MFS-RENSA-FAELT TO MOD                                          
191800     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
191900     CALL WMEDKONV USING MED-WMEDAREA                                     
192000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
192100     IF SPARR-ANT-KVAR                                                    
192200       MOVE MED-5 (SPR) TO MOD-TEMFSFEL                                   
192300     END-IF                                                               
192400                                                                          
192500     PERFORM MFS-FORM-ATTR                                                
192600     PERFORM MFS-ROER-EJ-FAELT-UT                                         
192700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
192800*****PERFORM MFS-RENSA-FAELT-IN                                           
192900* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
193000     .                                                                    
193100     EJECT                                                                
193200 HA-SKAPA-RETUR-ORDER SECTION.                                            
193300     SKIP2                                                                
193400*    OM MID-IDDC-SEND-TO = 11 ÄR DET EN RETUR TILL CDC OCH SKA            
193500*    DÄRMED HA SPECIELLA DISTRIKTSNUMMER OCH KUNDNUMMER.                  
193600                                                                          
193700     MOVE MID-IDDC-SEND-TO  TO TO-WS-IDDC                                 
193800     IF TO-CDC-SE                                                         
193900       PERFORM HAA-SKAPA-RETUR-CDC-KUND                                   
194000       IF MID-KDORDKL = YES OR JA                                         
194100*--                                            SNABB RETUR                
194200         PERFORM S03-SKAPA-MSG-KOM-AREA                                   
194300         PERFORM S12-SKAPA-ORDERNR-W411ORDN                               
194400         PERFORM S13-SKAPA-ORDERHUVUD                                     
194500         PERFORM S14-SKAPA-ORDERRADER                                     
194600         PERFORM S16-UPPDAT-RETBEO-O-SPARRKVAL                            
194700       ELSE                                                               
194800*--                                            NORMAL RETUR               
194900         PERFORM HAD-SKAPA-ORDER-WDE3                                     
195000       END-IF                                                             
195100     ELSE                                                                 
195200       PERFORM HAB-SKAPA-RETUR-EJ-CDC-KUND                                
195300       IF MID-KDORDKL = YES OR JA                                         
195400*--                                            SNABB-ORDER                
195500         PERFORM S03-SKAPA-MSG-KOM-AREA                                   
195600         PERFORM S12-SKAPA-ORDERNR-W411ORDN                               
195700         PERFORM S13-SKAPA-ORDERHUVUD                                     
195800         PERFORM S14-SKAPA-ORDERRADER                                     
195900         PERFORM S15-UPPDAT-KVBEART                                       
196000       ELSE                                                               
196100*--                                            NORMAL ORDER               
196200         PERFORM HAC-SKAPA-ORDER-WDE3                                     
196300       END-IF                                                             
196400     END-IF                                                               
196500     .                                                                    
196600     EJECT                                                                
196700 HAA-SKAPA-RETUR-CDC-KUND SECTION.                                        
196800     SKIP2                                                                
196900*--  RETUR                                                                
197000                                                                          
197100     MOVE DCS-IDDISTR-QRETUR TO SPARA-IDDISTR-NUM                         
197200     MOVE SPARA-IDDISTR-NUM   TO SPARA-IDDISTR                            
197300     IF MID-KDORDKL = YES OR JA                                           
197400*--  SNABB-ORDER                                                          
197500       MOVE DCS-IDKUNDNR-SQRET                                            
197600                             TO SPARA-IDKUNDNR-NUM                        
197700     ELSE                                                                 
197800       MOVE DCS-IDKUNDNR-QRETUR                                           
197900                             TO SPARA-IDKUNDNR-NUM                        
198000     END-IF                                                               
198100                                                                          
198200                                                                          
198300     MOVE SPARA-IDKUNDNR-NUM  TO SPARA-IDKUNDNR                           
198400     .                                                                    
198500     EJECT                                                                
198600                                                                          
198700 HAB-SKAPA-RETUR-EJ-CDC-KUND SECTION.                                     
198800     SKIP2                                                                
198900*--  VID TRANSFERS SÄTTS "FRÅN"-IDDC SOM KUNDNUMMER                       
199000*--  DESSUTOM FINNS DET ETT SPECIELLT DISTRIKTS-NUMMER                    
199100*--  FÖR VARJE MOTTAGARE. DETTA DISTRIKTSNUMMER KAN VARA                  
199200*--  OLIKA BEROENDE PÅ VILKET IDDC SOM ÄR AVSÄNDARE.                      
199300                                                                          
199400     MOVE TP4TRAN-IDDISTR   TO SPARA-IDDISTR-NUM                          
199500     MOVE SPARA-IDDISTR-NUM   TO SPARA-IDDISTR                            
199600     MOVE TP4TRAN-IDKUNDNR  TO SPARA-IDKUNDNR-NUM                         
199700     MOVE SPARA-IDKUNDNR-NUM  TO SPARA-IDKUNDNR                           
199800     .                                                                    
199900     EJECT                                                                
200000                                                                          
200100 HAC-SKAPA-ORDER-WDE3 SECTION.                                            
200200     SKIP2                                                                
200300     PERFORM IMS-GET-ARTC-CLAG                                            
200400                                                                          
200500     MOVE MID-IDDC-SEND-TO   TO W-IDDC                                    
200600                                TO-WS-IDDC                                
200700     PERFORM IMS-GET-ARTS-WLARTS01                                        
200800     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
200900                                                                          
201000     MOVE SLAG-IDPERSON-BUY                                               
201100                             TO WS-SPARA-IDPERSON-BUY                     
201200                                                                          
201300     MOVE  MID-IDDC-SEND-TO        TO W-IDDC-301                          
201400     MOVE  WS-SPARA-IDPERSON-BUY   TO W-IDPERSON-BUY                      
201500     MOVE 'T'                      TO W-KDREFTYP                          
201600     MOVE  W-IDARTNR               TO W-IDARTNR-301                       
201700     MOVE SPARA-IDDISTR            TO W-IDDISTR-301                       
201800     PERFORM IMS-GHU-ORDL-WLORDL01                                        
201900                                                                          
202000     IF SEGMENT-FINNS                                                     
202100       COMPUTE SLAG-KVBEART = SLAG-KVBEART - REF-KVBEART                  
202200       END-COMPUTE                                                        
202300                                                                          
202400       COMPUTE SLAG-KVBEART = SLAG-KVBEART + W-KVRETUR-BEORD              
202500       END-COMPUTE                                                        
202600                                                                          
202700       MOVE   W-KVRETUR-BEORD TO REF-KVBEART                              
202800       MOVE 'O'                  TO REF-KDREFORS                          
202810       MOVE MID-IDDC-SEND-TO     TO REF-IDDC-REF                          
202900                                                                          
203000       PERFORM IMS-REPL-ORDL-WLORDL01                                     
203100                                                                          
203200     ELSE                                                                 
203300       PERFORM HACA-INIT-WDE3-ORDER                                       
203400       PERFORM IMS-ISRT-ORDL-WLORDL01                                     
203500                                                                          
203600       COMPUTE SLAG-KVBEART = SLAG-KVBEART + W-KVRETUR-BEORD              
203700                                                                          
203800     END-IF                                                               
203900                                                                          
204000*      --- BOKAR NER KVALITETSSPÄRRAT ANTAL VID KVAL-RETUR                
204100       IF SLAG-KVSPARR-KVAL > +0                                          
204200         MOVE JA TO SPARR-ANT-SW                                          
204300         MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
204400         MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
204500       END-IF                                                             
204600       COMPUTE SLAG-KVSPARR-KVAL                                          
204700               = SLAG-KVSPARR-KVAL - W-KVRETUR-BEORD                      
204800               END-COMPUTE                                                
204900                                                                          
205000       IF SLAG-KVSPARR-KVAL < +0                                          
205100         MOVE +0 TO SLAG-KVSPARR-KVAL                                     
205200*        MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
205300*        MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
205400       END-IF                                                             
205500                                                                          
205600     PERFORM IMS-REPL-WLARTS11                                            
205700                                                                          
205800     .                                                                    
205900     EJECT                                                                
206000 HACA-INIT-WDE3-ORDER SECTION.                                            
206100     SKIP2                                                                
206200                                                                          
206300     MOVE MID-IDDC-SEND-TO          TO REF-IDDC                           
206400     MOVE WS-SPARA-IDPERSON-BUY     TO REF-IDPERSON-BUY                   
206500     MOVE 'T'                       TO REF-KDREFTYP                       
206600     MOVE W-IDARTNR                 TO REF-IDARTNR                        
206700     MOVE SPARA-IDDISTR             TO SPARA-IDDISTR-NUM                  
206800     MOVE SPARA-IDDISTR-NUM         TO REF-IDDISTR                        
206900     MOVE SLAG-ADLAGOMR             TO REF-ADLAGOMR-SDC                   
207000     MOVE SLAG-ADGANG               TO REF-ADGANG-SDC                     
207100     MOVE SLAG-ADPLATS              TO REF-ADPLATS-SDC                    
207200     MOVE CLAG-ADLAGOMR             TO REF-ADLAGOMR-CDC                   
207300     MOVE CLAG-ADGANG               TO REF-ADGANG-CDC                     
207400     MOVE CLAG-ADPLATS              TO REF-ADPLATS-CDC                    
207500     MOVE SPARA-IDKUNDNR            TO SPARA-IDKUNDNR-NUM                 
207600     MOVE SPARA-IDKUNDNR-NUM        TO REF-IDKUNDNR                       
207700     MOVE W-KVRETUR-BEORD           TO REF-KVBEART                        
207800     MOVE 'O'                       TO REF-KDREFORS                       
207900     MOVE SLAG-IDLEVNR              TO REF-IDLEVNR                        
208000     MOVE ZERO                      TO REF-KDREFTXT                       
208100                                       REF-KDFRAKT                        
208200                                       REF-KVBEART-CD                     
208300                                       REF-ADLAGOMR-CD                    
208400                                       REF-ADGANG-CD                      
208500                                       REF-ADPLATS-CD                     
208600     MOVE TP4TRAN-IDDC-SEND         TO REF-IDDC-REF                       
208700                                                                          
208800     .                                                                    
208900     EJECT                                                                
209000                                                                          
209100 HAD-SKAPA-ORDER-WDE3     SECTION.                                        
209200                                                                          
209300     PERFORM IMS-GET-ARTC-CLAG                                            
209400                                                                          
209500     PERFORM IMS-GET-ARTS-WLARTS01                                        
209600     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
209700                                                                          
209800     MOVE SLAG-IDPERSON-BUY                                               
209900                             TO WS-IDPERSON-BUY                           
210000                                                                          
210100     MOVE  W-IDDC-FROM             TO W-IDDC-301                          
210200     MOVE  WS-IDPERSON-BUY         TO W-IDPERSON-BUY                      
210300     MOVE  'R'                     TO W-KDREFTYP                          
210400     MOVE  W-IDARTNR               TO W-IDARTNR-301                       
210500     MOVE SPARA-IDDISTR            TO SPARA-IDDISTR-NUM                   
210600     MOVE SPARA-IDDISTR-NUM        TO W-IDDISTR-301                       
210700                                                                          
210800     PERFORM IMS-GHU-ORDL-WLORDL01                                        
210900                                                                          
211000     IF SEGMENT-FINNS                                                     
211100       MOVE W-KVRETUR-BEORD      TO REF-KVBEART                           
211200       MOVE 'N'                  TO REF-KDREFORS                          
211210       MOVE W-IDDC-FROM          TO REF-IDDC-REF                          
211300                                                                          
211400       PERFORM IMS-REPL-ORDL-WLORDL01                                     
211500                                                                          
211600     ELSE                                                                 
211700       PERFORM HADA-INIT-WDE3-ORDER                                       
211800       PERFORM IMS-ISRT-ORDL-WLORDL01                                     
211900                                                                          
212000     END-IF                                                               
212100                                                                          
212200     MOVE W-KVRETUR-BEORD    TO SLAG-KVRETUR-BEORD                        
212300                                                                          
212400*      --- BOKAR NER KVALITETSSPÄRRAT ANTAL VID KVAL-RETUR                
212500       IF SLAG-KVSPARR-KVAL > +0                                          
212600         MOVE JA TO SPARR-ANT-SW                                          
212700         MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
212800         MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
212900       END-IF                                                             
213000       COMPUTE SLAG-KVSPARR-KVAL                                          
213100               = SLAG-KVSPARR-KVAL - W-KVRETUR-BEORD                      
213200               END-COMPUTE                                                
213300                                                                          
213400       IF SLAG-KVSPARR-KVAL < +0                                          
213500         MOVE +0 TO SLAG-KVSPARR-KVAL                                     
213600*        MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
213700*        MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
213800       END-IF                                                             
213900     MOVE DAGENS-DATUM           TO SLAG-TIRETUR-BEORD                    
214000                                                                          
214100     PERFORM IMS-REPL-WLARTS11                                            
214200                                                                          
214300     .                                                                    
214400     EJECT                                                                
214500 HADA-INIT-WDE3-ORDER SECTION.                                            
214600     SKIP2                                                                
214700                                                                          
214800     MOVE W-IDDC-FROM               TO REF-IDDC                           
214900     MOVE WS-IDPERSON-BUY           TO REF-IDPERSON-BUY                   
215000     MOVE 'R'                       TO REF-KDREFTYP                       
215100     MOVE W-IDARTNR                 TO REF-IDARTNR                        
215200     MOVE SPARA-IDDISTR             TO SPARA-IDDISTR-NUM                  
215300     MOVE SPARA-IDDISTR-NUM         TO REF-IDDISTR                        
215400     MOVE SLAG-ADLAGOMR             TO REF-ADLAGOMR-SDC                   
215500     MOVE SLAG-ADGANG               TO REF-ADGANG-SDC                     
215600     MOVE SLAG-ADPLATS              TO REF-ADPLATS-SDC                    
215700     MOVE      CLAG-ADLAGOMR        TO REF-ADLAGOMR-CDC                   
215800     MOVE      CLAG-ADGANG          TO REF-ADGANG-CDC                     
215900     MOVE      CLAG-ADPLATS         TO REF-ADPLATS-CDC                    
216000     MOVE SPARA-IDKUNDNR            TO SPARA-IDKUNDNR-NUM                 
216100     MOVE SPARA-IDKUNDNR-NUM        TO REF-IDKUNDNR                       
216200     MOVE W-KVRETUR-BEORD           TO REF-KVBEART                        
216300     MOVE 'N'                       TO REF-KDREFORS                       
216400     MOVE SLAG-IDLEVNR              TO REF-IDLEVNR                        
216500     MOVE ZERO                      TO REF-KDREFTXT                       
216600                                       REF-KDFRAKT                        
216700                                       REF-KVBEART-CD                     
216800                                       REF-ADLAGOMR-CD                    
216900                                       REF-ADGANG-CD                      
217000                                       REF-ADPLATS-CD                     
217010     MOVE SLAG-IDDC-REF             TO REF-IDDC-REF                       
217100                                                                          
217200     .                                                                    
217300     EJECT                                                                
217400 HC-SKAPA-SKROT-ORDER     SECTION.                                        
217500     SKIP2                                                                
217600*SO???? FRÅGA SUSSI HUR VI SKILJER DE OLIKA QSKROT-DISTR. FÖR KINA        
217700*SO     DISTR 8490 BLIR EN LEV.ANM. KOD 55, MEN EJ DISTR 90!              
217800*SO SVAR: HÅRDKODA DISTR 90 I STEG W.BÄTTRE LÖSNING I STEG 1.             
217900*20180129 SÄTT DISTRIKT 8497 OCH KUND 7X/4X LOKAL SKROT USA/KINA.         
218000                                                                          
218100     MOVE MSGI-IDFTG   TO WS-IDFTG                                        
218200     IF (IDFTG-CN AND MSGI-IDDC(1:1) = '7') OR                            
218300        (IDFTG-US AND MSGI-IDDC(1:1) = '4')                               
218400       MOVE DCS-IDDISTR-RSKROT  TO SPARA-IDDISTR-NUM                      
218500       MOVE SPARA-IDDISTR-NUM   TO SPARA-IDDISTR                          
218600       MOVE DCS-IDKUNDNR-RSKROT                                           
218700                                TO SPARA-IDKUNDNR-NUM                     
218800                                                                          
218900       MOVE SPARA-IDKUNDNR-NUM  TO SPARA-IDKUNDNR                         
219000     ELSE                                                                 
219100       MOVE DCS-IDDISTR-QSKROT  TO SPARA-IDDISTR-NUM                      
219200       MOVE SPARA-IDDISTR-NUM   TO SPARA-IDDISTR                          
219300       MOVE DCS-IDKUNDNR-QSKROT                                           
219400                                TO SPARA-IDKUNDNR-NUM                     
219500                                                                          
219600       MOVE SPARA-IDKUNDNR-NUM  TO SPARA-IDKUNDNR                         
219700     END-IF                                                               
219800                                                                          
220500     IF DCS-CDC                                                           
220600     OR DCS-CDC-TR                                                        
220700        PERFORM HCE-UPPDATERA-WDK7                                        
220800        PERFORM HCD-SKAPA-HANDELSE-6321                                   
220900     ELSE                                                                 
221000        PERFORM HCC-UPPDATERA-WDK7                                        
221100        PERFORM HCD-SKAPA-HANDELSE-6321                                   
221200     END-IF                                                               
221400     .                                                                    
221500     EJECT                                                                
221600                                                                          
221700 HCA-SKAPA-SKROT-ORDERHUVUD SECTION.                                      
221800     SKIP2                                                                
221900     MOVE SPACE TO OHUV-MID-W4I25101                                      
222000     MOVE SPACE            TO KOM-AREA                                    
222100     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
222200     MOVE 'REFT'           TO OHUV-MID-IDSYSTEM                           
222300     MOVE W-IDDC-FROM      TO OHUV-MID-IDDC                               
222400     MOVE SPARA-IDDISTR    TO OHUV-MID-IDDISTR                            
222500     MOVE SPARA-IDKUNDNR   TO OHUV-MID-IDKUNDNR                           
222600     MOVE SPARA-IDORDNR    TO OHUV-MID-IDORDNR                            
222700     MOVE '3'              TO OHUV-MID-KDORDKL                            
222800                                                                          
222900     MOVE MID-SKROT-TEXT   TO OHUV-MID-BELAGINS                           
223000                                                                          
223100     MOVE SPARA-IDDISTR-NUM   TO TEST-IDDISTR                             
223200     IF DIST18-SCRAP-NDC-SC-LOCAL                                         
223300       MOVE JA             TO OHUV-MID-FLAUTFAK                           
223400     ELSE                                                                 
223500       MOVE NEJ            TO OHUV-MID-FLAUTFAK                           
223600     END-IF                                                               
223700                                                                          
223800     MOVE NEJ              TO OHUV-MID-FLAUTPAC                           
223900                              OHUV-MID-FLEMBORD                           
224000                              OHUV-MID-FLOVRLEV                           
224100                              OHUV-MID-FLFORBI                            
224200                              OHUV-MID-FLORDTIL                           
224300     MOVE ZERO             TO OHUV-MID-IDGROSS                            
224500                                                                          
224600*    MOVE +459             TO P-TO-P-KVLL                                 
224700     MOVE LENGTH OF OHUV-MID-W4I25101 TO P-TO-P-KVLL                      
224800     ADD  +17              TO P-TO-P-KVLL                                 
224900     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
225000     MOVE '4251'           TO P-TO-P-IDTRANS                              
225100     MOVE '1'              TO P-TO-P-KDMFSFOR                             
225200                                                                          
225300     MOVE KOM-AREA         TO P-TO-P-DATA                                 
225400     CALL W006KOM USING MSG-PCB                                           
225500                        ALT-PCB                                           
225600                        KOMA-PCB                                          
225700                        MSG-KOM-WMSGKOM                                   
225800                        P-TO-P-SW                                         
225900     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
226000*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
226100*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
226200        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
226300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
226400     END-IF                                                               
226500                                                                          
226600     MOVE SPACE TO KOM-AREA                                               
226700     .                                                                    
226800     EJECT                                                                
226900 HCB-SKAPA-SKROT-ORDERRAD SECTION.                                        
227000     SKIP2                                                                
227100     MOVE SPACE TO ORAD-MID-W4I25201                                      
227200     MOVE SPACE            TO KOM-AREA                                    
227300     MOVE 'W4I25201'       TO MSG-KOM-IDCPYTXT                            
227400                                                                          
227500     MOVE 'REFT'           TO ORAD-MID-IDSYSTEM                           
227600     MOVE SPARA-IDDISTR    TO ORAD-MID-IDDISTR                            
227700     MOVE SPARA-IDKUNDNR   TO ORAD-MID-IDKUNDNR                           
227800     MOVE SPARA-IDORDNR    TO ORAD-MID-IDORDNR                            
227900     MOVE MSGI-IDARTNR     TO ORAD-MID-IDARTNR (1)                        
228000     PERFORM S11-W009KSIF                                                 
228100     MOVE JA TO ORAD-MID-FLSLUT                                           
228200                                                                          
228300     PERFORM IMS-GET-ARTS-WLARTS01                                        
228400     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
228500                                                                          
228600*    --- REDIGERAR ORAD-MID-BERADREF MED KVARVARANDE ANTAL                
228700     MOVE ZERO TO NOLLOR                                                  
228800     MOVE SPACE TO W-KVSKROT-ORAD                                         
228900     INSPECT MID-KVSKROT TALLYING NOLLOR FOR LEADING ZEROS                
229000     IF NOLLOR = ZERO                                                     
229100       MOVE MID-KVSKROT TO W-KVSKROT-ORAD                                 
229200     ELSE                                                                 
229300       IF NOLLOR < 7                                                      
229400       MOVE MID-KVSKROT(( NOLLOR + 1 ):( 7 - NOLLOR ))                    
229500           TO W-KVSKROT-ORAD (( NOLLOR  ):( 7 - NOLLOR ))                 
229600       ELSE                                                               
229700       MOVE MID-KVSKROT(( NOLLOR):( 8 - NOLLOR ))                         
229800           TO W-KVSKROT-ORAD (( NOLLOR  ):( 8 - NOLLOR ))                 
229900       END-IF                                                             
230000     END-IF                                                               
230100     MOVE W-SKROT-TEXT-ORAD TO ORAD-MID-BERADREF (1)                      
230200                                                                          
230300*    --- BERÄKNAR SKROTAT ANTAL SOM LÄGGES I ORAD-MID-KVBEART             
230400     MOVE MID-KVSKROT TO W-KVSKROT-DISPLAY                                
230500     COMPUTE W-KVSKROT = W-KVSKROT-DISPLAY                                
230600             END-COMPUTE                                                  
230700     MOVE W-KVSKROT-DISPLAY TO W-KVSKROT-TRUNK                            
230800     MOVE W-KVSKROT-TRUNK   TO ORAD-MID-KVBEART (1)                       
230900                                                                          
231000     MOVE JA  TO SLAG-FLSKROT-BEORD                                       
231100     MOVE NEJ TO SLAG-FLSKROT-AUTO                                        
231200                                                                          
231300*      --- BOKAR NER KVALITETSSPÄRRAT ANTAL VID SKROTNING                 
231400       COMPUTE SLAG-KVSPARR-KVAL = SLAG-KVSPARR-KVAL - W-KVSKROT          
231500               END-COMPUTE                                                
231600                                                                          
231700       IF SLAG-KVSPARR-KVAL < +0                                          
231800         MOVE +0  TO SLAG-KVSPARR-KVAL                                    
231900       END-IF                                                             
232000                                                                          
232100       IF SLAG-KVSPARR-KVAL > +0                                          
232200         MOVE JA TO SPARR-ANT-SW                                          
232300         MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
232400         MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
232500       END-IF                                                             
232600                                                                          
232700     MOVE DAGENS-DATUM TO SLAG-TISKROT-BEORD                              
232800     PERFORM IMS-REPL-WLARTS11                                            
232900                                                                          
233000*    MOVE +945             TO P-TO-P-KVLL                                 
233100     MOVE LENGTH OF ORAD-MID-W4I25201 TO P-TO-P-KVLL                      
233200     ADD  +17              TO P-TO-P-KVLL                                 
233300     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
233400     MOVE '4252'           TO P-TO-P-IDTRANS                              
233500     MOVE '1'              TO P-TO-P-KDMFSFOR                             
233600                                                                          
233700     MOVE KOM-AREA         TO P-TO-P-DATA                                 
233800     CALL W006KOM USING MSG-PCB                                           
233900                        ALT-PCB                                           
234000                        KOMA-PCB                                          
234100                        MSG-KOM-WMSGKOM                                   
234200                        P-TO-P-SW                                         
234300     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
234400*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
234500*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
234600        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
234700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
234800     END-IF                                                               
234900                                                                          
235000     MOVE SPACE TO KOM-AREA                                               
235100     .                                                                    
235200     EJECT                                                                
235300 HCC-UPPDATERA-WDK7 SECTION.                                              
235400     SKIP2                                                                
235500     PERFORM IMS-GET-ARTS-WLARTS01                                        
235600     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
235700     MOVE NEJ                    TO SLAG-FLSKROT-AUTO                     
235800     MOVE JA                     TO SLAG-FLSKROT-BEORD                    
235900     MOVE DAGENS-DATUM           TO SLAG-TISKROT-BEORD                    
236000     MOVE MID-KVSKROT            TO W-KVSKROT-DISPLAY                     
236100     MOVE W-KVSKROT-DISPLAY      TO W-KVSKROT                             
236200                                                                          
236300     COMPUTE W-KVSKROT-KVAR     = SLAG-KVLS - W-KVSKROT                   
236400               END-COMPUTE                                                
236500                                                                          
236600     PERFORM IMS-REPL-WLARTS11                                            
236700     .                                                                    
236800     EJECT                                                                
236900 HCD-SKAPA-HANDELSE-6321  SECTION.                                        
237000     SKIP2                                                                
237100     MOVE WS-KDARBTYP       TO W-KDARBTYP-6321                            
237200     PERFORM IMS-GU-WDR501-6321                                           
237300     IF SEGMENT-SAKNAS                                                    
237400        MOVE '6321'         TO 6321-IDHTYP                                
237500        MOVE WS-KDARBTYP    TO 6321-KDARBTYP                              
237600        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
237700        PERFORM IMS-ISRT-WDR501-6321                                      
237800     END-IF                                                               
237900                                                                          
238000     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
238100     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
238200     PERFORM IMS-ISRT-WDGX6322                                            
238300                                                                          
238400     MOVE W-IDARTNR-K7      TO 6324-IDARTNR                               
238500     MOVE W-IDDC-FROM       TO 6324-IDDC                                  
238600     MOVE MID-SKROT-TEXT    TO 6324-BELAGINS-DEL                          
238700     MOVE NEJ               TO 6324-FLSKROT-GODK                          
238800     IF MID-IDANALYS NOT = ALL '+' AND                                    
238900        MID-IDANALYS > SPACE                                              
239000        MOVE MID-IDANALYS   TO 6324-IDANALYS                              
239100        MOVE MID-IDKONTO    TO 6324-IDKONTO                               
239200        MOVE space          TO 6324-IDKST                                 
239300     ELSE                                                                 
239400        MOVE SPACE          TO 6324-IDANALYS                              
239500        MOVE ZERO           TO 6324-IDKONTO                               
239600        MOVE space          TO 6324-IDKST                                 
239700     END-IF                                                               
239800     MOVE WS-IDPERSON       TO 6324-IDPERSON                              
239900     MOVE SPARA-IDDISTR     TO 6324-IDDISTR                               
240000     MOVE SPARA-IDKUNDNR    TO 6324-IDKUNDNR                              
240100     MOVE MSG-SIGNON-USERID TO 6324-IDUSER                                
240200     MOVE ZERO              TO 6324-KDFRAKT                               
240300                               6324-KVSKROT-KVAR                          
240400     MOVE 1                 TO 6324-KDORDKL                               
240500                               6324-KDSTASKR                              
240600     MOVE W-KVSKROT         TO 6324-KVSKROT-BEORD                         
240700     MOVE MID-FLJUSTBUFF    TO 6324-FLJUSTBUFF                            
240800     MOVE W-KVSKROT-KVAR    TO 6324-KVSKROT-ONDEM                         
240900     MOVE WS-SPAR-BEANST-GODK                                             
241000                            TO 6324-BEANST                                
241100     MOVE CLAG-KDERS        TO 6324-KDERS-UTG                             
241200     PERFORM HCDA-LAS-FLYTTA-WDK7                                         
241300     PERFORM IMS-GU-WDK901                                                
241400     IF SEGMENT-FINNS                                                     
241500        MOVE ART-SUTPO-TOT        TO 6324-SUTPO-TOT                       
241600                                                                          
241700        COMPUTE WS-KVOKS-CDC = ART-KVOKS-BULK       +                     
241800                               ART-KVOKS-DAG        +                     
241900                               ART-KVOKS-VOR                              
242000                                                                          
242100     ELSE                                                                 
242200        MOVE ZERO                      TO 6324-SUTPO-TOT                  
242300                                          WS-KVOKS-CDC                    
242400     END-IF                                                               
242500     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
242600            CLAG-KVLS     - CLAG-KVRESS                                   
242700                          - CLAG-KVROS                                    
242800                          - WS-KVOKS-CDC                                  
242900                                                                          
243000     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
243100            W-SDC-KVLS - W-SDC-KVOKS                                      
243200                                                                          
243300     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
243400          CLAG-KVAKS-CDC  + CLAG-KVAKS-PAV                                
243500                          + CLAG-KVAKS-T                                  
243600                                                                          
243700     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
243800          W-SDC-KVAKS                                                     
243900     PERFORM HCDB-LAS-FLYTTA-WDN6                                         
244000     PERFORM IMS-ISRT-WDGX6324                                            
244100     .                                                                    
244200     EJECT                                                                
244300 HCDA-LAS-FLYTTA-WDK7 SECTION.                                            
244400                                                                          
244500     PERFORM IMS-GET-ARTS-WLARTS01                                        
244600     IF SEGMENT-FINNS                                                     
244700        MOVE ZERO             TO W-SDC-KVLS                               
244800                                 W-SDC-KVAKS                              
244900                                 W-SDC-KVOKS                              
245000        PERFORM IMS-GNP-ARTS-WLARTS11                                     
245100        PERFORM UNTIL SEGMENT-SAKNAS                                      
245200*         MOVE WC-CDC-SE         TO WS-IDDC                               
245300*         IF CDC-SE OR                                                    
245400*            WC-CDC-SE = SLAG-IDDC                                        
245500             ADD SLAG-KVLS      TO W-SDC-KVLS                             
245600             ADD SLAG-KVAKS-SDC TO W-SDC-KVAKS                            
245700             ADD SLAG-KVAKS-PAV TO W-SDC-KVAKS                            
245800             ADD SLAG-KVOKS-DAG TO W-SDC-KVOKS                            
245900             ADD SLAG-KVOKS-BULK TO W-SDC-KVOKS                           
246000*         END-IF                                                          
246100          PERFORM IMS-GNP-ARTS-WLARTS11                                   
246200        END-PERFORM                                                       
246300     END-IF                                                               
246400     .                                                                    
246500     EJECT                                                                
246600                                                                          
246700 HCDB-LAS-FLYTTA-WDN6 SECTION.                                            
246800     MOVE +1 TO BEEMB-IX                                                  
246900     PERFORM UNTIL BEEMB-IX > 20                                          
247000        MOVE SPACE        TO 6324-BEEMBLEM (BEEMB-IX)                     
247100        ADD +1        TO BEEMB-IX                                         
247200     END-PERFORM                                                          
247300     PERFORM IMS-GU-WDN601                                                
247400     IF SEGMENT-FINNS                                                     
247500        PERFORM IMS-GNP-WDN611                                            
247600        MOVE +1 TO BEEMB-IX                                               
247700        PERFORM UNTIL BEEMB-IX > 20 OR SEGMENT-SAKNAS                     
247800           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (BEEMB-IX)                  
247900           ADD +1        TO BEEMB-IX                                      
248000           PERFORM IMS-GNP-WDN611                                         
248100        END-PERFORM                                                       
248200        IF SEGMENT-FINNS                                                  
248300           MOVE 'MORE' TO 6324-BEEMBLEM (20)                              
248400        END-IF                                                            
248500     END-IF                                                               
248600      .                                                                   
248700      EJECT                                                               
248800                                                                          
248900 HCE-UPPDATERA-WDK7 SECTION.                                              
249000     SKIP2                                                                
249100     PERFORM IMS-GU-ARTC-ARTC                                             
249200     PERFORM IMS-GHNP-CLAG                                                
249300     MOVE JA                     TO CLAG-FLSKROT-BEORD                    
249400     MOVE MID-KVSKROT            TO W-KVSKROT-DISPLAY                     
249500     MOVE W-KVSKROT-DISPLAY      TO W-KVSKROT                             
249600                                                                          
249700     COMPUTE W-KVSKROT-KVAR     = CLAG-KVLS - W-KVSKROT                   
249800             END-COMPUTE                                                  
249900                                                                          
250000     PERFORM IMS-REPL-WLARTC11                                            
250100     .                                                                    
250200     EJECT                                                                
250300 HD-SKAPA-UPPDATERA-WDK7-POST SECTION.                                    
250400                                                                          
250500* -- UPPDATERING AV DC-FLREFNYO PGA LAGERSALDOFÖRÄNDRING                  
250600*    FLREFNYO-FÄLTET FLYTTAT FRÅN WDL7 TILL WDK711                        
250700                                                                          
250800     PERFORM IMS-GET-ARTS-WLARTS01                                        
250900     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
251000     IF SEGMENT-FINNS                                                     
251100        MOVE NEJ          TO SLAG-FLREFNYO                                
251200        PERFORM IMS-REPL-WLARTS11                                         
251300     END-IF                                                               
251400     .                                                                    
251500     EJECT                                                                
251600 HE-UPPDATERA-WDK629 SECTION.                                             
251700                                                                          
251800* -- UPPDATERING AV FLREFNYO PÅ WDK629(RÖRELSEINDIKATORN)                 
251900     PERFORM IMS-GHU-WDK629                                               
252000     IF SEGMENT-FINNS                                                     
252100       MOVE NEJ                TO CREF-FLREFNYO                           
252200       PERFORM IMS-REPL-WDK629                                            
252300     END-IF                                                               
252400     .                                                                    
252500     EJECT                                                                
252600                                                                          
252700 S01-KONTROLLERA-ANALYSNR SECTION.                                        
252800     SKIP2                                                                
252900*          ANALYSNUMMER UPPDATERAS PÅ 4702                                
253000*          KONTROLL SKER HÄR                                              
253100                                                                          
253200     MOVE NEJ TO GILTIGT-ANALYSNR-SW                                      
253300                                                                          
253400*    MOVE MSGI-IDFTG TO W-IDFTG-4109                                      
253500     MOVE MSGI-IDFTG TO WS-IDFTG                                          
253600                                                                          
253700     IF SKROT-IFYLLT                                                      
253800       IF IDFTG-CN                                                        
253900         MOVE WC-IDFTG-CN    TO W-IDFTG-4109                              
254000       ELSE                                                               
254100         IF IDFTG-US                                                      
254200           MOVE WC-IDFTG-US  TO W-IDFTG-4109                              
254300         ELSE                                                             
254400           MOVE WC-IDFTG-PV  TO W-IDFTG-4109                              
254500         END-IF                                                           
254600       END-IF                                                             
254700     ELSE                                                                 
254800       MOVE WC-IDFTG-PV  TO W-IDFTG-4109                                  
254900     END-IF                                                               
255000                                                                          
255100     PERFORM IMS-GET-WL410901                                             
255200     IF SEGMENT-FINNS                                                     
255300       PERFORM IMS-GNP-WL410911-KVAL                                      
255400                                                                          
255500       PERFORM UNTIL SEGMENT-SAKNAS                                       
255600       OR GILTIGT-ANALYSNR                                                
255700           IF 4110-DAGILTIG-FOM <= DAGENS-DATUM-Y2K                       
255800           AND DAGENS-DATUM-Y2K <= 4110-DAGILTIG-TOM                      
255900               MOVE JA TO GILTIGT-ANALYSNR-SW                             
256000           ELSE                                                           
256100               PERFORM IMS-GNP-WL410911-KVAL                              
256200           END-IF                                                         
256300       END-PERFORM                                                        
256400     END-IF                                                               
256500     .                                                                    
256600     EJECT                                                                
256700                                                                          
256800 S02-KOLLA-USER-DC  SECTION.                                              
256900     MOVE 'GAC-KOLLA-USER-DC'  TO CURRENT-SECTION                         
257000                                                                          
257100     MOVE MSGI-IDFTG   TO WS-IDFTG                                        
257200                                                                          
257300*-   KINESER SKALL BARA KUNNA SKROTA PÅ KINA-LAGER.                       
257400     IF IDFTG-CN                                                          
257500       IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                           
257600         CONTINUE                                                         
257700       ELSE                                                               
257800         MOVE NEJ TO INDATA-SW                                            
257900         MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                     
258000         CALL WMEDKONV USING MED-WMEDAREA                                 
258100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
258200       END-IF                                                             
258300     END-IF                                                               
258400                                                                          
258500*-   USA SKALL BARA KUNNA SKROTA PÅ USA-LAGER.                            
258600     IF IDFTG-US                                                          
258700       IF (DCS-NDC-NA AND DCS-USA)                                        
258800         CONTINUE                                                         
258900       ELSE                                                               
259000         MOVE NEJ TO INDATA-SW                                            
259100         MOVE NOT-AUTHORIZED-TO-SCRAP TO MED-IDMFSINF                     
259200         CALL WMEDKONV USING MED-WMEDAREA                                 
259300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
259400       END-IF                                                             
259500     END-IF                                                               
259600     .                                                                    
259700     EJECT                                                                
259800                                                                          
259900 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
260000     SKIP2                                                                
260100     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
260200     MOVE +54                    TO MSG-KOM-KVLL                          
260300     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
260400     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
260500     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
260600     MOVE 'TRANSFER'             TO MSG-KOM-IDSNDNOD                      
260700*    MOVE 'REFT'                 TO MSG-KOM-IDSNDNOD                      
260800*    MOVE SPARA-IDDISTR          TO MSG-KOM-IDSNDNOD (5:4)                
260900     MOVE 'W6032100'             TO MSG-KOM-IDSNDJOB                      
261000     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
261100     MOVE DAGENS-TID             TO MSG-KOM-TIKLOCK                       
261200     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
261300     .                                                                    
261400     EJECT                                                                
261500 S10-KOLLA-LOKAL-ART-NDC SECTION.                                         
261600     MOVE 'S10-KOLLA-LOKAL-ART-NDC '  TO CURRENT-SECTION                  
261700*---                                                                      
261800*--- EN ART RÄKNAS SOM LOKALT ANSKAFFAD OM NÅGOT NDC I                    
261900*--- USA/KINA HAR EN LOKAL LEVERANTÖR.REFILL INOM USA/KINA.               
262000*--- OM LOKALT ANSKAFFAD (IDDC-REF = SPACE)                               
262100*---                                                                      
262200     IF DCS-NDC-NA                                                        
262300       MOVE '41'  TO W-IDDC-MIN                                           
262400       MOVE '49'  TO W-IDDC-MAX                                           
262500     ELSE                                                                 
262600       MOVE '71'  TO W-IDDC-MIN                                           
262700       MOVE '79'  TO W-IDDC-MAX                                           
262800     END-IF                                                               
262900                                                                          
263000     MOVE NEJ  TO SW-LOKAL-ART                                            
263100     PERFORM IMS-GET-ARTS-WLARTS01                                        
263200     IF SEGMENT-FINNS                                                     
263300       PERFORM IMS-GNP-WDK711-REF                                         
263400       PERFORM UNTIL SEGMENT-SAKNAS OR (SW-LOKAL-ART = JA)                
263500         IF SEGMENT-FINNS                                                 
263600           MOVE JA  TO SW-LOKAL-ART                                       
263700         END-IF                                                           
263800         PERFORM IMS-GNP-WDK711-REF                                       
263900       END-PERFORM                                                        
264000     END-IF                                                               
264100     .                                                                    
264200     EJECT                                                                
264300 S11-W009KSIF  SECTION.                                                   
264400     SKIP2                                                                
264500*    BERÄKNA KONTROLLSIFFRA FÖR ARTIKELNR                                 
264600     IF ORAD-MID-REKSIFFR (1) = SPACE OR                                  
264700        ORAD-MID-REKSIFFR (1) = '0'                                       
264800        IF ORAD-MID-IDARTNR (1) NUMERIC                                   
264900           MOVE ORAD-MID-IDARTNR (1) TO REK-IDARTNR                       
265000           MOVE 0                TO REK-REKSIFFR                          
265100           MOVE 9                TO REK-LNGD                              
265200           CALL W009KSIF USING REK-IDARTNR                                
265300                               REK-LNGD                                   
265400                               REK-REKSIFFR                               
265500           MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR (1)                 
265600        END-IF                                                            
265700     END-IF                                                               
265800     .                                                                    
265900     EJECT                                                                
266000                                                                          
266100                                                                          
266200 S12-SKAPA-ORDERNR-W411ORDN  SECTION.                                     
266300                                                                          
266400     MOVE 'W603'               TO ORDN-IDSYSTEM                           
266500                                                                          
266600     MOVE SPARA-IDDISTR        TO ORDN-IDDISTR                            
266700     MOVE SPARA-IDKUNDNR       TO ORDN-IDKUNDNR                           
266800                                                                          
266900     MOVE ZERO                 TO ORDN-IDORDNR-IN                         
267000                                                                          
267100     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
267200                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
267300                                                                          
267400     MOVE ORDN-IDORDNR-UT      TO SPARA-IDORDNR                           
267500     .                                                                    
267600     EJECT                                                                
267700                                                                          
267800                                                                          
267900 S13-SKAPA-ORDERHUVUD SECTION.                                            
268000     SKIP2                                                                
268100     MOVE SPACE TO OHUV-MID-W4I25101                                      
268200     MOVE SPACE            TO KOM-AREA                                    
268300     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
268400     MOVE 'REFT'           TO OHUV-MID-IDSYSTEM                           
268500     MOVE W-IDDC-FROM      TO OHUV-MID-IDDC                               
268600     MOVE SPARA-IDDISTR    TO OHUV-MID-IDDISTR                            
268700     MOVE SPARA-IDKUNDNR   TO OHUV-MID-IDKUNDNR                           
268800     MOVE SPARA-IDORDNR    TO OHUV-MID-IDORDNR                            
268900     MOVE SPACE            TO OHUV-MID-KDFRAKT                            
269000     MOVE '1'              TO OHUV-MID-KDORDKL                            
269100     IF DCS-CDC                                                           
269200     OR DCS-CDC-TR                                                        
269300*BS  OR (DCS-SDC                                                          
269400*BS  AND NOT DCS-SWEDEN)                                                  
269500        MOVE '17'          TO OHUV-MID-KDFRAKT                            
269600     END-IF                                                               
269700                                                                          
269800     MOVE SPACE TO W-RETUR-TEXT                                           
269900                                                                          
270000     IF MID-RETUR-TEXT      = ALL '+'                                     
270100       MOVE 'QUALITY'           TO W-RETUR-TEXT                           
270200     ELSE                                                                 
270300       MOVE MID-RETUR-TEXT      TO W-RETUR-TEXT                           
270400     END-IF                                                               
270500                                                                          
270600     MOVE W-RETUR-TEXT TO OHUV-MID-BELAGINS                               
270700                                                                          
270800     MOVE NEJ                  TO OHUV-MID-FLAUTFAK                       
270900     MOVE NEJ                  TO OHUV-MID-FLAUTPAC                       
271000                                  OHUV-MID-FLEMBORD                       
271100                                  OHUV-MID-FLOVRLEV                       
271200                                                                          
271300                                                                          
271400                                                                          
271500                                                                          
271600*    MOVE +459             TO P-TO-P-KVLL                                 
271700     MOVE LENGTH OF OHUV-MID-W4I25101 TO P-TO-P-KVLL                      
271800     ADD  +17              TO P-TO-P-KVLL                                 
271900     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
272000     MOVE '4251'           TO P-TO-P-IDTRANS                              
272100     MOVE '1'              TO P-TO-P-KDMFSFOR                             
272200                                                                          
272300     MOVE KOM-AREA                TO P-TO-P-DATA                          
272400     CALL W006KOM USING MSG-PCB                                           
272500                        ALT-PCB                                           
272600                        KOMA-PCB                                          
272700                        MSG-KOM-WMSGKOM                                   
272800                        P-TO-P-SW                                         
272900     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
273000*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
273100*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
273200        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
273300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
273400     END-IF                                                               
273500                                                                          
273600     MOVE SPACE TO KOM-AREA                                               
273700     .                                                                    
273800     EJECT                                                                
273900 S14-SKAPA-ORDERRADER SECTION.                                            
274000     SKIP2                                                                
274100                                                                          
274200     MOVE SPACE TO ORAD-MID-W4I25201                                      
274300     MOVE SPACE            TO KOM-AREA                                    
274400     MOVE 'W4I25201'       TO MSG-KOM-IDCPYTXT                            
274500                                                                          
274600     MOVE 'REFT'           TO ORAD-MID-IDSYSTEM                           
274700     MOVE SPARA-IDDISTR    TO ORAD-MID-IDDISTR                            
274800     MOVE SPARA-IDKUNDNR   TO ORAD-MID-IDKUNDNR                           
274900     MOVE SPARA-IDORDNR    TO ORAD-MID-IDORDNR                            
275000     MOVE MSGI-IDARTNR     TO ORAD-MID-IDARTNR (1)                        
275100     PERFORM S11-W009KSIF                                                 
275200*--  HÖGERJUSTERA OCH NOLLFYLL KVRETUR-BEORD                              
275300     MOVE W-KVRETUR-BEORD TO W-KVRETUR-BEORD-TRUNK                        
275400     MOVE W-KVRETUR-BEORD-TRUNK TO ORAD-MID-KVBEART (1)                   
275500     INSPECT ORAD-MID-KVBEART (1) REPLACING ALL SPACE BY ZERO             
275600     MOVE WS-ADART-X       TO ORAD-MID-BERADREF (1)                       
275700     MOVE JA               TO ORAD-MID-FLSLUT                             
275800                                                                          
275900                                                                          
276000*    MOVE +945             TO P-TO-P-KVLL                                 
276100     MOVE LENGTH OF ORAD-MID-W4I25201 TO P-TO-P-KVLL                      
276200     ADD  +17              TO P-TO-P-KVLL                                 
276300     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
276400     MOVE '4252'           TO P-TO-P-IDTRANS                              
276500     MOVE '1'              TO P-TO-P-KDMFSFOR                             
276600                                                                          
276700     MOVE KOM-AREA         TO P-TO-P-DATA                                 
276800     CALL W006KOM USING MSG-PCB                                           
276900                        ALT-PCB                                           
277000                        KOMA-PCB                                          
277100                        MSG-KOM-WMSGKOM                                   
277200                        P-TO-P-SW                                         
277300     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
277400*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
277500*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
277600        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
277700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
277800     END-IF                                                               
277900                                                                          
278000     MOVE SPACE TO KOM-AREA                                               
278100     .                                                                    
278200     EJECT                                                                
278300                                                                          
278400***************LDC2*********************                                  
278500 S15-UPPDAT-KVBEART SECTION.                                              
278600     SKIP2                                                                
278700     MOVE MID-IDDC-SEND-TO     TO TO-WS-IDDC                              
278800     IF NOT TO-CDC-SE                                                     
278900       MOVE MID-IDDC-SEND-TO TO W-IDDC                                    
279000       PERFORM IMS-GET-ARTS-WLARTS01                                      
279100       PERFORM IMS-GHNP-ARTS-WLARTS11                                     
279200       IF SEGMENT-FINNS                                                   
279300*        MOVE MID-KVRETUR-TRANSFER TO W-KVRETUR-BEORD                     
279400         COMPUTE SLAG-KVBEART =                                           
279500                 SLAG-KVBEART + W-KVRETUR-BEORD                           
279600         END-COMPUTE                                                      
279700         PERFORM IMS-REPL-WLARTS11                                        
279800       END-IF                                                             
279900                                                                          
280000       MOVE W-IDDC-B6        TO W-IDDC                                    
280100       PERFORM IMS-GET-ARTS-WLARTS01                                      
280200       PERFORM IMS-GHNP-ARTS-WLARTS11                                     
280300       IF SEGMENT-FINNS                                                   
280400*  --- BOKAR NER KVALITETSSPÄRRAT ANTAL VID KVAL-RETUR                    
280500         COMPUTE SLAG-KVSPARR-KVAL                                        
280600                 = SLAG-KVSPARR-KVAL - W-KVRETUR-BEORD                    
280700                 END-COMPUTE                                              
280800                                                                          
280900         IF SLAG-KVSPARR-KVAL < +0                                        
281000           MOVE +0 TO SLAG-KVSPARR-KVAL                                   
281100*          MOVE MSGI-IDUSER TO SLAG-IDUSER-SPKVAL                         
281200*          MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                         
281300         END-IF                                                           
281400                                                                          
281500         IF SLAG-KVSPARR-KVAL > +0                                        
281600           MOVE JA TO SPARR-ANT-SW                                        
281700           MOVE MSGI-IDUSER TO SLAG-IDUSER-SPKVAL                         
281800           MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                         
281900         END-IF                                                           
282000         PERFORM IMS-REPL-WLARTS11                                        
282100       END-IF                                                             
282200     END-IF                                                               
282300     .                                                                    
282400     EJECT                                                                
282500***************LDC2*********************                                  
282600                                                                          
282700 S16-UPPDAT-RETBEO-O-SPARRKVAL   SECTION.                                 
282800     SKIP2                                                                
282900*VID RETURER SKA MAN UPPDATERA RETURNERAT ANTAL SAMT TIDPUNKT             
283000*                                                                         
283100     PERFORM IMS-GET-ARTS-WLARTS01                                        
283200     PERFORM IMS-GHNP-ARTS-WLARTS11                                       
283300*?   ADD ???                                                              
283400     MOVE W-KVRETUR-BEORD      TO SLAG-KVRETUR-BEORD                      
283500     MOVE DAGENS-DATUM         TO SLAG-TIRETUR-BEORD                      
283600                                                                          
283700*      --- BOKAR NER KVALITETSSPÄRRAT ANTAL VID KVAL-RETUR                
283800       COMPUTE SLAG-KVSPARR-KVAL                                          
283900               = SLAG-KVSPARR-KVAL - W-KVRETUR-BEORD                      
284000               END-COMPUTE                                                
284100                                                                          
284200       IF SLAG-KVSPARR-KVAL < +0                                          
284300         MOVE +0 TO SLAG-KVSPARR-KVAL                                     
284400*        MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
284500*        MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
284600       END-IF                                                             
284700                                                                          
284800       IF SLAG-KVSPARR-KVAL > +0                                          
284900         MOVE JA TO SPARR-ANT-SW                                          
285000         MOVE MSGI-IDUSER    TO SLAG-IDUSER-SPKVAL                        
285100         MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                         
285200       END-IF                                                             
285300                                                                          
285400     PERFORM IMS-REPL-WLARTS11                                            
285500     MOVE MID-IDDC-SEND-TO     TO TO-WS-IDDC                              
285600     IF NOT TO-CDC-SE                                                     
285700       MOVE MID-IDDC-SEND-TO TO W-IDDC                                    
285800       PERFORM IMS-GET-ARTS-WLARTS01                                      
285900       PERFORM IMS-GHNP-ARTS-WLARTS11                                     
286000       IF SEGMENT-FINNS                                                   
286100         COMPUTE SLAG-KVBEART =                                           
286200                 SLAG-KVBEART + W-KVRETUR-BEORD                           
286300         END-COMPUTE                                                      
286400         PERFORM IMS-REPL-WLARTS11                                        
286500       END-IF                                                             
286600     END-IF                                                               
286700     .                                                                    
286800     EJECT                                                                
286900                                                                          
287000 MFS-RENSA-FAELT-UT SECTION.                                              
287100                                                                          
287200*    --- ALLA UTDATA-FÄLT                                                 
287300     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
287400                             MOD-KDERS                                    
287500                             MOD-IDLEVNR                                  
287600                             MOD-IDANSK                                   
287700                             MOD-KVSTOCK                                  
287800                             MOD-KVAKS-PAV                                
287900                             MOD-KVAKS-SDC                                
288000                             MOD-KVBEART                                  
288100                             MOD-KDLEVSP                                  
288200                             MOD-KVSPARR-KVAL                             
288300                             MOD-TIRETUR-BEORD                            
288400                             MOD-KVRETUR-BEORD                            
288500                             MOD-KDORDKL                                  
288600                             MOD-RETUR-TEXT                               
288700                             MOD-TISKROT                                  
288800                             MOD-KVSKROT                                  
288900                             MOD-VALUE-OF-SCRAP-QTY                       
289000                             MOD-SKROT-TEXT                               
289100                             MOD-IDPERSON-QUAL                            
289200                             MOD-IDPERSON-ESC                             
289300                             MOD-IDKONTO                                  
289400                             MOD-IDANALYS                                 
289500     .                                                                    
289600     EJECT                                                                
289700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
289800                                                                          
289900*    --- ALLA UTDATA-FÄLT                                                 
290000     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
290100                               MOD-KDERS                                  
290200                               MOD-IDLEVNR                                
290300                               MOD-IDANSK                                 
290400                               MOD-KVSTOCK                                
290500                               MOD-KVAKS-PAV                              
290600                               MOD-KVAKS-SDC                              
290700                               MOD-KVBEART                                
290800                               MOD-KDLEVSP                                
290900                               MOD-KVSPARR-KVAL                           
291000                               MOD-TIRETUR-BEORD                          
291100                               MOD-TISKROT                                
291200                               MOD-VALUE-OF-SCRAP-QTY                     
291300     .                                                                    
291400     EJECT                                                                
291500 MFS-RENSA-FAELT-IN SECTION.                                              
291600                                                                          
291700*    --- ALLA INDATA-FÄLT                                                 
291800     MOVE MFS-RENSA-FAELT TO                                              
291900                             MOD-IDDC-SEND-TO                             
292000                             MOD-KVRETUR-BEORD                            
292100                             MOD-KDORDKL                                  
292200                             MOD-RETUR-TEXT                               
292300                             MOD-KVSKROT                                  
292400                             MOD-SKROT-TEXT                               
292500                             MOD-IDPERSON-QUAL                            
292600                             MOD-IDPERSON-ESC                             
292700                             MOD-IDKONTO                                  
292800                             MOD-IDANALYS                                 
292900     .                                                                    
293000     EJECT                                                                
293100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
293200                                                                          
293300*    --- ALLA INDATA-FÄLT                                                 
293400     MOVE MFS-ROER-EJ-FAELT TO                                            
293500                               MOD-IDDC-SEND-TO                           
293600                               MOD-KVRETUR-BEORD                          
293700                               MOD-KDORDKL                                
293800                               MOD-RETUR-TEXT                             
293900                               MOD-KVSKROT                                
294000                               MOD-SKROT-TEXT                             
294100                               MOD-IDPERSON-QUAL                          
294200                               MOD-IDPERSON-ESC                           
294300                               MOD-IDKONTO                                
294400                               MOD-IDANALYS                               
294500     .                                                                    
294600     EJECT                                                                
294700 MFS-FORM-ATTR SECTION.                                                   
294800                                                                          
294900*    --- ALLA INDATA-FÄLT                                                 
295000     MOVE MFS-FORMATETS-ATTR TO                                           
295100                                MOD-IDDC-SEND-TO-ATTR                     
295200                                MOD-KVRETUR-BEORD-ATTR                    
295300                                MOD-KDORDKL-ATTR                          
295400                                MOD-RETUR-TEXT-ATTR                       
295500                                MOD-KVSKROT-ATTR                          
295600                                MOD-SKROT-TEXT-ATTR                       
295700                                MOD-IDPERSON-QUAL-ATTR                    
295800                                MOD-IDPERSON-ESC-ATTR                     
295900                                MOD-IDKONTO-ATTR                          
296000                                MOD-IDANALYS-ATTR                         
296100     .                                                                    
296200     SKIP2                                                                
296300 MFS-LAES-IN-IGEN SECTION.                                                
296400                                                                          
296500*    --- ALLA INDATA-FÄLT                                                 
296600     MOVE MFS-ADD-LAES-IN-FAELT TO                                        
296700                                   MOD-IDDC-SEND-TO-ATTR                  
296800                                   MOD-KVRETUR-BEORD-ATTR                 
296900                                   MOD-KDORDKL-ATTR                       
297000                                   MOD-RETUR-TEXT-ATTR                    
297100                                   MOD-KVSKROT-ATTR                       
297200                                   MOD-SKROT-TEXT-ATTR                    
297300                                   MOD-IDPERSON-QUAL-ATTR                 
297400                                   MOD-IDPERSON-ESC-ATTR                  
297500                                   MOD-IDKONTO-ATTR                       
297600                                   MOD-IDANALYS-ATTR                      
297700        .                                                                 
297800     EJECT                                                                
297900                                                                          
298000* --- IMS SEKTIONER ---                                                   
298100     SKIP3                                                                
298200 IMS-GET-MSG SECTION.                                                     
298300                                                                          
298400     MOVE '  QC' TO GODK-STATUSKODER                                      
298500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
298600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
298700     PERFORM IMS-STATUSKONTROLL                                           
298800     .                                                                    
298900     SKIP3                                                                
299000 IMS-INSERT-MSG SECTION.                                                  
299100                                                                          
299200***  IF ENGLISH-TEXT                                                      
299300       MOVE 'N' TO MFS-KDHUVOMR                                           
299400***  END-IF                                                               
299500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
299600     MOVE SPACE TO GODK-STATUSKODER                                       
299700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
299800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
299900     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
300100     EJECT                                                                
300200 IMS-GU-ARTC-ARTC SECTION.                                                
300300                                                                          
300400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
300500          DELIMITED BY SIZE INTO SSA1                                     
300600     MOVE '  GE' TO GODK-STATUSKODER                                      
300700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
300800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
300900     PERFORM IMS-STATUSKONTROLL                                           
301000     .                                                                    
301100     EJECT                                                                
301200 IMS-GET-ARTC-CLAG SECTION.                                               
301300                                                                          
301400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
301500          DELIMITED BY SIZE INTO SSA1                                     
301600     STRING 'WLARTC11 '                                                   
301700          DELIMITED BY SIZE INTO SSA2                                     
301800     MOVE '    ' TO GODK-STATUSKODER                                      
301900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
302000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
302100     PERFORM IMS-STATUSKONTROLL                                           
302200     .                                                                    
302300     SKIP3                                                                
302400 IMS-GET-ARTC-CLAG-GE SECTION.                                            
302500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
302600          DELIMITED BY SIZE INTO SSA1                                     
302700     STRING 'WLARTC11 '                                                   
302800          DELIMITED BY SIZE INTO SSA2                                     
302900     MOVE '  GE' TO GODK-STATUSKODER                                      
303000     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
303100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
303200     PERFORM IMS-STATUSKONTROLL                                           
303300     .                                                                    
303400     SKIP3                                                                
303500 IMS-GNP-CLAG SECTION.                                                    
303600                                                                          
303700     MOVE 'WLARTC11 '  TO SSA1                                            
303800     MOVE '  GE' TO GODK-STATUSKODER                                      
303900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
304000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
304100     PERFORM IMS-STATUSKONTROLL                                           
304200     .                                                                    
304300     EJECT                                                                
304400 IMS-GHNP-CLAG SECTION.                                                   
304500                                                                          
304600     MOVE 'WLARTC11 '  TO SSA1                                            
304700     MOVE '  GE' TO GODK-STATUSKODER                                      
304800     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC11 SSA1                  
304900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
305000     PERFORM IMS-STATUSKONTROLL                                           
305100     .                                                                    
305200     EJECT                                                                
305300 IMS-REPL-WLARTC11 SECTION.                                               
305400                                                                          
305500     MOVE '  ' TO GODK-STATUSKODER                                        
305600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
305700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
305800     PERFORM IMS-STATUSKONTROLL                                           
305900     .                                                                    
306000     EJECT                                                                
306100 IMS-GHU-WDK629 SECTION.                                                  
306200                                                                          
306300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
306400          DELIMITED BY SIZE INTO SSA1                                     
306500     MOVE 'WDK611  '         TO SSA2                                      
306600     MOVE 'WDK629  '         TO SSA3                                      
306700     MOVE '  GE' TO GODK-STATUSKODER                                      
306800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
306900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
307000     PERFORM IMS-STATUSKONTROLL                                           
307100     .                                                                    
307200     EJECT                                                                
307300 IMS-REPL-WDK629 SECTION.                                                 
307400                                                                          
307500     MOVE '  ' TO GODK-STATUSKODER                                        
307600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
307700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
307800     PERFORM IMS-STATUSKONTROLL                                           
307900     .                                                                    
308000     EJECT                                                                
308100 IMS-GET-ARTS-WLARTS01 SECTION.                                           
308200     MOVE 'IMS-GET-ARTS-WLARTS01 '  TO DBS-SECTION                        
308300                                                                          
308400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
308500          DELIMITED BY SIZE INTO SSA1                                     
308600     MOVE '  GE' TO GODK-STATUSKODER                                      
308700     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
308800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
308900     PERFORM IMS-STATUSKONTROLL                                           
309000     .                                                                    
309100     SKIP3                                                                
309200 IMS-GET-ARTS-WLARTS11 SECTION.                                           
309300     MOVE 'IMS-GET-ARTS-WLARTS11 ' TO DBS-SECTION                         
309400                                                                          
309500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
309600          DELIMITED BY SIZE INTO SSA1                                     
309700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
309800          DELIMITED BY SIZE INTO SSA2                                     
309900     MOVE '  GE' TO GODK-STATUSKODER                                      
310000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
310100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
310200     PERFORM IMS-STATUSKONTROLL                                           
310300     .                                                                    
310400     EJECT                                                                
310500 IMS-GHNP-ARTS-WLARTS11 SECTION.                                          
310600     MOVE 'IMS-GHNP-ARTS-WLARTS11 '  TO DBS-SECTION                       
310700                                                                          
310800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
310900          DELIMITED BY SIZE INTO SSA1                                     
311000     MOVE '  GE' TO GODK-STATUSKODER                                      
311100     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-ARTS11 SSA1                  
311200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
311300     PERFORM IMS-STATUSKONTROLL                                           
311400     .                                                                    
311500     SKIP3                                                                
311600 IMS-GNP-ARTS-WLARTS11 SECTION.                                           
311700     MOVE 'IMS-GNP-ARTS-WLARTS11 '  TO DBS-SECTION                        
311800                                                                          
311900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
312000          DELIMITED BY SIZE INTO SSA1                                     
312100     MOVE '  GE' TO GODK-STATUSKODER                                      
312200     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
312300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
312400     PERFORM IMS-STATUSKONTROLL                                           
312500     .                                                                    
312600     SKIP3                                                                
312700 IMS-REPL-WLARTS11 SECTION.                                               
312800     MOVE 'IMS-REPL-WLARTS11 '   TO DBS-SECTION                           
312900                                                                          
313000     MOVE '  ' TO GODK-STATUSKODER                                        
313100     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
313200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
313300     PERFORM IMS-STATUSKONTROLL                                           
313400     .                                                                    
313500     EJECT                                                                
313600     SKIP3                                                                
313700 IMS-GNP-WDK711-REF SECTION.                                              
313800     MOVE 'IMS-GNP-WDK711-REF '  TO DBS-SECTION                           
313900                                                                          
314000     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN-X                            
314100                    '&IDDC    <=' W-IDDC-MAX-X                            
314200                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
314300          DELIMITED BY SIZE INTO SSA1                                     
314400     MOVE '  GE' TO GODK-STATUSKODER                                      
314500     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
314600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
314700     PERFORM IMS-STATUSKONTROLL                                           
314800     .                                                                    
314900     SKIP3                                                                
315000 IMS-GU-BENA-TEXT SECTION.                                                
315100     SKIP2                                                                
315200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
315300            DELIMITED BY SIZE INTO SSA1                                   
315400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
315500            DELIMITED BY SIZE INTO SSA2                                   
315600     MOVE '  GE'                TO GODK-STATUSKODER                       
315700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
315800     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
315900     PERFORM IMS-STATUSKONTROLL                                           
316000     .                                                                    
316100     EJECT                                                                
316200 IMS-GET-WL410901 SECTION.                                                
316300                                                                          
316400     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
316500          DELIMITED BY SIZE INTO SSA1                                     
316600     MOVE '  GE' TO GODK-STATUSKODER                                      
316700     CALL CBLTDLI USING GU 4109-PCB DLI-IO-411001 SSA1                    
316800     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
316900     PERFORM IMS-STATUSKONTROLL                                           
317000     .                                                                    
317100     SKIP3                                                                
317200 IMS-GNP-WL410911-KVAL SECTION.                                           
317300                                                                          
317400     STRING 'WL410911(KEY4110 >=' W-KEY4110-MIN-X                         
317500                    '&KEY4110 <=' W-KEY4110-MAX-X ')'                     
317600          DELIMITED BY SIZE INTO SSA1                                     
317700     MOVE '  GE' TO GODK-STATUSKODER                                      
317800     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-411011 SSA1                   
317900     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
318000     PERFORM IMS-STATUSKONTROLL                                           
318100     .                                                                    
318200     EJECT                                                                
318300 IMS-GHU-ORDL-WLORDL01 SECTION.                                           
318400                                                                          
318500     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
318600          DELIMITED BY SIZE INTO SSA1                                     
318700     MOVE '  GE' TO GODK-STATUSKODER                                      
318800     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-ORDL01 SSA1                   
318900     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
319000     PERFORM IMS-STATUSKONTROLL                                           
319100     .                                                                    
319200     EJECT                                                                
319300 IMS-ISRT-ORDL-WLORDL01 SECTION.                                          
319400                                                                          
319500     MOVE 'WLORDL01 ' TO SSA1                                             
319600     MOVE '  II' TO GODK-STATUSKODER                                      
319700     CALL CBLTDLI USING ISRT ORDL-PCB DLI-IO-ORDL01 SSA1                  
319800     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
319900     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
320100     SKIP3                                                                
320200 IMS-REPL-ORDL-WLORDL01 SECTION.                                          
320300                                                                          
320400     MOVE '  ' TO GODK-STATUSKODER                                        
320500     CALL CBLTDLI USING REPL ORDL-PCB DLI-IO-ORDL01                       
320600     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
320700     PERFORM IMS-STATUSKONTROLL                                           
320800     .                                                                    
320900     EJECT                                                                
321000 IMS-GU-WDR501-6321 SECTION.                                              
321100                                                                          
321200     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
321300            DELIMITED BY SIZE INTO SSA1                                   
321400     MOVE 'GE  '                TO GODK-STATUSKODER                       
321500     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
321600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
321700     PERFORM IMS-STATUSKONTROLL                                           
321800     .                                                                    
321900     SKIP3                                                                
322000 IMS-ISRT-WDR501-6321 SECTION.                                            
322100                                                                          
322200     STRING 'WDR501     '                                                 
322300            DELIMITED BY SIZE INTO SSA1                                   
322400     MOVE '  '                  TO GODK-STATUSKODER                       
322500     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
322600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
322700     PERFORM IMS-STATUSKONTROLL                                           
322800     .                                                                    
322900     EJECT                                                                
323000 IMS-ISRT-WDGX6322 SECTION.                                               
323100                                                                          
323200     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
323300            DELIMITED BY SIZE INTO SSA1                                   
323400     MOVE 'WDGX6322'            TO SSA2                                   
323500     MOVE '  II'                TO GODK-STATUSKODER                       
323600     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
323700     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
323800     PERFORM IMS-STATUSKONTROLL                                           
323900     SKIP3                                                                
324000     .                                                                    
324100     SKIP3                                                                
324200 IMS-ISRT-WDGX6324 SECTION.                                               
324300                                                                          
324400     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
324500            DELIMITED BY SIZE INTO SSA1                                   
324600     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
324700            DELIMITED BY SIZE INTO SSA2                                   
324800     MOVE 'WDGX6324'            TO SSA3                                   
324900     MOVE '  '                  TO GODK-STATUSKODER                       
325000     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
325100                                      SSA1 SSA2 SSA3                      
325200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
325300     PERFORM IMS-STATUSKONTROLL                                           
325400     .                                                                    
325500     EJECT                                                                
325600 IMS-LAS-IDPERSON SECTION.                                                
325700     STRING 'WDP301  (KDARBTYP ='  W-KDARBTYP-X ')'                       
325800            DELIMITED BY SIZE INTO SSA1                                   
325900     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
326000            DELIMITED BY SIZE INTO SSA2                                   
326100     MOVE '  GE' TO GODK-STATUSKODER                                      
326200     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
326300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
326400     PERFORM IMS-STATUSKONTROLL                                           
326500     .                                                                    
326600     EJECT                                                                
326700 IMS-GU-WDB601    SECTION.                                                
326800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
326900          DELIMITED BY SIZE INTO SSA1                                     
327000     MOVE '  GE' TO GODK-STATUSKODER                                      
327100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
327200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
327300     PERFORM IMS-STATUSKONTROLL                                           
327400     .                                                                    
327500     EJECT                                                                
327593 IMS-GU-WDB601-FTG SECTION.                                               
327594     STRING 'WDB601  (IDFTG    =' W-IDFTG-B6 ')'                          
327595            DELIMITED BY SIZE INTO SSA1                                   
327596     MOVE '  GE'                 TO GODK-STATUSKODER                      
327597     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
327598     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
327599     PERFORM IMS-STATUSKONTROLL                                           
327600     .                                                                    
327601     SKIP3                                                                
327602                                                                          
327610 IMS-GET-WDR501-6327 SECTION.                                             
327700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
327800          DELIMITED BY SIZE INTO SSA1                                     
327900     MOVE '  GE' TO GODK-STATUSKODER                                      
328000     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501-6327 SSA1               
328100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
328200     PERFORM IMS-STATUSKONTROLL                                           
328300     .                                                                    
328400     SKIP3                                                                
328500 IMS-GET-WDGX6328 SECTION.                                                
328600     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
328700          DELIMITED BY SIZE INTO SSA1                                     
328800     MOVE '  GE' TO GODK-STATUSKODER                                      
328900     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX6328 SSA1                 
329000     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
329100     PERFORM IMS-STATUSKONTROLL                                           
329200     .                                                                    
329300     EJECT                                                                
329400 IMS-GU-WDN601 SECTION.                                                   
329500     SKIP2                                                                
329600     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
329700          DELIMITED BY SIZE INTO SSA1                                     
329800     MOVE '  GE' TO GODK-STATUSKODER                                      
329900     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
330000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
330100     PERFORM IMS-STATUSKONTROLL                                           
330200     .                                                                    
330300     EJECT                                                                
330400 IMS-GNP-WDN611 SECTION.                                                  
330500     SKIP2                                                                
330600     STRING 'WDN611   '                                                   
330700          DELIMITED BY SIZE INTO SSA1                                     
330800     MOVE '  GE' TO GODK-STATUSKODER                                      
330900     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
331000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
331100     PERFORM IMS-STATUSKONTROLL                                           
331200     .                                                                    
331300     EJECT                                                                
331400 IMS-GU-WDK901 SECTION.                                                   
331500                                                                          
331600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
331700          DELIMITED BY SIZE INTO SSA1                                     
331800     MOVE '  GE' TO GODK-STATUSKODER                                      
331900     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
332000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
332100     PERFORM IMS-STATUSKONTROLL                                           
332200     .                                                                    
332300     EJECT                                                                
332400 DB2-SELECT-TP4TRAN     SECTION.                                          
332500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
332600                                                                          
332700     MOVE 000100 TO GODK-SQLCODEKODER                                     
332800                                                                          
332900     EXEC SQL                                                             
333000           SELECT  KDARBTYP                                               
333100                  ,IDDC_SEND                                              
333200                  ,IDDC_REC                                               
333300                  ,IDDISTR                                                
333400                  ,IDKUNDNR                                               
333500                                                                          
333600           INTO   :TP4TRAN-KDARBTYP                                       
333700                 ,:TP4TRAN-IDDC-SEND                                      
333800                 ,:TP4TRAN-IDDC-REC                                       
333900                 ,:TP4TRAN-IDDISTR                                        
334000                 ,:TP4TRAN-IDKUNDNR                                       
334100                                                                          
334200           FROM    TP4TRAN                                                
334300                                                                          
334400           WHERE KDARBTYP  = :WS-KDARBTYP                                 
334500           AND   IDDC_SEND = :WS-IDDC-SEND                                
334600           AND   IDDC_REC  = :WS-IDDC-REC                                 
334700     END-EXEC                                                             
334800                                                                          
334900     MOVE SQLCODE TO SQLCODE-WS                                           
335000     PERFORM DB2-STATUSKONTROLL                                           
335100     .                                                                    
335200     EJECT                                                                
335300 IMS-STATUSKONTROLL SECTION.                                              
335400                                                                          
335500     SET STATUS-IX TO 1                                                   
335600     SEARCH GODK-STATUS                                                   
335700       AT END                                                             
335800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
335900         DELIMITED BY SIZE INTO FELTEXT                                   
336000         CALL FELLOG                                                      
336100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
336200         CONTINUE                                                         
336300     END-SEARCH                                                           
336400     .                                                                    
336500 DB2-STATUSKONTROLL  SECTION.                                             
336600                                                                          
336700     SET SQLCODE-IX TO 1                                                  
336800     SEARCH GODK-SQLCODE                                                  
336900       AT END                                                             
337000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
337100          DELIMITED BY SIZE INTO FELTEXT                                  
337200          CALL ABEND USING RKOD-ABEND-DB2                                 
337300       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
337400     END-SEARCH                                                           
337500     .                                                                    
