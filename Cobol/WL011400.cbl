000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL011400.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   JUNI 2005.                                               
000500                                                                          
000600     REMARKS.                                                             
000700* WL011400 PROGRAM IS A REPLICA OF W4010800 PROGRAM                       
000800* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
000900*                                                                         
001000*    NAMN:       CARPARTS.LDC.STOCKBALANCEINFO                            
001100*                                                                         
001200                                                                          
001300     REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
001600*                'BUFFERT-SALDO-INFO'.                                    
001700*                NYCKELN BESTÄMMER VILKET LAGER SOM GÄLLER.               
001800*                BASER ÄR WLBENA (WDD3) - BENÄMNINGSBAS                   
001900*                         WLARTC (WDK6) - ARTIKELBAS CDC                  
002000*                         WLARTC (WDK7) - ARTIKELBAS SDC                  
002100*                         WLARTD (WDD8) - SALDOBAS.                       
002200*                                                                         
002300*        SE W9011500 SOM EXEMPEL PÅ INTERNSORTERING + BLÄDDRING           
002400*                                                                         
002500*                                                                         
002600*    TRANSAKTION:                                                         
002700*                     WL0114U                                             
002800*    INDATA.                                                              
002900*        REQU:       WL0114I1                                             
003000*    UTDATA.                                                              
003100*        RESP:        WL0114O1 / WL0114O2                                 
003200*    SUBPROGRAM.                                                          
003300*        FELLOG                                                           
003400*                                                                         
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77  PROGRAM-NAMN            PIC X(8)    VALUE 'WL011400'.                
004300 01  FILLER                      PIC X(8) VALUE 'AAAAAAAA'.               
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004700 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004800 77  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
004900 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
005000*                                                                         
005100 77  JA                      PIC X       VALUE 'J'.                       
005200 77  YES                     PIC X       VALUE 'J'.                       
005300 77  NEJ                     PIC X       VALUE 'N'.                       
005400 77  NOO                     PIC X       VALUE 'N'.                       
005500 77  MSG-IX                  PIC S9(9)  VALUE +0   COMP SYNC.             
005600 77  IX                      PIC S9(3) COMP-3 VALUE 1.                    
005700 77  SPRAK-IX                PIC S9(3)   VALUE +0    COMP SYNC.           
005800 77  IDDC-WS                 PIC X(2).                                    
005900 77  WS-KVROS-SDC            PIC S9(7) COMP-3 VALUE ZERO.                 
006000 77  WS-KVRADER              PIC  9(7)        VALUE ZERO.                 
006100 77  WS-IDARTNR              PIC  9(9)        VALUE ZERO.                 
006200 77  WS-KVDISP-SLAG          PIC S9(7)   VALUE ZERO COMP-3.               
006300 77  WS-KVDISP-CLAG          PIC S9(7)   VALUE ZERO COMP-3.               
006400 77  WS-KVOKS-TOT-NDC        PIC S9(7)   VALUE ZERO COMP-3.               
006500 77  WS-KVOKS-TOT-CDC        PIC S9(7)   VALUE ZERO COMP-3.               
006600 77  WS-CN                   PIC X(2)    VALUE 'CN'.                      
006700 77  KEYS-SW                   PIC X      VALUE 'J'.                      
006800     88  KEYS-OK                          VALUE 'J'.                      
006900     88  KEYS-WRONG                       VALUE 'N'.                      
007000 77  WS-ADLAGOMR             PIC  9(2)        VALUE ZERO.                 
007100 77  WS-ADGANG               PIC  9(2)        VALUE ZERO.                 
007200 77  WS-ADPLATS              PIC  9(5)        VALUE ZERO.                 
007300                                                                          
007400 01  WS-KDARTHNT                 PIC 9(6).                                
007500 01  FILLER REDEFINES WS-KDARTHNT.                                        
007600     03  WS-KDARTHNT-V           PIC 9(3).                                
007700     03  WS-KDARTHNT-H           PIC 9(3).                                
007800                                                                          
007900 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
008000 77  WS-IDTRANS              PIC X(4).                                    
008100     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
008200                                     '4105' '4106' '4107' '4108'.         
008300     88  EGEN-MID              VALUE '4108'.                              
008400                                                                          
008500 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
008600     88  NYCKLAR-OK                      VALUE 'J'.                       
008700     88  NYCKLAR-FEL                     VALUE 'N'.                       
008800                                                                          
008900 77  ARTIKEL-AUTH-SW         PIC X       VALUE 'J'.                       
009000     88  ARTIKEL-AUTH-OK                 VALUE 'J'.                       
009100     88  ARTIKEL-AUTH-FEL                VALUE 'N'.                       
009200                                                                          
009300 77  ARTIKEL-SW              PIC X       VALUE 'N'.                       
009400     88  ARTIKEL-FOUND                   VALUE 'J'.                       
009500     88  ARTIKEL-MISSING                 VALUE 'N'.                       
009600                                                                          
009700 01  ALL-SPACE-UTF8.                                                      
009800     03  FILLER                  PIC X(100) VALUE ALL X'20'.              
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
010100 01  TABENTRY-PARM.                                                       
010200     03  STEGLANGD               PIC S9(9) COMP  VALUE 44.                
010300     03  ANTAL                   PIC S9(9) COMP.                          
010400     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 10.                
010500                                                                          
010600 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
010700 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
010800                                                                          
010900 01  TAB-MAX                     PIC S9(9) COMP VALUE 200.                
011000     EJECT                                                                
011100*    --- TABELL SOM SORTERAS AV WINTSOR                                   
011200 01  TABELL.                                                              
011300     03  TAB-POST  OCCURS 200.                                            
011400       04  TAB-RAD.                                                       
011500         05  TAB-PLATSTYP        PIC X(1).                                
011600         05  TAB-ADBUFFOMR       PIC 9(2).                                
011700         05  TAB-ADBUFFGANG      PIC 9(2).                                
011800         05  TAB-ADBUFFPL        PIC 9(5).                                
011900         05  TAB-KVBUFF-F        PIC 9(8).                                
012000         05  TAB-KVBUFF-OF       PIC 9(8).                                
012100         05  TAB-KVKOLLI-F       PIC 9(4).                                
012200         05  TAB-KVKOLLI-OF      PIC 9(4).                                
012300       04  TAB-SORT.                                                      
012400         05  TAB-PLATSTYP-SORT   PIC X(1).                                
012500         05  TAB-ADBUFFOMR-SORT  PIC 9(2).                                
012600         05  TAB-ADBUFFGANG-SORT PIC 9(2).                                
012700         05  TAB-ADBUFFPL-SORT   PIC 9(5).                                
012800                                                                          
012900                                                                          
013000     SKIP3                                                                
013100 01    W-BUFF-IX             PIC S9(9)   VALUE ZERO  COMP-3.              
013200 01    WS-SUBUFF-F           PIC S9(9)   VALUE ZERO  COMP-3.              
013300 01    WS-SUBUFF-OF          PIC S9(9)   VALUE ZERO  COMP-3.              
013400 01    WS-SUKOLLI-F          PIC S9(5)   VALUE ZERO  COMP-3.              
013500 01    WS-SUKOLLI-OF         PIC S9(5)   VALUE ZERO  COMP-3.              
013600                                                                          
013700 01    W-MINKEY-WDD8-X.                                                   
013800     03  W-MINKEY-IDTRANS          PIC  X(4)    VALUE '4108'.             
013900     03  W-MINKEY-WDD8-ENTER.                                             
014000       05  W-MINKEY-ADBUFFOMR-ENTER  PIC S9(3) VALUE ZERO  COMP-3.        
014100       05  W-MINKEY-DABUFPAF-ENTER   PIC  9(8) VALUE ZERO.                
014200       05  W-MINKEY-ADBUFFGANG-ENTER PIC S9(3) VALUE ZERO  COMP-3.        
014300       05  W-MINKEY-ADBUFFPL-ENTER   PIC S9(5) VALUE ZERO  COMP-3.        
014400                                                                          
014500     03  W-MINKEY-WDD8-NEXT.                                              
014600       05  W-MINKEY-ADBUFFOMR-NEXT   PIC S9(3) VALUE ZERO  COMP-3.        
014700       05  W-MINKEY-DABUFPAF-NEXT    PIC  9(8) VALUE ZERO.                
014800       05  W-MINKEY-ADBUFFGANG-NEXT  PIC S9(3) VALUE ZERO  COMP-3.        
014900       05  W-MINKEY-ADBUFFPL-NEXT    PIC S9(5) VALUE ZERO  COMP-3.        
015000                                                                          
015100 01    NYCKLAR-TILL-DLI.                                                  
015200   03    W-IDARTNR-X.                                                     
015300     05    W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.              
015400   03    W-IDDC-X.                                                        
015500     05    W-IDDC            PIC X(2)    VALUE SPACE.                     
015600   03    W-IDSKYLT-X.                                                     
015700     05    W-IDSKYLT         PIC X(3).                                    
015800                                                                          
015900   03  W-IDLEVNR-X.                                                       
016000     05    W-IDLEVNR         PIC X(5)    VALUE SPACE.                     
016100                                                                          
016200   03    W-WDD811KY-MIN-X.                                                
016300     05    W-IDDC-WDD8-MIN   PIC X(2).                                    
016400     05    W-ADBUFFOMR-MIN   PIC S9(3)   VALUE ZERO  COMP-3.              
016500     05    W-DABUFPAF-MIN    PIC  9(8)   VALUE ZERO.                      
016600     05    W-ADBUFFGANG-MIN  PIC S9(3)   VALUE ZERO  COMP-3.              
016700     05    W-ADBUFFPL-MIN    PIC S9(5)   VALUE ZERO  COMP-3.              
016800                                                                          
016900   03    W-WDD811KY-MAX-X.                                                
017000     05    W-IDDC-WDD8-MAX   PIC X(2).                                    
017100     05    W-ADBUFFOMR-MAX   PIC S9(3)   VALUE +99    COMP-3.             
017200     05    W-DABUFPAF-MAX    PIC  9(8)   VALUE  99999999.                 
017300     05    W-ADBUFFGANG-MAX  PIC S9(3)   VALUE +99    COMP-3.             
017400     05    W-ADBUFFPL-MAX    PIC S9(5)   VALUE +99999 COMP-3.             
017500                                                                          
017600   03    W-WDD7A1KY-MIN.                                                  
017700     05    W-IDARTNR-MIN7    PIC S9(9)  COMP-3 VALUE ZERO.                
017800     05    FILLER            PIC S9(9)  COMP-3 VALUE ZERO.                
017900     05    FILLER            PIC S9(3)  COMP-3 VALUE ZERO.                
018000                                                                          
018100   03    W-WDD7A1KY-MAX.                                                  
018200     05    W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                
018300     05    FILLER             PIC S9(9)  COMP-3 VALUE +999999999.         
018400     05    FILLER             PIC S9(3)  COMP-3 VALUE +999.               
018500                                                                          
018600  03  W-IDDC-B6-X.                                                        
018700      05 W-IDDC-B6               PIC X(2).                                
018800  03  W-IDLAND-X.                                                         
018900      05  W-IDLAND               PIC X(2)    VALUE SPACE.                 
019000                                                                          
019100     EJECT                                                                
019200 01    DYNAMISKA-SUBPROGRAM.                                              
019300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
019400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG'.              
019500   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
019600   03  WINTSOR                   PIC X(8)    VALUE 'WINTSOR'.             
019700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
019800   03  WZ01SEND                  PIC X(8)    VALUE 'WZ01SEND'.            
019900   03  WZ01SUB                   PIC X(8)    VALUE 'WZ01SUB '.            
020000   03  WZ01AUTH                  PIC X(8)    VALUE 'WZ01AUTH'.            
020100   03  WMSGCONV                  PIC X(8)    VALUE 'WMSGCONV'.            
020200   03  WTRAUTF8                  PIC X(8)    VALUE 'WTRAUTF8'.            
020300   03  WTRAEBCD                  PIC X(8)    VALUE 'WTRAEBCD'.            
020400   03  W271UTIL                  PIC X(8)    VALUE 'W271UTIL'.            
020500*                                                                         
020600*    --- PARAMETERS TO ABEND                                              
020700 77  FILLER                      PIC X(08)   VALUE 'ABENDKOD'.            
020800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
020900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
021000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
021100     SKIP2                                                                
021200 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
021300 01  MESSAGE-CODES.                                                       
021400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
021500     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
021600     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '023'.                 
021700     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
021800     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '316'.                 
021900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
022000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
022100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
022200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '021'.                 
022300     03  ERR-ONLY-ONE-OPTION     PIC X(3)    VALUE '319'.                 
022400     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '027'.                 
022500     03  ERR-EMPLOYEE-ID-MISSING PIC X(3)    VALUE '320'.                 
022600     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
022700     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
022800     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
022900     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '293'.                 
023000     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
023100     03  ERR-WRONG-COMMAND-CODE  PIC X(3)    VALUE '304'.                 
023200     03  ERR-PART-NOT-IN-AREA-42 PIC X(3)    VALUE '320'.                 
023300     03 INF-PART-REPLACED           PIC X(3) VALUE '321'.                 
023400     03 INF-PART-REPLACE2           PIC X(3) VALUE '32A'.                 
023500     03 INF-PART-REPLACE3           PIC X(3) VALUE '32B'.                 
023600     03 INF-PART-REPLACE4           PIC X(3) VALUE '32C'.                 
023700     03 INF-PART-EXPIRE             PIC X(3) VALUE '322'.                 
023800     03 INF-PART-EXPIR2             PIC X(3) VALUE '32B'.                 
023900     03 INF-PART-EXPIR3             PIC X(3) VALUE '32C'.                 
024000     03 INF-REPLACING-PART          PIC X(3) VALUE '323'.                 
024100*                                                                         
024200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024300*                                                                         
024400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
024500     SKIP3                                                                
024600*01  -COPY WZ01SUB                                                        
024700                                                                          
024800 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
024900*01  -COPY WMSGCONV                                                       
025000                                                                          
025100 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
025200*01  -COPY WZ01AUTH                                                       
025300                                                                          
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025600     SKIP3                                                                
025700 01  REQU-AREA.                                                           
025800*    03  -COPY WZ01REQ2                                                   
025900*    03  -COPY WL0114I1                                                   
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
026200                                                                          
026300 01  RESP-DATA-KVDLEN            PIC S9(9) BINARY.                        
026400 01  RESP-AREA.                                                           
026500*    03  -COPY WZ01RES2                                                   
026600     03  RESP-DATA-AREA.                                                  
026700*        05  -COPY WL0114O1 -L                                            
026800*        05  -COPY WL0114O2 -L                                            
026900                                                                          
027000*    -COPY WL0114O1 -PRE V1-                                              
027100*    -COPY WL0114O2                                                       
027200                                                                          
027300*01  -COPY WWDC99                                                         
027400*01  -COPY WWPRODSL                                                       
027500     EJECT                                                                
027600                                                                          
027700 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
027800*01  -COPY WTRAUTF8                                                       
027900 01  FILLER                  PIC X(16)  VALUE 'WTRAEBCD-AREA   '.         
028000*01  -COPY WTRAEBCD                                                       
028100 01  FILLER                  PIC X(16)  VALUE 'W271UTIL-AREA   '.         
028200*01  -COPY W271UTIL                                                       
028300******************************************************************        
028400*                                                                         
028500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028600*                                                                         
028700 01    IMS-WS.                                                            
028800   03    FILLER              PIC X(16)   VALUE 'IMS-WS'.                  
028900                                                                          
029000*                                STATUS-KOD FRÅN IMS                      
029100   03    STATUS-WS           PIC XX.                                      
029200     88    SEGMENT-FINNS                 VALUE '  '.                      
029300     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
029400                                                                          
029500   03    GODK-STATUSKODER.                                                
029600     05    GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.              
029700                                                                          
029800 01    SSA1                  PIC X(96).                                   
029900 01    SSA2                  PIC X(96).                                   
030000     EJECT                                                                
030100*                                IMS FUNKTIONSKODER                       
030200*01      -COPY W0003                                                      
030300     EJECT                                                                
030400*                                DLI INPUT-OUTPUT AREOR                   
030500 01    FILLER                PIC X(16)   VALUE 'DLI-IO-AREA'.             
030600 01    DLI-IO-AREA-WDF106.                                                
030700*      03  -COPY WDF106                                                   
030800     EJECT                                                                
030900 01    FILLER                PIC X(16)   VALUE 'DLI-IO-AREA'.             
031000 01    DLI-IO-AREA-601.                                                   
031100*03    WLARTC01 -COPY WDK601                                              
031200     EJECT                                                                
031300 01    DLI-IO-AREA-611.                                                   
031400*03    WLARTC11 -COPY WDK611                                              
031500     EJECT                                                                
031600 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK711'.              
031700 01    DLI-IO-AREA-711.                                                   
031800*03      -COPY WDK711                                                     
031900     EJECT                                                                
032000 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK712'.              
032100 01  DLI-IO-AREA-K712.                                                    
032200*    03  -COPY WDK712                                                     
032300 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK901'.              
032400 01  DLI-IO-AREA-K901.                                                    
032500*    03  -COPY WDK901 -PRE K9-                                            
032600 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDD311'.              
032700 01  DLI-IO-WDD311.                                                       
032800*    03  -COPY WDD311                                                     
032900 01    DLI-IO-AREA.                                                       
033000       03  IO-AREA           PIC X(120) VALUE SPACE.                      
033100     EJECT                                                                
033200*  03    WLARTD11 -COPY WDD811 -PRE BUFF-    -RED IO-AREA.                
033300     EJECT                                                                
033400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA01'.            
033500     SKIP3                                                                
033600 01  DLI-IO-AREA-ERSA01.                                                  
033700*  03  WLERSA01 -COPY WDD701  -PRE ERSA01-                                
033800     EJECT                                                                
033900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA11'.            
034000     SKIP3                                                                
034100 01  DLI-IO-AREA-ERSA11.                                                  
034200*  03  WLERSA11 -COPY WDD702  -PRE ERSA11-                                
034300     EJECT                                                                
034400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSB01'.            
034500     SKIP3                                                                
034600 01  DLI-IO-AREA-ERSB01.                                                  
034700*  03  WLERSB01 -COPY WDD7A1  -PRE ERSB01-                                
034800     EJECT                                                                
034900*01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
035000*01  DLI-IO-ARTS01.                                                       
035100*    03   COPY WDK701                                                     
035200*    EJECT                                                                
035300*                                                                         
035400*01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
035500*01  DLI-IO-ARTS11.                                                       
035600*    03   COPY WDK711                                                     
035700*    EJECT                                                                
035800                                                                          
035900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036000 01   DLI-IO-AREA-B601.                                                   
036100*     03  -COPY WDB601                                                    
036200                                                                          
036300                                                                          
036400 LINKAGE SECTION.                                                         
036500*01    -COPY W0009     -PRE MSG-                                          
036600 01  ATAB-PCB                 PIC X.                                      
036700*01    -COPY W0008     -PRE BENA-                                         
036800         05  FILLER           PIC X.                                      
036900*01    -COPY W0008     -PRE BENA2-                                        
037000         05  FILLER           PIC X.                                      
037100*01    -COPY W0008     -PRE ARTC-                                         
037200         05  FILLER           PIC X.                                      
037300*01    -COPY W0008     -PRE WDK7-                                         
037400         05  FILLER           PIC X.                                      
037500*01    -COPY W0008     -PRE WDK9-                                         
037600         05  FILLER           PIC X.                                      
037700*01    -COPY W0008     -PRE ARTD-                                         
037800         05  FILLER           PIC X.                                      
037900*01    -COPY W0008     -PRE ARTM-                                         
038000         05  FILLER           PIC X.                                      
038100*01    -COPY W0008     -PRE ERSA-                                         
038200         05  FILLER           PIC X.                                      
038300*01    -COPY W0008     -PRE ERSB-                                         
038400         05  FILLER           PIC X.                                      
038500*01    -COPY W0008     -PRE WDB6-                                         
038600         05  FILLER           PIC X.                                      
038700*01    -COPY W0008     -PRE WDF1-                                         
038800         05  FILLER           PIC X.                                      
038900*****W271UTIL*******                                                      
039000 01  UTIL-WDK6-PCB                       PIC X.                           
039100 01  UTIL-WDK7-PCB                       PIC X.                           
039200 01  UTIL-WDB6-PCB                       PIC X.                           
039300     EJECT                                                                
039400 PROCEDURE DIVISION USING MSG-PCB ATAB-PCB BENA-PCB BENA2-PCB             
039500                          ARTC-PCB WDK7-PCB ARTD-PCB                      
039600                          ARTM-PCB ERSA-PCB ERSB-PCB                      
039700                          WDB6-PCB WDF1-PCB WDK9-PCB                      
039800                          UTIL-WDK6-PCB                                   
039900                          UTIL-WDK7-PCB                                   
040000                          UTIL-WDB6-PCB.                                  
040100                                                                          
040200 MAIN SECTION.                                                            
040300                                                                          
040400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
040500     IF SUB-KDRC = 0                                                      
040600       PERFORM A-INIT                                                     
040700       IF  NYCKLAR-OK                                                     
040800         MOVE WS-IDARTNR   TO W-IDARTNR                                   
040900         MOVE IDDC-WS      TO W-IDDC                                      
041000         IF DCS-CDC                                                       
041100           PERFORM IMS-GET-ARTC-ART                                       
041200         ELSE                                                             
041300           PERFORM IMS-GET-WDK7-SLAG                                      
041400         END-IF                                                           
041500                                                                          
041600         IF  SEGMENT-FINNS                                                
041700           MOVE YES              TO ARTIKEL-SW                            
041800           PERFORM S01-ARTIKEL-BEHORIG                                    
041900           IF ARTIKEL-AUTH-OK                                             
042000             PERFORM B-BEHANDLA-ART                                       
042100             PERFORM G-SHOW-ARTS                                          
042200             PERFORM C-BEHANDLA-BENA                                      
042300             PERFORM D-BEHANDLA-ARTD                                      
042400             PERFORM E-SORTERA-PLATSER                                    
042500             PERFORM F-VISA-PLATSER                                       
042600             PERFORM H-VISA-WDD7-ERS                                      
042700           END-IF                                                         
042800         ELSE                                                             
042900           MOVE ERR-URVAL-SAKNAS   TO RESP-IDMSG-ERROR                    
043000         END-IF                                                           
043100       ELSE                                                               
043200         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
043300       END-IF                                                             
043400       IF SUB-KDTRANS(1:6) = 'WLA114'                                     
043500         PERFORM S11-MSG-CONV                                             
043600       END-IF                                                             
043700       PERFORM S02-RETURN-RESPONSE                                        
043800     END-IF                                                               
043900                                                                          
044000     MOVE ZERO TO RETURN-CODE                                             
044100     GOBACK                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 A-INIT SECTION.                                                          
044500     MOVE 'STA A-INIT        ' TO PGM-POS                                 
044600                                                                          
044700     MOVE SPACES               TO RESP-WZ01RES2                           
044800                                  V1-RESP-WL0114O1                        
044900     MOVE LOW-VALUES           TO RESP-WL0114O2                           
045000                                                                          
045100     MOVE YES                  TO ARTIKEL-AUTH-SW                         
045200     MOVE NOO                  TO ARTIKEL-SW                              
045300                                                                          
045400     MOVE 001                  TO RESP-IDRESVER                           
045500     MOVE SPACE                TO RESP-IDMSG-ERROR                        
045600                                  RESP-IDMSG-INFO                         
045700                                  RESP-IDELMT-ERROR                       
045800     MOVE 0                    TO RESP-KVRADER-MAX1                       
045900     IF SUB-KDTRANS(1:6) = 'WLA114'                                       
046000       MOVE 001                  TO AUTH-KDCALL                           
046100       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
046200                                    REQU-WZ01REQ2                         
046300       IF AUTH-KDRC > 0                                                   
046400         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
046500         MOVE NEJ                TO NYCKLAR-SW                            
046600       END-IF                                                             
046700       MOVE FUNCTION UPPER-CASE (REQU-IDUSER) TO                          
046800                                 REQU-IDUSER                              
046900       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
047000                                 REQU-IDDC-KEY                            
047100     END-IF                                                               
047200                                                                          
047300     IF (REQU-IDARTNR-KEY NUMERIC                                         
047400     AND REQU-IDARTNR-KEY > ZERO)                                         
047500         MOVE REQU-IDARTNR-KEY TO WS-IDARTNR                              
047600     END-IF                                                               
047700                                                                          
047800     MOVE JA TO NYCKLAR-SW                                                
047900                                                                          
048000     IF REQU-IDDC-KEY NOT = ALL '+'                                       
048100       MOVE REQU-IDDC-KEY TO IDDC-WS WS-IDDC                              
048200     END-IF                                                               
048300                                                                          
048400     MOVE WS-IDARTNR      TO RESP-IDARTNR-KEY                             
048500     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
048600                                                                          
048700     MOVE +2 TO SPRAK-IX                                                  
048800                                                                          
048900     IF WS-IDARTNR    NOT NUMERIC                                         
049000       MOVE NEJ TO NYCKLAR-SW                                             
049100     ELSE                                                                 
049200       MOVE IDDC-WS TO W-IDDC-B6                                          
049300       PERFORM IMS-GU-WDB601                                              
049400       IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                            
049500         MOVE WS-IDARTNR      TO W-IDARTNR                                
049600         MOVE IDDC-WS         TO W-IDDC                                   
049700       ELSE                                                               
049800         MOVE REQU-IDDC-KEY   TO W-IDDC                                   
049900                                 IDDC-WS                                  
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     MOVE IDDC-WS        TO RESP-IDDC-KEY                                 
050400     .                                                                    
050500     EJECT                                                                
050600 B-BEHANDLA-ART SECTION.                                                  
050700                                                                          
050800     IF DCS-CDC                                                           
050900       PERFORM BA-BEHANDLA-CDC                                            
051000     ELSE                                                                 
051100       PERFORM IMS-GET-ARTC-ART                                           
051200       PERFORM BB-BEHANDLA-SDC                                            
051300     END-IF                                                               
051400                                                                          
051500     IF NDC OR LDC OR SDC OR CDC                                          
051600        PERFORM BC-BEHANDLA-SUPPL                                         
051700     END-IF                                                               
051800                                                                          
051900* ENABLE EXECUTE TO EDIT LOCAL PART DESCRIPTION                           
052000     MOVE ART-KDPRODSL  TO TEST-KDPRODSL                                  
052100     IF  KDPRODSL-LOCAL                                                   
052200     AND ( DCS-IDSKYLT-DB NOT = 'GB')                                     
052300        MOVE JA    TO RESP-BEART-FL                                       
052400     END-IF                                                               
052500     .                                                                    
052600     SKIP3                                                                
052700 BA-BEHANDLA-CDC SECTION.                                                 
052800                                                                          
052900*    MOVE ART-KDERS-UTG     TO RESP-KDERS                                 
053000     PERFORM IMS-GET-ARTC-CLAG                                            
053100                                                                          
053200     MOVE CLAG-KVPB-SATS    TO RESP-KVPB-SATS                             
053300     COMPUTE RESP-KVPB-TOT = CLAG-KVPB-SATS + CLAG-KVPB-SEP               
053400     MOVE CLAG-BEFT         TO RESP-BEFT                                  
053500     MOVE CLAG-KVQPACK-1    TO RESP-KVQPACK-1                             
053600     MOVE CLAG-KVQPACK-3    TO RESP-KVQPACK-3                             
053700     MOVE CLAG-KVQPACK-4    TO RESP-KVQPACK-4                             
053800     MOVE CLAG-KVAKS-CDC    TO RESP-KVAKS                                 
053900     MOVE CLAG-KVUTRS       TO RESP-KVUTRS                                
054000     MOVE CLAG-KVEFRS       TO RESP-KVEFRS                                
054100     MOVE CLAG-KVLS         TO RESP-KVLS                                  
054200     MOVE CLAG-KVROS        TO RESP-KVROS                                 
054300     MOVE CLAG-KDERS        TO RESP-KDERS                                 
054400     MOVE 'CDC'             TO RESP-RUBR1                                 
054500     MOVE CLAG-ADLAGOMR     TO WS-ADLAGOMR                                
054600     MOVE CLAG-ADGANG       TO WS-ADGANG                                  
054700     MOVE CLAG-ADPLATS      TO WS-ADPLATS                                 
054800     MOVE WS-ADLAGOMR       TO RESP-ADLAGOMR                              
054900     MOVE WS-ADGANG         TO RESP-ADGANG                                
055000     MOVE WS-ADPLATS        TO RESP-ADPLATS                               
055100     MOVE CLAG-KDARTHNT     TO WS-KDARTHNT                                
055200     MOVE WS-KDARTHNT-H     TO RESP-KDARTHNT                              
055300                                                                          
055400     PERFORM IMS-GU-WDK901                                                
055500     IF SEGMENT-FINNS                                                     
055600       COMPUTE WS-KVDISP-CLAG = CLAG-KVLS                                 
055700                              - CLAG-KVRESS                               
055800                              - K9-ART-KVOKS-BULK                         
055900                              - K9-ART-KVOKS-DAG                          
056000                              - K9-ART-KVOKS-VOR                          
056100                                                                          
056200       COMPUTE WS-KVOKS-TOT-CDC = K9-ART-KVOKS-BULK                       
056300                                + K9-ART-KVOKS-DAG                        
056400                                + K9-ART-KVOKS-VOR                        
056500     ELSE                                                                 
056600       COMPUTE WS-KVDISP-CLAG = CLAG-KVLS                                 
056700                              - CLAG-KVRESS                               
056800                                                                          
056900       MOVE ZERO TO WS-KVOKS-TOT-NDC                                      
057000     END-IF                                                               
057100                                                                          
057200     MOVE WS-KVDISP-CLAG    TO RESP-KVDISP-NDC                            
057300     MOVE WS-KVOKS-TOT-CDC  TO RESP-KVOKS-NDC                             
057400     MOVE CLAG-KVRESS       TO RESP-KVRESS                                
057500     MOVE CLAG-KVBEART      TO RESP-KVBEART                               
057600     MOVE CLAG-KDARTURS     TO RESP-KDARTURS                              
057700     MOVE CLAG-KVAKS-PAV    TO RESP-KVAKS-PAV-NDC                         
057800     MOVE CLAG-KDLEVSP      TO RESP-KDLEVSP-NDC                           
057900     MOVE CLAG-KVSPARR-KVAL TO RESP-KVSPARR-KVAL-NDC                      
058000     MOVE CLAG-IDLEVNR-SHIP TO RESP-IDLEVNR                               
058100                               W-IDLEVNR                                  
058200                                                                          
058300     INITIALIZE  UTIL-W271UTIL                                            
058400     MOVE 003                   TO UTIL-KDCALL                            
058500     MOVE WS-IDARTNR            TO UTIL-IDARTNR                           
058600                                                                          
058700     CALL W271UTIL USING UTIL-W271UTIL                                    
058800                         UTIL-WDK6-PCB                                    
058900                         UTIL-WDK7-PCB                                    
059000                         UTIL-WDB6-PCB                                    
059100                                                                          
059200     IF UTIL-KDSVAR-OK                                                    
059300        MOVE UTIL-KVPB-TOT      TO RESP-KVPB-REF                          
059400     END-IF                                                               
059500                                                                          
059600     PERFORM IMS-GU-WDF106                                                
059700       IF SEGMENT-FINNS                                                   
059800         MOVE ADR-BELEV      TO RESP-BELEV                                
059900         MOVE ADR-ADLEV-RAD1 TO RESP-ADLEV-RAD1                           
060000         MOVE ADR-ADLEV-ORT  TO RESP-ADLEV-ORT                            
060100         MOVE ADR-ADLEVLND   TO RESP-ADLEVLND                             
060200       END-IF                                                             
060300                                                                          
060400     IF CLAG-ADLAGOMR-SVS > ZERO                                          
060500       MOVE 'P'             TO TAB-PLATSTYP (IX-RAD)                      
060600       MOVE '1'             TO TAB-PLATSTYP-SORT (IX-RAD)                 
060700       MOVE CLAG-ADLAGOMR-SVS                                             
060800                          TO TAB-ADBUFFOMR (IX-RAD)                       
060900                             TAB-ADBUFFOMR-SORT (IX-RAD)                  
061000       MOVE CLAG-ADGANG-SVS                                               
061100                          TO TAB-ADBUFFGANG (IX-RAD)                      
061200                             TAB-ADBUFFGANG-SORT (IX-RAD)                 
061300       MOVE CLAG-ADPLATS-SVS                                              
061400                          TO TAB-ADBUFFPL (IX-RAD)                        
061500                             TAB-ADBUFFPL-SORT (IX-RAD)                   
061600       MOVE CLAG-KVLS-SVS TO TAB-KVBUFF-F (IX-RAD)                        
061700       COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                                
061800                             CLAG-KVLS-SVS                                
061900       MOVE ZERO            TO TAB-KVBUFF-OF (IX-RAD)                     
062000                             TAB-KVKOLLI-F (IX-RAD)                       
062100                             TAB-KVKOLLI-OF (IX-RAD)                      
062200       ADD 1                TO IX-RAD                                     
062300     END-IF                                                               
062400                                                                          
062500     MOVE 1                 TO IX                                         
062600     PERFORM UNTIL IX > 4                                                 
062700     OR CLAG-ADLAGOMR-CD (IX) = ZERO                                      
062800       MOVE 'P'             TO TAB-PLATSTYP (IX-RAD)                      
062900       MOVE '1'             TO TAB-PLATSTYP-SORT (IX-RAD)                 
063000       MOVE CLAG-ADLAGOMR-CD (IX)                                         
063100                          TO TAB-ADBUFFOMR (IX-RAD)                       
063200                             TAB-ADBUFFOMR-SORT (IX-RAD)                  
063300       MOVE CLAG-ADGANG-CD (IX)                                           
063400                          TO TAB-ADBUFFGANG (IX-RAD)                      
063500                             TAB-ADBUFFGANG-SORT (IX-RAD)                 
063600       MOVE CLAG-ADPLATS-CD (IX)                                          
063700                          TO TAB-ADBUFFPL (IX-RAD)                        
063800                             TAB-ADBUFFPL-SORT (IX-RAD)                   
063900       MOVE CLAG-KVLS-CD (IX)                                             
064000                          TO TAB-KVBUFF-F (IX-RAD)                        
064100       COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                                
064200                             CLAG-KVLS-CD (IX)                            
064300       MOVE ZERO            TO TAB-KVBUFF-OF (IX-RAD)                     
064400                             TAB-KVKOLLI-F (IX-RAD)                       
064500                             TAB-KVKOLLI-OF (IX-RAD)                      
064600       ADD 1                TO IX                                         
064700                             IX-RAD                                       
064800     END-PERFORM                                                          
064900                                                                          
065000     .                                                                    
065100     EJECT                                                                
065200 BB-BEHANDLA-SDC SECTION.                                                 
065300                                                                          
065400     IF  ART-KDERS-UTG     > ZERO                                         
065500       IF ART-KDERS-UTG    = +29 OR +52                                   
065600         MOVE INF-PART-EXPIRE TO RESP-IDMSG-INFO                          
065700       ELSE                                                               
065800*        MOVE INF-PART-REPLACED TO RESP-IDMSG-INFO                        
065900       CONTINUE                                                           
066000       END-IF                                                             
066100     ELSE                                                                 
066200*      IF ART-FLERS     = JA                                              
066300*        MOVE INF-REPLACING-PART   TO RESP-IDMSG-ERROR                    
066400*      END-IF                                                             
066500*      MOVE ART-KDERS-UTG   TO RESP-KDERS                                 
066600       PERFORM IMS-GET-ARTC-CLAG                                          
066700       IF SEGMENT-FINNS                                                   
066800         MOVE CLAG-KDERS       TO RESP-KDERS                              
066900       END-IF                                                             
067000                                                                          
067100       MOVE ZERO            TO RESP-KVPB-SATS                             
067200                               RESP-KVPB-TOT                              
067300                               RESP-BEFT                                  
067400                               RESP-KVQPACK-1                             
067500                               RESP-KVQPACK-3                             
067600                               RESP-KVQPACK-4                             
067700                                                                          
067800       COMPUTE WS-KVROS-SDC = SLAG-KVROS-DAG + SLAG-KVROS-BULK            
067900       MOVE WS-KVROS-SDC    TO RESP-KVROS                                 
068000       MOVE SLAG-KVAKS-SDC  TO RESP-KVAKS                                 
068100       MOVE SLAG-KVEFRS     TO RESP-KVEFRS                                
068200       MOVE SLAG-KVLS       TO RESP-KVLS                                  
068300       MOVE SLAG-KVUTRS     TO RESP-KVUTRS                                
068400       MOVE SLAG-ADLAGOMR   TO WS-ADLAGOMR                                
068500       MOVE SLAG-ADGANG     TO WS-ADGANG                                  
068600       MOVE SLAG-ADPLATS    TO WS-ADPLATS                                 
068700       MOVE WS-ADLAGOMR     TO RESP-ADLAGOMR                              
068800       MOVE WS-ADGANG       TO RESP-ADGANG                                
068900       MOVE WS-ADPLATS      TO RESP-ADPLATS                               
069000       MOVE SLAG-IDLEVNR    TO RESP-IDLEVNR                               
069100                               W-IDLEVNR                                  
069200       MOVE SLAG-KVRESS     TO RESP-KVRESS                                
069300                                                                          
069400       PERFORM IMS-GU-WDF106                                              
069500       IF SEGMENT-FINNS                                                   
069600         MOVE ADR-BELEV      TO RESP-BELEV                                
069700         MOVE ADR-ADLEV-RAD1 TO RESP-ADLEV-RAD1                           
069800         MOVE ADR-ADLEV-ORT  TO RESP-ADLEV-ORT                            
069900         MOVE ADR-ADLEVLND   TO RESP-ADLEVLND                             
070000       END-IF                                                             
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 BC-BEHANDLA-SUPPL SECTION.                                               
070500                                                                          
070600     MOVE WS-IDARTNR             TO W-IDARTNR                             
070700     PERFORM IMS-GET-ARTC-ART                                             
070800     PERFORM IMS-GET-ARTC-CLAG                                            
070900     IF SEGMENT-FINNS                                                     
071000       MOVE CLAG-VKART       TO RESP-VKART                                
071100       MOVE CLAG-VLARTNTO    TO RESP-VLARTNTO                             
071200       MOVE CLAG-BEFT        TO RESP-BEFT                                 
071300       MOVE CLAG-KDERS       TO RESP-KDERS                                
071400       MOVE CLAG-KDARTHNT    TO WS-KDARTHNT                               
071500       MOVE WS-KDARTHNT-H    TO RESP-KDARTHNT                             
071600     END-IF                                                               
071700     IF SLAG-IDDC-REF > SPACE                                             
071800       MOVE CLAG-KDARTURS    TO RESP-KDARTURS                             
071900     END-IF                                                               
072000*                                                                         
072100     MOVE DCS-IDLANDX2     TO W-IDLAND                                    
072200     PERFORM IMS-GU-WDK712                                                
072300     IF SEGMENT-FINNS                                                     
072400       IF LART-VKART > 0                                                  
072500         MOVE LART-VKART    TO RESP-VKART                                 
072600       END-IF                                                             
072700       IF LART-VLARTNTO > 0                                               
072800         MOVE LART-VLARTNTO TO RESP-VLARTNTO                              
072900       END-IF                                                             
073000       IF LART-BEFT  > 0                                                  
073100         MOVE LART-BEFT     TO RESP-BEFT                                  
073200       END-IF                                                             
073300       IF LART-KDARTURS > SPACE                                           
073400         MOVE LART-KDARTURS TO RESP-KDARTURS                              
073500       END-IF                                                             
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 C-BEHANDLA-BENA SECTION.                                                 
074000                                                                          
074100* ENGLISH TEXT FOR ALL DC                                                 
074200     MOVE 'GB'   TO W-IDSKYLT                                             
074300     PERFORM IMS-GU-BENA11                                                
074400     IF SEGMENT-FINNS                                                     
074500         MOVE TEXT-BEART TO TRAUTF8-TECONV-FROM                           
074600     ELSE                                                                 
074700         MOVE SPACE      TO TRAUTF8-TECONV-FROM                           
074800     END-IF                                                               
074900                                                                          
075000* -- ENGLISH WILL BE TRANSLATED TO UNICODE                                
075100     MOVE '278 '   TO TRAUTF8-KDCP                                        
075200     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
075300     MOVE TRAUTF8-TECONV-TO  TO RESP-BEART                                
075400                                RESP-BEART-LOCAL                          
075500                                                                          
075600* LK UPDATE LOCAL PART DESCRIPTION                                        
075700     IF REQU-KDPGMACT = 'E'                                               
075800       IF REQU-BEART-LOCAL > SPACE                                        
075900         MOVE REQU-BEART-LOCAL    TO TRAEBCD-TECONV-FROM                  
076000         IF DCS-UNICODE-IDSKYLT                                           
076100           MOVE 'UTF8'          TO TRAEBCD-KDCP                           
076200         ELSE                                                             
076300           MOVE '278'           TO TRAEBCD-KDCP                           
076400         END-IF                                                           
076500         CALL WTRAEBCD USING TRAEBCD-AREA                                 
076600                                                                          
076700         MOVE DCS-IDSKYLT-DB   TO W-IDSKYLT                               
076800         PERFORM IMS-GHU-BENA11                                           
076900         IF SEGMENT-FINNS                                                 
077000           MOVE TRAEBCD-TECONV-TO TO TEXT-BEARTEXT                        
077100           PERFORM IMS-REPL-BENA11                                        
077200         END-IF                                                           
077300       ELSE                                                               
077400          MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                       
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800* LK READ LOCAL TEXT                                                      
077900     IF DCS-IDSKYLT-DB NOT = 'GB'                                         
078000       MOVE DCS-IDSKYLT-DB     TO W-IDSKYLT                               
078100       PERFORM IMS-GU-BENA11                                              
078200       IF SEGMENT-FINNS                                                   
078300           MOVE TEXT-BEARTEXT TO TRAUTF8-TECONV-FROM                      
078400       ELSE                                                               
078500           MOVE SPACE         TO TRAUTF8-TECONV-FROM                      
078600       END-IF                                                             
078700       IF DCS-UNICODE-IDSKYLT                                             
078800          MOVE 'UTF8'             TO TRAUTF8-KDCP                         
078900       ELSE                                                               
079000          MOVE '278 '             TO TRAUTF8-KDCP                         
079100       END-IF                                                             
079200                                                                          
079300       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
079400       MOVE TRAUTF8-TECONV-TO TO RESP-BEART-LOCAL                         
079500     ELSE                                                                 
079600       MOVE ALL-SPACE-UTF8    TO RESP-BEART-LOCAL                         
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 D-BEHANDLA-ARTD      SECTION.                                            
080100                                                                          
080200     PERFORM IMS-GET-ARTD-ART                                             
080300     IF SEGMENT-FINNS                                                     
080400        MOVE IDDC-WS         TO W-IDDC-WDD8-MIN                           
080500                                W-IDDC-WDD8-MAX                           
080600        MOVE ZERO            TO W-ADBUFFOMR-MIN                           
080700                                W-ADBUFFGANG-MIN                          
080800                                W-ADBUFFPL-MIN                            
080900                                W-DABUFPAF-MIN                            
081000                                                                          
081100        PERFORM IMS-GET-ARTD-SALDO-IDDC                                   
081200        PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > TAB-MAX                  
081300         IF SEGMENT-FINNS                                                 
081400           MOVE 'B'          TO TAB-PLATSTYP (IX-RAD)                     
081500           MOVE '2'          TO TAB-PLATSTYP-SORT (IX-RAD)                
081600           MOVE BUFF-SALDO-ADBUFFOMR                                      
081700                             TO TAB-ADBUFFOMR (IX-RAD)                    
081800                                TAB-ADBUFFOMR-SORT (IX-RAD)               
081900           MOVE BUFF-SALDO-ADBUFFGANG                                     
082000                             TO TAB-ADBUFFGANG (IX-RAD)                   
082100                                TAB-ADBUFFGANG-SORT (IX-RAD)              
082200           MOVE BUFF-SALDO-ADBUFFPL                                       
082300                             TO TAB-ADBUFFPL (IX-RAD)                     
082400                                TAB-ADBUFFPL-SORT (IX-RAD)                
082500           IF BUFF-SALDO-ADBUFFOMR = 1                                    
082600             MOVE BUFF-SALDO-KDPAF                                        
082700                             TO RESP-KDPAF                                
082800           END-IF                                                         
082900           MOVE BUFF-SALDO-KVBUFF-OF                                      
083000                             TO TAB-KVBUFF-OF (IX-RAD)                    
083100           COMPUTE WS-SUBUFF-OF = WS-SUBUFF-OF +                          
083200                                  BUFF-SALDO-KVBUFF-OF                    
083300           MOVE BUFF-SALDO-KVBUFF-F                                       
083400                             TO TAB-KVBUFF-F (IX-RAD)                     
083500           COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                            
083600                                  BUFF-SALDO-KVBUFF-F                     
083700           MOVE BUFF-SALDO-KVKOLLI-OF                                     
083800                             TO TAB-KVKOLLI-OF (IX-RAD)                   
083900           COMPUTE WS-SUKOLLI-OF = WS-SUKOLLI-OF +                        
084000                                  BUFF-SALDO-KVKOLLI-OF                   
084100           MOVE BUFF-SALDO-KVKOLLI-F                                      
084200                             TO TAB-KVKOLLI-F (IX-RAD)                    
084300           COMPUTE WS-SUKOLLI-F = WS-SUKOLLI-F +                          
084400                                  BUFF-SALDO-KVKOLLI-F                    
084500           ADD 1             TO IX-RAD                                    
084600         END-IF                                                           
084700         PERFORM IMS-GET-ARTD-SALDO-IDDC                                  
084800        END-PERFORM                                                       
084900        MOVE WS-SUBUFF-F               TO RESP-SUBUFF-F                   
085000        MOVE WS-SUBUFF-OF              TO RESP-SUBUFF-OF                  
085100        MOVE WS-SUKOLLI-F              TO RESP-SUKOLLI-F                  
085200        MOVE WS-SUKOLLI-OF             TO RESP-SUKOLLI-OF                 
085300                                                                          
085400     END-IF                                                               
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 E-SORTERA-PLATSER SECTION.                                               
085900                                                                          
086000     SUBTRACT 1 FROM IX-RAD                                               
086100     MOVE IX-RAD TO ANTAL                                                 
086200                                                                          
086300     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
086400                  TAB-SORT (1) NYCKELLANGD                                
086500     .                                                                    
086600     EJECT                                                                
086700                                                                          
086800 F-VISA-PLATSER SECTION.                                                  
086900                                                                          
087000     MOVE +1                 TO IX-RAD-TAB                                
087100                                                                          
087200     MOVE +1                 TO IX-RAD                                    
087300***  PERFORM UNTIL IX-RAD > 200                                           
087400     PERFORM UNTIL IX-RAD-TAB > TAB-MAX                                   
087500     OR TAB-PLATSTYP (IX-RAD-TAB) = SPACE                                 
087600     OR TAB-PLATSTYP (IX-RAD-TAB) = LOW-VALUE                             
087700       MOVE IX-RAD-TAB      TO RESP-KVRADER-MAX1                          
087800       MOVE TAB-PLATSTYP (IX-RAD-TAB)                                     
087900                            TO RESP-PLATSTYP (IX-RAD)                     
088000       MOVE TAB-ADBUFFOMR (IX-RAD-TAB)                                    
088100                            TO RESP-ADBUFFOMR (IX-RAD)                    
088200       MOVE TAB-ADBUFFGANG (IX-RAD-TAB)                                   
088300                            TO RESP-ADBUFFGANG (IX-RAD)                   
088400       MOVE TAB-ADBUFFPL (IX-RAD-TAB)                                     
088500                            TO RESP-ADBUFFPL (IX-RAD)                     
088600       MOVE TAB-KVBUFF-OF (IX-RAD-TAB)                                    
088700                            TO RESP-KVBUFF-OF (IX-RAD)                    
088800       MOVE TAB-KVBUFF-F (IX-RAD-TAB)                                     
088900                            TO RESP-KVBUFF-F (IX-RAD)                     
089000       MOVE TAB-KVKOLLI-OF (IX-RAD-TAB)                                   
089100                            TO RESP-KVKOLLI-OF (IX-RAD)                   
089200       MOVE TAB-KVKOLLI-F (IX-RAD-TAB)                                    
089300                            TO RESP-KVKOLLI-F (IX-RAD)                    
089400       ADD 1                TO IX-RAD                                     
089500                               IX-RAD-TAB                                 
089600     END-PERFORM                                                          
089700     .                                                                    
089800     EJECT                                                                
089900 G-SHOW-ARTS  SECTION.                                                    
090000                                                                          
090100     IF NOT DCS-CDC                                                       
090200       PERFORM GA-NDCINFO-TILL-MOD                                        
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600 GA-NDCINFO-TILL-MOD SECTION.                                             
090700                                                                          
090800                                                                          
090900     MOVE ZERO              TO WS-KVDISP-SLAG                             
091000                               WS-KVOKS-TOT-NDC                           
091100     COMPUTE WS-KVDISP-SLAG =                                             
091200             SLAG-KVLS - (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK)               
091300                       - SLAG-KVRESS                                      
091400                                                                          
091500     COMPUTE WS-KVOKS-TOT-NDC =                                           
091600                        SLAG-KVOKS-DAG + SLAG-KVOKS-BULK                  
091700                                                                          
091800     MOVE WS-KVDISP-SLAG    TO RESP-KVDISP-NDC                            
091900     MOVE SLAG-KVAKS-PAV    TO RESP-KVAKS-PAV-NDC                         
092000     MOVE WS-KVOKS-TOT-NDC  TO RESP-KVOKS-NDC                             
092100     MOVE SLAG-KVBEART      TO RESP-KVBEART                               
092200     MOVE SLAG-KDLEVSP      TO RESP-KDLEVSP-NDC                           
092300     MOVE SLAG-KVSPARR-KVAL TO RESP-KVSPARR-KVAL-NDC                      
092400     MOVE SLAG-KVPB-REF     TO RESP-KVPB-REF                              
092500     .                                                                    
092600     SKIP2                                                                
092700 H-VISA-WDD7-ERS  SECTION.                                                
092800                                                                          
092900     PERFORM IMS-GU-WDD7-ERSA01                                           
093000     IF SEGMENT-FINNS                                                     
093100        PERFORM IMS-GNP-WDD7-ERSA11                                       
093200        IF SEGMENT-FINNS                                                  
093300           PERFORM IMS-GNP-WDD7-ERSA11                                    
093400        END-IF                                                            
093500     END-IF                                                               
093600                                                                          
093700     IF ART-FLERS = JA                                                    
093800        MOVE ART-IDARTNR                                                  
093900                       TO W-IDARTNR-MIN7                                  
094000                          W-IDARTNR-MAX7                                  
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400 S01-ARTIKEL-BEHORIG  SECTION.                                            
094500                                                                          
094600     MOVE JA             TO ARTIKEL-AUTH-SW                               
094700     IF REQU-IDUSER = 'PCSSE01'                                           
094800**ENDAST LO 42  FÖR ID PCSSE01..........                                  
094900**ENDAST LO 43  FÖR ID PCSSE01.....BATTERI KUNGÄLV                        
095000       IF DCS-CDC                                                         
095100         PERFORM IMS-GET-ARTC-CLAG                                        
095200         IF CLAG-ADLAGOMR = 42 OR 43                                      
095300           CONTINUE                                                       
095400         ELSE                                                             
095500           MOVE NEJ      TO ARTIKEL-AUTH-SW                               
095600           MOVE ERR-PART-NOT-IN-AREA-42 TO RESP-IDMSG-ERROR               
095700         END-IF                                                           
095800       ELSE                                                               
095900         IF SLAG-ADLAGOMR = 42 OR 43                                      
096000           CONTINUE                                                       
096100         ELSE                                                             
096200           MOVE NEJ      TO ARTIKEL-AUTH-SW                               
096300           MOVE ERR-PART-NOT-IN-AREA-42 TO RESP-IDMSG-ERROR               
096400         END-IF                                                           
096500       END-IF                                                             
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
097000     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
097100                                                                          
097200     MOVE 'GETARG'               TO SUB-KDFUNC                            
097300     MOVE 'CARPARTS.LDC.STOCKBALANCEINFO'  TO SUB-ADDISPABS               
097400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
097500                                                                          
097600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
097700                                                                          
097800     IF SUB-KDRC > 0                                                      
097900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
098000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
098100       DELIMITED BY SIZE INTO FELTEXT                                     
098200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
098300     END-IF                                                               
098400     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
098500     .                                                                    
098600     SKIP3                                                                
098700 S02-RETURN-RESPONSE SECTION.                                             
098800     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
098900                                                                          
099000     PERFORM S02A-HANDLE-OUTPUT-VERSIONS                                  
099100                                                                          
099200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
099300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
099400     IF REQU-IDOUTVER = 002                                               
099500       COMPUTE RESP-DATA-KVDLEN = LENGTH OF RESP-WL0114O2                 
099600       MOVE RESP-WL0114O2 (1:RESP-DATA-KVDLEN)                            
099700                                 TO RESP-DATA-AREA                        
099800                                       (1:RESP-DATA-KVDLEN)               
099900     ELSE                                                                 
100000       COMPUTE RESP-DATA-KVDLEN = LENGTH OF V1-RESP-WL0114O1              
100100       MOVE V1-RESP-WL0114O1 (1:RESP-DATA-KVDLEN)                         
100200                                 TO RESP-DATA-AREA                        
100300                                       (1:RESP-DATA-KVDLEN)               
100400     END-IF                                                               
100500     COMPUTE SUB-KVDLEN           = LENGTH OF RESP-WZ01RES2               
100600                                  + RESP-DATA-KVDLEN                      
100700                                                                          
100800     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
100900                                    SUB-KVDLEN                            
101000                                    RESP-AREA (1: SUB-KVDLEN)             
101100                                                                          
101200     IF SUB-KDRC > 0                                                      
101300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
101400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
101500       DELIMITED BY SIZE INTO FELTEXT                                     
101600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
101700     END-IF                                                               
101800     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
101900     .                                                                    
102000                                                                          
102100 S02A-HANDLE-OUTPUT-VERSIONS SECTION.                                     
102200     IF REQU-IDOUTVER = 002                                               
102300       MOVE 002                  TO RESP-IDRESVER                         
102400     ELSE                                                                 
102500       MOVE 001                  TO RESP-IDRESVER                         
102600       IF RESP-IDDC-KEY NOT = LOW-VALUES                                  
102700         MOVE RESP-IDDC-KEY      TO V1-RESP-IDDC-KEY                      
102800       END-IF                                                             
102900       IF RESP-IDARTNR-KEY NOT = LOW-VALUES                               
103000         MOVE RESP-IDARTNR-KEY   TO V1-RESP-IDARTNR-KEY                   
103100       END-IF                                                             
103200       IF RESP-BEART NOT = LOW-VALUES                                     
103300         MOVE RESP-BEART         TO V1-RESP-BEART                         
103400       ELSE                                                               
103500         MOVE ALL-SPACE-UTF8     TO V1-RESP-BEART                         
103600       END-IF                                                             
103700       IF RESP-BEART-LOCAL NOT = LOW-VALUES                               
103800         MOVE RESP-BEART-LOCAL   TO V1-RESP-BEART-LOCAL                   
103900       ELSE                                                               
104000         MOVE ALL-SPACE-UTF8     TO V1-RESP-BEART-LOCAL                   
104100       END-IF                                                             
104200       IF RESP-BEART-FL NOT = LOW-VALUES                                  
104300         MOVE RESP-BEART-FL      TO V1-RESP-BEART-FL                      
104400       END-IF                                                             
104500       IF RESP-RUBR1 NOT = LOW-VALUES                                     
104600         MOVE RESP-RUBR1         TO V1-RESP-RUBR1                         
104700       END-IF                                                             
104800       IF RESP-ADLAGOMR NOT = LOW-VALUES                                  
104900         MOVE RESP-ADLAGOMR      TO V1-RESP-ADLAGOMR                      
105000       END-IF                                                             
105100       IF RESP-ADGANG NOT = LOW-VALUES                                    
105200         MOVE RESP-ADGANG        TO V1-RESP-ADGANG                        
105300       END-IF                                                             
105400       IF RESP-ADPLATS NOT = LOW-VALUES                                   
105500         MOVE RESP-ADPLATS       TO V1-RESP-ADPLATS                       
105600       END-IF                                                             
105700       IF RESP-KVAKS NOT = LOW-VALUES                                     
105800         MOVE RESP-KVAKS         TO V1-RESP-KVAKS                         
105900       END-IF                                                             
106000       IF RESP-KVQPACK-1 NOT = LOW-VALUES                                 
106100         MOVE RESP-KVQPACK-1     TO V1-RESP-KVQPACK-1                     
106200       END-IF                                                             
106300       IF RESP-KVLS NOT = LOW-VALUES                                      
106400         MOVE RESP-KVLS          TO V1-RESP-KVLS                          
106500       END-IF                                                             
106600       IF RESP-KVUTRS NOT = LOW-VALUES                                    
106700         MOVE RESP-KVUTRS        TO V1-RESP-KVUTRS                        
106800       END-IF                                                             
106900       IF RESP-KVQPACK-3 NOT = LOW-VALUES                                 
107000         MOVE RESP-KVQPACK-3     TO V1-RESP-KVQPACK-3                     
107100       END-IF                                                             
107200       IF RESP-KVEFRS NOT = LOW-VALUES                                    
107300         MOVE RESP-KVEFRS        TO V1-RESP-KVEFRS                        
107400       END-IF                                                             
107500       IF RESP-KVPB-TOT NOT = LOW-VALUES                                  
107600         MOVE RESP-KVPB-TOT      TO V1-RESP-KVPB-TOT                      
107700       END-IF                                                             
107800       IF RESP-KVQPACK-4 NOT = LOW-VALUES                                 
107900         MOVE RESP-KVQPACK-4     TO V1-RESP-KVQPACK-4                     
108000       END-IF                                                             
108100       IF RESP-KVROS NOT = LOW-VALUES                                     
108200         MOVE RESP-KVROS         TO V1-RESP-KVROS                         
108300       END-IF                                                             
108400       IF RESP-KVPB-SATS NOT = LOW-VALUES                                 
108500         MOVE RESP-KVPB-SATS     TO V1-RESP-KVPB-SATS                     
108600       END-IF                                                             
108700       IF RESP-KDERS NOT = LOW-VALUES                                     
108800         MOVE RESP-KDERS         TO V1-RESP-KDERS                         
108900       END-IF                                                             
109000       IF RESP-BEFT NOT = LOW-VALUES                                      
109100         MOVE RESP-BEFT          TO V1-RESP-BEFT                          
109200       END-IF                                                             
109300       IF RESP-KDPAF NOT = LOW-VALUES                                     
109400         MOVE RESP-KDPAF         TO V1-RESP-KDPAF                         
109500       END-IF                                                             
109600       IF RESP-KVDISP-NDC NOT = LOW-VALUES                                
109700         MOVE RESP-KVDISP-NDC    TO V1-RESP-KVDISP-NDC                    
109800       END-IF                                                             
109900       IF RESP-KVOKS-NDC NOT = LOW-VALUES                                 
110000         MOVE RESP-KVOKS-NDC     TO V1-RESP-KVOKS-NDC                     
110100       END-IF                                                             
110200       IF RESP-KVAKS-PAV-NDC NOT = LOW-VALUES                             
110300         MOVE RESP-KVAKS-PAV-NDC TO V1-RESP-KVAKS-PAV-NDC                 
110400       END-IF                                                             
110500       IF RESP-KVBEART NOT = LOW-VALUES                                   
110600         MOVE RESP-KVBEART       TO V1-RESP-KVBEART                       
110700       END-IF                                                             
110800       IF RESP-KDLEVSP-NDC NOT = LOW-VALUES                               
110900         MOVE RESP-KDLEVSP-NDC   TO V1-RESP-KDLEVSP-NDC                   
111000       END-IF                                                             
111100       IF RESP-KVSPARR-KVAL-NDC NOT = LOW-VALUES                          
111200         MOVE RESP-KVSPARR-KVAL-NDC                                       
111300                                 TO V1-RESP-KVSPARR-KVAL-NDC              
111400       END-IF                                                             
111500       IF RESP-SUBUFF-F NOT = LOW-VALUES                                  
111600         MOVE RESP-SUBUFF-F      TO V1-RESP-SUBUFF-F                      
111700       END-IF                                                             
111800       IF RESP-SUBUFF-OF NOT = LOW-VALUES                                 
111900         MOVE RESP-SUBUFF-OF     TO V1-RESP-SUBUFF-OF                     
112000       END-IF                                                             
112100       IF RESP-SUKOLLI-F NOT = LOW-VALUES                                 
112200         MOVE RESP-SUKOLLI-F     TO V1-RESP-SUKOLLI-F                     
112300       END-IF                                                             
112400       IF RESP-SUKOLLI-OF NOT = LOW-VALUES                                
112500         MOVE RESP-SUKOLLI-OF    TO V1-RESP-SUKOLLI-OF                    
112600       END-IF                                                             
112700       IF RESP-KVPB-REF NOT = LOW-VALUES                                  
112800         MOVE RESP-KVPB-REF      TO V1-RESP-KVPB-REF                      
112900       END-IF                                                             
113000*      For some reason, web is checking for KVRADER > 0 and               
113100*      do some additional processing. So, we will set to 1                
113200*      here until web is changed to check correctly.                      
113300       IF ARTIKEL-FOUND AND ARTIKEL-AUTH-OK                               
113400         MOVE 1                  TO V1-RESP-KVRADER-MAX1                  
113500       END-IF                                                             
113600       IF RESP-KVRADER-MAX1 NOT = LOW-VALUES AND                          
113700          RESP-KVRADER-MAX1 > 0                                           
113800         MOVE RESP-KVRADER-MAX1  TO V1-RESP-KVRADER-MAX1                  
113900       END-IF                                                             
114000       IF RESP-IDLEVNR NOT = LOW-VALUES                                   
114100         MOVE RESP-IDLEVNR       TO V1-RESP-IDLEVNR                       
114200       END-IF                                                             
114300       IF RESP-VKART NOT = LOW-VALUES                                     
114400         MOVE RESP-VKART         TO V1-RESP-VKART                         
114500       END-IF                                                             
114600       IF RESP-VLARTNTO NOT = LOW-VALUES                                  
114700         MOVE RESP-VLARTNTO      TO V1-RESP-VLARTNTO                      
114800       END-IF                                                             
114900       IF RESP-KDARTURS NOT = LOW-VALUES                                  
115000         MOVE RESP-KDARTURS      TO V1-RESP-KDARTURS                      
115100       END-IF                                                             
115200       IF RESP-KVRESS NOT = LOW-VALUES                                    
115300         MOVE RESP-KVRESS        TO V1-RESP-KVRESS                        
115400       END-IF                                                             
115500       IF RESP-BELEV NOT = LOW-VALUES                                     
115600         MOVE RESP-BELEV         TO V1-RESP-BELEV                         
115700       END-IF                                                             
115800       IF RESP-ADLEV-RAD1 NOT = LOW-VALUES                                
115900         MOVE RESP-ADLEV-RAD1    TO V1-RESP-ADLEV-RAD1                    
116000       END-IF                                                             
116100       IF RESP-ADLEV-ORT NOT = LOW-VALUES                                 
116200         MOVE RESP-ADLEV-ORT     TO V1-RESP-ADLEV-ORT                     
116300       END-IF                                                             
116400       IF RESP-ADLEVLND NOT = LOW-VALUES                                  
116500         MOVE RESP-ADLEVLND      TO V1-RESP-ADLEVLND                      
116600       END-IF                                                             
116700                                                                          
116800       PERFORM                                                            
116900       VARYING IX-RAD FROM 1 BY 1                                         
117000         UNTIL IX-RAD > RESP-KVRADER-MAX1                                 
117100         MOVE RESP-PLATSTYP   (IX-RAD)                                    
117200                                 TO V1-RESP-PLATSTYP   (IX-RAD)           
117300         MOVE RESP-ADBUFFOMR  (IX-RAD)                                    
117400                                 TO V1-RESP-ADBUFFOMR  (IX-RAD)           
117500         MOVE RESP-ADBUFFGANG (IX-RAD)                                    
117600                                 TO V1-RESP-ADBUFFGANG (IX-RAD)           
117700         MOVE RESP-ADBUFFPL   (IX-RAD)                                    
117800                                 TO V1-RESP-ADBUFFPL   (IX-RAD)           
117900         MOVE RESP-KVBUFF-F   (IX-RAD)                                    
118000                                 TO V1-RESP-KVBUFF-F   (IX-RAD)           
118100         MOVE RESP-KVBUFF-OF  (IX-RAD)                                    
118200                                 TO V1-RESP-KVBUFF-OF  (IX-RAD)           
118300         MOVE RESP-KVKOLLI-F  (IX-RAD)                                    
118400                                 TO V1-RESP-KVKOLLI-F  (IX-RAD)           
118500         MOVE RESP-KVKOLLI-OF (IX-RAD)                                    
118600                                 TO V1-RESP-KVKOLLI-OF (IX-RAD)           
118700       END-PERFORM                                                        
118800     END-IF                                                               
118900     .                                                                    
119000                                                                          
119100 S11-MSG-CONV SECTION.                                                    
119200     MOVE SPACES                  TO RESP-MESSAGES (1)                    
119300                                     RESP-MESSAGES (2)                    
119400     MOVE 1                       TO MSG-IX                               
119500*    REQUEST OK                                                           
119600     MOVE 200                     TO RESP-KDSTATUS-API                    
119700     IF RESP-IDMSG-INFO > SPACE                                           
119800       MOVE SPACES                TO MSG-CONV-AREA                        
119900       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
120000       CALL WMSGCONV           USING MSG-CONV-AREA                        
120100       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
120200       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
120300       ADD 1                      TO MSG-IX                               
120400     END-IF                                                               
120500     IF RESP-IDMSG-ERROR > SPACE                                          
120600*      BAD REQUEST                                                        
120700       MOVE 400                   TO RESP-KDSTATUS-API                    
120800       MOVE SPACES                TO MSG-CONV-AREA                        
120900       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
121000       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
121100       CALL WMSGCONV           USING MSG-CONV-AREA                        
121200       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
121300       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
121400     END-IF                                                               
121500     .                                                                    
121600* IMS SEKTIONER                                                           
121700 IMS-GU-WDF106 SECTION.                                                   
121800                                                                          
121900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
122000           DELIMITED BY SIZE INTO SSA1                                    
122100     MOVE 'WDF106  '        TO SSA2                                       
122200     MOVE 'GE  '            TO GODK-STATUSKODER                           
122300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-WDF106 SSA1 SSA2          
122400     MOVE WDF1-STATUS-CODE  TO STATUS-WS                                  
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     EJECT                                                                
122800 IMS-GU-BENA11 SECTION.                                                   
122900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
123000             DELIMITED BY SIZE INTO SSA1                                  
123100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
123200             DELIMITED BY SIZE INTO SSA2                                  
123300     MOVE '  GE' TO GODK-STATUSKODER                                      
123400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
123500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800     EJECT                                                                
123900 IMS-GHU-BENA11 SECTION.                                                  
124000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
124100             DELIMITED BY SIZE INTO SSA1                                  
124200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
124300             DELIMITED BY SIZE INTO SSA2                                  
124400     MOVE '  GE' TO GODK-STATUSKODER                                      
124500     CALL CBLTDLI USING GHU BENA2-PCB DLI-IO-WDD311 SSA1 SSA2             
124600     MOVE BENA2-STATUS-CODE TO STATUS-WS                                  
124700     PERFORM IMS-STATUSKONTROLL                                           
124800     .                                                                    
124900     EJECT                                                                
125000 IMS-REPL-BENA11 SECTION.                                                 
125100     MOVE '  ' TO GODK-STATUSKODER                                        
125200     CALL CBLTDLI USING REPL BENA2-PCB DLI-IO-WDD311                      
125300     MOVE BENA2-STATUS-CODE TO STATUS-WS                                  
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     .                                                                    
125600     EJECT                                                                
125700 IMS-GET-ARTC-ART SECTION.                                                
125800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
125900            DELIMITED BY SIZE INTO SSA1                                   
126000     MOVE '  GE' TO GODK-STATUSKODER                                      
126100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-601 SSA1                  
126200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
126300     PERFORM IMS-STATUSKONTROLL                                           
126400     SKIP3                                                                
126500     .                                                                    
126600 IMS-GET-ARTC-CLAG SECTION.                                               
126700     MOVE 'WLARTC11' TO SSA1                                              
126800     MOVE '  GE' TO GODK-STATUSKODER                                      
126900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-611 SSA1                 
127000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
127100     PERFORM IMS-STATUSKONTROLL                                           
127200     .                                                                    
127300     EJECT                                                                
127400 IMS-GET-WDK7-SLAG SECTION.                                               
127500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
127600            DELIMITED BY SIZE INTO SSA1                                   
127700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
127800            DELIMITED BY SIZE INTO SSA2                                   
127900     MOVE '  GE' TO GODK-STATUSKODER                                      
128000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-711 SSA1 SSA2             
128100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSKONTROLL                                           
128300     .                                                                    
128400     EJECT                                                                
128500 IMS-GET-ARTD-ART SECTION.                                                
128600     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
128700             DELIMITED BY SIZE INTO SSA1                                  
128800     MOVE '  GE' TO GODK-STATUSKODER                                      
128900     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1                      
129000     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
129100     PERFORM IMS-STATUSKONTROLL                                           
129200     SKIP3                                                                
129300     .                                                                    
129400 IMS-GET-ARTD-SALDO-IDDC SECTION.                                         
129500     STRING 'WLARTD11(WDD811KY>=' W-WDD811KY-MIN-X                        
129600                    '&WDD811KY<=' W-WDD811KY-MAX-X ')'                    
129700             DELIMITED BY SIZE INTO SSA1                                  
129800     MOVE '  GE' TO GODK-STATUSKODER                                      
129900     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
130000     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300     SKIP3                                                                
130400 IMS-GU-WDD7-ERSA01 SECTION.                                              
130500     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
130600            DELIMITED BY SIZE INTO SSA1                                   
130700     MOVE '  GE' TO GODK-STATUSKODER                                      
130800     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA-ERSA01 SSA1               
130900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     SKIP2                                                                
131300 IMS-GNP-WDD7-ERSA11 SECTION.                                             
131400     STRING 'WLERSA11(FLTEXT   =N)'                                       
131500            DELIMITED BY SIZE INTO SSA1                                   
131600     MOVE '  GE' TO GODK-STATUSKODER                                      
131700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-ERSA11 SSA1              
131800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100 IMS-GU-WDB601    SECTION.                                                
132200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
132300          DELIMITED BY SIZE INTO SSA1                                     
132400     MOVE '  GE' TO GODK-STATUSKODER                                      
132500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
132600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     IF SEGMENT-SAKNAS                                                    
132900         MOVE SPACE TO DCS-KDDC                                           
133000     END-IF                                                               
133100     .                                                                    
133200 IMS-GU-WDK712 SECTION.                                                   
133300     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
133400          DELIMITED BY SIZE INTO SSA1                                     
133500     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
133600          DELIMITED BY SIZE INTO SSA2                                     
133700     MOVE '  GE' TO GODK-STATUSKODER                                      
133800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K712 SSA1 SSA2            
133900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200     EJECT                                                                
134300 IMS-GU-WDK901 SECTION.                                                   
134400                                                                          
134500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
134600          DELIMITED BY SIZE INTO SSA1                                     
134700     MOVE '  GE' TO GODK-STATUSKODER                                      
134800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-K901 SSA1                 
134900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
135000     PERFORM IMS-STATUSKONTROLL                                           
135100     .                                                                    
135200     SKIP3                                                                
135300                                                                          
135400 IMS-STATUSKONTROLL SECTION.                                              
135500     SET STATUS-IX TO 1                                                   
135600     SEARCH GODK-STATUS AT END CALL FELLOG                                
135700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
135800     END-SEARCH                                                           
135900     .                                                                    
