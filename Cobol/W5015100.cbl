000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5015100.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   95/10/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        REPLACEMENT FROM SPARE PART TO EXCHANGE PARTT                    
001000*                                                                         
001100*        THE PROGRAM UPDATES   WLARTC (WDK6)                              
001200*        THE PROGRAM UPDATES   WLARTS (WDK7)                              
001300*        THE PROGRAM READS     WLBENA (WDD3)                              
001400*        THE PROGRAM UPDATES   WLINLE (WDL2)                              
001500*        THE PROGRAM UPDATES   WLINLC (WDL6)                              
001600*        THE PROGRAM UPDATES   WLFILB (WDR8)                              
001700*        THE PROGRAM UPDATES   WLLOGA (WDL9)                              
001800*        THE PROGRAM UPDATES   WLSAPA (WDR9)- PEDAL                       
001900*        THE PROGRAM UPDATES   WDA9   (WDA9)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W5T151                                              
002300*        MID:         W5I15101                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        MOD:         W5O15101                                            
002700*                                                                         
002800*    E'TRACKER 4823800  DAT. 20071204  NEW LDC                            
002900*    E'TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                      PIC X(08)   VALUE 'W5015100'.             
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                 PIC X(80) VALUE SPACE.                    
004300 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
004400 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
004500                                                                          
004600 77  YES                         PIC X      VALUE 'Y'.                    
004700 77  NOO                         PIC X      VALUE 'N'.                    
004800                                                                          
004900*01  -COPY WWDCKONS                                                       
005000                                                                          
005100 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +300 COMP SYNC.         
005200 77  WS-KVLS-TO-BEFORE           PIC S9(7)  VALUE 0    COMP-3.            
005300 77  WS-KVLS-TO-AFTER            PIC S9(7)  VALUE 0    COMP-3.            
005400 77  WS-KVLS-FROM-BEFORE         PIC S9(7)  VALUE 0    COMP-3.            
005500 77  WS-KVLS-FROM-AFTER          PIC S9(7)  VALUE 0    COMP-3.            
005600 77  WS-IDARTNR-TO               PIC S9(9)  VALUE 0    COMP-3.            
005700 77  WS-IDARTNR-FROM             PIC S9(9)  VALUE 0    COMP-3.            
005800 77  WS-FROM-ART-IDFKNGRP        PIC S9(5).                               
005900 77  WS-TO-ART-IDFKNGRP          PIC S9(5).                               
006000 77  WS-KVANTAL                  PIC  X(6).                               
006100 77  WS-KVANTAL-NUM              PIC S9(6)   VALUE 0.                     
006200 77  WS-IDAVINR                  PIC  X(6).                               
006300 77  WS-FROM-ART-KDPRODSL        PIC  9(2).                               
006400 77  WS-TO-ART-KDPRODSL          PIC  9(2).                               
006500 77  WS-FROM-CLAG-KDPSLLOC       PIC  9(2).                               
006600 77  WS-TO-CLAG-KDPSLLOC         PIC  9(2).                               
006700 77  WS-FROM-SLAG-PRAVCOST       PIC S9(7)V9(2)  COMP-3.                  
006800 77  WS-TO-SLAG-PRAVCOST         PIC S9(7)V9(2)  COMP-3.                  
006900 77  WS-IDANALYS                 PIC X(12) VALUE SPACE.                   
007000 77  WS-IDKONTO                  PIC X(10) VALUE SPACE.                   
007100 77  FILLER                      PIC  X(8)   VALUE 'AAAAAAAA'.            
007200 77  DC-IX                       PIC S9(4)   VALUE ZERO COMP-3.           
007300 77  WS-KDVALISO                 PIC  X(3).                               
007400 77  WS-IDFTG                    PIC X(2).                                
007500 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007600 77  W-EKH-IDARTNR               PIC X(9)    VALUE SPACE.                 
007700 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
007800 77  WS-DAAAPP                   PIC 9(6)    VALUE 200000.                
007900*YYMMDD                                                                   
008000 77  WS-TODAYS-DATE              PIC  9(6).                               
008100 77  WS-TODAYS-DATE-LOCAL        PIC  9(6).                               
008200 77  DAGENS-DATUM                PIC  9(8).                               
008300 77  TRANS-TID                   PIC  9(9).                               
008400*HHMMSSTH                                                                 
008500 77  WS-TIMECLOCK                PIC  9(8).                               
008600 77  WS-TIMECLOCK-LOCAL          PIC  9(8).                               
008700*                                                                         
008800 77  INDX                        PIC S9(9)   VALUE ZERO COMP-3.           
008900 77  TAB-IX                      PIC 9(3)    COMP-3.                      
009000 77  MAX-ANALYSNR-TAB-VALUE      PIC 9(3)    COMP-3  VALUE 30.            
009100                                                                          
009200*     -- INLEVERANS-ID (9-KOMPLEMENT TILL DATE+TIME)                      
009300 77      WS-DAINLEV              PIC 9(16)  VALUE ZERO.                   
009400*     -- DATE + TIME                                                      
009500 01      WS-TIAAAAMMDDTTMMSSTH   PIC 9(16)   VALUE ZERO.                  
009600 01      FILLER                  REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
009700   03    WS-TISEKEL               PIC 9(2).                               
009800   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
009900   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
010000                                                                          
010100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
010200                                                                          
010300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
010400     88  INDATA-OK                           VALUE 'Y'.                   
010500     88  INDATA-WRONG                        VALUE 'N'.                   
010600                                                                          
010700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
010800     88  KEYS-OK                             VALUE 'Y'.                   
010900     88  KEYS-WRONG                          VALUE 'N'.                   
011000                                                                          
011100 77  NEW-KEY-SW                  PIC X       VALUE 'N'.                   
011200     88  NEW-KEYS                            VALUE 'Y'.                   
011300     88  OLD-KEYS                            VALUE 'N'.                   
011400                                                                          
011500 01  ANALYSNR-FOUND-SW           PIC X.                                   
011600     88  ANALYSNR-FOUND          VALUE 'Y'.                               
011700                                                                          
011800 77  FROM-PART-KDPRODSL-OK       PIC X       VALUE 'Y'.                   
011900                                                                          
012000 77  IDARTNR-FROM-IS-EXCHANGE-SW PIC X.                                   
012100     88  IDARTNR-FROM-IS-EXCHANGE            VALUE 'Y'.                   
012200*                                                                         
012300*01  -COPY WWPRODSL                                                       
012400*                                                                         
012500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012600     88  OWN-MID                             VALUE '5151'.                
012700     88  GOOD-MID                            VALUE '5151'.                
012800     88  HELP-MID                            VALUE '0551'.                
012900                                                                          
013000**   ---- EKONOMITRANS FR. NDC-TRANSAKTION.                               
013100 01  -COPY W510A19    -PRE EKOA19-                                        
013200                                                                          
013300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013400 01  GENERAL-SUBPROGRAM.                                                  
013500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
014100*                                                                         
014200     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
014300*            SAP KONTROLL                                                 
014400     EJECT                                                                
014500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
014600*01 -COPY WMEDAREA                                                        
014700     SKIP3                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
014900*01 -COPY W009CIA                                                         
015000                                                                          
015100 01  FILLER                      PIC  X(12) VALUE                         
015200                                               'SAP KONTROLL'.            
015300*                                                                         
015400*01 -COPY W411SAP                                                         
015500     EJECT                                                                
015600 01  MESSAGE-CODES.                                                       
015700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016200     EJECT                                                                
016300 01  W-ERR-1.                                                             
016400   03    ERR-1.                                                           
016500     05  FILLER                  PIC X(40)   VALUE                        
016600        '501 FRÅN ART.NR. SAKNAS.                '.                       
016700     05  FILLER                  PIC X(40)   VALUE                        
016800        '501 FROM PARTNO. MISSING.               '.                       
016900   03    FILLER  REDEFINES  ERR-1.                                        
017000     05  FEL-501     OCCURS 2    PIC X(40).                               
017100*                                                                         
017200 01  W-ERR-2.                                                             
017300   03    ERR-2.                                                           
017400     05  FILLER                  PIC X(40)   VALUE                        
017500        '502 TILL ART.NR. SAKNAS.                '.                       
017600     05  FILLER                  PIC X(40)   VALUE                        
017700        '502 TO PARTNO. MISSING.                 '.                       
017800   03    FILLER  REDEFINES  ERR-2.                                        
017900     05  FEL-502     OCCURS 2    PIC X(40).                               
018000*                                                                         
018100 01  W-ERR-3.                                                             
018200   03    ERR-3.                                                           
018300     05  FILLER                  PIC X(40)   VALUE                        
018400        '503 ERS.KOD FÖR TILL ART. SKALL VARA <20'.                       
018500     05  FILLER                  PIC X(40)   VALUE                        
018600        '503 REPLACECODE FOR TO PART MUST BE < 20'.                       
018700   03    FILLER  REDEFINES  ERR-3.                                        
018800     05  FEL-503     OCCURS 2    PIC X(40).                               
018900*                                                                         
019000 01  W-ERR-4.                                                             
019100   03    ERR-4.                                                           
019200     05  FILLER                  PIC X(40)   VALUE                        
019300        '504 EN AV ART.SKALL HA PROD.SLAG14/24/34'.                       
019400     05  FILLER                  PIC X(40)   VALUE                        
019500        '504 ONE PART MUST HAVE PCODE 14/24/34/94'.                       
019600   03    FILLER  REDEFINES  ERR-4.                                        
019700     05  FEL-504     OCCURS 2    PIC X(40).                               
019800****  PRODUKTSLAG 94 GÄLLER ENDAST FÖR LOKALA ARTIKLAR USA/CANADA         
019900*                                                                         
020000 01  W-ERR-5.                                                             
020100   03    ERR-5.                                                           
020200     05  FILLER                  PIC X(40)   VALUE                        
020300        '505 FYLL I INDATA FÄLT                  '.                       
020400     05  FILLER                  PIC X(40)   VALUE                        
020500        '505 FILL INDATA FIELD                   '.                       
020600   03    FILLER  REDEFINES  ERR-5.                                        
020700     05  FEL-505     OCCURS 2    PIC X(40).                               
020800*                                                                         
020900 01  W-ERR-6.                                                             
021000   03    ERR-6.                                                           
021100     05  FILLER                  PIC X(40)   VALUE                        
021200        '506 EJ NUMERISKT                        '.                       
021300     05  FILLER                  PIC X(40)   VALUE                        
021400        '506 NOT NUMERIC                         '.                       
021500   03    FILLER  REDEFINES  ERR-6.                                        
021600     05  FEL-506     OCCURS 2    PIC X(40).                               
021700*                                                                         
021800 01  W-ERR-7.                                                             
021900   03    ERR-7.                                                           
022000     05  FILLER                  PIC X(40)   VALUE                        
022100        '507 KVANT STÖRRE ÄN FRÅN SALDO          '.                       
022200     05  FILLER                  PIC X(40)   VALUE                        
022300        '507 QUANT GR. THEN FROM PART SALDO      '.                       
022400   03    FILLER  REDEFINES  ERR-7.                                        
022500     05  FEL-507     OCCURS 2    PIC X(40).                               
022600*                                                                         
022700 01  W-ERR-8.                                                             
022800   03    ERR-8.                                                           
022900     05  FILLER                  PIC X(40)   VALUE                        
023000        '508 AVINR FÅR EJ BÖRJA MED 57.          '.                       
023100     05  FILLER                  PIC X(40)   VALUE                        
023200        '508 AVINR MAY NOT START WITH 57.        '.                       
023300   03    FILLER  REDEFINES  ERR-8.                                        
023400     05  FEL-508     OCCURS 2    PIC X(40).                               
023500*                                                                         
023600 01  W-ERR-9.                                                             
023700   03    ERR-9.                                                           
023800     05  FILLER                  PIC X(40)   VALUE                        
023900        '509 NOLL EJ GODKÄNT                     '.                       
024000     05  FILLER                  PIC X(40)   VALUE                        
024100        '509 ZERO IS NOT APPROVED                '.                       
024200   03    FILLER  REDEFINES  ERR-9.                                        
024300     05  FEL-509     OCCURS 2    PIC X(40).                               
024400*                                                                         
024500 01  W-ERR-10.                                                            
024600   03    ERR-10.                                                          
024700     05  FILLER                  PIC X(40)   VALUE                        
024800        '510 LAGERSALDO ÄR NOLL FÖR FRÅN ARTIKEL.'.                       
024900     05  FILLER                  PIC X(40)   VALUE                        
025000        '510 STOCK BALANCE IS ZERO AT FROM PART. '.                       
025100   03    FILLER  REDEFINES  ERR-10.                                       
025200     05  FEL-510     OCCURS 2    PIC X(40).                               
025300*                                                                         
025400 01  W-ERR-11.                                                            
025500   03    ERR-11.                                                          
025600     05  FILLER                  PIC X(40)   VALUE                        
025700        '511 LAGERSALDO ÄR NOLL FÖR TILL ARTIKEL.'.                       
025800     05  FILLER                  PIC X(40)   VALUE                        
025900        '511 STAOCK BALANCE IS ZERO AT TO PART.  '.                       
026000   03    FILLER  REDEFINES  ERR-11.                                       
026100     05  FEL-511     OCCURS 2    PIC X(40).                               
026200*                                                                         
026300 01  W-ERR-12.                                                            
026400   03    ERR-12.                                                          
026500     05  FILLER                  PIC X(40)   VALUE                        
026600        '512 KVANT MINDRE ÄN TILL SALDO          '.                       
026700     05  FILLER                  PIC X(40)   VALUE                        
026800        '512 QUANT LESS THEN TO PART SALDO       '.                       
026900   03    FILLER  REDEFINES  ERR-12.                                       
027000     05  FEL-512     OCCURS 2    PIC X(40).                               
027100*                                                                         
027200 01  W-ERR-13.                                                            
027300   03    ERR-13.                                                          
027400     05  FILLER                  PIC X(40)   VALUE                        
027500        '513 FEL ANALYSNR                       '.                        
027600     05  FILLER                  PIC X(40)   VALUE                        
027700        '513 WRONG ACCOUNT                      '.                        
027800   03    FILLER  REDEFINES  ERR-13.                                       
027900     05  FEL-513    OCCURS 2    PIC X(40).                                
028000*                                                                         
028100 01  W-ERR-14.                                                            
028200   03    ERR-14.                                                          
028300     05  FILLER                  PIC X(40)   VALUE                        
028400        '514 OIKA FUNKTIONSGRUPPER EJ TILLÅTET. '.                        
028500     05  FILLER                  PIC X(40)   VALUE                        
028600        '514 THE PARTS HAVE DIFFERENT FUNC.GROUP'.                        
028700   03    FILLER  REDEFINES  ERR-14.                                       
028800     05  FEL-514    OCCURS 2    PIC X(40).                                
028900*                                                                         
029000 01  W-ERR-15.                                                            
029100   03    ERR-15.                                                          
029200     05  FILLER                  PIC X(40)   VALUE                        
029300        '515 FEL DC                           '.                          
029400     05  FILLER                  PIC X(40)   VALUE                        
029500        '515 WRONG DC                         '.                          
029600   03    FILLER  REDEFINES  ERR-15.                                       
029700     05  FEL-515    OCCURS 2    PIC X(40).                                
029800*                                                                         
029900 01  W-ERR-16.                                                            
030000   03    ERR-16.                                                          
030100     05  FILLER                  PIC X(40)   VALUE                        
030200        '516 EJ OMLÄGGNING TILL SAMMA ARTIKELNR.'.                        
030300     05  FILLER                  PIC X(40)   VALUE                        
030400        '516 NO CHANGE TO SAME PART NUMBER.     '.                        
030500   03    FILLER  REDEFINES  ERR-16.                                       
030600     05  FEL-516    OCCURS 2    PIC X(40).                                
030700*                                                                         
030800 01  W-ERR-17.                                                            
030900   03    ERR-17.                                                          
031000     05  FILLER                  PIC X(40)   VALUE                        
031100        '517 EJ OMLÄGGNING MELLAN BYTES ARTIKEL.'.                        
031200     05  FILLER                  PIC X(40)   VALUE                        
031300        '517 NO CHANGE BETWEEN EXCHANGE PARTS.  '.                        
031400   03    FILLER  REDEFINES  ERR-17.                                       
031500     05  FEL-517    OCCURS 2    PIC X(40).                                
031600 01  W-ERR-18.                                                            
031700   03    ERR-18.                                                          
031800     05  FILLER                  PIC X(40)   VALUE                        
031900        '518 INGEN UPPDATERNING GJORD.          '.                        
032000     05  FILLER                  PIC X(40)   VALUE                        
032100        '518 NO UPDATE DONE.                    '.                        
032200   03    FILLER  REDEFINES  ERR-18.                                       
032300     05  FEL-518    OCCURS 2    PIC X(40).                                
032400*******INFO MESSAGES*********************************                     
032500 01  W-INF-01.                                                            
032600   03    INF-01.                                                          
032700     05  FILLER                  PIC X(40)   VALUE                        
032800        '501 VARNING! OLIKA FUNKTIONSGRUPPER.   '.                        
032900     05  FILLER                  PIC X(40)   VALUE                        
033000        '501 WARNING! DIFFERENT FUNCTION GROUPS.'.                        
033100   03    FILLER  REDEFINES  INF-01.                                       
033200     05  INF-501    OCCURS 2    PIC X(40).                                
033300*                                                                         
033400*- - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                  
033500 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
033600 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
033700*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
033800     EJECT                                                                
033900*                                                                         
034000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
034100*                                                                         
034200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
034300     SKIP3                                                                
034400*01 -COPY WMSGINIT                                                        
034500     SKIP3                                                                
034600 01  FILLER                      PIC X(16)  VALUE 'WDATKONV '.            
034700*   -COPY WDATAREA                                                        
034800     EJECT                                                                
034900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
035000*                                                                         
035100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
035200     SKIP3                                                                
035300*01  MID -COPY W5I15101                                                   
035400     EJECT                                                                
035500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
035600     SKIP3                                                                
035700*01  -COPY WMSGAREA                                                       
035800     EJECT                                                                
035900     03  MOD REDEFINES MSG-AREA.                                          
036000*      05  -COPY W5O15101                                                 
036100     EJECT                                                                
036200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
036300     SKIP3                                                                
036400*01  -COPY WMFSAREA                                                       
036500     EJECT                                                                
036600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
036700*                                                                         
036800     EJECT                                                                
036900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037000     SKIP3                                                                
037100 01  KEYS-TO-DLI.                                                         
037200     03  W-IDARTNR-X.                                                     
037300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
037400     03  W-IDARTNR-FROM-X.                                                
037500         05  W-IDARTNR-FROM      PIC S9(9)   VALUE ZERO COMP-3.           
037600     03  W-IDARTNR-TO-X.                                                  
037700         05  W-IDARTNR-TO        PIC S9(9)   VALUE ZERO COMP-3.           
037800                                                                          
037900     03  W-KDSEGKEY-X.                                                    
038000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
038100                                                                          
038200     03  W-IDDC-X.                                                        
038300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
038400                                                                          
038500     03  W-DAINLEV-X.                                                     
038600         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
038700                                                                          
038800     03  W-WDR801KY-X.                                                    
038900         05  W-WDR801KY          PIC X(27)    VALUE SPACE.                
039000*----> SEKUNDÄR INDEX ARTIKELBENÄMNING                                    
039100                                                                          
039200     03  FILLER                  PIC X(8)    VALUE ALL 'B'.               
039300     03  W-WDD3BSEQ-X.                                                    
039400         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
039500                                                                          
039600     03  W-IDSKYLT-X             PIC X(3).                                
039700                                                                          
039800     03  W-IDARTNR-WDA9-X.                                                
039900         05  W-IDARTNR-WDA9      PIC S9(9)  COMP-3.                       
040000     03  W-DAAAPP-WDA9-X.                                                 
040100         05  W-DAAAPP-WDA9       PIC  9(6).                               
040200     03  W-IDDC-B6-X.                                                     
040300         05 W-IDDC-B6                  PIC X(2).                          
040400                                                                          
040500     SKIP2                                                                
040600*    --- STATUS-KOD FRÅN IMS                                              
040700 01  STATUS-WS                   PIC XX.                                  
040800     88  SEGMENT-FOUND                       VALUE '  '.                  
040900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
041000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
041100     SKIP2                                                                
041200 01  GOOD-STATUSCODES.                                                    
041300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041400     SKIP3                                                                
041500 01  SSA1                        PIC X(256).                              
041600 01  SSA2                        PIC X(512).                              
041700     EJECT                                                                
041800*    --- IMS FUNCTION CODES                                               
041900*01  -COPY W0003                                                          
042000     EJECT                                                                
042100*    ---  DLI INPUT-OUTPUT AREA                                           
042200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
042300 01  DLI-IO-AREA-1.                                                       
042400     SKIP3                                                                
042500*    03  IO-AREA-1               PIC X(900)  VALUE SPACE.                 
042600*    03  WLARTC01 REDEFINES IO-AREA-1.                                    
042700*    03  -COPY WDK601   -PRE FROM-                                        
042800*    03  WLARTC11 REDEFINES IO-AREA-1.                                    
042900*    03  -COPY WDK611   -PRE FROM-                                        
043000     SKIP3                                                                
043100     SKIP3                                                                
043200 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA2'.         
043300 01  DLI-IO-AREA-2.                                                       
043400     03  IO-AREA-2               PIC X(512)  VALUE SPACE.                 
043500     SKIP3                                                                
043600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
043700 01  DLI-IO-AREA-3.                                                       
043800     SKIP3                                                                
043900*    03  IO-AREA-3               PIC X(608)  VALUE SPACE.                 
044000*    03  WLARTC01 REDEFINES IO-AREA-3.                                    
044100*    03  -COPY WDK601 -PRE TO-                                            
044200*    03  WLARTC11 REDEFINES IO-AREA-3.                                    
044300*    03  -COPY WDK611   -PRE TO-                                          
044400     SKIP3                                                                
044500     SKIP3                                                                
044600 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA4'.         
044700 01  DLI-IO-AREA-4.                                                       
044800     03  IO-AREA-4               PIC X(512)  VALUE SPACE.                 
044900     SKIP3                                                                
045000 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA5'.         
045100 01  DLI-IO-AREA-5.                                                       
045200     03  IO-AREA-5               PIC X(300)  VALUE SPACE.                 
045300     03  WLARTS01 REDEFINES IO-AREA-5.                                    
045400*        05  -COPY WDK711   -PRE FROM-                                    
045500     SKIP3                                                                
045600 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA6'.         
045700 01  DLI-IO-AREA-6.                                                       
045800     03  IO-AREA-6               PIC X(300)  VALUE SPACE.                 
045900     03  WLARTS11 REDEFINES IO-AREA-6.                                    
046000*        05  -COPY WDK711   -PRE TO-                                      
046100     SKIP3                                                                
046200 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA7'.         
046300 01  DLI-IO-AREA-7.                                                       
046400     03  IO-AREA-7               PIC X(230)  VALUE SPACE.                 
046500     SKIP3                                                                
046600     03  WLBENA11 REDEFINES IO-AREA-7.                                    
046700*        05  -COPY WDD311                                                 
046800     SKIP3                                                                
046900 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA8'.         
047000 01  DLI-IO-AREA-8.                                                       
047100     03  IO-AREA-8               PIC X(230)  VALUE SPACE.                 
047200     03  WLINLE01 REDEFINES IO-AREA-8.                                    
047300*        05  -COPY WDL201                                                 
047400     SKIP3                                                                
047500     03  WLINLE11 REDEFINES IO-AREA-8.                                    
047600*        05  -COPY WDL211   -PRE WDL211-                                  
047700     SKIP3                                                                
047800     03  WLINLE21 REDEFINES IO-AREA-8.                                    
047900*        05  -COPY WDL221                                                 
048000     SKIP3                                                                
048100     03  WLINLC01 REDEFINES IO-AREA-8.                                    
048200*        05  -COPY WDL601                                                 
048300     SKIP3                                                                
048400     03  WLINLC11 REDEFINES IO-AREA-8.                                    
048500*        05  -COPY WDL611                                                 
048600     SKIP3                                                                
048700 01    FILLER                    PIC X(16)  VALUE 'DLI IO-AREA9'.         
048800 01  DLI-IO-AREA-9.                                                       
048900     03  IO-AREA-9               PIC X(256)  VALUE SPACE.                 
049000     03  WLFILB01 REDEFINES IO-AREA-9.                                    
049100*        05  -COPY WDR801                                                 
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
049400*01  WLLOGA01    -COPY WDL901                                             
049500     EJECT                                                                
049600 01  FILLER                      PIC X(16)   VALUE 'WLSAPA01   '.         
049700 01  WLSAPA01   -COPY WDR901.                                             
049800     05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
049900     EJECT                                                                
050000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA901'.         
050100 01  DLI-IO-WDA901.                                                       
050200*    03  -COPY WDA901                                                     
050300     EJECT                                                                
050400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA912'.         
050500 01  DLI-IO-WDA912.                                                       
050600*    03  -COPY WDA912                                                     
050700                                                                          
050800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050900 01   DLI-IO-AREA-B601.                                                   
051000*     03  -COPY WDB601                                                    
051100     EJECT                                                                
051200                                                                          
051300 LINKAGE SECTION.                                                         
051400                                                                          
051500*01  -COPY W0009   -PRE MSG-                                              
051600*01  -COPY W0008   -PRE USEA-                                             
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
051900*01  -COPY W0008   -PRE ARTC-                                             
052000     05  FILLER                  PIC X.                                   
052100     EJECT                                                                
052200*01  -COPY W0008   -PRE ARTC2-                                            
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01  -COPY W0008   -PRE ARTS-                                             
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01  -COPY W0008   -PRE ARTS2-                                            
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100*01  -COPY W0008   -PRE BENA-                                             
053200     05  FILLER                  PIC X.                                   
053300     EJECT                                                                
053400*01  -COPY W0008   -PRE INLE-                                             
053500     05  FILLER                  PIC X.                                   
053600     EJECT                                                                
053700*01  -COPY W0008   -PRE INLC-                                             
053800     05  FILLER                  PIC X.                                   
053900     EJECT                                                                
054000*01  -COPY W0008   -PRE FILB-                                             
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300*01  -COPY W0008   -PRE LOGA-                                             
054400     05  FILLER                  PIC X.                                   
054500     EJECT                                                                
054600*01  -COPY W0008   -PRE SAPA-                                             
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01  -COPY W0008   -PRE SAPC-                                             
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01  -COPY W0008   -PRE WDA9-                                             
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01  -COPY W0008   -PRE WDB6-                                             
055600     05  FILLER                  PIC X.                                   
055700     EJECT                                                                
055800 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB ARTC-PCB ARTC2-PCB           
055900                           ARTS-PCB ARTS2-PCB BENA-PCB INLE-PCB           
056000                           INLC-PCB FILB-PCB LOGA-PCB SAPA-PCB            
056100                           SAPC-PCB WDA9-PCB WDB6-PCB.                    
056200 W50151 SECTION.                                                          
056300                                                                          
056400     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB ARTC-PCB ARTC2-PCB           
056500                           ARTS-PCB ARTS2-PCB BENA-PCB INLE-PCB           
056600                           INLC-PCB FILB-PCB LOGA-PCB SAPA-PCB            
056700                           SAPC-PCB WDA9-PCB WDB6-PCB.                    
056800                                                                          
056900     PERFORM IMS-GET-MSG                                                  
057000     IF SEGMENT-FOUND                                                     
057100       PERFORM A-INIT                                                     
057200       PERFORM B-MOVE-KEYS                                                
057300       IF KEYS-OK                                                         
057400         PERFORM D-CHECK-KEYS                                             
057500         IF KEYS-OK                                                       
057600           IF MFS-UPDATE                                                  
057700              PERFORM G-CHECK-INPUT                                       
057800              IF INDATA-OK                                                
057900                 PERFORM H-UPDATE                                         
058000              END-IF                                                      
058100           ELSE                                                           
058200             PERFORM E-SAME-PAGE                                          
058300           END-IF                                                         
058400         ELSE                                                             
058500           IF MFS-UPDATE                                                  
058600             MOVE FEL-518   (INDX)   TO MOD-TEMFSFEL                      
058700           END-IF                                                         
058800         END-IF                                                           
058900       END-IF                                                             
059000       MOVE MAX-MOD-LENGTH TO MSG-KVLL                                    
059100       PERFORM IMS-INSERT-MSG                                             
059200     END-IF                                                               
059300                                                                          
059400     MOVE ZERO TO RETURN-CODE                                             
059500     GOBACK                                                               
059600     .                                                                    
059700     EJECT                                                                
059800 A-INIT SECTION.                                                          
059900                                                                          
060000     IF MSG-DOUBLE-TRANSACTIONS                                           
060100       MOVE MSG-INDATA-MINUS-2-TRANSACT  TO MID-W5I15101                  
060200       MOVE MSG-IDTRANS-2                TO MFS-IDTRANS                   
060300       MOVE MSG-KDMFSFOR-2               TO MFS-KDMFSFOR                  
060400     ELSE                                                                 
060500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I15101                  
060600       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
060700       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
060800     END-IF                                                               
060900                                                                          
061000     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
061100     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
061200     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
061300                                                                          
061400     MOVE 'N'             TO NEW-KEY-SW                                   
061500                                                                          
061600     MOVE LOW-VALUE       TO MSG-AREA                                     
061700     MOVE 'W5O151N1'      TO MFS-IDMOD                                    
061800     MOVE '5151'          TO MOD-IDTRANS                                  
061900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL                                 
062000                             MOD-TEMFSINF                                 
062100     MOVE '1'             TO W-KDSEGKEY                                   
062200                                                                          
062300     ACCEPT WS-TODAYS-DATE   FROM DATE                                    
062400     ACCEPT WS-TIMECLOCK     FROM TIME                                    
062500                                                                          
062600*    -- IF ANSWER TO SCREEN:        MSG-KVLL = MOD-LENGTH + 4             
062700*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
062800     COMPUTE MSG-KVLL = LENGTH OF MAX-MOD-LENGTH  +  4                    
062900                                                                          
063000     IF ENGLISH-TEXT                                                      
063100         MOVE +2                          TO   INDX                       
063200     ELSE                                                                 
063300         MOVE +1                          TO   INDX                       
063400     END-IF                                                               
063500                                                                          
063600     IF OWN-MID OR HELP-MID                                               
063700       CONTINUE                                                           
063800     ELSE                                                                 
063900       MOVE ZERO                    TO MID-FROM-IDARTNR-UT                
064000                                       MID-TO-IDARTNR-UT                  
064100       MOVE SPACE                   TO MID-IDDC-UT                        
064200       MOVE '+++++++++'             TO MID-FROM-IDARTNR-IN                
064300                                       MID-TO-IDARTNR-IN                  
064400       MOVE '++'                    TO MID-IDDC-IN                        
064500       MOVE '++++++'                TO MID-KVANTAL                        
064600       MOVE '++++++++++++'          TO MID-IDANALYS                       
064700       MOVE '++++++++++'            TO MID-IDKONTO                        
064800       MOVE '++++++'                TO MID-IDAVINR                        
064900     END-IF                                                               
065000                                                                          
065100     .                                                                    
065200     EJECT                                                                
065300 B-MOVE-KEYS SECTION.                                                     
065400                                                                          
065500     MOVE YES TO KEYS-SW                                                  
065600                                                                          
065700*                                                                         
065800*    -- CHECK OF IDARTNR FROM                                             
065900*                                                                         
066000                                                                          
066100     IF MID-FROM-IDARTNR-IN = ALL '+'                                     
066200        MOVE MID-FROM-IDARTNR-UT     TO MOD-FROM-IDARTNR-UT               
066300                                        W-IDARTNR-FROM                    
066400                                        WS-IDARTNR-FROM                   
066500     ELSE                                                                 
066600       IF MID-FROM-IDARTNR-IN NUMERIC                                     
066700         IF MID-FROM-IDARTNR-IN > ZERO                                    
066800           MOVE MID-FROM-IDARTNR-IN   TO MOD-FROM-IDARTNR-UT              
066900                                         W-IDARTNR-FROM                   
067000                                         WS-IDARTNR-FROM                  
067100         ELSE                                                             
067200            MOVE FEL-509  (INDX)   TO MOD-TEMFSFEL                        
067300            MOVE NOO               TO KEYS-SW                             
067400         END-IF                                                           
067500       ELSE                                                               
067600          MOVE FEL-506    (INDX)   TO MOD-TEMFSFEL                        
067700          MOVE NOO                 TO KEYS-SW                             
067800       END-IF                                                             
067900       MOVE YES                    TO NEW-KEY-SW                          
068000     END-IF                                                               
068100                                                                          
068200*                                                                         
068300*    -- CHECK OF IDARTNR TO                                               
068400*                                                                         
068500                                                                          
068600     IF MID-TO-IDARTNR-IN = ALL '+'                                       
068700       MOVE MID-TO-IDARTNR-UT      TO MOD-TO-IDARTNR-UT                   
068800                                      WS-IDARTNR-TO                       
068900                                      W-IDARTNR-TO                        
069000                                      W-IDARTNR-WDA9                      
069100     ELSE                                                                 
069200       IF MID-TO-IDARTNR-IN NUMERIC                                       
069300         IF MID-TO-IDARTNR-IN > ZERO                                      
069400           MOVE MID-TO-IDARTNR-IN  TO MOD-TO-IDARTNR-UT                   
069500                                      WS-IDARTNR-TO                       
069600                                      W-IDARTNR-TO                        
069700                                      W-IDARTNR-WDA9                      
069800         ELSE                                                             
069900           IF KEYS-OK                                                     
070000             MOVE FEL-509 (INDX)   TO MOD-TEMFSFEL                        
070100             MOVE NOO              TO KEYS-SW                             
070200           END-IF                                                         
070300         END-IF                                                           
070400       ELSE                                                               
070500         IF KEYS-OK                                                       
070600           MOVE FEL-506   (INDX)   TO MOD-TEMFSFEL                        
070700           MOVE NOO                TO KEYS-SW                             
070800         END-IF                                                           
070900       END-IF                                                             
071000       MOVE YES                    TO NEW-KEY-SW                          
071100     END-IF                                                               
071200                                                                          
071300     IF KEYS-OK                                                           
071400       IF  WS-IDARTNR-FROM > ZERO                                         
071500       AND WS-IDARTNR-TO   > ZERO                                         
071600         IF WS-IDARTNR-FROM = WS-IDARTNR-TO                               
071700           MOVE FEL-516 (INDX)     TO MOD-TEMFSFEL                        
071800           MOVE NOO                TO KEYS-SW                             
071900         END-IF                                                           
072000       END-IF                                                             
072100     END-IF                                                               
072200*                                                                         
072300*    -- CHECK OF IDDC                                                     
072400*                                                                         
072500                                                                          
072600     IF MID-IDDC-IN = ALL '+'                                             
072700       MOVE MID-IDDC-UT            TO W-IDDC                              
072800     ELSE                                                                 
072900       MOVE MID-IDDC-IN            TO W-IDDC                              
073000       MOVE YES                    TO NEW-KEY-SW                          
073100     END-IF                                                               
073200                                                                          
073300     IF W-IDDC > SPACE                                                    
073400        MOVE W-IDDC TO W-IDDC-B6                                          
073500        PERFORM IMS-GU-WDB601                                             
073600        IF DCS-CDC OR DCS-CDC-TR OR                                       
073700          (DCS-SDC AND DCS-HOLLAND) OR                                    
073800           DCS-NDC-NA OR DCS-NDC-PF                                       
073900           CONTINUE                                                       
074000        ELSE                                                              
074100           IF KEYS-OK                                                     
074200              MOVE FEL-515   (INDX)   TO MOD-TEMFSFEL                     
074300              MOVE NOO                TO KEYS-SW                          
074400           END-IF                                                         
074500        END-IF                                                            
074600     ELSE                                                                 
074700       IF KEYS-OK                                                         
074800         MOVE FEL-515     (INDX)   TO MOD-TEMFSFEL                        
074900         MOVE NOO                  TO KEYS-SW                             
075000       END-IF                                                             
075100     END-IF                                                               
075200     MOVE W-IDDC                   TO MOD-IDDC-UT                         
075300                                                                          
075400     IF KEYS-OK                                                           
075500     MOVE ALL '+'              TO MSGI-WMSGINIT                           
075600     MOVE '001'                TO MSGI-KDCALL                             
075700     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
075800     MOVE '5151'               TO MSGI-IDTRANS                            
075900     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
076000     MOVE MID-IDDC-IN          TO MSGI-IDDC                               
076100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
076200                                                                          
076300       IF DCS-NDC-NA OR                                                   
076400          DCS-IDDC = WC-LDC-GB-3A                                         
076500                                                                          
076600         MOVE ALL '+'          TO MSGI-WMSGINIT                           
076700         MOVE '011'            TO MSGI-KDCALL                             
076800         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
076900                                    MSGI-IDLTERM-USER                     
077000         MOVE MID-IDDC-IN      TO MSGI-IDDC                               
077100         MOVE WS-TODAYS-DATE   TO MSGI-TILOKDAT                           
077200         MOVE WS-TIMECLOCK(1:4) TO MSGI-TILOKTID                          
077300         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
077400         MOVE MSGI-TILOKDAT    TO WS-TODAYS-DATE-LOCAL                    
077500         MOVE MSGI-TILOKTID    TO WS-TIMECLOCK-LOCAL                      
077600       END-IF                                                             
077700     END-IF                                                               
077800                                                                          
077900     MOVE MFS-ERASE-FIELD          TO MOD-FROM-IDARTNR-IN                 
078000                                      MOD-TO-IDARTNR-IN                   
078100                                      MOD-IDDC-IN                         
078200                                                                          
078300     IF NOT GOOD-MID                                                      
078400       MOVE MFS-ERASE-FIELD        TO MOD-FROM-IDARTNR-UT                 
078500                                      MOD-TO-IDARTNR-UT                   
078600                                      MOD-IDDC-UT                         
078700     END-IF                                                               
078800                                                                          
078900     IF KEYS-WRONG                                                        
079000       PERFORM MFS-ERASE-FIELD-IN                                         
079100     END-IF                                                               
079200                                                                          
079300     IF NEW-KEYS                                                          
079400       MOVE MFS-ERASE-FIELD        TO MOD-KVANTAL                         
079500                                      MOD-IDKONTO                         
079600                                      MOD-IDANALYS                        
079700                                      MOD-IDAVINR                         
079800       MOVE SPACE                  TO MFS-KDTRTYP                         
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200 D-CHECK-KEYS                SECTION.                                     
080300                                                                          
080400       IF WS-IDARTNR-FROM > ZERO                                          
080500          MOVE WS-IDARTNR-FROM     TO MOD-FROM-IDARTNR-UT                 
080600                                      W-IDARTNR-FROM                      
080700       ELSE                                                               
080800          MOVE FEL-501    (INDX)   TO MOD-TEMFSFEL                        
080900          MOVE NOO                 TO KEYS-SW                             
081000       END-IF                                                             
081100                                                                          
081200       IF WS-IDARTNR-TO > ZERO                                            
081300          MOVE WS-IDARTNR-TO       TO MOD-TO-IDARTNR-UT                   
081400                                      W-IDARTNR-TO                        
081500                                      W-IDARTNR-WDA9                      
081600       ELSE                                                               
081700         IF KEYS-OK                                                       
081800           MOVE FEL-502   (INDX)   TO MOD-TEMFSFEL                        
081900           MOVE NOO                TO KEYS-SW                             
082000         END-IF                                                           
082100       END-IF                                                             
082200                                                                          
082300     IF KEYS-WRONG                                                        
082400       PERFORM MFS-ERASE-FIELD-IN                                         
082500     ELSE                                                                 
082600       PERFORM S01-CHECK-IDARTNR-FROM-WDK6                                
082700                                                                          
082800       PERFORM S02-CHECK-IDARTNR-TO-WDK6                                  
082900                                                                          
083000       IF (DCS-SDC AND DCS-HOLLAND) OR                                    
083100           DCS-NDC-NA OR DCS-NDC-PF                                       
083200         PERFORM S03-CHECK-FROM-PARTNR-WDK7                               
083300                                                                          
083400         PERFORM S04-CHECK-TO-PARTNR-WDK7                                 
083500       END-IF                                                             
083600                                                                          
083700       IF INDATA-OK                                                       
083800         PERFORM S05-SHOW-PART-TEXT-WDD3                                  
083900       END-IF                                                             
084000                                                                          
084100       IF INDATA-OK                                                       
084200         PERFORM S06-SHOW-INFO                                            
084300       END-IF                                                             
084400                                                                          
084500       IF OLD-KEYS                                                        
084600         IF MID-KVANTAL NOT = '++++++'                                    
084700           PERFORM DA-SHOW-COMP-MOD-KVANTAL                               
084800         END-IF                                                           
084900       END-IF                                                             
085000     END-IF                                                               
085100                                                                          
085200     .                                                                    
085300     EJECT                                                                
085400 DA-SHOW-COMP-MOD-KVANTAL             SECTION.                            
085500                                                                          
085600*SHOW AMOUNT BEFORE AND AFTER COMPUTATION.                                
085700     IF MID-KVANTAL  = ALL '+'                                            
085800       CONTINUE                                                           
085900     ELSE                                                                 
086000       IF MID-KVANTAL NUMERIC                                             
086100         MOVE MID-KVANTAL              TO WS-KVANTAL                      
086200                                                                          
086300         INSPECT WS-KVANTAL REPLACING LEADING SPACE BY ZERO               
086400         MOVE WS-KVANTAL               TO WS-KVANTAL-NUM                  
086500       ELSE                                                               
086600         MOVE NOO TO INDATA-SW                                            
086700       END-IF                                                             
086800     END-IF                                                               
086900                                                                          
087000                                                                          
087100     IF INDATA-OK                                                         
087200                                                                          
087300       IF WS-KVANTAL-NUM > ZERO                                           
087400         IF WS-KVANTAL-NUM <= WS-KVLS-FROM-BEFORE                         
087500                                                                          
087600            COMPUTE WS-KVLS-TO-AFTER =                                    
087700                    WS-KVLS-TO-BEFORE + WS-KVANTAL-NUM                    
087800            END-COMPUTE                                                   
087900            MOVE WS-KVLS-TO-AFTER  TO MOD-TO-KVLS-AFTER                   
088000                                                                          
088100                                                                          
088200            COMPUTE WS-KVLS-FROM-AFTER =                                  
088300                    WS-KVLS-FROM-BEFORE - WS-KVANTAL-NUM                  
088400            END-COMPUTE                                                   
088500            MOVE WS-KVLS-FROM-AFTER  TO MOD-FROM-KVLS-AFTER               
088600                                                                          
088700            IF WS-KVLS-FROM-AFTER >= ZERO                                 
088800               MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                
088900               MOVE MFS-NUM-FIELD-OK    TO MOD-KVANTAL-ATTR               
089000            ELSE                                                          
089100               MOVE FEL-507 (INDX) TO MOD-TEMFSFEL                        
089200               MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                
089300               MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR               
089400               MOVE NOO TO INDATA-SW                                      
089500            END-IF                                                        
089600                                                                          
089700         ELSE                                                             
089800            MOVE FEL-507 (INDX) TO  MOD-TEMFSFEL                          
089900            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                   
090000            MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR                  
090100            MOVE NOO TO INDATA-SW                                         
090200         END-IF                                                           
090300       ELSE                                                               
090400          MOVE FEL-509 (INDX)  TO   MOD-TEMFSFEL                          
090500          MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                     
090600          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR                    
090700          MOVE NOO TO INDATA-SW                                           
090800       END-IF                                                             
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200 E-SAME-PAGE SECTION.                                                     
091300                                                                          
091400     IF (OWN-MID OR HELP-MID) AND OLD-KEYS                                
091500       IF MID-KVANTAL        = ALL '+'   AND                              
091600          MID-IDKONTO        = ALL '+'   AND                              
091700          MID-IDANALYS       = ALL '+'   AND                              
091800          MID-IDAVINR        = ALL '+'                                    
091900          CONTINUE                                                        
092000         PERFORM MFS-ERASE-FIELD-IN                                       
092100         PERFORM MFS-ERASE-FIELD-OUT                                      
092200       ELSE                                                               
092300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
092400         CALL WMEDKONV USING MED-WMEDAREA                                 
092500         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
092600         PERFORM EA-MID-INDATA-TO-MOD                                     
092700       END-IF                                                             
092800     ELSE                                                                 
092900       PERFORM MFS-ERASE-FIELD-IN                                         
093000       MOVE    MFS-ERASE-FIELD TO MOD-FROM-KVLS-AFTER                     
093100                                  MOD-TO-KVLS-AFTER                       
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500 EA-MID-INDATA-TO-MOD SECTION.                                            
093600                                                                          
093700* * * FOR EVERY MID-FIELD                                                 
093800     IF MID-KVANTAL = ALL '+'                                             
093900       MOVE MFS-ERASE-FIELD         TO MOD-KVANTAL                        
094000     ELSE                                                                 
094100       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                        
094200       MOVE MFS-ADD-READ-FIELD      TO MOD-KVANTAL-ATTR                   
094300     END-IF                                                               
094400                                                                          
094500     IF MID-IDANALYS   = ALL '+'                                          
094600       MOVE MFS-ERASE-FIELD         TO MOD-IDANALYS                       
094700     ELSE                                                                 
094800       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDANALYS                       
094900       MOVE MFS-ADD-READ-FIELD      TO MOD-IDANALYS-ATTR                  
095000     END-IF                                                               
095100                                                                          
095200     IF MID-IDKONTO    = ALL '+'                                          
095300       MOVE MFS-ERASE-FIELD         TO MOD-IDKONTO                        
095400     ELSE                                                                 
095500       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDKONTO                        
095600       MOVE MFS-ADD-READ-FIELD      TO MOD-IDKONTO-ATTR                   
095700     END-IF                                                               
095800                                                                          
095900     IF MID-IDAVINR = ALL '+'                                             
096000       MOVE MFS-ERASE-FIELD         TO MOD-IDAVINR                        
096100     ELSE                                                                 
096200       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDAVINR                        
096300       MOVE MFS-ADD-READ-FIELD      TO MOD-IDAVINR-ATTR                   
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700 G-CHECK-INPUT SECTION.                                                   
096800                                                                          
096900     MOVE YES  TO INDATA-SW                                               
097000     IF MID-KVANTAL      = ALL '+'   AND                                  
097100        MID-IDAVINR      = ALL '+'   AND                                  
097200        MID-IDKONTO      = ALL '+'   AND                                  
097300        MID-IDANALYS     = ALL '+'                                        
097400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
097500       CALL WMEDKONV USING MED-WMEDAREA                                   
097600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
097700       PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                  
097800       PERFORM MFS-DO-NOT-TOUCH-FIELD-OUT                                 
097900       MOVE NOO TO INDATA-SW                                              
098000     ELSE                                                                 
098100                                                                          
098200       PERFORM GA-CHECK-MID-KVANTAL                                       
098300                                                                          
098400       IF INDATA-OK                                                       
098500         PERFORM GC-CHECK-MID-IDAVINR                                     
098600       ELSE                                                               
098700         MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDAVINR                      
098800       END-IF                                                             
098900                                                                          
099000       IF INDATA-OK                                                       
099100          PERFORM GE-CHECK-MID-IDKONTO                                    
099200       ELSE                                                               
099300         MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDKONTO                      
099400       END-IF                                                             
099500                                                                          
099600       IF INDATA-OK                                                       
099700         PERFORM GB-CHECK-MID-IDANALYS                                    
099800       ELSE                                                               
099900         MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDANALYS                     
100000       END-IF                                                             
100100                                                                          
100200       IF INDATA-WRONG                                                    
100300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
100400         CALL WMEDKONV USING MED-WMEDAREA                                 
100500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
100600         PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                
100700         PERFORM MFS-DO-NOT-TOUCH-FIELD-OUT                               
100800       END-IF                                                             
100900     END-IF                                                               
101000     .                                                                    
101100     EJECT                                                                
101200 GA-CHECK-MID-KVANTAL             SECTION.                                
101300                                                                          
101400     IF MID-KVANTAL  = ALL '+'                                            
101500        MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KVANTAL                     
101600        MOVE MFS-NUM-FIELD-WRONG       TO MOD-KVANTAL-ATTR                
101700        MOVE ZERO                      TO MOD-KVANTAL                     
101800        IF INDATA-OK                                                      
101900          MOVE FEL-505 (INDX)          TO MOD-TEMFSFEL                    
102000          MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                     
102100          MOVE MFS-NUM-FIELD-WRONG     TO MOD-KVANTAL-ATTR                
102200          MOVE NOO TO INDATA-SW                                           
102300        END-IF                                                            
102400     ELSE                                                                 
102500       IF MID-KVANTAL NUMERIC                                             
102600          MOVE MID-KVANTAL             TO WS-KVANTAL                      
102700                                          MOD-KVANTAL                     
102800          INSPECT WS-KVANTAL REPLACING LEADING SPACE BY ZERO              
102900          INSPECT MOD-KVANTAL REPLACING LEADING ZERO BY SPACE             
103000          MOVE WS-KVANTAL              TO WS-KVANTAL-NUM                  
103100          MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                     
103200          MOVE MFS-NUM-FIELD-OK        TO MOD-KVANTAL-ATTR                
103300       ELSE                                                               
103400                                                                          
103500          IF INDATA-OK                                                    
103600            MOVE FEL-506 (INDX)        TO MOD-TEMFSFEL                    
103700            MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVANTAL                    
103800            MOVE MFS-NUM-FIELD-WRONG   TO MOD-KVANTAL-ATTR                
103900            MOVE NOO TO INDATA-SW                                         
104000          END-IF                                                          
104100       END-IF                                                             
104200     END-IF                                                               
104300                                                                          
104400                                                                          
104500     IF INDATA-OK                                                         
104600*HUR MYCKET (ANTAL) SKALL DET TILLÅTAS ATT MATA IN I MID-KVANTAL?         
104700       IF WS-KVANTAL-NUM > ZERO                                           
104800         IF WS-KVANTAL-NUM <= WS-KVLS-FROM-BEFORE                         
104900            MOVE MFS-NUM-FIELD-OK TO MOD-KVANTAL-ATTR                     
105000                                                                          
105100            COMPUTE WS-KVLS-TO-AFTER =                                    
105200                    WS-KVLS-TO-BEFORE + WS-KVANTAL-NUM                    
105300            END-COMPUTE                                                   
105400            MOVE WS-KVLS-TO-AFTER  TO MOD-TO-KVLS-AFTER                   
105500                                                                          
105600                                                                          
105700            COMPUTE WS-KVLS-FROM-AFTER =                                  
105800                    WS-KVLS-FROM-BEFORE - WS-KVANTAL-NUM                  
105900            END-COMPUTE                                                   
106000            MOVE WS-KVLS-FROM-AFTER  TO MOD-FROM-KVLS-AFTER               
106100                                                                          
106200            IF WS-KVLS-FROM-AFTER >= ZERO                                 
106300               MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                
106400               MOVE MFS-NUM-FIELD-OK    TO MOD-KVANTAL-ATTR               
106500            ELSE                                                          
106600               MOVE FEL-507 (INDX) TO MOD-TEMFSFEL                        
106700               MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                
106800               MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR               
106900               MOVE NOO TO INDATA-SW                                      
107000            END-IF                                                        
107100                                                                          
107200         ELSE                                                             
107300            MOVE FEL-507 (INDX) TO  MOD-TEMFSFEL                          
107400            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                   
107500            MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR                  
107600            MOVE NOO TO INDATA-SW                                         
107700         END-IF                                                           
107800       ELSE                                                               
107900          MOVE FEL-509 (INDX)  TO   MOD-TEMFSFEL                          
108000          MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVANTAL                     
108100          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVANTAL-ATTR                    
108200          MOVE NOO TO INDATA-SW                                           
108300       END-IF                                                             
108400     END-IF                                                               
108500     .                                                                    
108600     EJECT                                                                
108700 GB-CHECK-MID-IDANALYS              SECTION.                              
108800                                                                          
108900***  NY SUBMODUL SKA ANVÄNDAS HÄR FÖR VALIDERING                          
109000**   AV ANALYSNR                                                          
109100     IF NOT DCS-NDC-NA                                                    
109200        IF DCS-CHINA                                                      
109300           MOVE 'CN05'            TO SAP-KDTRADP                          
109400        ELSE                                                              
109500           MOVE 'SEPV'            TO SAP-KDTRADP                          
109600       END-IF                                                             
109700       MOVE SPACE                 TO SAP-IDKST                            
109800       IF MID-IDKONTO   = ALL '+' OR ALL ' '                              
109900         MOVE ZERO                TO SAP-IDKONTO                          
110000       ELSE                                                               
110100         MOVE MID-IDKONTO         TO SAP-IDKONTO                          
110200       END-IF                                                             
110300       IF MID-IDANALYS  = ALL '+'                                         
110400         MOVE ALL '0'             TO SAP-IDANALYS                         
110500       ELSE                                                               
110600         MOVE MID-IDANALYS        TO SAP-IDANALYS                         
110700       END-IF                                                             
110800       MOVE ZERO                  TO SAP-IDDISTR                          
110900       MOVE SPACE                 TO SAP-KDFAKTYP                         
111000       MOVE ZERO                  TO SAP-IDFTG                            
111100       MOVE SPACE                 TO SAP-IDPROFIT                         
111200       MOVE +2                    TO SAP-KDCALL                           
111300                                                                          
111400       CALL W411SAP USING SAP-W411SAP SAPC-PCB                            
111500                                                                          
111600       IF SAP-BEFEL NOT = SPACE                                           
111700         MOVE SAP-BEFEL           TO MOD-TEMFSFEL                         
111800         MOVE NOO                 TO INDATA-SW                            
111900       END-IF                                                             
112000                                                                          
112100       IF SAP-IDFTG-OK = NOO                                              
112200         MOVE NOO                 TO INDATA-SW                            
112300       END-IF                                                             
112400                                                                          
112500       IF SAP-IDKST-OK = NOO                                              
112600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                    
112700         MOVE NOO                 TO INDATA-SW                            
112800       END-IF                                                             
112900                                                                          
113000       IF SAP-IDKONTO-OK = NOO                                            
113100         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                     
113200         MOVE NOO                 TO INDATA-SW                            
113300       END-IF                                                             
113400                                                                          
113500       IF SAP-IDANALYS-OK = NOO                                           
113600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                    
113700         MOVE NOO                 TO INDATA-SW                            
113800       END-IF                                                             
113900                                                                          
114000       IF INDATA-OK                                                       
114100         MOVE MID-IDANALYS           TO WS-IDANALYS                       
114200         MOVE WS-IDANALYS            TO MOD-IDANALYS                      
114300         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDANALYS                      
114400         MOVE MFS-NUM-FIELD-OK       TO MOD-IDANALYS-ATTR                 
114500       END-IF                                                             
114600     END-IF                                                               
114700     .                                                                    
114800     EJECT                                                                
114900 GC-CHECK-MID-IDAVINR             SECTION.                                
115000                                                                          
115100     IF MID-IDAVINR     = ALL '+'                                         
115200        MOVE FEL-505 (INDX)          TO MOD-TEMFSFEL                      
115300        MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDAVINR                       
115400        MOVE MFS-NUM-FIELD-WRONG     TO MOD-IDAVINR-ATTR                  
115500        MOVE NOO TO INDATA-SW                                             
115600     ELSE                                                                 
115700                                                                          
115800       IF MID-IDAVINR NUMERIC                                             
115900         IF MID-IDAVINR > ZERO                                            
116000           MOVE MID-IDAVINR     TO WS-IDAVINR                             
116100                                     MOD-IDAVINR                          
116200           INSPECT WS-IDAVINR REPLACING LEADING SPACE BY ZERO             
116300           INSPECT MOD-IDAVINR REPLACING LEADING ZERO BY SPACE            
116400                                                                          
116500           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR                     
116600           MOVE MFS-NUM-FIELD-OK TO MOD-IDAVINR-ATTR                      
116700         ELSE                                                             
116800           IF INDATA-OK                                                   
116900             MOVE FEL-509 (INDX) TO MOD-TEMFSFEL                          
117000             MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR                   
117100             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR                 
117200             MOVE NOO TO INDATA-SW                                        
117300           END-IF                                                         
117400         END-IF                                                           
117500       ELSE                                                               
117600                                                                          
117700          IF INDATA-OK                                                    
117800            MOVE FEL-506 (INDX) TO  MOD-TEMFSFEL                          
117900            MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR                    
118000            MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR                  
118100            MOVE NOO TO INDATA-SW                                         
118200          END-IF                                                          
118300       END-IF                                                             
118400     END-IF                                                               
118500                                                                          
118600     IF INDATA-OK                                                         
118700       IF MID-IDAVINR(1:2) = '57'                                         
118800          MOVE FEL-508 (INDX)  TO   MOD-TEMFSFEL                          
118900          MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDAVINR                     
119000          MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR                    
119100          MOVE NOO TO INDATA-SW                                           
119200       ELSE                                                               
119300         IF MID-IDAVINR(1:1) = ZERO AND                                   
119400            MID-IDAVINR(2:2) = '57'                                       
119500            MOVE FEL-508 (INDX) TO  MOD-TEMFSFEL                          
119600            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDAVINR                   
119700            MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR                  
119800            MOVE NOO TO INDATA-SW                                         
119900         ELSE                                                             
120000           IF MID-IDAVINR(1:2) = ZERO AND                                 
120100              MID-IDAVINR(3:2) = '57'                                     
120200              MOVE FEL-508 (INDX) TO MOD-TEMFSFEL                         
120300              MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR                  
120400              MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR                
120500              MOVE NOO TO INDATA-SW                                       
120600           ELSE                                                           
120700             IF MID-IDAVINR(1:3) = ZERO AND                               
120800                MID-IDAVINR(4:2) = '57'                                   
120900                MOVE FEL-508 (INDX) TO MOD-TEMFSFEL                       
121000                MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR                
121100                MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR              
121200                MOVE NOO TO INDATA-SW                                     
121300             ELSE                                                         
121400               IF MID-IDAVINR(1:4) = ZERO AND                             
121500                  MID-IDAVINR(5:2) = '57'                                 
121600                  MOVE FEL-508 (INDX) TO MOD-TEMFSFEL                     
121700                  MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDAVINR              
121800                  MOVE MFS-NUM-FIELD-WRONG TO MOD-IDAVINR-ATTR            
121900                  MOVE NOO TO INDATA-SW                                   
122000               END-IF                                                     
122100             END-IF                                                       
122200           END-IF                                                         
122300         END-IF                                                           
122400       END-IF                                                             
122500     END-IF                                                               
122600                                                                          
122700     .                                                                    
122800     EJECT                                                                
122900 GE-CHECK-MID-IDKONTO               SECTION.                              
123000                                                                          
123100**** NY SUBMODUL SKA ANVÄNDAS HÄR FÖR VALIDERING                          
123200**** AV IDKONTO                                                           
123300     IF NOT DCS-NDC-NA                                                    
123400       IF DCS-CHINA                                                       
123500          MOVE 'CN05'          TO SAP-KDTRADP                             
123600       ELSE                                                               
123700          MOVE 'SEPV'          TO SAP-KDTRADP                             
123800       END-IF                                                             
123900       MOVE SPACE              TO SAP-IDKST                               
124000       IF MID-IDKONTO   = ALL '+' OR ALL ' '                              
124100         MOVE ZERO             TO SAP-IDKONTO                             
124200       ELSE                                                               
124300         MOVE MID-IDKONTO      TO SAP-IDKONTO                             
124400       END-IF                                                             
124500       IF MID-IDANALYS  = ALL '+'                                         
124600         MOVE ALL '0'          TO SAP-IDANALYS                            
124700       ELSE                                                               
124800         MOVE MID-IDANALYS     TO SAP-IDANALYS                            
124900       END-IF                                                             
125000       MOVE ZERO               TO SAP-IDDISTR                             
125100       MOVE SPACE              TO SAP-KDFAKTYP                            
125200       MOVE ZERO               TO SAP-IDFTG                               
125300       MOVE SPACE              TO SAP-IDPROFIT                            
125400       MOVE +2                 TO SAP-KDCALL                              
125500       CALL W411SAP USING SAP-W411SAP SAPC-PCB                            
125600                                                                          
125700       IF SAP-IDFTG-OK = NOO                                              
125800         MOVE NOO               TO INDATA-SW                              
125900       END-IF                                                             
126000                                                                          
126100       IF SAP-IDKST-OK = NOO                                              
126200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-ATTR                      
126300         MOVE NOO               TO INDATA-SW                              
126400       END-IF                                                             
126500                                                                          
126600       IF SAP-IDKONTO-OK = NOO                                            
126700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                       
126800         MOVE NOO               TO INDATA-SW                              
126900       END-IF                                                             
127000                                                                          
127100       IF SAP-IDANALYS-OK = NOO                                           
127200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-ATTR                      
127300         MOVE NOO               TO INDATA-SW                              
127400       END-IF                                                             
127500                                                                          
127600       IF INDATA-OK                                                       
127700         MOVE MID-IDKONTO            TO WS-IDKONTO                        
127800         MOVE WS-IDKONTO             TO MOD-IDKONTO                       
127900         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKONTO                       
128000         MOVE MFS-NUM-FIELD-OK       TO MOD-IDANALYS-ATTR                 
128100       END-IF                                                             
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 H-UPDATE                          SECTION.                               
128700                                                                          
128800     PERFORM HA-UPDATE-ARTREG-KVLS                                        
128900     MOVE DCS-IDLEVNR-DC TO WS-IDLEVNR                                    
129000                                                                          
129100     IF DCS-NDC-NA                                                        
129200       PERFORM HH-SET-IDFTG-KDVALISO                                      
129300       PERFORM HI-EKOA19-R32-FROM-PART                                    
129400       PERFORM HJ-EKOA19-R32-TO-PART                                      
129500     ELSE                                                                 
129600       IF DCS-CHINA                                                       
129700         PERFORM HH-SET-IDFTG-KDVALISO                                    
129800       END-IF                                                             
129900       PERFORM HB-EKOAVV-R32-FROM-PART                                    
130000       PERFORM HC-EKOAVV-R32-TO-PART                                      
130100     END-IF                                                               
130200                                                                          
130300     IF DCS-CDC                                                           
130400       PERFORM HD-INLEV-CDC-R32-FROM-PART                                 
130500       PERFORM HE-INLEV-CDC-R32-TO-PART                                   
130600     ELSE                                                                 
130700       PERFORM HF-INLEV-NDC-SDC-R32-FROM-PART                             
130800       PERFORM HG-INLEV-NDC-SDC-R32-TO-PART                               
130900     END-IF                                                               
131000                                                                          
131100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
131200     CALL WMEDKONV USING MED-WMEDAREA                                     
131300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
131400     PERFORM MFS-FORM-ATTR                                                
131500     PERFORM MFS-ERASE-FIELD-IN                                           
131600     .                                                                    
131700     EJECT                                                                
131800 HA-UPDATE-ARTREG-KVLS             SECTION.                               
131900* UPPDATERAR SALDOLOGG + WDK6 ELLER WDK7                                  
132000                                                                          
132100     IF DCS-CDC                                                           
132200                                                                          
132300       MOVE WS-IDARTNR-FROM    TO W-IDARTNR-FROM                          
132400                                  W-IDARTNR                               
132500       PERFORM IMS-GHU-ARTC-FROM                                          
132600       MOVE WS-KVLS-FROM-AFTER TO FROM-CLAG-KVLS                          
132700                                  LOGG-KVLS                               
132800       PERFORM IMS-REPL-ARTC-FROM                                         
132900                                                                          
133000       MOVE '-'                TO LOGG-IDTECKEN-KVLS                      
133100       MOVE FROM-CLAG-KVAKS-CDC     TO LOGG-KVAKS                         
133200       MOVE FROM-CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                     
133300       MOVE FROM-CLAG-KVEFRS        TO LOGG-KVEFRS                        
133400       COMPUTE LOGG-KVAKS = FROM-CLAG-KVAKS-CDC +                         
133500                            FROM-CLAG-KVAKS-T                             
133600       MOVE SPACE               TO LOGG-REF                               
133700       MOVE WS-IDARTNR-TO       TO LOGG-IDLOPNRM                          
133800       IF WS-IDAVINR NUMERIC                                              
133900         MOVE WS-IDAVINR        TO LOGG-IDAVINR                           
134000       END-IF                                                             
134100       PERFORM HAA-FLYTTA-LOGG-WDK6                                       
134200       PERFORM HAC-UPPDATERA-LOGG                                         
134300                                                                          
134400       MOVE WS-IDARTNR-TO      TO W-IDARTNR-TO                            
134500                                  W-IDARTNR                               
134600                                  W-IDARTNR-WDA9                          
134700                                  LOGG-IDARTNR                            
134800       PERFORM IMS-GHU-ARTC-TO                                            
134900       MOVE WS-KVLS-TO-AFTER   TO TO-CLAG-KVLS                            
135000                                  LOGG-KVLS                               
135100       PERFORM IMS-REPL-ARTC-TO                                           
135200                                                                          
135300       MOVE '+'                TO LOGG-IDTECKEN-KVLS                      
135400       MOVE TO-CLAG-KVAKS-CDC     TO LOGG-KVAKS                           
135500       MOVE TO-CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
135600       MOVE TO-CLAG-KVEFRS        TO LOGG-KVEFRS                          
135700       COMPUTE LOGG-KVAKS = TO-CLAG-KVAKS-CDC +                           
135800                            TO-CLAG-KVAKS-T                               
135900       MOVE SPACE               TO LOGG-REF                               
136000       MOVE WS-IDARTNR-FROM     TO LOGG-IDLOPNRM                          
136100       IF WS-IDAVINR NUMERIC                                              
136200         MOVE WS-IDAVINR        TO LOGG-IDAVINR                           
136300       END-IF                                                             
136400       PERFORM HAA-FLYTTA-LOGG-WDK6                                       
136500       PERFORM HAC-UPPDATERA-LOGG                                         
136600       PERFORM HAD-UPPDATERA-WDA9                                         
136700     ELSE                                                                 
136800       MOVE WS-IDARTNR-FROM    TO W-IDARTNR-FROM                          
136900                                  W-IDARTNR                               
137000       PERFORM IMS-GHU-ARTS-FROM                                          
137100       MOVE WS-KVLS-FROM-AFTER TO FROM-SLAG-KVLS                          
137200                                  LOGG-KVLS                               
137300       PERFORM IMS-REPL-ARTS-FROM                                         
137400       MOVE '-'                TO LOGG-IDTECKEN-KVLS                      
137500       MOVE FROM-SLAG-KVAKS-SDC     TO LOGG-KVAKS                         
137600       MOVE FROM-SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                     
137700       MOVE FROM-SLAG-KVEFRS        TO LOGG-KVEFRS                        
137800       MOVE SPACE               TO LOGG-REF                               
137900       MOVE WS-IDARTNR-TO       TO LOGG-IDLOPNRM                          
138000       IF WS-IDAVINR NUMERIC                                              
138100         MOVE WS-IDAVINR        TO LOGG-IDAVINR                           
138200       END-IF                                                             
138300       PERFORM HAB-FLYTTA-LOGG-WDK7                                       
138400       PERFORM HAC-UPPDATERA-LOGG                                         
138500                                                                          
138600       MOVE WS-IDARTNR-TO      TO W-IDARTNR-TO                            
138700                                  W-IDARTNR                               
138800                                  W-IDARTNR-WDA9                          
138900                                  LOGG-IDARTNR                            
139000       PERFORM IMS-GHU-ARTS-TO                                            
139100       MOVE WS-KVLS-TO-AFTER   TO TO-SLAG-KVLS                            
139200                                  LOGG-KVLS                               
139300       PERFORM IMS-REPL-ARTS-TO                                           
139400                                                                          
139500       MOVE '+'                TO LOGG-IDTECKEN-KVLS                      
139600       MOVE TO-SLAG-KVAKS-SDC     TO LOGG-KVAKS                           
139700       MOVE TO-SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
139800       MOVE TO-SLAG-KVEFRS        TO LOGG-KVEFRS                          
139900       MOVE SPACE               TO LOGG-REF                               
140000       MOVE WS-IDARTNR-FROM     TO LOGG-IDLOPNRM                          
140100       IF WS-IDAVINR NUMERIC                                              
140200         MOVE WS-IDAVINR        TO LOGG-IDAVINR                           
140300       END-IF                                                             
140400       PERFORM HAB-FLYTTA-LOGG-WDK7                                       
140500       PERFORM HAC-UPPDATERA-LOGG                                         
140600       PERFORM HAD-UPPDATERA-WDA9                                         
140700     END-IF                                                               
140800     .                                                                    
140900     SKIP2                                                                
141000 HAA-FLYTTA-LOGG-WDK6 SECTION.                                            
141100* LÄGGER UPP SALDOLOGG I WDL9                                             
141200     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
141300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
141400     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
141500     ACCEPT TRANS-TID FROM TIME                                           
141600     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
141700     MOVE 9                       TO LOGG-IDSEKVNR                        
141800     MOVE W-IDDC                  TO LOGG-IDDC                            
141900     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
142000     MOVE 'R32'                   TO LOGG-IDSUBTYP                        
142100     MOVE IDPGM                   TO LOGG-IDPGM                           
142200     MOVE '5151'                  TO LOGG-IDTRANS                         
142300     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
142400     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
142500     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
142600     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
142700     MOVE WS-KVANTAL-NUM          TO LOGG-KVART-SALDO                     
142800     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
142900     .                                                                    
143000     EJECT                                                                
143100 HAB-FLYTTA-LOGG-WDK7 SECTION.                                            
143200* LÄGGER UPP SALDOLOGG I WDL9                                             
143300     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
143400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
143500     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
143600     ACCEPT TRANS-TID FROM TIME                                           
143700     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
143800     MOVE 9                       TO LOGG-IDSEKVNR                        
143900     MOVE W-IDDC                  TO LOGG-IDDC                            
144000     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
144100     MOVE 'R32'                   TO LOGG-IDSUBTYP                        
144200     MOVE IDPGM                   TO LOGG-IDPGM                           
144300     MOVE '5151'                  TO LOGG-IDTRANS                         
144400     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
144500     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
144600     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
144700     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
144800     MOVE WS-KVANTAL-NUM          TO LOGG-KVART-SALDO                     
144900     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
145000     .                                                                    
145100     EJECT                                                                
145200 HAC-UPPDATERA-LOGG SECTION.                                              
145300     PERFORM IMS-ISRT-WDL901                                              
145400     IF SEGMENT-FOUND-EXISTS                                              
145500        PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                            
145600          ADD -1 TO LOGG-IDSEKVNR                                         
145700          PERFORM IMS-ISRT-WDL901                                         
145800        END-PERFORM                                                       
145900     END-IF                                                               
146000     .                                                                    
146100     EJECT                                                                
146200 HAD-UPPDATERA-WDA9 SECTION.                                              
146300     PERFORM IMS-GHU-WDA901                                               
146400     IF SEGMENT-MISSING                                                   
146500       MOVE W-IDARTNR-WDA9              TO UPB-IDARTNR                    
146600       MOVE TO-ART-IDFKNGRP             TO UPB-IDFKNGRP                   
146700       PERFORM IMS-ISRT-WDA901                                            
146800     END-IF                                                               
146900                                                                          
147000     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
147100     MOVE FUNCTION CURRENT-DATE (3:6)   TO DAT-I-TIDATUM                  
147200                                                                          
147300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
147400                         DAT-O-TIDATUM DAT-KDSVAR                         
147500                                                                          
147600     IF DAT-KDSVAR-OK                                                     
147700       MOVE 200000                      TO WS-DAAAPP                      
147800       ADD DAT-TIAAPP                   TO WS-DAAAPP                      
147900       MOVE WS-DAAAPP                   TO W-DAAAPP-WDA9                  
148000     ELSE                                                                 
148100       DISPLAY 'FELAKTIGT DATUM'                                          
148200       CALL FELLOG                                                        
148300     END-IF                                                               
148400                                                                          
148500     PERFORM IMS-GHU-WDA912                                               
148600     IF SEGMENT-MISSING                                                   
148700       PERFORM IMS-GHU-WDA901                                             
148800       MOVE WS-DAAAPP                   TO UPA-DAAAPP                     
148900       MOVE WS-KVANTAL-NUM              TO UPA-SUINVEST-DC                
149000       MOVE ZERO                        TO UPA-SULEVANT-DC                
149100                                           UPA-SUMOTT-CP                  
149200                                           UPA-SUSKROT-DC                 
149300       PERFORM IMS-ISRT-WDA912                                            
149400     ELSE                                                                 
149500       ADD WS-KVANTAL-NUM               TO UPA-SUINVEST-DC                
149600       PERFORM IMS-REPL-WDA912                                            
149700     END-IF                                                               
149800     .                                                                    
149900     EJECT                                                                
150000                                                                          
150100 HB-EKOAVV-R32-FROM-PART   SECTION.                                       
150200                                                                          
150300     COMPUTE EKH-KVANTAL = WS-KVANTAL-NUM * -1                            
150400     MOVE '402'                TO EKH-KDEKSHT                             
150500     MOVE W-IDARTNR-FROM       TO EKH-IDARTNR                             
150600     MOVE W-IDARTNR-FROM       TO W-EKH-IDARTNR                           
150700                                                                          
150800     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
150900     MOVE W-IDARTNR-FROM   TO CIA-IDARTBET-IN                             
151000     CALL W009CIA USING       CIA-W009CIA                                 
151100     MOVE CIA-IDARTBET-UT  TO EKH-IDVERGL                                 
151200                                                                          
151300     MOVE FROM-CLAG-PRARTSTD   TO EKH-PRARTSTD                            
151400     MOVE WS-FROM-ART-KDPRODSL TO EKH-KDPRODSL                            
151500     PERFORM HBA-UPPDATERA-WDR9                                           
151600     .                                                                    
151700     SKIP2                                                                
151800 HBA-UPPDATERA-WDR9 SECTION.                                              
151900     MOVE 'W5015100'       TO FIL-IDPGM IN FIL-WDR901                     
152000     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
152100     MOVE DAGENS-DATUM     TO FIL-DAREGDAT                                
152200     ACCEPT    FIL-TIKLOCK IN FIL-WDR901 FROM TIME                        
152300     MOVE 1                TO FIL-IDSEKVNR IN FIL-WDR901                  
152400     MOVE 'W510EKHA'       TO FIL-IDCPYTXT IN FIL-WDR901                  
152500     MOVE MSG-SIGNON-USERID TO FIL-IDUSER                                 
152600     MOVE '405'            TO EKH-KDEKHHT                                 
152700     MOVE 'DET'            TO EKH-KDEKNIVA                                
152800     MOVE W-IDDC           TO EKH-IDDC-SEND                               
152900     MOVE W-IDDC           TO EKH-IDDC-REC                                
153000     MOVE +0               TO EKH-IDDISTR                                 
153100     MOVE +0               TO EKH-IDKUNDNR                                
153200     MOVE DAGENS-DATUM     TO EKH-DAVERDAT                                
153300     MOVE ZERO             TO EKH-KDPSLLOC                                
153400     MOVE SPACE            TO EKH-FLLSBOK                                 
153500     MOVE 'SEK'            TO EKH-KDVALISO                                
153600     MOVE 1.00             TO EKH-PRKURS                                  
153700     MOVE ZERO             TO EKH-PRARTNTO                                
153800     MOVE ZERO             TO EKH-PRARTSJK                                
153900     MOVE ZERO             TO EKH-PRHEMTAG                                
154000     MOVE ZERO             TO EKH-PRLANDCO                                
154100     MOVE ZERO             TO EKH-PRINK                                   
154200     MOVE ZERO             TO EKH-PRDIRLON                                
154300     MOVE ZERO             TO EKH-PRDMTRL                                 
154400     MOVE ZERO             TO EKH-PROVRPAL                                
154500     MOVE ZERO             TO EKH-SUBEL                                   
154600     MOVE '5151'           TO EKH-IDTRANS                                 
154700     MOVE WS-IDKONTO       TO EKH-IDKONTO                                 
154800                                                                          
154900     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
155000     MOVE WS-IDANALYS       TO CIA-IDARTBET-IN                            
155100     CALL W009CIA USING        CIA-W009CIA                                
155200     MOVE CIA-IDARTBET-UT   TO EKH-IDANALYS                               
155300                                                                          
155400     MOVE ZERO                   TO EKH-BEVAT                             
155500                                    EKH-KDANMORS                          
155600                                    EKH-KDFRAKT                           
155700                                    EKH-SUVAT                             
155800     MOVE ZERO                   TO EKH-DAAVIDAT                          
155900                                    EKH-IDAVINR                           
156000                                    EKH-KDAVVTYP                          
156100                                    EKH-KDRT                              
156200                                    EKH-KVANTMOT                          
156300                                    EKH-KVAVIS                            
156400     MOVE SPACE                  TO EKH-KDSORT                            
156500                                    EKH-IDLEVNR                           
156600     MOVE SPACE                  TO EKH-KDTRADP                           
156700                                    EKH-IDKST                             
156800     MOVE SPACE                  TO EKH-FLDCET                            
156900     MOVE SPACE                  TO EKH-IDKUNDRF                          
157000     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
157100                                                                          
157200     PERFORM IMS-ISRT-WDR901                                              
157300     PERFORM UNTIL SEGMENT-FOUND                                          
157400       ADD +1  TO FIL-IDSEKVNR IN FIL-WDR901                              
157500       PERFORM IMS-ISRT-WDR901                                            
157600     END-PERFORM                                                          
157700     .                                                                    
157800     EJECT                                                                
157900                                                                          
158000 HC-EKOAVV-R32-TO-PART     SECTION.                                       
158100                                                                          
158200     MOVE '403'                TO EKH-KDEKSHT                             
158300     MOVE WS-KVANTAL-NUM       TO EKH-KVANTAL                             
158400     MOVE W-IDARTNR-TO         TO EKH-IDARTNR                             
158500     MOVE W-IDARTNR-TO         TO W-EKH-IDARTNR                           
158600                                                                          
158700     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
158800     MOVE W-IDARTNR-TO         TO CIA-IDARTBET-IN                         
158900     CALL W009CIA USING           CIA-W009CIA                             
159000     MOVE CIA-IDARTBET-UT      TO EKH-IDVERGL                             
159100                                                                          
159200     MOVE TO-CLAG-PRARTSTD     TO EKH-PRARTSTD                            
159300     MOVE WS-TO-ART-KDPRODSL   TO EKH-KDPRODSL                            
159400     PERFORM HBA-UPPDATERA-WDR9                                           
159500     .                                                                    
159600     SKIP2                                                                
159700 HD-INLEV-CDC-R32-FROM-PART        SECTION.                               
159800                                                                          
159900*INCOMING GOODS HISTORIC TO CDC R32 REPORT.                               
160000*    -- SKAPA IDINLEV                                                     
160100     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
160200     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
160300     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
160400     COMPUTE WS-DAINLEV          = 9999999999999999                       
160500                                 - WS-TIAAAAMMDDTTMMSSTH                  
160600     END-COMPUTE                                                          
160700     MOVE WS-DAINLEV           TO W-DAINLEV                               
160800     MOVE W-IDARTNR-FROM       TO W-IDARTNR                               
160900     PERFORM IMS-GHU-INLE01                                               
161000                                                                          
161100     IF  SEGMENT-MISSING                                                  
161200       MOVE W-IDARTNR-FROM     TO ART-IDARTNR                             
161300                               IN ART-WDL201                              
161400       PERFORM IMS-ISRT-INLE01                                            
161500     END-IF                                                               
161600                                                                          
161700     MOVE WS-DAINLEV           TO WDL211-INL-DAINLEV                      
161800     PERFORM IMS-ISRT-INLE11                                              
161900                                                                          
162000     MOVE 'R32'                TO MOT-IDPTYP                              
162100*W-IDARTNR-TO IS THE PART NUMBER TO-BE (TILLKOMMANDE).                    
162200     MOVE W-IDARTNR-TO         TO MOT-IDLOPNRM                            
162300     MOVE WS-IDAVINR           TO MOT-IDAVINR                             
162400     MOVE WS-IDKONTO           TO MOT-IDKONTO                             
162500     MOVE WS-IDANALYS          TO MOT-IDANALYS                            
162600     MOVE SPACE                TO MOT-IDFS                                
162700                                  MOT-IDLEVNR                             
162800                                  MOT-IDKST                               
162900     MOVE +0                   TO MOT-ADLAGOMR                            
163000                                  MOT-ADPLATS                             
163100                                  MOT-ADGANG                              
163200                                  MOT-KDAVVANT                            
163300                                  MOT-KDAVVKV                             
163400                                  MOT-KVAVIS                              
163500                                  MOT-KVFORDEL                            
163600                                  MOT-KVRETUR                             
163700                                  MOT-KVFORV                              
163800                                  MOT-IDSHIPM                             
163900     MOVE WC-CDC-SE            TO MOT-IDDC                                
164000     MOVE +006                 TO MOT-KDRT                                
164100     MOVE WS-KVANTAL-NUM       TO MOT-KVANTMOT                            
164200     COMPUTE MOT-KVANTMOT  =  MOT-KVANTMOT * -1                           
164300     END-COMPUTE                                                          
164400                                                                          
164500     MOVE WS-TODAYS-DATE       TO MOT-TIUPPDAT                            
164600                                  MOT-TIAVIDAT                            
164700                                                                          
164800     PERFORM IMS-ISRT-INLE21                                              
164900     .                                                                    
165000     SKIP2                                                                
165100 HE-INLEV-CDC-R32-TO-PART          SECTION.                               
165200                                                                          
165300*INCOMING GOODS HISTORIC TO CDC R32 REPORT.                               
165400*    -- SKAPA IDINLEV                                                     
165500     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
165600     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
165700     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
165800     COMPUTE WS-DAINLEV          = 9999999999999999                       
165900                                 - WS-TIAAAAMMDDTTMMSSTH                  
166000     END-COMPUTE                                                          
166100     MOVE WS-DAINLEV           TO W-DAINLEV                               
166200     MOVE W-IDARTNR-TO         TO W-IDARTNR                               
166300     PERFORM IMS-GHU-INLE01                                               
166400                                                                          
166500     IF  SEGMENT-MISSING                                                  
166600       MOVE W-IDARTNR-TO       TO ART-IDARTNR                             
166700                               IN ART-WDL201                              
166800       PERFORM IMS-ISRT-INLE01                                            
166900     END-IF                                                               
167000                                                                          
167100     MOVE WS-DAINLEV           TO WDL211-INL-DAINLEV                      
167200     PERFORM IMS-ISRT-INLE11                                              
167300                                                                          
167400     MOVE 'R32'                TO MOT-IDPTYP                              
167500*FROM PART IS THE ORIGIN PART (URSPRUNGLIG).                              
167600     MOVE W-IDARTNR-FROM       TO MOT-IDLOPNRM                            
167700     MOVE WS-IDAVINR           TO MOT-IDAVINR                             
167800     MOVE WS-IDKONTO           TO MOT-IDKONTO                             
167900     MOVE WS-IDANALYS          TO MOT-IDANALYS                            
168000     MOVE SPACE                TO MOT-IDFS                                
168100                                  MOT-IDLEVNR                             
168200                                  MOT-IDKST                               
168300     MOVE +0                   TO MOT-ADLAGOMR                            
168400                                  MOT-ADPLATS                             
168500                                  MOT-ADGANG                              
168600                                  MOT-KDAVVANT                            
168700                                  MOT-KDAVVKV                             
168800                                  MOT-KVAVIS                              
168900                                  MOT-KVFORDEL                            
169000                                  MOT-KVRETUR                             
169100                                  MOT-IDSHIPM                             
169200     MOVE WC-CDC-SE            TO MOT-IDDC                                
169300     MOVE +006                 TO MOT-KDRT                                
169400     MOVE WS-KVANTAL-NUM       TO MOT-KVANTMOT                            
169500                                                                          
169600     MOVE WS-TODAYS-DATE       TO MOT-TIUPPDAT                            
169700                                  MOT-TIAVIDAT                            
169800     PERFORM IMS-ISRT-INLE21                                              
169900     .                                                                    
170000     SKIP2                                                                
170100 HF-INLEV-NDC-SDC-R32-FROM-PART         SECTION.                          
170200                                                                          
170300*INCOMING GOODS HISTORIC TO CDC R32 REPORT.                               
170400                                                                          
170500     MOVE W-IDARTNR-FROM          TO W-IDARTNR                            
170600     PERFORM IMS-GHU-INLC01                                               
170700     IF SEGMENT-MISSING                                                   
170800       MOVE W-IDARTNR-FROM        TO ART-IDARTNR                          
170900                                  IN ART-WDL601                           
171000       PERFORM IMS-ISRT-INLC01                                            
171100     END-IF                                                               
171200                                                                          
171300*    -- SKAPA IDINLEV                                                     
171400     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
171500     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
171600     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
171700     COMPUTE WS-DAINLEV          = 9999999999999999                       
171800                                 - WS-TIAAAAMMDDTTMMSSTH                  
171900     END-COMPUTE                                                          
172000                                                                          
172100     MOVE WS-DAINLEV              TO INL-DAINLEV                          
172200     MOVE ZERO                    TO INL-ADLAGOMR                         
172300                                     INL-ADGANG                           
172400                                     INL-ADPLATS                          
172500                                     INL-IDGMTREF                         
172600                                     INL-IDKOLLI                          
172700                                     INL-KVAVIS                           
172800                                     INL-IDFAKT                           
172900                                     INL-KDFRAKT                          
173000                                     INL-IDLOPNRM                         
173100                                     INL-IDDISTR                          
173200                                     INL-IDKUNDNR                         
173300                                     INL-PRKURS                           
173400                                     INL-TIBERANK                         
173500                                     INL-TIINLMTI                         
173600                                     INL-TIINLITI                         
173700                                     INL-KVART-SKROT                      
173800     MOVE W-IDDC                  TO INL-IDDC                             
173900                                                                          
174000     MOVE WS-IDLEVNR              TO INL-IDLEVNR                          
174100     MOVE WS-KDVALISO             TO INL-KDVALISO                         
174200     MOVE WS-FROM-SLAG-PRAVCOST   TO INL-PRARTNTO                         
174300                                                                          
174400     MOVE '30'                    TO INL-KDRT                             
174500     MOVE 'R32'                   TO INL-IDPTYP                           
174600     MOVE WS-KVANTAL-NUM          TO INL-KVANTMOT                         
174700     COMPUTE INL-KVANTMOT  =  INL-KVANTMOT * -1                           
174800     END-COMPUTE                                                          
174900     MOVE WS-TODAYS-DATE-LOCAL    TO INL-TIINLMOT                         
175000                                     INL-TIINLINL                         
175100     MOVE SPACE                   TO INL-KDKOLLI                          
175200                                     INL-IDKUNDRF                         
175300                                     INL-IDANALYS                         
175400                                     INL-IDKST                            
175500                                     INL-IDUSER-003                       
175600                                     INL-IDDC-LEV                         
175700     MOVE ZERO                    TO INL-IDKONTO                          
176000                                     INL-KVTULRET                         
176010                                     INL-KVRETUR                          
176100                                     INL-KDAVVANT                         
176200                                     INL-TIAVIDAT                         
176300     MOVE NOO                     TO INL-FLPRIO                           
176400                                     INL-FLMAKUL                          
176500                                     INL-FLSKAKOL                         
176510                                     INL-FLTULLST                         
176600                                                                          
176700     PERFORM IMS-ISRT-INLC11                                              
176800     .                                                                    
176900     SKIP2                                                                
177000 HG-INLEV-NDC-SDC-R32-TO-PART           SECTION.                          
177100                                                                          
177200*INCOMING GOODS HISTORIC TO CDC R32 REPORT.                               
177300     MOVE W-IDARTNR-TO            TO W-IDARTNR                            
177400     PERFORM IMS-GHU-INLC01                                               
177500     IF SEGMENT-MISSING                                                   
177600       MOVE W-IDARTNR-TO          TO ART-IDARTNR                          
177700                                  IN ART-WDL601                           
177800       PERFORM IMS-ISRT-INLC01                                            
177900     END-IF                                                               
178000                                                                          
178100*    -- SKAPA DAINLEV                                                     
178200     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
178300     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
178400     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
178500     COMPUTE WS-DAINLEV          = 9999999999999999                       
178600                                 - WS-TIAAAAMMDDTTMMSSTH                  
178700     END-COMPUTE                                                          
178800                                                                          
178900     MOVE WS-DAINLEV              TO INL-DAINLEV                          
179000     MOVE ZERO                    TO INL-ADLAGOMR                         
179100                                     INL-ADGANG                           
179200                                     INL-ADPLATS                          
179300                                     INL-IDGMTREF                         
179400                                     INL-IDKOLLI                          
179500                                     INL-KVAVIS                           
179600                                     INL-IDFAKT                           
179700                                     INL-KDFRAKT                          
179800                                     INL-IDLOPNRM                         
179900                                     INL-IDDISTR                          
180000                                     INL-IDKUNDNR                         
180100                                     INL-PRKURS                           
180200                                     INL-TIBERANK                         
180300                                     INL-KVART-SKROT                      
180400     MOVE W-IDDC                  TO INL-IDDC                             
180500                                                                          
180600     MOVE WS-IDLEVNR              TO INL-IDLEVNR                          
180700     MOVE WS-KDVALISO             TO INL-KDVALISO                         
180800     MOVE WS-TO-SLAG-PRAVCOST     TO INL-PRARTNTO                         
180900     MOVE '30'                    TO INL-KDRT                             
181000     MOVE 'R32'                   TO INL-IDPTYP                           
181100                                                                          
181200     MOVE WS-KVANTAL-NUM          TO INL-KVANTMOT                         
181300                                                                          
181400     MOVE WS-TODAYS-DATE-LOCAL    TO INL-TIINLMOT                         
181500                                     INL-TIINLINL                         
181600     MOVE SPACE                   TO INL-KDKOLLI                          
181700                                     INL-IDKUNDRF                         
181800                                     INL-IDANALYS                         
181900                                     INL-IDKST                            
182000                                     INL-IDUSER-003                       
182100                                     INL-IDDC-LEV                         
182200     MOVE ZERO                    TO INL-IDKONTO                          
182500                                     INL-KVTULRET                         
182510                                     INL-KVRETUR                          
182600                                     INL-KDAVVANT                         
182700                                     INL-TIAVIDAT                         
182800     MOVE NOO                     TO INL-FLPRIO                           
182900                                     INL-FLMAKUL                          
183000                                     INL-FLSKAKOL                         
183010                                     INL-FLTULLST                         
183100                                                                          
183200     PERFORM IMS-ISRT-INLC11                                              
183300     .                                                                    
183400     SKIP2                                                                
183500 HH-SET-IDFTG-KDVALISO              SECTION.                              
183600                                                                          
183700     EVALUATE TRUE                                                        
183800       WHEN DCS-NDC-NA AND DCS-USA                                        
183900          MOVE '53'            TO WS-IDFTG                                
184000       WHEN DCS-NDC-NA AND DCS-CANADA                                     
184100          MOVE '54'            TO WS-IDFTG                                
184200       WHEN DCS-CHINA                                                     
184300          MOVE '60'            TO WS-IDFTG                                
184400     END-EVALUATE                                                         
184500                                                                          
184600     EVALUATE TRUE                                                        
184700       WHEN DCS-NDC-NA AND DCS-USA                                        
184800          MOVE 'USD'           TO WS-KDVALISO                             
184900       WHEN DCS-NDC-NA AND DCS-CANADA                                     
185000          MOVE 'CAD'           TO WS-KDVALISO                             
185100       WHEN DCS-CHINA                                                     
185200          MOVE 'CNY'           TO WS-KDVALISO                             
185300       WHEN OTHER                                                         
185400          MOVE 'SEK'           TO WS-KDVALISO                             
185500     END-EVALUATE                                                         
185600     .                                                                    
185700     SKIP2                                                                
185800 HI-EKOA19-R32-FROM-PART   SECTION.                                       
185900                                                                          
186000*WDR801 TRANSACTION TO BATCH WITH EKONOMIC SUPER TRANS W510A19.           
186100     MOVE W-IDDC               TO EKOA19-IDDC-SEND                        
186200                                  EKOA19-IDDC-REC                         
186300     MOVE 'A19'                TO EKOA19-IDPTYP                           
186400     MOVE 'M30'                TO EKOA19-KDEKOHT                          
186500*IDPTYP HAR VÄRDET 'A19' PGA EKO-SYST. KRAV. IDPTYP SKALL VARA            
186600*R32 SOM ETT KORREKT VÄRDE.                                               
186700     MOVE W-IDARTNR-FROM       TO EKOA19-IDARTNR                          
186800                                                                          
186900     MOVE WS-KVANTAL-NUM       TO EKOA19-KVJUSTKV                         
187000     COMPUTE EKOA19-KVJUSTKV = EKOA19-KVJUSTKV * -1                       
187100     END-COMPUTE                                                          
187200                                                                          
187300     MOVE WS-FROM-ART-KDPRODSL  TO EKOA19-KDPRODSL                        
187400     MOVE WS-FROM-CLAG-KDPSLLOC TO EKOA19-KDPSLLOC                        
187500     MOVE WS-IDAVINR            TO EKOA19-IDFS                            
187600     MOVE WS-IDFTG              TO EKOA19-IDFTG                           
187700     MOVE FUNCTION CURRENT-DATE (1:8)  TO EKOA19-DAREGDAT                 
187800     MOVE WS-TODAYS-DATE-LOCAL  TO EKOA19-DAREGDAT(3:6)                   
187900     MOVE WS-FROM-SLAG-PRAVCOST TO EKOA19-PRAVCOST                        
188000                                                                          
188100     MOVE IDPGM                TO FIL-IDPGM IN FIL-WDR801                 
188200     MOVE WS-TODAYS-DATE       TO FIL-TIREGDAT                            
188300     ADD +1                    TO WS-TIMECLOCK                            
188400     MOVE WS-TIMECLOCK         TO FIL-TIKLOCK IN FIL-WDR801               
188500                                                                          
188600     MOVE +1                   TO FIL-IDSEKVNR IN FIL-WDR801              
188700     MOVE 'W510A19 '           TO FIL-IDCPYTXT IN FIL-WDR801              
188800     MOVE EKOA19-W510A19       TO FIL-WDR801-DATA                         
188900                                                                          
189000     PERFORM IMS-ISRT-FILB01                                              
189100                                                                          
189200     PERFORM UNTIL SEGMENT-FOUND                                          
189300         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
189400         PERFORM IMS-ISRT-FILB01                                          
189500     END-PERFORM                                                          
189600                                                                          
189700     .                                                                    
189800     SKIP2                                                                
189900 HJ-EKOA19-R32-TO-PART     SECTION.                                       
190000                                                                          
190100*WDR801 TRANSACTION TO BATCH FOR LAB USA AND CANADA                       
190200     MOVE W-IDDC               TO EKOA19-IDDC-SEND                        
190300                                  EKOA19-IDDC-REC                         
190400     MOVE 'A19'                TO EKOA19-IDPTYP                           
190500     MOVE 'M30'                TO EKOA19-KDEKOHT                          
190600*IDPTYP HAR VÄRDET 'A19' PGA LAB-SYSTEMETS POSTTYP                        
190700*ETT KORREKT VÄRDE ÄR R32                                                 
190800     MOVE W-IDARTNR-TO         TO EKOA19-IDARTNR                          
190900     MOVE WS-KVANTAL-NUM       TO EKOA19-KVJUSTKV                         
191000                                                                          
191100     MOVE WS-TO-ART-KDPRODSL   TO EKOA19-KDPRODSL                         
191200     MOVE WS-TO-CLAG-KDPSLLOC  TO EKOA19-KDPSLLOC                         
191300     MOVE WS-IDAVINR           TO EKOA19-IDFS                             
191400     MOVE WS-IDFTG             TO EKOA19-IDFTG                            
191500     MOVE FUNCTION CURRENT-DATE (1:8)  TO EKOA19-DAREGDAT                 
191600     MOVE WS-TODAYS-DATE-LOCAL TO EKOA19-DAREGDAT(3:6)                    
191700     MOVE WS-TO-SLAG-PRAVCOST  TO EKOA19-PRAVCOST                         
191800                                                                          
191900     MOVE IDPGM                TO FIL-IDPGM IN FIL-WDR801                 
192000     MOVE WS-TODAYS-DATE       TO FIL-TIREGDAT                            
192100     MOVE WS-TIMECLOCK         TO FIL-TIKLOCK IN FIL-WDR801               
192200                                                                          
192300     MOVE +1                   TO FIL-IDSEKVNR IN FIL-WDR801              
192400     MOVE 'W510A19 '           TO FIL-IDCPYTXT IN FIL-WDR801              
192500                                                                          
192600     MOVE EKOA19-W510A19       TO FIL-WDR801-DATA                         
192700                                                                          
192800     PERFORM IMS-ISRT-FILB01                                              
192900                                                                          
193000     PERFORM UNTIL SEGMENT-FOUND                                          
193100         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
193200         PERFORM IMS-ISRT-FILB01                                          
193300     END-PERFORM                                                          
193400     .                                                                    
193500     SKIP2                                                                
193600 S01-CHECK-IDARTNR-FROM-WDK6   SECTION.                                   
193700                                                                          
193800     MOVE W-IDARTNR-FROM            TO W-IDARTNR                          
193900     MOVE YES                       TO FROM-PART-KDPRODSL-OK              
194000     PERFORM IMS-GU-ARTC-FROM                                             
194100     IF SEGMENT-MISSING                                                   
194200       MOVE FEL-501(INDX)           TO MOD-TEMFSFEL                       
194300       MOVE NOO                     TO INDATA-SW                          
194400       PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                              
194500     ELSE                                                                 
194600       MOVE FROM-ART-IDFKNGRP       TO WS-FROM-ART-IDFKNGRP               
194700       MOVE FROM-ART-KDPRODSL       TO WS-FROM-ART-KDPRODSL               
194800*ART-KDPRODSL MUST BE 14/24/34/64/94 FOR EITHER "FROM" OR                 
194900*''TO'' PART.                                                             
195000                                                                          
195100       MOVE FROM-ART-KDPRODSL       TO TEST-KDPRODSL                      
195200       IF KDPRODSL-VOLVO-BYTES  OR                                        
195300          KDPRODSL-VOLVO-WHEELS OR                                        
195400          KDPRODSL-LOCAL-BYTES  OR                                        
195500          KDPRODSL-LOCAL-WHEELS                                           
195600         CONTINUE                                                         
195700       ELSE                                                               
195800         MOVE NOO                   TO FROM-PART-KDPRODSL-OK              
195900       END-IF                                                             
196000                                                                          
196100       IF DCS-CDC                                                         
196200         IF FROM-CLAG-KVLS > ZERO                                         
196300           MOVE FROM-CLAG-KVLS      TO WS-KVLS-FROM-BEFORE                
196400         ELSE                                                             
196500           MOVE FEL-510    (INDX)   TO MOD-TEMFSFEL                       
196600           MOVE NOO                 TO INDATA-SW                          
196700                                       KEYS-SW                            
196800                                       FROM-PART-KDPRODSL-OK              
196900           PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                          
197000         END-IF                                                           
197100       ELSE                                                               
197200         MOVE FROM-CLAG-KDPSLLOC    TO WS-FROM-CLAG-KDPSLLOC              
197300       END-IF                                                             
197400                                                                          
197500       MOVE WS-IDARTNR-FROM         TO TEST-IDARTNR                       
197600       IF BYT01-BYTES                                                     
197700         MOVE YES                   TO IDARTNR-FROM-IS-EXCHANGE-SW        
197800       END-IF                                                             
197900                                                                          
198000     END-IF                                                               
198100     .                                                                    
198200     EJECT                                                                
198300 S02-CHECK-IDARTNR-TO-WDK6  SECTION.                                      
198400                                                                          
198500     MOVE WS-IDARTNR-TO             TO W-IDARTNR-TO                       
198600     PERFORM IMS-GU-ARTC-TO                                               
198700     IF SEGMENT-MISSING                                                   
198800       IF INDATA-OK                                                       
198900         MOVE FEL-502    (INDX)     TO MOD-TEMFSFEL                       
199000         MOVE NOO                   TO INDATA-SW                          
199100                                       KEYS-SW                            
199200         PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                            
199300       END-IF                                                             
199400     ELSE                                                                 
199500       MOVE TO-ART-IDFKNGRP         TO WS-TO-ART-IDFKNGRP                 
199600       MOVE TO-ART-KDPRODSL         TO WS-TO-ART-KDPRODSL                 
199700       IF INDATA-OK                                                       
199800         MOVE TO-CLAG-KVLS          TO WS-KVLS-TO-BEFORE                  
199900         MOVE TO-CLAG-KDPSLLOC      TO WS-TO-CLAG-KDPSLLOC                
200000       END-IF                                                             
200100                                                                          
200200       IF INDATA-OK                                                       
200300         IF WS-FROM-ART-IDFKNGRP = WS-TO-ART-IDFKNGRP                     
200400           CONTINUE                                                       
200500         ELSE                                                             
200600******* INF: 'VARNING OLIKA FUNKTIONSGRUPPER.'                            
200700           CONTINUE                                                       
200800           MOVE INF-501    (INDX)   TO MOD-TEMFSINF                       
200900           MOVE 'N' TO KEYS-SW                                            
201000           MOVE 'N' TO INDATA-SW                                          
201100         END-IF                                                           
201200       END-IF                                                             
201300                                                                          
201400*ART-KDERS-UTG MUST BE LESS THAN 20 FOR THE "TO" PART.                    
201500       IF INDATA-OK                                                       
201600         IF TO-ART-KDERS-UTG >= 20                                        
201700           MOVE FEL-503    (INDX)   TO MOD-TEMFSFEL                       
201800           MOVE NOO                 TO INDATA-SW                          
201900                                       KEYS-SW                            
202000           PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                          
202100         END-IF                                                           
202200       END-IF                                                             
202300                                                                          
202400       IF INDATA-OK                                                       
202500         IF FROM-PART-KDPRODSL-OK = NOO                                   
202600           MOVE TO-ART-KDPRODSL     TO TEST-KDPRODSL                      
202700           IF KDPRODSL-VOLVO-BYTES  OR                                    
202800              KDPRODSL-VOLVO-WHEELS OR                                    
202900              KDPRODSL-LOCAL-BYTES  OR                                    
203000              KDPRODSL-LOCAL-WHEELS                                       
203100             MOVE TO-CLAG-KVLS      TO WS-KVLS-TO-BEFORE                  
203200           ELSE                                                           
203300             MOVE FEL-504    (INDX) TO MOD-TEMFSFEL                       
203400             MOVE NOO               TO INDATA-SW                          
203500                                       KEYS-SW                            
203600             PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                        
203700           END-IF                                                         
203800         END-IF                                                           
203900       END-IF                                                             
204000                                                                          
204100       IF INDATA-OK                                                       
204200         IF IDARTNR-FROM-IS-EXCHANGE                                      
204300           MOVE WS-IDARTNR-TO       TO TEST-IDARTNR                       
204400           IF BYT01-BYTES                                                 
204500             MOVE FEL-517    (INDX) TO MOD-TEMFSFEL                       
204600             MOVE NOO               TO INDATA-SW                          
204700                                         KEYS-SW                          
204800             PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                        
204900           END-IF                                                         
205000         END-IF                                                           
205100       END-IF                                                             
205200                                                                          
205300     END-IF                                                               
205400                                                                          
205500     .                                                                    
205600     EJECT                                                                
205700 S03-CHECK-FROM-PARTNR-WDK7    SECTION.                                   
205800                                                                          
205900*FROM PART NUMBER (STANDARD-ART).                                         
206000     MOVE WS-IDARTNR-FROM           TO W-IDARTNR-FROM                     
206100     PERFORM IMS-GU-ARTS-FROM                                             
206200     IF SEGMENT-MISSING                                                   
206300       MOVE FEL-501  (INDX)         TO MOD-TEMFSFEL                       
206400       MOVE NOO                     TO INDATA-SW                          
206500                                       KEYS-SW                            
206600       PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                              
206700     ELSE                                                                 
206800         IF FROM-SLAG-KVLS > ZERO                                         
206900           MOVE FROM-SLAG-KVLS      TO WS-KVLS-FROM-BEFORE                
207000           MOVE FROM-SLAG-PRAVCOST  TO WS-FROM-SLAG-PRAVCOST              
207100         ELSE                                                             
207200                                                                          
207300           MOVE FEL-510    (INDX)   TO MOD-TEMFSFEL                       
207400           MOVE NOO                 TO INDATA-SW                          
207500                                       KEYS-SW                            
207600           PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                          
207700         END-IF                                                           
207800     END-IF                                                               
207900     .                                                                    
208000     EJECT                                                                
208100 S04-CHECK-TO-PARTNR-WDK7    SECTION.                                     
208200                                                                          
208300*TO PART NUMBER (BYTES-ART).                                              
208400     MOVE WS-IDARTNR-TO             TO W-IDARTNR-TO                       
208500     PERFORM IMS-GU-ARTS-TO                                               
208600     IF SEGMENT-MISSING                                                   
208700       IF INDATA-OK                                                       
208800         MOVE FEL-502 (INDX)        TO MOD-TEMFSFEL                       
208900         MOVE NOO                   TO INDATA-SW                          
209000                                       KEYS-SW                            
209100         PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                            
209200       END-IF                                                             
209300     ELSE                                                                 
209400         MOVE TO-ART-IDFKNGRP       TO WS-TO-ART-IDFKNGRP                 
209500         MOVE TO-SLAG-KVLS          TO WS-KVLS-TO-BEFORE                  
209600         MOVE TO-SLAG-PRAVCOST      TO WS-TO-SLAG-PRAVCOST                
209700     END-IF                                                               
209800     .                                                                    
209900     EJECT                                                                
210000 S05-SHOW-PART-TEXT-WDD3            SECTION.                              
210100                                                                          
210200     EVALUATE TRUE                                                        
210300        WHEN DCS-CDC                                                      
210400           MOVE 'S  '               TO W-IDSKYLT-X                        
210500        WHEN DCS-SDC AND DCS-HOLLAND                                      
210600           MOVE 'NL '               TO W-IDSKYLT-X                        
210700        WHEN OTHER                                                        
210800           MOVE 'GB '               TO W-IDSKYLT-X                        
210900     END-EVALUATE                                                         
211000                                                                          
211100                                                                          
211200*FROM PART NUMBER (STANDARD-ART).                                         
211300     MOVE W-IDARTNR-FROM            TO W-D3BSEQ-IDARTNR                   
211400                                                                          
211500     PERFORM IMS-GU-BENA11                                                
211600     IF SEGMENT-FOUND                                                     
211700        MOVE TEXT-BEART             TO MOD-FROM-BEART                     
211800     ELSE                                                                 
211900        MOVE 'NAME MISSING'         TO MOD-FROM-BEART                     
212000     END-IF                                                               
212100                                                                          
212200*TO PART NUMBER (BYTES-ART).                                              
212300     MOVE W-IDARTNR-TO              TO W-D3BSEQ-IDARTNR                   
212400                                                                          
212500     PERFORM IMS-GU-BENA11                                                
212600     IF SEGMENT-FOUND                                                     
212700        MOVE TEXT-BEART             TO MOD-TO-BEART                       
212800     ELSE                                                                 
212900        MOVE 'NAME MISSING'         TO MOD-TO-BEART                       
213000     END-IF                                                               
213100     .                                                                    
213200     EJECT                                                                
213300 S06-SHOW-INFO           SECTION.                                         
213400                                                                          
213500     MOVE WS-KVLS-FROM-BEFORE       TO MOD-FROM-KVLS-BEFORE               
213600                                                                          
213700     MOVE WS-KVLS-TO-BEFORE         TO MOD-TO-KVLS-BEFORE                 
213800     .                                                                    
213900     EJECT                                                                
214000 S07-ERR-ROUTINE-ROER-EJ-FAELT SECTION.                                   
214100     PERFORM MFS-DO-NOT-TOUCH-FIELD-OUT                                   
214200     PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                    
214300     .                                                                    
214400     EJECT                                                                
214500 MFS-ERASE-FIELD-OUT SECTION.                                             
214600                                                                          
214700*    --- ALLA UTDATA-FÄLT                                                 
214800     MOVE MFS-ERASE-FIELD           TO MOD-KVANTAL                        
214900                                       MOD-IDANALYS                       
215000                                       MOD-IDKONTO                        
215100                                       MOD-IDAVINR                        
215200                                                                          
215300     .                                                                    
215400     SKIP3                                                                
215500 MFS-ERASE-FIELD-IN SECTION.                                              
215600                                                                          
215700*    --- ALLA INDATA-FÄLT                                                 
215800     MOVE MFS-ERASE-FIELD           TO MOD-KVANTAL                        
215900                                       MOD-IDANALYS                       
216000                                       MOD-IDKONTO                        
216100                                       MOD-IDAVINR                        
216200                                                                          
216300     .                                                                    
216400     EJECT                                                                
216500 MFS-DO-NOT-TOUCH-FIELD-OUT  SECTION.                                     
216600                                                                          
216700*    --- ALLA UTDATA-FÄLT                                                 
216800     MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KVANTAL                        
216900                                       MOD-IDANALYS                       
217000                                       MOD-IDKONTO                        
217100                                       MOD-IDAVINR                        
217200     .                                                                    
217300     SKIP3                                                                
217400 MFS-DO-NOT-TOUCH-FIELD-IN  SECTION.                                      
217500                                                                          
217600*    --- ALLA INDATA-FÄLT                                                 
217700     MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KVANTAL                        
217800                                       MOD-IDANALYS                       
217900                                       MOD-IDKONTO                        
218000                                       MOD-IDAVINR                        
218100     .                                                                    
218200     EJECT                                                                
218300 MFS-FORM-ATTR SECTION.                                                   
218400                                                                          
218500*    --- ALL INDATA-FIELDS                                                
218600                                                                          
218700     MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-KVANTAL-ATTR                   
218800                                       MOD-IDANALYS-ATTR                  
218900                                       MOD-IDKONTO-ATTR                   
219000                                       MOD-IDAVINR-ATTR                   
219100     .                                                                    
219200     SKIP2                                                                
219300* --- IMS SECTIONS ---                                                    
219400     SKIP3                                                                
219500 IMS-GET-MSG SECTION.                                                     
219600                                                                          
219700     MOVE '  QC' TO GOOD-STATUSCODES                                      
219800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
219900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
220000     PERFORM IMS-STATUSCHECK                                              
220100     .                                                                    
220200     SKIP3                                                                
220300 IMS-INSERT-MSG SECTION.                                                  
220400                                                                          
220500     IF ENGLISH-TEXT                                                      
220600       MOVE 'N' TO MFS-KDHUVOMR                                           
220700     ELSE                                                                 
220800       MOVE '0' TO MFS-KDHUVOMR                                           
220900     END-IF                                                               
221000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
221100     MOVE SPACE TO GOOD-STATUSCODES                                       
221200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
221300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
221400     PERFORM IMS-STATUSCHECK                                              
221500     .                                                                    
221600     EJECT                                                                
221700 IMS-GU-ARTC-FROM     SECTION.                                            
221800                                                                          
221900     STRING 'WLARTC01*D(IDARTNR  =' W-IDARTNR-FROM-X ')'                  
222000          DELIMITED BY SIZE INTO SSA1                                     
222100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
222200          DELIMITED BY SIZE INTO SSA2                                     
222300     MOVE '  GE' TO GOOD-STATUSCODES                                      
222400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2               
222500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSCHECK                                              
222700     .                                                                    
222800     EJECT                                                                
222900 IMS-GU-ARTC-TO      SECTION.                                             
223000                                                                          
223100     STRING 'WLARTC01*D(IDARTNR  =' W-IDARTNR-TO-X ')'                    
223200          DELIMITED BY SIZE INTO SSA1                                     
223300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
223400          DELIMITED BY SIZE INTO SSA2                                     
223500     MOVE '  GE' TO GOOD-STATUSCODES                                      
223600     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA-3 SSA1 SSA2              
223700     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
223800     PERFORM IMS-STATUSCHECK                                              
223900     .                                                                    
224000     EJECT                                                                
224100 IMS-GHU-ARTC-FROM  SECTION.                                              
224200                                                                          
224300     STRING 'WLARTC01*D(IDARTNR  =' W-IDARTNR-FROM-X ')'                  
224400          DELIMITED BY SIZE INTO SSA1                                     
224500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
224600          DELIMITED BY SIZE INTO SSA2                                     
224700     MOVE '  GE' TO GOOD-STATUSCODES                                      
224800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2              
224900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
225000     PERFORM IMS-STATUSCHECK                                              
225100     .                                                                    
225200     EJECT                                                                
225300 IMS-GHU-ARTC-TO  SECTION.                                                
225400                                                                          
225500     STRING 'WLARTC01*D(IDARTNR  =' W-IDARTNR-TO-X ')'                    
225600          DELIMITED BY SIZE INTO SSA1                                     
225700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
225800          DELIMITED BY SIZE INTO SSA2                                     
225900     MOVE '  GE' TO GOOD-STATUSCODES                                      
226000     CALL CBLTDLI USING GHU ARTC2-PCB DLI-IO-AREA-3 SSA1 SSA2             
226100     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
226200     PERFORM IMS-STATUSCHECK                                              
226300     .                                                                    
226400     EJECT                                                                
226500 IMS-REPL-ARTC-FROM    SECTION.                                           
226600                                                                          
226700     MOVE '  ' TO GOOD-STATUSCODES                                        
226800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-1                       
226900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
227000     PERFORM IMS-STATUSCHECK                                              
227100     .                                                                    
227200     EJECT                                                                
227300 IMS-REPL-ARTC-TO     SECTION.                                            
227400                                                                          
227500     MOVE '  ' TO GOOD-STATUSCODES                                        
227600     CALL CBLTDLI USING REPL ARTC2-PCB DLI-IO-AREA-3                      
227700     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
227800     PERFORM IMS-STATUSCHECK                                              
227900     .                                                                    
228000     EJECT                                                                
228100 IMS-ISRT-WDL901 SECTION.                                                 
228200     SKIP2                                                                
228300     MOVE 'WLLOGA01 ' TO SSA1                                             
228400     MOVE '  II' TO GOOD-STATUSCODES                                      
228500     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
228600     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
228700     PERFORM IMS-STATUSCHECK                                              
228800     .                                                                    
228900     EJECT                                                                
229000 IMS-GU-ARTS-FROM      SECTION.                                           
229100                                                                          
229200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-FROM-X ')'                    
229300          DELIMITED BY SIZE INTO SSA1                                     
229400     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
229500          DELIMITED BY SIZE INTO SSA2                                     
229600     MOVE '  GE' TO GOOD-STATUSCODES                                      
229700     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-5 SSA1 SSA2               
229800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
229900     PERFORM IMS-STATUSCHECK                                              
230000     .                                                                    
230100     EJECT                                                                
230200 IMS-GU-ARTS-TO       SECTION.                                            
230300                                                                          
230400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-TO-X ')'                      
230500          DELIMITED BY SIZE INTO SSA1                                     
230600     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
230700          DELIMITED BY SIZE INTO SSA2                                     
230800     MOVE '  GE' TO GOOD-STATUSCODES                                      
230900     CALL CBLTDLI USING GU ARTS2-PCB DLI-IO-AREA-6 SSA1 SSA2              
231000     MOVE ARTS2-STATUS-CODE TO STATUS-WS                                  
231100     PERFORM IMS-STATUSCHECK                                              
231200     .                                                                    
231300     EJECT                                                                
231400 IMS-GHU-ARTS-FROM     SECTION.                                           
231500                                                                          
231600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-FROM-X ')'                    
231700          DELIMITED BY SIZE INTO SSA1                                     
231800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
231900          DELIMITED BY SIZE INTO SSA2                                     
232000     MOVE '  GE' TO GOOD-STATUSCODES                                      
232100     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-5 SSA1 SSA2              
232200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
232300     PERFORM IMS-STATUSCHECK                                              
232400     .                                                                    
232500     EJECT                                                                
232600 IMS-GHU-ARTS-TO      SECTION.                                            
232700                                                                          
232800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-TO-X ')'                      
232900          DELIMITED BY SIZE INTO SSA1                                     
233000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
233100          DELIMITED BY SIZE INTO SSA2                                     
233200     MOVE '  GE' TO GOOD-STATUSCODES                                      
233300     CALL CBLTDLI USING GHU ARTS2-PCB DLI-IO-AREA-6 SSA1 SSA2             
233400     MOVE ARTS2-STATUS-CODE TO STATUS-WS                                  
233500     PERFORM IMS-STATUSCHECK                                              
233600     .                                                                    
233700     EJECT                                                                
233800 IMS-REPL-ARTS-FROM      SECTION.                                         
233900                                                                          
234000     MOVE '  ' TO GOOD-STATUSCODES                                        
234100     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-5                       
234200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
234300     PERFORM IMS-STATUSCHECK                                              
234400     .                                                                    
234500     EJECT                                                                
234600 IMS-REPL-ARTS-TO       SECTION.                                          
234700                                                                          
234800     MOVE '  ' TO GOOD-STATUSCODES                                        
234900     CALL CBLTDLI USING REPL ARTS2-PCB DLI-IO-AREA-6                      
235000     MOVE ARTS2-STATUS-CODE TO STATUS-WS                                  
235100     PERFORM IMS-STATUSCHECK                                              
235200     .                                                                    
235300     EJECT                                                                
235400 IMS-GU-BENA11       SECTION.                                             
235500                                                                          
235600     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
235700          DELIMITED BY SIZE INTO SSA1                                     
235800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
235900          DELIMITED BY SIZE INTO SSA2                                     
236000     MOVE '  GE' TO GOOD-STATUSCODES                                      
236100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-7 SSA1 SSA2               
236200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
236300     PERFORM IMS-STATUSCHECK                                              
236400     .                                                                    
236500     EJECT                                                                
236600 IMS-GHU-INLE01    SECTION.                                               
236700                                                                          
236800     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
236900          DELIMITED BY SIZE INTO SSA1                                     
237000     MOVE '  GE' TO GOOD-STATUSCODES                                      
237100     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-AREA-8 SSA1                   
237200     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
237300     PERFORM IMS-STATUSCHECK                                              
237400     .                                                                    
237500     SKIP3                                                                
237600 IMS-ISRT-INLE01    SECTION.                                              
237700                                                                          
237800     MOVE 'WLINLE01 '         TO SSA1                                     
237900     MOVE '  II' TO GOOD-STATUSCODES                                      
238000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-8 SSA1                  
238100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
238200     PERFORM IMS-STATUSCHECK                                              
238300     .                                                                    
238400     EJECT                                                                
238500 IMS-ISRT-INLE11    SECTION.                                              
238600                                                                          
238700     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
238800          DELIMITED BY SIZE INTO SSA1                                     
238900     MOVE 'WLINLE11 '         TO SSA2                                     
239000     MOVE '  II' TO GOOD-STATUSCODES                                      
239100     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-8 SSA1 SSA2             
239200     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
239300     PERFORM IMS-STATUSCHECK                                              
239400     .                                                                    
239500     SKIP3                                                                
239600 IMS-ISRT-INLE21   SECTION.                                               
239700     MOVE 'WLINLE21 ' TO SSA1                                             
239800     MOVE '  ' TO GOOD-STATUSCODES                                        
239900     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-8 SSA1                  
240000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
240100     PERFORM IMS-STATUSCHECK                                              
240200     .                                                                    
240300     SKIP3                                                                
240400 IMS-GHU-INLC01     SECTION.                                              
240500                                                                          
240600     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
240700          DELIMITED BY SIZE INTO SSA1                                     
240800     MOVE '  GE' TO GOOD-STATUSCODES                                      
240900     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-AREA-8 SSA1                   
241000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
241100     PERFORM IMS-STATUSCHECK                                              
241200     .                                                                    
241300     SKIP2                                                                
241400 IMS-ISRT-INLC01    SECTION.                                              
241500                                                                          
241600     MOVE 'WLINLC01 ' TO SSA1                                             
241700     MOVE '  II' TO GOOD-STATUSCODES                                      
241800     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-8 SSA1                  
241900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
242000     PERFORM IMS-STATUSCHECK                                              
242100     .                                                                    
242200     SKIP2                                                                
242300 IMS-ISRT-INLC11    SECTION.                                              
242400                                                                          
242500     MOVE 'WLINLC11 '         TO SSA1                                     
242600     MOVE '  II' TO GOOD-STATUSCODES                                      
242700     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-8 SSA1                  
242800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
242900     PERFORM IMS-STATUSCHECK                                              
243000     .                                                                    
243100     SKIP3                                                                
243200 IMS-ISRT-FILB01   SECTION.                                               
243300                                                                          
243400     MOVE 'WLFILB01 ' TO SSA1                                             
243500     MOVE '  II' TO GOOD-STATUSCODES                                      
243600     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-9 SSA1                  
243700     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
243800     PERFORM IMS-STATUSCHECK                                              
243900     .                                                                    
244000     EJECT                                                                
244100 IMS-ISRT-WDR901 SECTION.                                                 
244200                                                                          
244300     MOVE 'WLSAPA01 ' TO SSA1                                             
244400     MOVE '  II' TO GOOD-STATUSCODES                                      
244500     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
244600     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
244700     PERFORM IMS-STATUSCHECK                                              
244800     .                                                                    
244900     EJECT                                                                
245000 IMS-GHU-WDA901 SECTION.                                                  
245100                                                                          
245200     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
245300          DELIMITED BY SIZE INTO SSA1                                     
245400     MOVE '  GE'           TO GOOD-STATUSCODES                            
245500     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
245600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
245700     PERFORM IMS-STATUSCHECK                                              
245800     .                                                                    
245900 IMS-GHU-WDA912 SECTION.                                                  
246000                                                                          
246100     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
246200          DELIMITED BY SIZE INTO SSA1                                     
246300     STRING 'WDA912  (DAAAPPR  =' W-DAAAPP-WDA9-X ')'                     
246400          DELIMITED BY SIZE INTO SSA2                                     
246500     MOVE '  GE'           TO GOOD-STATUSCODES                            
246600     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA912 SSA1                   
246700                                                   SSA2                   
246800     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
246900     PERFORM IMS-STATUSCHECK                                              
247000     .                                                                    
247100 IMS-ISRT-WDA901 SECTION.                                                 
247200                                                                          
247300     MOVE 'WDA901   '      TO SSA1                                        
247400     MOVE '  '             TO GOOD-STATUSCODES                            
247500     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
247600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
247700     PERFORM IMS-STATUSCHECK                                              
247800     .                                                                    
247900 IMS-ISRT-WDA912 SECTION.                                                 
248000                                                                          
248100     MOVE 'WDA912   '      TO SSA1                                        
248200     MOVE '  '             TO GOOD-STATUSCODES                            
248300     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA912 SSA1                  
248400     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
248500     PERFORM IMS-STATUSCHECK                                              
248600     .                                                                    
248700 IMS-REPL-WDA912 SECTION.                                                 
248800                                                                          
248900     MOVE '  ' TO GOOD-STATUSCODES                                        
249000     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA912                       
249100     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
249200     PERFORM IMS-STATUSCHECK                                              
249300     .                                                                    
249400 IMS-GU-WDB601    SECTION.                                                
249500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
249600          DELIMITED BY SIZE INTO SSA1                                     
249700     MOVE '  ' TO GOOD-STATUSCODES                                        
249800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
249900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
250000     PERFORM IMS-STATUSCHECK                                              
250100     .                                                                    
250200     EJECT                                                                
250300 IMS-STATUSCHECK SECTION.                                                 
250400                                                                          
250500     SET STATUS-IX TO 1                                                   
250600     SEARCH GOOD-STATUS                                                   
250700       AT END                                                             
250800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
250900         DELIMITED BY SIZE INTO ERROR-TEXT                                
251000         CALL FELLOG                                                      
251100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
251200         CONTINUE                                                         
251300     END-SEARCH                                                           
251400     .                                                                    
