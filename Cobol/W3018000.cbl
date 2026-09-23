000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3018000.                                                
000400 AUTHOR.         REDDY RAHUL.                                             
000500 DATE-WRITTEN.   14/10/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CARPARTS.NES.REMANORDER                                  
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        NES SCREEN TO DISPLAY ALL CORE PARTS ASSOCIATED TO A             
001200*        REMANUFACTURER AND ALLOW TO ORDER.                               
001300*                                                                         
001400*        THE PROGRAM READS     WDD3                                       
001500*        THE PROGRAM READS     WDK7                                       
001600*        THE PROGRAM READS     WDA9                                       
001700*        THE PROGRAM READS     WDA9A                                      
001800*        THE PROGRAM UPDATES   WDR2                                       
001900*        THE PROGRAM READS     WDR4                                       
002000*        THE PROGRAM READS     WDD9                                       
002100*        THE PROGRAM READS     BYART                                      
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSACTION: W30180T                                             
002500*        REQUEST:     W30180I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        RESPONSE:    W30180O1                                            
002900*                                                                         
003000*    CHANGE LOG:                                                          
003100*                                                                         
003200*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003300*      ----------------------------------------------------------         
003400*      14/10/16 - REDDY RAHUL     - INITIAL VERSION.                      
003500*                                   ETRACKER 10179385                     
003600*                                                                         
003700*      15/11/25 - REDDY RAHUL     - ADD CALLOFFS AND ARREARS.             
003800*                                   ETRACKER 10251640                     
003900*                                                                         
004000*      16/06/20 - HÅKAN BOHLIN    - CREATE CSV FILE FROM NES.             
004100*                                   ETRACKER 10251631                     
004200*                                                                         
004300*                                                                         
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     SKIP2                                                                
004800 INPUT-OUTPUT SECTION.                                                    
004900                                                                          
005000 FILE-CONTROL.                                                            
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP3                                                                
005400 FILE SECTION.                                                            
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700 77  IDPGM                       PIC X(08)   VALUE 'W3018000'.            
005800 77  WS-TRANS                    PIC X(04)   VALUE '3180'.                
005900                                                                          
006000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006100 77  FILLER                      PIC X(08) VALUE 'ERR-TEXT'.              
006200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006300 77  CURRENT-DB-SEC              PIC X(32) VALUE SPACE.                   
006400 77  KDRC-DISPLAY                PIC Z(5).                                
006500                                                                          
006600 77  YES                         PIC X       VALUE 'Y'.                   
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NOO                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  IX                          PIC 9(5)    VALUE ZERO.                  
007100                                                                          
007200 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
007300     88  KEYS-OK                             VALUE 'Y'.                   
007400     88  KEYS-WRONG                          VALUE 'N'.                   
007500                                                                          
007600 77  FIRST-SW                   PIC X       VALUE 'Y'.                    
007700     88  FIRST-LINE                         VALUE 'Y'.                    
007800                                                                          
007900 77  ACTION-SW                   PIC X       VALUE SPACE.                 
008000     88  SELECT-PARTS                        VALUE 'S' '3'.               
008100     88  ORDER-PARTS                         VALUE 'V'.                   
008200     88  CREATE-CSV-FILE                     VALUE 'E'.                   
008300                                                                          
008400 01  ALL-SPACE.                                                           
008500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
008600                                                                          
008700 01  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
008800 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
008900     03  WS-CURRENT-DATE-YY      PIC 9(2).                                
009000     03  WS-CURRENT-DATE-YYMMDD  PIC 9(6).                                
009100 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
009200                                                                          
009300 01  WS-CURRENT-DATE-TIME.                                                
009400     03  WS-YEAR                 PIC 9(4).                                
009500     03  WS-MONTH                PIC 9(2).                                
009600     03  WS-DAY                  PIC 9(2).                                
009700     03  WS-HOUR                 PIC 9(2).                                
009800     03  WS-MINUTE               PIC 9(2).                                
009900 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
010000     03  FILLER                  PIC X(2).                                
010100     03  WS-TIYYMMDDHHMM         PIC X(10).                               
010200                                                                          
010300 01  WS-IDARTNR                  PIC 9(9) VALUE ZERO.                     
010400 01  FILLER REDEFINES WS-IDARTNR.                                         
010500   03  FILLER                    PIC 9(5).                                
010600   03  WS-ARTSIFFRA              PIC 9(1).                                
010700     88  ART-0                   VALUE 6.                                 
010800     88  ART-1                   VALUE 4  7.                              
010900     88  ART-2                   VALUE 5  8.                              
011000     88  ART-3                   VALUE 9.                                 
011100   03  FILLER                    PIC 9(3).                                
011200                                                                          
011300 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
011400 01  WS-IDORDER                  PIC 9(7)    VALUE ZERO.                  
011500 01  WS-KVAVBART                 PIC 9(7)    VALUE ZERO.                  
011600 01  WS-TIREGTID                 PIC S9(7)   COMP-3 VALUE ZERO.           
011700                                                                          
011800 01  WS-PARTS-TO-ORDER           PIC 9(8)    VALUE ZERO.                  
011900 01  WS-KVLS-REM                 PIC S9(7)   VALUE ZERO.                  
012000 01  WS-KVADV-REM                PIC 9(7)    VALUE ZERO.                  
012100 01  WS-IDARTNR-BYT              PIC S9(9) COMP-3 VALUE ZERO.             
012200 01  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
012300 01  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
012400 01  WS-IDDISTR-WSEC             PIC 9(4)    VALUE ZERO.                  
012600 01  WS-KVBEART-UPD-NUM          PIC 9(7)    VALUE ZERO.                  
012700                                                                          
012800 01  WS-KVAVROP                  PIC S9(7) COMP-3 VALUE ZERO.             
012900 01  WS-KVAVROP-GAM              PIC S9(7) COMP-3 VALUE ZERO.             
013000                                                                          
013100 01  WS-CURR-WEEK                PIC 9(6)    VALUE ZERO.                  
013200 01  FILLER REDEFINES WS-CURR-WEEK.                                       
013300     03  WS-CURR-WEEK-CC         PIC 9(2).                                
013400     03  WS-CURR-WEEK-AAVV       PIC 9(4).                                
013500                                                                          
013600 01  WS-END-WEEK                 PIC 9(6)    VALUE ZERO.                  
013700 01  FILLER REDEFINES WS-END-WEEK.                                        
013800     03  WS-END-WEEK-CC          PIC 9(2).                                
013900     03  WS-END-WEEK-AAVV        PIC 9(4).                                
014000                                                                          
014100     EJECT                                                                
014200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014300 01  GENERAL-SUBPROGRAMS.                                                 
012410     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
014800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
015000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
013110     EJECT                                                                
013120*    --- PARAMETERS TO W005INIT                                           
013130 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013140*01 -COPY WMSGINIT                                                        
015200     SKIP3                                                                
015300*    --- PARAMETERS TO WDATKONV                                           
015400                                                                          
015500*01  -COPY WDATAREA                                                       
015600     EJECT                                                                
015700*    --- PARAMETERS TO W009VADD                                           
015800                                                                          
015900 01  W009VADD-AAVV               PIC S9(5)   COMP-3.                      
016000 01  W009VADD-ANTAL              PIC S9(3)   COMP-3.                      
016100     EJECT                                                                
016200                                                                          
016300*    --- PARAMETERS TO ABEND                                              
016400                                                                          
016500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016800     SKIP3                                                                
016900 01  MESSAGE-CODES.                                                       
017000     03  REMAN-INVALID           PIC X(80)                                
017100               VALUE '109 REMANUFACTURER IS INVALID'.                     
017200                                                                          
017300     03  REMAN-NOT-NUMERIC       PIC X(80)                                
017400               VALUE '110 REMANUFACTURER MUST BE NUMERIC'.                
017500                                                                          
017600     03  REMAN-NOT-ENTERED       PIC X(80)                                
017700               VALUE '111 REMANUFACTURER MUST BE ENTERED'.                
017800                                                                          
017900     03  NOT-AUTHORIZED          PIC X(80)                                
018000               VALUE '114 YOUR ID IS NOT AUTHORIZED FOR THIS REMAN        
018100-                    'UFACTURER'.                                         
018200                                                                          
018300     03  PREV-ORDER-PENDING      PIC X(80)                                
018400               VALUE '117 PREVIOUS ORDER IS STILL PROCESSING'.            
018500                                                                          
018600     03  NO-PART-TO-ORDER        PIC X(80)                                
018700               VALUE '118 NO PARTNO HAS AN ORDER QTY > 0 '.               
018800                                                                          
018900     03  QTY-NOT-NUMERIC         PIC X(80)                                
019000               VALUE '119 ORDER QUANTITY NOT NUMERIC'.                    
019100                                                                          
019200     03  NO-PART-CAN-BE-ORDERED  PIC X(80)                                
019300               VALUE '121 NO PARTNO CAN BE ORDERED'.                      
019400                                                                          
019500     03  CSV-FILE-STARTED        PIC X(80)                                
019600               VALUE 'COPY TO EXCEL FUNCTION STARTED'.                    
019700                                                                          
019800     EJECT                                                                
019900                                                                          
020000*01  -COPY WWDCKONS                                                       
020100     EJECT                                                                
020200                                                                          
020300 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
020400*01  FILLER -COPY WWDIS134    -RED TEST-IDDISTR.                          
020500     EJECT                                                                
020600                                                                          
020700 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
020800*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
020900*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
021000     EJECT                                                                
021100*                                                                         
021200*01  -COPY WSECAREA                                                       
021300     EJECT                                                                
021400*                                                                         
021500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
021600     SKIP3                                                                
021700*01  -COPY WZ01SUB                                                        
021800     EJECT                                                                
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
022100     SKIP3                                                                
022200*01  -COPY WZ01SEND                                                       
022300     EJECT                                                                
022400 01  SEND-AREA-TO-CSV-LINE.                                               
022500     03 CSV-IDDISTR              PIC Z(3)9.                               
022600     03 FILLER                   PIC X(1) VALUE ';'.                      
022700     03 CSV-IDARTNR-OBJ          PIC Z(7)9.                               
022800     03 FILLER                   PIC X(1) VALUE ';'.                      
022900     03 CSV-BEART                PIC X(25).                               
023000     03 FILLER                   PIC X(1) VALUE ';'.                      
023100     03 CSV-KVLS-91              PIC -(7)9.                               
023200     03 FILLER                   PIC X(1) VALUE ';'.                      
023300     03 CSV-KVLS-REM             PIC -(7)9.                               
023400     03 FILLER                   PIC X(1) VALUE ';'.                      
023500     03 CSV-KVBYTPKO             PIC Z(2)9.                               
023600     03 FILLER                   PIC X(1) VALUE ';'.                      
023700     03 CSV-KVBEART-UPD          PIC Z(5)9.                               
023800     03 FILLER                   PIC X(1) VALUE ';'.                      
023900     03 CSV-KVANTAL-PREADV       PIC Z(5)9.                               
024000     03 FILLER                   PIC X(1) VALUE ';'.                      
024100     03 CSV-KVAVROP              PIC Z(6)9.                               
024200     03 FILLER                   PIC X(1) VALUE ';'.                      
024300     03 CSV-KVAVROP-GAM          PIC Z(6)9.                               
024400     03 FILLER                   PIC X(1) VALUE ';'.                      
024500     03 CSV-IDORDER              PIC Z(6)9.                               
024600     03 FILLER                   PIC X(1) VALUE ';'.                      
024700     03 CSV-DAORDDAT             PIC 9(8).                                
024800     03 FILLER                   PIC X(1) VALUE ';'.                      
024900     03 CSV-KVBEART              PIC Z(5)9.                               
025000     03 FILLER                   PIC X(1) VALUE ';'.                      
025100                                                                          
025200 01  SEND-AREA-TO-CSV-TITLE.                                              
025300     03 FILLER           PIC X(14) VALUE 'REMANUFACTURER'.                
025400     03 FILLER           PIC X(1)  VALUE ';'.                             
025500     03 FILLER           PIC X(7)  VALUE 'CORE NO'.                       
025600     03 FILLER           PIC X(1)  VALUE ';'.                             
025700     03 FILLER           PIC X(11) VALUE 'DESCRIPTION'.                   
025800     03 FILLER           PIC X(1)  VALUE ';'.                             
025900     03 FILLER           PIC X(18) VALUE 'AVAILABLE STOCK ET'.            
026000     03 FILLER           PIC X(1)  VALUE ';'.                             
026100     03 FILLER           PIC X(14) VALUE 'STOCK AT REMAN'.                
026200     03 FILLER           PIC X(1)  VALUE ';'.                             
026300     03 FILLER           PIC X(13) VALUE 'STD UNIT LOAD'.                 
026500     03 FILLER           PIC X(1)  VALUE ';'.                             
026600     03 FILLER           PIC X(9)  VALUE 'ORDER QTY'.                     
026700     03 FILLER           PIC X(1)  VALUE ';'.                             
026800     03 FILLER           PIC X(11) VALUE 'ADVICED QTY'.                   
026900     03 FILLER           PIC X(1)  VALUE ';'.                             
027000     03 FILLER           PIC X(20) VALUE 'CALL-OFFS REMAN PART'.          
027100     03 FILLER           PIC X(1)  VALUE ';'.                             
027200     03 FILLER           PIC X(19) VALUE 'TOTAL QTY OF ARREAR'.           
027300     03 FILLER           PIC X(1)  VALUE ';'.                             
027400     03 FILLER           PIC X(13) VALUE 'LAST ORDER NO'.                 
027500     03 FILLER           PIC X(1)  VALUE ';'.                             
027600     03 FILLER           PIC X(15) VALUE 'LAST ORDER DATE'.               
027700     03 FILLER           PIC X(1)  VALUE ';'.                             
027800     03 FILLER           PIC X(14) VALUE 'LAST ORDER QTY'.                
027900     03 FILLER           PIC X(1)  VALUE ';'.                             
028000                                                                          
028100 01  HDR-AREA.                                                            
028200*    03 -COPY WZ01REQU -PRE HDR-                                          
028300*    03 -COPY WZ04HDR                                                     
028400     EJECT                                                                
028500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
028600     SKIP3                                                                
028700 01  REQU-AREA.                                                           
028800*    03  -COPY WMIDPRE2                                                   
028900*    03  -COPY W30180I1                                                   
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
029200     SKIP3                                                                
029300 01  RESP-AREA.                                                           
029400*    03  -COPY WMODPRE2                                                   
029500*    03  -COPY W30180O1                                                   
029600     EJECT                                                                
029700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
029800*                                                                         
029900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030000     SKIP3                                                                
030100 01  KEYS-FOR-DLI.                                                        
030200     03  W-IDARTNR-X.                                                     
030300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
030400                                                                          
030500     03  W-IDARTNR-K6-X.                                                  
030600         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
030700                                                                          
030800     03  W-IDSKYLT-X.                                                     
030900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
031000                                                                          
031100     03  W-IDDC-X.                                                        
031200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
031300                                                                          
031400     03  W-IDDISTR-X.                                                     
031500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
031600                                                                          
031700     03  W-WDA9A1KY-MIN-X.                                                
031800         05  W-IDDISTR-A9A-MIN   PIC S9(5)   VALUE ZERO COMP-3.           
031900         05  FILLER              PIC X(5)    VALUE LOW-VALUE.             
032000     03  W-WDA9A1KY-MAX-X.                                                
032100         05  W-IDDISTR-A9A-MAX   PIC S9(5)   VALUE ZERO COMP-3.           
032200         05  W-IDARTNR-A9A-MAX   PIC X(5)    VALUE HIGH-VALUE.            
032300     03  W-WDGXKEY-3151-X.                                                
032400         05  W-3151-IDHTYP       PIC X(4)    VALUE '3151'.                
032500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
032600     03  W-W3152KY-X.                                                     
032700         05  W-3152-IDUSER       PIC X(8)    VALUE SPACE.                 
032800         05  W-3152-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
032900     03  W-W3154KY-X.                                                     
033000         05  W-IDARTNRO          PIC S9(9)   VALUE ZERO COMP-3.           
033100     03  W-WDGXKEY-3161-X.                                                
033200         05  W-3161-IDHTYP       PIC X(4)    VALUE '3161'.                
033300         05  W-3161-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
033400         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
033500     03  W-WDD901KY-X.                                                    
033600         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
033700         05  W-IDDC-D9           PIC X(2)    VALUE '11'.                  
033800     03  W-IDLEVNR-X.                                                     
033900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
034000                                                                          
034100     SKIP2                                                                
034200*    --- STATUS-KOD FRÅN IMS                                              
034300 01  STATUS-WS                   PIC XX.                                  
034400     88  SEGMENT-FOUND                       VALUE '  '.                  
034500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
034600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
034700     88  END-OF-DATABASE                     VALUE 'GB'.                  
034800     SKIP2                                                                
034900 01  GOOD-STATUSCODES.                                                    
035000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035100     SKIP3                                                                
035200 01  SSA1                        PIC X(64).                               
035300 01  SSA2                        PIC X(64).                               
035400 01  SSA3                        PIC X(64).                               
035500     EJECT                                                                
035600*    --- WORK-AREAS FOR DB2-SECTIONS                                      
035700*                                                                         
035800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
035900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
036000     SKIP3                                                                
036100*                                                                         
036200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
036300 01  DB2-WS.                                                              
036400     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
036500         88  ROW-FOUND                       VALUE  000.                  
036600         88  ROW-NOTFOUND                    VALUE  100.                  
036700         88  RESOURCE-UNAVAILABLE            VALUE  904.                  
036800     03  GOOD-SQLCODECODES.                                               
036900         05  GOOD-SQLCODE OCCURS 5                                        
037000           INDEXED BY SQLCODE-IX PIC  9(3).                               
037100*                                                                         
037200     EJECT                                                                
037300*    --- DB2 HOST-COPYTEXT                                                
037400 01  FILLER                      PIC X(16)   VALUE 'DB2-WS'.              
037500*01  -COPY BYART -PRE BYART-                                              
037600     EJECT                                                                
037700     SKIP3                                                                
037800*    --- DB2 DCL                                                          
037900 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
038000       EXEC SQL INCLUDE BYART END-EXEC.                                   
038100*    SKIP3                                                                
038200*    --- IMS FUNCTION CODES                                               
038300*01  -COPY W0003                                                          
038400     EJECT                                                                
038500                                                                          
038600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
038700 01  DLI-IO-WDD311.                                                       
038800*    03  -COPY WDD311                                                     
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
039000 01  DLI-IO-WDK711.                                                       
039100*    03  -COPY WDK711                                                     
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
039300 01  DLI-IO-WDA901.                                                       
039400*    03  -COPY WDA901                                                     
039500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
039600 01  DLI-IO-WDA911.                                                       
039700*    03  -COPY WDA911                                                     
039800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA9A'.                       
039900 01  DLI-IO-WDA9A.                                                        
040000*    03  -COPY WDA9A1                                                     
040100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3151'.                    
040200 01  DLI-IO-WDGX3151.                                                     
040300*    03  -COPY WDGX01                                                     
040400     EJECT                                                                
040500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3152'.                    
040600 01  DLI-IO-WDGX3152.                                                     
040700*    03  -COPY WDGX3152                                                   
040800     EJECT                                                                
040900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3154'.                    
041000 01  DLI-IO-WDGX3154.                                                     
041100*    03  -COPY WDGX3154                                                   
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
041300 01  DLI-IO-WDGX3161.                                                     
041400*    03  -COPY WDGX3161                                                   
041500     EJECT                                                                
041600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
041700 01  DLI-IO-WDGX3162.                                                     
041800*    03  -COPY WDGX3162                                                   
041900     EJECT                                                                
042000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
042100 01  DLI-IO-WDK601.                                                       
042200*    03  -COPY WDK601                                                     
042300     EJECT                                                                
042400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
042500 01  DLI-IO-WDD902.                                                       
042600*    03  -COPY WDD902                                                     
042700     EJECT                                                                
042800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
042900 01  DLI-IO-WDD905.                                                       
043000*    03  -COPY WDD905                                                     
043100     EJECT                                                                
043200 LINKAGE SECTION.                                                         
043300*01  -COPY W0009   -PRE MSG-                                              
                                                                                
043500*01  -COPY W0009   -PRE DISTRDOC-                                         
043600                                                                          
      *01  -COPY W0008  -PRE USEA-                                              
           05  FILLER                  PIC X.                                   
                                                                                
043700*01  -COPY W0008  -PRE WDD3-                                              
043800     05  FILLER                  PIC X.                                   
043900                                                                          
044000*01  -COPY W0008  -PRE WDK7-                                              
044100     05  FILLER                  PIC X.                                   
044200                                                                          
044300*01  -COPY W0008  -PRE WDA9-                                              
044400     05  FILLER                  PIC X.                                   
044500                                                                          
044600*01  -COPY W0008  -PRE WDA9A-                                             
044700     05  FILLER                  PIC X.                                   
044800                                                                          
044900*01  -COPY W0008  -PRE 3151-                                              
045000     05  FILLER                  PIC X.                                   
045100                                                                          
045200*01  -COPY W0008  -PRE 3161-                                              
045300     05  FILLER                  PIC X.                                   
045400                                                                          
045500*01  -COPY W0008  -PRE WDD9-                                              
045600     05  FILLER                  PIC X.                                   
045700                                                                          
045800*01  -COPY W0008  -PRE WDK6-                                              
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB USEA-PCB WDD3-PCB         
037100     WDK7-PCB WDA9-PCB WDA9A-PCB 3151-PCB 3161-PCB WDD9-PCB               
037110     WDK6-PCB.                                                            
046300 MAIN SECTION.                                                            
046400     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB USEA-PCB WDD3-PCB         
037400           WDK7-PCB WDA9-PCB WDA9A-PCB 3151-PCB 3161-PCB WDD9-PCB         
037410     WDK6-PCB.                                                            
046600                                                                          
046700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
046800     IF SUB-KDRC = 0                                                      
046900       PERFORM A-INIT                                                     
047000       PERFORM B-CHECK-KEYS                                               
047100       IF KEYS-OK                                                         
047200         IF ORDER-PARTS                                                   
047300           PERFORM G-CHECK-UPDATE                                         
047400           IF KEYS-OK                                                     
047500             PERFORM H-UPDATE                                             
047600           END-IF                                                         
047700         END-IF                                                           
047800         IF SELECT-PARTS                                                  
047900           PERFORM F-READ-SHOW-INFO                                       
048000         END-IF                                                           
048100         IF CREATE-CSV-FILE                                               
048200           PERFORM F-READ-SHOW-INFO                                       
048300         END-IF                                                           
048400       END-IF                                                             
048500       PERFORM S02-RETURN-RESPONSE                                        
048600     END-IF                                                               
048700                                                                          
048800     PERFORM Z-FINIT                                                      
048900     MOVE ZERO                   TO RETURN-CODE                           
049000     GOBACK                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 A-INIT SECTION.                                                          
049400                                                                          
049500     MOVE ALL '+'                TO MOD-W30180O1                          
049600     INITIALIZE MOD-MODPREF                                               
049700     MOVE MID-KDPGMACT           TO MOD-KDPGMACT                          
049800     MOVE MID-KDDIASTATE         TO MOD-KDDIASTATE                        
049900     MOVE SPACE                  TO MOD-TEWEBERR                          
050000                                    MOD-TEWEBINF                          
050100     MOVE MID-IDDISTR            TO MOD-IDDISTR                           
050200     MOVE MID-KVRADER            TO MOD-KVRADER                           
050300                                                                          
050400     IF MID-IDSPRAK = 'SV'                                                
050500       MOVE 'S'                  TO W-IDSKYLT                             
050600     ELSE                                                                 
050700       MOVE 'GB'                 TO W-IDSKYLT                             
050800     END-IF                                                               
050900                                                                          
051000     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
051100     CALL WDATKONV            USING DAT-KDDATFORM                         
051200                                    DAT-I-TIDATUM                         
051300                                    DAT-O-TIDATUM                         
051400                                    DAT-KDSVAR                            
051500                                                                          
051600     MOVE DAT-TIAAVV-GRP         TO WS-CURR-WEEK-AAVV                     
051700     MOVE DAT-TISEKEL            TO WS-CURR-WEEK-CC                       
051800                                    WS-END-WEEK-CC                        
051900                                                                          
052000     MOVE WS-CURR-WEEK-AAVV      TO W009VADD-AAVV                         
052100     MOVE +8                     TO W009VADD-ANTAL                        
052200     CALL W009VADD            USING W009VADD-AAVV                         
052300                                    W009VADD-ANTAL                        
052400     MOVE W009VADD-AAVV          TO WS-END-WEEK-AAVV                      
052500                                                                          
052600     MOVE FUNCTION CURRENT-DATE(1:8)                                      
052700                                 TO WS-CURRENT-DATE                       
052800     MOVE FUNCTION CURRENT-DATE(9:8)                                      
052900                                 TO WS-CURRENT-TIME                       
053000     .                                                                    
053100     EJECT                                                                
053200 B-CHECK-KEYS SECTION.                                                    
053300                                                                          
043910     MOVE ALL '+'             TO MSGI-WMSGINIT                            
043920     MOVE '001'               TO MSGI-KDCALL                              
043930     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
043940     MOVE MID-IDUSER          TO MSGI-IDUSER                              
043950     MOVE '3180'              TO MSGI-IDTRANS                             
043960     MOVE MID-IDDISTR         TO MSGI-IDDISTR                             
053500                                                                          
043970     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043980                                                                          
044000     MOVE YES                    TO KEYS-SW                               
053600     IF MID-IDDISTR NOT = ALL '+'                                         
053700       INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                
053800       IF MID-IDDISTR NUMERIC                                             
053900         MOVE MID-IDDISTR        TO TEST-IDDISTR                          
054000         IF DIS134-BYTESRENOV                                             
054100           MOVE MID-IDDISTR      TO WS-IDDISTR-NUM                        
054200                                    WS-IDDISTR-WSEC                       
054300         ELSE                                                             
054400           MOVE REMAN-INVALID    TO MOD-TEWEBERR                          
054500           MOVE NOO              TO KEYS-SW                               
054600         END-IF                                                           
054700       ELSE                                                               
054800         MOVE REMAN-NOT-NUMERIC  TO MOD-TEWEBERR                          
054900         MOVE NOO                TO KEYS-SW                               
055000       END-IF                                                             
055100     ELSE                                                                 
055200       MOVE REMAN-NOT-ENTERED    TO MOD-TEWEBERR                          
055300       MOVE NOO                  TO KEYS-SW                               
055400     END-IF                                                               
055500                                                                          
055600     MOVE MID-IDUSER             TO SEC-IDUSER                            
055700     MOVE WS-TRANS               TO SEC-IDTRANS                           
055800     MOVE WS-IDDISTR-WSEC        TO SEC-IDKEY                             
055900     CALL WSECURIT            USING SEC-IDUSER                            
056000                                    SEC-IDTRANS                           
056100                                    SEC-IDKEY                             
056200                                    SEC-KDSVAR                            
056300     IF SEC-KDSVAR = 'F'                                                  
056400       MOVE NOT-AUTHORIZED       TO MOD-TEWEBERR                          
056500       MOVE NOO                  TO KEYS-SW                               
056600     END-IF                                                               
056700                                                                          
056800     IF KEYS-WRONG                                                        
056900       MOVE ZERO                 TO MOD-KVRADER                           
057000     ELSE                                                                 
057100       EVALUATE MID-KDPGMACT                                              
057200         WHEN 'S'                                                         
057300           SET SELECT-PARTS      TO TRUE                                  
057400         WHEN '3'                                                         
057500           SET SELECT-PARTS      TO TRUE                                  
057600         WHEN 'V'                                                         
057700           SET ORDER-PARTS       TO TRUE                                  
057800         WHEN 'E'                                                         
057900           SET CREATE-CSV-FILE   TO TRUE                                  
058000         WHEN OTHER                                                       
058100           MOVE NOO              TO KEYS-SW                               
058200           MOVE ZERO             TO MOD-KVRADER                           
058300       END-EVALUATE                                                       
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 F-READ-SHOW-INFO SECTION.                                                
058800                                                                          
058900     MOVE ZERO                   TO MOD-KVRADER                           
059000                                                                          
059100     MOVE MID-IDUSER             TO W-3152-IDUSER                         
059200     MOVE WS-IDDISTR-NUM         TO W-3152-IDDISTR                        
059300                                    W-IDDISTR-A9A-MIN                     
059400                                    W-IDDISTR-A9A-MAX                     
059500                                    W-IDDISTR                             
059600     PERFORM FA-CHECK-IN-PROCESS-ORDER                                    
059700                                                                          
059800     IF KEYS-OK                                                           
059900       MOVE 1                    TO IX                                    
060000                                                                          
060100       PERFORM IMS-GN-WDA9A                                               
060200       PERFORM                                                            
060300         UNTIL SEGMENT-MISSING OR END-OF-DATABASE                         
060400            OR IX > 1000                                                  
060500         MOVE SEQA-IDARTNR       TO W-IDARTNR                             
060600                                    W-IDARTNRO                            
060700                                    WS-IDARTNR                            
060800                                    TEST-IDARTNR                          
060900                                    WS-IDARTNR-NUM                        
061000                                                                          
061100         IF BYT01-BYTES                                                   
061200           IF BYT16-RADIO                                                 
061300             MOVE 3              TO WS-ARTSIFFRA                          
061400           ELSE                                                           
061500             IF ART-0                                                     
061600               MOVE 0            TO WS-ARTSIFFRA                          
061700             ELSE                                                         
061800               IF ART-1                                                   
061900                 MOVE 1          TO WS-ARTSIFFRA                          
062000               ELSE                                                       
062100                 IF ART-2                                                 
062200                   MOVE 2        TO WS-ARTSIFFRA                          
062300                 ELSE                                                     
062400                   IF ART-3                                               
062500                     MOVE 3      TO WS-ARTSIFFRA                          
062600                   END-IF                                                 
062700                 END-IF                                                   
062800               END-IF                                                     
062900             END-IF                                                       
063000           END-IF                                                         
063100         END-IF                                                           
063200                                                                          
063300         MOVE WS-IDARTNR         TO WS-IDARTNR-BYT                        
063400                                    W-IDARTNR-D9                          
063500                                    W-IDARTNR-K6                          
063600                                                                          
063700         PERFORM DB2-SELECT-BYART                                         
063800         IF ROW-FOUND AND                                                 
063900            WS-IDDISTR-NUM = BYART-IDDISTR-RENOV                          
064000           MOVE WS-IDARTNR-NUM   TO MOD-IDARTNR-OBJ (IX)                  
064100           MOVE BYART-KVBYTPKO   TO MOD-KVBYTPKO (IX)                     
064200                                                                          
064300           PERFORM IMS-GU-WDD3-BSEQ-BENA11                                
064400           IF SEGMENT-FOUND                                               
064500             MOVE TEXT-BEART     TO MOD-BEART (IX)                        
064600           ELSE                                                           
064700             MOVE SPACE          TO MOD-BEART (IX)                        
064800           END-IF                                                         
064900                                                                          
055200           MOVE MSGI-IDDC        TO W-IDDC                                
065100           PERFORM IMS-GU-WDK711                                          
065200           IF SEGMENT-FOUND                                               
065300             MOVE SLAG-KVLS      TO MOD-KVLS-91 (IX)                      
065400           ELSE                                                           
065500             MOVE ZERO           TO MOD-KVLS-91 (IX)                      
065600           END-IF                                                         
065700                                                                          
065800           PERFORM IMS-GU-WDA901                                          
065900           IF SEGMENT-FOUND                                               
066000             MOVE ZERO           TO WS-KVLS-REM                           
066100                                    WS-KVADV-REM                          
066200                                    WS-DAREGDAT                           
066300                                    WS-IDORDER                            
066400                                    WS-KVAVBART                           
066500                                    WS-TIREGTID                           
066600             PERFORM IMS-GNP-WDA911                                       
066700             IF SEGMENT-FOUND                                             
066800               MOVE UPD-IDDISTR  TO TEST-IDDISTR                          
066900               IF DIS134-BYTESREN-WEB                                     
067000                 ADD UPD-KVLS-REM                                         
067100                                 TO WS-KVLS-REM                           
067200                 MOVE UPD-IDDISTR                                         
067300                                 TO W-3161-IDDISTR                        
067400                 PERFORM FB-READ-3161                                     
067500               END-IF                                                     
067600             END-IF                                                       
067700                                                                          
067800             MOVE WS-KVLS-REM    TO MOD-KVLS-REM (IX)                     
068000             MOVE WS-KVADV-REM   TO MOD-KVANTAL-PREADV (IX)               
068100             IF WS-IDORDER > ZERO                                         
068200               MOVE WS-DAREGDAT  TO MOD-DAORDDAT (IX)                     
068300               MOVE WS-IDORDER   TO MOD-IDORDER  (IX)                     
068400               MOVE WS-KVAVBART  TO MOD-KVBEART  (IX)                     
068500             ELSE                                                         
068600               MOVE ALL-SPACE    TO MOD-DAORDDAT (IX)                     
068700                                    MOD-IDORDER  (IX)                     
068800                                    MOD-KVBEART  (IX)                     
068900             END-IF                                                       
069000           END-IF                                                         
069100                                                                          
069200           PERFORM IMS-GU-WDGX3154                                        
069300           IF SEGMENT-FOUND                                               
069400             MOVE 3154-KVBEART   TO MOD-KVBEART-UPD (IX)                  
069500           ELSE                                                           
069600             MOVE ZERO           TO MOD-KVBEART-UPD (IX)                  
069700           END-IF                                                         
069800                                                                          
069900           PERFORM FC-GET-CALLOFFS-ARREARS                                
070000           IF CREATE-CSV-FILE                                             
070100              PERFORM FD-CREATE-CSV-FILE                                  
070200           END-IF                                                         
070300                                                                          
070400           ADD +1                TO IX                                    
070500                                                                          
070600         END-IF                                                           
070700         PERFORM IMS-GN-WDA9A                                             
070800       END-PERFORM                                                        
070900       IF CREATE-CSV-FILE AND (NOT FIRST-LINE)                            
071000          PERFORM S13-SEND-CLOSE                                          
071100          MOVE CSV-FILE-STARTED TO MOD-TEWEBINF                           
071200       END-IF                                                             
071300       COMPUTE IX = IX - 1                                                
071400       MOVE IX                   TO MOD-KVRADER                           
071500       IF IX = ZERO                                                       
071600         MOVE NO-PART-CAN-BE-ORDERED                                      
071700                                 TO MOD-TEWEBINF                          
071800       END-IF                                                             
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 FA-CHECK-IN-PROCESS-ORDER SECTION.                                       
072300                                                                          
072400     PERFORM IMS-GU-WDGX3152                                              
072500                                                                          
072600     IF SEGMENT-FOUND                                                     
072700       IF 3152-FLKLAR = YES OR JA                                         
072800         MOVE PREV-ORDER-PENDING TO MOD-TEWEBERR                          
072900         MOVE NOO                TO KEYS-SW                               
073000       END-IF                                                             
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400 FB-READ-3161 SECTION.                                                    
073500                                                                          
073600     PERFORM IMS-GU-WDGX3161                                              
073700     IF SEGMENT-FOUND                                                     
073800       PERFORM                                                            
073900         UNTIL SEGMENT-MISSING                                            
074000         PERFORM IMS-GNP-WDGX3162                                         
074100         IF SEGMENT-FOUND                                                 
074200* GET ADVICED QTY                                                         
074300           IF 3162-IDARTNR = UPB-IDARTNR AND                              
074400              3162-IDUSER  = SPACE                                        
074500             ADD 3162-KVAVIS     TO WS-KVADV-REM                          
074600                                                                          
074700           END-IF                                                         
074800* GET THE LATEST ORDER DETAILS FOR THE GIVEN REMAN(IDDISTR)               
074900           IF 3162-IDARTNR = UPB-IDARTNR    AND                           
075000              UPD-IDDISTR  = WS-IDDISTR-NUM AND                           
075100              3162-IDUSER  > SPACE                                        
075200             IF WS-DAREGDAT = ZERO AND                                    
075300                3162-IDUSER > SPACE                                       
075400               MOVE 3162-DAREGDAT                                         
075500                                 TO WS-DAREGDAT                           
075600               MOVE 3162-IDORDER TO WS-IDORDER                            
075700               MOVE 3162-KVAVBART                                         
075800                                 TO WS-KVAVBART                           
075900               MOVE 3162-TIREGTID                                         
076000                                 TO WS-TIREGTID                           
076100             ELSE                                                         
076200               IF 3162-IDUSER > SPACE AND                                 
076300                  (3162-DAREGDAT > WS-DAREGDAT OR                         
076400                  (3162-DAREGDAT = WS-DAREGDAT AND                        
076500                   3162-TIREGTID > WS-TIREGTID))                          
076600                 MOVE 3162-DAREGDAT                                       
076700                                 TO WS-DAREGDAT                           
076800                 MOVE 3162-IDORDER                                        
076900                                 TO WS-IDORDER                            
077000                 MOVE 3162-KVAVBART                                       
077100                                 TO WS-KVAVBART                           
077200                 MOVE 3162-TIREGTID                                       
077300                                 TO WS-TIREGTID                           
077400               END-IF                                                     
077500             END-IF                                                       
077600           END-IF                                                         
077700         END-IF                                                           
077800       END-PERFORM                                                        
077900     END-IF                                                               
078000     .                                                                    
078100     EJECT                                                                
078200 FC-GET-CALLOFFS-ARREARS SECTION.                                         
078300                                                                          
078400     MOVE ZERO                   TO WS-KVAVROP                            
078500                                    WS-KVAVROP-GAM                        
078600                                                                          
078700     PERFORM IMS-GU-WDK601                                                
078800     IF SEGMENT-FOUND                                                     
078900       MOVE ART-IDLEVNR          TO W-IDLEVNR                             
079000       PERFORM IMS-GU-WDD902                                              
079100       IF SEGMENT-FOUND                                                   
079200         PERFORM IMS-GNP-WDD905                                           
079300         PERFORM                                                          
079400           UNTIL SEGMENT-MISSING                                          
079500           IF KDAVROP = 2                                                 
079600             IF DAAVROP-AVS < WS-CURR-WEEK                                
079700*               ARREARS, LESS THAN CURRENT WEEK                           
079800               COMPUTE WS-KVAVROP-GAM = WS-KVAVROP-GAM + KVAVROP          
079900             ELSE                                                         
080000               IF DAAVROP-AVS >  WS-CURR-WEEK AND                         
080100                  DAAVROP-AVS <= WS-END-WEEK                              
080200*                 CALL-OFFS, NEXT WEEK UNTIL 8 WEEKS                      
080300                 COMPUTE WS-KVAVROP = WS-KVAVROP + KVAVROP                
080400               END-IF                                                     
080500             END-IF                                                       
080600           END-IF                                                         
080700           PERFORM IMS-GNP-WDD905                                         
080800         END-PERFORM                                                      
080900       END-IF                                                             
081000     END-IF                                                               
081100     MOVE WS-KVAVROP             TO MOD-KVAVROP     (IX)                  
081200     MOVE WS-KVAVROP-GAM         TO MOD-KVAVROP-GAM (IX)                  
081300     .                                                                    
081400     EJECT                                                                
081500 FD-CREATE-CSV-FILE SECTION.                                              
081600     IF FIRST-LINE                                                        
081700        PERFORM S10-SEND-OPEN                                             
081800        PERFORM S11-PUT-HEADER                                            
081900        PERFORM S12-PUT-LINE                                              
082000        MOVE NOO TO FIRST-SW                                              
082100     END-IF                                                               
082200     MOVE MOD-IDDISTR             TO CSV-IDDISTR                          
082300     MOVE MOD-IDARTNR-OBJ (IX)    TO CSV-IDARTNR-OBJ                      
082400     MOVE MOD-BEART (IX)          TO CSV-BEART                            
082500     MOVE MOD-KVLS-91 (IX)        TO CSV-KVLS-91                          
082600     MOVE MOD-KVLS-REM (IX)       TO CSV-KVLS-REM                         
082700     MOVE MOD-KVBYTPKO (IX)       TO CSV-KVBYTPKO                         
082800     MOVE MOD-KVBEART-UPD (IX)    TO CSV-KVBEART-UPD                      
082900     MOVE MOD-KVANTAL-PREADV (IX) TO CSV-KVANTAL-PREADV                   
083000     MOVE MOD-KVAVROP (IX)        TO CSV-KVAVROP                          
083100     MOVE MOD-KVAVROP-GAM (IX)    TO CSV-KVAVROP-GAM                      
083200     MOVE MOD-IDORDER (IX)        TO CSV-IDORDER                          
083300     MOVE MOD-DAORDDAT (IX)       TO CSV-DAORDDAT                         
083400     MOVE MOD-KVBEART (IX)        TO CSV-KVBEART                          
083500     PERFORM S12-PUT-LINE                                                 
083600     .                                                                    
083700     EJECT                                                                
083800 G-CHECK-UPDATE SECTION.                                                  
083900                                                                          
084000     PERFORM                                                              
084100     VARYING IX FROM +1 BY +1                                             
084200       UNTIL IX > MID-KVRADER                                             
084300       IF MID-KVBEART-UPD (IX) NOT = ALL '+'                              
084400         INSPECT MID-KVBEART-UPD (IX) REPLACING                           
084500           LEADING SPACE BY ZERO                                          
084600         IF MID-KVBEART-UPD (IX) IS NUMERIC                               
084700           CONTINUE                                                       
084800         ELSE                                                             
084900           MOVE NOO              TO KEYS-SW                               
085000           MOVE QTY-NOT-NUMERIC  TO MOD-TEWEBERR                          
085100         END-IF                                                           
085200       END-IF                                                             
085300     END-PERFORM                                                          
085400                                                                          
085500     .                                                                    
085600     EJECT                                                                
085700 H-UPDATE SECTION.                                                        
085800                                                                          
085900     MOVE ZERO                   TO WS-PARTS-TO-ORDER                     
086000                                                                          
086100     MOVE MID-IDUSER             TO W-3152-IDUSER                         
086200     MOVE WS-IDDISTR-NUM         TO W-3152-IDDISTR                        
086300                                                                          
086400     PERFORM IMS-GHU-WDGX3152                                             
086500     IF SEGMENT-MISSING                                                   
086600       MOVE MID-IDUSER           TO 3152-IDUSER-OREG                      
086700       MOVE WS-IDDISTR-NUM       TO 3152-IDDISTR                          
086800       MOVE NOO                  TO 3152-FLKLAR                           
086900       MOVE WS-CURRENT-DATE-YYMMDD                                        
087000                                 TO 3152-TIREGDAT                         
087100       MOVE WS-CURRENT-TIME      TO 3152-TIKLOCK                          
087200       PERFORM IMS-ISRT-WDGX3152                                          
087300     END-IF                                                               
087400                                                                          
087500     PERFORM                                                              
087600     VARYING IX FROM +1 BY +1                                             
087700       UNTIL IX > MID-KVRADER                                             
087800       INSPECT MID-IDARTNR-OBJ (IX) REPLACING                             
087900         LEADING SPACE BY ZERO                                            
088000       MOVE MID-IDARTNR-OBJ (IX) TO WS-IDARTNR-NUM                        
088100       MOVE WS-IDARTNR-NUM       TO W-IDARTNRO                            
088200       MOVE MID-KVBEART-UPD (IX) TO WS-KVBEART-UPD-NUM                    
088300                                                                          
088400       PERFORM IMS-GHU-WDGX3154                                           
088500       EVALUATE TRUE                                                      
088600         WHEN SEGMENT-FOUND                                               
088700           PERFORM HA-UPDATE-3154                                         
088800         WHEN SEGMENT-MISSING AND                                         
088900              WS-KVBEART-UPD-NUM > ZERO                                   
089000           PERFORM HB-INSERT-3154                                         
089100       END-EVALUATE                                                       
089200     END-PERFORM                                                          
089300     IF WS-PARTS-TO-ORDER = ZERO                                          
089400       MOVE NO-PART-TO-ORDER     TO MOD-TEWEBINF                          
089500     END-IF                                                               
089600     .                                                                    
089700     EJECT                                                                
089800 HA-UPDATE-3154 SECTION.                                                  
089900                                                                          
090000     IF WS-KVBEART-UPD-NUM = 3154-KVBEART                                 
090100       IF 3154-KVBEART > ZERO                                             
090200         ADD +1                  TO WS-PARTS-TO-ORDER                     
090300       END-IF                                                             
090400     ELSE                                                                 
090500       IF 3154-KVBEART > ZERO AND                                         
090600          WS-KVBEART-UPD-NUM = ZERO                                       
090700         PERFORM IMS-DLET-WDGX3154                                        
090800       ELSE                                                               
090900         MOVE WS-KVBEART-UPD-NUM TO 3154-KVBEART                          
091000         PERFORM IMS-REPL-WDGX3154                                        
091100         ADD +1                  TO WS-PARTS-TO-ORDER                     
091200       END-IF                                                             
091300     END-IF                                                               
091400     .                                                                    
091500     EJECT                                                                
091600 HB-INSERT-3154 SECTION.                                                  
091700                                                                          
091800     MOVE WS-IDARTNR-NUM         TO 3154-IDARTNR-OBJ                      
091900     MOVE WS-KVBEART-UPD-NUM     TO 3154-KVBEART                          
092000     PERFORM IMS-ISRT-WDGX3154                                            
092100     ADD +1                      TO WS-PARTS-TO-ORDER                     
092200     .                                                                    
092300     EJECT                                                                
092400 Z-FINIT SECTION.                                                         
092500     CONTINUE                                                             
092600     .                                                                    
092700     EJECT                                                                
092800*    --- DISPATCHER SECTIONS                                              
092900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
093000                                                                          
093100     MOVE 'GETARG'               TO SUB-KDFUNC                            
093200     MOVE 'CARPARTS.NES.REMANORDER'          TO SUB-ADDISPABS             
093300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
093400                                                                          
093500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
093600                                                                          
093700     IF SUB-KDRC > 0                                                      
093800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
093900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
094000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
094100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
094200     END-IF                                                               
094300     .                                                                    
094400     SKIP3                                                                
094500 S02-RETURN-RESPONSE SECTION.                                             
094600                                                                          
094700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
094800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
094900                                                                          
095000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
095100                                                                          
095200     IF SUB-KDRC > 0                                                      
095300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
095400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
095500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
095600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
095700     END-IF                                                               
095800     .                                                                    
095900 S10-SEND-OPEN SECTION.                                                   
096000     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
096100     MOVE 'OPEN'                  TO SEND-KDFUNC                          
096200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
096300                                     SEND-OPEN-AREA                       
096400     IF SEND-KDRC > ZERO                                                  
096500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
096600       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
096700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
096800       DISPLAY ERROR-TEXT                                                 
096900       CALL FELLOG                                                        
097000     END-IF                                                               
097100     .                                                                    
097200     EJECT                                                                
097300 S11-PUT-HEADER SECTION.                                                  
097400     MOVE FUNCTION CURRENT-DATE(1:12)                                     
097500                TO WS-CURRENT-DATE-TIME                                   
097600     MOVE 1                       TO HDR-REQU-IDMSGVER                    
097700     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
097800     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
097900     MOVE 'W30180'                TO HDR-IDOUTTYPE                        
098000     MOVE WS-IDDISTR-WSEC         TO HDR-IDOUTREC                         
098100     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
098200     MOVE 'PUT'                   TO SEND-KDFUNC                          
098300     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
098400     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
098500                                     SEND-KVDLEN                          
098600                                     HDR-AREA                             
098700     IF SEND-KDRC > ZERO                                                  
098800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
098900       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
099000       DELIMITED BY SIZE       INTO ERROR-TEXT                            
099100       DISPLAY ERROR-TEXT                                                 
099200       CALL FELLOG                                                        
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 S12-PUT-LINE SECTION.                                                    
099700     IF FIRST-LINE                                                        
099800        MOVE 'PUT'                   TO SEND-KDFUNC                       
099900        MOVE LENGTH OF SEND-AREA-TO-CSV-TITLE TO SEND-KVDLEN              
100000        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
100100                                        SEND-KVDLEN                       
100200                                        SEND-AREA-TO-CSV-TITLE            
100300     ELSE                                                                 
100400        MOVE 'PUT'                   TO SEND-KDFUNC                       
100500        MOVE LENGTH OF SEND-AREA-TO-CSV-LINE TO SEND-KVDLEN               
100600        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
100700                                        SEND-KVDLEN                       
100800                                        SEND-AREA-TO-CSV-LINE             
100900     END-IF                                                               
101000     IF SEND-KDRC > ZERO                                                  
101100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
101200       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
101300       DELIMITED BY SIZE       INTO ERROR-TEXT                            
101400       DISPLAY ERROR-TEXT                                                 
101500       CALL FELLOG                                                        
101600     END-IF                                                               
101700     .                                                                    
101800     SKIP2                                                                
101900 S13-SEND-CLOSE SECTION.                                                  
102000     MOVE 'CLOSE'                TO SEND-KDFUNC                           
102100     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
102200     IF SEND-KDRC > 0                                                     
102300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
102400       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
102500       DELIMITED BY SIZE       INTO ERROR-TEXT                            
102600       DISPLAY ERROR-TEXT                                                 
102700       CALL FELLOG                                                        
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 IMS-GU-WDD3-BSEQ-BENA11   SECTION.                                       
103200                                                                          
103300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
103400             DELIMITED BY SIZE INTO SSA1                                  
103500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
103600             DELIMITED BY SIZE INTO SSA2                                  
103700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
103800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
103900     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
104000     PERFORM IMS-STATUSCHECK                                              
104100     .                                                                    
104200     EJECT                                                                
104300 IMS-GU-WDK711 SECTION.                                                   
104400                                                                          
104500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
104600             DELIMITED BY SIZE INTO SSA1                                  
104700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
104800             DELIMITED BY SIZE INTO SSA2                                  
104900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
105000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
105100     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
105200     PERFORM IMS-STATUSCHECK                                              
105300     .                                                                    
105400     EJECT                                                                
105500 IMS-GU-WDK601 SECTION.                                                   
105600                                                                          
105700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
105800             DELIMITED BY SIZE INTO SSA1                                  
105900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
106000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
106100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
106200     PERFORM IMS-STATUSCHECK                                              
106300     .                                                                    
106400     EJECT                                                                
106500 IMS-GU-WDD902 SECTION.                                                   
106600                                                                          
106700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
106800             DELIMITED BY SIZE INTO SSA1                                  
106900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
107000             DELIMITED BY SIZE INTO SSA2                                  
107100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
107200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
107300     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
107400     PERFORM IMS-STATUSCHECK                                              
107500     .                                                                    
107600     EJECT                                                                
107700 IMS-GNP-WDD905 SECTION.                                                  
107800                                                                          
107900     MOVE 'WDD905  '             TO SSA1                                  
108000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
108100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
108200     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
108300     PERFORM IMS-STATUSCHECK                                              
108400     .                                                                    
108500     EJECT                                                                
108600 IMS-GU-WDA901 SECTION.                                                   
108700     MOVE 'IMS-GU-WDA901   '     TO CURRENT-DB-SEC                        
108800                                                                          
108900     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-X ')'                         
109000             DELIMITED BY SIZE INTO SSA1                                  
109100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
109200     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
109300     MOVE WDA9-STATUS-CODE       TO STATUS-WS                             
109400     PERFORM IMS-STATUSCHECK                                              
109500     .                                                                    
109600     EJECT                                                                
109700 IMS-GNP-WDA911 SECTION.                                                  
109800     MOVE 'IMS-GNP-WDA911  '     TO CURRENT-DB-SEC                        
109900                                                                          
110000     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-X ')'                         
110100          DELIMITED BY SIZE INTO SSA1                                     
110200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
110300     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
110400     MOVE WDA9-STATUS-CODE       TO STATUS-WS                             
110500     PERFORM IMS-STATUSCHECK                                              
110600     .                                                                    
110700     EJECT                                                                
110800 IMS-GN-WDA9A SECTION.                                                    
110900     MOVE 'IMS-GN-WDA9A    '     TO CURRENT-DB-SEC                        
111000                                                                          
111100     STRING 'WDA9A1  (WDA9A1KY>=' W-WDA9A1KY-MIN-X                        
111200                    '&WDA9A1KY<=' W-WDA9A1KY-MAX-X ')'                    
111300             DELIMITED BY SIZE INTO SSA1                                  
111400     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
111500     CALL CBLTDLI USING GN WDA9A-PCB DLI-IO-WDA9A SSA1                    
111600     MOVE WDA9A-STATUS-CODE      TO STATUS-WS                             
111700     PERFORM IMS-STATUSCHECK                                              
111800     .                                                                    
111900     EJECT                                                                
112000 IMS-GU-WDGX3152 SECTION.                                                 
112100                                                                          
112200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
112300             DELIMITED BY SIZE INTO SSA1                                  
112400     STRING 'WDGX3152(KY3152   =' W-W3152KY-X ')'                         
112500             DELIMITED BY SIZE INTO SSA2                                  
112600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
112700     CALL CBLTDLI USING GU  3151-PCB DLI-IO-WDGX3152 SSA1 SSA2            
112800     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
112900     PERFORM IMS-STATUSCHECK                                              
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GHU-WDGX3152 SECTION.                                                
113300                                                                          
113400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
113500             DELIMITED BY SIZE INTO SSA1                                  
113600     STRING 'WDGX3152(KY3152   =' W-W3152KY-X ')'                         
113700             DELIMITED BY SIZE INTO SSA2                                  
113800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
113900     CALL CBLTDLI USING GHU 3151-PCB DLI-IO-WDGX3152 SSA1 SSA2            
114000     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
114100     PERFORM IMS-STATUSCHECK                                              
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-ISRT-WDGX3152 SECTION.                                               
114500                                                                          
114600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
114700             DELIMITED BY SIZE INTO SSA1                                  
114800     MOVE 'WDGX3152 '            TO SSA2                                  
114900     MOVE '  '                   TO GOOD-STATUSCODES                      
115000     CALL CBLTDLI USING ISRT 3151-PCB DLI-IO-WDGX3152 SSA1 SSA2           
115100     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
115200     PERFORM IMS-STATUSCHECK                                              
115300     .                                                                    
115400     EJECT                                                                
115500 IMS-GU-WDGX3154 SECTION.                                                 
115600                                                                          
115700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
115800             DELIMITED BY SIZE INTO SSA1                                  
115900     STRING 'WDGX3152(KY3152   =' W-W3152KY-X ')'                         
116000             DELIMITED BY SIZE INTO SSA2                                  
116100     STRING 'WDGX3154(IDARTNRO =' W-W3154KY-X ')'                         
116200             DELIMITED BY SIZE INTO SSA3                                  
116300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
116400     CALL CBLTDLI USING GU 3151-PCB DLI-IO-WDGX3154 SSA1 SSA2 SSA3        
116500     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
116600     PERFORM IMS-STATUSCHECK                                              
116700     .                                                                    
116800     EJECT                                                                
116900 IMS-GHU-WDGX3154 SECTION.                                                
117000                                                                          
117100     STRING 'WDGX3154(IDARTNRO =' W-W3154KY-X ')'                         
117200             DELIMITED BY SIZE INTO SSA1                                  
117300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
117400     CALL CBLTDLI USING GHU 3151-PCB DLI-IO-WDGX3154 SSA1                 
117500     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
117600     PERFORM IMS-STATUSCHECK                                              
117700     .                                                                    
117800     EJECT                                                                
117900 IMS-ISRT-WDGX3154 SECTION.                                               
118000                                                                          
118100     MOVE 'WDGX3154 '            TO SSA1                                  
118200     MOVE '  '                   TO GOOD-STATUSCODES                      
118300     CALL CBLTDLI USING ISRT 3151-PCB DLI-IO-WDGX3154 SSA1                
118400     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
118500     PERFORM IMS-STATUSCHECK                                              
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-REPL-WDGX3154 SECTION.                                               
118900                                                                          
119000     MOVE 'WDGX3154 '            TO SSA1                                  
119100     MOVE '  '                   TO GOOD-STATUSCODES                      
119200     CALL CBLTDLI USING REPL 3151-PCB DLI-IO-WDGX3154 SSA1                
119300     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
119400     PERFORM IMS-STATUSCHECK                                              
119500     .                                                                    
119600     EJECT                                                                
119700 IMS-DLET-WDGX3154 SECTION.                                               
119800                                                                          
119900     MOVE 'WDGX3154 '            TO SSA1                                  
120000     MOVE '  '                   TO GOOD-STATUSCODES                      
120100     CALL CBLTDLI USING DLET 3151-PCB DLI-IO-WDGX3154 SSA1                
120200     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
120300     PERFORM IMS-STATUSCHECK                                              
120400     .                                                                    
120500     EJECT                                                                
120600 IMS-GU-WDGX3161 SECTION.                                                 
120700                                                                          
120800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3161-X ')'                    
120900             DELIMITED BY SIZE INTO SSA1                                  
121000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
121100     CALL CBLTDLI USING GU 3161-PCB DLI-IO-WDGX3161 SSA1                  
121200     MOVE 3161-STATUS-CODE       TO STATUS-WS                             
121300     PERFORM IMS-STATUSCHECK                                              
121400     .                                                                    
121500     EJECT                                                                
121600 IMS-GNP-WDGX3162 SECTION.                                                
121700                                                                          
121800     MOVE 'WDGX3162 '            TO SSA1                                  
121900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
122000     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
122100     MOVE 3161-STATUS-CODE       TO STATUS-WS                             
122200     PERFORM IMS-STATUSCHECK                                              
122300     .                                                                    
122400     EJECT                                                                
122500 IMS-STATUSCHECK SECTION.                                                 
122600                                                                          
122700     SET STATUS-IX               TO 1                                     
122800     SEARCH GOOD-STATUS                                                   
122900       AT END                                                             
123000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
123100             DELIMITED BY SIZE INTO ERROR-TEXT                            
123200         CALL FELLOG                                                      
123300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
123400         CONTINUE                                                         
123500     END-SEARCH                                                           
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 DB2-SELECT-BYART SECTION.                                                
124000     MOVE 'DB2-SELECT-BYART'     TO CURRENT-DB-SEC                        
124100                                                                          
124200     MOVE 000100                 TO GOOD-SQLCODECODES                     
124300     EXEC SQL SELECT                                                      
124400                  IDDISTR_RENOV,                                          
124500                  KVBYTPKO                                                
124600              INTO                                                        
124700                  :BYART-IDDISTR-RENOV,                                   
124800                  :BYART-KVBYTPKO                                         
124900              FROM BYART                                                  
125000             WHERE IDARTNR_BYT = :WS-IDARTNR-BYT                          
125100     END-EXEC                                                             
125200     MOVE SQLCODE                TO SQLCODE-WS                            
125300     PERFORM DB2-STATUS-KONTROLL                                          
125400     .                                                                    
125500     EJECT                                                                
125600 DB2-STATUS-KONTROLL SECTION.                                             
125700                                                                          
125800     SET SQLCODE-IX              TO 1                                     
125900     SEARCH GOOD-SQLCODE                                                  
126000       AT END                                                             
126100         STRING ' INVALID RETURN CODE FROM DB2: ' SQLCODE-WS              
126200             DELIMITED BY SIZE INTO ERROR-TEXT                            
126300         CALL FELLOG                                                      
126400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
126500         CONTINUE                                                         
126600     END-SEARCH                                                           
126700     .                                                                    
