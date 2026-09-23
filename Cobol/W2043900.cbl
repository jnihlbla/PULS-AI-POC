000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2043900.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/09/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS SCREEN IS USED FOR SPLITTING OF CALL OFFS INTO              
000900*        DIFFERENT DAYS PER WEEK                                          
001000*                                                                         
001100*        THE PROGRAM READS     WDK6                                       
001200*        THE PROGRAM READS     WDF1                                       
001300*        THE PROGRAM READS     WDK7                                       
001400*        THE PROGRAM READS     WDF3                                       
001500*        THE PROGRAM READS     WDGX2206(WDR2)                             
001600*                                                                         
001700*        THE PROGRAM UPDATES   WDGX2248(WDR5)                             
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W2T439                                              
002100*        MID:         W2I43901                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W2O439N1                                            
002500*                                                                         
002600******************************************************************        
002700* PROGRAMÄNDRINGAR:                                                       
002800*    2013-11-07   E-TRACKER: 10205391  VECKO/PERIOD BATCH VIA BILD        
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900*    -COPY WY2000W3                                                       
004000     SKIP3                                                                
004100*    -COPY WY2000W6                                                       
004200     SKIP3                                                                
004300*    -COPY WY2000W7                                                       
004400     SKIP3                                                                
004500*    -COPY WY2000W9                                                       
004600     SKIP3                                                                
004700     EJECT                                                                
004800 77  IDPGM                       PIC X(08)   VALUE 'W2043900'.            
004900                                                                          
005000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  YES                         PIC X       VALUE 'J'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005800 77  MAX-PERIOD-TAB-INDX     PIC S9(9)   VALUE +2    COMP SYNC.           
005900                                                                          
006000*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006100                                                                          
006200                                                                          
006300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006400     88  KEYS-OK                             VALUE 'J'.                   
006500     88  KEYS-WRONG                          VALUE 'N'.                   
006600                                                                          
006700 77  DC-SW                       PIC X       VALUE 'J'.                   
006800     88  DC-OK                               VALUE 'J'.                   
006900     88  DC-WRONG                            VALUE 'N'.                   
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  XLAG-DATA-SW                PIC X       VALUE 'N'.                   
007600     88  XLAG-DATA-OK                        VALUE 'J'.                   
007700     88  XLAG-DATA-FEL                       VALUE 'N'.                   
007800                                                                          
007900 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
008000     88  PASSED-SEC-CHECK                    VALUE 'J'.                   
008100     88  BLOCKED-SEC-CHECK                   VALUE 'N'.                   
008200                                                                          
008300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008400     88  OWN-MID                             VALUE '2439'.                
008500     88  GOOD-MID                            VALUE '2431' '2432'          
008600                                                   '2433' '2434'          
008700                                                   '2435' '2436'          
008800                                                   '2437' '2438'          
008900                                                   '2439'.                
009000     88  HELP-MID                            VALUE '0551'.                
009100     EJECT                                                                
009200 01  WS-VARIABLES.                                                        
009300     03 WS-VALID-KDBEHX          PIC X(1)    VALUE SPACE.                 
009400        88  GOOD-KDBEHX                      VALUE ' ' 'V' '+'.           
009500     03 WS-IDLEVNR-8             PIC X(8)    VALUE SPACE.                 
009600     03 WS-IDARTNR               PIC X(9)    VALUE SPACE.                 
009700     03 WS-DASH                  PIC X(1)    VALUE '-'.                   
009800     03 W-SAVE-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009900     03 WS-IDLEVNR               PIC X(5)    VALUE SPACE.                 
010000     03 WS-PERIOD                PIC X(4)    VALUE SPACE.                 
010100     03 WS-KDBEHX-PLAN           PIC X(1)    VALUE SPACE.                 
010200     03 WS-IDLANDX2              PIC X(2)    VALUE SPACE.                 
010300     03 WS-CURRENT-DATE          PIC 9(8)    VALUE ZERO.                  
010400     03 WS-TILPSP                PIC 9(4)    VALUE ZERO.                  
010500     03 WS-KVDAGAR-TT            PIC S9(3)   VALUE ZERO COMP-3.           
010600     03 WS-KVDAGAR-INLEV         PIC S9(3)   VALUE ZERO COMP-3.           
010700     03 W-KVAVROP-RED            PIC Z(5)9   VALUE ZERO.                  
010800     03 W-START-PER-AAPP         PIC  9(4)   VALUE ZERO.                  
010900     03 W-START-PER-AA           PIC  9(2)   VALUE ZERO.                  
011000     03 W-START-PER-PP           PIC  9(2)   VALUE ZERO.                  
011100     03 W-WEEK                   PIC  9(2)   VALUE ZERO.                  
011200     03 WS-AAPP                  PIC 9(4)    VALUE ZERO.                  
011300     03 FILLER REDEFINES     WS-AAPP.                                     
011400        05 WS-AA                 PIC 9(2).                                
011500        05 WS-PP                 PIC 9(2).                                
011600     03 FILLER                   PIC X(16)   VALUE 'AVROP-TABLE'.         
011700     03 W-AVROP-TAB-RAD      OCCURS 2.                                    
011800         05  W-PERIOD-TAB-RAD    PIC 9(2).                                
011900         05  W-AVROP-TAB-KOL OCCURS 5.                                    
012000             10  W-KVAVROP-TAB   PIC S9(7) COMP-3.                        
012100     03  W-DAAVROP-AVS           PIC 9(6).                                
012200     03  FILLER  REDEFINES W-DAAVROP-AVS.                                 
012300         05  FILLER              PIC 9(2).                                
012400         05  W-TIAVROP-AVS       PIC 9(4).                                
012500     03  W-TIAAMMDD-AVS          PIC 9(6).                                
012600                                                                          
012700 01  INDEXES.                                                             
012800     03  IX                  PIC S9(9)               COMP SYNC.           
012900     03  IX1                 PIC S9(9)               COMP SYNC.           
013000     03  IX2                 PIC S9(9)               COMP SYNC.           
013100     03  IX3                 PIC S9(9)               COMP SYNC.           
013200     03  IX-VECKA            PIC S9(9)               COMP SYNC.           
013300     03  IX-PP               PIC S9(9)               COMP SYNC.           
013400     03  IY                  PIC S9(9)               COMP SYNC.           
013500     03  IX-AAPP             PIC  9(2).                                   
013600     03  DAYS-INDX           PIC  9(2).                                   
013700                                                                          
013800 01  PERIODINDELNING.                                                     
013900     03  PERIOD-TAB          OCCURS 2.                                    
014000         05  PER-CENTURY     PIC  9(2).                                   
014100         05  PER-START-AAVV  PIC  9(4).                                   
014200         05  FILLER REDEFINES PER-START-AAVV.                             
014300           07 PER-START-AA   PIC  9(2).                                   
014400           07 PER-START-VV   PIC  9(2).                                   
014500                                                                          
014600         05  PER-END-AAVV   PIC  9(4).                                    
014700         05  FILLER REDEFINES PER-END-AAVV.                               
014800           07 PER-END-AA    PIC  9(2).                                    
014900           07 PER-END-VV    PIC  9(2).                                    
015000                                                                          
015100         05  PER-ANT-VV      PIC  9(1).                                   
015200                                                                          
015300         05  PER-AAPP        PIC 9(4).                                    
015400         05  FILLER          REDEFINES PER-AAPP.                          
015500             07  PER-AAPP-AA PIC 9(2).                                    
015600             07  PER-AAPP-PP PIC 9(2).                                    
015700*                                                                         
015800                                                                          
015900 01  WS-AAAAVV               PIC 9(6).                                    
016000 01  FILLER  REDEFINES WS-AAAAVV.                                         
016100     03  WS-CENTURY          PIC 9(2).                                    
016200     03  WS-AAVV             PIC 9(4).                                    
016300     EJECT                                                                
016310 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
016320*01 -COPY WWIDFTG                                                         
016330     EJECT                                                                
016400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016500 01  GENERAL-SUBPROGRAMS.                                                 
016600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
017300     EJECT                                                                
017400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
017500*01 -COPY WMEDAREA                                                        
017600     SKIP3                                                                
017700*01  -COPY WORKAREA                                                       
017800                                                                          
017900 01  MESSAGE-CODES.                                                       
018000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
018100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018400     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
018500     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
018600                                                                          
018700     03  ERR-CORR-HILITE-FIELDS  PIC X(3)    VALUE '001'.                 
018800     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
018900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019000     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
019100     03  ERR-PART-EXPIRED        PIC X(3)    VALUE '018'.                 
019200     03  ERR-FIELD-NOT-NUMERIC   PIC X(3)    VALUE '020'.                 
019300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019400     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
019500     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
019600     03  ERR-NOT-LOCAL-SOURCED   PIC X(3)    VALUE '433'.                 
019700     03  ERR-PART-REPLACED       PIC X(30)                                
019800                         VALUE 'PART REPLACED'.                           
019900     03  ERR-NO-CALL-ON-EMPTY-WK PIC X(30)                                
020000                         VALUE 'NO CALL ON EMPTY WEEK'.                   
020100     03  ERR-INVALID-PERIOD      PIC X(30)                                
020200                         VALUE 'INVALID PERIOD'.                          
020300     03  ERR-CALL-ORDERED        PIC X(30)                                
020400                         VALUE 'CALL ORDERED (CHANGE IN 2303)'.           
020500     03  ERR-NON-DISPATCH-DAY    PIC X(30)                                
020600                         VALUE 'CALL ON NON-DISPATCH-DAY'.                
020700     03  ERR-SUPPLIER-HOLIDAY    PIC X(30)                                
020800                         VALUE 'CALL ON SUPPLIER HOLIDAY'.                
020900     EJECT                                                                
021000*01  -COPY WDATAREA                                                       
021100     EJECT                                                                
021200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
021300*                                                                         
021400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
021500     SKIP3                                                                
021600*01 -COPY WMSGINIT                                                        
021700     EJECT                                                                
021800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022100     SKIP3                                                                
022200*01  MID -COPY W2I43901    -PRE MID-.                                     
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022500     SKIP3                                                                
022600*01  -COPY WMSGAREA                                                       
022700     EJECT                                                                
022800     03  MOD REDEFINES MSG-AREA.                                          
022900*      05  -COPY W2O43901  -PRE MOD-.                                     
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023200     SKIP3                                                                
023300*01  -COPY WMFSAREA                                                       
023400     EJECT                                                                
023500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
023600*                                                                         
023700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023800     SKIP3                                                                
023900 01  KEYS-FOR-DLI.                                                        
024000     03  W-IDARTNR-X.                                                     
024100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024200     03  W-IDDC-X.                                                        
024300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024400     03  W-IDLAND-X.                                                      
024500         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
024600     03  W-IDLEVNR-X.                                                     
024700         05  W-IDLEVNR           PIC X(5).                                
024800     03  W-IDLEVNR-SHIP-X.                                                
024900         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
025000     03  W-KDAVROP-X.                                                     
025100         05  W-KDAVROP           PIC S9(1)   COMP-3 VALUE 2.              
025200     03  W-WDD901KY-X.                                                    
025300         05 W-IDARTNR-D9         PIC S9(9)   VALUE ZERO COMP-3.           
025400         05 W-IDDC-D9            PIC X(2)    VALUE SPACE.                 
025500     03  W-WDD905KY-X.                                                    
025600         05  W-DAAVROP-X.                                                 
025700             07  W-DAAVROP       PIC  9(6)   VALUE ZERO.                  
025800             07  FILLER REDEFINES W-DAAVROP.                              
025900                 09  W-DAAVROP-SS                                         
026000                                 PIC  9(2).                               
026100                 09  W-DAAVROP-AAVV                                       
026200                                 PIC  9(4).                               
026300         05  W-TILEVDAG-X.                                                
026400             07  W-TILEVDAG      PIC  S9     VALUE ZERO COMP-3.           
026500     03  W-IDSKYLT-X.                                                     
026600         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
026700     03  W-WDF301KY-X.                                                    
026800         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
026900         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
027000         05  FILLER  REDEFINES W-DADATUM-HELG.                            
027100             07  W-DADATUM-HELG-SS                                        
027200                                 PIC 9(2).                                
027300             07  W-DADATUM-HELG-AAMMDD                                    
027400                                 PIC 9(6).                                
027500     03  W-WDGXKEY-2247-X.                                                
027600         05  W-IDHTYP            PIC X(04)   VALUE '2247'.                
027700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
027800                                                                          
027900     03  W-WDGXKEY-2247-P-X.                                              
028000         05  W-IDHTYP            PIC X(04)   VALUE '2247'.                
028100         05  W-FLLEVPLP          PIC X(1)    VALUE 'J'.                   
028200         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
028300                                                                          
028400     03  W-WDGXKEY-2205-X.                                                
028500         05  W-IDHTYP            PIC X(04)    VALUE '2205'.               
028600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
028700     03  W-KY2206-X.                                                      
028800         05  W-IDLEVNR-2206      PIC X(5)     VALUE SPACE.                
028900         05  W-IDDC-2206         PIC X(2)     VALUE SPACE.                
029000     03  W-KDSEGKEY-K722-X.                                               
029100         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
029200     SKIP2                                                                
029300*    --- STATUS CODES FROM IMS                                            
029400 01  STATUS-WS                   PIC XX.                                  
029500     88  SEGMENT-FOUND                       VALUE '  '.                  
029600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
029700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
029800     SKIP2                                                                
029900 01  GOOD-STATUSCODES.                                                    
030000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030100     SKIP3                                                                
030200 01  ALL-SSA.                                                             
030300     03 SSA1                     PIC X(64).                               
030400     03 SSA2                     PIC X(64).                               
030500     03 SSA3                     PIC X(64).                               
030600     EJECT                                                                
030700*    --- IMS FUNCTION CODES                                               
030800*01  -COPY W0003                                                          
030900     EJECT                                                                
031000*    ---  DLI INPUT-OUTPUT AREA                                           
031100                                                                          
031200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
031300 01  DLI-IO-WDK601.                                                       
031400*    03  -COPY WDK601                                                     
031500     EJECT                                                                
031600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
031700 01  DLI-IO-WDF101.                                                       
031800*    03  -COPY WDF101                                                     
031900     EJECT                                                                
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
032100 01  DLI-IO-WDF106.                                                       
032200*    03  -COPY WDF106                                                     
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
032400 01  DLI-IO-WDF116.                                                       
032500*    03  -COPY WDF116                                                     
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
032700 01  DLI-IO-WDK701.                                                       
032800*    03  -COPY WDK701                                                     
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
033100 01  DLI-IO-WDK711.                                                       
033200*    03  -COPY WDK711                                                     
033300     EJECT                                                                
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
033500 01  DLI-IO-WDK712.                                                       
033600*    03  -COPY WDK712                                                     
033700     EJECT                                                                
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
033900 01  DLI-IO-WDK722.                                                       
034000*    03  -COPY WDK722                                                     
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
034200 01  DLI-IO-WDK723.                                                       
034300*    03  -COPY WDK723                                                     
034400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF301'.                      
034500 01  DLI-IO-WDF301.                                                       
034600*    03  -COPY WDF301                                                     
034700     EJECT                                                                
034800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF311'.                      
034900 01  DLI-IO-WDF311.                                                       
035000*    03  -COPY WDF311                                                     
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
035200 01  DLI-IO-WDD311.                                                       
035300*    03  -COPY WDD311                                                     
035400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
035500 01  DLI-IO-WDB601.                                                       
035600*    03  -COPY WDB601                                                     
035700     EJECT                                                                
035800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
035900 01  DLI-IO-WDGX2248.                                                     
036000*    03  -COPY WDGX2248                                                   
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
036200 01  DLI-IO-WDGX2206.                                                     
036300*    03  -COPY WDGX2206                                                   
036400     EJECT                                                                
036500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
036600 01  DLI-IO-WDD901.                                                       
036700*    03  -COPY WDD901                                                     
036800     EJECT                                                                
036900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
037000 01  DLI-IO-WDD902.                                                       
037100*    03  -COPY WDD902                                                     
037200     EJECT                                                                
037300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
037400 01  DLI-IO-WDD904.                                                       
037500*    03  -COPY WDD904                                                     
037600     EJECT                                                                
037700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
037800 01  DLI-IO-WDD905.                                                       
037900*    03  -COPY WDD905                                                     
038000     EJECT                                                                
038100 LINKAGE SECTION.                                                         
038200*01  -COPY W0009   -PRE MSG-                                              
038300*01  -COPY W0008   -PRE WDP7-                                             
038400     05  FILLER                  PIC X.                                   
038500                                                                          
038600*01  -COPY W0008  -PRE WDK6-                                              
038700     05  FILLER                  PIC X.                                   
038800                                                                          
038900*01  -COPY W0008  -PRE WDF1-                                              
039000     05  FILLER                  PIC X.                                   
039100                                                                          
039200*01  -COPY W0008  -PRE WDK7-                                              
039300     05  FILLER                  PIC X.                                   
039400                                                                          
039500*01  -COPY W0008  -PRE WDF3-                                              
039600     05  FILLER                  PIC X.                                   
039700                                                                          
039800*01  -COPY W0008  -PRE WDD3-                                              
039900     05  FILLER                  PIC X.                                   
040000                                                                          
040100*01  -COPY W0008  -PRE WDB6-                                              
040200     05  FILLER                  PIC X.                                   
040300                                                                          
040400*01  -COPY W0008  -PRE WDR5-                                              
040500     05  FILLER                  PIC X.                                   
040600                                                                          
040700*01  -COPY W0008  -PRE WDR2-                                              
040800     05  FILLER                  PIC X.                                   
040900                                                                          
041000*01  -COPY W0008  -PRE WDD9-                                              
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDF1-PCB             
041400                           WDK7-PCB WDF3-PCB WDD3-PCB WDB6-PCB            
041500                           WDR5-PCB WDR2-PCB WDD9-PCB.                    
041600 MAIN SECTION.                                                            
041700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDF1-PCB             
041800                           WDK7-PCB WDF3-PCB WDD3-PCB WDB6-PCB            
041900                           WDR5-PCB WDR2-PCB WDD9-PCB.                    
042000                                                                          
042100     PERFORM IMS-GET-MSG                                                  
042200     IF SEGMENT-FOUND                                                     
042300       PERFORM A-INIT                                                     
042400       PERFORM B-CHECK-KEYS                                               
042500       IF KEYS-OK AND INDATA-OK                                           
042600         PERFORM S10-AUTH-USER-CHECK                                      
042700         IF INDATA-OK                                                     
042800           IF MFS-UPDATE                                                  
042900             PERFORM G-CHECK-INPUT                                        
043000             IF INDATA-OK                                                 
043100               PERFORM H-UPDATE                                           
043200             END-IF                                                       
043300           ELSE                                                           
043400             IF MFS-FIRST                                                 
043500               PERFORM C-FIRST-PAGE                                       
043600             ELSE                                                         
043700               PERFORM E-SAME-PAGE                                        
043800             END-IF                                                       
043900           END-IF                                                         
044000           IF INDATA-OK                                                   
044100             PERFORM F-READ-SHOW-INFO                                     
044200           END-IF                                                         
044300*---                                                                      
044400*---       SAVE MFG SUPPLIER USING INIT-IO-AREA                           
044500           IF WS-IDLEVNR > SPACES                                         
044600              MOVE ALL '+'           TO MSGI-WMSGINIT                     
044700              MOVE '001'             TO MSGI-KDCALL                       
044800              MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                 
044900              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
045000              MOVE '2439'            TO MSGI-IDTRANS                      
045100              MOVE WS-IDLEVNR        TO MSGI-IDLEVNR                      
045200                                                                          
045300              CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                  
045400           END-IF                                                         
045500         END-IF                                                           
045600       END-IF                                                             
045700       PERFORM D-CLEAR-INVALID-WEEKS                                      
045800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
045900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O43901 + 4                      
046000       PERFORM IMS-INSERT-MSG                                             
046100     END-IF                                                               
046200                                                                          
046300     MOVE ZERO TO RETURN-CODE                                             
046400     GOBACK                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 A-INIT SECTION.                                                          
046800     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
046900                                                                          
047000     IF MSG-DOUBLE-TRANSACTIONS                                           
047100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I43901                 
047200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
047300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
047400     ELSE                                                                 
047500       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W2I43901                 
047600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
047700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
047800     END-IF                                                               
047900                                                                          
048000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
048200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048300                                                                          
048400     MOVE LOW-VALUE   TO MSG-AREA                                         
048500     MOVE 'W2O439N1'  TO MFS-IDMOD                                        
048600     MOVE '2439'      TO MOD-IDTRANS                                      
048700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
048800                                                                          
048900     IF OWN-MID OR HELP-MID                                               
049000       CONTINUE                                                           
049100     ELSE                                                                 
049200       MOVE SPACE TO MFS-KDTRTYP                                          
049300       MOVE '7' TO MFS-IDPFK                                              
049400     END-IF                                                               
049500                                                                          
049600     PERFORM AA-DAGENS-DATUM                                              
049700     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-CURRENT-DATE                  
049800     .                                                                    
049900     EJECT                                                                
050000 AA-DAGENS-DATUM SECTION.                                                 
050100     MOVE 'AA-DAGENS-DATUM ' TO CURRENT-SECTION                           
050200                                                                          
050300     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
050400                                                                          
050500     CALL WDATKONV USING DAT-KDDATFORM                                    
050600                         DAT-I-TIDATUM                                    
050700                         DAT-O-TIDATUM                                    
050800                         DAT-KDSVAR                                       
050900                                                                          
051000     MOVE DAT-TIAA           TO W-START-PER-AA                            
051100     MOVE DAT-TIRP           TO W-START-PER-PP                            
051200     MOVE DAT-TIAARP         TO W-START-PER-AAPP                          
051300     .                                                                    
051400     EJECT                                                                
051500 B-CHECK-KEYS SECTION.                                                    
051600     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
051700                                                                          
051800     MOVE ALL '+'             TO MSGI-WMSGINIT                            
051900     MOVE '001'               TO MSGI-KDCALL                              
052000     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
052100     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
052200     MOVE '2439'              TO MSGI-IDTRANS                             
052300     IF GOOD-MID                                                          
052400         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
052500         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
052600     ELSE                                                                 
052700         MOVE ALL '+'         TO MID-IDDC-IN                              
052800                                 MID-IDLEVNR-IN                           
052900                                 MID-KDBEHX-PLAN-IN                       
053000                                 MID-PERIOD-IN                            
053100         MOVE SPACE           TO MID-IDLEVNR-UT                           
053200                                 MID-KDBEHX-PLAN-UT                       
053300                                 MID-PERIOD-UT                            
053400     END-IF                                                               
053500     MOVE SPACES              TO MSGI-SPAR-AREA                           
053600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
053700     MOVE MSGI-IDDC-KEY       TO MID-IDDC-UT                              
053800                                                                          
053900*    - LANGUAGE TO BE USED BY MEDKONV                                     
054000     MOVE 'GB'                TO MED-IDSKYLT                              
054100                                                                          
054200     MOVE YES                 TO KEYS-SW                                  
054300                                                                          
054400                                                                          
054500*    -- CHECK OF IDARTNR                                                  
054600     MOVE MFS-ERASE-FIELD     TO MOD-IDARTNR-IN                           
054700                                                                          
054800     IF MSGI-IDARTNR = ALL '+' OR SPACE                                   
054900       MOVE 0                 TO WS-IDARTNR                               
055000       MOVE '7'               TO MFS-IDPFK                                
055100       MOVE SPACE             TO MFS-KDTRTYP                              
055200       MOVE MID-IDARTNR-IN    TO WS-IDARTNR                               
055300     ELSE                                                                 
055400       MOVE MSGI-IDARTNR      TO WS-IDARTNR                               
055500     END-IF                                                               
055600                                                                          
055700     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
055800     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
055900        MOVE WS-IDARTNR       TO W-IDARTNR                                
056000     ELSE                                                                 
056100        MOVE NOO              TO KEYS-SW                                  
056200     END-IF                                                               
056300                                                                          
056400* GET THE CONTROL DIGIT TO THE PART NUMBER                                
056500     PERFORM BA-CHECK-IDARTNR                                             
056600                                                                          
056700*    -- CHECK OF IDDC                                                     
056800     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
056900                                                                          
057000     IF MID-IDDC-IN = ALL '+' OR SPACE                                    
057100       IF MID-IDDC-UT = ALL '+' OR SPACE                                  
057200          MOVE SPACES       TO W-IDDC                                     
057300       ELSE                                                               
057400          MOVE MID-IDDC-UT  TO W-IDDC                                     
057500       END-IF                                                             
057600     ELSE                                                                 
057700       MOVE '7'             TO MFS-IDPFK                                  
057800       MOVE SPACE           TO MFS-KDTRTYP                                
057900       MOVE MID-IDDC-IN     TO W-IDDC                                     
058000     END-IF                                                               
058100                                                                          
058200     PERFORM BB-CHECK-IDDC                                                
058300                                                                          
058400     IF DC-WRONG                                                          
058500       MOVE ERR-WRONG-DC    TO MED-IDMFSFEL                               
058600       CALL WMEDKONV     USING MED-WMEDAREA                               
058700       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
058800       PERFORM MFS-ERASE-FIELD-IN                                         
058900       PERFORM MFS-ERASE-FIELD-OUT                                        
059000     END-IF                                                               
059100                                                                          
059200     IF KEYS-WRONG                                                        
059300       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
059400       CALL WMEDKONV     USING MED-WMEDAREA                               
059500       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
059600       PERFORM MFS-ERASE-FIELD-IN                                         
059700       PERFORM MFS-ERASE-FIELD-OUT                                        
059800     END-IF                                                               
059900                                                                          
060000     IF KEYS-WRONG AND (WS-IDARTNR = 0 AND W-IDDC = SPACES)               
060100       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
060200       CALL WMEDKONV     USING MED-WMEDAREA                               
060300       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
060400       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR                                
060500       PERFORM MFS-ERASE-FIELD-IN                                         
060600       PERFORM MFS-ERASE-FIELD-OUT                                        
060700     END-IF                                                               
060800                                                                          
060900     PERFORM BC-CHECK-KDBEHX                                              
061000                                                                          
061100     IF MID-IDLEVNR-IN = ALL '+'                                          
061200       IF MFS-QUERY AND MFS-FIRST                                         
061300          MOVE SPACE        TO WS-IDLEVNR                                 
061400          IF MFS-IDTRANS     = '2403'                                     
061500             MOVE MSGI-IDLEVNR                                            
061600                            TO WS-IDLEVNR                                 
061700          END-IF                                                          
061800       ELSE                                                               
061900          MOVE MID-IDLEVNR-UT                                             
062000                            TO WS-IDLEVNR                                 
062100       END-IF                                                             
062200     ELSE                                                                 
062300       MOVE MID-IDLEVNR-IN  TO WS-IDLEVNR                                 
062400     END-IF                                                               
062500                                                                          
062600     IF MID-PERIOD-IN = ALL '+'                                           
062700       MOVE MID-PERIOD-UT   TO WS-PERIOD                                  
062800     ELSE                                                                 
062900       MOVE MID-PERIOD-IN   TO WS-PERIOD                                  
063000     END-IF                                                               
063100     INSPECT WS-PERIOD REPLACING LEADING SPACE BY ZERO                    
063200                                                                          
063300     IF WS-PERIOD = ZERO                                                  
063400        MOVE W-START-PER-AAPP                                             
063500                            TO WS-PERIOD                                  
063600     END-IF                                                               
063700                                                                          
063800     MOVE W-IDARTNR         TO MOD-IDARTNR-UT                             
063900                               W-IDARTNR-D9                               
064000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
064100     MOVE W-IDDC            TO MOD-IDDC-UT                                
064200                               W-IDDC-D9                                  
064300     MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                             
064400     MOVE WS-KDBEHX-PLAN    TO MOD-KDBEHX-PLAN-UT                         
064500     MOVE WS-PERIOD         TO MOD-PERIOD-UT                              
064600                                                                          
064700     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
064800                               MOD-IDDC-IN                                
064900                               MOD-IDLEVNR-IN                             
065000                               MOD-KDBEHX-PLAN-IN                         
065100                               MOD-PERIOD-IN                              
065200     IF KEYS-OK                                                           
065300        PERFORM S20-PERIOD-TAB                                            
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
065700 BA-CHECK-IDARTNR SECTION.                                                
065800     MOVE 'BA-CHECK-IDARTNR' TO CURRENT-SECTION                           
065900                                                                          
066000     PERFORM IMS-GU-WDK601                                                
066100     IF SEGMENT-FOUND                                                     
066200        MOVE ART-REKSIFFR      TO MOD-REKSIFFR                            
066300        MOVE WS-DASH           TO MOD-DASH-1                              
066400     ELSE                                                                 
066500        MOVE NOO               TO INDATA-SW                               
066600        MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                            
066700        CALL WMEDKONV USING MED-WMEDAREA                                  
066800        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
066900        PERFORM MFS-ERASE-FIELD-IN                                        
067000        PERFORM MFS-ERASE-FIELD-OUT                                       
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 BB-CHECK-IDDC SECTION.                                                   
067500     MOVE 'BB-CHECK-IDDC   ' TO CURRENT-SECTION                           
067600                                                                          
067700     PERFORM IMS-GU-WDB601                                                
067800     IF SEGMENT-FOUND                                                     
067900        IF DCS-NDC-CN OR                                                  
068000          (DCS-NDC-NA AND DCS-USA)                                        
068100           MOVE YES   TO DC-SW                                            
068200        ELSE                                                              
068300           MOVE NOO   TO DC-SW                                            
068400                         INDATA-SW                                        
068500        END-IF                                                            
068600     ELSE                                                                 
068700        MOVE NOO      TO DC-SW                                            
068800                         INDATA-SW                                        
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 BC-CHECK-KDBEHX SECTION.                                                 
069300     MOVE 'BC-CHECK-KDBEHX ' TO CURRENT-SECTION                           
069400                                                                          
069500     MOVE MID-KDBEHX-PLAN-IN  TO WS-VALID-KDBEHX                          
069600     IF GOOD-KDBEHX                                                       
069700       IF MID-KDBEHX-PLAN-IN = ALL '+' OR SPACE                           
069800         MOVE MID-KDBEHX-PLAN-UT                                          
069900                              TO WS-KDBEHX-PLAN                           
070000                                 MOD-KDBEHX-PLAN-UT                       
070100       ELSE                                                               
070200         MOVE MID-KDBEHX-PLAN-IN                                          
070300                              TO WS-KDBEHX-PLAN                           
070400                                 MOD-KDBEHX-PLAN-UT                       
070500       END-IF                                                             
070600                                                                          
070700       IF WS-KDBEHX-PLAN = ALL '+' OR SPACE                               
070800          MOVE 'V'            TO MID-KDBEHX-PLAN-UT                       
070900                                 WS-KDBEHX-PLAN                           
071000       END-IF                                                             
071100                                                                          
071200     ELSE                                                                 
071300       MOVE NOO               TO INDATA-SW                                
071400       MOVE MID-KDBEHX-PLAN-IN                                            
071500                              TO WS-KDBEHX-PLAN                           
071600       MOVE MFS-ALPHA-FIELD-WRONG                                         
071700                              TO MOD-KDBEHX-PLAN-IN-ATTR                  
071800       MOVE ERR-CORR-HILITE-FIELDS                                        
071900                              TO MED-IDMFSFEL                             
072000       CALL WMEDKONV USING MED-WMEDAREA                                   
072100       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600 C-FIRST-PAGE SECTION.                                                    
072700     MOVE 'C-FIRST-PAGE    ' TO CURRENT-SECTION                           
072800                                                                          
072900     PERFORM MFS-ERASE-FIELD-IN                                           
073000     .                                                                    
073100     EJECT                                                                
073200 D-CLEAR-INVALID-WEEKS SECTION.                                           
073300     MOVE 'D-CLEAR-INV-WEEK' TO CURRENT-SECTION                           
073400                                                                          
073500     MOVE 1                  TO IX                                        
073600                                                                          
073700     PERFORM UNTIL IX > 2                                                 
073800                                                                          
073900       IF PER-ANT-VV (IX) < 5                                             
074000          MOVE MFS-CLOSE-FIELD-NOMOD                                      
074100                             TO MOD-KVAVROP-DAY-IN-ATTR (IX, 5, 1)        
074200                                MOD-KVAVROP-DAY-IN-ATTR (IX, 5, 2)        
074300                                MOD-KVAVROP-DAY-IN-ATTR (IX, 5, 3)        
074400                                MOD-KVAVROP-DAY-IN-ATTR (IX, 5, 4)        
074500                                MOD-KVAVROP-DAY-IN-ATTR (IX, 5, 5)        
074600          MOVE SPACE         TO MOD-KVAVROP-TAB (IX, 5)                   
074700       END-IF                                                             
074800       ADD 1                 TO IX                                        
074900     END-PERFORM                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 E-SAME-PAGE SECTION.                                                     
075300     MOVE 'E-SAME-PAGE     ' TO CURRENT-SECTION                           
075400                                                                          
075500     IF MID-INPUT = ALL '+'                                               
075600       PERFORM MFS-ERASE-FIELD-IN                                         
075700     ELSE                                                                 
075800       IF OWN-MID OR HELP-MID                                             
075900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
076000         CALL WMEDKONV USING MED-WMEDAREA                                 
076100         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
076200                                                                          
076300         PERFORM EA-MID-INDATA-TILL-MOD                                   
076400       ELSE                                                               
076500         PERFORM MFS-ERASE-FIELD-IN                                       
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900 EA-MID-INDATA-TILL-MOD SECTION.                                          
077000     MOVE 'EA-MID-TO-MOD   ' TO CURRENT-SECTION                           
077100                                                                          
077200     MOVE 1 TO IX1                                                        
077300     PERFORM UNTIL IX1 > 2                                                
077400        MOVE 1         TO IX2                                             
077500        PERFORM UNTIL IX2 > 5                                             
077600           MOVE 1      TO IX3                                             
077700           PERFORM UNTIL IX3 > 5                                          
077800              IF MID-KVAVROP-DAY (IX1, IX2, IX3) = ALL '+'                
077900                MOVE MFS-ERASE-FIELD                                      
078000                       TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)              
078100              ELSE                                                        
078200                MOVE MFS-ADD-READ-FIELD                                   
078300                       TO MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)         
078400                MOVE MFS-DO-NOT-TOUCH-FIELD                               
078500                       TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)              
078600              END-IF                                                      
078700              ADD 1    TO IX3                                             
078800           END-PERFORM                                                    
078900           ADD 1       TO IX2                                             
079000        END-PERFORM                                                       
079100        ADD 1          TO IX1                                             
079200     END-PERFORM                                                          
079300     .                                                                    
079400     EJECT                                                                
079500 F-READ-SHOW-INFO SECTION.                                                
079600     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
079700                                                                          
079800     MOVE WS-IDARTNR     TO W-IDARTNR                                     
079900     MOVE WS-IDLEVNR     TO W-IDLEVNR                                     
080000                                                                          
080100     PERFORM IMS-GU-WDK601                                                
080200     IF  SEGMENT-FOUND                                                    
080300       IF  ART-KDERS-UTG = ZERO                                           
080400         PERFORM FA-BUILD-WDK722-DATA                                     
080500         PERFORM FB-GET-DESC                                              
080600         PERFORM FC-FILL-PERIOD-DATA                                      
080700         IF WS-KDBEHX-PLAN = 'G'                                          
080800           MOVE MFS-OPEN-NUM-FIELD                                        
080900                           TO MOD-IDARTNR-IN-ATTR                         
081000         END-IF                                                           
081100       ELSE                                                               
081200         MOVE NOO                 TO INDATA-SW                            
081300         IF  ART-KDERS-UTG < 20                                           
081400           MOVE ERR-PART-EXPIRED  TO MED-IDMFSFEL                         
081500           CALL WMEDKONV       USING MED-WMEDAREA                         
081600           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
081700           PERFORM MFS-CLOSE-FIELD-IN                                     
081800           PERFORM MFS-ERASE-OUTPUT-FIELDS                                
081900         ELSE                                                             
082000           MOVE ERR-PART-REPLACED TO MOD-TEMFSFEL                         
082100         END-IF                                                           
082200       END-IF                                                             
082300     ELSE                                                                 
082400       MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                         
082500       CALL WMEDKONV           USING MED-WMEDAREA                         
082600       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
082700       PERFORM MFS-CLOSE-FIELD-IN                                         
082800       PERFORM MFS-ERASE-OUTPUT-FIELDS                                    
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 FA-BUILD-WDK722-DATA SECTION.                                            
083300     MOVE 'FA-BUILD-WDK722 ' TO CURRENT-SECTION                           
083400                                                                          
083500     PERFORM IMS-GU-WDK711                                                
083600     IF SEGMENT-FOUND                                                     
083700       IF WS-IDLEVNR = SPACE                                              
083800       OR SLAG-IDLEVNR = WS-IDLEVNR                                       
083900          MOVE SLAG-IDLEVNR      TO W-IDLEVNR                             
084000                                    MOD-IDLEVNR-UT                        
084100       ELSE                                                               
084200          IF SLAG-IDLEVNR NOT = WS-IDLEVNR                                
084300             PERFORM FAA-GET-WDK723-IDLEVNR                               
084400          END-IF                                                          
084500       END-IF                                                             
084600       PERFORM IMS-GU-WDK722                                              
084700       IF SEGMENT-FOUND                                                   
084800          IF WS-IDLEVNR = SPACE                                           
084900          OR SLAG-IDLEVNR = WS-IDLEVNR                                    
085000             MOVE XLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                
085100                                       W-IDLEVNR-SHIP                     
085200          END-IF                                                          
085300          MOVE XLAG-IDANSK       TO MOD-IDANSK                            
085400          MOVE XLAG-KVVECKOR-LT  TO MOD-KVVECKOR-LT                       
085500          MOVE XLAG-KDLPSP       TO MOD-KDLPSP                            
085600          MOVE XLAG-TILPSP       TO WS-TILPSP                             
085700          IF WS-TILPSP > ZERO                                             
085800             MOVE WS-TILPSP      TO MOD-TILPSP                            
085900          ELSE                                                            
086000             MOVE SPACE          TO MOD-TILPSP                            
086100          END-IF                                                          
086200          MOVE YES               TO XLAG-DATA-SW                          
086300       ELSE                                                               
086400          MOVE NOO               TO XLAG-DATA-SW                          
086500                                                                          
086600          MOVE ERR-NOT-LOCAL-SOURCED  TO MED-IDMFSFEL                     
086700          CALL WMEDKONV           USING MED-WMEDAREA                      
086800          MOVE MED-MFSFEL            TO MOD-TEMFSFEL                      
086900          PERFORM MFS-CLOSE-FIELD-IN                                      
087000          PERFORM MFS-ERASE-OUTPUT-FIELDS                                 
087100       END-IF                                                             
087200       PERFORM FAB-MOVE-DISPATCH-DAY-DATA                                 
087300     ELSE                                                                 
087400       MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                         
087500       CALL WMEDKONV           USING MED-WMEDAREA                         
087600       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
087700       PERFORM MFS-CLOSE-FIELD-IN                                         
087800       PERFORM MFS-ERASE-OUTPUT-FIELDS                                    
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 FAA-GET-WDK723-IDLEVNR SECTION.                                          
088300     MOVE 'FAA-K723-IDLEVNR' TO CURRENT-SECTION                           
088400                                                                          
088500     PERFORM IMS-GNP-WDK723                                               
088600     IF SEGMENT-FOUND                                                     
088700        IF WS-IDLEVNR = SAVT-IDLEVNR-AVT                                  
088800           MOVE SAVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                  
088900        ELSE                                                              
089000           MOVE WS-IDLEVNR       TO MOD-IDLEVNR-SHIP-UT                   
089100        END-IF                                                            
089200     ELSE                                                                 
089300        MOVE WS-IDLEVNR          TO MOD-IDLEVNR-SHIP-UT                   
089400     END-IF                                                               
089500     MOVE MOD-IDLEVNR-SHIP-UT    TO W-IDLEVNR-SHIP                        
089600     .                                                                    
089700     EJECT                                                                
089800 FAB-MOVE-DISPATCH-DAY-DATA SECTION.                                      
089900     MOVE 'FAB-DISP-DAY-DAT' TO CURRENT-SECTION                           
090000     MOVE SPACE             TO MOD-DISP-DAY (1)                           
090100                               MOD-DISP-DAY (2)                           
090200                               MOD-DISP-DAY (3)                           
090300                               MOD-DISP-DAY (4)                           
090400                               MOD-DISP-DAY (5)                           
090500                                                                          
090600     MOVE ZERO        TO DAYS-INDX                                        
090700     IF XLAG-DATA-OK                                                      
090800       IF XLAG-TILEVDAG (1) > ZERO                                        
090900          MOVE 'MO'     TO MOD-DISP-DAY (1)                               
091000          ADD +1        TO DAYS-INDX                                      
091100       END-IF                                                             
091200       IF XLAG-TILEVDAG (2) > ZERO                                        
091300          MOVE 'TU'     TO MOD-DISP-DAY (2)                               
091400          ADD +1        TO DAYS-INDX                                      
091500       END-IF                                                             
091600       IF XLAG-TILEVDAG (3) > ZERO                                        
091700          MOVE 'WE'     TO MOD-DISP-DAY (3)                               
091800          ADD +1        TO DAYS-INDX                                      
091900       END-IF                                                             
092000       IF XLAG-TILEVDAG (4) > ZERO                                        
092100          MOVE 'TH'     TO MOD-DISP-DAY (4)                               
092200          ADD +1        TO DAYS-INDX                                      
092300       END-IF                                                             
092400       IF XLAG-TILEVDAG (5) > ZERO                                        
092500          MOVE 'FR'     TO MOD-DISP-DAY (5)                               
092600          ADD +1        TO DAYS-INDX                                      
092700       END-IF                                                             
092800     END-IF                                                               
092900                                                                          
093000     IF DAYS-INDX = ZERO                                                  
093100        PERFORM IMS-GU-WDF116                                             
093200        IF SEGMENT-FOUND                                                  
093300           IF NDC-TILEVDAG (1) > ZERO                                     
093400              MOVE 'MO'     TO MOD-DISP-DAY (1)                           
093500           END-IF                                                         
093600           IF NDC-TILEVDAG (2) > ZERO                                     
093700              MOVE 'TU'     TO MOD-DISP-DAY (2)                           
093800           END-IF                                                         
093900           IF NDC-TILEVDAG (3) > ZERO                                     
094000              MOVE 'WE'     TO MOD-DISP-DAY (3)                           
094100           END-IF                                                         
094200           IF NDC-TILEVDAG (4) > ZERO                                     
094300              MOVE 'TH'     TO MOD-DISP-DAY (4)                           
094400           END-IF                                                         
094500           IF NDC-TILEVDAG (5) > ZERO                                     
094600              MOVE 'FR'     TO MOD-DISP-DAY (5)                           
094700           END-IF                                                         
094800        END-IF                                                            
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 FB-GET-DESC SECTION.                                                     
095300     MOVE 'FB-GET-DESC     ' TO CURRENT-SECTION                           
095400     MOVE 'GB'          TO W-IDSKYLT                                      
095500     PERFORM IMS-GU-WDD311                                                
095600     IF SEGMENT-FOUND                                                     
095700        MOVE TEXT-BEART TO MOD-BEART-ENG                                  
095800     ELSE                                                                 
095900        MOVE SPACES     TO MOD-BEART-ENG                                  
096000     END-IF                                                               
096100     .                                                                    
096200     EJECT                                                                
096300 FC-FILL-PERIOD-DATA SECTION.                                             
096400     MOVE 'FC-FILL-PERIOD  ' TO CURRENT-SECTION                           
096500                                                                          
096600     PERFORM FCA-INITIATE-TABLE                                           
096700     PERFORM IMS-GU-WDD902                                                
096800     IF SEGMENT-FOUND                                                     
096900        PERFORM IMS-GNP-WDD905                                            
097000        PERFORM UNTIL                                                     
097100        NOT ( SEGMENT-FOUND )                                             
097200         MOVE DAAVROP-AVS          TO W-DAAVROP-AVS                       
097300         MOVE W-TIAVROP-AVS        TO TMP1-YYWW                           
097400         MOVE PER-START-AAVV (1)   TO TMP2-YYWW                           
097500         MOVE PER-END-AAVV (2)     TO TMP3-YYWW                           
097600         PERFORM WY2000Q3                                                 
097700         IF  TMP1-YYWW >= TMP2-YYWW                                       
097800         AND TMP1-YYWW <= TMP3-YYWW                                       
097900         AND KDAVROP    = W-KDAVROP                                       
098000            PERFORM FCB-CALC-PERIOD-TABLE                                 
098100         END-IF                                                           
098200                                                                          
098300         PERFORM IMS-GNP-WDD905                                           
098400        END-PERFORM                                                       
098500        PERFORM FCC-MOVE-PERIOD-TO-MOD                                    
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 FCA-INITIATE-TABLE SECTION.                                              
099000     MOVE 'FCA-INIT-TABLE  ' TO CURRENT-SECTION                           
099100                                                                          
099200     MOVE PER-AAPP (1)        TO MOD-PERIOD-AARP (1)                      
099300     MOVE PER-AAPP (2)        TO MOD-PERIOD-AARP (2)                      
099400     MOVE 1                   TO IY                                       
099500                                                                          
099600     PERFORM UNTIL IY > MAX-PERIOD-TAB-INDX                               
099700       MOVE 1                 TO IX                                       
099800       MOVE PER-START-VV (IY) TO W-WEEK                                   
099900       PERFORM UNTIL W-WEEK > PER-END-VV (IY)                             
100000         MOVE W-WEEK          TO MOD-TIAVROP-AVS-TAB (IY, IX)             
100100         MOVE '-'             TO MOD-SIGN-TAB (IY, IX)                    
100200         MOVE ZERO            TO W-KVAVROP-RED                            
100300         MOVE W-KVAVROP-RED   TO MOD-KVAVROP-TAB (IY, IX)                 
100400         ADD 1                TO W-WEEK                                   
100500                                 IX                                       
100600       END-PERFORM                                                        
100700       ADD 1                  TO IY                                       
100800     END-PERFORM                                                          
100900                                                                          
101000     MOVE 1                   TO IY                                       
101100     PERFORM UNTIL IY > MAX-PERIOD-TAB-INDX                               
101200       MOVE 1                 TO IX                                       
101300       PERFORM UNTIL IX > 5                                               
101400         MOVE ZERO            TO W-KVAVROP-TAB (IY, IX)                   
101500         ADD 1                TO IX                                       
101600       END-PERFORM                                                        
101700       ADD 1                  TO IY                                       
101800     END-PERFORM                                                          
101900     .                                                                    
102000     EJECT                                                                
102100 FCB-CALC-PERIOD-TABLE SECTION.                                           
102200     MOVE 'FCB-CALC-PERIOD ' TO CURRENT-SECTION                           
102300                                                                          
102400     MOVE DAAVROP-AVS  TO W-DAAVROP-AVS                                   
102500     MOVE W-TIAVROP-AVS      TO TMP1-YYWW                                 
102600     MOVE PER-START-AAVV (1) TO TMP2-YYWW                                 
102700     MOVE PER-END-AAVV (1)  TO TMP3-YYWW                                  
102800     PERFORM WY2000Q3                                                     
102900     IF  TMP1-YYWW >= TMP2-YYWW                                           
103000     AND TMP1-YYWW <= TMP3-YYWW                                           
103100*                                                                         
103200*  PERIOD 1                                                               
103300*                                                                         
103400       MOVE W-TIAVROP-AVS (3:2)                                           
103500                             TO W-WEEK                                    
103600       COMPUTE IX-VECKA = W-WEEK - PER-START-VV (1) + 1                   
103700       MOVE KVAVROP                                                       
103800               TO MOD-KVAVROP-DAY-UT (1, IX-VECKA, TILEVDAG)              
103900       ADD KVAVROP     TO W-KVAVROP-TAB (1, IX-VECKA)                     
104000     ELSE                                                                 
104100*                                                                         
104200*  PERIOD 2                                                               
104300*                                                                         
104400       MOVE W-TIAVROP-AVS (3:2)                                           
104500                             TO W-WEEK                                    
104600       COMPUTE IX-VECKA = W-WEEK - PER-START-VV (2) + 1                   
104700       MOVE KVAVROP TO                                                    
104800                  MOD-KVAVROP-DAY-UT (2, IX-VECKA, TILEVDAG)              
104900       ADD KVAVROP     TO W-KVAVROP-TAB (2, IX-VECKA)                     
105000     END-IF                                                               
105100     .                                                                    
105200     EJECT                                                                
105300 FCC-MOVE-PERIOD-TO-MOD SECTION.                                          
105400     MOVE 'FCC-PER-TO-MOD  ' TO CURRENT-SECTION                           
105500                                                                          
105600     MOVE 1                          TO IY                                
105700                                                                          
105800     PERFORM UNTIL IY > MAX-PERIOD-TAB-INDX                               
105900       MOVE 1                        TO IX                                
106000       PERFORM UNTIL IX > 5                                               
106100         MOVE W-KVAVROP-TAB (IY, IX) TO W-KVAVROP-RED                     
106200         MOVE W-KVAVROP-RED          TO MOD-KVAVROP-TAB (IY, IX)          
106300         ADD  1                      TO IX                                
106400       END-PERFORM                                                        
106500       ADD 1                         TO IY                                
106600     END-PERFORM                                                          
106700     .                                                                    
106800     EJECT                                                                
106900 G-CHECK-INPUT SECTION.                                                   
107000     MOVE 'G-CHECK-INPUT   ' TO CURRENT-SECTION                           
107100                                                                          
107200     MOVE YES            TO INDATA-SW                                     
107201     PERFORM GA-AUTH-USER-CHECK                                           
107202     IF INDATA-OK                                                         
107210                                                                          
107300        MOVE WS-IDARTNR     TO W-IDARTNR                                  
107400        MOVE WS-IDLEVNR     TO W-IDLEVNR                                  
107500                                                                          
107600        PERFORM IMS-GU-WDK601                                             
107700        IF  SEGMENT-FOUND                                                 
107800          IF  ART-KDERS-UTG = ZERO                                        
107900            PERFORM IMS-GU-WDK711                                         
108000            IF SEGMENT-FOUND                                              
108100               CONTINUE                                                   
108200            ELSE                                                          
108300               MOVE NOO                   TO INDATA-SW                    
108400               MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                 
108500               CALL WMEDKONV           USING MED-WMEDAREA                 
108600               MOVE MED-MFSFEL            TO MOD-TEMFSFEL                 
108700            END-IF                                                        
108800          ELSE                                                            
108900            MOVE NOO            TO INDATA-SW                              
109000            IF ART-KDERS-UTG < 20                                         
109100              MOVE ERR-PART-EXPIRED       TO MED-IDMFSFEL                 
109200              CALL WMEDKONV            USING MED-WMEDAREA                 
109300              MOVE MED-MFSFEL             TO MOD-TEMFSFEL                 
109400            ELSE                                                          
109500              MOVE ERR-PART-REPLACED      TO MOD-TEMFSFEL                 
109600            END-IF                                                        
109700          END-IF                                                          
109800        ELSE                                                              
109900          MOVE NOO                        TO INDATA-SW                    
110000          MOVE ERR-PART-MISSING           TO MED-IDMFSFEL                 
110100          CALL WMEDKONV                USING MED-WMEDAREA                 
110200          MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                 
110300        END-IF                                                            
110400                                                                          
110500*CHECK IF PART NUM AND DC EXIST ON WDD901                                 
110600        PERFORM IMS-GU-WDD901                                             
110700        IF SEGMENT-MISSING                                                
110800           MOVE NOO                       TO INDATA-SW                    
110900           MOVE ERR-PART-MISSING          TO MED-IDMFSFEL                 
111000           CALL WMEDKONV               USING MED-WMEDAREA                 
111100           MOVE MED-MFSFEL                TO MOD-TEMFSFEL                 
111200        ELSE                                                              
111300           PERFORM IMS-GNP-WDD902                                         
111400           IF SEGMENT-MISSING                                             
111500             MOVE NOO                     TO INDATA-SW                    
111600             MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                 
111700             CALL WMEDKONV             USING MED-WMEDAREA                 
111800             MOVE MED-MFSFEL              TO MOD-TEMFSFEL                 
111900           END-IF                                                         
112000        END-IF                                                            
112100                                                                          
112200        IF INDATA-OK                                                      
112300          IF MID-INPUT = ALL '+' AND SEGMENT-FOUND                        
112400            MOVE ERR-PF11-AND-NO-DATA     TO MED-IDMFSFEL                 
112500            CALL WMEDKONV              USING MED-WMEDAREA                 
112600            MOVE MED-MFSFEL               TO MOD-TEMFSFEL                 
112700            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
112800            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
112900            MOVE NOO                      TO INDATA-SW                    
113000          ELSE                                                            
113100            IF INDATA-OK                                                  
113200              PERFORM GB-CHECK-DATAELEMENT                                
113300            END-IF                                                        
113400            IF INDATA-FEL                                                 
113500              PERFORM MFS-DONT-TOUCH-FIELD-IN                             
113600              PERFORM MFS-DONT-TOUCH-FIELD-OUT                            
113700            END-IF                                                        
113800          END-IF                                                          
113810       END-IF                                                             
113900     END-IF                                                               
114000     .                                                                    
114100     EJECT                                                                
114200                                                                          
114210 GA-AUTH-USER-CHECK SECTION.                                              
114220                                                                          
114280     PERFORM IMS-GU-WDB601                                                
114290     IF SEGMENT-FOUND                                                     
114291       IF DCS-NDC-CN                                                      
114292       OR (DCS-NDC-NA AND DCS-USA)                                        
114293         MOVE MSGI-IDFTG             TO WS-IDFTG                          
114294         IF (DCS-NDC-CN AND IDFTG-CN)                                     
114295         OR (DCS-NDC-NA AND IDFTG-US)                                     
114296         OR MSGI-IDFTG  = WC-IDFTG-PV                                     
114297            CONTINUE                                                      
114298         ELSE                                                             
114299            MOVE NOO                 TO INDATA-SW                         
114300            MOVE ERR-USER-NOT-AUTH   TO MED-IDMFSFEL                      
114301            CALL WMEDKONV         USING MED-WMEDAREA                      
114302            MOVE MED-MFSFEL          TO MOD-TEMFSFEL                      
114303            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
114304         END-IF                                                           
114305       ELSE                                                               
114306         MOVE NOO                    TO INDATA-SW                         
114307         MOVE ERR-USER-NOT-AUTH      TO MED-IDMFSFEL                      
114308         CALL WMEDKONV            USING MED-WMEDAREA                      
114309         MOVE MED-MFSFEL             TO MOD-TEMFSFEL                      
114310         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
114311       END-IF                                                             
114312     ELSE                                                                 
114313       MOVE NOO                      TO INDATA-SW                         
114314       MOVE ERR-USER-NOT-AUTH        TO MED-IDMFSFEL                      
114315       CALL WMEDKONV              USING MED-WMEDAREA                      
114316       MOVE MED-MFSFEL               TO MOD-TEMFSFEL                      
114317       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
114318     END-IF                                                               
114319     .                                                                    
114320     EJECT                                                                
114330 GB-CHECK-DATAELEMENT SECTION.                                            
114400     MOVE 'GB-CH-DATAELEMEN' TO CURRENT-SECTION                           
114500                                                                          
114600     MOVE 1          TO IX1                                               
114700     PERFORM UNTIL IX1 > 2                                                
114800       MOVE 1        TO IX2                                               
114900       PERFORM UNTIL IX2 > 5                                              
115000         MOVE 1      TO IX3                                               
115100         PERFORM UNTIL IX3 > 5                                            
115200           IF MID-KVAVROP-DAY (IX1, IX2, IX3) = ALL '+'                   
115300             MOVE MFS-ERASE-FIELD                                         
115400                     TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)                
115500           ELSE                                                           
115600             IF MID-KVAVROP-DAY (IX1, IX2, IX3) NUMERIC                   
115700                MOVE MID-KVAVROP-DAY (IX1, IX2, IX3)                      
115800                     TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)                
115900             ELSE                                                         
116000               MOVE NOO                        TO INDATA-SW               
116100               MOVE MFS-NUM-FIELD-WRONG                                   
116200                    TO MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)            
116300               MOVE ERR-FIELD-NOT-NUMERIC      TO MED-IDMFSFEL            
116400               CALL WMEDKONV                USING MED-WMEDAREA            
116500               MOVE MED-MFSFEL                 TO MOD-TEMFSFEL            
116600             END-IF                                                       
116700           END-IF                                                         
116800           ADD 1                               TO IX3                     
116900         END-PERFORM                                                      
117000         ADD 1                                 TO IX2                     
117100       END-PERFORM                                                        
117200       ADD 1                                   TO IX1                     
117300     END-PERFORM                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 H-UPDATE SECTION.                                                        
117700     MOVE 'H-UPDATE        ' TO CURRENT-SECTION                           
117800                                                                          
117900     MOVE W-IDLEVNR           TO W-SAVE-IDLEVNR                           
118000     PERFORM IMS-GU-WDK711                                                
118100     IF W-IDLEVNR = SLAG-IDLEVNR                                          
118200       PERFORM IMS-GNP-WDK722                                             
118300       MOVE XLAG-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                           
118400     ELSE                                                                 
118500       PERFORM IMS-GNP-WDK723                                             
118600       IF SEGMENT-FOUND                                                   
118700          IF W-IDLEVNR = SAVT-IDLEVNR-AVT                                 
118800             MOVE SAVT-IDLEVNR-SHIP                                       
118900                              TO W-IDLEVNR-SHIP                           
119000          ELSE                                                            
119100             MOVE W-IDLEVNR   TO W-IDLEVNR-SHIP                           
119200          END-IF                                                          
119300       ELSE                                                               
119400          MOVE W-IDLEVNR      TO W-IDLEVNR-SHIP                           
119500       END-IF                                                             
119600     END-IF                                                               
119700                                                                          
119800     MOVE W-SAVE-IDLEVNR      TO W-IDLEVNR                                
119900                                                                          
120000     PERFORM IMS-GU-WDD901                                                
120100     MOVE 1 TO IX1                                                        
120200     PERFORM UNTIL IX1 > 2                                                
120300        MOVE 1               TO IX2                                       
120400        MOVE PER-CENTURY (IX1)                                            
120500                             TO W-DAAVROP-SS                              
120600        MOVE PER-START-AAVV (IX1)                                         
120700                             TO W-DAAVROP-AAVV                            
120800        MOVE PER-START-VV (IX1)                                           
120900                             TO W-WEEK                                    
121000        PERFORM UNTIL W-WEEK > PER-END-VV (IX1)                           
121100           MOVE 1            TO IX3                                       
121200           PERFORM UNTIL IX3 > 5                                          
121300              IF MID-KVAVROP-DAY (IX1, IX2, IX3) = ALL '+'                
121400                CONTINUE                                                  
121500              ELSE                                                        
121600                MOVE IX3     TO W-TILEVDAG                                
121700                PERFORM IMS-GHNP-WDD905                                   
121800                PERFORM UNTIL SEGMENT-MISSING                             
121900                OR KDAVROP = W-KDAVROP                                    
122000                   PERFORM IMS-GHNP-WDD905                                
122100                END-PERFORM                                               
122200                IF SEGMENT-FOUND                                          
122300                   IF MID-KVAVROP-DAY (IX1, IX2, IX3) > ZERO              
122400                      MOVE MID-KVAVROP-DAY (IX1, IX2, IX3)                
122500                             TO KVAVROP                                   
122600                      PERFORM IMS-REPL-WDD905                             
122620                      PERFORM HB-CHECK-FOR-HOLIDAY                        
122700                   ELSE                                                   
122800                      PERFORM IMS-DLET-WDD905                             
122900                   END-IF                                                 
123000                ELSE                                                      
123100                   IF MID-KVAVROP-DAY (IX1, IX2, IX3) > ZERO              
123200                                                                          
123300                      PERFORM HA-ISRT-AVROP                               
123400                                                                          
123500                   END-IF                                                 
123600                END-IF                                                    
123700              END-IF                                                      
123800              ADD 1          TO IX3                                       
123900           END-PERFORM                                                    
124000           ADD 1             TO IX2                                       
124100                                W-WEEK                                    
124200                                W-DAAVROP-AAVV                            
124300        END-PERFORM                                                       
124400        ADD 1                TO IX1                                       
124500     END-PERFORM                                                          
124600                                                                          
124700     PERFORM S30-OTHER-UPDATES                                            
124800                                                                          
124900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
125000     CALL WMEDKONV     USING MED-WMEDAREA                                 
125100     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
125200     PERFORM MFS-FORM-ATTR                                                
125300     PERFORM MFS-ERASE-FIELD-IN                                           
125400     .                                                                    
125500     EJECT                                                                
125600*-----------------------------------------------------------------        
125700* INSERT WDD905 SEGMENT                                                   
125800*-----------------------------------------------------------------        
125900 HA-ISRT-AVROP SECTION.                                                   
126000     MOVE 'HA-ISRT-AVROP   ' TO CURRENT-SECTION                           
126100                                                                          
126200     MOVE W-KDAVROP           TO KDAVROP                                  
126300                                                                          
126400     MOVE W-DAAVROP           TO DAAVROP-AVS                              
126500                                                                          
126600     MOVE W-TILEVDAG          TO TILEVDAG                                 
126700                                                                          
126800     PERFORM HAA-CHECK-FOR-DISPATCH-DAY                                   
126900     PERFORM HAB-GET-TRNSPRT-DELIVRY-TIME                                 
127000                                                                          
127100*AVS-AAMMDD                                                               
127200     MOVE 'AAVVD'             TO DAT-KDDATFORM                            
127300     COMPUTE DAT-I-TIDATUM =  10 * W-DAAVROP-AAVV +                       
127400                                   W-TILEVDAG                             
127500     CALL WDATKONV USING      DAT-KDDATFORM                               
127600                              DAT-I-TIDATUM                               
127700                              DAT-O-TIDATUM                               
127800                              DAT-KDSVAR                                  
127900     MOVE DAT-TIAAMMDD        TO W-TIAAMMDD-AVS                           
128000                                 W-DADATUM-HELG-AAMMDD                    
128100*   AVROP-TIAVRDAT-INL                                                    
128200     MOVE 2                   TO WORK-KDCALL                              
128300     MOVE W-IDDC              TO WORK-IDDC                                
128400     MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                        
128500     MOVE WS-KVDAGAR-TT       TO WORK-KVWORKD                             
128600     ADD +1                   TO WORK-KVWORKD                             
128700     CALL WORKDAY USING  WORK-KDCALL                                      
128800          WORK-DATE-AREA WORK-KDSVAR                                      
128900     MOVE WORK-TIAAMMDD-TOM   TO TIAVRDAT-INL                             
129000*   AVROP-TIAVRDAT-DISP                                                   
129100     MOVE 2                   TO WORK-KDCALL                              
129200     MOVE W-IDDC              TO WORK-IDDC                                
129300     MOVE TIAVRDAT-INL        TO WORK-TIAAMMDD-FOM                        
129400     MOVE WS-KVDAGAR-INLEV    TO WORK-KVWORKD                             
129500     ADD +1                   TO WORK-KVWORKD                             
129600     CALL WORKDAY USING  WORK-KDCALL                                      
129700          WORK-DATE-AREA WORK-KDSVAR                                      
129800     MOVE WORK-TIAAMMDD-TOM   TO TIAVRDAT-DISP                            
129900                                                                          
130000     MOVE MID-KVAVROP-DAY (IX1, IX2, IX3)                                 
130100            TO KVAVROP                                                    
130200                                                                          
130300     PERFORM IMS-ISRT-WDD905                                              
130400                                                                          
130500* CHECK IF THERE WAS A HOLIDAY                                            
130600     PERFORM HAC-CHECK-FOR-HOLIDAY                                        
130700     .                                                                    
130800     EJECT                                                                
130900*-----------------------------------------------------------------        
131000* CHECK IF QUANTITY ADDED ON THE SCREEN HAS BEEN ADDED ON A               
131100* DISPATCH DAY                                                            
131200*-----------------------------------------------------------------        
131300 HAA-CHECK-FOR-DISPATCH-DAY SECTION.                                      
131400     MOVE 'HAA-CHK-DISPDAY ' TO CURRENT-SECTION                           
131500                                                                          
131600     PERFORM IMS-GU-WDK722                                                
131700     IF  XLAG-TILEVDAG(1) = ZERO                                          
131800     AND XLAG-TILEVDAG(2) = ZERO                                          
131900     AND XLAG-TILEVDAG(3) = ZERO                                          
132000     AND XLAG-TILEVDAG(4) = ZERO                                          
132100     AND XLAG-TILEVDAG(5) = ZERO                                          
132200                                                                          
132300        PERFORM IMS-GU-WDF116                                             
132400        IF SEGMENT-FOUND                                                  
132500           IF  NDC-TILEVDAG(1) = ZERO                                     
132600           AND NDC-TILEVDAG(2) = ZERO                                     
132700           AND NDC-TILEVDAG(3) = ZERO                                     
132800           AND NDC-TILEVDAG(4) = ZERO                                     
132900           AND NDC-TILEVDAG(5) = ZERO                                     
133000                                                                          
133100              CONTINUE                                                    
133200           ELSE                                                           
133300              IF NDC-TILEVDAG(IX3) > ZERO                                 
133400                 CONTINUE                                                 
133500              ELSE                                                        
133600                 MOVE ERR-NON-DISPATCH-DAY  TO MOD-TEMFSFEL               
133700                 MOVE MFS-ADD-HILIGHT-FIELD TO                            
133800                      MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)             
133900              END-IF                                                      
134000           END-IF                                                         
134100        END-IF                                                            
134200     ELSE                                                                 
134300        IF XLAG-TILEVDAG(IX3) > ZERO                                      
134400           CONTINUE                                                       
134500        ELSE                                                              
134600           MOVE ERR-NON-DISPATCH-DAY  TO MOD-TEMFSFEL                     
134700           MOVE MFS-ADD-HILIGHT-FIELD TO                                  
134800                MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)                   
134900        END-IF                                                            
135000     END-IF                                                               
135100     .                                                                    
135200     EJECT                                                                
135300*-----------------------------------------------------------------        
135400* DETERMINE THE TRANPORT TIME AND THE DELIVERY TIME                       
135500*-----------------------------------------------------------------        
135600 HAB-GET-TRNSPRT-DELIVRY-TIME SECTION.                                    
135700     MOVE 'HAB-TRP-DEL-TIME' TO CURRENT-SECTION                           
135800                                                                          
135900     PERFORM IMS-GU-WDF116                                                
136000     IF SEGMENT-FOUND                                                     
136100        MOVE NDC-KVDAGAR-TT     TO WS-KVDAGAR-TT                          
136200     END-IF                                                               
136300                                                                          
136400     MOVE DCS-IDLANDX2          TO W-IDLAND                               
136500     PERFORM IMS-GU-WDK712                                                
136600     MOVE LART-KVDAGAR-INLEV    TO WS-KVDAGAR-INLEV                       
136700     .                                                                    
136800     EJECT                                                                
136900*-----------------------------------------------------------------        
137000* CHECK IF QUANTITY ADDED ON THE SCREEN HAS BEEN ADDED ON A               
137100* SUPPLIER HOLIDAY                                                        
137200*-----------------------------------------------------------------        
137210 HAC-CHECK-FOR-HOLIDAY SECTION.                                           
137220     MOVE 'HAC-CHECK-FOR-HOLIDAY' TO CURRENT-SECTION                      
137230                                                                          
137240*      (IX1 PERIOD,  IX2 VECKA,   IX3 VECKODAG)                           
137250                                                                          
137260     MOVE SPACE                    TO WS-IDLANDX2                         
137270     PERFORM IMS-GU-WDF106                                                
137280     IF SEGMENT-FOUND                                                     
137290        MOVE ADR-IDLANDX2          TO WS-IDLANDX2                         
137291     END-IF                                                               
137292     MOVE WS-IDLANDX2              TO W-IDLANDX2                          
137293     MOVE 20                       TO W-DADATUM-HELG-SS                   
137294     PERFORM IMS-GU-WDF301                                                
137295     IF SEGMENT-FOUND                                                     
137296        MOVE ERR-SUPPLIER-HOLIDAY  TO MOD-TEMFSFEL                        
137297        MOVE MFS-ADD-HILIGHT-FIELD TO                                     
137298             MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)                      
137300     END-IF                                                               
137301     .                                                                    
137302     EJECT                                                                
137310 HB-CHECK-FOR-HOLIDAY SECTION.                                            
137400     MOVE 'HB-CHECK-FOR-HOLIDAY'   TO CURRENT-SECTION                     
137500                                                                          
137800     MOVE SPACE                    TO WS-IDLANDX2                         
137900     PERFORM IMS-GU-WDF106                                                
138000     IF SEGMENT-FOUND                                                     
138100        MOVE ADR-IDLANDX2          TO WS-IDLANDX2                         
138200     END-IF                                                               
138300     MOVE WS-IDLANDX2              TO W-IDLANDX2                          
138400     MOVE 20                       TO W-DADATUM-HELG-SS                   
138402                                                                          
138403     MOVE 'AAVVD'             TO DAT-KDDATFORM                            
138404     COMPUTE DAT-I-TIDATUM =  10 * DAAVROP-AVS + TILEVDAG                 
138405     CALL WDATKONV USING      DAT-KDDATFORM                               
138406                              DAT-I-TIDATUM                               
138407                              DAT-O-TIDATUM                               
138408                              DAT-KDSVAR                                  
138409     MOVE DAT-TIAAMMDD        TO W-TIAAMMDD-AVS                           
138410                                 W-DADATUM-HELG-AAMMDD                    
138430                                                                          
138500     PERFORM IMS-GU-WDF301                                                
138600     IF SEGMENT-FOUND                                                     
138700        MOVE ERR-SUPPLIER-HOLIDAY  TO MOD-TEMFSFEL                        
138800        MOVE MFS-ADD-HILIGHT-FIELD TO                                     
138900             MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)                      
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 S10-AUTH-USER-CHECK SECTION.                                             
139400     MOVE 'S10-AUTH-CHECK  ' TO CURRENT-SECTION                           
139500                                                                          
139600     PERFORM IMS-GU-WDK711                                                
139700     IF SEGMENT-FOUND                                                     
139800        MOVE SLAG-IDLEVNR          TO WS-IDLEVNR-8                        
139900        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
140000        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
140100*          --- AUTHORIZED USER                                            
140200           SET PASSED-SEC-CHECK    TO TRUE                                
140300           IF SLAG-IDDC-REF NOT = SPACES                                  
140400             MOVE NOO               TO INDATA-SW                          
140500             MOVE INF-REFILL-PART   TO MED-IDMFSINF                       
140600             CALL WMEDKONV       USING MED-WMEDAREA                       
140700             MOVE MED-MFSINF        TO MOD-TEMFSINF                       
140800           END-IF                                                         
140900        ELSE                                                              
141000           MOVE NOO                TO INDATA-SW                           
141100           MOVE ERR-USER-NOT-AUTH  TO MED-IDMFSFEL                        
141200           CALL WMEDKONV USING MED-WMEDAREA                               
141300           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
141400        END-IF                                                            
141500     ELSE                                                                 
141600        MOVE NOO                   TO INDATA-SW                           
141700        MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                        
141800        CALL WMEDKONV USING MED-WMEDAREA                                  
141900        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
142000     END-IF                                                               
142100     .                                                                    
142200     EJECT                                                                
142300 S20-PERIOD-TAB SECTION.                                                  
142400     MOVE 'S20-PERIOD-TAB  ' TO CURRENT-SECTION                           
142500                                                                          
142600     MOVE WS-PERIOD          TO WS-AAPP                                   
142700     MOVE 'AARP'             TO DAT-KDDATFORM                             
142800     MOVE +1                 TO IX-PP                                     
142900                                                                          
143000     PERFORM UNTIL IX-PP      >  2                                        
143100       IF WS-PP = 13                                                      
143200          MOVE +1             TO WS-PP                                    
143300          ADD  +1             TO WS-AA                                    
143400       END-IF                                                             
143500       MOVE WS-AAPP          TO DAT-I-TIDATUM                             
143600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
143700                             DAT-O-TIDATUM DAT-KDSVAR                     
143800       IF DAT-KDSVAR-OK                                                   
143900          MOVE DAT-TISEKEL   TO PER-CENTURY (IX-PP)                       
144000          MOVE DAT-TIAA      TO PER-START-AA (IX-PP)                      
144100                                PER-END-AA (IX-PP)                        
144200          MOVE DAT-TIVV      TO PER-START-VV (IX-PP)                      
144300          MOVE DAT-KVVIPER   TO PER-ANT-VV   (IX-PP)                      
144400          MOVE DAT-TIAARP    TO PER-AAPP     (IX-PP)                      
144500          COMPUTE PER-END-VV (IX-PP) =                                    
144600                  PER-START-VV (IX-PP) + DAT-KVVIPER - 1                  
144700          ADD +1             TO WS-PP IX-PP                               
144800       ELSE                                                               
144900          MOVE ERR-INVALID-PERIOD TO MOD-TEMFSFEL                         
145000          MOVE NOO                TO INDATA-SW                            
145100          MOVE +3                 TO IX-PP                                
145200       END-IF                                                             
145300     END-PERFORM                                                          
145400     MOVE W-START-PER-AAPP   TO TMP1-YYPP                                 
145500     MOVE PER-AAPP (1)       TO TMP2-YYPP                                 
145600     PERFORM WY2000P6                                                     
145700     .                                                                    
145800     EJECT                                                                
145900 S30-OTHER-UPDATES SECTION.                                               
146000     MOVE 'S30-OTHER-UPDATE' TO CURRENT-SECTION                           
146100                                                                          
146200*UPDATE CURRENT DATE IN WDD902                                            
146300     PERFORM IMS-GHU-WDD902                                               
146400     IF SEGMENT-FOUND                                                     
146500        MOVE WS-CURRENT-DATE TO TILEVPL                                   
146600        PERFORM IMS-REPL-WDD902                                           
146700     END-IF                                                               
146800*                                                                         
146900     PERFORM IMS-GHNP-WDD904                                              
147000     IF SEGMENT-FOUND                                                     
147100       PERFORM IMS-GU-WDK722                                              
147200       IF SEGMENT-FOUND                                                   
147300          IF XLAG-KDLPSP NOT = 5                                          
147400          OR W-IDLEVNR NOT = SLAG-IDLEVNR                                 
147500             PERFORM IMS-DLET-WDD904                                      
147600          END-IF                                                          
147700       END-IF                                                             
147800     END-IF                                                               
147900                                                                          
148000*                                                                         
148100     MOVE W-IDLEVNR            TO W-IDLEVNR-2206                          
148200     MOVE W-IDDC               TO W-IDDC-2206                             
148300     PERFORM IMS-GU-WDGX2206                                              
148400     IF SEGMENT-FOUND                                                     
148500     AND (2206-KDEDI NOT = 'T')                                           
148600       MOVE W-IDDC           TO 2248-IDDC                                 
148700       MOVE W-IDLEVNR        TO 2248-IDLEVNR                              
148800       MOVE W-IDARTNR        TO 2248-IDARTNR                              
148900       MOVE ZERO             TO 2248-KVDAGAR                              
149000                                2248-KVBEART                              
149100                                                                          
149200                                                                          
149300       IF 2206-KDVECKOSL NOT = 'P'                                        
149400         PERFORM IMS-ISRT-WDGX2248                                        
149500       END-IF                                                             
149600                                                                          
149700*-- LOGGA EV. TOMPLANER FÖR VECKO-/PERIOD-BATCHERNA.                      
149800       IF 2206-FLLEVPLP = JA OR                                           
149900          2206-FLLEVVB  = JA                                              
150000                                                                          
150100         PERFORM IMS-ISRT-WDGX2248-PERIOD                                 
150200       END-IF                                                             
150300     END-IF                                                               
150400     .                                                                    
150500     EJECT                                                                
150600 MFS-ERASE-FIELD-OUT SECTION.                                             
150700                                                                          
150800*    --- ALLA UTDATA-FÄLT                                                 
150900     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
151000                               MOD-IDDC-IN                                
151100                               MOD-KDBEHX-PLAN-IN                         
151200                               MOD-IDLEVNR-IN                             
151300                               MOD-IDARTNR                                
151400                               MOD-BEART-ENG                              
151500                               MOD-DISP-DAY (1)                           
151600                               MOD-DISP-DAY (2)                           
151700                               MOD-DISP-DAY (3)                           
151800                               MOD-DISP-DAY (4)                           
151900                               MOD-DISP-DAY (5)                           
152000                               MOD-IDANSK                                 
152100                               MOD-KVVECKOR-LT                            
152200                               MOD-KDLPSP                                 
152300                               MOD-TILPSP                                 
152400                                                                          
152500     MOVE 1 TO IX1                                                        
152600     PERFORM UNTIL IX1 > 2                                                
152700        MOVE MFS-ERASE-FIELD                                              
152800                            TO MOD-PERIOD-AARP (IX1)                      
152900        MOVE 1         TO IX2                                             
153000        PERFORM UNTIL IX2 > 5                                             
153100           MOVE MFS-ERASE-FIELD                                           
153200                       TO MOD-KVAVROP-TAB (IX1, IX2)                      
153300                          MOD-SIGN-TAB (IX1, IX2)                         
153400                          MOD-TIAVROP-AVS-TAB (IX1, IX2)                  
153500           MOVE 1      TO IX3                                             
153600           PERFORM UNTIL IX3 > 5                                          
153700              MOVE MFS-ERASE-FIELD                                        
153800                       TO MOD-KVAVROP-DAY-UT (IX1, IX2, IX3)              
153900              ADD 1    TO IX3                                             
154000           END-PERFORM                                                    
154100           ADD 1       TO IX2                                             
154200        END-PERFORM                                                       
154300        ADD 1          TO IX1                                             
154400     END-PERFORM                                                          
154500     .                                                                    
154600     SKIP3                                                                
154700 MFS-ERASE-OUTPUT-FIELDS SECTION.                                         
154800                                                                          
154900*    --- ALLA UTDATA-FÄLT                                                 
155000     MOVE MFS-ERASE-FIELD   TO MOD-BEART-ENG                              
155100                               MOD-DISP-DAY (1)                           
155200                               MOD-DISP-DAY (2)                           
155300                               MOD-DISP-DAY (3)                           
155400                               MOD-DISP-DAY (4)                           
155500                               MOD-DISP-DAY (5)                           
155600                               MOD-IDANSK                                 
155700                               MOD-KVVECKOR-LT                            
155800                               MOD-KDLPSP                                 
155900                               MOD-TILPSP                                 
156000                                                                          
156100     MOVE 1 TO IX1                                                        
156200     PERFORM UNTIL IX1 > 2                                                
156300        MOVE MFS-ERASE-FIELD                                              
156400                            TO MOD-PERIOD-AARP (IX1)                      
156500        MOVE 1         TO IX2                                             
156600        PERFORM UNTIL IX2 > 5                                             
156700           MOVE MFS-ERASE-FIELD                                           
156800                       TO MOD-KVAVROP-TAB (IX1, IX2)                      
156900                          MOD-SIGN-TAB (IX1, IX2)                         
157000                          MOD-TIAVROP-AVS-TAB (IX1, IX2)                  
157100           MOVE 1      TO IX3                                             
157200           PERFORM UNTIL IX3 > 5                                          
157300              MOVE MFS-ERASE-FIELD                                        
157400                       TO MOD-KVAVROP-DAY-UT (IX1, IX2, IX3)              
157500              ADD 1    TO IX3                                             
157600           END-PERFORM                                                    
157700           ADD 1       TO IX2                                             
157800        END-PERFORM                                                       
157900        ADD 1          TO IX1                                             
158000     END-PERFORM                                                          
158100     .                                                                    
158200     SKIP3                                                                
158300 MFS-ERASE-FIELD-IN SECTION.                                              
158400                                                                          
158500*    --- ALLA INDATA-FÄLT                                                 
158600     MOVE 1 TO IX1                                                        
158700     PERFORM UNTIL IX1 > 2                                                
158800        MOVE 1         TO IX2                                             
158900        PERFORM UNTIL IX2 > 5                                             
159000           MOVE 1      TO IX3                                             
159100           PERFORM UNTIL IX3 > 5                                          
159200              MOVE MFS-ERASE-FIELD                                        
159300                       TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)              
159400              ADD 1    TO IX3                                             
159500           END-PERFORM                                                    
159600           ADD 1       TO IX2                                             
159700        END-PERFORM                                                       
159800        ADD 1          TO IX1                                             
159900     END-PERFORM                                                          
160000     .                                                                    
160100     EJECT                                                                
160200 MFS-CLOSE-FIELD-IN SECTION.                                              
160300                                                                          
160400*    --- ALLA INDATA-FÄLT                                                 
160500     MOVE 1 TO IX1                                                        
160600     PERFORM UNTIL IX1 > 2                                                
160700        MOVE 1         TO IX2                                             
160800        PERFORM UNTIL IX2 > 5                                             
160900           MOVE 1      TO IX3                                             
161000           PERFORM UNTIL IX3 > 5                                          
161100              MOVE MFS-CLOSE-FIELD                                        
161200                       TO MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)         
161300              ADD 1    TO IX3                                             
161400           END-PERFORM                                                    
161500           ADD 1       TO IX2                                             
161600        END-PERFORM                                                       
161700        ADD 1          TO IX1                                             
161800     END-PERFORM                                                          
161900     .                                                                    
162000     EJECT                                                                
162100 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
162200                                                                          
162300*    --- ALLA UTDATA-FÄLT                                                 
162400     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-IN                        
162500                                    MOD-IDDC-IN                           
162600                                    MOD-KDBEHX-PLAN-IN                    
162700                                    MOD-IDLEVNR-IN                        
162800                                    MOD-IDARTNR                           
162900                                    MOD-BEART-ENG                         
163000                                    MOD-DISP-DAY (1)                      
163100                                    MOD-DISP-DAY (2)                      
163200                                    MOD-DISP-DAY (3)                      
163300                                    MOD-DISP-DAY (4)                      
163400                                    MOD-DISP-DAY (5)                      
163500                                    MOD-IDANSK                            
163600                                    MOD-KVVECKOR-LT                       
163700                                    MOD-KDLPSP                            
163800                                    MOD-TILPSP                            
163900                                                                          
164000     MOVE 1 TO IX1                                                        
164100     PERFORM UNTIL IX1 > 2                                                
164200        MOVE MFS-DO-NOT-TOUCH-FIELD                                       
164300                            TO MOD-PERIOD-AARP (IX1)                      
164400        MOVE 1         TO IX2                                             
164500        PERFORM UNTIL IX2 > 5                                             
164600           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
164700                       TO MOD-KVAVROP-TAB (IX1, IX2)                      
164800                          MOD-SIGN-TAB (IX1, IX2)                         
164900                          MOD-TIAVROP-AVS-TAB (IX1, IX2)                  
165000           MOVE 1      TO IX3                                             
165100           PERFORM UNTIL IX3 > 5                                          
165200              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
165300                       TO MOD-KVAVROP-DAY-UT (IX1, IX2, IX3)              
165400              ADD 1    TO IX3                                             
165500           END-PERFORM                                                    
165600           ADD 1       TO IX2                                             
165700        END-PERFORM                                                       
165800        ADD 1          TO IX1                                             
165900     END-PERFORM                                                          
166000     .                                                                    
166100     SKIP3                                                                
166200 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
166300                                                                          
166400*    --- ALLA INDATA-FÄLT                                                 
166500     MOVE 1 TO IX1                                                        
166600     PERFORM UNTIL IX1 > 2                                                
166700        MOVE 1         TO IX2                                             
166800        PERFORM UNTIL IX2 > 5                                             
166900           MOVE 1      TO IX3                                             
167000           PERFORM UNTIL IX3 > 5                                          
167100              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
167200                       TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)              
167300              ADD 1    TO IX3                                             
167400           END-PERFORM                                                    
167500           ADD 1       TO IX2                                             
167600        END-PERFORM                                                       
167700        ADD 1          TO IX1                                             
167800     END-PERFORM                                                          
167900     .                                                                    
168000     EJECT                                                                
168100 MFS-FORM-ATTR SECTION.                                                   
168200                                                                          
168300*    --- ALL INDATA-FIELDS                                                
168400     MOVE 1 TO IX1                                                        
168500     PERFORM UNTIL IX1 > 2                                                
168600        MOVE 1         TO IX2                                             
168700        PERFORM UNTIL IX2 > 5                                             
168800           MOVE 1      TO IX3                                             
168900           PERFORM UNTIL IX3 > 5                                          
169000              MOVE MFS-FORMAT-DEFAULT-ATTR                                
169100                       TO MOD-KVAVROP-DAY-IN-ATTR (IX1, IX2, IX3)         
169200              ADD 1    TO IX3                                             
169300           END-PERFORM                                                    
169400           ADD 1       TO IX2                                             
169500        END-PERFORM                                                       
169600        ADD 1          TO IX1                                             
169700     END-PERFORM                                                          
169800     .                                                                    
169900     SKIP2                                                                
170000 MFS-READ-IN-AGAIN SECTION.                                               
170100                                                                          
170200*    --- ALL INDATA-FIELDS                                                
170300     MOVE 1 TO IX1                                                        
170400     PERFORM UNTIL IX1 > 2                                                
170500        MOVE 1         TO IX2                                             
170600        PERFORM UNTIL IX2 > 5                                             
170700           MOVE 1      TO IX3                                             
170800           PERFORM UNTIL IX3 > 5                                          
170900             MOVE MFS-ADD-READ-FIELD                                      
171000                       TO MOD-KVAVROP-DAY-IN (IX1, IX2, IX3)              
171100              ADD 1    TO IX3                                             
171200           END-PERFORM                                                    
171300           ADD 1       TO IX2                                             
171400        END-PERFORM                                                       
171500        ADD 1          TO IX1                                             
171600     END-PERFORM                                                          
171700     .                                                                    
171800     EJECT                                                                
171900* --- IMS SECTIONS ---                                                    
172000     SKIP3                                                                
172100 IMS-GET-MSG SECTION.                                                     
172200                                                                          
172300     MOVE '  QC' TO GOOD-STATUSCODES                                      
172400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
172500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
172600     PERFORM IMS-STATUSCHECK                                              
172700     .                                                                    
172800     SKIP3                                                                
172900 IMS-INSERT-MSG SECTION.                                                  
173000                                                                          
173100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
173200     MOVE SPACE TO GOOD-STATUSCODES                                       
173300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
173400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
173500     PERFORM IMS-STATUSCHECK                                              
173600     .                                                                    
173700     EJECT                                                                
173800 IMS-GU-WDB601 SECTION.                                                   
173900     MOVE 'IMS-GU-WDB601   '  TO CURRENT-IMS-SECTION                      
174000                                                                          
174100     MOVE SPACES              TO ALL-SSA                                  
174200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
174300          DELIMITED BY SIZE INTO SSA1                                     
174400     MOVE '  GE'              TO GOOD-STATUSCODES                         
174500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
174600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
174700     PERFORM IMS-STATUSCHECK                                              
174800     .                                                                    
174900     EJECT                                                                
175000 IMS-GU-WDK601 SECTION.                                                   
175100     MOVE 'IMS-GU-WDK601   '  TO CURRENT-IMS-SECTION                      
175200                                                                          
175300     MOVE SPACE               TO ALL-SSA                                  
175400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
175500          DELIMITED BY SIZE INTO SSA1                                     
175600     MOVE '  GE'              TO GOOD-STATUSCODES                         
175700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
175800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
175900     PERFORM IMS-STATUSCHECK                                              
176000     .                                                                    
176100     EJECT                                                                
176200 IMS-GU-WDF101 SECTION.                                                   
176300     MOVE 'IMS-GU-WDF101   '  TO CURRENT-IMS-SECTION                      
176400                                                                          
176500     MOVE SPACE               TO ALL-SSA                                  
176600                                                                          
176700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
176800          DELIMITED BY SIZE INTO SSA1                                     
176900     MOVE '  GE'              TO GOOD-STATUSCODES                         
177000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
177100     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
177200     PERFORM IMS-STATUSCHECK                                              
177300     .                                                                    
177400     EJECT                                                                
177500 IMS-GU-WDF106 SECTION.                                                   
177600     MOVE 'IMS-GU-WDF106   '  TO CURRENT-IMS-SECTION                      
177700                                                                          
177800     MOVE SPACE               TO ALL-SSA                                  
177900                                                                          
178000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
178100          DELIMITED BY SIZE INTO SSA1                                     
178200     STRING 'WDF106     '                                                 
178300          DELIMITED BY SIZE INTO SSA2                                     
178400     MOVE '  GE'              TO GOOD-STATUSCODES                         
178500     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2               
178600     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
178700     PERFORM IMS-STATUSCHECK                                              
178800     .                                                                    
178900     EJECT                                                                
179000 IMS-GU-WDF116 SECTION.                                                   
179100     MOVE 'IMS-GU-WDF116   '  TO CURRENT-IMS-SECTION                      
179200                                                                          
179300     MOVE SPACE               TO ALL-SSA                                  
179400                                                                          
179500     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
179600          DELIMITED BY SIZE INTO SSA1                                     
179700     STRING 'WDF116  (IDDC     =' W-IDDC-X         ')'                    
179800          DELIMITED BY SIZE INTO SSA2                                     
179900     MOVE '  GE'              TO GOOD-STATUSCODES                         
180000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
180100     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
180200     PERFORM IMS-STATUSCHECK                                              
180300     .                                                                    
180400     EJECT                                                                
180500 IMS-GU-WDK701 SECTION.                                                   
180600     MOVE 'IMS-GU-WDK701   '  TO CURRENT-IMS-SECTION                      
180700                                                                          
180800     MOVE SPACE               TO ALL-SSA                                  
180900                                                                          
181000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
181100          DELIMITED BY SIZE INTO SSA1                                     
181200     MOVE '  GE'              TO GOOD-STATUSCODES                         
181300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
181400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
181500     PERFORM IMS-STATUSCHECK                                              
181600     .                                                                    
181700     EJECT                                                                
181800 IMS-GU-WDK711 SECTION.                                                   
181900     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
182000                                                                          
182100     MOVE SPACE               TO ALL-SSA                                  
182200                                                                          
182300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
182400          DELIMITED BY SIZE INTO SSA1                                     
182500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
182600          DELIMITED BY SIZE INTO SSA2                                     
182700     MOVE '  GE'              TO GOOD-STATUSCODES                         
182800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
182900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
183000     PERFORM IMS-STATUSCHECK                                              
183100     .                                                                    
183200     EJECT                                                                
183300 IMS-GU-WDK712 SECTION.                                                   
183400     MOVE 'IMS-GU-WDK712   '  TO CURRENT-IMS-SECTION                      
183500                                                                          
183600     MOVE SPACE               TO ALL-SSA                                  
183700                                                                          
183800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
183900          DELIMITED BY SIZE INTO SSA1                                     
184000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
184100          DELIMITED BY SIZE INTO SSA2                                     
184200     MOVE '    '              TO GOOD-STATUSCODES                         
184300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
184400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
184500     PERFORM IMS-STATUSCHECK                                              
184600     .                                                                    
184700     EJECT                                                                
184800 IMS-GNP-WDK722 SECTION.                                                  
184900     MOVE 'IMS-GNP-WDK722  '  TO CURRENT-IMS-SECTION                      
185000                                                                          
185100     MOVE SPACE               TO ALL-SSA                                  
185200     MOVE 'WDK722   '         TO SSA1                                     
185300     MOVE '  GE'              TO GOOD-STATUSCODES                         
185400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
185500     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
185600     PERFORM IMS-STATUSCHECK                                              
185700     .                                                                    
185800     EJECT                                                                
185900 IMS-GNP-WDK723 SECTION.                                                  
186000     MOVE 'IMS-GNP-WDK723  '  TO CURRENT-IMS-SECTION                      
186100                                                                          
186200     MOVE SPACE               TO ALL-SSA                                  
186300     MOVE 'WDK723   '         TO SSA1                                     
186400     MOVE '  GE'              TO GOOD-STATUSCODES                         
186500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
186600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
186700     PERFORM IMS-STATUSCHECK                                              
186800     .                                                                    
186900     EJECT                                                                
187000 IMS-GU-WDK722 SECTION.                                                   
187100     MOVE 'IMS-GU-WDK722   '  TO CURRENT-IMS-SECTION                      
187200                                                                          
187300     MOVE SPACE               TO ALL-SSA                                  
187400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
187500          DELIMITED BY SIZE INTO SSA1                                     
187600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
187700          DELIMITED BY SIZE INTO SSA2                                     
187800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
187900          DELIMITED BY SIZE INTO SSA3                                     
188000     MOVE '  GE'              TO GOOD-STATUSCODES                         
188100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
188200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
188300     PERFORM IMS-STATUSCHECK                                              
188400     .                                                                    
188500     EJECT                                                                
188600 IMS-GU-WDF301 SECTION.                                                   
188700     MOVE 'IMS-GU-WDF301   '  TO CURRENT-IMS-SECTION                      
188800                                                                          
188900     MOVE SPACE               TO ALL-SSA                                  
189000                                                                          
189100     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE '  GE'              TO GOOD-STATUSCODES                         
189400     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF301 SSA1                    
189500     MOVE WDF3-STATUS-CODE    TO STATUS-WS                                
189600     PERFORM IMS-STATUSCHECK                                              
189700     .                                                                    
189800     EJECT                                                                
189900 IMS-GU-WDF311 SECTION.                                                   
190000     MOVE 'IMS-GU-WDF311   '  TO CURRENT-IMS-SECTION                      
190100                                                                          
190200     MOVE SPACE               TO ALL-SSA                                  
190300     STRING 'WDF311  (IDLEVNR  =' W-IDLEVNR-X ')'                         
190400          DELIMITED BY SIZE INTO SSA1                                     
190500     MOVE '  GE'              TO GOOD-STATUSCODES                         
190600     CALL CBLTDLI USING GNP WDF3-PCB DLI-IO-WDF311 SSA1                   
190700     MOVE WDF3-STATUS-CODE    TO STATUS-WS                                
190800     PERFORM IMS-STATUSCHECK                                              
190900     .                                                                    
191000     EJECT                                                                
191100 IMS-GU-WDD311 SECTION.                                                   
191200     MOVE 'IMS-GU-WDD311   '  TO CURRENT-IMS-SECTION                      
191300                                                                          
191400     MOVE SPACE               TO ALL-SSA                                  
191500                                                                          
191600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
191700          DELIMITED BY SIZE INTO SSA1                                     
191800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
191900          DELIMITED BY SIZE INTO SSA2                                     
192000     MOVE '  GE'              TO GOOD-STATUSCODES                         
192100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
192200     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
192300     PERFORM IMS-STATUSCHECK                                              
192400     .                                                                    
192500     EJECT                                                                
192600 IMS-GU-WDD901 SECTION.                                                   
192700     MOVE 'IMS-GU-WDD901   '  TO CURRENT-IMS-SECTION                      
192800                                                                          
192900     MOVE SPACE               TO ALL-SSA                                  
193000                                                                          
193100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
193200          DELIMITED BY SIZE INTO SSA1                                     
193300     MOVE '  GE'              TO GOOD-STATUSCODES                         
193400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
193500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
193600     PERFORM IMS-STATUSCHECK                                              
193700     .                                                                    
193800     SKIP3                                                                
193900 IMS-GU-WDD902 SECTION.                                                   
194000     MOVE 'IMS-GU-WDD902   '  TO CURRENT-IMS-SECTION                      
194100                                                                          
194200     MOVE SPACE               TO ALL-SSA                                  
194300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
194400          DELIMITED BY SIZE INTO SSA1                                     
194500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
194600          DELIMITED BY SIZE INTO SSA2                                     
194700     MOVE '  GE'              TO GOOD-STATUSCODES                         
194800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
194900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
195000     PERFORM IMS-STATUSCHECK                                              
195100     .                                                                    
195200     EJECT                                                                
195300 IMS-GNP-WDD902 SECTION.                                                  
195400     MOVE 'IMS-GNP-WDD902  '  TO CURRENT-IMS-SECTION                      
195500                                                                          
195600     MOVE SPACE               TO ALL-SSA                                  
195700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
195800          DELIMITED BY SIZE INTO SSA1                                     
195900     MOVE '  GE'              TO GOOD-STATUSCODES                         
196000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
196100     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
196200     PERFORM IMS-STATUSCHECK                                              
196300     .                                                                    
196400     EJECT                                                                
196500 IMS-GNP-WDD905 SECTION.                                                  
196600     MOVE 'IMS-GNP-WDD905  '  TO CURRENT-IMS-SECTION                      
196700                                                                          
196800     MOVE SPACE               TO ALL-SSA                                  
196900     MOVE 'WDD905   '         TO SSA1                                     
197000     MOVE '  GE' TO GOOD-STATUSCODES                                      
197100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
197200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
197300     PERFORM IMS-STATUSCHECK                                              
197400     .                                                                    
197500     EJECT                                                                
197600 IMS-GU-WDD905-AAAAVV SECTION.                                            
197700     MOVE 'GU-D905-AAAAVV  '  TO CURRENT-IMS-SECTION                      
197800                                                                          
197900     MOVE SPACE               TO ALL-SSA                                  
198000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
198100            DELIMITED BY SIZE INTO SSA1                                   
198200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
198300            DELIMITED BY SIZE INTO SSA2                                   
198400     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
198500            DELIMITED BY SIZE INTO SSA3                                   
198600     MOVE '  GE'                TO GOOD-STATUSCODES                       
198700     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
198800     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
198900     PERFORM IMS-STATUSCHECK                                              
199000     .                                                                    
199100     EJECT                                                                
199200 IMS-GHNP-WDD905 SECTION.                                                 
199300     MOVE 'IMS-GHNP-WDD905 '  TO CURRENT-IMS-SECTION                      
199400                                                                          
199500     MOVE SPACE               TO ALL-SSA                                  
199600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
199700          DELIMITED BY SIZE INTO SSA1                                     
199800     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X ')'                        
199900          DELIMITED BY SIZE INTO SSA2                                     
200000     MOVE '  GE'              TO GOOD-STATUSCODES                         
200100     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2             
200200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
200300     PERFORM IMS-STATUSCHECK                                              
200400     .                                                                    
200500     EJECT                                                                
200600 IMS-ISRT-WDD905 SECTION.                                                 
200700     MOVE 'IMS-ISRT-WDD905 '  TO CURRENT-IMS-SECTION                      
200800                                                                          
200900     MOVE SPACE               TO ALL-SSA                                  
201000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
201100          DELIMITED BY SIZE INTO SSA1                                     
201200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
201300          DELIMITED BY SIZE INTO SSA2                                     
201400     MOVE   'WDD905   '       TO SSA3                                     
201500     MOVE '  '                TO GOOD-STATUSCODES                         
201600     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905                       
201700                        SSA1 SSA2 SSA3                                    
201800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
201900     PERFORM IMS-STATUSCHECK                                              
202000     .                                                                    
202100     EJECT                                                                
202200 IMS-REPL-WDD905 SECTION.                                                 
202300     MOVE 'IMS-REPL-WDD905 '  TO CURRENT-IMS-SECTION                      
202400                                                                          
202500     MOVE SPACE               TO ALL-SSA                                  
202600                                                                          
202700     MOVE '  '                TO GOOD-STATUSCODES                         
202800     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
202900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
203000     PERFORM IMS-STATUSCHECK                                              
203100     .                                                                    
203200     EJECT                                                                
203300 IMS-DLET-WDD905 SECTION.                                                 
203400     MOVE 'IMS-DLET-WDD905 '  TO CURRENT-IMS-SECTION                      
203500                                                                          
203600     MOVE SPACE               TO ALL-SSA                                  
203700     MOVE '  '                TO GOOD-STATUSCODES                         
203800     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
203900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
204000     PERFORM IMS-STATUSCHECK                                              
204100     .                                                                    
204200     EJECT                                                                
204300 IMS-GHU-WDD902 SECTION.                                                  
204400     MOVE 'IMS-GHU-WDD902  '  TO CURRENT-IMS-SECTION                      
204500                                                                          
204600     MOVE SPACE               TO ALL-SSA                                  
204700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
204800          DELIMITED BY SIZE INTO SSA1                                     
204900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
205000          DELIMITED BY SIZE INTO SSA2                                     
205100     MOVE '  '                TO GOOD-STATUSCODES                         
205200     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
205300     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
205400     PERFORM IMS-STATUSCHECK                                              
205500     .                                                                    
205600     EJECT                                                                
205700 IMS-GHNP-WDD904 SECTION.                                                 
205800     MOVE 'IMS-GHNP-WDD904 '  TO CURRENT-IMS-SECTION                      
205900                                                                          
206000     MOVE SPACE               TO ALL-SSA                                  
206100     MOVE 'WDD904  '          TO SSA1                                     
206200     MOVE '  GE'              TO GOOD-STATUSCODES                         
206300     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD904 SSA1                  
206400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
206500     PERFORM IMS-STATUSCHECK                                              
206600     .                                                                    
206700     EJECT                                                                
206800 IMS-REPL-WDD902 SECTION.                                                 
206900     MOVE 'IMS-REPL-WDD902 '  TO CURRENT-IMS-SECTION                      
207000                                                                          
207100     MOVE SPACE               TO ALL-SSA                                  
207200     MOVE '  '                TO GOOD-STATUSCODES                         
207300     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD902                       
207400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
207500     PERFORM IMS-STATUSCHECK                                              
207600     .                                                                    
207700     EJECT                                                                
207800 IMS-DLET-WDD904 SECTION.                                                 
207900     MOVE 'IMS-DLET-WDD904 '  TO CURRENT-IMS-SECTION                      
208000                                                                          
208100     MOVE SPACE               TO ALL-SSA                                  
208200     MOVE '  '                TO GOOD-STATUSCODES                         
208300     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
208400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
208500     PERFORM IMS-STATUSCHECK                                              
208600     .                                                                    
208700     EJECT                                                                
208800 IMS-GU-WDGX2206 SECTION.                                                 
208900     MOVE 'IMS-GU-WDGX2206 '  TO CURRENT-IMS-SECTION                      
209000                                                                          
209100     MOVE SPACE               TO ALL-SSA                                  
209200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
209300          DELIMITED BY SIZE INTO SSA1                                     
209400     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
209500          DELIMITED BY SIZE INTO SSA2                                     
209600     MOVE '  GE'              TO GOOD-STATUSCODES                         
209700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2206 SSA1 SSA2             
209800     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
209900     PERFORM IMS-STATUSCHECK                                              
210000     .                                                                    
210100     SKIP3                                                                
210200 IMS-ISRT-WDGX2248 SECTION.                                               
210300     MOVE 'IMS-ISRT-WDGX2248 '  TO CURRENT-IMS-SECTION                    
210400                                                                          
210500     MOVE SPACE               TO ALL-SSA                                  
210600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
210700          DELIMITED BY SIZE INTO SSA1                                     
210800     MOVE   'WDGX2248'        TO SSA2                                     
210900     MOVE '  II'              TO GOOD-STATUSCODES                         
211000     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2248 SSA1 SSA2           
211100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
211200     PERFORM IMS-STATUSCHECK                                              
211300     .                                                                    
211400     EJECT                                                                
211500 IMS-ISRT-WDGX2248-PERIOD SECTION.                                        
211600     MOVE 'IMS-ISRT-WDGX2248-PERIOD '  TO CURRENT-IMS-SECTION             
211700                                                                          
211800     MOVE SPACE               TO ALL-SSA                                  
211900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-P-X ')'                  
212000          DELIMITED BY SIZE INTO SSA1                                     
212100     MOVE   'WDGX2248'        TO SSA2                                     
212200     MOVE '  II'              TO GOOD-STATUSCODES                         
212300     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2248 SSA1 SSA2           
212400     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
212500     PERFORM IMS-STATUSCHECK                                              
212600     .                                                                    
212700     EJECT                                                                
212800 IMS-STATUSCHECK SECTION.                                                 
212900                                                                          
213000     SET STATUS-IX TO 1                                                   
213100     SEARCH GOOD-STATUS                                                   
213200       AT END                                                             
213300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
213400         DELIMITED BY SIZE INTO ERROR-TEXT                                
213500         CALL FELLOG                                                      
213600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
213700         CONTINUE                                                         
213800     END-SEARCH                                                           
213900     .                                                                    
214000*    -COPY WY2000P1                                                       
214100     EJECT                                                                
214200*    -COPY WY2000P3                                                       
214300     EJECT                                                                
214400*    -COPY WY2000P6                                                       
214500     EJECT                                                                
214600*    -COPY WY2000P9                                                       
214700     EJECT                                                                
214800*    -COPY WY2000Q3                                                       
214900     EJECT                                                                
215000*    -COPY WY2000Q7                                                       
