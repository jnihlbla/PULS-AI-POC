000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2043100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/10/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN WITH POSSIBILITIES TO UPDATE SPECIFIC PROCUREMENT         
000900*        DATA. KEYS FOR ENTERING THE SCREEN ARE PARTNUMBER ID AND         
001000*        DC-ID.                                                           
001100*                                                                         
001200*        THE PROGRAM READS     WDK6                                       
001300*        THE PROGRAM READS     WDF1                                       
001400*        THE PROGRAM READS     WDK7                                       
001500*        THE PROGRAM READS     WDB6                                       
001600*        THE PROGRAM READS     WDR2                                       
001700*        THE PROGRAM READS     WDD7A                                      
001800*        THE PROGRAM READS     WDD3                                       
001900*        THE PROGRAM READS     WDD9                                       
002000*        THE PROGRAM READS     WDR2 (WDGX2502)                            
002100*                              WDR2 (WDGX2232)                            
002200*        THE PROGRAM UPDATES   WDK7 WDR2                                  
002300*                              WDR5 (WDGX2224)                            
002400*                              WDG3 (WDGX2254)                            
002500*                              WDR4 (WDGX4506)                            
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSACTION: W2T431                                              
002900*                     W2T431U                                             
003000*                     W2T431V                                             
003100*                                                                         
003200*        MID:         W2I43101                                            
003300*                                                                         
003400*    OUTDATA.                                                             
003500*        MOD:         W2O431N1                                            
003600******************************************************************        
003700*    ÄNDRINGAR:                                                           
003800* 2013-09-10   E'TRACKER 10211193  TAG BORT 2204-TRIGGER ORSAKSKOD        
003900*                                  =11 VID LEV.BYTE.GÖRS REDAN I          
004000*                                  PROGRAM W2135200.                      
004100*                                                                         
004200* 2015-04-22   E'TRACKER 10130993                                         
004300*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
004400*                                                                         
004500* 2018-05-14   JIRA 976                                                   
004600*              GLOBAL EXPORT PURCHASE PLANNING                            
004700*                                                                         
004800                                                                          
004900     SKIP3                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100                                                                          
005200 DATA DIVISION.                                                           
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500 77  IDPGM                       PIC X(08)   VALUE 'W2043100'.            
005600 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005700 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005800                                                                          
005900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006100                                                                          
006200 77  YES                         PIC X       VALUE 'J'.                   
006300 77  NOO                         PIC X       VALUE 'N'.                   
006400 77  IX                          PIC 9(2)    VALUE ZERO.                  
006500 77  IX2                         PIC 9(2)    VALUE ZERO.                  
006600 77  WS-IDARTNR-IN         PIC S9(09) COMP-3 VALUE ZERO.                  
006700                                                                          
006800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-WRONG                        VALUE 'N'.                   
007300                                                                          
007400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007500     88  KEYS-OK                             VALUE 'J'.                   
007600     88  KEYS-WRONG                          VALUE 'N'.                   
007700                                                                          
007800 77  LOCK-FIELD-SW               PIC X       VALUE 'N'.                   
007900     88  LOCK-FIELD                          VALUE 'J'.                   
008000                                                                          
008100 77  REFILL-PART-SW              PIC X       VALUE 'N'.                   
008200     88  REFILL-PART                         VALUE 'J'.                   
008300                                                                          
008400 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
008500     88  PASSED-SEC-CHECK                    VALUE 'J'.                   
008600                                                                          
008700 77  VISA-INFO-SW                PIC X       VALUE 'J'.                   
008800     88  VISA-INFO-OK                        VALUE 'J'.                   
008900     88  VISA-INFO-FEL                       VALUE 'N'.                   
009000                                                                          
009100 77  CHECK-LOCAL-PART-SW         PIC X       VALUE 'N'.                   
009200     88  CHECK-LOCAL-PART                    VALUE 'J'.                   
009300                                                                          
009400 77  REPLACE-WDK711-SW           PIC X       VALUE 'N'.                   
009500     88  REPLACE-WDK711-OK                   VALUE 'J'.                   
009600                                                                          
009700 77  REPLACE-WDK722-SW           PIC X       VALUE 'N'.                   
009800     88  REPLACE-WDK722-OK                   VALUE 'J'.                   
009900                                                                          
010000 77  WDK722-FINNS-SW             PIC X       VALUE 'N'.                   
010100     88  WDK722-FINNS                        VALUE 'J'.                   
010200     88  WDK722-SAKNAS                       VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  OWN-MID                             VALUE '2431'.                
010600     88  GOOD-MID                            VALUE '2431' '2432'          
010700                                                   '2433' '2434'          
010800                                                   '2435' '2436'          
010900                                                   '2437' '2438'          
011000                                                   '2439'.                
011100     88  HELP-MID                            VALUE '0551'.                
011200                                                                          
011300     SKIP2                                                                
011400                                                                          
011500     EJECT                                                                
011600 01  WS-VARIABLES.                                                        
011700     03 WS-IDLEVNR-8             PIC X(8)    VALUE SPACE.                 
011800     03 WS-START-DATUM           PIC 9(6)    VALUE ZERO.                  
011900     03 WS-DASH                  PIC X(1)    VALUE '-'.                   
012000     03 WS-IDREFTAB-ID           PIC X(1).                                
012100        88 VALID-IDREFTAB-ID                 VALUE 'A' THRU 'Z'.          
012200     03 WS-PLANGR-AG             PIC X(1).                                
012300        88 VALID-PLANGR                      VALUE '0' THRU '9'.          
012400     03 WS-CURRENT-DATE          PIC 9(8)    VALUE ZERO.                  
012500     03 WS-CURRENT-YYWWD         PIC 9(5)    VALUE ZERO.                  
012600     03 WS-CURRENT-YYWWD-2AAR    PIC 9(5)    VALUE ZERO.                  
012700     03 DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                  
012800     03 WS-KVVECKOR-FT           PIC 9(3)    VALUE ZERO.                  
012900     03 WS-AVAIL-QTY             PIC 9(7)    VALUE ZERO.                  
013000     03 WS-SLAG-KVROS            PIC 9(7)    VALUE ZERO.                  
013100     03 WS-KVSPANT               PIC 9(7)    VALUE ZERO.                  
013200     03 SW-ALARM                 PIC X(1)    VALUE 'N'.                   
013300     03 INDX                     PIC 9(5)    VALUE ZERO.                  
013400     03 MAX-DAYS-INDX            PIC 9(5)    VALUE 5.                     
013500     03 WS-MFG-UPD               PIC X(1)    VALUE SPACE.                 
013600     03 WS-SHP-UPD               PIC X(1)    VALUE SPACE.                 
013700     03 WS-LEVDAT-UPD            PIC X(1)    VALUE SPACE.                 
013800     03 WS-IDDC-REF              PIC X(2)    VALUE SPACE.                 
013900     03 WS-XLAG-IDLEVNR-FRAM     PIC X(5)    VALUE SPACE.                 
014000     03 WS-XLAG-IDLEVNR-SHIP-FRAM  PIC X(5)  VALUE SPACE.                 
014100     03 WS-XLAG-TILEVDAT         PIC S9(7) VALUE ZERO COMP-3.             
014200     03 SPAR-IDPLANGR-AG         PIC 9(1)    VALUE ZERO.                  
014300     03 SPAR-TIMANSEC            PIC S9(7) VALUE ZERO COMP-3.             
014400     03 SPAR-TIMANLED            PIC S9(7) VALUE ZERO COMP-3.             
014500     03 SPAR-KVSLUTKP            PIC S9(7) VALUE ZERO COMP-3.             
014600     03 SPAR-WDK601-TIFINLV      PIC S9(5) VALUE ZERO COMP-3.             
014700     03 WS-IDINK                 PIC X(4)    VALUE SPACE.                 
014800     03  FILLER REDEFINES WS-IDINK.                                       
014900         05  WS-IDINK-3          PIC X(3).                                
015000         05  FILLER              PIC X.                                   
015100                                                                          
015200 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
015300*01 -COPY WWIDFTG                                                         
015400                                                                          
015500*    --- VALID IDDC CODES                                                 
015600*                                                                         
015700*01  -COPY WWDCKONS                                                       
015800                                                                          
015900*01  -COPY WWDC99                                                         
016000                                                                          
016100     EJECT                                                                
016200 01  W-DAGENS-DATUM-PL-ONE-YEAR            PIC 9(6) VALUE ZERO.           
016300 01  FILLER REDEFINES W-DAGENS-DATUM-PL-ONE-YEAR.                         
016400     03  W-DAGENS-DATUM-PL-ONE-YEAR-YY     PIC 9(2).                      
016500     03  W-DAGENS-DATUM-PL-ONE-YEAR-MM     PIC 9(2).                      
016600     03  W-DAGENS-DATUM-PL-ONE-YEAR-DD     PIC 9(2).                      
016700                                                                          
016800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016900 01  GENERAL-SUBPROGRAMS.                                                 
017000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017200     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
017300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017600     EJECT                                                                
017700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
017800*01 -COPY WMEDAREA                                                        
017900*    --- PARAMETERS FOR DATE CONVERSIONS                                  
018000*01 -COPY WY2000W1                                                        
018100     SKIP3                                                                
018200 01  MESSAGE-CODES.                                                       
018300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
018500     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
018600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018800     03  ERR-NO-UPDATE-DONE      PIC X(3)    VALUE '034'.                 
018900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019000     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
019100     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
019200     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
019300     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
019400     03  ERR-NOT-LOCAL-SOURCED   PIC X(3)    VALUE '433'.                 
019500     03  ERR-PART-MISS-ON-FILE   PIC X(3)    VALUE '769'.                 
019600     03  ERR-FUTURE-DATE         PIC X(3)    VALUE '363'.                 
019700     03  ERR-ONE-YEAR-IN-FUTURE  PIC X(3)    VALUE '364'.                 
019800     03  ERR-PART-MISSING-IN-DC  PIC X(3)    VALUE '305'.                 
019900     EJECT                                                                
020000 01  MEDDELANDE.                                                          
020100     03  MED-1                  PIC X(30)                                 
020200         VALUE 'FUTURE FORECAST EXISTS        '.                          
020210     03  MED-2                  PIC X(30)                                 
020220         VALUE 'REFILL FLOW NOT AVAILABLE     '.                          
020300*01  -COPY WDATAREA                                                       
020400     EJECT                                                                
020500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
020600*                                                                         
020700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
020800     SKIP3                                                                
020900*01 -COPY WMSGINIT                                                        
021000     EJECT                                                                
021100*    --- PARAMETERS FOR SUB PROGRAM W005WDK7                              
021200*                                                                         
021300 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
021400     SKIP3                                                                
021500*01 -COPY W005WDK7                                                        
021600     EJECT                                                                
021700*                                                                         
021800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
021900*                                                                         
022000 01  SAVE-AREA.                                                           
022100     03  SAVE-IDTRANS           PIC X(4)    VALUE '2431'.                 
022200     EJECT                                                                
022300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
022400*                                                                         
022500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022600     SKIP3                                                                
022700*01  MID -COPY W2I43101 -PRE MID-.                                        
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023000     SKIP3                                                                
023100*01  -COPY WMSGAREA                                                       
023200     EJECT                                                                
023300     03  MOD REDEFINES MSG-AREA.                                          
023400*      05  -COPY W2O43101 -PRE MOD-.                                      
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023700     SKIP3                                                                
023800*01  -COPY WMFSAREA                                                       
023900     EJECT                                                                
024000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024300     SKIP3                                                                
024400 01  KEYS-FOR-DLI.                                                        
024500     03  W-IDARTNR-X.                                                     
024600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024700     03  W-IDDC-X.                                                        
024800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024900     03  W-IDDC-REF-X.                                                    
025000         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
025100     03  W-IDLAND-X.                                                      
025200         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
025300     03  W-IDLEVNR-X.                                                     
025400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
025500     03  W-IDLEVNR-B6-X.                                                  
025600         05  W-IDLEVNR-B6        PIC X(5)    VALUE SPACE.                 
025700     03  W-IDSKYLT-X.                                                     
025800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
025900     03  W-WDD7A1KY-MIN.                                                  
026000         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
026100         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
026200                                                                          
026300     03  W-WDD7A1KY-MAX.                                                  
026400         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3 VALUE ZERO.           
026500         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
026600                                                                          
026700     03  W-WDGXKEY-4505-X.                                                
026800         05  W-IDHTYP-4505       PIC X(4)    VALUE '4505'.                
026900         05  W-IDDC-4505         PIC X(2)    VALUE SPACE.                 
027000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
027100                                                                          
027200     03  W-KDAVROP-X.                                                     
027300         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
027400                                                                          
027500     03  W-WDGX2203-X.                                                    
027600         05  W-IDHTYP-2203       PIC X(4)     VALUE '2203'.               
027700         05  W-IDDC-2203         PIC X(2)     VALUE SPACE.                
027800         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
027900                                                                          
028000     03  W-WDGX2213-X.                                                    
028100         05  W-IDHTYP-2213       PIC X(4)    VALUE '2213'.                
028200         05  W-IDDC-2213         PIC X(2)    VALUE SPACES.                
028300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
028400                                                                          
028500     03  W-WDGX2223-X.                                                    
028600         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
028700         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
028800         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
028900                                                                          
029000     03  W-WDGX2231-X.                                                    
029100         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
029200         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
029300                                                                          
029400     03  W-WDGX2232-X.                                                    
029500         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
029600         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
029700                                                                          
029800     03  W-WDGXKEY-2501-X.                                                
029900          05 W-IDHTYP-2501       PIC X(4)    VALUE '2501'.                
030000          05 W-IDDC-2501         PIC X(2)    VALUE SPACE.                 
030100          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
030200                                                                          
030300     03  W-WDGXKEY-2261-X.                                                
030400          05 W-IDHTYP-2261       PIC X(4)    VALUE '2261'.                
030500          05 W-IDDC-2261         PIC X(2)    VALUE SPACE.                 
030600          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
030700                                                                          
030800     03  W-WDGXKEY-2253-X.                                                
030900          05 W-IDHTYP-2253       PIC X(4)    VALUE '2253'.                
031000          05 FILLER              PIC X(26)   VALUE LOW-VALUE.             
031100                                                                          
031200     03  W-WDGXKEY-2254-X.                                                
031300          05 W-IDDC-2254         PIC X(2)    VALUE SPACE.                 
031400          05 W-IDARTNR-2254      PIC S9(9)   VALUE ZERO COMP-3.           
031500                                                                          
031600     03  W-IDREFTAB-X.                                                    
031700         05  W-IDREFTAB-2502     PIC X(1)    VALUE SPACE.                 
031800                                                                          
031900     03  W-WDD901KY-X.                                                    
032000         05  W-IDARTNR-D9         PIC S9(9)  VALUE ZERO COMP-3.           
032100         05  W-IDDC-D9            PIC X(2)   VALUE SPACE.                 
032200                                                                          
032300     03  W-WDB615KY-X.                                                    
032400         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
032500         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
032600                                                                          
032700                                                                          
032800     SKIP2                                                                
032900*    --- STATUS CODES FROM IMS                                            
033000 01  STATUS-WS                   PIC XX.                                  
033100     88  SEGMENT-FOUND                       VALUE '  '.                  
033200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
033300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
033400     SKIP2                                                                
033500 01  GOOD-STATUSCODES.                                                    
033600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033700     SKIP3                                                                
033800 01  ALL-SSA.                                                             
033900     03 SSA1                     PIC X(64).                               
034000     03 SSA2                     PIC X(64).                               
034100     03 SSA3                     PIC X(64).                               
034200     EJECT                                                                
034300*    --- IMS FUNCTION CODES                                               
034400*01  -COPY W0003                                                          
034500     EJECT                                                                
034600*    ---  DLI INPUT-OUTPUT AREA                                           
034700                                                                          
034800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
034900 01  DLI-IO-WDK601.                                                       
035000*    03  -COPY WDK601                                                     
035100     EJECT                                                                
035200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
035300 01  DLI-IO-WDK611.                                                       
035400*    03  -COPY WDK611                                                     
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
035600 01  DLI-IO-WDD311.                                                       
035700*    03  -COPY WDD311                                                     
035800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
035900 01  DLI-IO-WDF101.                                                       
036000*    03  -COPY WDF101                                                     
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
036200 01  DLI-IO-WDF116.                                                       
036300*    03  -COPY WDF116                                                     
036400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
036500 01  DLI-IO-WDK701.                                                       
036600*    03  -COPY WDK701                                                     
036700     EJECT                                                                
036800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
036900 01  DLI-IO-WDK711.                                                       
037000*    03  -COPY WDK711                                                     
037100     EJECT                                                                
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK727'.                      
037300 01  DLI-IO-WDK727.                                                       
037400*    03  -COPY WDK727                                                     
037500     EJECT                                                                
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-K711-REF'.                    
037700 01  DLI-IO-K711-REF.                                                     
037800*    03  -COPY WDK711  -PRE REF-                                          
037900     EJECT                                                                
038000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
038100 01  DLI-IO-WDK712.                                                       
038200*    03  -COPY WDK712                                                     
038300     EJECT                                                                
038400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
038500 01  DLI-IO-WDK722.                                                       
038600*    03  -COPY WDK722                                                     
038700     EJECT                                                                
038800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
038900 01  DLI-IO-WDB601.                                                       
039000*    03  -COPY WDB601                                                     
039100     EJECT                                                                
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-B601-LEV'.                    
039300 01  DLI-IO-B601-LEV.                                                     
039400*    03  -COPY WDB601 -PRE LEVDC-                                         
039500     EJECT                                                                
039600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
039700 01  DLI-IO-WDB615.                                                       
039800*    03  -COPY WDB615                                                     
039900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
040000 01  DLI-IO-WDB616.                                                       
040100*    03  -COPY WDB616                                                     
040200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX2262'.                    
040300 01  DLI-IO-WDGX2262.                                                     
040400*    03  -COPY WDGX2262                                                   
040500     EJECT                                                                
040600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
040700 01  DLI-IO-WDD7A1.                                                       
040800*    03  -COPY WDD7A1                                                     
040900     EJECT                                                                
041000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
041100 01  DLI-IO-WDD901.                                                       
041200*    03  -COPY WDD901                                                     
041300     EJECT                                                                
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
041500 01  DLI-IO-WDD902.                                                       
041600*    03  -COPY WDD902                                                     
041700     EJECT                                                                
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
041900 01  DLI-IO-WDD905.                                                       
042000*    03  -COPY WDD905                                                     
042100     EJECT                                                                
042200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4506'.                    
042300 01  DLI-IO-WDGX4506.                                                     
042400*    03  -COPY WDGX4506                                                   
042500     EJECT                                                                
042600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
042700 01  DLI-IO-WDGX2204.                                                     
042800*    03  -COPY WDGX2204                                                   
042900     EJECT                                                                
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
043100 01  DLI-IO-WDGX2214.                                                     
043200*    03  -COPY WDGX2214                                                   
043300     EJECT                                                                
043400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
043500 01  DLI-IO-WDGX2223.                                                     
043600*    03  -COPY WDGX2223                                                   
043700     EJECT                                                                
043800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
043900 01  DLI-IO-WDGX2224.                                                     
044000*    03  -COPY WDGX2224                                                   
044100     EJECT                                                                
044200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
044300 01  DLI-IO-WDGX2232.                                                     
044400*    03  -COPY WDGX2232                                                   
044500     EJECT                                                                
044600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2502'.                    
044700 01  DLI-IO-WDGX2502.                                                     
044800*    03  -COPY WDGX2502                                                   
044900     EJECT                                                                
045000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2254'.                    
045100 01  DLI-IO-WDGX2254.                                                     
045200*    03  -COPY WDGX2254                                                   
045300     EJECT                                                                
045400 LINKAGE SECTION.                                                         
045500*01  -COPY W0009   -PRE MSG-                                              
045600*01  -COPY W0008   -PRE WDP7-                                             
045700     05  FILLER                  PIC X.                                   
045800                                                                          
045900*01  -COPY W0008  -PRE WDK6-                                              
046000     05  FILLER                  PIC X.                                   
046100                                                                          
046200*01  -COPY W0008  -PRE WDF1-                                              
046300     05  FILLER                  PIC X.                                   
046400                                                                          
046500*01  -COPY W0008  -PRE WDK7-                                              
046600     05  FILLER                  PIC X.                                   
046700                                                                          
046800*01  -COPY W0008  -PRE WDB6-                                              
046900     05  FILLER                  PIC X.                                   
047000                                                                          
047100*01  -COPY W0008  -PRE 2261-                                              
047200     05  FILLER                  PIC X.                                   
047300                                                                          
047400*01  -COPY W0008  -PRE WDD3-                                              
047500     05  FILLER                  PIC X.                                   
047600                                                                          
047700*01  -COPY W0008  -PRE WDD7A-                                             
047800     05  FILLER                  PIC X.                                   
047900                                                                          
048000*01  -COPY W0008  -PRE WDR5-                                              
048100     05  FILLER                  PIC X.                                   
048200                                                                          
048300*01  -COPY W0008  -PRE WDG3-                                              
048400     05  FILLER                  PIC X.                                   
048500                                                                          
048600*01  -COPY W0008  -PRE WDR4-                                              
048700     05  FILLER                  PIC X.                                   
048800                                                                          
048900*01  -COPY W0008  -PRE 2501-                                              
049000     05  FILLER                  PIC X.                                   
049100                                                                          
049200*01  -COPY W0008  -PRE WDD9-                                              
049300     05  FILLER                  PIC X.                                   
049400*01  -COPY W0008  -PRE WDR2-                                              
049500     05  FILLER                  PIC X.                                   
049600*01  -COPY W0008  -PRE WDK72-                                             
049700     05  FILLER                  PIC X.                                   
049800                                                                          
049900     EJECT                                                                
050000 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDF1-PCB             
050100     WDK7-PCB WDB6-PCB 2261-PCB WDD3-PCB WDD7A-PCB                        
050200     WDR5-PCB WDG3-PCB WDR4-PCB 2501-PCB WDD9-PCB WDR2-PCB                
050300     WDK72-PCB.                                                           
050400                                                                          
050500 MAIN SECTION.                                                            
050600     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDF1-PCB             
050700     WDK7-PCB WDB6-PCB 2261-PCB WDD3-PCB WDD7A-PCB                        
050800     WDR5-PCB WDG3-PCB WDR4-PCB 2501-PCB WDD9-PCB WDR2-PCB                
050900     WDK72-PCB.                                                           
051000                                                                          
051100     PERFORM IMS-GET-MSG                                                  
051300     IF SEGMENT-FOUND                                                     
051400       PERFORM A-INIT                                                     
051500       PERFORM B-CHECK-KEYS                                               
051600       IF KEYS-OK AND INDATA-OK                                           
051700         PERFORM S10-AUTH-USER-CHECK                                      
051800         IF PASSED-SEC-CHECK AND INDATA-OK                                
051900           IF MFS-UPDATE OR MFS-UPD-V                                     
052000             PERFORM G-CHECK-INPUT                                        
052100             IF INDATA-OK                                                 
052200               PERFORM H-UPDATE                                           
052300             END-IF                                                       
052400           ELSE                                                           
052500             IF MFS-FIRST                                                 
052600               PERFORM C-FIRST-PAGE                                       
052700             ELSE                                                         
052800               PERFORM E-SAME-PAGE                                        
052900             END-IF                                                       
053000           END-IF                                                         
053100           IF INDATA-OK                                                   
053200             PERFORM F-READ-SHOW-INFO                                     
053300           END-IF                                                         
053400         END-IF                                                           
053500       END-IF                                                             
053600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O43101 + 4                      
053700       PERFORM IMS-INSERT-MSG                                             
053800     END-IF                                                               
053900                                                                          
054000     MOVE ZERO TO RETURN-CODE                                             
054100     GOBACK                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 A-INIT SECTION.                                                          
054500     MOVE 'A-INIT '   TO CURRENT-SECTION                                  
054600                                                                          
054700     IF MSG-DOUBLE-TRANSACTIONS                                           
054800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I43101                 
054900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
055000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
055100     ELSE                                                                 
055200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I43101                  
055300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
055400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
055500     END-IF                                                               
055600                                                                          
055700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
055800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
055900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
056000                                                                          
056100     MOVE LOW-VALUE TO MSG-AREA                                           
056200     MOVE 'W2O431N1' TO MFS-IDMOD                                         
056300     MOVE '2431' TO MOD-IDTRANS                                           
056400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
056500                                                                          
056600     MOVE SPACE              TO MED-IDMFSINF                              
056700                                MED-IDMFSFEL                              
056800                                                                          
056900     IF OWN-MID OR HELP-MID                                               
057000       CONTINUE                                                           
057100     ELSE                                                                 
057200       MOVE SPACE TO MFS-KDTRTYP                                          
057300       MOVE '7' TO MFS-IDPFK                                              
057400     END-IF                                                               
057500                                                                          
057600     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-CURRENT-DATE                  
057700     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
057800     MOVE WS-CURRENT-DATE(3:6)  TO DAT-I-TIDATUM                          
057900                                                                          
058000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
058100                         DAT-O-TIDATUM DAT-KDSVAR                         
058200                                                                          
058300     IF DAT-KDSVAR-OK                                                     
058400        MOVE DAT-TIAAVVD        TO WS-CURRENT-YYWWD                       
058500     END-IF                                                               
058600                                                                          
058700     ACCEPT DAGENS-DATUM            FROM DATE                             
058800     .                                                                    
058900     EJECT                                                                
059000 B-CHECK-KEYS SECTION.                                                    
059100     MOVE 'B-CHECK-KEYS '   TO CURRENT-SECTION                            
059200                                                                          
059300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
059400     MOVE '001'             TO MSGI-KDCALL                                
059500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
059600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
059700     MOVE '2431'            TO MSGI-IDTRANS                               
059800     IF OWN-MID                                                           
059900       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
060000       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
060100     END-IF                                                               
060200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
060300     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
060400                                                                          
060500*    - LANGUAGE TO BE USED BY MEDKONV                                     
060600     MOVE 'GB'            TO MED-IDSKYLT                                  
060700                                                                          
060800     MOVE YES             TO KEYS-SW                                      
060900                             INDATA-SW                                    
061000                                                                          
061100*    -- CHECK OF IDARTNR                                                  
061200     MOVE MFS-ERASE-FIELD    TO MOD-IDARTNR-IN                            
061300                                                                          
061400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
061500       MOVE '7'              TO MFS-IDPFK                                 
061600       MOVE SPACE            TO MFS-KDTRTYP                               
061700     END-IF                                                               
061800                                                                          
061900     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
062000     IF MSGI-IDARTNR NUMERIC                                              
062100       MOVE MSGI-IDARTNR     TO W-IDARTNR                                 
062200     ELSE                                                                 
062300        MOVE NOO             TO KEYS-SW                                   
062400     END-IF                                                               
062500                                                                          
062600                                                                          
062700* GET THE CONTROL DIGIT TO THE PART NUMBER                                
062800     IF KEYS-OK                                                           
062900        PERFORM BA-GET-CNTRL-DIGIT                                        
063000     END-IF                                                               
063100                                                                          
063200*    -- CHECK OF IDDC                                                     
063300     MOVE MFS-ERASE-FIELD  TO MOD-IDDC-IN                                 
063400                                                                          
063500     MOVE MSGI-IDDC-KEY    TO W-IDDC                                      
063600     IF MID-IDDC-IN NOT = ALL '+'                                         
063700       MOVE '7'            TO MFS-IDPFK                                   
063800       MOVE SPACE          TO MFS-KDTRTYP                                 
063900     END-IF                                                               
064000                                                                          
064100     IF MSGI-IDDC-KEY  = ALL '+' OR SPACE OR ZEROS                        
064200        MOVE NOO         TO KEYS-SW                                       
064300     END-IF                                                               
064400                                                                          
064500*** ONLY CHINA AND USA NDC IS ALLOWED AS KEY INPUT                        
064600     IF KEYS-OK                                                           
064700       PERFORM BB-CHECK-IDDC                                              
064800     END-IF                                                               
064900                                                                          
065000                                                                          
065100     IF GOOD-MID OR KEYS-OK                                               
065200       MOVE MSGI-IDARTNR       TO MOD-IDARTNR-UT                          
065300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZEROS BY SPACES           
065400       MOVE MSGI-IDDC-KEY      TO MOD-IDDC-UT                             
065500     END-IF                                                               
065600                                                                          
065700     IF KEYS-WRONG                                                        
065800       IF W-IDARTNR = ZEROS OR W-IDDC = SPACES                            
065900         MOVE ERR-KEY-MISSING   TO MED-IDMFSFEL                           
066000         MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR                            
066100       ELSE                                                               
066200         MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                           
066300       END-IF                                                             
066400       CALL WMEDKONV USING MED-WMEDAREA                                   
066500       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
066600       PERFORM MFS-ERASE-FIELD-IN                                         
066700       PERFORM MFS-ERASE-FIELD-OUT                                        
066800       IF LOCK-FIELD                                                      
066900         PERFORM MFS-SET-ATTR-FOR-REFILL-PART                             
067000         MOVE 'WRONG DC'      TO MOD-TEMFSFEL                             
067100       END-IF                                                             
067200     END-IF                                                               
067300                                                                          
067400     .                                                                    
067500     EJECT                                                                
067600 BA-GET-CNTRL-DIGIT SECTION.                                              
067700     MOVE 'BA-GET-CNTRL-DIGIT '   TO CURRENT-SECTION                      
067800                                                                          
067900     PERFORM IMS-GU-WDK601                                                
068000     IF SEGMENT-FOUND                                                     
068100        MOVE ART-REKSIFFR      TO MOD-REKSIFFR                            
068200        MOVE WS-DASH           TO MOD-DASH-1                              
068300        IF ART-TIFINLV > 50000                                            
068400          MOVE ZERO            TO SPAR-WDK601-TIFINLV                     
068500        ELSE                                                              
068600          MOVE ART-TIFINLV     TO SPAR-WDK601-TIFINLV                     
068700        END-IF                                                            
068800     ELSE                                                                 
068900        MOVE NOO               TO INDATA-SW                               
069000        MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                            
069100        CALL WMEDKONV USING MED-WMEDAREA                                  
069200        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
069300        PERFORM MFS-ERASE-FIELD-IN                                        
069400        PERFORM MFS-ERASE-FIELD-OUT                                       
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 BB-CHECK-IDDC SECTION.                                                   
069900     MOVE 'BB-CHECK-IDDC '   TO CURRENT-SECTION                           
070000                                                                          
070100     IF W-IDDC NOT = SPACES                                               
070200        PERFORM IMS-GU-WDB601                                             
070300        IF SEGMENT-FOUND                                                  
070400           IF DCS-NDC-CN                                                  
070500           OR (DCS-NDC-NA AND DCS-USA)                                    
070600             IF DCS-IDDC = '92'                                           
070700               MOVE NOO   TO KEYS-SW                                      
070800               MOVE YES   TO LOCK-FIELD-SW                                
070900             ELSE                                                         
071000               MOVE YES   TO KEYS-SW                                      
071100             END-IF                                                       
071200           ELSE                                                           
071300              MOVE NOO   TO KEYS-SW                                       
071400              MOVE YES   TO LOCK-FIELD-SW                                 
071500           END-IF                                                         
071600        ELSE                                                              
071700           MOVE NOO      TO KEYS-SW                                       
071800           MOVE YES      TO LOCK-FIELD-SW                                 
071900        END-IF                                                            
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 C-FIRST-PAGE SECTION.                                                    
072400     MOVE 'C-FIRST-PAGE '   TO CURRENT-SECTION                            
072500                                                                          
072600     PERFORM MFS-ERASE-FIELD-IN                                           
072700     .                                                                    
072800     EJECT                                                                
072900 E-SAME-PAGE SECTION.                                                     
073000     MOVE 'E-SAME-PAGE '    TO CURRENT-SECTION                            
073100                                                                          
073200     IF OWN-MID OR HELP-MID                                               
073300       IF MID-INPUT = ALL '+'                                             
073400                                                                          
073500         PERFORM MFS-ERASE-FIELD-IN                                       
073600       ELSE                                                               
073700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
073800         CALL WMEDKONV USING MED-WMEDAREA                                 
073900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
074000         PERFORM EA-MID-INDATA-TO-MOD                                     
074100       END-IF                                                             
074200     ELSE                                                                 
074300       PERFORM MFS-ERASE-FIELD-IN                                         
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 EA-MID-INDATA-TO-MOD SECTION.                                            
074800     MOVE 'EA-MID-INDATA-TO-MOD '   TO CURRENT-SECTION                    
074900                                                                          
075000     IF MID-IDANSK-IN = ALL '+'                                           
075100       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-IN                        
075200     ELSE                                                                 
075300       MOVE MID-IDANSK-IN         TO MOD-IDANSK-IN                        
075400       MOVE MFS-ADD-READ-FIELD    TO MOD-IDANSK-ATTR-IN                   
075500     END-IF                                                               
075600                                                                          
075700     IF MID-IDPLANGR-AG-IN = ALL '+'                                      
075800       MOVE MFS-ERASE-FIELD       TO MOD-IDPLANGR-AG-IN                   
075900     ELSE                                                                 
076000       MOVE MID-IDPLANGR-AG-IN    TO MOD-IDPLANGR-AG-IN                   
076100       MOVE MFS-ADD-READ-FIELD    TO MOD-IDPLANGR-AG-ATTR-IN              
076200     END-IF                                                               
076300                                                                          
076400     IF MID-IDINK-IN = ALL '+'                                            
076500       MOVE MFS-ERASE-FIELD       TO MOD-IDINK-IN                         
076600     ELSE                                                                 
076700       MOVE MID-IDINK-IN          TO MOD-IDINK-IN                         
076800       MOVE MFS-ADD-READ-FIELD    TO MOD-IDINK-ATTR-IN                    
076900     END-IF                                                               
077000                                                                          
077100     IF MID-IDLEVNR-FRAM-IN = ALL '+'                                     
077200       MOVE MFS-ERASE-FIELD       TO MOD-IDLEVNR-FRAM-IN                  
077300     ELSE                                                                 
077400       MOVE MID-IDLEVNR-FRAM-IN   TO MOD-IDLEVNR-FRAM-IN                  
077500       MOVE MFS-ADD-READ-FIELD    TO MOD-IDLEVNR-FRAM-ATTR-IN             
077600     END-IF                                                               
077700                                                                          
077800     IF MID-IDLEVNR-SHIP-FRAM-IN = ALL '+'                                
077900       MOVE MFS-ERASE-FIELD       TO MOD-IDLEVNR-SHIP-FRAM-IN             
078000     ELSE                                                                 
078100       MOVE MID-IDLEVNR-SHIP-FRAM-IN TO MOD-IDLEVNR-SHIP-FRAM-IN          
078200       MOVE MFS-ADD-READ-FIELD    TO MOD-IDLEVNR-SHIP-FRAM-ATTR-IN        
078300     END-IF                                                               
078400                                                                          
078500     IF MID-TILEVDAT-IN = ALL '+'                                         
078600       MOVE MFS-ERASE-FIELD       TO MOD-TILEVDAT-IN                      
078700     ELSE                                                                 
078800       MOVE MID-TILEVDAT-IN       TO MOD-TILEVDAT-IN                      
078900       MOVE MFS-ADD-READ-FIELD    TO MOD-TILEVDAT-ATTR-IN                 
079000     END-IF                                                               
079100                                                                          
079200     IF MID-KVSPANT-IN = ALL '+'                                          
079300       MOVE MFS-ERASE-FIELD       TO MOD-KVSPANT-IN                       
079400     ELSE                                                                 
079500       MOVE MID-KVSPANT-IN        TO MOD-KVSPANT-IN                       
079600       MOVE MFS-ADD-READ-FIELD    TO MOD-KVSPANT-ATTR-IN                  
079700     END-IF                                                               
079800                                                                          
079900     IF MID-TIREFSTO-LOC-IN = ALL '+'                                     
080000       MOVE MFS-ERASE-FIELD       TO MOD-TIREFSTO-LOC-IN                  
080100     ELSE                                                                 
080200       IF MID-TIREFSTO-LOC-IN = '0     ' OR '00    ' OR                   
080300                              = '000   ' OR '0000  ' OR                   
080400                              = '00000 '                                  
080500          MOVE ZERO               TO MID-TIREFSTO-LOC-IN                  
080600*  BARA FÖR SÄKERHETS SKULL :-)                                           
080700       END-IF                                                             
080800       MOVE MID-TIREFSTO-LOC-IN   TO MOD-TIREFSTO-LOC-IN                  
080900       MOVE MFS-ADD-READ-FIELD    TO MOD-TIREFSTO-LOC-ATTR-IN             
081000     END-IF                                                               
081100                                                                          
081200     IF MID-FLJIT-IN = ALL '+'                                            
081300       MOVE MFS-ERASE-FIELD       TO MOD-FLJIT-IN                         
081400     ELSE                                                                 
081500       MOVE MID-FLJIT-IN          TO MOD-FLJIT-IN                         
081600       MOVE MFS-ADD-READ-FIELD    TO MOD-FLJIT-ATTR-IN                    
081700     END-IF                                                               
081800                                                                          
081900     IF MID-FLWILSON-IN = ALL '+'                                         
082000       MOVE MFS-ERASE-FIELD       TO MOD-FLWILSON-IN                      
082100     ELSE                                                                 
082200       MOVE MID-FLWILSON-IN       TO MOD-FLWILSON-IN                      
082300       MOVE MFS-ADD-READ-FIELD    TO MOD-FLWILSON-ATTR-IN                 
082400     END-IF                                                               
082500                                                                          
082600     IF MID-IDREFTAB-IN = ALL '+'                                         
082700       MOVE MFS-ERASE-FIELD       TO MOD-IDREFTAB-IN                      
082800     ELSE                                                                 
082900       MOVE MID-IDREFTAB-IN       TO MOD-IDREFTAB-IN                      
083000       MOVE MFS-ADD-READ-FIELD    TO MOD-IDREFTAB-ATTR-IN                 
083100     END-IF                                                               
083200                                                                          
083300     IF MID-KVSLAGER-IN = ALL '+'                                         
083400       MOVE MFS-ERASE-FIELD       TO MOD-KVSLAGER-IN                      
083500     ELSE                                                                 
083600       MOVE MID-KVSLAGER-IN       TO MOD-KVSLAGER-IN                      
083700       MOVE MFS-ADD-READ-FIELD    TO MOD-KVSLAGER-ATTR-IN                 
083800     END-IF                                                               
083900                                                                          
084000     IF MID-TIMANSEC-IN = ALL '+'                                         
084100       MOVE MFS-ERASE-FIELD       TO MOD-TIMANSEC-IN                      
084200     ELSE                                                                 
084300       MOVE MID-TIMANSEC-IN                                               
084400                                  TO MOD-TIMANSEC-IN                      
084500       MOVE MFS-ADD-READ-FIELD    TO MOD-TIMANSEC-ATTR-IN                 
084600     END-IF                                                               
084700                                                                          
084800     IF MID-KVPALL-IN = ALL '+'                                           
084900       MOVE MFS-ERASE-FIELD       TO MOD-KVPALL-IN                        
085000     ELSE                                                                 
085100       MOVE MID-KVPALL-IN         TO MOD-KVPALL-IN                        
085200       MOVE MFS-ADD-READ-FIELD    TO MOD-KVPALL-ATTR-IN                   
085300     END-IF                                                               
085400                                                                          
085500     IF MID-KDAVT-IN = ALL '+'                                            
085600       MOVE MFS-ERASE-FIELD       TO MOD-KDAVT-IN                         
085700     ELSE                                                                 
085800       MOVE MID-KDAVT-IN          TO MOD-KDAVT-IN                         
085900       MOVE MFS-ADD-READ-FIELD    TO MOD-KDAVT-ATTR-IN                    
086000     END-IF                                                               
086100                                                                          
086200     IF MID-KVREFBER-IN = ALL '+'                                         
086300       MOVE MFS-ERASE-FIELD       TO MOD-KVREFBER-IN                      
086400     ELSE                                                                 
086500       MOVE MID-KVREFBER-IN       TO MOD-KVREFBER-IN                      
086600       MOVE MFS-ADD-READ-FIELD    TO MOD-KVREFBER-ATTR-IN                 
086700     END-IF                                                               
086800                                                                          
086900     IF MID-TIREFPAF-IN = ALL '+'                                         
087000       MOVE MFS-ERASE-FIELD       TO MOD-TIREFPAF-IN                      
087100     ELSE                                                                 
087200       MOVE MID-TIREFPAF-IN       TO MOD-TIREFPAF-IN                      
087300       MOVE MFS-ADD-READ-FIELD    TO MOD-TIREFPAF-ATTR-IN                 
087400     END-IF                                                               
087500                                                                          
087600     IF MID-KVULOAD-IN = ALL '+'                                          
087700       MOVE MFS-ERASE-FIELD       TO MOD-KVULOAD-IN                       
087800     ELSE                                                                 
087900       MOVE MID-KVULOAD-IN        TO MOD-KVULOAD-IN                       
088000       MOVE MFS-ADD-READ-FIELD    TO MOD-KVULOAD-ATTR-IN                  
088100     END-IF                                                               
088200                                                                          
088300     IF MID-TISTODAT-LARM-IN = ALL '+'                                    
088400       MOVE MFS-ERASE-FIELD       TO MOD-TISTODAT-LARM-IN                 
088500     ELSE                                                                 
088600       MOVE MID-TISTODAT-LARM-IN  TO MOD-TISTODAT-LARM-IN                 
088700       MOVE MFS-ADD-READ-FIELD    TO MOD-TISTODAT-LARM-ATTR-IN            
088800     END-IF                                                               
088900                                                                          
089000     IF MID-KVVECKOR-LT-IN = ALL '+'                                      
089100       MOVE MFS-ERASE-FIELD       TO MOD-KVVECKOR-LT-IN                   
089200     ELSE                                                                 
089300       MOVE MID-KVVECKOR-LT-IN    TO MOD-KVVECKOR-LT-IN                   
089400       MOVE MFS-ADD-READ-FIELD    TO MOD-KVVECKOR-LT-ATTR-IN              
089500     END-IF                                                               
089600                                                                          
089700     IF MID-TIMANLED-IN = ALL '+'                                         
089800       MOVE MFS-ERASE-FIELD       TO MOD-TIMANLED-IN                      
089900     ELSE                                                                 
090000       MOVE MID-TIMANLED-IN       TO MOD-TIMANLED-IN                      
090100       MOVE MFS-ADD-READ-FIELD    TO MOD-TIMANLED-ATTR-IN                 
090200     END-IF                                                               
090300                                                                          
090400     IF MID-KVSLUTKP-IN = ALL '+'                                         
090500       MOVE MFS-ERASE-FIELD       TO MOD-KVSLUTKP-IN                      
090600     ELSE                                                                 
090700       MOVE MID-KVSLUTKP-IN       TO MOD-KVSLUTKP-IN                      
090800       MOVE MFS-ADD-READ-FIELD    TO MOD-KVSLUTKP-ATTR-IN                 
090900     END-IF                                                               
091000                                                                          
091100     IF MID-TISLUTKP-IN = ALL '+'                                         
091200       MOVE MFS-ERASE-FIELD       TO MOD-TISLUTKP-IN                      
091300     ELSE                                                                 
091400       MOVE MID-TISLUTKP-IN                                               
091500                                  TO MOD-TISLUTKP-IN                      
091600       MOVE MFS-ADD-READ-FIELD    TO MOD-TISLUTKP-ATTR-IN                 
091700     END-IF                                                               
091800                                                                          
091900     IF MID-KDOPPLAN-IN = ALL '+'                                         
092000       MOVE MFS-ERASE-FIELD       TO MOD-KDOPPLAN-IN                      
092100     ELSE                                                                 
092200       MOVE MID-KDOPPLAN-IN       TO MOD-KDOPPLAN-IN                      
092300       MOVE MFS-ADD-READ-FIELD    TO MOD-KDOPPLAN-ATTR-IN                 
092400     END-IF                                                               
092500                                                                          
092600     IF MID-DAPUBL-IN = ALL '+'                                           
092700       MOVE MFS-ERASE-FIELD       TO MOD-DAPUBL-IN                        
092800     ELSE                                                                 
092900       MOVE MID-DAPUBL-IN         TO MOD-DAPUBL-IN                        
093000       MOVE MFS-ADD-READ-FIELD    TO MOD-DAPUBL-ATTR-IN                   
093100     END-IF                                                               
093200                                                                          
093300     PERFORM VARYING INDX FROM 1 BY 1                                     
093400     UNTIL INDX > 5                                                       
093500       IF MID-DISP-DAY(INDX) = ALL '+'                                    
093600          MOVE MFS-DO-NOT-TOUCH-FIELD                                     
093700                                     TO MOD-DISP-DAY(INDX)                
093800       ELSE                                                               
093900          MOVE MID-DISP-DAY(INDX)    TO MOD-DISP-DAY(INDX)                
094000          MOVE MFS-ADD-READ-FIELD    TO MOD-DISP-DAY-ATTR-IN(INDX)        
094100       END-IF                                                             
094200     END-PERFORM                                                          
094300                                                                          
094400     IF MID-NOTES-1 = ALL '+'                                             
094500       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-NOTES-1                       
094600     ELSE                                                                 
094700       MOVE MID-NOTES-1              TO MOD-NOTES-1                       
094800       MOVE MFS-ADD-READ-FIELD       TO MOD-NOTES-1-ATTR-IN               
094900     END-IF                                                               
095000                                                                          
095100     IF MID-NOTES-2 = ALL '+'                                             
095200       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-NOTES-2                       
095300     ELSE                                                                 
095400       MOVE MID-NOTES-2              TO MOD-NOTES-2                       
095500       MOVE MFS-ADD-READ-FIELD       TO MOD-NOTES-2-ATTR-IN               
095600     END-IF                                                               
095700     .                                                                    
095800     EJECT                                                                
095900 F-READ-SHOW-INFO SECTION.                                                
096000     MOVE 'F-READ-SHOW-INFO '   TO CURRENT-SECTION                        
096100                                                                          
096200     PERFORM FA-READ-BASICDATA                                            
096300     IF REFILL-PART                                                       
096400        IF MOD-TEMFSINF(1:3) NOT = INF-PRESS-PF11                         
096500          IF MED-IDMFSINF = SPACE                                         
096600            MOVE INF-REFILL-PART    TO MED-IDMFSINF                       
096700            CALL WMEDKONV        USING MED-WMEDAREA                       
096800            MOVE MED-MFSINF         TO MOD-TEMFSINF                       
096900          END-IF                                                          
097000        END-IF                                                            
097100        PERFORM MFS-SET-ATTR-FOR-REFILL-PART                              
097200        MOVE MFS-OPEN-NUM-FIELD   TO MOD-DAPUBL-ATTR-IN                   
097300                                     MOD-TILEVDAT-ATTR-IN                 
097400        MOVE MFS-OPEN-ALPHA-FIELD TO MOD-IDLEVNR-FRAM-ATTR-IN             
097500                                     MOD-IDLEVNR-SHIP-FRAM-ATTR-IN        
097600        MOVE MFS-OPEN-ALPHA-FIELD TO MOD-NOTES-1-ATTR-IN                  
097700                                     MOD-NOTES-2-ATTR-IN                  
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100 FA-READ-BASICDATA SECTION.                                               
098200     MOVE 'FA-READ-BASICDATA '   TO CURRENT-SECTION                       
098300                                                                          
098400     PERFORM FAA-GET-DESC                                                 
098500     PERFORM FAB-GET-WDK7-DATA                                            
098600                                                                          
098700     IF  MFS-QUERY                                                        
098800     AND MFS-ENTER                                                        
098900     AND MID-NOTES-DATA NOT = ALL '+'                                     
099000         CONTINUE                                                         
099100     ELSE                                                                 
099200       MOVE W-IDDC                TO W-IDDC-2261                          
099300       PERFORM IMS-GHU-WDGX2262                                           
099400       IF SEGMENT-FOUND                                                   
099500          MOVE 2262-TEREFMED(1:36)  TO MOD-NOTES-1                        
099600          MOVE 2262-TEREFMED(37:39) TO MOD-NOTES-2                        
099700       ELSE                                                               
099800          MOVE MFS-ERASE-FIELD    TO MOD-NOTES-1                          
099900                                     MOD-NOTES-2                          
100000       END-IF                                                             
100100       MOVE MFS-ADD-LAES-IN-FAELT                                         
100200                             TO MOD-NOTES-1-ATTR-IN                       
100300                                MOD-NOTES-2-ATTR-IN                       
100400     END-IF                                                               
100500                                                                          
100600     MOVE DCS-IDLANDX2          TO W-IDLANDX2                             
100700     PERFORM IMS-GHU-WDK712                                               
100800     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
100900     MOVE LART-DAPUBL(3:6)      TO DAT-I-TIDATUM                          
101000                                                                          
101100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
101200                         DAT-O-TIDATUM DAT-KDSVAR                         
101300                                                                          
101400      IF DAT-KDSVAR-OK                                                    
101500         MOVE DAT-TIAAVVD       TO MOD-DAPUBL-UT                          
101600      END-IF                                                              
101700     .                                                                    
101800     EJECT                                                                
101900 FAA-GET-DESC SECTION.                                                    
102000     MOVE 'FAA-GET-DESC '    TO CURRENT-SECTION                           
102100                                                                          
102200     MOVE 'GB'                   TO W-IDSKYLT                             
102300     PERFORM IMS-GU-WDD311                                                
102400     IF SEGMENT-FOUND                                                     
102500        MOVE TEXT-BEART          TO MOD-BEART-ENG                         
102600     ELSE                                                                 
102700        MOVE SPACES              TO MOD-BEART-ENG                         
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 FAB-GET-WDK7-DATA SECTION.                                               
103200     MOVE 'FAB-GET-WDK7-DATA '   TO CURRENT-SECTION                       
103300                                                                          
103400     MOVE YES       TO VISA-INFO-SW                                       
103500                                                                          
103600     IF MFS-UPDATE OR MFS-UPD-V                                           
103700       PERFORM IMS-GU-WDK711                                              
103800       IF SEGMENT-FOUND                                                   
103900         MOVE YES   TO VISA-INFO-SW                                       
104000       ELSE                                                               
104100         MOVE NOO   TO VISA-INFO-SW                                       
104200       END-IF                                                             
104300     END-IF                                                               
104400                                                                          
104500     IF VISA-INFO-OK                                                      
104600        IF SLAG-FLWILSON = YES OR 'Y'                                     
104700           MOVE 'Y'              TO MOD-FLWILSON-UT                       
104800        ELSE                                                              
104900           MOVE SLAG-FLWILSON    TO MOD-FLWILSON-UT                       
105000        END-IF                                                            
105100        MOVE SLAG-IDREFTAB       TO MOD-IDREFTAB-UT                       
105200        MOVE SLAG-KVREFBER       TO MOD-KVREFBER-UT                       
105300        IF SLAG-TIREFPAF > ZERO                                           
105400           MOVE SLAG-TIREFPAF    TO MOD-TIREFPAF-UT                       
105500        ELSE                                                              
105600           MOVE MFS-RENSA-FAELT  TO MOD-TIREFPAF-UT                       
105700        END-IF                                                            
105800                                                                          
105900        IF SLAG-TISTODAT-LARM > ZERO                                      
106000           MOVE SLAG-TISTODAT-LARM TO MOD-TISTODAT-LARM-UT                
106100        ELSE                                                              
106200           MOVE MFS-RENSA-FAELT  TO MOD-TISTODAT-LARM-UT                  
106300        END-IF                                                            
106400                                                                          
106500        IF SLAG-IDDC-REF = SPACES                                         
106600           PERFORM FABA-GET-PROCUREMENT-INFO                              
106700        ELSE                                                              
106800           PERFORM MFS-ERASE-WDK722-FIELD-OUT                             
106900           PERFORM FABB-GET-PROC-REFILL-INFO                              
107000           MOVE YES              TO REFILL-PART-SW                        
107100        END-IF                                                            
107200     ELSE                                                                 
107300        MOVE MFS-RENSA-FAELT     TO MOD-FLWILSON-UT                       
107400                                    MOD-IDREFTAB-UT                       
107500                                    MOD-KVREFBER-UT                       
107600                                    MOD-TIREFPAF-UT                       
107700                                    MOD-TIREFPAF-UT                       
107800                                                                          
107900        PERFORM MFS-ERASE-WDK722-FIELD-OUT                                
108000     END-IF                                                               
108100     .                                                                    
108200     EJECT                                                                
108300 FABA-GET-PROCUREMENT-INFO SECTION.                                       
108400     MOVE 'FABA-GET-PROCUREMENT-INFO'  TO CURRENT-SECTION                 
108500                                                                          
108600     PERFORM IMS-GNP-WDK722                                               
108700     IF SEGMENT-FOUND                                                     
108800        MOVE XLAG-IDANSK         TO MOD-IDANSK-UT                         
108900        MOVE XLAG-IDPLANGR-AG    TO MOD-IDPLANGR-AG-UT                    
109000        MOVE XLAG-IDINK          TO MOD-IDINK-UT                          
109100        MOVE XLAG-IDLEVNR-FRAM   TO MOD-IDLEVNR-FRAM-UT                   
109200        MOVE XLAG-IDLEVNR-SHIP-FRAM TO MOD-IDLEVNR-SHIP-FRAM-UT           
109300        MOVE XLAG-KVSPANT        TO MOD-KVSPANT-UT                        
109400        IF XLAG-FLJIT = YES OR 'Y'                                        
109500           MOVE 'Y'              TO MOD-FLJIT-UT                          
109600        ELSE                                                              
109700           MOVE XLAG-FLJIT       TO MOD-FLJIT-UT                          
109800        END-IF                                                            
109900        MOVE XLAG-KVSLAGER       TO MOD-KVSLAGER-UT                       
110000        MOVE XLAG-KVPALL         TO MOD-KVPALL-UT                         
110100        MOVE XLAG-KDAVT          TO MOD-KDAVT-UT                          
110200        MOVE XLAG-KVULOAD        TO MOD-KVULOAD-UT                        
110300        MOVE XLAG-KVVECKOR-LT    TO MOD-KVVECKOR-LT-UT                    
110400        MOVE XLAG-KVSLUTKP       TO MOD-KVSLUTKP-UT                       
110500        IF XLAG-KDOPPLAN = YES OR 'Y'                                     
110600           MOVE 'Y'              TO MOD-KDOPPLAN-UT                       
110700        ELSE                                                              
110800           MOVE XLAG-KDOPPLAN    TO MOD-KDOPPLAN-UT                       
110900        END-IF                                                            
111000        PERFORM FABAA-GET-DISPATCH-DAY                                    
111100                                                                          
111200        IF XLAG-TILEVDAT > 0                                              
111300           MOVE XLAG-TILEVDAT    TO MOD-TILEVDAT-UT                       
111400        ELSE                                                              
111500           MOVE MFS-RENSA-FAELT  TO MOD-TILEVDAT-UT                       
111600        END-IF                                                            
111700                                                                          
111800        IF XLAG-TIMANSEC > 0                                              
111900           MOVE XLAG-TIMANSEC    TO MOD-TIMANSEC-UT                       
112000        ELSE                                                              
112100           MOVE MFS-RENSA-FAELT  TO MOD-TIMANSEC-UT                       
112200        END-IF                                                            
112300                                                                          
112400        IF XLAG-TISLUTKP > 0                                              
112500           MOVE XLAG-TISLUTKP    TO DAT-I-TIDATUM                         
112600           MOVE 'AAMMDD'         TO DAT-KDDATFORM                         
112700           CALL WDATKONV      USING DAT-KDDATFORM                         
112800                                    DAT-I-TIDATUM                         
112900                                    DAT-O-TIDATUM                         
113000                                    DAT-KDSVAR                            
113100           MOVE DAT-TIAAMMDD     TO MOD-TISLUTKP-UT                       
113200        ELSE                                                              
113300           MOVE MFS-RENSA-FAELT  TO MOD-TISLUTKP-UT                       
113400        END-IF                                                            
113500                                                                          
113600        IF XLAG-TIMANLED > 0                                              
113700           MOVE XLAG-TIMANLED    TO MOD-TIMANLED-UT                       
113800        ELSE                                                              
113900           MOVE MFS-RENSA-FAELT  TO MOD-TIMANLED-UT                       
114000        END-IF                                                            
114100                                                                          
114200        IF XLAG-TIREFSTO-LOC > 0                                          
114300           MOVE XLAG-TIREFSTO-LOC                                         
114400                                 TO MOD-TIREFSTO-LOC-UT                   
114500        ELSE                                                              
114600           MOVE MFS-RENSA-FAELT  TO MOD-TIREFSTO-LOC-UT                   
114700        END-IF                                                            
114800     ELSE                                                                 
114900        PERFORM MFS-ERASE-WDK722-FIELD-OUT                                
115000     END-IF                                                               
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115400 FABAA-GET-DISPATCH-DAY SECTION.                                          
115500     MOVE 'FABAA-GET-DISPATCH-DAY '  TO CURRENT-SECTION                   
115600                                                                          
115700        MOVE +1                  TO INDX                                  
115800        IF XLAG-TILEVDAG (1) = 1                                          
115900           MOVE 'MO'             TO MOD-SAVE-DISP-DAY(INDX)               
116000                                                                          
116100           ADD +1                TO INDX                                  
116200        END-IF                                                            
116300        IF XLAG-TILEVDAG (2) = 2                                          
116400           MOVE 'TU'             TO MOD-SAVE-DISP-DAY(INDX)               
116500           ADD +1                TO INDX                                  
116600        END-IF                                                            
116700        IF XLAG-TILEVDAG (3) = 3                                          
116800           MOVE 'WE'             TO MOD-SAVE-DISP-DAY(INDX)               
116900           ADD +1                TO INDX                                  
117000        END-IF                                                            
117100        IF XLAG-TILEVDAG (4) = 4                                          
117200           MOVE 'TH'             TO MOD-SAVE-DISP-DAY(INDX)               
117300           ADD +1                TO INDX                                  
117400        END-IF                                                            
117500        IF XLAG-TILEVDAG (5) = 5                                          
117600           MOVE 'FR'             TO MOD-SAVE-DISP-DAY(INDX)               
117700        END-IF                                                            
117800        IF INDX < MAX-DAYS-INDX                                           
117900          ADD 1                     TO INDX                               
118000          PERFORM UNTIL INDX > MAX-DAYS-INDX                              
118100            MOVE MFS-ERASE-FIELD    TO MOD-SAVE-DISP-DAY(INDX)            
118200            ADD 1                   TO INDX                               
118300          END-PERFORM                                                     
118400        END-IF                                                            
118500     IF  MFS-QUERY                                                        
118600     AND MFS-ENTER                                                        
118700     AND MID-DISP-DAY-INPUT NOT = ALL '+'                                 
118800        CONTINUE                                                          
118900     ELSE                                                                 
119000        PERFORM                                                           
119100        VARYING INDX FROM 1 BY 1                                          
119200          UNTIL INDX > MAX-DAYS-INDX                                      
119300          MOVE MOD-SAVE-DISP-DAY(INDX)                                    
119400                                TO MOD-DISP-DAY (INDX)                    
119500        END-PERFORM                                                       
119600     END-IF                                                               
119700     .                                                                    
119800     EJECT                                                                
119900 FABB-GET-PROC-REFILL-INFO SECTION.                                       
120000     MOVE 'FABB-GET-PROC-REFILL-INFO'  TO CURRENT-SECTION                 
120100                                                                          
120200     PERFORM IMS-GNP-WDK722                                               
120300     IF SEGMENT-FOUND                                                     
120400        MOVE XLAG-IDLEVNR-FRAM      TO MOD-IDLEVNR-FRAM-UT                
120500        MOVE XLAG-IDLEVNR-SHIP-FRAM TO MOD-IDLEVNR-SHIP-FRAM-UT           
120600                                                                          
120700        IF XLAG-TILEVDAT > 0                                              
120800           MOVE XLAG-TILEVDAT       TO MOD-TILEVDAT-UT                    
120900        END-IF                                                            
121000     END-IF                                                               
121100     .                                                                    
121200     EJECT                                                                
121300 G-CHECK-INPUT SECTION.                                                   
121400     MOVE 'G-CHECK-INPUT '  TO CURRENT-SECTION                            
121500                                                                          
121600     MOVE YES               TO INDATA-SW                                  
121700     MOVE ZERO              TO MED-IDMFSFEL                               
121800     MOVE NOO               TO CHECK-LOCAL-PART-SW                        
121900                                                                          
122000     IF MID-INPUT = ALL '+'                                               
122100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
122200       CALL WMEDKONV          USING MED-WMEDAREA                          
122300       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
122400       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
122500       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
122600       MOVE NOO                  TO INDATA-SW                             
122700     ELSE                                                                 
122800       PERFORM GA-CHECK-DC-FTG-USER                                       
122900       IF INDATA-OK                                                       
123000          PERFORM IMS-GNP-WDK722                                          
123100          IF SEGMENT-FOUND                                                
123200             MOVE YES   TO WDK722-FINNS-SW                                
123300          ELSE                                                            
123400             MOVE NOO   TO WDK722-FINNS-SW                                
123500          END-IF                                                          
123600          PERFORM GB-CHECK-MID-INPUT-LINE-1                               
123700       END-IF                                                             
123800                                                                          
123900       IF INDATA-OK                                                       
124000         PERFORM GC-CHECK-MID-INPUT-LINE-2                                
124100       END-IF                                                             
124200                                                                          
124300       IF INDATA-OK                                                       
124400         PERFORM GD-CHECK-MID-INPUT-LINE-3                                
124500       END-IF                                                             
124600                                                                          
124700       IF INDATA-OK                                                       
124800         PERFORM GE-CHECK-MID-INPUT-LINE-4                                
124900       END-IF                                                             
125000                                                                          
125100       IF INDATA-OK                                                       
125200         PERFORM GF-CHECK-MID-INPUT-LINE-5                                
125300       END-IF                                                             
125400                                                                          
125500       IF INDATA-OK                                                       
125600         PERFORM GG-CHECK-MID-INPUT-LINE-6                                
125700       END-IF                                                             
125800                                                                          
125900       IF INDATA-OK                                                       
126000         PERFORM GH-CHECK-MID-INPUT-LINE-7                                
126100       END-IF                                                             
126200                                                                          
126300* FIELDS CAN BE UPDATED ONLY IF ITS A LOCALLY SUPPLIED PART               
126400       IF CHECK-LOCAL-PART                                                
126500         IF WS-IDDC-REF NOT = SPACE                                       
126600           MOVE ERR-NOT-LOCAL-SOURCED TO MED-IDMFSFEL                     
126700           MOVE NOO                   TO INDATA-SW                        
126800         END-IF                                                           
126900       END-IF                                                             
127000                                                                          
127100       IF INDATA-WRONG                                                    
127200         IF MED-IDMFSFEL = ERR-FUTURE-DATE OR                             
127300                           ERR-ONE-YEAR-IN-FUTURE OR                      
127400                           ERR-USER-NOT-AUTH OR                           
127500                           ERR-UPDATE-NOT-ALLOWED OR                      
127600                           ERR-PART-MISSING-IN-DC                         
127700           CONTINUE                                                       
127800         ELSE                                                             
127900           IF MED-IDMFSFEL = ZERO                                         
128000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
128100           END-IF                                                         
128200         END-IF                                                           
128300         CALL WMEDKONV             USING MED-WMEDAREA                     
128400         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
128500         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
128600         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
128700       END-IF                                                             
128800     END-IF                                                               
128900     .                                                                    
129000     EJECT                                                                
129100 GA-CHECK-DC-FTG-USER SECTION.                                            
129200     MOVE 'GA-CHECK-DC-FTG-USER ' TO CURRENT-SECTION                      
129300                                                                          
129400     PERFORM IMS-GU-WDB601                                                
129500     IF SEGMENT-FOUND                                                     
129600        IF DCS-NDC-CN                                                     
129700        OR (DCS-NDC-NA AND DCS-USA)                                       
129800           MOVE MSGI-IDFTG          TO WS-IDFTG                           
129900           IF (DCS-NDC-CN AND IDFTG-CN)                                   
130000           OR (DCS-NDC-NA AND IDFTG-US)                                   
130100           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
130200             CONTINUE                                                     
130300           ELSE                                                           
130400             MOVE ERR-USER-NOT-AUTH     TO MED-IDMFSFEL                   
130500             MOVE NOO                   TO INDATA-SW                      
130600           END-IF                                                         
130700        ELSE                                                              
130800           MOVE ERR-USER-NOT-AUTH       TO MED-IDMFSFEL                   
130900           MOVE NOO                     TO INDATA-SW                      
131000        END-IF                                                            
131100     ELSE                                                                 
131200        MOVE ERR-USER-NOT-AUTH          TO MED-IDMFSFEL                   
131300        MOVE NOO                        TO INDATA-SW                      
131400     END-IF                                                               
131500     .                                                                    
131600     EJECT                                                                
131700 GB-CHECK-MID-INPUT-LINE-1 SECTION.                                       
131800     MOVE 'GB-CHECK-MID-INPUT-LINE-1'   TO CURRENT-SECTION                
131900                                                                          
132000     IF MID-IDANSK-IN NOT = ALL '+'                                       
132100        INSPECT MID-IDANSK-IN REPLACING LEADING SPACES BY ZEROS           
132200        IF MID-IDANSK-IN NUMERIC                                          
132300           MOVE MFS-NUM-FIELD-OK    TO MOD-IDANSK-ATTR-IN                 
132400           MOVE YES                 TO CHECK-LOCAL-PART-SW                
132500        ELSE                                                              
132600           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-ATTR-IN                 
132700           MOVE NOO                 TO INDATA-SW                          
132800        END-IF                                                            
132900     END-IF                                                               
133000                                                                          
133100     IF MID-IDANSK-IN NOT = ALL '+'                                       
133200       IF WDK722-FINNS                                                    
133300          MOVE XLAG-IDPLANGR-AG     TO SPAR-IDPLANGR-AG                   
133400       ELSE                                                               
133500          MOVE ZERO                 TO SPAR-IDPLANGR-AG                   
133600       END-IF                                                             
133700                                                                          
133800       IF MID-IDPLANGR-AG-IN NOT = ALL '+'                                
133900         MOVE MID-IDPLANGR-AG-IN    TO SPAR-IDPLANGR-AG                   
134000       END-IF                                                             
134100                                                                          
134200       IF SPAR-IDPLANGR-AG = 9                                            
134300       OR SLAG-IDLEVNR = SPACE                                            
134400       OR SLAG-IDLEVNR = '9996'                                           
134500       OR SLAG-IDLEVNR = '9997'                                           
134600       OR SLAG-IDLEVNR = '9998'                                           
134700       OR SLAG-IDLEVNR = '9999'                                           
134800                                                                          
134900          CONTINUE                                                        
135000       ELSE                                                               
135100          MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDANSK-ATTR-IN               
135200          MOVE NOO                    TO INDATA-SW                        
135300       END-IF                                                             
135400     END-IF                                                               
135500                                                                          
135600*---                                                                      
135700                                                                          
135800     IF MID-IDPLANGR-AG-IN NOT = ALL '+'                                  
135900        IF MID-IDPLANGR-AG-IN NUMERIC                                     
136000           MOVE MID-IDPLANGR-AG-IN  TO WS-PLANGR-AG                       
136100* VALID VALUES ARE 0 - 9                                                  
136200           IF VALID-PLANGR                                                
136300              MOVE MFS-NUM-FIELD-OK TO MOD-IDPLANGR-AG-ATTR-IN            
136400              MOVE YES              TO CHECK-LOCAL-PART-SW                
136500           ELSE                                                           
136600              MOVE MFS-NUM-FIELD-WRONG                                    
136700                                    TO MOD-IDPLANGR-AG-ATTR-IN            
136800              MOVE NOO              TO INDATA-SW                          
136900           END-IF                                                         
137000        ELSE                                                              
137100           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDPLANGR-AG-ATTR-IN            
137200           MOVE NOO                 TO INDATA-SW                          
137300        END-IF                                                            
137400     END-IF                                                               
137500                                                                          
137600*---                                                                      
137700                                                                          
137800     IF MID-IDINK-IN NOT = ALL '+'                                        
137900       INSPECT MID-IDINK-IN REPLACING LEADING SPACES BY ZEROS             
138000       IF MID-IDINK-IN NUMERIC                                            
138100          MOVE MFS-NUM-FIELD-OK     TO MOD-IDINK-ATTR-IN                  
138200          MOVE YES                  TO CHECK-LOCAL-PART-SW                
138300       ELSE                                                               
138400          MOVE MFS-NUM-FIELD-WRONG                                        
138500                                    TO MOD-IDINK-ATTR-IN                  
138600          MOVE NOO                  TO INDATA-SW                          
138700       END-IF                                                             
138800     END-IF                                                               
138900                                                                          
139000*---                                                                      
139100     IF MID-TILEVDAT-IN NOT = ALL '+'                                     
139200       INSPECT MID-TILEVDAT-IN REPLACING LEADING SPACES BY ZEROS          
139300       IF MID-IDLEVNR-SHIP-FRAM-IN NOT = ALL '+'                          
139400          CONTINUE                                                        
139500       ELSE                                                               
139600         IF MID-IDLEVNR-FRAM-IN = ALL '+'                                 
139700           MOVE NOO                   TO INDATA-SW                        
139800           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDLEVNR-FRAM-ATTR-IN         
139900         END-IF                                                           
140000       END-IF                                                             
140100     END-IF                                                               
140200                                                                          
140300     IF INDATA-OK                                                         
140400       IF MID-IDLEVNR-FRAM-IN NOT = ALL '+'                               
140500         PERFORM GI-CHECK-REFILL-PART                                     
140600       END-IF                                                             
140700     END-IF                                                               
140800                                                                          
140900     IF INDATA-OK                                                         
141000       IF MID-IDLEVNR-FRAM-IN NOT = ALL '+'                               
141100         IF MID-TILEVDAT-IN NOT = ALL '+'                                 
141200           IF MID-TILEVDAT-IN > ZERO                                      
141300             MOVE MID-IDLEVNR-FRAM-IN       TO W-IDLEVNR                  
141400             PERFORM IMS-GU-WDF101                                        
141500             IF SEGMENT-FOUND                                             
141600                PERFORM GBB-CHECK-FUTURE-FC                               
141700                IF INDATA-WRONG                                           
141800                  MOVE MFS-ALPHA-FIELD-WRONG TO                           
141900                                MOD-IDLEVNR-SHIP-FRAM-ATTR-IN             
142000                ELSE                                                      
142100                  MOVE MFS-ALPHA-FIELD-OK   TO                            
142200                                        MOD-IDLEVNR-FRAM-ATTR-IN          
142300                END-IF                                                    
142400             ELSE                                                         
142500                MOVE NOO                    TO INDATA-SW                  
142600                MOVE MFS-ALPHA-FIELD-WRONG  TO                            
142700                                        MOD-IDLEVNR-FRAM-ATTR-IN          
142800             END-IF                                                       
142900           ELSE                                                           
143000             MOVE NOO                    TO INDATA-SW                     
143100             MOVE MFS-ALPHA-FIELD-WRONG  TO                               
143200                                        MOD-IDLEVNR-FRAM-ATTR-IN          
143300           END-IF                                                         
143400         ELSE                                                             
143500            MOVE NOO                    TO INDATA-SW                      
143600            MOVE MFS-ALPHA-FIELD-WRONG  TO                                
143700                                        MOD-IDLEVNR-FRAM-ATTR-IN          
143800            MOVE MFS-NUM-FIELD-WRONG    TO MOD-TILEVDAT-ATTR-IN           
143900         END-IF                                                           
144000       END-IF                                                             
144100                                                                          
144200       IF MID-IDLEVNR-SHIP-FRAM-IN NOT = ALL '+'                          
144300* IF SHIPPING SUPPLIER IS BEING UPDATED, MAIN SUPPLIER HAS TO             
144400* BE GIVEN                                                                
144500        IF MID-IDLEVNR-FRAM-IN NOT = ALL '+'                              
144600          IF MID-TILEVDAT-IN NOT = ALL '+'                                
144700            IF MID-TILEVDAT-IN > ZERO                                     
144800              MOVE MID-IDLEVNR-SHIP-FRAM-IN  TO W-IDLEVNR                 
144900              PERFORM IMS-GU-WDF101                                       
145000              IF SEGMENT-FOUND                                            
145100                 MOVE MFS-ALPHA-FIELD-OK     TO                           
145200                                MOD-IDLEVNR-SHIP-FRAM-ATTR-IN             
145300              ELSE                                                        
145400                 MOVE NOO                    TO INDATA-SW                 
145500                 MOVE MFS-ALPHA-FIELD-WRONG  TO                           
145600                                MOD-IDLEVNR-SHIP-FRAM-ATTR-IN             
145700              END-IF                                                      
145800            ELSE                                                          
145900               MOVE NOO                    TO INDATA-SW                   
146000               MOVE MFS-ALPHA-FIELD-WRONG  TO                             
146100                                 MOD-IDLEVNR-SHIP-FRAM-ATTR-IN            
146200            END-IF                                                        
146300          ELSE                                                            
146400            MOVE NOO                       TO INDATA-SW                   
146500            MOVE MFS-ALPHA-FIELD-WRONG     TO                             
146600                                 MOD-IDLEVNR-SHIP-FRAM-ATTR-IN            
146700            MOVE MFS-NUM-FIELD-WRONG     TO                               
146800                                 MOD-TILEVDAT-ATTR-IN                     
146900          END-IF                                                          
147000        ELSE                                                              
147100          MOVE NOO                       TO INDATA-SW                     
147200          MOVE MFS-ALPHA-FIELD-WRONG     TO                               
147300                                 MOD-IDLEVNR-SHIP-FRAM-ATTR-IN            
147400          MOVE MFS-ALPHA-FIELD-WRONG     TO                               
147500                                 MOD-IDLEVNR-FRAM-ATTR-IN                 
147600        END-IF                                                            
147700       END-IF                                                             
147800                                                                          
147900       PERFORM GBA-CHECK-DATE-FIELDS-MFG                                  
148000                                                                          
148100     END-IF                                                               
148200     .                                                                    
148300     EJECT                                                                
148400                                                                          
148500 GBA-CHECK-DATE-FIELDS-MFG SECTION.                                       
148600     MOVE 'GBA-CHECK-DATE-FIELDS-MFG '   TO CURRENT-SECTION               
148700                                                                          
148800     IF MID-TILEVDAT-IN NOT = ALL '+'                                     
148900      IF MID-TILEVDAT-IN NUMERIC                                          
149000*     AND MID-TILEVDAT-IN > ZERO                                          
149100        IF MID-TILEVDAT-IN > ZERO                                         
149200          MOVE MID-TILEVDAT-IN TO DAT-I-TIDATUM                           
149300          MOVE 'AAMMDD'                TO DAT-KDDATFORM                   
149400          CALL WDATKONV             USING DAT-KDDATFORM                   
149500                                          DAT-I-TIDATUM                   
149600                                          DAT-O-TIDATUM                   
149700                                          DAT-KDSVAR                      
149800          MOVE DAT-TIAAMMDD            TO TMP1-YYMMDD                     
149900          MOVE DAGENS-DATUM            TO TMP2-YYMMDD                     
150000          PERFORM WY2000P1                                                
150100          IF  DAT-KDSVAR-OK                                               
150200          AND TMP1-YYMMDD > TMP2-YYMMDD                                   
150300          AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                        
150400            MOVE MFS-NUM-FIELD-OK      TO                                 
150500                                    MOD-TILEVDAT-ATTR-IN                  
150600          ELSE                                                            
150700            MOVE MFS-NUM-FIELD-WRONG   TO                                 
150800                                    MOD-TILEVDAT-ATTR-IN                  
150900            MOVE NOO                   TO INDATA-SW                       
151000          END-IF                                                          
151100        ELSE                                                              
151200          MOVE MFS-NUM-FIELD-OK TO  MOD-TILEVDAT-ATTR-IN                  
151300        END-IF                                                            
151400      ELSE                                                                
151500        MOVE MFS-NUM-FIELD-WRONG       TO                                 
151600                                    MOD-TILEVDAT-ATTR-IN                  
151700        MOVE NOO                       TO INDATA-SW                       
151800      END-IF                                                              
151900     END-IF                                                               
152000     .                                                                    
152100     EJECT                                                                
152200                                                                          
152300 GBB-CHECK-FUTURE-FC SECTION.                                             
152400                                                                          
152500     PERFORM IMS-GHU-WDK727                                               
152600     IF SEGMENT-FOUND                                                     
152700       IF PROG-KVPB-JUST (1) > ZERO                                       
152800         MOVE NOO    TO INDATA-SW                                         
152900         MOVE MED-1  TO MOD-TEMFSINF                                      
153000       END-IF                                                             
153100     END-IF                                                               
153200     .                                                                    
153300     EJECT                                                                
153400                                                                          
153500 GC-CHECK-MID-INPUT-LINE-2 SECTION.                                       
153600     MOVE 'GC-CHECK-MID-INPUT-LINE-2'   TO CURRENT-SECTION                
153700                                                                          
153800     IF MID-KVSPANT-IN NOT = ALL '+'                                      
153900        INSPECT MID-KVSPANT-IN REPLACING LEADING SPACES                   
154000          BY ZEROS                                                        
154100        IF MID-KVSPANT-IN NUMERIC                                         
154200           MOVE MFS-NUM-FIELD-OK    TO MOD-KVSPANT-ATTR-IN                
154300           MOVE YES                 TO CHECK-LOCAL-PART-SW                
154400        ELSE                                                              
154500           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVSPANT-ATTR-IN                
154600           MOVE NOO                 TO INDATA-SW                          
154700        END-IF                                                            
154800     END-IF                                                               
154900                                                                          
155000*---                                                                      
155100                                                                          
155200     IF MID-TIREFSTO-LOC-IN NOT = ALL '+'                                 
155300      IF MID-TIREFSTO-LOC-IN = '0     '                                   
155400         MOVE ZERO TO MID-TIREFSTO-LOC-IN                                 
155500      END-IF                                                              
155600      IF MID-TIREFSTO-LOC-IN NUMERIC                                      
155700        IF MID-TIREFSTO-LOC-IN > ZERO                                     
155800           MOVE MID-TIREFSTO-LOC-IN    TO DAT-I-TIDATUM                   
155900           MOVE 'AAMMDD'               TO DAT-KDDATFORM                   
156000           CALL WDATKONV            USING DAT-KDDATFORM                   
156100                                           DAT-I-TIDATUM                  
156200                                           DAT-O-TIDATUM                  
156300                                           DAT-KDSVAR                     
156400           MOVE DAT-TIAAMMDD           TO TMP1-YYMMDD                     
156500           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
156600           PERFORM WY2000P1                                               
156700           IF DAT-KDSVAR-OK                                               
156800           AND TMP1-YYMMDD > TMP2-YYMMDD                                  
156900           AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                       
157000             MOVE MFS-NUM-FIELD-OK     TO                                 
157100                                     MOD-TIREFSTO-LOC-ATTR-IN             
157200             MOVE YES               TO CHECK-LOCAL-PART-SW                
157300           ELSE                                                           
157400             MOVE MFS-NUM-FIELD-WRONG  TO                                 
157500                                     MOD-TIREFSTO-LOC-ATTR-IN             
157600             MOVE NOO                  TO INDATA-SW                       
157700           END-IF                                                         
157800        ELSE                                                              
157900           MOVE MFS-NUM-FIELD-OK    TO                                    
158000                                   MOD-TIREFSTO-LOC-ATTR-IN               
158100           MOVE YES                 TO CHECK-LOCAL-PART-SW                
158200        END-IF                                                            
158300      ELSE                                                                
158400        MOVE MFS-NUM-FIELD-WRONG       TO                                 
158500                                    MOD-TIREFSTO-LOC-ATTR-IN              
158600        MOVE NOO                       TO INDATA-SW                       
158700      END-IF                                                              
158800     END-IF                                                               
158900                                                                          
159000*---                                                                      
159100                                                                          
159200     IF MID-FLJIT-IN NOT = ALL '+'                                        
159300       IF MID-FLJIT-IN = 'Y' OR YES OR NOO                                
159400          MOVE MFS-ALPHA-FIELD-OK   TO MOD-FLJIT-ATTR-IN                  
159500          MOVE YES                  TO CHECK-LOCAL-PART-SW                
159600       ELSE                                                               
159700          MOVE MFS-ALPHA-FIELD-WRONG                                      
159800                                    TO MOD-FLJIT-ATTR-IN                  
159900          MOVE NOO                  TO INDATA-SW                          
160000       END-IF                                                             
160100     END-IF                                                               
160200                                                                          
160300*---                                                                      
160400                                                                          
160500     IF MID-FLWILSON-IN NOT = ALL '+'                                     
160600       IF MID-FLWILSON-IN = 'Y' OR YES OR NOO                             
160700          MOVE MFS-ALPHA-FIELD-OK   TO MOD-FLWILSON-ATTR-IN               
160800          MOVE YES                  TO CHECK-LOCAL-PART-SW                
160900       ELSE                                                               
161000          MOVE MFS-ALPHA-FIELD-WRONG                                      
161100                                    TO MOD-FLWILSON-ATTR-IN               
161200          MOVE NOO                  TO INDATA-SW                          
161300       END-IF                                                             
161400     END-IF                                                               
161500                                                                          
161600*---                                                                      
161700                                                                          
161800     IF MID-IDREFTAB-IN NOT = ALL '+'                                     
161900       MOVE MID-IDREFTAB-IN         TO WS-IDREFTAB-ID                     
162000* VALID VALUES ARE A - Z                                                  
162100       IF VALID-IDREFTAB-ID                                               
162200          MOVE MFS-ALPHA-FIELD-OK   TO MOD-IDREFTAB-ATTR-IN               
162300          MOVE YES                  TO CHECK-LOCAL-PART-SW                
162400       ELSE                                                               
162500          MOVE MFS-ALPHA-FIELD-WRONG                                      
162600                                    TO MOD-IDREFTAB-ATTR-IN               
162700          MOVE NOO                  TO INDATA-SW                          
162800       END-IF                                                             
162900     END-IF                                                               
163000                                                                          
163100     IF MID-IDREFTAB-IN NOT = ALL '+'                                     
163200       MOVE W-IDDC                  TO W-IDDC-2501                        
163300       MOVE MID-IDREFTAB-IN         TO W-IDREFTAB-2502                    
163400       PERFORM IMS-GU-WDR2-WDGX2502                                       
163500       IF SEGMENT-MISSING                                                 
163600          MOVE MFS-ALPHA-FIELD-WRONG                                      
163700                                    TO MOD-IDREFTAB-ATTR-IN               
163800          MOVE NOO                  TO INDATA-SW                          
163900       END-IF                                                             
164000     END-IF                                                               
164100                                                                          
164200     .                                                                    
164300     EJECT                                                                
164400                                                                          
164500 GD-CHECK-MID-INPUT-LINE-3 SECTION.                                       
164600     MOVE 'GD-CHECK-MID-INPUT-LINE-3'   TO CURRENT-SECTION                
164700                                                                          
164800     IF MID-KVSLAGER-IN NOT = ALL '+'                                     
164900       INSPECT MID-KVSLAGER-IN REPLACING LEADING SPACES BY   ZEROS        
165000       IF MID-KVSLAGER-IN NUMERIC                                         
165100          MOVE MFS-NUM-FIELD-OK     TO MOD-KVSLAGER-ATTR-IN               
165200          MOVE YES                  TO CHECK-LOCAL-PART-SW                
165300       ELSE                                                               
165400          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVSLAGER-ATTR-IN               
165500          MOVE NOO                  TO INDATA-SW                          
165600       END-IF                                                             
165700     END-IF                                                               
165800                                                                          
165900     IF MID-KVSLAGER-IN NOT = ALL '+'                                     
166000       IF WDK722-FINNS                                                    
166100          MOVE XLAG-TIMANSEC       TO SPAR-TIMANSEC                       
166200       ELSE                                                               
166300          MOVE ZERO                TO SPAR-TIMANSEC                       
166400       END-IF                                                             
166500                                                                          
166600*      IF MID-TIMANSEC-IN > ZERO                                          
166700*      OR SPAR-TIMANSEC > ZERO                                            
166800*                                                                         
166900*        CONTINUE                                                         
167000*      ELSE                                                               
167100*        MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVSLAGER-ATTR-IN                
167200*        MOVE NOO                  TO INDATA-SW                           
167300*      END-IF                                                             
167400     END-IF                                                               
167500                                                                          
167600     IF MID-TIMANSEC-IN NOT = ALL '+'                                     
167700      IF MID-TIMANSEC-IN NUMERIC                                          
167800*     AND MID-TIMANSEC-IN > ZERO                                          
167900         IF MID-TIMANSEC-IN > ZERO                                        
168000            MOVE MID-TIMANSEC-IN       TO DAT-I-TIDATUM                   
168100            MOVE 'AAMMDD'              TO DAT-KDDATFORM                   
168200            CALL WDATKONV           USING DAT-KDDATFORM                   
168300                                          DAT-I-TIDATUM                   
168400                                          DAT-O-TIDATUM                   
168500                                          DAT-KDSVAR                      
168600            MOVE DAT-TIAAMMDD          TO TMP1-YYMMDD                     
168700            MOVE DAGENS-DATUM          TO TMP2-YYMMDD                     
168800            PERFORM WY2000P1                                              
168900            IF DAT-KDSVAR-OK                                              
169000            AND TMP1-YYMMDD > TMP2-YYMMDD                                 
169100            AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                      
169200              MOVE MFS-NUM-FIELD-OK    TO                                 
169300                                      MOD-TIMANSEC-ATTR-IN                
169400              MOVE YES              TO CHECK-LOCAL-PART-SW                
169500            ELSE                                                          
169600              MOVE MFS-NUM-FIELD-WRONG TO                                 
169700                                      MOD-TIMANSEC-ATTR-IN                
169800              MOVE NOO                 TO INDATA-SW                       
169900            END-IF                                                        
170000         ELSE                                                             
170100            MOVE MFS-NUM-FIELD-OK TO MOD-TIMANSEC-ATTR-IN                 
170200            MOVE YES              TO CHECK-LOCAL-PART-SW                  
170300         END-IF                                                           
170400      ELSE                                                                
170500        MOVE MFS-NUM-FIELD-WRONG       TO                                 
170600                                    MOD-TIMANSEC-ATTR-IN                  
170700        MOVE NOO                       TO INDATA-SW                       
170800      END-IF                                                              
170900     END-IF                                                               
171000                                                                          
171100*---                                                                      
171200                                                                          
171300     IF MID-KVPALL-IN NOT = ALL '+'                                       
171400       INSPECT MID-KVPALL-IN REPLACING LEADING SPACES BY ZEROS            
171500       IF MID-KVPALL-IN NUMERIC                                           
171600          MOVE MFS-NUM-FIELD-OK     TO MOD-KVPALL-ATTR-IN                 
171700          MOVE YES                  TO CHECK-LOCAL-PART-SW                
171800       ELSE                                                               
171900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVPALL-ATTR-IN                 
172000          MOVE NOO                  TO INDATA-SW                          
172100       END-IF                                                             
172200     END-IF                                                               
172300                                                                          
172400*---                                                                      
172500                                                                          
172600     IF MID-KDAVT-IN NOT = ALL '+'                                        
172700       IF MFS-UPD-V                                                       
172800         IF MID-KDAVT-IN = '0' OR '1' OR '3'                              
172900           MOVE MFS-NUM-FIELD-OK       TO MOD-KDAVT-ATTR-IN               
173000           MOVE YES                    TO CHECK-LOCAL-PART-SW             
173100         ELSE                                                             
173200           MOVE MFS-NUM-FIELD-WRONG    TO MOD-KDAVT-ATTR-IN               
173300           MOVE NOO                    TO INDATA-SW                       
173400         END-IF                                                           
173500       ELSE                                                               
173600         IF MFS-UPDATE                                                    
173700           IF MID-KDAVT-IN = '0' OR '3'                                   
173800             MOVE MFS-NUM-FIELD-OK     TO MOD-KDAVT-ATTR-IN               
173900             MOVE YES                  TO CHECK-LOCAL-PART-SW             
174000           ELSE                                                           
174100             MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDAVT-ATTR-IN               
174200             MOVE NOO                  TO INDATA-SW                       
174300           END-IF                                                         
174400         END-IF                                                           
174500       END-IF                                                             
174600     END-IF                                                               
174700     .                                                                    
174800     EJECT                                                                
174900                                                                          
175000 GE-CHECK-MID-INPUT-LINE-4 SECTION.                                       
175100     MOVE 'GE-CHECK-MID-INPUT-LINE-4'   TO CURRENT-SECTION                
175200                                                                          
175300     IF MID-KVREFBER-IN NOT = ALL '+'                                     
175400       INSPECT MID-KVREFBER-IN REPLACING LEADING SPACES BY ZEROS          
175500       IF MID-KVREFBER-IN NUMERIC                                         
175600          MOVE MFS-NUM-FIELD-OK     TO MOD-KVREFBER-ATTR-IN               
175700          MOVE YES                  TO CHECK-LOCAL-PART-SW                
175800       ELSE                                                               
175900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVREFBER-ATTR-IN               
176000          MOVE NOO                  TO INDATA-SW                          
176100       END-IF                                                             
176200     END-IF                                                               
176300                                                                          
176400*    IF MID-KVREFBER-IN NOT = ALL '+'                                     
176500*      IF MID-TIREFPAF-IN > ZERO                                          
176600*      OR SLAG-TIREFPAF > ZERO                                            
176700*                                                                         
176800*        CONTINUE                                                         
176900*      ELSE                                                               
177000*         MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVREFBER-ATTR-IN               
177100*         MOVE NOO                  TO INDATA-SW                          
177200*      END-IF                                                             
177300*    END-IF                                                               
177400                                                                          
177500     IF MID-TIREFPAF-IN NOT = ALL '+'                                     
177600      IF MID-TIREFPAF-IN NUMERIC                                          
177700*     AND MID-TIREFPAF-IN > ZERO                                          
177800         IF MID-TIREFPAF-IN > ZERO                                        
177900            MOVE MID-TIREFPAF-IN       TO DAT-I-TIDATUM                   
178000            MOVE 'AAMMDD'              TO DAT-KDDATFORM                   
178100            CALL WDATKONV           USING DAT-KDDATFORM                   
178200                                          DAT-I-TIDATUM                   
178300                                          DAT-O-TIDATUM                   
178400                                          DAT-KDSVAR                      
178500            MOVE DAT-TIAAMMDD          TO TMP1-YYMMDD                     
178600            MOVE DAGENS-DATUM          TO TMP2-YYMMDD                     
178700            PERFORM WY2000P1                                              
178800            IF DAT-KDSVAR-OK                                              
178900            AND TMP1-YYMMDD > TMP2-YYMMDD                                 
179000            AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                      
179100              MOVE MFS-NUM-FIELD-OK    TO                                 
179200                                      MOD-TIREFPAF-ATTR-IN                
179300              MOVE YES              TO CHECK-LOCAL-PART-SW                
179400            ELSE                                                          
179500              MOVE MFS-NUM-FIELD-WRONG TO                                 
179600                                      MOD-TIREFPAF-ATTR-IN                
179700              MOVE NOO                 TO INDATA-SW                       
179800            END-IF                                                        
179900         ELSE                                                             
180000            MOVE MFS-NUM-FIELD-OK TO  MOD-TIREFPAF-ATTR-IN                
180100            MOVE YES              TO CHECK-LOCAL-PART-SW                  
180200         END-IF                                                           
180300      ELSE                                                                
180400        MOVE MFS-NUM-FIELD-WRONG       TO                                 
180500                                    MOD-TIREFPAF-ATTR-IN                  
180600        MOVE NOO                       TO INDATA-SW                       
180700      END-IF                                                              
180800     END-IF                                                               
180900                                                                          
181000     IF MID-KVREFBER-IN = ALL '+'                                         
181100     OR MID-TIREFPAF-IN = ALL '+'                                         
181200       CONTINUE                                                           
181300     ELSE                                                                 
181400       IF MID-KVREFBER-IN = ZERO                                          
181500         IF MID-TIREFPAF-IN > ZERO                                        
181600           MOVE MFS-NUM-FIELD-WRONG    TO                                 
181700                                      MOD-KVREFBER-ATTR-IN                
181800                                      MOD-TIREFPAF-ATTR-IN                
181900           MOVE NOO                    TO INDATA-SW                       
182000         END-IF                                                           
182100*      ELSE                                                               
182200*        IF MID-TIREFPAF-IN = ZERO                                        
182300*          MOVE MFS-NUM-FIELD-WRONG    TO                                 
182400*                                     MOD-KVREFBER-ATTR-IN                
182500*                                     MOD-TIREFPAF-ATTR-IN                
182600*          MOVE NOO                    TO INDATA-SW                       
182700*        END-IF                                                           
182800       END-IF                                                             
182900     END-IF                                                               
183000                                                                          
183100*---                                                                      
183200                                                                          
183300     IF MID-KVULOAD-IN NOT = ALL '+'                                      
183400       INSPECT MID-KVULOAD-IN REPLACING LEADING SPACES BY ZEROS           
183500       IF MID-KVULOAD-IN NUMERIC                                          
183600          MOVE MFS-NUM-FIELD-OK     TO MOD-KVULOAD-ATTR-IN                
183700          MOVE YES                  TO CHECK-LOCAL-PART-SW                
183800       ELSE                                                               
183900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVULOAD-ATTR-IN                
184000          MOVE NOO                  TO INDATA-SW                          
184100       END-IF                                                             
184200     END-IF                                                               
184300                                                                          
184400     IF MID-TISTODAT-LARM-IN NOT = ALL '+'                                
184500      INSPECT MID-TISTODAT-LARM-IN REPLACING LEADING SPACE BY ZERO        
184600       IF MID-TISTODAT-LARM-IN NUMERIC                                    
184700         IF MID-TISTODAT-LARM-IN = '999999' OR ZEROES                     
184800           MOVE MFS-NUM-FIELD-OK    TO MOD-TISTODAT-LARM-ATTR-IN          
184900           MOVE YES                 TO CHECK-LOCAL-PART-SW                
185000         ELSE                                                             
185100           MOVE MID-TISTODAT-LARM-IN TO WS-START-DATUM                    
185200           MOVE WS-START-DATUM       TO DAT-I-TIDATUM                     
185300           MOVE 'AAMMDD'             TO DAT-KDDATFORM                     
185400           CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM               
185500                               DAT-O-TIDATUM, DAT-KDSVAR                  
185600           IF DAT-KDSVAR-OK                                               
185700             MOVE DAGENS-DATUM TO W-DAGENS-DATUM-PL-ONE-YEAR              
185800             ADD +1            TO W-DAGENS-DATUM-PL-ONE-YEAR-YY           
185900             IF WS-START-DATUM NOT > DAGENS-DATUM                         
186000               MOVE ERR-FUTURE-DATE   TO MED-IDMFSFEL                     
186100               MOVE MFS-NUM-FIELD-WRONG                                   
186200                                      TO MOD-TISTODAT-LARM-ATTR-IN        
186300               MOVE NOO               TO INDATA-SW                        
186400             ELSE                                                         
186500               IF WS-START-DATUM > W-DAGENS-DATUM-PL-ONE-YEAR             
186600                 MOVE ERR-ONE-YEAR-IN-FUTURE TO MED-IDMFSFEL              
186700                 MOVE MFS-NUM-FIELD-WRONG                                 
186800                                      TO MOD-TISTODAT-LARM-ATTR-IN        
186900                 MOVE NOO             TO INDATA-SW                        
187000               ELSE                                                       
187100                 MOVE MFS-NUM-FIELD-OK                                    
187200                                     TO MOD-TISTODAT-LARM-ATTR-IN         
187300                 MOVE YES            TO CHECK-LOCAL-PART-SW               
187400               END-IF                                                     
187500             END-IF                                                       
187600           ELSE                                                           
187700             MOVE MFS-NUM-FIELD-WRONG TO MOD-TISTODAT-LARM-ATTR-IN        
187800             MOVE NOO                TO INDATA-SW                         
187900           END-IF                                                         
188000         END-IF                                                           
188100       ELSE                                                               
188200         MOVE MFS-NUM-FIELD-WRONG    TO MOD-TISTODAT-LARM-ATTR-IN         
188300         MOVE NOO                    TO INDATA-SW                         
188400       END-IF                                                             
188500     ELSE                                                                 
188600       MOVE MFS-RENSA-FAELT          TO MOD-TISTODAT-LARM-ATTR-IN         
188700     END-IF                                                               
188800                                                                          
188900     .                                                                    
189000     EJECT                                                                
189100                                                                          
189200 GF-CHECK-MID-INPUT-LINE-5 SECTION.                                       
189300     MOVE 'GF-CHECK-MID-INPUT-LINE-5'   TO CURRENT-SECTION                
189400                                                                          
189500     IF MID-KVVECKOR-LT-IN NOT = ALL '+'                                  
189600       INSPECT MID-KVVECKOR-LT-IN                                         
189700              REPLACING LEADING SPACES BY ZEROS                           
189800       IF MID-KVVECKOR-LT-IN NUMERIC                                      
189900          MOVE MFS-NUM-FIELD-OK     TO MOD-KVVECKOR-LT-ATTR-IN            
190000          MOVE YES                  TO CHECK-LOCAL-PART-SW                
190100       ELSE                                                               
190200          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVVECKOR-LT-ATTR-IN            
190300          MOVE NOO                  TO INDATA-SW                          
190400       END-IF                                                             
190500     END-IF                                                               
190600                                                                          
190700     IF MID-KVVECKOR-LT-IN NOT = ALL '+'                                  
190800       IF WDK722-FINNS                                                    
190900         MOVE XLAG-TIMANLED         TO SPAR-TIMANLED                      
191000       ELSE                                                               
191100         MOVE ZERO                  TO SPAR-TIMANLED                      
191200       END-IF                                                             
191300                                                                          
191400       IF MID-TIMANLED-IN > ZERO                                          
191500       OR SPAR-TIMANLED > ZERO                                            
191600                                                                          
191700          CONTINUE                                                        
191800       ELSE                                                               
191900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVVECKOR-LT-ATTR-IN            
192000          MOVE NOO                  TO INDATA-SW                          
192100       END-IF                                                             
192200     END-IF                                                               
192300                                                                          
192400                                                                          
192500     IF MID-TIMANLED-IN NOT = ALL '+'                                     
192600      IF MID-TIMANLED-IN NUMERIC                                          
192700       IF MID-TIMANLED-IN = 999999                                        
192800         MOVE MFS-NUM-FIELD-OK      TO                                    
192900                                    MOD-TIMANLED-ATTR-IN                  
193000         MOVE YES                   TO CHECK-LOCAL-PART-SW                
193100       ELSE                                                               
193200         IF MID-TIMANLED-IN > ZERO                                        
193300            MOVE MID-TIMANLED-IN TO DAT-I-TIDATUM                         
193400            MOVE 'AAMMDD'                TO DAT-KDDATFORM                 
193500            CALL WDATKONV             USING DAT-KDDATFORM                 
193600                                            DAT-I-TIDATUM                 
193700                                            DAT-O-TIDATUM                 
193800                                            DAT-KDSVAR                    
193900            MOVE DAT-TIAAMMDD            TO TMP1-YYMMDD                   
194000            MOVE DAGENS-DATUM            TO TMP2-YYMMDD                   
194100            PERFORM WY2000P1                                              
194200            IF  DAT-KDSVAR-OK                                             
194300            AND TMP1-YYMMDD > TMP2-YYMMDD                                 
194400              MOVE MFS-NUM-FIELD-OK      TO                               
194500                                      MOD-TIMANLED-ATTR-IN                
194600              MOVE YES                TO CHECK-LOCAL-PART-SW              
194700            ELSE                                                          
194800              MOVE MFS-NUM-FIELD-WRONG   TO                               
194900                                      MOD-TIMANLED-ATTR-IN                
195000              MOVE NOO                   TO INDATA-SW                     
195100            END-IF                                                        
195200         ELSE                                                             
195300            MOVE MFS-NUM-FIELD-OK     TO MOD-TIMANLED-ATTR-IN             
195400            MOVE YES                  TO CHECK-LOCAL-PART-SW              
195500         END-IF                                                           
195600       END-IF                                                             
195700      ELSE                                                                
195800        MOVE MFS-NUM-FIELD-WRONG       TO                                 
195900                                    MOD-TIMANLED-ATTR-IN                  
196000        MOVE NOO                       TO INDATA-SW                       
196100      END-IF                                                              
196200     END-IF                                                               
196300                                                                          
196400*---                                                                      
196500                                                                          
196600     IF MID-KVSLUTKP-IN NOT = ALL '+'                                     
196700       INSPECT MID-KVSLUTKP-IN REPLACING LEADING SPACES BY    ZERO        
196800       IF MID-KVSLUTKP-IN NUMERIC                                         
196900          MOVE MFS-NUM-FIELD-OK     TO MOD-KVSLUTKP-ATTR-IN               
197000          MOVE YES                  TO CHECK-LOCAL-PART-SW                
197100       ELSE                                                               
197200          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVSLUTKP-ATTR-IN               
197300          MOVE NOO                  TO INDATA-SW                          
197400       END-IF                                                             
197500     END-IF                                                               
197600                                                                          
197700     IF WDK722-FINNS                                                      
197800       MOVE XLAG-KVSLUTKP    TO SPAR-KVSLUTKP                             
197900     ELSE                                                                 
198000       MOVE ZERO             TO SPAR-KVSLUTKP                             
198100     END-IF                                                               
198200                                                                          
198300     IF  (MID-KVSLUTKP-IN = ZERO                                          
198400         AND MID-TISLUTKP-IN > ZERO)                                      
198500     OR  (MID-KVSLUTKP-IN > ZERO                                          
198600         AND MID-TISLUTKP-IN = ZERO )                                     
198700       MOVE MFS-NUM-FIELD-WRONG      TO MOD-KVSLUTKP-ATTR-IN              
198800                                        MOD-TISLUTKP-ATTR-IN              
198900       MOVE NOO                      TO INDATA-SW                         
199000     ELSE                                                                 
199100       IF  MID-TISLUTKP-IN NOT = ALL '+'                                  
199200       AND MID-TISLUTKP-IN > ZERO                                         
199300                                                                          
199400         IF (SPAR-KVSLUTKP = ZERO AND                                     
199500             MID-KVSLUTKP-IN = ALL '+')                                   
199600         OR MID-KVSLUTKP-IN = ZERO                                        
199700                                                                          
199800            MOVE MFS-NUM-FIELD-WRONG TO MOD-TISLUTKP-ATTR-IN              
199900            MOVE NOO                 TO INDATA-SW                         
200000         END-IF                                                           
200100       END-IF                                                             
200200     END-IF                                                               
200300                                                                          
200400     IF MID-TISLUTKP-IN NOT = ALL '+'                                     
200500      IF MID-TISLUTKP-IN NUMERIC                                          
200600      AND MID-TISLUTKP-IN > ZERO                                          
200700          MOVE MID-TISLUTKP-IN TO DAT-I-TIDATUM                           
200800          MOVE 'AAMMDD'                TO DAT-KDDATFORM                   
200900          CALL WDATKONV             USING DAT-KDDATFORM                   
201000                                          DAT-I-TIDATUM                   
201100                                          DAT-O-TIDATUM                   
201200                                          DAT-KDSVAR                      
201300          MOVE DAT-TIAAMMDD            TO TMP1-YYMMDD                     
201400          MOVE DAGENS-DATUM            TO TMP2-YYMMDD                     
201500          PERFORM WY2000P1                                                
201600          IF  DAT-KDSVAR-OK                                               
201700          AND TMP1-YYMMDD > TMP2-YYMMDD                                   
201800          AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                        
201900            MOVE MFS-NUM-FIELD-OK      TO                                 
202000                                    MOD-TISLUTKP-ATTR-IN                  
202100            MOVE YES                TO CHECK-LOCAL-PART-SW                
202200          ELSE                                                            
202300            MOVE MFS-NUM-FIELD-WRONG   TO                                 
202400                                    MOD-TISLUTKP-ATTR-IN                  
202500            MOVE NOO                   TO INDATA-SW                       
202600          END-IF                                                          
202700      ELSE                                                                
202800        MOVE MFS-NUM-FIELD-WRONG       TO                                 
202900                                    MOD-TISLUTKP-ATTR-IN                  
203000        MOVE NOO                       TO INDATA-SW                       
203100      END-IF                                                              
203200     END-IF                                                               
203300                                                                          
203400     IF MID-KVSLUTKP-IN = ALL '+'                                         
203500     OR MID-TISLUTKP-IN = ALL '+'                                         
203600       CONTINUE                                                           
203700     ELSE                                                                 
203800       IF MID-KVSLUTKP-IN = ZERO                                          
203900         IF MID-TISLUTKP-IN > ZERO                                        
204000           MOVE MFS-NUM-FIELD-WRONG    TO                                 
204100                                      MOD-KVSLUTKP-ATTR-IN                
204200                                      MOD-TISLUTKP-ATTR-IN                
204300           MOVE NOO                    TO INDATA-SW                       
204400         END-IF                                                           
204500       ELSE                                                               
204600         IF MID-TISLUTKP-IN = ZERO                                        
204700           MOVE MFS-NUM-FIELD-WRONG    TO                                 
204800                                      MOD-KVSLUTKP-ATTR-IN                
204900                                      MOD-TISLUTKP-ATTR-IN                
205000           MOVE NOO                    TO INDATA-SW                       
205100         END-IF                                                           
205200       END-IF                                                             
205300     END-IF                                                               
205400                                                                          
205500     .                                                                    
205600     EJECT                                                                
205700                                                                          
205800 GG-CHECK-MID-INPUT-LINE-6 SECTION.                                       
205900     MOVE 'GG-CHECK-MID-INPUT-LINE-6'   TO CURRENT-SECTION                
206000                                                                          
206100     IF MID-KDOPPLAN-IN NOT = ALL '+'                                     
206200       IF MID-KDOPPLAN-IN = 'Y' OR YES OR NOO                             
206300          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KDOPPLAN-ATTR-IN               
206400          MOVE YES                  TO CHECK-LOCAL-PART-SW                
206500       ELSE                                                               
206600          MOVE MFS-ALPHA-FIELD-WRONG                                      
206700                                    TO MOD-KDOPPLAN-ATTR-IN               
206800          MOVE NOO                  TO INDATA-SW                          
206900       END-IF                                                             
207000     END-IF                                                               
207100                                                                          
207200*---                                                                      
207300                                                                          
207400     IF MID-DAPUBL-IN NOT = ALL '+'                                       
207500       IF MID-DAPUBL-IN NUMERIC                                           
207600*      AND MID-DAPUBL-IN > ZERO                                           
207700         IF MID-DAPUBL-IN > ZERO                                          
207800           IF MID-DAPUBL-IN >= WS-CURRENT-YYWWD                           
207900              MOVE 'AAVVD'            TO DAT-KDDATFORM                    
208000              MOVE MID-DAPUBL-IN      TO DAT-I-TIDATUM                    
208100                                                                          
208200              CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM             
208300                                  DAT-O-TIDATUM DAT-KDSVAR                
208400                                                                          
208500              IF DAT-KDSVAR-OK                                            
208600                 MOVE MFS-NUM-FIELD-OK TO MOD-DAPUBL-ATTR-IN              
208700              ELSE                                                        
208800                 MOVE MFS-NUM-FIELD-WRONG                                 
208900                                        TO MOD-DAPUBL-ATTR-IN             
209000                 MOVE NOO             TO INDATA-SW                        
209100              END-IF                                                      
209200           ELSE                                                           
209300             MOVE MFS-NUM-FIELD-WRONG TO                                  
209400                                     MOD-DAPUBL-ATTR-IN                   
209500             MOVE NOO                 TO INDATA-SW                        
209600           END-IF                                                         
209700         ELSE                                                             
209800           MOVE MFS-NUM-FIELD-OK      TO MOD-DAPUBL-ATTR-IN               
209900         END-IF                                                           
210000       ELSE                                                               
210100         MOVE MFS-NUM-FIELD-WRONG       TO                                
210200                                     MOD-DAPUBL-ATTR-IN                   
210300         MOVE NOO                       TO INDATA-SW                      
210400       END-IF                                                             
210500     END-IF                                                               
210600                                                                          
210700     IF MID-DAPUBL-IN NOT = ALL '+'                                       
210800        PERFORM  GGA-CHECK-DAPUBL-OK                                      
210900     END-IF                                                               
211000                                                                          
211100*---                                                                      
211200                                                                          
211300*- DISPATCH DAY: XLAG-TILEVDAG (1=MÅ, 2=TI, 3=ONS, 4=TOR, 5=FRE)          
211400*--                                                                       
211500     PERFORM VARYING INDX FROM 1 BY 1                                     
211600     UNTIL INDX > MAX-DAYS-INDX                                           
211700       IF MID-DISP-DAY(INDX) NOT = ALL '+'                                
211800          IF MID-DISP-DAY(INDX)  NOT = SPACE                              
211900          OR (MID-DISP-DAY(INDX)     = SPACE AND                          
212000              MID-SAVE-DAY(INDX) NOT = SPACE)                             
212100            MOVE YES     TO CHECK-LOCAL-PART-SW                           
212200                                                                          
212300            IF MID-DISP-DAY(INDX) = '  ' OR 'MO' OR 'TU' OR               
212400                                    'WE' OR 'TH' OR 'FR'                  
212500               CONTINUE                                                   
212600            ELSE                                                          
212700               MOVE MFS-ALPHA-FIELD-WRONG TO                              
212800                               MOD-DISP-DAY-ATTR-IN(INDX)                 
212900               MOVE NOO                   TO INDATA-SW                    
213000            END-IF                                                        
213100          END-IF                                                          
213200       END-IF                                                             
213300     END-PERFORM                                                          
213400                                                                          
213500     .                                                                    
213600     EJECT                                                                
213700                                                                          
213800 GGA-CHECK-DAPUBL-OK  SECTION.                                            
213900     MOVE 'GGA-CHECK-DAPUBL-OK '   TO CURRENT-SECTION                     
214000                                                                          
214100     IF MID-DAPUBL-IN > ZERO                                              
214200       COMPUTE WS-CURRENT-YYWWD-2AAR =                                    
214300               WS-CURRENT-YYWWD + 2000                                    
214400       IF MID-DAPUBL-IN > WS-CURRENT-YYWWD-2AAR                           
214500         MOVE NOO                  TO INDATA-SW                           
214600         MOVE MFS-NUM-FIELD-WRONG TO MOD-DAPUBL-ATTR-IN                   
214700       ELSE                                                               
214800         CONTINUE                                                         
214900       END-IF                                                             
215000     END-IF                                                               
215100                                                                          
215200                                                                          
215300     .                                                                    
215400     EJECT                                                                
215500                                                                          
215600 GH-CHECK-MID-INPUT-LINE-7 SECTION.                                       
215700     MOVE 'GH-CHECK-MID-INPUT-LINE-7'   TO CURRENT-SECTION                
215800                                                                          
215900     IF MID-NOTES-1 NOT = ALL '+'                                         
216000       MOVE MFS-ALPHA-FIELD-OK    TO MOD-NOTES-1-ATTR-IN                  
216100     END-IF                                                               
216200                                                                          
216300     IF MID-NOTES-2 NOT = ALL '+'                                         
216400       MOVE MFS-ALPHA-FIELD-OK    TO MOD-NOTES-2-ATTR-IN                  
216500     END-IF                                                               
216600                                                                          
216700     .                                                                    
216800     EJECT                                                                
216900 GI-CHECK-REFILL-PART  SECTION.                                           
217000     MOVE 'GI-CHECK-REFILL-PART '  TO CURRENT-SECTION                     
217100                                                                          
217200     MOVE MID-IDLEVNR-FRAM-IN      TO W-IDLEVNR-B6                        
217300     PERFORM IMS-GU-WDB601-LEVNR                                          
217400     IF SEGMENT-FOUND                                                     
217700       IF SLAG-IDDC-REF = SPACE                                           
217800         MOVE LEVDC-DCS-IDDC TO WS-IDDC                                   
218000                                                                          
218100         IF LEVDC-DCS-IDDC = SLAG-IDDC                                    
218200           MOVE NOO                    TO INDATA-SW                       
218400           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
218500           MOVE MFS-ALPHA-FIELD-WRONG  TO                                 
218600                                MOD-IDLEVNR-FRAM-ATTR-IN                  
218700         END-IF                                                           
218800                                                                          
221100         IF INDATA-OK                                                     
221200           IF CDC-SE                                                      
221300             PERFORM IMS-GU-WDK601                                        
221400             IF SEGMENT-FOUND                                             
221500               PERFORM IMS-GNP-WDK611                                     
221600               IF SEGMENT-FOUND                                           
221700                 IF CLAG-IDDC-REF = SLAG-IDDC                             
221800                   MOVE NOO                      TO INDATA-SW             
222000                   MOVE ERR-UPDATE-NOT-ALLOWED TO                         
222100                                               MED-IDMFSFEL               
222200                   MOVE MFS-ALPHA-FIELD-WRONG    TO                       
222300                                   MOD-IDLEVNR-FRAM-ATTR-IN               
222400                 END-IF                                                   
222500               END-IF                                                     
222600             ELSE                                                         
222700               MOVE NOO                      TO INDATA-SW                 
222800               MOVE ERR-PART-MISSING-IN-DC TO MED-IDMFSFEL                
222900               MOVE MFS-ALPHA-FIELD-WRONG    TO                           
223000                                  MOD-IDLEVNR-FRAM-ATTR-IN                
223100             END-IF                                                       
223200           ELSE                                                           
223300             MOVE LEVDC-DCS-IDDC TO W-IDDC-REF                            
223400             PERFORM IMS-GU-WDK711-REF                                    
223500             IF SEGMENT-FOUND                                             
223600               IF (REF-SLAG-IDDC-REF = WC-CDC-SE) OR                      
223700                  (REF-SLAG-IDDC-REF = SLAG-IDDC)                         
223800                                                                          
223900                 MOVE NOO                      TO INDATA-SW               
224100                 MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL              
224200                 MOVE MFS-ALPHA-FIELD-WRONG    TO                         
224300                                    MOD-IDLEVNR-FRAM-ATTR-IN              
224400               END-IF                                                     
224500             ELSE                                                         
224600               MOVE NOO                      TO INDATA-SW                 
224700               MOVE ERR-PART-MISSING-IN-DC TO MED-IDMFSFEL                
224800               MOVE MFS-ALPHA-FIELD-WRONG    TO                           
224900                                  MOD-IDLEVNR-FRAM-ATTR-IN                
225000             END-IF                                                       
225100           END-IF                                                         
225200         END-IF                                                           
225201                                                                          
225210         IF INDATA-OK                                                     
225211*- CHECK THAT THE REFILL FLOW IS AVAILABLE                                
225230           MOVE MSGI-IDDC-KEY  TO W-IDDC                                  
225240           MOVE '4408'         TO W-IDTRANS-B6                            
225250           MOVE LEVDC-DCS-IDDC TO W-IDDC-REF-B6                           
225260           PERFORM IMS-GU-WDB615                                          
225270           IF SEGMENT-FOUND                                               
225280             IF LEVDC-DCS-IDDC       = WC-CDC-SE                          
225290*------------ DISTRICT DETAILS FOR CDC ARE STORED ON WDB601               
225291               MOVE MSGI-IDDC-KEY        TO W-IDDC                        
225292               PERFORM IMS-GU-WDB601                                      
225293               IF  SEGMENT-FOUND                                          
225294               AND DCS-IDDISTR-REFILL > ZERO                              
225295                 MOVE MFS-ALFA-FAELT-RAETT                                
225296                                      TO MOD-IDLEVNR-FRAM-ATTR-IN         
225297               ELSE                                                       
225298                 MOVE MFS-ALFA-FAELT-FEL                                  
225299                                      TO MOD-IDLEVNR-FRAM-ATTR-IN         
225301                 MOVE NOO             TO INDATA-SW                        
225302                 MOVE MED-2           TO MOD-TEMFSINF                     
225304               END-IF                                                     
225305             ELSE                                                         
225306*------------ DISTRICT DETAILS FOR OTHER DCS ARE STORED ON WDB616         
225307               MOVE LEVDC-DCS-IDDC    TO W-IDDC-REF                       
225308               PERFORM IMS-GU-WDB616                                      
225309               IF  SEGMENT-FOUND                                          
225310               AND REF-IDDISTR-REFILL > ZERO                              
225311                 MOVE MFS-ALFA-FAELT-RAETT                                
225312                                      TO MOD-IDLEVNR-FRAM-ATTR-IN         
225314               ELSE                                                       
225315                 MOVE MFS-ALFA-FAELT-FEL                                  
225316                                      TO MOD-IDLEVNR-FRAM-ATTR-IN         
225318                 MOVE NOO             TO INDATA-SW                        
225319                 MOVE MED-2           TO MOD-TEMFSINF                     
225320               END-IF                                                     
225321             END-IF                                                       
225322           ELSE                                                           
225323             MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-FRAM-ATTR-IN         
225326             MOVE NOO                 TO INDATA-SW                        
225327             MOVE MED-2               TO MOD-TEMFSINF                     
225328           END-IF                                                         
225330         END-IF                                                           
225340                                                                          
225600         IF INDATA-OK                                                     
225610*- CHECK SO THERE IS NO FUTURE FORECAST IN PURCHASING SEGMENT             
225620*- WDK722. CHANGE PURCH TO REFILL                                         
225700           IF WDK722-FINNS                                                
225800             IF XLAG-KVPB-JUST1 > ZERO                                    
225900             OR XLAG-KVPB-JUST2 > ZERO                                    
226000               MOVE NOO              TO INDATA-SW                         
226100               MOVE MED-1            TO MOD-TEMFSINF                      
226200             END-IF                                                       
226300           END-IF                                                         
226400         END-IF                                                           
226500       ELSE                                                               
226600*- EJ OK ATT BYTA REF-TO-REF HÄR, GÖRS I REFILLEN.                        
226700         MOVE NOO                    TO INDATA-SW                         
226900         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
227000         MOVE MFS-ALPHA-FIELD-WRONG  TO                                   
227100                                MOD-IDLEVNR-FRAM-ATTR-IN                  
227200       END-IF                                                             
227300     END-IF                                                               
227400     .                                                                    
227500     EJECT                                                                
227600 H-UPDATE SECTION.                                                        
227700     MOVE 'H-UPDATE '  TO CURRENT-SECTION                                 
227800                                                                          
227900     PERFORM IMS-GHU-WDK711                                               
228000     IF SEGMENT-FOUND                                                     
228100                                                                          
228200*-FIELDS CAN BE UPDATED ONLY IF IT'S A LOCALLY SUPPLIED PART              
228300       IF CHECK-LOCAL-PART-SW = YES                                       
228400                                                                          
228500         PERFORM HA-UPDATE-DC-WDK711                                      
228600                                                                          
228700         PERFORM IMS-GHNP-WDK722                                          
228800         IF SEGMENT-FOUND                                                 
228900           PERFORM HB-UPDATE-PROC-DATA                                    
229000         ELSE                                                             
229100           MOVE ALL '+'      TO WDK7-W005WDK7                             
229200           PERFORM S01-INSERT-WDK722                                      
229300                                                                          
229400           PERFORM IMS-GHU-WDK722                                         
229500           IF SEGMENT-FOUND                                               
229600             PERFORM HB-UPDATE-PROC-DATA                                  
229700           END-IF                                                         
229800         END-IF                                                           
229900       END-IF                                                             
230000                                                                          
230100       IF (MID-IDLEVNR-FRAM-IN = ALL '+') AND                             
230200          (MID-IDLEVNR-SHIP-FRAM-IN = ALL '+') AND                        
230300          (MID-TILEVDAT-IN = ALL '+')                                     
230400                                                                          
230500         CONTINUE                                                         
230600       ELSE                                                               
230700         PERFORM HC-UPDATE-MFG                                            
230800       END-IF                                                             
230900                                                                          
231000       IF (MID-NOTES-1 = ALL '+') AND                                     
231100          (MID-NOTES-2 = ALL '+')                                         
231200                                                                          
231300         CONTINUE                                                         
231400       ELSE                                                               
231500         PERFORM HD-UPDATE-NOTES                                          
231600       END-IF                                                             
231700                                                                          
231800       IF MID-DAPUBL-IN     NOT = ALL '+'                                 
231900         PERFORM HI-UPDATE-DAPUBL                                         
232000       END-IF                                                             
232100                                                                          
232200       MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                        
232300       CALL WMEDKONV            USING MED-WMEDAREA                        
232400       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
232500       PERFORM MFS-FORM-ATTR                                              
232600       PERFORM MFS-ERASE-FIELD-IN                                         
232700     ELSE                                                                 
232800       MOVE ERR-NO-UPDATE-DONE     TO MED-IDMFSFEL                        
232900       CALL WMEDKONV USING MED-WMEDAREA                                   
233000       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
233100       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
233200     END-IF                                                               
233300                                                                          
233400     .                                                                    
233500     EJECT                                                                
233600 HA-UPDATE-DC-WDK711 SECTION.                                             
233700     MOVE 'HA-UPDATE-DC-WDK711 '   TO CURRENT-SECTION                     
233800                                                                          
233900     MOVE NOO    TO REPLACE-WDK711-SW                                     
234000                                                                          
234100*UPDATE WILSON FLAG                                                       
234200     IF MID-FLWILSON-IN NOT = ALL '+'                                     
234300       IF MID-FLWILSON-IN = 'Y' OR YES                                    
234400          MOVE YES                   TO SLAG-FLWILSON                     
234500       ELSE                                                               
234600          MOVE MID-FLWILSON-IN       TO SLAG-FLWILSON                     
234700       END-IF                                                             
234800       MOVE YES    TO REPLACE-WDK711-SW                                   
234900       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-FLWILSON-ATTR-UT              
235000     END-IF                                                               
235100                                                                          
235200*UPDATE TABLE ID                                                          
235300     IF MID-IDREFTAB-IN NOT = ALL '+'                                     
235400       MOVE W-IDDC                   TO W-IDDC-2501                       
235500       MOVE MID-IDREFTAB-IN          TO W-IDREFTAB-2502                   
235600       PERFORM IMS-GU-WDR2-WDGX2502                                       
235700       IF SEGMENT-FOUND                                                   
235800          MOVE MID-IDREFTAB-IN       TO SLAG-IDREFTAB                     
235900                                                                          
236000          MOVE YES    TO REPLACE-WDK711-SW                                
236100          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-IDREFTAB-ATTR-UT              
236200                                                                          
236300          MOVE W-IDDC                TO W-IDDC-2203                       
236400          MOVE W-IDARTNR             TO 2204-IDARTNR                      
236500          MOVE 22                    TO 2204-KDLPORS                      
236600          PERFORM IMS-ISRT-WDGX2204                                       
236700       END-IF                                                             
236800     END-IF                                                               
236900                                                                          
237000*UPDATE Q-QUANTITY                                                        
237100                                                                          
237200     IF MID-KVREFBER-IN NOT = ALL '+'                                     
237300        MOVE MID-KVREFBER-IN      TO SLAG-KVREFBER                        
237400                                                                          
237500        MOVE YES    TO REPLACE-WDK711-SW                                  
237600        MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVREFBER-ATTR-UT                
237700     END-IF                                                               
237800                                                                          
237900*UPDATE Q-QUANTITY DATE                                                   
238000     IF MID-TIREFPAF-IN         NOT = ALL '+'                             
238100        IF MID-TIREFPAF-IN         > ZERO                                 
238200           MOVE MID-TIREFPAF-IN   TO SLAG-TIREFPAF                        
238300        ELSE                                                              
238400           MOVE ZERO              TO SLAG-TIREFPAF                        
238500        END-IF                                                            
238600                                                                          
238700        MOVE YES                   TO REPLACE-WDK711-SW                   
238800        MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TIREFPAF-ATTR-UT                
238900     END-IF                                                               
239000                                                                          
239100*UPDATE ALERT BL DATE                                                     
239200     IF MID-TISTODAT-LARM-IN NOT = ALL '+'                                
239300        MOVE MID-TISTODAT-LARM-IN TO SLAG-TISTODAT-LARM                   
239400        MOVE YES                  TO REPLACE-WDK711-SW                    
239500        MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TISTODAT-LARM-ATTR-UT           
239600     END-IF                                                               
239700                                                                          
239800     IF REPLACE-WDK711-OK                                                 
239900       PERFORM IMS-REPL-WDK711                                            
240000     END-IF                                                               
240100                                                                          
240200     .                                                                    
240300     EJECT                                                                
240400                                                                          
240500 HB-UPDATE-PROC-DATA SECTION.                                             
240600     MOVE 'HB-UPDATE-PROC-DATA '   TO CURRENT-SECTION                     
240700                                                                          
240800     MOVE NOO    TO REPLACE-WDK722-SW                                     
240900                                                                          
241000     PERFORM HBA-UPDATE-LINE-1                                            
241100                                                                          
241200     PERFORM HBB-UPDATE-LINE-2                                            
241300                                                                          
241400     PERFORM HBC-UPDATE-LINE-3                                            
241500                                                                          
241600     PERFORM HBD-UPDATE-LINE-4                                            
241700                                                                          
241800     PERFORM HBE-UPDATE-LINE-5                                            
241900                                                                          
242000     PERFORM HBF-UPDATE-LINE-6                                            
242100                                                                          
242200                                                                          
242300     IF REPLACE-WDK722-OK                                                 
242400       PERFORM IMS-REPL-WDK722                                            
242500     END-IF                                                               
242600                                                                          
242700     .                                                                    
242800     EJECT                                                                
242900 HBA-UPDATE-LINE-1  SECTION.                                              
243000     MOVE 'HBA-UPDATE-LINE-1 '    TO CURRENT-SECTION                      
243100                                                                          
243200* UPDATE PROC                                                             
243300     IF MID-IDANSK-IN NOT = ALL '+'                                       
243400        MOVE MID-IDANSK-IN          TO XLAG-IDANSK                        
243500        MOVE MFS-ADD-HILIGHT-FIELD  TO MOD-IDANSK-ATTR-UT                 
243600                                                                          
243700        MOVE YES    TO REPLACE-WDK722-SW                                  
243800                                                                          
243900        MOVE W-IDDC                 TO W-IDDC-2203                        
244000        MOVE W-IDARTNR              TO 2204-IDARTNR                       
244100        MOVE 15                     TO 2204-KDLPORS                       
244200        PERFORM IMS-ISRT-WDGX2204                                         
244300     END-IF                                                               
244400                                                                          
244500                                                                          
244600* UPDATE PLANGR-AG                                                        
244700     IF MID-IDPLANGR-AG-IN NOT = ALL '+'                                  
244800       MOVE MID-IDPLANGR-AG-IN       TO XLAG-IDPLANGR-AG                  
244900       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-IDPLANGR-AG-ATTR-UT           
245000       MOVE YES    TO REPLACE-WDK722-SW                                   
245100                                                                          
245200* IF PLANGR-AG IS LESS THAN 9, UPDATE THE PROCURER FIELD                  
245300* FROM SUPPLIER DB WDF1                                                   
245400       IF MID-IDPLANGR-AG-IN > ZERO                                       
245500         IF MID-IDPLANGR-AG-IN < 9                                        
245600           MOVE SLAG-IDLEVNR         TO W-IDLEVNR                         
245700           PERFORM IMS-GU-WDF116                                          
245800           IF SEGMENT-FOUND                                               
245900              MOVE NDC-IDANSK-PG(MID-IDPLANGR-AG-IN)                      
246000                                     TO XLAG-IDANSK                       
246100              MOVE MFS-ADD-HILIGHT-FIELD  TO MOD-IDANSK-ATTR-UT           
246200                                                                          
246300              MOVE YES    TO REPLACE-WDK722-SW                            
246400                                                                          
246500              MOVE W-IDDC            TO W-IDDC-2203                       
246600              MOVE W-IDARTNR         TO 2204-IDARTNR                      
246700              MOVE 15                TO 2204-KDLPORS                      
246800              PERFORM IMS-ISRT-WDGX2204                                   
246900           END-IF                                                         
247000         END-IF                                                           
247100       END-IF                                                             
247200     END-IF                                                               
247300                                                                          
247400* UPDATE PURCHASER                                                        
247500     IF MID-IDINK-IN NOT = ALL '+'                                        
247600       MOVE MID-IDINK-IN (1:3)        TO WS-IDINK-3                       
247700       MOVE WS-IDINK                  TO XLAG-IDINK                       
247800       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-IDINK-ATTR-UT                 
247900                                                                          
248000       MOVE YES    TO REPLACE-WDK722-SW                                   
248100     END-IF                                                               
248200                                                                          
248300     .                                                                    
248400     EJECT                                                                
248500                                                                          
248600 HBB-UPDATE-LINE-2 SECTION.                                               
248700     MOVE 'HBB-UPDATE-LINE-2 '    TO CURRENT-SECTION                      
248800*UPDATE BLOCKED QTY                                                       
248900     IF MID-KVSPANT-IN NOT = ALL '+'                                      
249000       MOVE MID-KVSPANT-IN         TO WS-KVSPANT                          
249100       IF WS-KVSPANT  < XLAG-KVSPANT                                      
249200        COMPUTE WS-SLAG-KVROS = SLAG-KVROS-BULK +                         
249300                                SLAG-KVROS-DAG                            
249400        IF WS-SLAG-KVROS > 0                                              
249500           IF SLAG-KDLEVSP = 20 OR 22                                     
249600              CONTINUE                                                    
249700           ELSE                                                           
249800              MOVE ZERO            TO WS-AVAIL-QTY                        
249900              COMPUTE WS-AVAIL-QTY =                                      
250000                      SLAG-KVLS - SLAG-KVUTRS - SLAG-KVRESS               
250100                      - XLAG-KVSPANT                                      
250200              IF WS-AVAIL-QTY > 0                                         
250300                 MOVE W-IDDC       TO W-IDDC-4505                         
250400                 MOVE W-IDARTNR    TO 4506-IDARTNR                        
250500                 MOVE 13           TO 4506-KDTAKORS                       
250600                 MOVE ZERO         TO 4506-KVANTMOT                       
250700                 PERFORM IMS-ISRT-WDGX4506                                
250800              END-IF                                                      
250900           END-IF                                                         
251000        END-IF                                                            
251100       END-IF                                                             
251200       MOVE WS-KVSPANT             TO XLAG-KVSPANT                        
251300       MOVE MFS-ADD-HILIGHT-FIELD  TO MOD-KVSPANT-ATTR-UT                 
251400       MOVE YES    TO REPLACE-WDK722-SW                                   
251500     END-IF                                                               
251600                                                                          
251700*UPDATE STOP REF.UNTIL                                                    
251800     IF MID-TIREFSTO-LOC-IN NOT = ALL '+'                                 
251900        IF MID-TIREFSTO-LOC-IN > ZERO                                     
252000           MOVE MID-TIREFSTO-LOC-IN       TO XLAG-TIREFSTO-LOC            
252100        ELSE                                                              
252200           MOVE ZERO                      TO XLAG-TIREFSTO-LOC            
252300        END-IF                                                            
252400        MOVE MFS-ADD-HILIGHT-FIELD        TO                              
252500                                        MOD-TIREFSTO-LOC-ATTR-UT          
252600                                                                          
252700        MOVE YES    TO REPLACE-WDK722-SW                                  
252800     END-IF                                                               
252900                                                                          
253000*UPDATE JUST-IN-TIME FLAG                                                 
253100     IF MID-FLJIT-IN NOT = ALL '+'                                        
253200       IF MID-FLJIT-IN = 'Y' OR YES                                       
253300          MOVE YES                   TO XLAG-FLJIT                        
253400       ELSE                                                               
253500          MOVE MID-FLJIT-IN          TO XLAG-FLJIT                        
253600       END-IF                                                             
253700       MOVE YES    TO REPLACE-WDK722-SW                                   
253800       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-FLJIT-ATTR-UT                 
253900     END-IF                                                               
254000                                                                          
254100     .                                                                    
254200     EJECT                                                                
254300 HBC-UPDATE-LINE-3   SECTION.                                             
254400     MOVE 'HBC-UPDATE-LINE-3 '   TO CURRENT-SECTION                       
254500                                                                          
254600*UPDATE SAFETY STOCK                                                      
254700     IF MID-KVSLAGER-IN NOT = ALL '+'                                     
254800        MOVE MID-KVSLAGER-IN      TO XLAG-KVSLAGER                        
254900        MOVE YES                  TO REPLACE-WDK722-SW                    
255000        MOVE MFS-ADD-HILIGHT-FIELD                                        
255100                                  TO MOD-KVSLAGER-ATTR-UT                 
255200     END-IF                                                               
255300                                                                          
255400*UPDATE SAFETY STOCK DATE                                                 
255500     IF MID-TIMANSEC-IN         NOT = ALL '+'                             
255600        IF MID-TIMANSEC-IN         > ZERO                                 
255700           MOVE MID-TIMANSEC-IN        TO XLAG-TIMANSEC                   
255800        ELSE                                                              
255900           MOVE ZERO                   TO XLAG-TIMANSEC                   
256000        END-IF                                                            
256100        MOVE MFS-ADD-HILIGHT-FIELD     TO                                 
256200                                     MOD-TIMANSEC-ATTR-UT                 
256300        MOVE YES    TO REPLACE-WDK722-SW                                  
256400     END-IF                                                               
256500                                                                          
256600*UPDATE MINIMUM QUANTITY                                                  
256700     IF MID-KVPALL-IN NOT = ALL '+'                                       
256800       MOVE MID-KVPALL-IN         TO XLAG-KVPALL                          
256900       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVPALL-ATTR-UT                   
257000                                                                          
257100       MOVE YES    TO REPLACE-WDK722-SW                                   
257200                                                                          
257300     END-IF                                                               
257400                                                                          
257500*UPDATE AGREEMENT                                                         
257600     IF MID-KDAVT-IN NOT = ALL '+'                                        
257700       MOVE MID-KDAVT-IN          TO XLAG-KDAVT                           
257800       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KDAVT-ATTR-UT                    
257900                                                                          
258000       MOVE YES    TO REPLACE-WDK722-SW                                   
258100     END-IF                                                               
258200                                                                          
258300     .                                                                    
258400     EJECT                                                                
258500 HBD-UPDATE-LINE-4  SECTION.                                              
258600     MOVE 'HBD-UPDATE-LINE-4 '    TO CURRENT-SECTION                      
258700                                                                          
258800*UPDATE MINIMUM LOAD                                                      
258900     IF MID-KVULOAD-IN NOT = ALL '+'                                      
259000        MOVE MID-KVULOAD-IN        TO XLAG-KVULOAD                        
259100        MOVE YES                   TO REPLACE-WDK722-SW                   
259200        MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVULOAD-ATTR-UT                 
259300                                                                          
259400     END-IF                                                               
259500     .                                                                    
259600     EJECT                                                                
259700 HBE-UPDATE-LINE-5   SECTION.                                             
259800     MOVE 'HBE-UPDATE-LINE-5 '    TO CURRENT-SECTION                      
259900                                                                          
260000* UPDATE LEAD TIME                                                        
260100     IF MID-KVVECKOR-LT-IN NOT = ALL '+'                                  
260200        MOVE MID-KVVECKOR-LT-IN    TO XLAG-KVVECKOR-LT                    
260300                                                                          
260400* ALSO UPDATE XLAG-KVVECKOR-FT                                            
260500        COMPUTE WS-KVVECKOR-FT ROUNDED =                                  
260600            MID-KVVECKOR-LT-IN + (XLAG-KVDAGAR-FFH / 5)                   
260700        MOVE WS-KVVECKOR-FT        TO XLAG-KVVECKOR-FT                    
260800                                                                          
260900        MOVE YES    TO REPLACE-WDK722-SW                                  
261000        MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVVECKOR-LT-ATTR-UT             
261100                                                                          
261200        MOVE W-IDDC               TO W-IDDC-2203                          
261300        MOVE W-IDARTNR            TO 2204-IDARTNR                         
261400        MOVE 22                   TO 2204-KDLPORS                         
261500        PERFORM IMS-ISRT-WDGX2204                                         
261600                                                                          
261700        MOVE W-IDDC               TO W-IDDC-2213                          
261800        MOVE W-IDARTNR            TO 2214-IDARTNR                         
261900        PERFORM IMS-ISRT-WDGX2214                                         
262000     END-IF                                                               
262100                                                                          
262200* UPDATE LEAD TIME DATE                                                   
262300     IF MID-TIMANLED-IN NOT = ALL '+'                                     
262400        IF MID-TIMANLED-IN > ZERO                                         
262500           MOVE MID-TIMANLED-IN       TO XLAG-TIMANLED                    
262600        ELSE                                                              
262700           MOVE ZERO                  TO XLAG-TIMANLED                    
262800                                                                          
262900           MOVE SLAG-IDLEVNR          TO W-IDLEVNR                        
263000           PERFORM IMS-GU-WDF101                                          
263100           IF SEGMENT-FOUND                                               
263200              MOVE LEV-KVVECKOR-LT    TO XLAG-KVVECKOR-LT                 
263300                                                                          
263400              MOVE W-IDDC             TO W-IDDC-2213                      
263500              MOVE W-IDARTNR          TO 2214-IDARTNR                     
263600              PERFORM IMS-ISRT-WDGX2214                                   
263700           END-IF                                                         
263800        END-IF                                                            
263900        MOVE YES    TO REPLACE-WDK722-SW                                  
264000        MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-TIMANLED-ATTR-UT             
264100                                                                          
264200*- MAN SKALL KUNNA NOLLA MANUELL LEDTID UTAN ÄNDRING AV LEDTIDEN.         
264300*- MAN SÄTTER TIDEN TILL ZERO OCH DÅ SKALL MAN LOGGA ÄVEN HÄR.            
264400        IF MID-TIMANLED-IN = ZERO                                         
264500          MOVE W-IDDC               TO W-IDDC-2203                        
264600          MOVE W-IDARTNR            TO 2204-IDARTNR                       
264700          MOVE 22                   TO 2204-KDLPORS                       
264800          PERFORM IMS-ISRT-WDGX2204                                       
264900                                                                          
265000          MOVE W-IDDC               TO W-IDDC-2213                        
265100          MOVE W-IDARTNR            TO 2214-IDARTNR                       
265200          PERFORM IMS-ISRT-WDGX2214                                       
265300        END-IF                                                            
265400     END-IF                                                               
265500                                                                          
265600* UPDATE FINAL PURCHASE INFO                                              
265700                                                                          
265800     PERFORM HBEA-UPD-KVSLUTKP-INFO                                       
265900                                                                          
266000     .                                                                    
266100     EJECT                                                                
266200 HBEA-UPD-KVSLUTKP-INFO SECTION.                                          
266300     MOVE 'HBEA-UPD-KVSLUTKP-INFO '    TO CURRENT-SECTION                 
266400                                                                          
266500     IF MID-KVSLUTKP-IN NOT = ALL '+'                                     
266600        IF MID-KVSLUTKP-IN = ZERO                                         
266700          MOVE ZERO                  TO XLAG-KVSLUTKP                     
266800                                        XLAG-TISLUTKP                     
266900          MOVE MFS-ADD-HILIGHT-FIELD TO                                   
267000                                     MOD-KVSLUTKP-ATTR-IN                 
267100                                     MOD-TISLUTKP-ATTR-IN                 
267200        ELSE                                                              
267300          MOVE MID-KVSLUTKP-IN       TO XLAG-KVSLUTKP                     
267400          MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVSLUTKP-ATTR-IN              
267500        END-IF                                                            
267600                                                                          
267700        MOVE YES    TO REPLACE-WDK722-SW                                  
267800     END-IF                                                               
267900                                                                          
268000     IF MID-TISLUTKP-IN NOT = ALL '+'                                     
268100        IF MID-TISLUTKP-IN > ZERO                                         
268200           MOVE MID-TISLUTKP-IN           TO XLAG-TISLUTKP                
268300        ELSE                                                              
268400           MOVE ZERO                      TO XLAG-TISLUTKP                
268500        END-IF                                                            
268600                                                                          
268700        MOVE YES    TO REPLACE-WDK722-SW                                  
268800        MOVE MFS-ADD-HILIGHT-FIELD        TO                              
268900                                      MOD-TISLUTKP-ATTR-UT                
269000     END-IF                                                               
269100     .                                                                    
269200     EJECT                                                                
269300 HBF-UPDATE-LINE-6  SECTION.                                              
269400     MOVE 'HBF-UPDATE-LINE-6 '   TO CURRENT-SECTION                       
269500                                                                          
269600*UPDATE OPTIMAL PLAN FLAG                                                 
269700                                                                          
269800     IF MID-KDOPPLAN-IN NOT = ALL '+'                                     
269900       IF MID-KDOPPLAN-IN = XLAG-KDOPPLAN                                 
270000         CONTINUE                                                         
270100       ELSE                                                               
270200         IF (MID-KDOPPLAN-IN = 'Y' OR YES) AND                            
270300            XLAG-KDOPPLAN = 'N'                                           
270400                                                                          
270500           MOVE 20                     TO 2204-KDLPORS                    
270600         END-IF                                                           
270700                                                                          
270800         IF MID-KDOPPLAN-IN = 'N' AND                                     
270900           (XLAG-KDOPPLAN = 'Y' OR YES)                                   
271000                                                                          
271100           MOVE 22                     TO 2204-KDLPORS                    
271200         END-IF                                                           
271300                                                                          
271400         IF MID-KDOPPLAN-IN = 'Y' OR YES                                  
271500            MOVE YES                   TO XLAG-KDOPPLAN                   
271600         ELSE                                                             
271700            MOVE MID-KDOPPLAN-IN       TO XLAG-KDOPPLAN                   
271800         END-IF                                                           
271900                                                                          
272000         MOVE YES    TO REPLACE-WDK722-SW                                 
272100         MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-KDOPPLAN-ATTR-UT            
272200                                                                          
272300         MOVE W-IDDC                   TO W-IDDC-2203                     
272400         MOVE W-IDARTNR                TO 2204-IDARTNR                    
272500         PERFORM IMS-ISRT-WDGX2204                                        
272600       END-IF                                                             
272700     END-IF                                                               
272800                                                                          
272900     PERFORM VARYING INDX FROM 1 BY 1                                     
273000     UNTIL INDX > MAX-DAYS-INDX                                           
273100       IF MID-DISP-DAY (INDX) NOT = ALL '+'                               
273200          PERFORM HBFA-UPD-DISP-DAY                                       
273300       ELSE                                                               
273400           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
273500                                   TO MOD-DISP-DAY(INDX)                  
273600                                      MOD-SAVE-DISP-DAY(INDX)             
273700       END-IF                                                             
273800     END-PERFORM                                                          
273900     .                                                                    
274000     EJECT                                                                
274100                                                                          
274200 HBFA-UPD-DISP-DAY    SECTION.                                            
274300     MOVE 'HBFA-UPD-DISP-DAY '   TO CURRENT-SECTION                       
274400                                                                          
274500     IF MID-DISP-DAY(INDX) = MID-SAVE-DAY(INDX)                           
274600        CONTINUE                                                          
274700     ELSE                                                                 
274800        EVALUATE MID-SAVE-DAY(INDX)                                       
274900          WHEN 'MO'                                                       
275000             MOVE ZERO TO XLAG-TILEVDAG(1)                                
275100          WHEN 'TU'                                                       
275200             MOVE ZERO TO XLAG-TILEVDAG(2)                                
275300          WHEN 'WE'                                                       
275400             MOVE ZERO TO XLAG-TILEVDAG(3)                                
275500          WHEN 'TH'                                                       
275600             MOVE ZERO TO XLAG-TILEVDAG(4)                                
275700          WHEN 'FR'                                                       
275800             MOVE ZERO TO XLAG-TILEVDAG(5)                                
275900          WHEN OTHER                                                      
276000             CONTINUE                                                     
276100        END-EVALUATE                                                      
276200                                                                          
276300        EVALUATE MID-DISP-DAY(INDX)                                       
276400          WHEN 'MO'                                                       
276500             MOVE 1    TO XLAG-TILEVDAG(1)                                
276600          WHEN 'TU'                                                       
276700             MOVE 2    TO XLAG-TILEVDAG(2)                                
276800          WHEN 'WE'                                                       
276900             MOVE 3    TO XLAG-TILEVDAG(3)                                
277000          WHEN 'TH'                                                       
277100             MOVE 4    TO XLAG-TILEVDAG(4)                                
277200          WHEN 'FR'                                                       
277300             MOVE 5    TO XLAG-TILEVDAG(5)                                
277400        END-EVALUATE                                                      
277500     END-IF                                                               
277600                                                                          
277700     MOVE YES    TO REPLACE-WDK722-SW                                     
277800                                                                          
277900     MOVE W-IDDC               TO W-IDDC-2203                             
278000     MOVE W-IDARTNR            TO 2204-IDARTNR                            
278100     MOVE 19                   TO 2204-KDLPORS                            
278200     PERFORM IMS-ISRT-WDGX2204                                            
278300                                                                          
278400     .                                                                    
278500     EJECT                                                                
278600 HC-UPDATE-MFG  SECTION.                                                  
278700     MOVE 'HC-UPDATE-MFG '    TO CURRENT-SECTION                          
278800                                                                          
278900***************************************************************           
279000* SUPPLIER AND/OR SHIP IN THE FUTURE CAN BE UPDATED FOR BOTH A            
279100* REFILLING PART AND LOCALLY SUPPLIED PART                                
279200***************************************************************           
279300                                                                          
279400     MOVE SPACE  TO WS-XLAG-IDLEVNR-FRAM                                  
279500                    WS-XLAG-IDLEVNR-SHIP-FRAM                             
279600     MOVE ZERO   TO WS-XLAG-TILEVDAT                                      
279700     MOVE NOO    TO REPLACE-WDK722-SW                                     
279800                                                                          
279900                                                                          
280000* UPDATE MFG                                                              
280100     IF MID-IDLEVNR-FRAM-IN NOT = ALL '+'                                 
280200        MOVE MID-IDLEVNR-FRAM-IN    TO WS-XLAG-IDLEVNR-FRAM               
280300        MOVE MFS-ADD-HILIGHT-FIELD  TO                                    
280400                                    MOD-IDLEVNR-FRAM-ATTR-UT              
280500        MOVE YES                    TO WS-MFG-UPD                         
280600        MOVE YES  TO REPLACE-WDK722-SW                                    
280700     END-IF                                                               
280800                                                                          
280900* UPDATE MFG DATE                                                         
281000     IF MID-TILEVDAT-IN NOT = ALL '+'                                     
281100       MOVE MID-TILEVDAT-IN          TO WS-XLAG-TILEVDAT                  
281200       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-TILEVDAT-ATTR-UT              
281300                                                                          
281400       MOVE YES  TO REPLACE-WDK722-SW                                     
281500                                                                          
281600       MOVE YES  TO WS-LEVDAT-UPD                                         
281700     END-IF                                                               
281800                                                                          
281900* UPDATE SHP                                                              
282000     IF MID-IDLEVNR-SHIP-FRAM-IN NOT = ALL '+'                            
282100        MOVE MID-IDLEVNR-SHIP-FRAM-IN TO                                  
282200                                   WS-XLAG-IDLEVNR-SHIP-FRAM              
282300        MOVE MID-IDLEVNR-FRAM-IN   TO WS-XLAG-IDLEVNR-FRAM                
282400        MOVE MFS-ADD-HILIGHT-FIELD TO                                     
282500                                   MOD-IDLEVNR-SHIP-FRAM-ATTR-UT          
282600                                   MOD-IDLEVNR-FRAM-ATTR-UT               
282700                                                                          
282800        MOVE YES   TO REPLACE-WDK722-SW                                   
282900                                                                          
283000        MOVE YES                   TO WS-SHP-UPD                          
283100     END-IF                                                               
283200                                                                          
283300     IF WS-MFG-UPD = YES                                                  
283400     OR WS-SHP-UPD = YES                                                  
283500     OR WS-LEVDAT-UPD = YES                                               
283600       MOVE W-IDDC                     TO W-IDDC-2254                     
283700       MOVE W-IDARTNR                  TO W-IDARTNR-2254                  
283800       PERFORM IMS-GHU-WDGX2254                                           
283900       IF SEGMENT-FOUND                                                   
284000          PERFORM HCA-UPD-WDG3-2254                                       
284100          IF MID-TILEVDAT-IN > ZERO                                       
284200             PERFORM IMS-REPL-WDGX2254                                    
284300          ELSE                                                            
284400             PERFORM IMS-DLET-WDGX2254                                    
284500             MOVE SPACE                TO XLAG-IDLEVNR-FRAM               
284600                                          XLAG-IDLEVNR-SHIP-FRAM          
284700          END-IF                                                          
284800       ELSE                                                               
284900          IF MID-TILEVDAT-IN > ZERO                                       
285000             INITIALIZE 2254-WDGX2254                                     
285100             MOVE W-IDDC               TO 2254-IDDC                       
285200             MOVE W-IDARTNR            TO 2254-IDARTNR                    
285300             PERFORM HCA-UPD-WDG3-2254                                    
285400             PERFORM IMS-ISRT-WDGX2254                                    
285500          END-IF                                                          
285600       END-IF                                                             
285700                                                                          
285800       IF REPLACE-WDK722-OK                                               
285900         PERFORM IMS-GHU-WDK722                                           
286000                                                                          
286100         IF SEGMENT-FOUND                                                 
286200           IF  WS-XLAG-IDLEVNR-FRAM > SPACE                               
286300           OR (WS-XLAG-IDLEVNR-FRAM = SPACE AND                           
286400              WS-XLAG-TILEVDAT = ZERO)                                    
286500             MOVE WS-XLAG-IDLEVNR-FRAM TO XLAG-IDLEVNR-FRAM               
286600           END-IF                                                         
286700                                                                          
286800           IF WS-XLAG-IDLEVNR-SHIP-FRAM > SPACE                           
286900           OR (WS-XLAG-IDLEVNR-SHIP-FRAM = SPACE AND                      
287000              WS-XLAG-TILEVDAT = ZERO)                                    
287100              MOVE WS-XLAG-IDLEVNR-SHIP-FRAM TO                           
287200                                          XLAG-IDLEVNR-SHIP-FRAM          
287300           END-IF                                                         
287400                                                                          
287500*          IF WS-XLAG-TILEVDAT > ZERO                                     
287600             MOVE WS-XLAG-TILEVDAT     TO XLAG-TILEVDAT                   
287700*          END-IF                                                         
287800                                                                          
287900           PERFORM IMS-REPL-WDK722                                        
288000         ELSE                                                             
288100           MOVE ALL '+'       TO WDK7-W005WDK7                            
288200                                                                          
288300           IF WS-XLAG-IDLEVNR-FRAM > SPACE                                
288400             MOVE WS-XLAG-IDLEVNR-FRAM TO WDK7-IDLEVNR-FRAM               
288500           END-IF                                                         
288600                                                                          
288700           IF WS-XLAG-IDLEVNR-SHIP-FRAM > SPACE                           
288800              MOVE WS-XLAG-IDLEVNR-SHIP-FRAM TO                           
288900                                          WDK7-IDLEVNR-SHIP-FRAM          
289000           END-IF                                                         
289100                                                                          
289200           IF WS-XLAG-TILEVDAT > ZERO                                     
289300             MOVE WS-XLAG-TILEVDAT     TO WDK7-TILEVDAT                   
289400           END-IF                                                         
289500                                                                          
289600           PERFORM S01-INSERT-WDK722                                      
289700         END-IF                                                           
289800       END-IF                                                             
289900     END-IF                                                               
290000                                                                          
290100     .                                                                    
290200     EJECT                                                                
290300 HCA-UPD-WDG3-2254 SECTION.                                               
290400     MOVE 'HCA-UPD-WDG3-2254 '    TO CURRENT-SECTION                      
290500                                                                          
290600     IF WS-MFG-UPD = YES                                                  
290700        MOVE MID-IDLEVNR-FRAM-IN   TO 2254-IDLEVNR-FRAM                   
290800        IF WS-SHP-UPD = NOO                                               
290900          MOVE SPACE               TO 2254-IDLEVNR-SHIP-FRAM              
291000        END-IF                                                            
291100     END-IF                                                               
291200                                                                          
291300     IF WS-SHP-UPD = YES                                                  
291400        MOVE MID-IDLEVNR-SHIP-FRAM-IN                                     
291500                                   TO 2254-IDLEVNR-SHIP-FRAM              
291600        MOVE MID-IDLEVNR-FRAM-IN   TO 2254-IDLEVNR-FRAM                   
291700     END-IF                                                               
291800                                                                          
291900     MOVE MID-TILEVDAT-IN          TO 2254-TILEVDAT                       
292000                                                                          
292100     .                                                                    
292200     EJECT                                                                
292300 HD-UPDATE-NOTES SECTION.                                                 
292400     MOVE 'HD-UPDATE-NOTES '    TO CURRENT-SECTION                        
292500                                                                          
292600* UPDATE NOTES 1 LINE                                                     
292700     IF MID-NOTES-1 = ALL '+'                                             
292800     AND MID-NOTES-2 = ALL '+'                                            
292900        CONTINUE                                                          
293000     ELSE                                                                 
293100       MOVE W-IDDC                TO W-IDDC-2261                          
293200       PERFORM IMS-GHU-WDGX2262                                           
293300       IF SEGMENT-FOUND                                                   
293400         IF MID-NOTES-1 NOT = ALL '+'                                     
293500           IF MID-NOTES-1 = SPACE                                         
293600             MOVE SPACE         TO MOD-NOTES-1                            
293700                                   2262-TEREFMED (1:36)                   
293800           ELSE                                                           
293900             MOVE MID-NOTES-1   TO MOD-NOTES-1                            
294000                                   2262-TEREFMED (1:36)                   
294100           END-IF                                                         
294200           MOVE MFS-ADD-HILIGHT-FIELD                                     
294300                                   TO MOD-NOTES-1-ATTR-IN                 
294400         END-IF                                                           
294500* UPDATE NOTES 2 LINE                                                     
294600         IF MID-NOTES-2 NOT = ALL '+'                                     
294700           IF MID-NOTES-2 = SPACE                                         
294800             MOVE SPACE         TO MOD-NOTES-2                            
294900                                   2262-TEREFMED (37:39)                  
295000           ELSE                                                           
295100             MOVE MID-NOTES-2   TO MOD-NOTES-2                            
295200                                   2262-TEREFMED (37:39)                  
295300           END-IF                                                         
295400           MOVE MFS-ADD-HILIGHT-FIELD                                     
295500                                   TO MOD-NOTES-2-ATTR-IN                 
295600         END-IF                                                           
295700         PERFORM IMS-REPL-WDGX2262                                        
295800       ELSE                                                               
295900         MOVE W-IDARTNR     TO 2262-IDARTNR                               
296000         MOVE SPACE         TO 2262-TEREFMED                              
296100         IF MID-NOTES-1 NOT = ALL '+'                                     
296200            MOVE MID-NOTES-1   TO 2262-TEREFMED (1:36)                    
296300                                  MOD-NOTES-1                             
296400            MOVE MFS-ADD-HILIGHT-FIELD                                    
296500                                 TO MOD-NOTES-1-ATTR-IN                   
296600         END-IF                                                           
296700* UPDATE NOTES 2 LINE                                                     
296800         IF MID-NOTES-2 NOT = ALL '+'                                     
296900            MOVE MID-NOTES-2   TO 2262-TEREFMED (37:39)                   
297000                                  MOD-NOTES-2                             
297100            MOVE MFS-ADD-HILIGHT-FIELD                                    
297200                                 TO MOD-NOTES-2-ATTR-IN                   
297300         END-IF                                                           
297400         PERFORM IMS-ISRT-WDGX2262                                        
297500       END-IF                                                             
297600     END-IF                                                               
297700                                                                          
297800     .                                                                    
297900     EJECT                                                                
298000                                                                          
298100 HI-UPDATE-DAPUBL SECTION.                                                
298200     MOVE 'HI-UPDATE-DAPUBL '  TO CURRENT-SECTION                         
298300                                                                          
298400* PUBLICATION DATE CAN BE UPDATED FOR A BOTH A REFILLING PART AND         
298500* LOCALLY SUPPLIED PART                                                   
298600*                                                                         
298700     MOVE DCS-IDLANDX2          TO W-IDLANDX2                             
298800     PERFORM IMS-GHU-WDK712                                               
298900                                                                          
299000     IF MID-DAPUBL-IN > ZERO                                              
299100        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
299200        MOVE MID-DAPUBL-IN      TO DAT-I-TIDATUM                          
299300                                                                          
299400        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
299500                            DAT-O-TIDATUM DAT-KDSVAR                      
299600                                                                          
299700        IF DAT-KDSVAR-OK                                                  
299800           MOVE DAT-TIAAMMDD    TO LART-DAPUBL                            
299900           ADD 20000000         TO LART-DAPUBL                            
300000        END-IF                                                            
300100     ELSE                                                                 
300200        MOVE ZERO               TO LART-DAPUBL                            
300300     END-IF                                                               
300400                                                                          
300500     PERFORM IMS-REPL-WDK712                                              
300600     MOVE MFS-ADD-HILIGHT-FIELD TO                                        
300700                                 MOD-DAPUBL-ATTR-UT                       
300800                                                                          
300900     PERFORM HIA-CHECK-CREATE-ALERT                                       
301000                                                                          
301100     .                                                                    
301200     EJECT                                                                
301300*                                                                         
301400*----------------------------------------------------                     
301500* AN ALERT WILL BE CREATED ON WDR5-H-TYPE 2223 SEGMENT IF THE             
301600* FOLLOWING CONDITIONS ARE SATISFIED                                      
301700*                                                                         
301800*   IF WDK601-ART-FLERS = 'J'  AND                                        
301900*   AND IF THE PART ON WDD7A1 IS A REPLACING PART                         
302000*   AND WDK712-LART-TIERSDAT-VIPS = ZERO                                  
302100*   AND A DELIVERY SCHEDULE EXISTS( WDD902-SEGMENT-FOUND)                 
302200*   AND A CALL OFF EXISTS(WDD905-SEGMENT-FOUND)                           
302300*----------------------------------------------------                     
302400 HIA-CHECK-CREATE-ALERT SECTION.                                          
302500     MOVE 'HIA-CHECK-CREATE-ALERT '   TO CURRENT-SECTION                  
302600                                                                          
302700     MOVE W-IDARTNR   TO WS-IDARTNR-IN                                    
302800                                                                          
302900     PERFORM IMS-GU-WDK601                                                
303000     IF SEGMENT-FOUND                                                     
303100        MOVE NOO                     TO SW-ALARM                          
303200        IF ART-FLERS = YES                                                
303300           MOVE W-IDARTNR            TO W-IDARTNR-MIN7                    
303400                                        W-IDARTNR-MAX7                    
303500           PERFORM IMS-GN-WDD7A1                                          
303600           IF SEGMENT-FOUND                                               
303700             PERFORM UNTIL SEGMENT-MISSING                                
303800                     OR SW-ALARM = YES                                    
303900               MOVE ERS-IDARTNR       TO W-IDARTNR                        
304000               PERFORM IMS-GU-WDK601                                      
304100               IF SEGMENT-FOUND AND ART-KDERS-UTG = ZERO                  
304200                  PERFORM IMS-GNP-WDK611                                  
304300                  IF SEGMENT-FOUND AND CLAG-KDERS < 20                    
304400                    MOVE ERS-IDARTNR  TO W-IDARTNR                        
304500                    MOVE DCS-IDLANDX2 TO W-IDLANDX2                       
304600                    PERFORM IMS-GU-WDK712-ERS                             
304700                    IF SEGMENT-FOUND                                      
304800                      IF LART-TIERSDAT-VIPS = ZERO                        
304900                         MOVE WS-IDARTNR-IN   TO W-IDARTNR                
305000                         PERFORM HIAA-CHECK-FOR-DLVRY-SCHEDULE            
305100                      END-IF                                              
305200                    END-IF                                                
305300                  END-IF                                                  
305400               END-IF                                                     
305500               PERFORM IMS-GN-WDD7A1                                      
305600             END-PERFORM                                                  
305700           END-IF                                                         
305800        END-IF                                                            
305900*******ÅTERSTÄLL NYCKELN                                                  
306000       MOVE WS-IDARTNR-IN   TO W-IDARTNR                                  
306100     END-IF                                                               
306200                                                                          
306300                                                                          
306400     IF SW-ALARM = YES                                                    
306500        PERFORM HIAB-GET-PROCURER                                         
306600        PERFORM HIAC-INSERT-ALERT-600                                     
306700     END-IF                                                               
306800     .                                                                    
306900     EJECT                                                                
307000 HIAA-CHECK-FOR-DLVRY-SCHEDULE SECTION.                                   
307100     MOVE 'HIAA-CHECK-FOR-DLVRY-SCHEDULE' TO CURRENT-SECTION              
307200                                                                          
307300     MOVE W-IDARTNR              TO W-IDARTNR-D9                          
307400     MOVE W-IDDC                 TO W-IDDC-D9                             
307500     PERFORM IMS-GU-WDD901                                                
307600     IF SEGMENT-FOUND                                                     
307700        PERFORM IMS-GNP-WDD902                                            
307800     END-IF                                                               
307900     PERFORM UNTIL SEGMENT-MISSING OR SW-ALARM = YES                      
308000       PERFORM IMS-GNP-WDD905                                             
308100       PERFORM UNTIL SEGMENT-MISSING OR SW-ALARM = YES                    
308200         IF KDAVROP = 2 AND KVAVROP > ZERO                                
308300            MOVE YES             TO SW-ALARM                              
308400         END-IF                                                           
308500         PERFORM IMS-GNP-WDD905                                           
308600       END-PERFORM                                                        
308700       PERFORM IMS-GNP-WDD902                                             
308800     END-PERFORM                                                          
308900                                                                          
309000     .                                                                    
309100     EJECT                                                                
309200 HIAB-GET-PROCURER SECTION.                                               
309300     MOVE 'HIAB-GET-PROCURER '   TO CURRENT-SECTION                       
309400                                                                          
309500     MOVE XLAG-IDANSK              TO W-IDANSK-2232                       
309600     PERFORM IMS-GU-WDR220                                                
309700     IF SEGMENT-FOUND                                                     
309800        MOVE 2232-IDANSK-LARM      TO W-IDANSK-2223                       
309900     ELSE                                                                 
310000        MOVE ZERO                  TO W-IDANSK-2223                       
310100     END-IF                                                               
310200     .                                                                    
310300     EJECT                                                                
310400                                                                          
310500 HIAC-INSERT-ALERT-600 SECTION.                                           
310600     MOVE 'HIAC-INSERT-ALERT-600 '   TO CURRENT-SECTION                   
310700                                                                          
310800* INSERT WDR501/WDGX2223                                                  
310900     MOVE W-IDANSK-2223           TO 2223-IDANSK                          
311000     PERFORM IMS-ISRT-WDR501-WDGX2223                                     
311100                                                                          
311200* INSERT WDR550/WDGX2224                                                  
311300     PERFORM IMS-GHU-WDR501                                               
311400     MOVE FUNCTION CURRENT-DATE(3:6)                                      
311500                                  TO 2224-TISENBEK-DAG                    
311600     MOVE FUNCTION CURRENT-DATE(11:6)                                     
311700                                  TO 2224-TISENBEK-KL                     
311800     MOVE 600                     TO 2224-KDLARM                          
311900     MOVE W-IDARTNR               TO 2224-IDARTNR                         
312000     MOVE W-IDDC                  TO 2224-IDDC                            
312100     MOVE YES                     TO 2224-FLNYLARM                        
312200     MOVE ZERO                    TO 2224-IDDISTR                         
312300                                     2224-IDKUNDNR                        
312400     MOVE '0000000   '            TO 2224-IDKUNDRF                        
312500     MOVE 1                       TO 2224-IDLOPNR                         
312600     MOVE WS-CURRENT-DATE         TO 2224-TIREGDAT                        
312700     MOVE SPACE                   TO 2224-IDTRANS                         
312800                                     2224-KDMFSFOR                        
312900     MOVE ZERO                    TO 2224-IDKR                            
313000     MOVE SLAG-IDLEVNR            TO 2224-IDLEVNR                         
313100     PERFORM IMS-ISRT-WDR550-WDGX2224                                     
313200     .                                                                    
313300     EJECT                                                                
313400 S01-INSERT-WDK722 SECTION.                                               
313500     MOVE 'S01-INSERT-WDK722 '    TO CURRENT-SECTION                      
313600                                                                          
313700     MOVE 'WDK722'           TO WDK7-IDSEGM                               
313800     MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                          
313900     MOVE W-IDDC             TO WDK7-IDDC-KFB                             
314000                                                                          
314100     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
314200                                       WDK7-PCB                           
314300     .                                                                    
314400     EJECT                                                                
314500 S10-AUTH-USER-CHECK SECTION.                                             
314600     MOVE 'S10-AUTH-USER-CHECK '   TO CURRENT-SECTION                     
314700                                                                          
314800     PERFORM IMS-GU-WDK711                                                
314900     IF SEGMENT-FOUND                                                     
315000        MOVE SLAG-IDLEVNR          TO WS-IDLEVNR-8                        
315100        MOVE SLAG-IDDC-REF         TO WS-IDDC-REF                         
315200        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
315300        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
315400*          --- AUTHORIZED USER                                            
315500           SET PASSED-SEC-CHECK    TO TRUE                                
315600        ELSE                                                              
315700           MOVE NOO                TO INDATA-SW                           
315800           MOVE ERR-USER-NOT-AUTH  TO MED-IDMFSFEL                        
315900           CALL WMEDKONV USING MED-WMEDAREA                               
316000           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
316100           PERFORM MFS-ERASE-FIELD-IN                                     
316200           PERFORM MFS-ERASE-FIELD-OUT                                    
316300        END-IF                                                            
316400     ELSE                                                                 
316500       MOVE NOO                    TO INDATA-SW                           
316600       MOVE ERR-PART-MISS-ON-FILE  TO MED-IDMFSFEL                        
316700       CALL WMEDKONV            USING MED-WMEDAREA                        
316800       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
316900       PERFORM MFS-ERASE-FIELD-IN                                         
317000       PERFORM MFS-ERASE-FIELD-OUT                                        
317100     END-IF                                                               
317200     .                                                                    
317300     EJECT                                                                
317400 MFS-ERASE-WDK722-FIELD-OUT SECTION.                                      
317500                                                                          
317600*    --- ALLA UTDATA-FÄLT                                                 
317700     MOVE MFS-ERASE-FIELD     TO MOD-IDANSK-UT                            
317800                                 MOD-IDPLANGR-AG-UT                       
317900                                 MOD-IDINK-UT                             
318000                                 MOD-IDLEVNR-FRAM-UT                      
318100                                 MOD-IDLEVNR-SHIP-FRAM-UT                 
318200                                 MOD-TILEVDAT-UT                          
318300                                 MOD-KVSPANT-UT                           
318400                                 MOD-TIREFSTO-LOC-UT                      
318500                                 MOD-FLJIT-UT                             
318600                                 MOD-KVSLAGER-UT                          
318700                                 MOD-TIMANSEC-UT                          
318800                                 MOD-KVPALL-UT                            
318900                                 MOD-KDAVT-UT                             
319000                                 MOD-KVULOAD-UT                           
319100                                 MOD-TISTODAT-LARM-UT                     
319200                                 MOD-KVVECKOR-LT-UT                       
319300                                 MOD-TIMANLED-UT                          
319400                                 MOD-KVSLUTKP-UT                          
319500                                 MOD-TISLUTKP-UT                          
319600                                 MOD-KDOPPLAN-UT                          
319700                                 MOD-DISP-DAY (1)                         
319800                                 MOD-DISP-DAY (2)                         
319900                                 MOD-DISP-DAY (3)                         
320000                                 MOD-DISP-DAY (4)                         
320100                                 MOD-DISP-DAY (5)                         
320200     .                                                                    
320300     SKIP3                                                                
320400 MFS-ERASE-FIELD-OUT SECTION.                                             
320500                                                                          
320600*    --- ALLA UTDATA-FÄLT                                                 
320700     MOVE MFS-ERASE-FIELD     TO MOD-IDANSK-UT                            
320800                                 MOD-IDPLANGR-AG-UT                       
320900                                 MOD-IDINK-UT                             
321000                                 MOD-IDLEVNR-FRAM-UT                      
321100                                 MOD-IDLEVNR-SHIP-FRAM-UT                 
321200                                 MOD-TILEVDAT-UT                          
321300                                 MOD-KVSPANT-UT                           
321400                                 MOD-TIREFSTO-LOC-UT                      
321500                                 MOD-FLJIT-UT                             
321600                                 MOD-KVSLAGER-UT                          
321700                                 MOD-TIMANSEC-UT                          
321800                                 MOD-KVPALL-UT                            
321900                                 MOD-KDAVT-UT                             
322000                                 MOD-KVULOAD-UT                           
322100                                 MOD-TISTODAT-LARM-UT                     
322200                                 MOD-KVVECKOR-LT-UT                       
322300                                 MOD-TIMANLED-UT                          
322400                                 MOD-KVSLUTKP-UT                          
322500                                 MOD-TISLUTKP-UT                          
322600                                 MOD-KDOPPLAN-UT                          
322700                                 MOD-DAPUBL-UT                            
322800                                 MOD-FLWILSON-UT                          
322900                                 MOD-IDREFTAB-UT                          
323000                                 MOD-KVREFBER-UT                          
323100                                 MOD-TIREFPAF-UT                          
323200     .                                                                    
323300     SKIP3                                                                
323400 MFS-ERASE-FIELD-IN SECTION.                                              
323500                                                                          
323600*    --- ALLA INDATA-FÄLT                                                 
323700     MOVE MFS-ERASE-FIELD     TO MOD-IDANSK-IN                            
323800                                 MOD-IDPLANGR-AG-IN                       
323900                                 MOD-IDINK-IN                             
324000                                 MOD-IDLEVNR-FRAM-IN                      
324100                                 MOD-IDLEVNR-SHIP-FRAM-IN                 
324200                                 MOD-TILEVDAT-IN                          
324300                                 MOD-KVSPANT-IN                           
324400                                 MOD-TIREFSTO-LOC-IN                      
324500                                 MOD-FLJIT-IN                             
324600                                 MOD-KVSLAGER-IN                          
324700                                 MOD-TIMANSEC-IN                          
324800                                 MOD-KVPALL-IN                            
324900                                 MOD-KDAVT-IN                             
325000                                 MOD-KVULOAD-IN                           
325100                                 MOD-TISTODAT-LARM-IN                     
325200                                 MOD-KVVECKOR-LT-IN                       
325300                                 MOD-TIMANLED-IN                          
325400                                 MOD-KVSLUTKP-IN                          
325500                                 MOD-TISLUTKP-IN                          
325600                                 MOD-KDOPPLAN-IN                          
325700                                 MOD-DISP-DAY (1)                         
325800                                 MOD-DISP-DAY (2)                         
325900                                 MOD-DISP-DAY (3)                         
326000                                 MOD-DISP-DAY (4)                         
326100                                 MOD-DISP-DAY (5)                         
326200                                 MOD-SAVE-DISP-DAY (1)                    
326300                                 MOD-SAVE-DISP-DAY (2)                    
326400                                 MOD-SAVE-DISP-DAY (3)                    
326500                                 MOD-SAVE-DISP-DAY (4)                    
326600                                 MOD-SAVE-DISP-DAY (5)                    
326700                                 MOD-DAPUBL-IN                            
326800                                 MOD-FLWILSON-IN                          
326900                                 MOD-IDREFTAB-IN                          
327000                                 MOD-KVREFBER-IN                          
327100                                 MOD-TIREFPAF-IN                          
327200     .                                                                    
327300     EJECT                                                                
327400 MFS-SET-ATTR-FOR-REFILL-PART SECTION.                                    
327500                                                                          
327600     MOVE MFS-CLOSE-FIELD     TO MOD-IDANSK-ATTR-IN                       
327700                                 MOD-IDPLANGR-AG-ATTR-IN                  
327800                                 MOD-IDINK-ATTR-IN                        
327900                                 MOD-IDLEVNR-FRAM-ATTR-IN                 
328000                                 MOD-IDLEVNR-SHIP-FRAM-ATTR-IN            
328100                                 MOD-TILEVDAT-ATTR-IN                     
328200                                 MOD-KVSPANT-ATTR-IN                      
328300                                 MOD-TIREFSTO-LOC-ATTR-IN                 
328400                                 MOD-FLJIT-ATTR-IN                        
328500                                 MOD-KVSLAGER-ATTR-IN                     
328600                                 MOD-TIMANSEC-ATTR-IN                     
328700                                 MOD-KVPALL-ATTR-IN                       
328800                                 MOD-KDAVT-ATTR-IN                        
328900                                 MOD-KVULOAD-ATTR-IN                      
329000                                 MOD-TISTODAT-LARM-ATTR-IN                
329100                                 MOD-KVVECKOR-LT-ATTR-IN                  
329200                                 MOD-TIMANLED-ATTR-IN                     
329300                                 MOD-KVSLUTKP-ATTR-IN                     
329400                                 MOD-TISLUTKP-ATTR-IN                     
329500                                 MOD-KDOPPLAN-ATTR-IN                     
329600                                 MOD-DISP-DAY-ATTR-IN(1)                  
329700                                 MOD-DISP-DAY-ATTR-IN(2)                  
329800                                 MOD-DISP-DAY-ATTR-IN(3)                  
329900                                 MOD-DISP-DAY-ATTR-IN(4)                  
330000                                 MOD-DISP-DAY-ATTR-IN(5)                  
330100                                 MOD-KDOPPLAN-ATTR-IN                     
330200                                 MOD-FLWILSON-ATTR-IN                     
330300                                 MOD-IDREFTAB-ATTR-IN                     
330400                                 MOD-KVREFBER-ATTR-IN                     
330500                                 MOD-TIREFPAF-ATTR-IN                     
330600                                 MOD-NOTES-1-ATTR-IN                      
330700                                 MOD-NOTES-2-ATTR-IN                      
330800     IF LOCK-FIELD                                                        
330900       MOVE MFS-CLOSE-FIELD   TO MOD-DAPUBL-ATTR-IN                       
331000     END-IF                                                               
331100     .                                                                    
331200     EJECT                                                                
331300                                                                          
331400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
331500                                                                          
331600*    --- ALLA UTDATA-FÄLT                                                 
331700     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
331800                                 MOD-IDANSK-UT                            
331900                                 MOD-IDPLANGR-AG-UT                       
332000                                 MOD-IDINK-UT                             
332100                                 MOD-IDLEVNR-FRAM-UT                      
332200                                 MOD-IDLEVNR-SHIP-FRAM-UT                 
332300                                 MOD-TILEVDAT-UT                          
332400                                 MOD-KVSPANT-UT                           
332500                                 MOD-TIREFSTO-LOC-UT                      
332600                                 MOD-FLJIT-UT                             
332700                                 MOD-KVSLAGER-UT                          
332800                                 MOD-TIMANSEC-UT                          
332900                                 MOD-KVPALL-UT                            
333000                                 MOD-KDAVT-UT                             
333100                                 MOD-KVULOAD-UT                           
333200                                 MOD-TISTODAT-LARM-UT                     
333300                                 MOD-KVVECKOR-LT-UT                       
333400                                 MOD-TIMANLED-UT                          
333500                                 MOD-KVSLUTKP-UT                          
333600                                 MOD-TISLUTKP-UT                          
333700                                 MOD-KDOPPLAN-UT                          
333800                                 MOD-KDOPPLAN-UT                          
333900                                 MOD-DISP-DAY (1)                         
334000                                 MOD-DISP-DAY (2)                         
334100                                 MOD-DISP-DAY (3)                         
334200                                 MOD-DISP-DAY (4)                         
334300                                 MOD-DISP-DAY (5)                         
334400                                 MOD-SAVE-DISP-DAY (1)                    
334500                                 MOD-SAVE-DISP-DAY (2)                    
334600                                 MOD-SAVE-DISP-DAY (3)                    
334700                                 MOD-SAVE-DISP-DAY (4)                    
334800                                 MOD-SAVE-DISP-DAY (5)                    
334900                                 MOD-KDOPPLAN-UT                          
335000                                 MOD-DAPUBL-UT                            
335100                                 MOD-FLWILSON-UT                          
335200                                 MOD-IDREFTAB-UT                          
335300                                 MOD-KVREFBER-UT                          
335400                                 MOD-TIREFPAF-UT                          
335500                                 MOD-NOTES-1                              
335600                                 MOD-NOTES-2                              
335700     .                                                                    
335800     SKIP3                                                                
335900 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
336000                                                                          
336100*    --- ALLA INDATA-FÄLT                                                 
336200     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
336300                                 MOD-IDANSK-IN                            
336400                                 MOD-IDPLANGR-AG-IN                       
336500                                 MOD-IDINK-IN                             
336600                                 MOD-IDLEVNR-FRAM-IN                      
336700                                 MOD-IDLEVNR-SHIP-FRAM-IN                 
336800                                 MOD-TILEVDAT-IN                          
336900                                 MOD-KVSPANT-IN                           
337000                                 MOD-TIREFSTO-LOC-IN                      
337100                                 MOD-FLJIT-IN                             
337200                                 MOD-KVSLAGER-IN                          
337300                                 MOD-TIMANSEC-IN                          
337400                                 MOD-KVPALL-IN                            
337500                                 MOD-KDAVT-IN                             
337600                                 MOD-KVULOAD-IN                           
337700                                 MOD-TISTODAT-LARM-IN                     
337800                                 MOD-KVVECKOR-LT-IN                       
337900                                 MOD-TIMANLED-IN                          
338000                                 MOD-KVSLUTKP-IN                          
338100                                 MOD-TISLUTKP-IN                          
338200                                 MOD-KDOPPLAN-IN                          
338300                                 MOD-KDOPPLAN-IN                          
338400                                 MOD-KDOPPLAN-IN                          
338500                                 MOD-DAPUBL-IN                            
338600                                 MOD-FLWILSON-IN                          
338700                                 MOD-IDREFTAB-IN                          
338800                                 MOD-KVREFBER-IN                          
338900                                 MOD-TIREFPAF-IN                          
339000                                 MOD-NOTES-1                              
339100                                 MOD-NOTES-2                              
339200     .                                                                    
339300     EJECT                                                                
339400 MFS-FORM-ATTR SECTION.                                                   
339500                                                                          
339600*    --- ALL INDATA-FIELDS                                                
339700     MOVE MFS-FORMAT-DEFAULT-ATTR TO                                      
339800                                 MOD-IDANSK-ATTR-IN                       
339900                                 MOD-IDPLANGR-AG-ATTR-IN                  
340000                                 MOD-IDINK-ATTR-IN                        
340100                                 MOD-IDLEVNR-FRAM-ATTR-IN                 
340200                                 MOD-IDLEVNR-SHIP-FRAM-ATTR-IN            
340300                                 MOD-TILEVDAT-ATTR-IN                     
340400                                 MOD-KVSPANT-ATTR-IN                      
340500                                 MOD-TIREFSTO-LOC-ATTR-IN                 
340600                                 MOD-FLJIT-ATTR-IN                        
340700                                 MOD-KVSLAGER-ATTR-IN                     
340800                                 MOD-TIMANSEC-ATTR-IN                     
340900                                 MOD-KVPALL-ATTR-IN                       
341000                                 MOD-KDAVT-ATTR-IN                        
341100                                 MOD-KVULOAD-ATTR-IN                      
341200                                 MOD-TISTODAT-LARM-ATTR-IN                
341300                                 MOD-KVVECKOR-LT-ATTR-IN                  
341400                                 MOD-TIMANLED-ATTR-IN                     
341500                                 MOD-KVSLUTKP-ATTR-IN                     
341600                                 MOD-TISLUTKP-ATTR-IN                     
341700                                 MOD-KDOPPLAN-ATTR-IN                     
341800                                 MOD-KDOPPLAN-ATTR-IN                     
341900                                 MOD-DISP-DAY-ATTR-IN(1)                  
342000                                 MOD-DISP-DAY-ATTR-IN(2)                  
342100                                 MOD-DISP-DAY-ATTR-IN(3)                  
342200                                 MOD-DISP-DAY-ATTR-IN(4)                  
342300                                 MOD-DISP-DAY-ATTR-IN(5)                  
342400                                 MOD-KDOPPLAN-ATTR-IN                     
342500                                 MOD-DAPUBL-ATTR-IN                       
342600                                 MOD-FLWILSON-ATTR-IN                     
342700                                 MOD-IDREFTAB-ATTR-IN                     
342800                                 MOD-KVREFBER-ATTR-IN                     
342900                                 MOD-TIREFPAF-ATTR-IN                     
343000                                 MOD-NOTES-1-ATTR-IN                      
343100                                 MOD-NOTES-2-ATTR-IN                      
343200     .                                                                    
343300     SKIP2                                                                
343400* --- IMS SECTIONS ---                                                    
343500     SKIP3                                                                
343600 IMS-GET-MSG SECTION.                                                     
343700                                                                          
343800     MOVE '  QC' TO GOOD-STATUSCODES                                      
343900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
344000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
344100     PERFORM IMS-STATUSCHECK                                              
344200     .                                                                    
344300     SKIP3                                                                
344400 IMS-INSERT-MSG SECTION.                                                  
344500                                                                          
344600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
344700     MOVE SPACE TO GOOD-STATUSCODES                                       
344800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
344900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
345000     PERFORM IMS-STATUSCHECK                                              
345100     .                                                                    
345200     EJECT                                                                
345300 IMS-GU-WDD311 SECTION.                                                   
345400     MOVE 'IMS-GU-WDD311 '   TO DBS-SECTION                               
345500                                                                          
345600     MOVE SPACE               TO ALL-SSA                                  
345700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
345800            DELIMITED BY SIZE INTO SSA1                                   
345900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
346000            DELIMITED BY SIZE INTO SSA2                                   
346100     MOVE '  GE' TO GOOD-STATUSCODES                                      
346200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
346300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
346400     PERFORM IMS-STATUSCHECK                                              
346500     .                                                                    
346600     EJECT                                                                
346700 IMS-GU-WDK601 SECTION.                                                   
346800     MOVE 'IMS-GU-WDK601 '   TO DBS-SECTION                               
346900                                                                          
347000     MOVE SPACE               TO ALL-SSA                                  
347100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
347200          DELIMITED BY SIZE INTO SSA1                                     
347300     MOVE '  GE' TO GOOD-STATUSCODES                                      
347400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
347500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
347600     PERFORM IMS-STATUSCHECK                                              
347700     .                                                                    
347800     EJECT                                                                
347900 IMS-GNP-WDK611 SECTION.                                                  
348000     MOVE 'IMS-GNP-WDK611 '   TO DBS-SECTION                              
348100                                                                          
348200     MOVE SPACE               TO ALL-SSA                                  
348300     MOVE 'WDK611   ' TO SSA1                                             
348400     MOVE '  GE' TO GOOD-STATUSCODES                                      
348500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
348600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
348700     PERFORM IMS-STATUSCHECK                                              
348800     .                                                                    
348900     EJECT                                                                
349000 IMS-GN-WDD7A1 SECTION.                                                   
349100     MOVE 'IMS-GN-WDD7A1 '   TO DBS-SECTION                               
349200                                                                          
349300     MOVE SPACE               TO ALL-SSA                                  
349400     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
349500                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
349600          DELIMITED BY SIZE INTO SSA1                                     
349700     MOVE '  GE' TO GOOD-STATUSCODES                                      
349800     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-WDD7A1 SSA1                   
349900     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
350000     PERFORM IMS-STATUSCHECK                                              
350100     .                                                                    
350200     EJECT                                                                
350300 IMS-GU-WDF101 SECTION.                                                   
350400     MOVE 'IMS-GU-WDF101 '   TO DBS-SECTION                               
350500                                                                          
350600     MOVE SPACE               TO ALL-SSA                                  
350700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
350800          DELIMITED BY SIZE INTO SSA1                                     
350900     MOVE '  GE' TO GOOD-STATUSCODES                                      
351000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
351100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
351200     PERFORM IMS-STATUSCHECK                                              
351300     .                                                                    
351400     EJECT                                                                
351500 IMS-GU-WDF116 SECTION.                                                   
351600     MOVE 'IMS-GU-WDF116 '   TO DBS-SECTION                               
351700                                                                          
351800     MOVE SPACE               TO ALL-SSA                                  
351900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
352000          DELIMITED BY SIZE INTO SSA1                                     
352100     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
352200          DELIMITED BY SIZE INTO SSA2                                     
352300     MOVE '  GE' TO GOOD-STATUSCODES                                      
352400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
352500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
352600     PERFORM IMS-STATUSCHECK                                              
352700     .                                                                    
352800     EJECT                                                                
352900 IMS-GU-WDK711 SECTION.                                                   
353000     MOVE 'IMS-GU-WDK711 '    TO DBS-SECTION                              
353100                                                                          
353200     MOVE SPACE               TO ALL-SSA                                  
353300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
353400          DELIMITED BY SIZE INTO SSA1                                     
353500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
353600          DELIMITED BY SIZE INTO SSA2                                     
353700     MOVE '  GE' TO GOOD-STATUSCODES                                      
353800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
353900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
354000     PERFORM IMS-STATUSCHECK                                              
354100     .                                                                    
354200     EJECT                                                                
354300 IMS-GU-WDK711-REF SECTION.                                               
354400     MOVE 'IMS-GU-WDK711-REF '    TO DBS-SECTION                          
354500                                                                          
354600     MOVE SPACE               TO ALL-SSA                                  
354700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
354800          DELIMITED BY SIZE INTO SSA1                                     
354900     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
355000          DELIMITED BY SIZE INTO SSA2                                     
355100     MOVE '  GE' TO GOOD-STATUSCODES                                      
355200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-K711-REF SSA1 SSA2             
355300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
355400     PERFORM IMS-STATUSCHECK                                              
355500     .                                                                    
355600     EJECT                                                                
355700 IMS-GNP-WDK722 SECTION.                                                  
355800     MOVE 'IMS-GNP-WDK722 '   TO DBS-SECTION                              
355900                                                                          
356000     MOVE SPACE               TO ALL-SSA                                  
356100     MOVE 'WDK722   '         TO SSA1                                     
356200     MOVE '  GE'              TO GOOD-STATUSCODES                         
356300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
356400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
356500     PERFORM IMS-STATUSCHECK                                              
356600     .                                                                    
356700     EJECT                                                                
356800 IMS-GHU-WDK711 SECTION.                                                  
356900     MOVE 'IMS-GHU-WDK711 '   TO DBS-SECTION                              
357000                                                                          
357100     MOVE SPACE               TO ALL-SSA                                  
357200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
357300          DELIMITED BY SIZE INTO SSA1                                     
357400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
357500          DELIMITED BY SIZE INTO SSA2                                     
357600     MOVE '  GE' TO GOOD-STATUSCODES                                      
357700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
357800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
357900     PERFORM IMS-STATUSCHECK                                              
358000     .                                                                    
358100     EJECT                                                                
358200 IMS-REPL-WDK711 SECTION.                                                 
358300     MOVE 'IMS-REPL-WDK711 '    TO DBS-SECTION                            
358400                                                                          
358500     MOVE SPACE               TO ALL-SSA                                  
358600     MOVE '  ' TO GOOD-STATUSCODES                                        
358700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
358800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
358900     PERFORM IMS-STATUSCHECK                                              
359000     .                                                                    
359100     EJECT                                                                
359200 IMS-GHNP-WDK722 SECTION.                                                 
359300     MOVE 'IMS-GHNP-WDK722 '   TO DBS-SECTION                             
359400                                                                          
359500     MOVE SPACE               TO ALL-SSA                                  
359600     MOVE 'WDK722   '         TO SSA1                                     
359700     MOVE '  GE'              TO GOOD-STATUSCODES                         
359800     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK722 SSA1                  
359900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
360000     PERFORM IMS-STATUSCHECK                                              
360100     .                                                                    
360200     EJECT                                                                
360300 IMS-REPL-WDK722 SECTION.                                                 
360400     MOVE 'IMS-REPL-WDK722 '    TO DBS-SECTION                            
360500                                                                          
360600     MOVE SPACE               TO ALL-SSA                                  
360700     MOVE '  ' TO GOOD-STATUSCODES                                        
360800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
360900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
361000     PERFORM IMS-STATUSCHECK                                              
361100     .                                                                    
361200     EJECT                                                                
361300 IMS-GHU-WDK722 SECTION.                                                  
361400     MOVE 'IMS-GHU-WDK722 '   TO DBS-SECTION                              
361500                                                                          
361600     MOVE SPACE               TO ALL-SSA                                  
361700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
361800          DELIMITED BY SIZE INTO SSA1                                     
361900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
362000          DELIMITED BY SIZE INTO SSA2                                     
362100     MOVE 'WDK722   ' TO SSA3                                             
362200     MOVE '  GE'              TO GOOD-STATUSCODES                         
362300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
362400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
362500     PERFORM IMS-STATUSCHECK                                              
362600     .                                                                    
362700     EJECT                                                                
362800 IMS-GU-WDK712-ERS SECTION.                                               
362900     MOVE 'IMS-GU-WDK712-ERS '   TO DBS-SECTION                           
363000                                                                          
363100     MOVE SPACE               TO ALL-SSA                                  
363200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
363300          DELIMITED BY SIZE INTO SSA1                                     
363400     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
363500          DELIMITED BY SIZE INTO SSA2                                     
363600     MOVE '  GE' TO GOOD-STATUSCODES                                      
363700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
363800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
363900     PERFORM IMS-STATUSCHECK                                              
364000     .                                                                    
364100     EJECT                                                                
364200 IMS-GHU-WDK712 SECTION.                                                  
364300     MOVE 'IMS-GHU-WDK712 '   TO DBS-SECTION                              
364400                                                                          
364500     MOVE SPACE               TO ALL-SSA                                  
364600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
364700          DELIMITED BY SIZE INTO SSA1                                     
364800     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
364900          DELIMITED BY SIZE INTO SSA2                                     
365000     MOVE '  ' TO GOOD-STATUSCODES                                        
365100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
365200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
365300     PERFORM IMS-STATUSCHECK                                              
365400     .                                                                    
365500     EJECT                                                                
365600 IMS-REPL-WDK712 SECTION.                                                 
365700     MOVE 'IMS-REPL-WDK712 '   TO DBS-SECTION                             
365800                                                                          
365900     MOVE SPACE               TO ALL-SSA                                  
366000     MOVE '  ' TO GOOD-STATUSCODES                                        
366100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
366200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
366300     PERFORM IMS-STATUSCHECK                                              
366400     .                                                                    
366500     EJECT                                                                
366600 IMS-GHU-WDK727 SECTION.                                                  
366700     MOVE 'IMS-GHU-WDK727 '   TO DBS-SECTION                              
366800                                                                          
366900     MOVE SPACE               TO ALL-SSA                                  
367000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
367100          DELIMITED BY SIZE INTO SSA1                                     
367200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
367300          DELIMITED BY SIZE INTO SSA2                                     
367400     MOVE 'WDK727   ' TO SSA3                                             
367500     MOVE '  GE'              TO GOOD-STATUSCODES                         
367600     CALL CBLTDLI USING GHU WDK72-PCB DLI-IO-WDK727 SSA1 SSA2 SSA3        
367700     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
367800     PERFORM IMS-STATUSCHECK                                              
367900     .                                                                    
368000     EJECT                                                                
368100 IMS-GHU-WDGX2262 SECTION.                                                
368200                                                                          
368300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
368400          DELIMITED BY SIZE INTO SSA1                                     
368500     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-X ')'                         
368600          DELIMITED BY SIZE INTO SSA2                                     
368700     MOVE '  GE' TO GOOD-STATUSCODES                                      
368800     CALL CBLTDLI USING GHU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2            
368900     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
369000     PERFORM IMS-STATUSCHECK                                              
369100     .                                                                    
369200     SKIP3                                                                
369300 IMS-ISRT-WDGX2262   SECTION.                                             
369400                                                                          
369500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
369600          DELIMITED BY SIZE INTO SSA1                                     
369700     MOVE 'WDGX2262 ' TO SSA2                                             
369800     MOVE '  ' TO GOOD-STATUSCODES                                        
369900     CALL CBLTDLI USING ISRT 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2           
370000     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
370100     PERFORM IMS-STATUSCHECK                                              
370200     .                                                                    
370300     SKIP3                                                                
370400 IMS-REPL-WDGX2262 SECTION.                                               
370500                                                                          
370600     MOVE '  ' TO GOOD-STATUSCODES                                        
370700     CALL CBLTDLI USING REPL 2261-PCB DLI-IO-WDGX2262                     
370800     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
370900     PERFORM IMS-STATUSCHECK                                              
371000     .                                                                    
371100     EJECT                                                                
371200 IMS-GU-WDB601 SECTION.                                                   
371300     MOVE 'IMS-GU-WDB601 '   TO DBS-SECTION                               
371400                                                                          
371500     MOVE SPACE               TO ALL-SSA                                  
371600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
371700          DELIMITED BY SIZE INTO SSA1                                     
371800     MOVE '  GE' TO GOOD-STATUSCODES                                      
371900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
372000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
372100     PERFORM IMS-STATUSCHECK                                              
372200     .                                                                    
372300     EJECT                                                                
372400 IMS-GU-WDB601-LEVNR SECTION.                                             
372500     MOVE 'IMS-GU-WDB601-LEVNR '   TO DBS-SECTION                         
372600                                                                          
372700     MOVE SPACE               TO ALL-SSA                                  
372800     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-B6-X ')'                      
372900          DELIMITED BY SIZE INTO SSA1                                     
373000     MOVE '  GE' TO GOOD-STATUSCODES                                      
373100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601-LEV SSA1                  
373200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
373300     PERFORM IMS-STATUSCHECK                                              
373400     .                                                                    
373500     EJECT                                                                
373600 IMS-GU-WDB615 SECTION.                                                   
373700                                                                          
373800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
373900          DELIMITED BY SIZE INTO SSA1                                     
374000     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
374100          DELIMITED BY SIZE INTO SSA2                                     
374200     MOVE '  GE' TO GOOD-STATUSCODES                                      
374300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
374400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
374500     PERFORM IMS-STATUSCHECK                                              
374600     .                                                                    
374700     SKIP3                                                                
374800 IMS-GU-WDB616 SECTION.                                                   
374900                                                                          
375000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
375100          DELIMITED BY SIZE INTO SSA1                                     
375200     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
375300          DELIMITED BY SIZE INTO SSA2                                     
375400     MOVE '  GE' TO GOOD-STATUSCODES                                      
375500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
375600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
375700     PERFORM IMS-STATUSCHECK                                              
375800     .                                                                    
375900 IMS-GU-WDD901 SECTION.                                                   
376000     MOVE 'IMS-GU-WDD901 '   TO DBS-SECTION                               
376100                                                                          
376200     MOVE SPACE               TO ALL-SSA                                  
376300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
376400          DELIMITED BY SIZE INTO SSA1                                     
376500     MOVE '  GE' TO GOOD-STATUSCODES                                      
376600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
376700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
376800     PERFORM IMS-STATUSCHECK                                              
376900     .                                                                    
377000     EJECT                                                                
377100 IMS-GNP-WDD902 SECTION.                                                  
377200     MOVE 'IMS-GNP-WDD902 '   TO DBS-SECTION                              
377300                                                                          
377400     MOVE SPACE               TO ALL-SSA                                  
377500     MOVE 'WDD902    '      TO SSA1                                       
377600     MOVE '  GE' TO GOOD-STATUSCODES                                      
377700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
377800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
377900     PERFORM IMS-STATUSCHECK                                              
378000     .                                                                    
378100     EJECT                                                                
378200 IMS-GNP-WDD905 SECTION.                                                  
378300     MOVE 'IMS-GNP-WDD905 '   TO DBS-SECTION                              
378400                                                                          
378500     MOVE SPACE               TO ALL-SSA                                  
378600     MOVE 'WDD905  '       TO SSA1                                        
378700     MOVE '  GE' TO GOOD-STATUSCODES                                      
378800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
378900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
379000     PERFORM IMS-STATUSCHECK                                              
379100     .                                                                    
379200     EJECT                                                                
379300 IMS-GU-WDR220 SECTION.                                                   
379400     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
379500                                                                          
379600     MOVE SPACE               TO ALL-SSA                                  
379700     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
379800          DELIMITED BY SIZE INTO SSA1                                     
379900     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
380000          DELIMITED BY SIZE INTO SSA2                                     
380100     MOVE '  GE' TO GOOD-STATUSCODES                                      
380200     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
380300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
380400     PERFORM IMS-STATUSCHECK                                              
380500     .                                                                    
380600     SKIP2                                                                
380700 IMS-GHU-WDR501 SECTION.                                                  
380800     MOVE 'IMS-GHU-WDR501 '   TO DBS-SECTION                              
380900                                                                          
381000     MOVE SPACE               TO ALL-SSA                                  
381100     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
381200          DELIMITED BY SIZE INTO SSA1                                     
381300     MOVE '  GE'           TO GOOD-STATUSCODES                            
381400     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2223 SSA1                 
381500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
381600     PERFORM IMS-STATUSCHECK                                              
381700     .                                                                    
381800 IMS-GU-WDR2-WDGX2502 SECTION.                                            
381900     MOVE 'IMS-GU-WDR2-WDGX2502 '   TO DBS-SECTION                        
382000                                                                          
382100     MOVE SPACE               TO ALL-SSA                                  
382200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2501-X ')'                    
382300          DELIMITED BY SIZE INTO SSA1                                     
382400     STRING 'WDGX2502(IDREFTAB =' W-IDREFTAB-X ')'                        
382500          DELIMITED BY SIZE INTO SSA2                                     
382600     MOVE '  GE' TO GOOD-STATUSCODES                                      
382700     CALL CBLTDLI USING GU 2501-PCB DLI-IO-WDGX2502 SSA1 SSA2             
382800     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
382900     PERFORM IMS-STATUSCHECK                                              
383000     .                                                                    
383100     SKIP2                                                                
383200 IMS-GHU-WDGX2254 SECTION.                                                
383300     MOVE 'IMS-GHU-WDGX2254 '   TO DBS-SECTION                            
383400                                                                          
383500     MOVE SPACE               TO ALL-SSA                                  
383600     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2253-X ')'                    
383700          DELIMITED BY SIZE INTO SSA1                                     
383800     STRING 'WDGX2254(KY2254   =' W-WDGXKEY-2254-X ')'                    
383900          DELIMITED BY SIZE INTO SSA2                                     
384000     MOVE '  GE' TO GOOD-STATUSCODES                                      
384100     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-WDGX2254 SSA1 SSA2            
384200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
384300     PERFORM IMS-STATUSCHECK                                              
384400     .                                                                    
384500     SKIP2                                                                
384600 IMS-REPL-WDGX2254 SECTION.                                               
384700     MOVE 'IMS-REPL-WDGX2254 '   TO DBS-SECTION                           
384800                                                                          
384900     MOVE SPACE               TO ALL-SSA                                  
385000     MOVE '  ' TO GOOD-STATUSCODES                                        
385100     CALL CBLTDLI USING REPL WDG3-PCB DLI-IO-WDGX2254                     
385200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
385300     PERFORM IMS-STATUSCHECK                                              
385400     .                                                                    
385500     EJECT                                                                
385600 IMS-DLET-WDGX2254 SECTION.                                               
385700     MOVE 'IMS-DLET-WDGX2254 '   TO DBS-SECTION                           
385800                                                                          
385900     MOVE SPACE               TO ALL-SSA                                  
386000     MOVE '  ' TO GOOD-STATUSCODES                                        
386100     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2254                     
386200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
386300     PERFORM IMS-STATUSCHECK                                              
386400     .                                                                    
386500     EJECT                                                                
386600 IMS-ISRT-WDGX4506 SECTION.                                               
386700     MOVE 'IMS-ISRT-WDGX4506 '   TO DBS-SECTION                           
386800                                                                          
386900     MOVE SPACE               TO ALL-SSA                                  
387000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4505-X ')'                    
387100            DELIMITED BY SIZE INTO SSA1                                   
387200     MOVE 'WDGX4506 '           TO SSA2                                   
387300     MOVE '  II' TO GOOD-STATUSCODES                                      
387400     CALL CBLTDLI USING ISRT WDR4-PCB DLI-IO-WDGX4506 SSA1 SSA2           
387500     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
387600     PERFORM IMS-STATUSCHECK                                              
387700     .                                                                    
387800     EJECT                                                                
387900 IMS-ISRT-WDGX2204 SECTION.                                               
388000     MOVE 'IMS-ISRT-WDGX2204 '   TO DBS-SECTION                           
388100                                                                          
388200     MOVE SPACE               TO ALL-SSA                                  
388300     STRING 'WDG301  (WDG3KEY  =' W-WDGX2203-X ')'                        
388400          DELIMITED BY SIZE INTO SSA1                                     
388500     MOVE   'WDG302  '        TO SSA2                                     
388600     MOVE '  '              TO GOOD-STATUSCODES                           
388700     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
388800     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
388900     PERFORM IMS-STATUSCHECK                                              
389000     .                                                                    
389100     EJECT                                                                
389200 IMS-ISRT-WDGX2214 SECTION.                                               
389300     MOVE 'IMS-ISRT-WDGX2214 '   TO DBS-SECTION                           
389400                                                                          
389500     MOVE SPACE               TO ALL-SSA                                  
389600     STRING 'WDG301  (WDG3KEY  =' W-WDGX2213-X ')'                        
389700          DELIMITED BY SIZE INTO SSA1                                     
389800     MOVE   'WDG302  '        TO SSA2                                     
389900     MOVE '  '              TO GOOD-STATUSCODES                           
390000     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2214 SSA1 SSA2           
390100     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
390200     PERFORM IMS-STATUSCHECK                                              
390300     .                                                                    
390400     EJECT                                                                
390500 IMS-ISRT-WDR501-WDGX2223 SECTION.                                        
390600     MOVE 'IMS-ISRT-WDR501-WDGX2223 '  TO DBS-SECTION                     
390700                                                                          
390800     MOVE SPACE               TO ALL-SSA                                  
390900     MOVE  'WDR501  '         TO SSA1                                     
391000     MOVE '  II'              TO GOOD-STATUSCODES                         
391100     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2223 SSA1                
391200     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
391300     PERFORM IMS-STATUSCHECK                                              
391400     .                                                                    
391500     EJECT                                                                
391600 IMS-ISRT-WDR550-WDGX2224 SECTION.                                        
391700     MOVE 'IMS-ISRT-WDR550-WDGX2224 '   TO DBS-SECTION                    
391800                                                                          
391900     MOVE SPACE               TO ALL-SSA                                  
392000     MOVE  'WDR550  '         TO SSA1                                     
392100     MOVE '  II'              TO GOOD-STATUSCODES                         
392200     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2224 SSA1                
392300     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
392400     PERFORM IMS-STATUSCHECK                                              
392500     .                                                                    
392600 IMS-ISRT-WDGX2254 SECTION.                                               
392700     MOVE 'IMS-ISRT-WDGX2254 '  TO DBS-SECTION                            
392800                                                                          
392900     MOVE SPACE               TO ALL-SSA                                  
393000     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2253-X ')'                    
393100          DELIMITED BY SIZE INTO SSA1                                     
393200     MOVE   'WDGX2254'        TO SSA2                                     
393300     MOVE '  II'              TO GOOD-STATUSCODES                         
393400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2254 SSA1 SSA2           
393500     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
393600     PERFORM IMS-STATUSCHECK                                              
393700     .                                                                    
393800     EJECT                                                                
393900 IMS-STATUSCHECK SECTION.                                                 
394000                                                                          
394100     SET STATUS-IX TO 1                                                   
394200     SEARCH GOOD-STATUS                                                   
394300       AT END                                                             
394400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
394500         DELIMITED BY SIZE INTO ERROR-TEXT                                
394600         CALL FELLOG                                                      
394700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
394800         CONTINUE                                                         
394900     END-SEARCH                                                           
395000     .                                                                    
395100     EJECT                                                                
395200*    -COPY WY2000P1                                                       
