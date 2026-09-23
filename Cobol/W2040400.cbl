000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040400.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   12/10/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN FOR INFORMATION ABOUT INCOMING ORDER STATISTICS.          
000900*        KEYS FOR ENTERING THE SCREEN ARE PART NUMBER ID AND              
001000*        DC-CODE. THE SCREEN HAS SOME UPDATING FUNCTIONALITIES.           
001100*        THIS IS A MIXTURE OF THE SCREENS 2104 AND 2342.                  
001200*        WHEN UPDATING A NEW FORECAST THE PART SHALL BE INSERTED          
001300*        ON THE SELECTED DC WITH THE SAME FUNCTIONALITY AS ON THE         
001400*        2342-SCREEN.                                                     
001500*                                                                         
001600*        THE ONLY VALID KEYS IN THE DC-FIELD SHOULD BE 71-79 CHINA        
001610*        and 41-49 NDC-US.                                                
001700*        REFILL PARTS ARE NOT SUPPORTED BY THE SCREEN                     
001800*                                                                         
001900*        THE PROGRAM READS     WDB6                                       
002000*        THE PROGRAM READS     WDK6                                       
002100*        THE PROGRAM UPDATES   WDK7                                       
002200*        THE PROGRAM READS     WDL7 + WDL4                                
002300*        THE PROGRAM READS     WDL8                                       
002400*        THE PROGRAM READS     WDD3                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: W2T404                                              
002800*        MID:         W2I40401                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        MOD:         W2O40401                                            
003200*                                                                         
003300*                                                                         
003400******************************************************************        
003500*    ÄNDRINGAR:                                                           
003600*    2013-08-27 e'Tracker 10205400  China prio-II listan skall            
003700*                                   kunna uppdatera gamla datum           
003800*                                   blocked until, ESC och Date i         
003900*                                   Planned foercast.                     
004000*                                                                         
004100*                                                                         
004200                                                                          
004300                                                                          
004400 ENVIRONMENT DIVISION.                                                    
004500                                                                          
004600 DATA DIVISION.                                                           
004700                                                                          
004800 WORKING-STORAGE SECTION.                                                 
004900 77  IDPGM                       PIC X(08)   VALUE 'W2040400'.            
005000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005200                                                                          
005300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005500                                                                          
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800 77  HYPHEN                      PIC X       VALUE '-'.                   
005900 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
006000 77  IDLEVNR-WS                  PIC X(5)    VALUE SPACE.                 
006100 77  WS-IDDC-REF                 PIC X(2)    VALUE SPACE.                 
006200 77  WEEK-IX                     PIC S9(9)   COMP SYNC.                   
006300 77  PER-IX                      PIC S9(9)   COMP SYNC.                   
006400 77  MOD-IX                      PIC S9(9)   COMP SYNC.                   
006500 77  IX1                         PIC S9(9)   COMP SYNC.                   
006600                                                                          
006700 01  WS-TODAYS-AAAAMMDD          PIC 9(8).                                
006800 01  FILLER REDEFINES WS-TODAYS-AAAAMMDD.                                 
006900     03  WS-TODAYS-SS            PIC 9(2).                                
007000     03  WS-TODAYS-AAMMDD        PIC 9(6).                                
007100 01  WS-TEMP-AAAAMMDD            PIC 9(8).                                
007200 01  FILLER REDEFINES WS-TEMP-AAAAMMDD.                                   
007300     03  WS-TEMP-SS              PIC 9(2).                                
007400     03  WS-TEMP-AAMMDD          PIC 9(6).                                
007500 01  WS-TISEKEL-AA.                                                       
007600     03  WS-TODAYS-CENTURY       PIC 99.                                  
007700     03  WS-TODAYS-AA            PIC 99.                                  
007800 01  FILLER REDEFINES WS-TISEKEL-AA.                                      
007900     03  WS-TODAYS-TIAAAA        PIC 9(4).                                
008000 01  WS-TODAYS-TIAAAA-1          PIC 9(4).                                
008100 01  WS-TIAAAA                   PIC 9(4)    VALUE ZERO.                  
008200 01  FILLER REDEFINES WS-TIAAAA.                                          
008300     03 FILLER                   PIC 9(2).                                
008400     03 WS-YEAR                  PIC 9(2).                                
008500                                                                          
008600 01  START-DATUM.                                                         
008700     03  START-AA            PIC 99.                                      
008800     03  START-PERIOD        PIC 99.                                      
008900                                                                          
009000 01  WS-FALT.                                                             
009100     03 WS-KVOI-DC               PIC S9(9)   VALUE ZERO.                  
009200     03 WS-KVOT-OH               PIC S9(9)   VALUE ZERO.                  
009300     03 WS-KVOI-DC-SUM           PIC S9(9)   VALUE ZERO.                  
009400     03 WS-KVOT-OH-SUM           PIC S9(9)   VALUE ZERO.                  
009500     03 WS-YEAR-DC-SUM           PIC S9(9)   VALUE ZERO.                  
009600     03 WS-YEAR-OH-SUM           PIC S9(9)   VALUE ZERO.                  
009700     03 WS-KVOI-INNEV-DC-SUM     PIC S9(9)   VALUE ZERO.                  
009800     03 WS-KVOT-INNEV-OH-SUM     PIC S9(9)   VALUE ZERO.                  
009900     03 WS-KVOI-LATE-12-DC       PIC S9(9)   VALUE ZERO.                  
010000     03 WS-KVOT-LATE-12-OH       PIC S9(9)   VALUE ZERO.                  
010100     03 WS-KVOI-LATE-6-DC        PIC S9(9)   VALUE ZERO.                  
010200     03 WS-KVOT-LATE-6-OH        PIC S9(9)   VALUE ZERO.                  
010300     03 WS-KVOI-AVER-12-DC       PIC S9(9)V9 VALUE ZERO.                  
010400     03 WS-KVOI-AVER-6-DC        PIC S9(9)V9 VALUE ZERO.                  
010500     03 WS-KVPB-DC-IN            PIC 9(6)V9  VALUE ZERO.                  
010510     03 WS-KVPBREOI-DC-IN        PIC 9(6)V9  VALUE ZERO.                  
010600     03 WS-KVPB-PLAN-IN          PIC 9(6)V9  VALUE ZERO.                  
010700                                                                          
010800     03 WS-KVOI-OTH               PIC S9(9)   VALUE ZERO.                 
010900     03 WS-KVOI-OTH-TOT OCCURS 12 PIC S9(9).                              
011000     03 WS-KVOI-OTH-SUM           PIC S9(9)   VALUE ZERO.                 
011100     03 WS-YEAR-OTH-SUM           PIC S9(9)   VALUE ZERO.                 
011200     03 WS-YEAR-OTH-TOT OCCURS 3  PIC S9(9).                              
011300     03 WS-KVOI-INNEV-OTH-SUM     PIC S9(9)   VALUE ZERO.                 
011400     03 WS-KVOI-LATE-12-OTH       PIC S9(9)   VALUE ZERO.                 
011500     03 WS-KVOI-LATE-6-OTH        PIC S9(9)   VALUE ZERO.                 
011600     03 WS-KVOI-AVER-12-OTH       PIC S9(9)V9 VALUE ZERO.                 
011700     03 WS-KVOI-AVER-6-OTH        PIC S9(9)V9 VALUE ZERO.                 
011800     03 WS-KVPB-SEP-OTH           PIC 9(6)V9  VALUE ZERO.                 
011900*                                                                         
012000     03 WS-PER                   PIC 9(4)     VALUE ZERO.                 
012100     03 FILLER REDEFINES WS-PER.                                          
012200             05 WS-PER-AA        PIC 9(2).                                
012300             05 WS-PER-PP        PIC 9(2).                                
012400     03 WS-PERIODTABELL-AR-1     OCCURS 12.                               
012500        05 WS-PERTAB-START-VV-1  PIC 9(2).                                
012600        05 WS-PERTAB-END-VV-1    PIC 9(2).                                
012700        05 WS-PERTAB-KVVIPER-1   PIC 9(2).                                
012800     03 WS-PERIODTABELL-AR-0     OCCURS 12.                               
012900        05 WS-PERTAB-START-VV-0  PIC 9(2).                                
013000        05 WS-PERTAB-END-VV-0    PIC 9(2).                                
013100        05 WS-PERTAB-KVVIPER-0   PIC 9(2).                                
013200     03 FILLER                   PIC X(10)   VALUE 'WS-TIAAPP'.           
013300     03 WS-TIAAPP                PIC 9(4)    VALUE ZERO.                  
013400     03  FILLER REDEFINES WS-TIAAPP.                                      
013500        05 WS-TIAA               PIC 9(2).                                
013600        05 WS-TIPP               PIC 9(2).                                
013700     03 WS-END-VV-0              PIC 9(2)    VALUE ZERO.                  
013800     03 WS-END-VV-1              PIC 9(2)    VALUE ZERO.                  
013900                                                                          
014000     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
014100     03  FILLER REDEFINES WS-TIAAVV.                                      
014200         05 WS-TIAAVV-AA         PIC 9(2).                                
014300         05 WS-TIAAVV-VV         PIC 9(2).                                
014400                                                                          
014500     03  WS-PRESENT-WEEK.                                                 
014600         05 WS-PRESENT-WEEK-TEXT PIC X(3)    VALUE 'Pr.'.                 
014700         05 WS-WEEK-IN-PER       PIC 9(1)    VALUE ZERO.                  
014800         05 WS-COLON             PIC X(1)    VALUE SPACE.                 
014900         05 WS-KVVIPER           PIC 9(1)    VALUE ZERO.                  
015000                                                                          
015100 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
015200*01 -COPY WWIDFTG                                                         
015300                                                                          
015400     EJECT                                                                
015500 01  WS-TODAYS-AAVVD.                                                     
015600     03  TODAYS-AA               PIC 99.                                  
015700     03  TODAYS-WEEK             PIC 99.                                  
015800     03  TODAYS-DAG              PIC 9.                                   
015900                                                                          
016000 01  TODAYS-PERIOD               PIC 9(2).                                
016100                                                                          
016200                                                                          
016300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
016400                                                                          
016500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
016600     88  INDATA-OK                           VALUE 'J'.                   
016700     88  INDATA-WRONG                        VALUE 'N'.                   
016800                                                                          
016900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
017000     88  KEYS-OK                             VALUE 'J'.                   
017100     88  KEYS-WRONG                          VALUE 'N'.                   
017200                                                                          
017300 77  REFILL-PART-SW              PIC X       VALUE 'N'.                   
017400     88  REFILL-PART                         VALUE 'J'.                   
017500                                                                          
017600 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
017700     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
017800     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
017900                                                                          
018000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018100     88  OWN-MID                             VALUE '2404'.                
018200     88  GOOD-MID                            VALUE '2401' '2402'          
018300                                                   '2403' '2404'          
018400                                                   '2405' '2406'          
018500                                                   '2407' '2408'          
018600                                                   '2409'.                
018700     88  HELP-MID                            VALUE '0551'.                
018800                                                                          
018900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
019000 01  GENERAL-SUBPROGRAMS.                                                 
019100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
019500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019800                                                                          
019900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
020000*01 -COPY WMEDAREA                                                        
020100                                                                          
020200*    --- PARAMETRAR FOR SUBPROGRAM WDATKONV                               
020300*01 -COPY WDATAREA                                                        
020400                                                                          
020500*    --- PARAMETERS FOR SUBPROGRAM WDECEDIT                               
020600*01  -COPY WDECAREA                                                       
020700                                                                          
020800 01  MESSAGE-CODES.                                                       
020900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
021000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
021100     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
021200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021300     03  ERR-PART-NOT-NUMERIC    PIC X(3)    VALUE '020'.                 
021400     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
021500     03  INF-PART-EXPIRE         PIC X(3)    VALUE '258'.                 
021600     03  INF-REPLACING-PART      PIC X(3)    VALUE '259'.                 
021700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021800     03  INF-UPDATED             PIC X(3)    VALUE '404'.                 
021900     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
022000     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
022100     03  ERR-INVALID-DC          PIC X(3)    VALUE '440'.                 
022200     03  ERR-PART-MISSING        PIC X(3)    VALUE '769'.                 
022300                                                                          
022400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
022500*                                                                         
022600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022700                                                                          
022800*01 -COPY WMSGINIT                                                        
022900                                                                          
023000*    --- PARAMETERS FOR SUB PROGRAM W005WDK7                              
023100*                                                                         
023200 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
023300*   -COPY W005WDK7                                                        
023400                                                                          
023500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
023600*                                                                         
023700 01  SAVE-AREA.                                                           
023800     03  SAVE-IDTRANS            PIC X(4)    VALUE '2404'.                
023900                                                                          
024000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024300                                                                          
024400*01  MID -COPY W2I40401                                                   
024500                                                                          
024600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
024700                                                                          
024800*01  -COPY WMSGAREA                                                       
024900                                                                          
025000     03  MOD REDEFINES MSG-AREA.                                          
025100*      05  -COPY W2O40401                                                 
025200                                                                          
025300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025400                                                                          
025500*01  -COPY WMFSAREA                                                       
025600                                                                          
025700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
025800*                                                                         
025900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026000                                                                          
026100 01  KEYS-FOR-DLI.                                                        
026200     03  W-IDDC-X.                                                        
026300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026400     03  W-IDDC-L7-X.                                                     
026500         05  W-IDDC-L7           PIC X(2)    VALUE SPACE.                 
026600     03  W-IDDCREF-X.                                                     
026700         05  W-IDDCREF           PIC X(2)    VALUE SPACE.                 
026800     03  W-IDARTNR-X.                                                     
026900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027000     03  W-KDSEGKEY-X.                                                    
027100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
027200     03  W-IDSKYLT-X.                                                     
027300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
027400     03  W-TIAAAA-X.                                                      
027500         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
027600                                                                          
027700*    --- STATUS CODES FROM IMS                                            
027800 01  STATUS-WS                   PIC XX.                                  
027900     88  SEGMENT-FOUND                       VALUE '  '.                  
028000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
028200     88  NO-MORE-SEGMENTS                    VALUE 'GB'.                  
028300                                                                          
028400 01  GOOD-STATUSCODES.                                                    
028500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028600                                                                          
028700 01  ALL-SSA.                                                             
028800     03 SSA1                     PIC X(64).                               
028900     03 SSA2                     PIC X(64).                               
029000     03 SSA3                     PIC X(64).                               
029100                                                                          
029200*    --- IMS FUNCTION CODES                                               
029300*01  -COPY W0003                                                          
029400                                                                          
029500*    ---  DLI INPUT-OUTPUT AREA                                           
029600                                                                          
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
029800 01  DLI-IO-WDB601.                                                       
029900*    03  -COPY WDB601                                                     
030000                                                                          
030100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
030200 01  DLI-IO-WDK601.                                                       
030300*    03  -COPY WDK601                                                     
030400                                                                          
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
030600 01  DLI-IO-WDK611.                                                       
030700*    03  -COPY WDK611                                                     
030800                                                                          
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
031000 01  DLI-IO-WDK629.                                                       
031100*    03  -COPY WDK629                                                     
031200                                                                          
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
031400 01  DLI-IO-WDK701.                                                       
031500*    03  -COPY WDK701                                                     
031600                                                                          
031700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
031800 01  DLI-IO-WDK711.                                                       
031900*    03  -COPY WDK711                                                     
032000                                                                          
032100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
032200 01  DLI-IO-WDK722.                                                       
032300*    03  -COPY WDK722                                                     
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL711'.                      
032500 01  DLI-IO-WDL711.                                                       
032600*    03  -COPY WDL711                                                     
032700                                                                          
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
032900 01  DLI-IO-WDL811.                                                       
033000*    03  -COPY WDL811                                                     
033100                                                                          
033200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL411'.                      
033300 01  DLI-IO-WDL411.                                                       
033400*    03  -COPY WDL411                                                     
033500                                                                          
033600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
033700 01  DLI-IO-WDD311.                                                       
033800*    03  -COPY WDD311                                                     
033900                                                                          
034000 LINKAGE SECTION.                                                         
034100*01  -COPY W0009   -PRE MSG-                                              
034200*01  -COPY W0008   -PRE WDP7-                                             
034300     05  FILLER                  PIC X.                                   
034400                                                                          
034500*01  -COPY W0008  -PRE WDB6-                                              
034600     05  FILLER                  PIC X.                                   
034700                                                                          
034800*01  -COPY W0008  -PRE WDK6-                                              
034900     05  FILLER                  PIC X.                                   
035000                                                                          
035100*01  -COPY W0008  -PRE WDK7-                                              
035200     05  FILLER                  PIC X.                                   
035300                                                                          
035400*01  -COPY W0008  -PRE WDL7-                                              
035500     05  FILLER                  PIC X.                                   
035600                                                                          
035700*01  -COPY W0008  -PRE WDL8-                                              
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000*01  -COPY W0008  -PRE WDL4-                                              
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01  -COPY W0008  -PRE WDD3-                                              
036400     05  FILLER                  PIC X.                                   
036500                                                                          
036600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB WDK6-PCB             
036700     WDK7-PCB WDL7-PCB WDL8-PCB WDL4-PCB WDD3-PCB.                        
036800 MAIN SECTION.                                                            
036900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB WDK6-PCB             
037000     WDK7-PCB WDL7-PCB WDL8-PCB WDL4-PCB WDD3-PCB.                        
037100                                                                          
037200     PERFORM IMS-GET-MSG                                                  
037300     IF SEGMENT-FOUND                                                     
037400       PERFORM A-INIT                                                     
037500       PERFORM B-CHECK-KEYS                                               
037600       IF KEYS-OK                                                         
037700         PERFORM S1-SECURITY-CHECK                                        
037800         IF PASSED-SECURITY-CHECK                                         
037900           IF MFS-UPDATE                                                  
038000             PERFORM G-CHECK-INPUT                                        
038100             IF INDATA-OK                                                 
038200               PERFORM H-UPDATE                                           
038300             END-IF                                                       
038400           ELSE                                                           
038500             IF MFS-FIRST                                                 
038600               PERFORM C-FIRST-PAGE                                       
038700             ELSE                                                         
038800               PERFORM E-SAME-PAGE                                        
038900             END-IF                                                       
039000           END-IF                                                         
039100           PERFORM F-READ-SHOW-INFO                                       
039200*---                                                                      
039300*---       SAVE MFG SUPPLIER USING INIT-IO-AREA                           
039400           IF IDLEVNR-WS > SPACES                                         
039500              MOVE ALL '+'           TO MSGI-WMSGINIT                     
039600              MOVE '001'             TO MSGI-KDCALL                       
039700              MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                 
039800              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
039900              MOVE '2404'            TO MSGI-IDTRANS                      
040000              MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                      
040100                                                                          
040200              CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                  
040300           END-IF                                                         
040400         END-IF                                                           
040500       END-IF                                                             
040600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40401 + 4                      
040700       PERFORM IMS-INSERT-MSG                                             
040800     END-IF                                                               
040900                                                                          
041000     MOVE ZERO TO RETURN-CODE                                             
041100     GOBACK                                                               
041200     .                                                                    
041300                                                                          
041400 A-INIT SECTION.                                                          
041500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
041600                                                                          
041700     IF MSG-DOUBLE-TRANSACTIONS                                           
041800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I40401                 
041900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
042000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
042100     ELSE                                                                 
042200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I40401                  
042300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
042400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
042500     END-IF                                                               
042600                                                                          
042700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
042900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
043000                                                                          
043100     MOVE LOW-VALUE TO MSG-AREA                                           
043200     MOVE 'W2O404N1' TO MFS-IDMOD                                         
043300     MOVE '2404' TO MOD-IDTRANS                                           
043400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
043500                                                                          
043600     IF OWN-MID OR HELP-MID                                               
043700       CONTINUE                                                           
043800     ELSE                                                                 
043900       MOVE SPACE TO MFS-KDTRTYP                                          
044000       MOVE '7' TO MFS-IDPFK                                              
044100     END-IF                                                               
044200                                                                          
044300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-TODAYS-AAAAMMDD               
044400                                                                          
044500     PERFORM S2-DATE                                                      
044600     PERFORM AA-INITIALIZE-PERIODTAB                                      
044700                                                                          
044800     MOVE WS-TODAYS-TIAAAA-1 (3:2)                                        
044900                             TO WS-PER-AA                                 
045000     MOVE TODAYS-PERIOD      TO WS-PER-PP                                 
045100     MOVE +1                 TO MOD-IX                                    
045200     MOVE 20                 TO WS-TEMP-SS                                
045300                                                                          
045400     PERFORM UNTIL MOD-IX > +12                                           
045500       MOVE WS-PER           TO MOD-TIAARP (MOD-IX)                       
045600       ADD +1                TO MOD-IX                                    
045700                                WS-PER-PP                                 
045800       IF WS-PER-PP > +12                                                 
045900         MOVE +1             TO WS-PER-PP                                 
046000         IF WS-PER-AA = 99                                                
046100           MOVE ZERO         TO WS-PER-AA                                 
046200         ELSE                                                             
046300           ADD 1             TO WS-PER-AA                                 
046400         END-IF                                                           
046500       END-IF                                                             
046600     END-PERFORM                                                          
046700     MOVE WS-PRESENT-WEEK    TO MOD-WEEK-IN-PERIOD                        
046800     .                                                                    
046900                                                                          
047000 AA-INITIALIZE-PERIODTAB SECTION.                                         
047100     MOVE 'AA-INIT-PER-TAB ' TO CURRENT-SECTION                           
047200*--------------------------------------------------------------*          
047300* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
047400* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
047500* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
047600* PER.NR + START/ENDWEEK FÖR ATT KUNNA UTFÖRA SUMMERINGAR      *          
047700*--------------------------------------------------------------*          
047800                                                                          
047900     COMPUTE WS-TODAYS-TIAAAA-1 = WS-TODAYS-TIAAAA - 1                    
048000                                                                          
048100     PERFORM AAA-INITIALIZE-PERIODTAB-AR-1                                
048200     PERFORM AAB-INITIALIZE-PERIODTAB-AR-0                                
048300     .                                                                    
048400                                                                          
048500 AAA-INITIALIZE-PERIODTAB-AR-1 SECTION.                                   
048600     MOVE 'AAA-INIT-AR-1   ' TO CURRENT-SECTION                           
048700*--------------------------------------------------------------*          
048800* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
048900* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
049000* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
049100* PER.NR + START/ENDWEEK FÖR ATT KUNNA UTFÖRA SUMMERINGAR      *          
049200*--------------------------------------------------------------*          
049300                                                                          
049400     MOVE WS-TODAYS-TIAAAA-1 (3:2)                                        
049500                             TO WS-TIAA                                   
049600     MOVE 1                  TO WS-TIPP                                   
049700                                                                          
049800     PERFORM UNTIL WS-TIPP > 12                                           
049900                                                                          
050000       MOVE 'AARP'           TO DAT-KDDATFORM                             
050100       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
050200                                                                          
050300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
050400                           DAT-O-TIDATUM DAT-KDSVAR                       
050500                                                                          
050600       IF DAT-KDSVAR-OK                                                   
050700*--- START-WEEK ÄR ALLTID WEEK 1 PÅ NYTT ÅR                               
050800         IF DAT-TIVV = +52 OR +53                                         
050900            MOVE 1           TO WS-PERTAB-START-VV-1 (WS-TIPP)            
051000         ELSE                                                             
051100            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-1 (WS-TIPP)            
051200         END-IF                                                           
051300         MOVE DAT-KVVIPER    TO WS-PERTAB-KVVIPER-1 (WS-TIPP)             
051400                                                                          
051500       ELSE                                                               
051600           STRING ' ERROR FROM DATE ROUTINE WDATKONV AA-1'                
051700           DELIMITED BY SIZE INTO ERROR-TEXT                              
051800           CALL FELLOG                                                    
051900       END-IF                                                             
052000                                                                          
052100       ADD 1                 TO WS-TIPP                                   
052200     END-PERFORM                                                          
052300                                                                          
052400     MOVE 1                  TO PER-IX                                    
052500                                                                          
052600     PERFORM UNTIL PER-IX > 11                                            
052700                                                                          
052800       COMPUTE WS-PERTAB-END-VV-1 (PER-IX) =                              
052900               WS-PERTAB-START-VV-1 (PER-IX + 1) - 1                      
053000                                                                          
053100       ADD 1                 TO PER-IX                                    
053200     END-PERFORM                                                          
053300                                                                          
053400     MOVE WS-TIAA            TO WS-TIAAVV-AA                              
053500     PERFORM AAAB-KOLLA-ANTAL-VECKOR-1                                    
053600     MOVE WS-END-VV-1        TO WS-PERTAB-END-VV-1 (12)                   
053700     .                                                                    
053800                                                                          
053900 AAAB-KOLLA-ANTAL-VECKOR-1 SECTION.                                       
054000     MOVE 'AAB-KOLLA-ANTAL-VECKOR-1'  TO CURRENT-SECTION                  
054100                                                                          
054200* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
054300                                                                          
054400     MOVE 53        TO WS-TIAAVV-VV                                       
054500     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
054600     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
054700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
054800                         DAT-O-TIDATUM DAT-KDSVAR                         
054900     IF DAT-KDSVAR-OK                                                     
055000       MOVE 53 TO WS-END-VV-1                                             
055100     ELSE                                                                 
055200       MOVE 52 TO WS-END-VV-1                                             
055300     END-IF                                                               
055400     .                                                                    
055500                                                                          
055600 AAB-INITIALIZE-PERIODTAB-AR-0 SECTION.                                   
055700     MOVE 'AAB-INIT-AR-0   ' TO CURRENT-SECTION                           
055800*--------------------------------------------------------------*          
055900* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR PEROD 1       *          
056000* INDX 1 MOTSVARAR PEROD 1 (DVS JANUARI)                       *          
056100* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
056200* PER.NR + START/ENDWEEK FÖR ATT KUNNA UTFÖRA SUMMERINGAR      *          
056300*--------------------------------------------------------------*          
056400                                                                          
056500     MOVE WS-TODAYS-TIAAAA (3:2)                                          
056600                             TO WS-TIAA                                   
056700     MOVE 1                  TO WS-TIPP                                   
056800                                                                          
056900     PERFORM UNTIL WS-TIPP > 12                                           
057000                                                                          
057100       MOVE 'AARP'           TO DAT-KDDATFORM                             
057200       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
057300                                                                          
057400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
057500                           DAT-O-TIDATUM DAT-KDSVAR                       
057600                                                                          
057700       IF DAT-KDSVAR-OK                                                   
057800*--- START-WEEK ÄR ALLTID WEEK 1 PÅ NYTT ÅR                               
057900         IF DAT-TIVV = +52 OR +53                                         
058000            MOVE 1           TO WS-PERTAB-START-VV-0 (WS-TIPP)            
058100         ELSE                                                             
058200            MOVE DAT-TIVV    TO WS-PERTAB-START-VV-0 (WS-TIPP)            
058300         END-IF                                                           
058400         MOVE DAT-KVVIPER    TO WS-PERTAB-KVVIPER-0 (WS-TIPP)             
058500                                                                          
058600       ELSE                                                               
058700           STRING ' ERROR FROM DATE ROUTINE WDATKONV AA-0'                
058800           DELIMITED BY SIZE INTO ERROR-TEXT                              
058900           CALL FELLOG                                                    
059000       END-IF                                                             
059100                                                                          
059200       ADD 1                 TO WS-TIPP                                   
059300     END-PERFORM                                                          
059400                                                                          
059500     MOVE 1                  TO PER-IX                                    
059600                                                                          
059700     PERFORM UNTIL PER-IX > 11                                            
059800                                                                          
059900       COMPUTE WS-PERTAB-END-VV-0 (PER-IX) =                              
060000               WS-PERTAB-START-VV-0 (PER-IX + 1) - 1                      
060100                                                                          
060200       ADD 1                 TO PER-IX                                    
060300     END-PERFORM                                                          
060400                                                                          
060500     MOVE WS-TIAA            TO WS-TIAAVV-AA                              
060600     PERFORM AABA-KOLLA-ANTAL-VECKOR-0                                    
060700     MOVE WS-END-VV-0        TO WS-PERTAB-END-VV-0 (12)                   
060800     .                                                                    
060900                                                                          
061000 AABA-KOLLA-ANTAL-VECKOR-0 SECTION.                                       
061100     MOVE 'AB-KOLLA-ANT-VEC' TO CURRENT-SECTION                           
061200                                                                          
061300* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
061400                                                                          
061500     MOVE 53        TO WS-TIAAVV-VV                                       
061600     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
061700     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
061800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
061900                         DAT-O-TIDATUM DAT-KDSVAR                         
062000     IF DAT-KDSVAR-OK                                                     
062100       MOVE 53 TO WS-END-VV-0                                             
062200     ELSE                                                                 
062300       MOVE 52 TO WS-END-VV-0                                             
062400     END-IF                                                               
062500     .                                                                    
062600                                                                          
062700 B-CHECK-KEYS SECTION.                                                    
062800     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
062900                                                                          
063000     MOVE ALL '+'             TO MSGI-WMSGINIT                            
063100     MOVE '001'               TO MSGI-KDCALL                              
063200     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
063300     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
063400     MOVE '2404'              TO MSGI-IDTRANS                             
063500     MOVE YES                 TO KEYS-SW                                  
063600                                                                          
063700     IF OWN-MID                                                           
063800       IF MID-IDARTNR-IN = ALL '+'                                        
063900         IF MID-IDARTNR-UT NUMERIC                                        
064000         AND MID-IDARTNR-UT > ZERO                                        
064100           MOVE MID-IDARTNR-UT                                            
064200                            TO MSGI-IDARTNR                               
064300         END-IF                                                           
064400       ELSE                                                               
064500         IF MID-IDARTNR-IN NUMERIC                                        
064600         AND MID-IDARTNR-IN > ZERO                                        
064700           MOVE MID-IDARTNR-IN                                            
064800                            TO MSGI-IDARTNR                               
064900         ELSE                                                             
065000           MOVE ERR-PART-NOT-NUMERIC                                      
065100                            TO MED-IDMFSFEL                               
065200           MOVE NOO         TO KEYS-SW                                    
065300         END-IF                                                           
065400       END-IF                                                             
065500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
065600     ELSE                                                                 
065700       IF MID-IDARTNR-IN NUMERIC                                          
065800       AND MID-IDARTNR-IN > ZERO                                          
065900         MOVE MID-IDARTNR-IN                                              
066000                            TO MSGI-IDARTNR                               
066100       END-IF                                                             
066200     END-IF                                                               
066300                                                                          
066400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
066500     MOVE MSGI-SPAR-AREA      TO SAVE-AREA                                
066600                                                                          
066700*    - LANGUAGE TO BE USED BY MEDKONV                                     
066800*    MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
066900     MOVE 'GB'                TO MED-IDSKYLT                              
067000                                 W-IDSKYLT                                
067100                                                                          
067200*    -- CHECK OF IDDC                                                     
067300     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                              
067400                                                                          
067500     IF MID-IDDC-IN NOT = ALL '+'                                         
067600       MOVE '7'               TO MFS-IDPFK                                
067700       MOVE SPACE             TO MFS-KDTRTYP                              
067800     END-IF                                                               
067900     MOVE MSGI-IDDC-KEY       TO W-IDDC                                   
068000                                                                          
068100     PERFORM IMS-GU-WDB601                                                
068200                                                                          
068300     IF DCS-NDC-CN                                                        
068400     OR (DCS-NDC-NA AND DCS-USA)                                          
068500       CONTINUE                                                           
068600     ELSE                                                                 
068700       MOVE NOO               TO KEYS-SW                                  
068800       MOVE ERR-INVALID-DC    TO MED-IDMFSINF                             
068900       CALL WMEDKONV USING MED-WMEDAREA                                   
069000       MOVE MED-TEMFSINF      TO MOD-TEMFSINF                             
069100     END-IF                                                               
069200                                                                          
069300*    -- CHECK OF IDARTNR                                                  
069400     MOVE MFS-ERASE-FIELD     TO MOD-IDARTNR-IN                           
069500                                                                          
069600     IF MID-IDARTNR-IN NOT = ALL '+'                                      
069700       MOVE '7'               TO MFS-IDPFK                                
069800       MOVE SPACE             TO MFS-KDTRTYP                              
069900     END-IF                                                               
070000                                                                          
070100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
070200     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
070300       MOVE MSGI-IDARTNR      TO W-IDARTNR                                
070400     ELSE                                                                 
070500       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
070600       MOVE NOO               TO KEYS-SW                                  
070700     END-IF                                                               
070800                                                                          
070900     IF GOOD-MID OR KEYS-OK                                               
071000       MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                              
071100       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                           
071200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
071300     ELSE                                                                 
071400       MOVE MFS-ERASE-FIELD   TO MOD-IDDC-UT                              
071500                                 MOD-IDARTNR-UT                           
071600                                 MOD-HYPHEN                               
071700                                 MOD-REKSIFFR                             
071800     END-IF                                                               
071900                                                                          
072000     IF KEYS-WRONG                                                        
072100       CALL WMEDKONV USING MED-WMEDAREA                                   
072200       MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                             
072300       PERFORM MFS-ERASE-FIELD-IN                                         
072400       PERFORM MFS-ERASE-FIELD-OUT                                        
072500     END-IF                                                               
072600                                                                          
072700     MOVE '002'               TO MSGI-KDCALL                              
072800     MOVE '2404'              TO SAVE-IDTRANS                             
072900     MOVE SAVE-AREA           TO MSGI-SPAR-AREA                           
073000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
073100     .                                                                    
073200                                                                          
073300 C-FIRST-PAGE SECTION.                                                    
073400     MOVE 'C-FIRST-PAGE    ' TO CURRENT-SECTION                           
073500                                                                          
073600     PERFORM MFS-ERASE-FIELD-IN                                           
073700     .                                                                    
073800                                                                          
073900 E-SAME-PAGE SECTION.                                                     
074000     MOVE 'E-SAME-PAGE     ' TO CURRENT-SECTION                           
074100                                                                          
074200     IF OWN-MID OR HELP-MID                                               
074300       IF MID-INPUT = ALL '+'                                             
074400         PERFORM MFS-ERASE-FIELD-IN                                       
074500       ELSE                                                               
074600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
074700         CALL WMEDKONV USING MED-WMEDAREA                                 
074800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
074900         PERFORM EA-MID-INDATA-TO-MOD                                     
075000       END-IF                                                             
075100     ELSE                                                                 
075200       PERFORM MFS-ERASE-FIELD-IN                                         
075300     END-IF                                                               
075400     .                                                                    
075500                                                                          
075600 EA-MID-INDATA-TO-MOD SECTION.                                            
075700     MOVE 'EA-MID-TO-MOD   ' TO CURRENT-SECTION                           
075800                                                                          
075900     IF MID-KVPB-DC-IN NOT = ALL '+'                                      
076000       INSPECT MID-KVPB-DC-IN REPLACING LEADING SPACES                    
076100                              BY ZERO                                     
076200       MOVE MID-KVPB-DC-IN  TO DEC-IDFRIDATA                              
076300       MOVE 6               TO DEC-KVHELTAL                               
076400       MOVE 1               TO DEC-KVDECIMAL                              
076500                                                                          
076600       CALL WDECEDIT USING DEC-WDECAREA                                   
076700                                                                          
076800       IF DEC-KDSVAR-OK                                                   
076900         MOVE DEC-IDEDITDATA                                              
077000                            TO MOD-KVPB-DC-IN                             
077100         MOVE MFS-ADD-READ-FIELD                                          
077200                            TO MOD-KVPB-DC-IN-ATTR                        
077300       ELSE                                                               
077400         MOVE MFS-ERASE-FIELD                                             
077500                            TO MOD-KVPB-DC-IN                             
077600       END-IF                                                             
077700     END-IF                                                               
077800                                                                          
077810     IF MID-KVPBREOI-DC-IN NOT = ALL '+'                                  
077820       INSPECT MID-KVPBREOI-DC-IN REPLACING LEADING SPACES                
077830                              BY ZERO                                     
077840       MOVE MID-KVPBREOI-DC-IN TO DEC-IDFRIDATA                           
077850       MOVE 6               TO DEC-KVHELTAL                               
077860       MOVE 1               TO DEC-KVDECIMAL                              
077870                                                                          
077880       CALL WDECEDIT USING DEC-WDECAREA                                   
077890                                                                          
077891       IF DEC-KDSVAR-OK                                                   
077892         MOVE DEC-IDEDITDATA                                              
077893                            TO MOD-KVPBREOI-DC-IN                         
077894         MOVE MFS-ADD-READ-FIELD                                          
077895                            TO MOD-KVPBREOI-DC-IN-ATTR                    
077896       ELSE                                                               
077897         MOVE MFS-ERASE-FIELD                                             
077898                            TO MOD-KVPBREOI-DC-IN                         
077899       END-IF                                                             
077900     END-IF                                                               
077901                                                                          
077910     IF MID-TIREFMPB-IN NOT = ALL '+'                                     
078000       MOVE MID-TIREFMPB-IN TO MOD-TIREFMPB-IN                            
078100       MOVE MFS-ADD-READ-FIELD                                            
078200                            TO MOD-TIREFMPB-IN-ATTR                       
078300     ELSE                                                                 
078400       MOVE MFS-ERASE-FIELD TO MOD-TIREFMPB-IN                            
078500     END-IF                                                               
078600                                                                          
078700     IF MID-DAREFESC-IN NOT = ALL '+'                                     
078800       MOVE MID-DAREFESC-IN TO MOD-DAREFESC-IN                            
078900       MOVE MFS-ADD-READ-FIELD                                            
079000                            TO MOD-DAREFESC-IN-ATTR                       
079100     ELSE                                                                 
079200       MOVE MFS-ERASE-FIELD TO MOD-DAREFESC-IN                            
079300     END-IF                                                               
079400                                                                          
079500     IF MID-KVPB-PLAN-IN NOT = ALL '+'                                    
079600       INSPECT MID-KVPB-PLAN-IN REPLACING LEADING SPACES                  
079700                                BY ZERO                                   
079800       MOVE MID-KVPB-PLAN-IN                                              
079900                            TO DEC-IDFRIDATA                              
080000       MOVE 6               TO DEC-KVHELTAL                               
080100       MOVE 1               TO DEC-KVDECIMAL                              
080200                                                                          
080300       CALL WDECEDIT USING DEC-WDECAREA                                   
080400                                                                          
080500       IF DEC-KDSVAR-OK                                                   
080600         MOVE DEC-IDEDITDATA                                              
080700                            TO MOD-KVPB-PLAN-IN                           
080800         MOVE MFS-ADD-READ-FIELD                                          
080900                            TO MOD-KVPB-PLAN-IN-ATTR                      
081000       ELSE                                                               
081100         MOVE MFS-ERASE-FIELD                                             
081200                            TO MOD-KVPB-PLAN-IN                           
081300       END-IF                                                             
081400     END-IF                                                               
081500                                                                          
081600     IF MID-DAPBPLAN-IN NOT = ALL '+'                                     
081700       MOVE MID-DAPBPLAN-IN TO MOD-DAPBPLAN-IN                            
081800       MOVE MFS-ADD-READ-FIELD                                            
081900                            TO MOD-DAPBPLAN-IN-ATTR                       
082000     ELSE                                                                 
082100       MOVE MFS-ERASE-FIELD TO MOD-DAPBPLAN-IN                            
082200     END-IF                                                               
082300     .                                                                    
082400                                                                          
082500 F-READ-SHOW-INFO SECTION.                                                
082600     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
082700                                                                          
082800     MOVE HYPHEN            TO MOD-HYPHEN                                 
082900     MOVE ART-REKSIFFR      TO MOD-REKSIFFR                               
083000                                                                          
083100     IF ART-KDERS-UTG > ZERO                                              
083200       IF ART-KDERS-UTG = +29 OR +52                                      
083300         MOVE INF-PART-EXPIRE                                             
083400                            TO MED-IDMFSINF                               
083500       ELSE                                                               
083600         MOVE INF-PART-SUPERSEDED                                         
083700                            TO MED-IDMFSINF                               
083800       END-IF                                                             
083900       CALL WMEDKONV USING MED-WMEDAREA                                   
084000       MOVE MED-TEMFSINF    TO MOD-TEMFSINF                               
084100     ELSE                                                                 
084200       IF ART-FLERS = YES                                                 
084300         MOVE INF-REPLACING-PART                                          
084400                            TO MED-IDMFSINF                               
084500         CALL WMEDKONV USING MED-WMEDAREA                                 
084600         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
084700       END-IF                                                             
084800                                                                          
084900       PERFORM FA-READ-SHOW-WDK711                                        
085000       IF NOT REFILL-PART                                                 
085100         PERFORM FB-READ-SHOW-WDK722                                      
085200         PERFORM FC-READ-SHOW-WDL711-L4                                   
085300         PERFORM FD-READ-SHOW-WDL711-L4-OTH                               
085400         PERFORM FE-READ-SHOW-WDD311                                      
085500       ELSE                                                               
085600         PERFORM MFS-CLOSE-FIELD-IN                                       
085700       END-IF                                                             
085800     END-IF                                                               
085900     .                                                                    
086000                                                                          
086100 FA-READ-SHOW-WDK711 SECTION.                                             
086200     MOVE 'FA-SHOW-WDK711  ' TO CURRENT-SECTION                           
086300*-----------------------------------------------------------              
086400*--- NDC KAN REFILLA CDC OCH DÅ SUMMERAR MAN TILL PB-PLAN                 
086500*--- FÖR MOD-KVPB-SEP-OTH - GLOBAL EXPORT 2018                            
086600*-----------------------------------------------------------              
086700                                                                          
086800     PERFORM IMS-GU-WDK711                                                
086900     IF SEGMENT-FOUND                                                     
087000       IF SLAG-IDDC-REF NOT = SPACE                                       
087100         MOVE YES                TO REFILL-PART-SW                        
087200         MOVE ERR-REFILL-PART    TO MED-IDMFSFEL                          
087300         CALL WMEDKONV USING MED-WMEDAREA                                 
087400         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
087500         PERFORM MFS-ERASE-FIELD-OUT                                      
087600         PERFORM MFS-ERASE-FIELD-IN                                       
087700       ELSE                                                               
087800         MOVE SLAG-KVPB-REF      TO MOD-KVPB-SEP-DC                       
087900         MOVE SLAG-KVPBREOI      TO MOD-KVPBREOI-DC                       
088000         MOVE SLAG-TIREFMPB      TO MOD-TIREFMPB                          
088100         MOVE SLAG-DAREFESC (3:6)                                         
088200                                 TO MOD-DAREFESC                          
088300       END-IF                                                             
088400       MOVE SLAG-IDLEVNR         TO IDLEVNR-WS                            
088500     ELSE                                                                 
088600         MOVE ZERO               TO MOD-KVPB-SEP-DC                       
088610                                    MOD-KVPBREOI-DC                       
088700                                    MOD-TIREFMPB                          
088800                                    MOD-DAREFESC                          
088900                                    MOD-KVPB-PLAN                         
089000                                    MOD-DAPBPLAN                          
089100                                    MOD-KVPB-TREND                        
089200     END-IF                                                               
089300                                                                          
089400     MOVE ZERO                   TO WS-KVPB-SEP-OTH                       
089500                                    MOD-KVPB-SEP-OTH                      
089600                                                                          
089700     PERFORM IMS-GU-WDK701                                                
089800     IF SEGMENT-FOUND                                                     
089900        MOVE W-IDDC              TO W-IDDCREF                             
090000        PERFORM IMS-GNP-WDK711                                            
090100        PERFORM UNTIL SEGMENT-MISSING                                     
090200           COMPUTE WS-KVPB-SEP-OTH = WS-KVPB-SEP-OTH                      
090300                    + SLAG-KVPB-REF                                       
090400                    + SLAG-KVPBREOI                                       
090500           PERFORM IMS-GNP-WDK711                                         
090600        END-PERFORM                                                       
090700        MOVE WS-KVPB-SEP-OTH     TO MOD-KVPB-SEP-OTH                      
090800     END-IF                                                               
090900                                                                          
091000     MOVE SPACE                    TO WS-IDDC-REF                         
091100     PERFORM IMS-GNP-WDK611                                               
091200     IF SEGMENT-FOUND                                                     
091300       MOVE CLAG-IDDC-REF          TO WS-IDDC-REF                         
091400       IF CLAG-IDDC-REF = W-IDDC                                          
091500         PERFORM IMS-GNP-WDK629                                           
091600         IF SEGMENT-FOUND                                                 
091700           IF CLAG-DAPBPLAN >= WS-TODAYS-AAAAMMDD                         
091800              ADD CLAG-KVPB-PLAN   TO WS-KVPB-SEP-OTH                     
091900           ELSE                                                           
092000              ADD CREF-KVPB-PLAN   TO WS-KVPB-SEP-OTH                     
092100           END-IF                                                         
092200                                                                          
092300           MOVE WS-KVPB-SEP-OTH    TO MOD-KVPB-SEP-OTH                    
092400         END-IF                                                           
092500       END-IF                                                             
092600     END-IF                                                               
092700     .                                                                    
092800                                                                          
092900 FB-READ-SHOW-WDK722 SECTION.                                             
093000     MOVE 'FB-SHOW-WDK722  ' TO CURRENT-SECTION                           
093100                                                                          
093200     PERFORM IMS-GU-WDK722                                                
093300     IF SEGMENT-FOUND                                                     
093400       MOVE XLAG-KVPB-PLAN       TO MOD-KVPB-PLAN                         
093500       MOVE XLAG-DAPBPLAN (3:6)  TO MOD-DAPBPLAN                          
093600       MOVE XLAG-KVPB-TREND      TO MOD-KVPB-TREND                        
093700     END-IF                                                               
093800     .                                                                    
093900                                                                          
094000 FC-READ-SHOW-WDL711-L4 SECTION.                                          
094100     MOVE 'FC-SHOW-WDL711-L4  ' TO CURRENT-SECTION                        
094200                                                                          
094300     MOVE +1                 TO MOD-IX                                    
094400     MOVE ZERO               TO WS-KVOI-DC-SUM                            
094500                                WS-KVOT-OH-SUM                            
094600                                WS-KVOI-INNEV-DC-SUM                      
094700                                WS-KVOT-INNEV-OH-SUM                      
094800                                WS-YEAR-DC-SUM                            
094900                                WS-YEAR-OH-SUM                            
095000                                WS-KVOI-LATE-12-DC                        
095100                                WS-KVOT-LATE-12-OH                        
095200                                WS-KVOI-LATE-6-DC                         
095300                                WS-KVOT-LATE-6-OH                         
095400                                WS-KVOI-AVER-12-DC                        
095500                                WS-KVOI-AVER-6-DC                         
095600                                                                          
095700     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA - 2                             
095800     MOVE WS-YEAR            TO MOD-YEAR (1)                              
095900                                                                          
096000     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA - 1                             
096100     MOVE WS-YEAR            TO MOD-YEAR (2)                              
096200                                                                          
096300     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA                                 
096400     MOVE WS-YEAR            TO MOD-YEAR (3)                              
096500                                                                          
096600     MOVE W-IDDC             TO W-IDDC-L7                                 
096700     PERFORM IMS-GU-WDL711                                                
096800     IF SEGMENT-FOUND                                                     
096900       PERFORM IMS-GU-WDL411                                              
097000       IF SEGMENT-MISSING                                                 
097100******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
097200******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
097300         INITIALIZE OIHD-WDL411                                           
097400       END-IF                                                             
097500************* LAST YEAR **************                                    
097600       MOVE TODAYS-PERIOD    TO PER-IX                                    
097700       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
097800                             TO WEEK-IX                                   
097900                                                                          
098000       PERFORM UNTIL WEEK-IX > +53                                        
098100                  OR WEEK-IX > WS-END-VV-1                                
098200         MOVE ZERO           TO WS-KVOI-DC                                
098300                                WS-KVOT-OH                                
098400*                               WS-KVOI-OTH                               
098500                                                                          
098600         PERFORM UNTIL WEEK-IX >                                          
098700                       WS-PERTAB-END-VV-1 (PER-IX)                        
098800                                                                          
098900           ADD DC-KVOI-RULL (WEEK-IX)                                     
099000                             TO WS-KVOI-DC                                
099100           ADD DC-KVOT-RULL (WEEK-IX)                                     
099200                             TO WS-KVOT-OH                                
099300           ADD +1            TO WEEK-IX                                   
099400         END-PERFORM                                                      
099500                                                                          
099600         MOVE WS-PERTAB-KVVIPER-1 (PER-IX)                                
099700                             TO MOD-KVVIPER (MOD-IX)                      
099800         MOVE WS-KVOI-DC     TO MOD-KVOI-DC (MOD-IX)                      
099900         MOVE WS-KVOT-OH     TO MOD-KVOT-OH (MOD-IX)                      
100000                                                                          
100100         ADD WS-KVOI-DC      TO WS-KVOI-LATE-12-DC                        
100200         ADD WS-KVOT-OH      TO WS-KVOT-LATE-12-OH                        
100300                                                                          
100400         IF MOD-IX > +6                                                   
100500           ADD WS-KVOI-DC    TO WS-KVOI-LATE-6-DC                         
100600           ADD WS-KVOT-OH    TO WS-KVOT-LATE-6-OH                         
100700         END-IF                                                           
100800                                                                          
100900         ADD +1              TO PER-IX                                    
101000                                MOD-IX                                    
101100       END-PERFORM                                                        
101200                                                                          
101300*********** PRESENT YEAR *************                                    
101400       MOVE +1               TO WEEK-IX                                   
101500                                PER-IX                                    
101600                                                                          
101700       IF TODAYS-PERIOD > 1                                               
101800         PERFORM UNTIL WEEK-IX >                                          
101900                            WS-PERTAB-END-VV-0 (TODAYS-PERIOD - 1)        
102000           MOVE ZERO         TO WS-KVOI-DC                                
102100                                WS-KVOT-OH                                
102200                                                                          
102300           PERFORM UNTIL WEEK-IX >                                        
102400                         WS-PERTAB-END-VV-0 (PER-IX)                      
102500                                                                          
102600             ADD DC-KVOI-RULL (WEEK-IX)                                   
102700                             TO WS-KVOI-DC                                
102800                                WS-KVOI-DC-SUM                            
102900             ADD DC-KVOT-RULL (WEEK-IX)                                   
103000                             TO WS-KVOT-OH                                
103100                                WS-KVOT-OH-SUM                            
103200             ADD +1          TO WEEK-IX                                   
103300           END-PERFORM                                                    
103400                                                                          
103500           MOVE WS-PERTAB-KVVIPER-0 (PER-IX)                              
103600                             TO MOD-KVVIPER (MOD-IX)                      
103700           MOVE WS-KVOI-DC   TO MOD-KVOI-DC (MOD-IX)                      
103800           MOVE WS-KVOT-OH   TO MOD-KVOT-OH (MOD-IX)                      
103900                                                                          
104000           ADD WS-KVOI-DC    TO WS-KVOI-LATE-12-DC                        
104100           ADD WS-KVOT-OH    TO WS-KVOT-LATE-12-OH                        
104200                                                                          
104300           IF MOD-IX > +6                                                 
104400             ADD WS-KVOI-DC  TO WS-KVOI-LATE-6-DC                         
104500             ADD WS-KVOT-OH  TO WS-KVOT-LATE-6-OH                         
104600           END-IF                                                         
104700                                                                          
104800           ADD +1            TO PER-IX                                    
104900                                MOD-IX                                    
105000         END-PERFORM                                                      
105100       END-IF                                                             
105200                                                                          
105300       MOVE WS-KVOI-LATE-12-DC                                            
105400                             TO MOD-LATE-12-DC                            
105500       MOVE WS-KVOT-LATE-12-OH                                            
105600                             TO MOD-LATE-12-OH                            
105700       MOVE WS-KVOI-LATE-6-DC                                             
105800                             TO MOD-LATE-6-DC                             
105900       MOVE WS-KVOT-LATE-6-OH                                             
106000                             TO MOD-LATE-6-OH                             
106100                                                                          
106200       COMPUTE WS-KVOI-AVER-12-DC ROUNDED =                               
106300                             (WS-KVOI-LATE-12-DC / 12)                    
106400       COMPUTE WS-KVOI-AVER-6-DC ROUNDED =                                
106500                             (WS-KVOI-LATE-6-DC / 6)                      
106600                                                                          
106700       MOVE WS-KVOI-AVER-12-DC                                            
106800                             TO MOD-AVER-12-DC                            
106900                                                                          
107000       MOVE WS-KVOI-AVER-6-DC                                             
107100                             TO MOD-AVER-6-DC                             
107200                                                                          
107300       MOVE +1               TO IX1                                       
107400       PERFORM UNTIL IX1 > 5                                              
107500         ADD DC-KVOI-INNEV (IX1)                                          
107600                             TO WS-KVOI-INNEV-DC-SUM                      
107700                                WS-KVOI-DC-SUM                            
107710         ADD DC-KVOI-PP-INNEV (IX1)                                       
107720                             TO WS-KVOI-INNEV-DC-SUM                      
107730                                WS-KVOI-DC-SUM                            
107800         ADD DC-KVOT-INNEV (IX1)                                          
107900                             TO WS-KVOT-INNEV-OH-SUM                      
108000                                WS-KVOT-OH-SUM                            
108010         ADD DC-KVOT-PP-INNEV (IX1)                                       
108020                             TO WS-KVOT-INNEV-OH-SUM                      
108030                                WS-KVOT-OH-SUM                            
108100         ADD +1              TO IX1                                       
108200       END-PERFORM                                                        
108300                                                                          
108400       MOVE WS-KVOI-INNEV-DC-SUM                                          
108500                             TO MOD-KVOI-DC-TOT                           
108600       MOVE WS-KVOT-INNEV-OH-SUM                                          
108700                             TO MOD-KVOT-OH-TOT                           
108800                                                                          
108900************ FIRST YEAR ************                                      
109000       MOVE +1               TO PER-IX                                    
109100       PERFORM UNTIL PER-IX > 12                                          
109200         ADD OIHD-KVOI (2, PER-IX)                                        
109300                             TO WS-YEAR-DC-SUM                            
109400         ADD +1              TO PER-IX                                    
109500       END-PERFORM                                                        
109600       MOVE OIHD-KVOT-FOREG (2)                                           
109700                             TO WS-YEAR-OH-SUM                            
109800       MOVE WS-YEAR-DC-SUM   TO MOD-YEAR-DC (1)                           
109900       MOVE WS-YEAR-OH-SUM   TO MOD-YEAR-OH (1)                           
110000                                                                          
110100************ SECOND YEAR ***********                                      
110200       MOVE ZERO             TO WS-YEAR-DC-SUM                            
110300                                WS-YEAR-OH-SUM                            
110400                                                                          
110500       MOVE +1               TO PER-IX                                    
110600       PERFORM UNTIL PER-IX > 12                                          
110700         ADD OIHD-KVOI (1, PER-IX)                                        
110800                             TO WS-YEAR-DC-SUM                            
110900         ADD +1              TO PER-IX                                    
111000       END-PERFORM                                                        
111100       MOVE OIHD-KVOT-FOREG (1)                                           
111200                             TO WS-YEAR-OH-SUM                            
111300       MOVE WS-YEAR-DC-SUM   TO MOD-YEAR-DC (2)                           
111400       MOVE WS-YEAR-OH-SUM   TO MOD-YEAR-OH (2)                           
111500                                                                          
111600************* LAST YEAR ************                                      
111700       MOVE WS-KVOI-DC-SUM   TO MOD-YEAR-DC (3)                           
111800       MOVE WS-KVOT-OH-SUM   TO MOD-YEAR-OH (3)                           
111900     END-IF                                                               
112000     .                                                                    
112100 FD-READ-SHOW-WDL711-L4-OTH SECTION.                                      
112200     MOVE 'FD-SHOW-WDL711-O' TO CURRENT-SECTION                           
112300                                                                          
112400     MOVE +1                 TO MOD-IX                                    
112500     PERFORM UNTIL MOD-IX > 12                                            
112600        MOVE ZERO TO WS-KVOI-OTH-TOT (MOD-IX)                             
112700        ADD 1 TO MOD-IX                                                   
112800     END-PERFORM                                                          
112900                                                                          
113000     MOVE +1                 TO MOD-IX                                    
113100     MOVE ZERO               TO WS-KVOI-OTH-SUM                           
113200                                WS-KVOI-INNEV-OTH-SUM                     
113300                                WS-YEAR-OTH-SUM                           
113400                                WS-KVOI-LATE-12-OTH                       
113500                                WS-KVOI-LATE-6-OTH                        
113600                                WS-KVOI-AVER-12-OTH                       
113700                                WS-KVOI-AVER-6-OTH                        
113800                                WS-YEAR-OTH-TOT(1)                        
113900                                WS-YEAR-OTH-TOT(2)                        
114000                                WS-YEAR-OTH-TOT(3)                        
114100                                                                          
114200     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA - 2                             
114300     MOVE WS-YEAR            TO MOD-YEAR (1)                              
114400                                                                          
114500     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA - 1                             
114600     MOVE WS-YEAR            TO MOD-YEAR (2)                              
114700                                                                          
114800     COMPUTE WS-TIAAAA = WS-TODAYS-TIAAAA                                 
114900     MOVE WS-YEAR            TO MOD-YEAR (3)                              
115000                                                                          
115100     PERFORM IMS-GU-WDK701                                                
115200     IF SEGMENT-FOUND                                                     
115300        MOVE W-IDDC TO W-IDDCREF                                          
115400        PERFORM IMS-GNP-WDK711                                            
115500        PERFORM UNTIL SEGMENT-MISSING                                     
115600                                                                          
115700           MOVE SLAG-IDDC           TO W-IDDC-L7                          
115800           PERFORM IMS-GU-WDL711                                          
115900           IF SEGMENT-FOUND                                               
116000              PERFORM IMS-GU-WDL411                                       
116100              IF SEGMENT-MISSING                                          
116200******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
116300******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
116400                INITIALIZE OIHD-WDL411                                    
116500              END-IF                                                      
116600************* LAST YEAR **************                                    
116700              MOVE TODAYS-PERIOD    TO PER-IX                             
116800              MOVE WS-PERTAB-START-VV-1 (PER-IX)                          
116900                                    TO WEEK-IX                            
117000                                                                          
117100              PERFORM UNTIL WEEK-IX > +53                                 
117200                         OR WEEK-IX > WS-END-VV-1                         
117300                 MOVE ZERO          TO WS-KVOI-OTH                        
117400                                       WS-KVOI-OTH-SUM                    
117500                                                                          
117600                 PERFORM UNTIL WEEK-IX >                                  
117700                               WS-PERTAB-END-VV-1 (PER-IX)                
117800                                                                          
117900                    ADD DC-KVOI-RULL (WEEK-IX)                            
118000                                    TO WS-KVOI-OTH                        
118100                    ADD DC-KVOI-REF-RULL (WEEK-IX)                        
118200                                    TO WS-KVOI-OTH                        
118300                    ADD +1          TO WEEK-IX                            
118400                 END-PERFORM                                              
118500                                                                          
118600                 ADD  WS-KVOI-OTH   TO WS-KVOI-OTH-TOT (MOD-IX)           
118700                                                                          
118800                 ADD WS-KVOI-OTH    TO WS-KVOI-LATE-12-OTH                
118900                                                                          
119000                 IF MOD-IX > +6                                           
119100                    ADD WS-KVOI-OTH TO WS-KVOI-LATE-6-OTH                 
119200                 END-IF                                                   
119300                                                                          
119400                 ADD +1             TO PER-IX                             
119500                                       MOD-IX                             
119600              END-PERFORM                                                 
119700                                                                          
119800*********** PRESENT YEAR *************                                    
119900              MOVE +1               TO WEEK-IX                            
120000                                       PER-IX                             
120100                                                                          
120200              IF TODAYS-PERIOD > 1                                        
120300                 PERFORM UNTIL WEEK-IX >                                  
120400                            WS-PERTAB-END-VV-0 (TODAYS-PERIOD - 1)        
120500                    MOVE ZERO       TO WS-KVOI-OTH                        
120600                                                                          
120700                    PERFORM UNTIL WEEK-IX >                               
120800                            WS-PERTAB-END-VV-0 (PER-IX)                   
120900                                                                          
121000                       ADD DC-KVOI-RULL (WEEK-IX)                         
121100                                     TO WS-KVOI-OTH                       
121200                                        WS-KVOI-OTH-SUM                   
121300                       ADD DC-KVOI-REF-RULL (WEEK-IX)                     
121400                                     TO WS-KVOI-OTH                       
121500                                        WS-KVOI-OTH-SUM                   
121600                       ADD +1        TO WEEK-IX                           
121700                    END-PERFORM                                           
121800                                                                          
121900                    ADD WS-KVOI-OTH  TO WS-KVOI-OTH-TOT (MOD-IX)          
122000                                                                          
122100                    ADD WS-KVOI-OTH  TO WS-KVOI-LATE-12-OTH               
122200                                                                          
122300                    IF MOD-IX > +6                                        
122400                       ADD WS-KVOI-OTH TO WS-KVOI-LATE-6-OTH              
122500                    END-IF                                                
122600                                                                          
122700                    ADD +1           TO PER-IX                            
122800                                        MOD-IX                            
122900                 END-PERFORM                                              
123000              END-IF                                                      
123100                                                                          
123200              MOVE +1              TO IX1                                 
123300              PERFORM UNTIL IX1 > 5                                       
123400                 ADD DC-KVOI-INNEV (IX1)                                  
123500                                   TO WS-KVOI-INNEV-OTH-SUM               
123600                                      WS-KVOI-OTH-SUM                     
123700                 ADD DC-KVOI-REF-INNEV (IX1)                              
123800                                   TO WS-KVOI-INNEV-OTH-SUM               
123900                                      WS-KVOI-OTH-SUM                     
124000                 ADD +1            TO IX1                                 
124100              END-PERFORM                                                 
124200                                                                          
124300************ FIRST YEAR ************                                      
124400              MOVE +1              TO PER-IX                              
124500              PERFORM UNTIL PER-IX > 12                                   
124600                 ADD OIHD-KVOI (2, PER-IX)                                
124700                                   TO WS-YEAR-OTH-SUM                     
124800                 ADD OIHD-KVOI-REFILL (2, PER-IX)                         
124900                                   TO WS-YEAR-OTH-SUM                     
125000                 ADD +1            TO PER-IX                              
125100              END-PERFORM                                                 
125200              ADD  WS-YEAR-OTH-SUM TO WS-YEAR-OTH-TOT (1)                 
125300                                                                          
125400************ SECOND YEAR ***********                                      
125500              MOVE ZERO            TO WS-YEAR-OTH-SUM                     
125600                                                                          
125700              MOVE +1              TO PER-IX                              
125800              PERFORM UNTIL PER-IX > 12                                   
125900                 ADD OIHD-KVOI (1, PER-IX)                                
126000                                   TO WS-YEAR-OTH-SUM                     
126100                 ADD OIHD-KVOI-REFILL (1, PER-IX)                         
126200                                   TO WS-YEAR-OTH-SUM                     
126300                 ADD +1            TO PER-IX                              
126400              END-PERFORM                                                 
126500              ADD  WS-YEAR-OTH-SUM TO WS-YEAR-OTH-TOT (2)                 
126600                                                                          
126700************* LAST YEAR ************                                      
126800                                                                          
126900              ADD  WS-KVOI-OTH-SUM TO WS-YEAR-OTH-TOT (3)                 
127000           END-IF                                                         
127100           MOVE +1                 TO MOD-IX                              
127200           PERFORM IMS-GNP-WDK711                                         
127300        END-PERFORM                                                       
127400     END-IF                                                               
127500                                                                          
127600     IF W-IDDC = WS-IDDC-REF                                              
127700        PERFORM FDA-READ-SHOW-WDL811-OTH                                  
127800     END-IF                                                               
127900                                                                          
128000     MOVE WS-KVOI-LATE-12-OTH      TO MOD-LATE-12-OTH                     
128100     MOVE WS-KVOI-LATE-6-OTH       TO MOD-LATE-6-OTH                      
128200                                                                          
128300     COMPUTE WS-KVOI-AVER-12-OTH ROUNDED =                                
128400                             (WS-KVOI-LATE-12-OTH / 12)                   
128500     COMPUTE WS-KVOI-AVER-6-OTH ROUNDED =                                 
128600                             (WS-KVOI-LATE-6-OTH / 6)                     
128700                                                                          
128800     MOVE WS-KVOI-AVER-12-OTH      TO MOD-AVER-12-OTH                     
128900     MOVE WS-KVOI-AVER-6-OTH       TO MOD-AVER-6-OTH                      
129000     MOVE WS-KVOI-INNEV-OTH-SUM    TO MOD-KVOI-OTH-TOT                    
129100                                                                          
129200     MOVE WS-YEAR-OTH-TOT (1)      TO MOD-YEAR-OTH (1)                    
129300     MOVE WS-YEAR-OTH-TOT (2)      TO MOD-YEAR-OTH (2)                    
129400     MOVE WS-YEAR-OTH-TOT (3)      TO MOD-YEAR-OTH (3)                    
129500                                                                          
129600     MOVE +1                 TO MOD-IX                                    
129700     PERFORM UNTIL MOD-IX > 12                                            
129800        MOVE WS-KVOI-OTH-TOT (MOD-IX)                                     
129900                                   TO MOD-KVOI-OTH (MOD-IX)               
130000        ADD 1 TO MOD-IX                                                   
130100     END-PERFORM                                                          
130200     .                                                                    
130300                                                                          
130400 FDA-READ-SHOW-WDL811-OTH SECTION.                                        
130500     MOVE 'FDA-READ-SHOW-WDL811-OTH ' TO CURRENT-SECTION                  
130600                                                                          
130700     MOVE TODAYS-PERIOD              TO START-PERIOD                      
130800     IF TODAYS-AA = 00                                                    
130900       MOVE 98                       TO START-AA                          
131000     ELSE                                                                 
131100       IF TODAYS-AA = 01                                                  
131200         MOVE 99                     TO START-AA                          
131300       ELSE                                                               
131400         COMPUTE START-AA = TODAYS-AA - 2                                 
131500       END-IF                                                             
131600     END-IF                                                               
131700                                                                          
131800     MOVE +1                         TO MOD-IX                            
131900                                                                          
132000     MOVE ZERO                       TO WS-KVOI-OTH                       
132100                                        WS-YEAR-OTH-SUM                   
132200                                                                          
132300************* FIRST YEAR **********                                       
132400     COMPUTE W-TIAAAA = WS-TODAYS-TIAAAA - 2                              
132500     MOVE W-TIAAAA (3:2)             TO MOD-YEAR (1)                      
132600                                                                          
132700     PERFORM IMS-GU-WDL811                                                
132800     IF SEGMENT-FOUND                                                     
132900       MOVE +1                       TO WEEK-IX                           
133000                                                                          
133100       PERFORM UNTIL WEEK-IX > +52                                        
133200         COMPUTE WS-KVOI-OTH = WS-KVOI-OTH                                
133300                             + AAR-KVOI-PROG (WEEK-IX)                    
133400                             + AAR-KVOI-DIV  (WEEK-IX)                    
133500                             + AAR-KVOI-SATS (WEEK-IX)                    
133510                             + AAR-KVOI-REFILL (WEEK-IX)                  
133600         ADD +1                      TO WEEK-IX                           
133700       END-PERFORM                                                        
133800                                                                          
133900       ADD WS-KVOI-OTH               TO WS-YEAR-OTH-TOT(01)               
134000                                                                          
134100     END-IF                                                               
134200                                                                          
134300************* SECOND YEAR **************                                  
134400     MOVE ZERO                       TO WS-KVOI-OTH                       
134500                                        WS-YEAR-OTH-SUM                   
134600                                                                          
134700     COMPUTE W-TIAAAA = WS-TODAYS-TIAAAA - 1                              
134800     MOVE W-TIAAAA (3:2)             TO MOD-YEAR (2)                      
134900     PERFORM IMS-GU-WDL811                                                
135000     IF SEGMENT-FOUND                                                     
135100                                                                          
135200       MOVE +1                       TO PER-IX                            
135300       MOVE WS-PERTAB-START-VV-1 (PER-IX)                                 
135400                                     TO WEEK-IX                           
135500                                                                          
135600       PERFORM UNTIL WEEK-IX NOT <                                        
135700                     WS-PERTAB-START-VV-1 (TODAYS-PERIOD)                 
135800         COMPUTE WS-KVOI-OTH = WS-KVOI-OTH                                
135900                             + AAR-KVOI-PROG (WEEK-IX)                    
136000                             + AAR-KVOI-DIV  (WEEK-IX)                    
136100                             + AAR-KVOI-SATS (WEEK-IX)                    
136110                             + AAR-KVOI-REFILL (WEEK-IX)                  
136200         ADD +1                      TO WEEK-IX                           
136300       END-PERFORM                                                        
136400                                                                          
136600       ADD WS-KVOI-OTH               TO WS-YEAR-OTH-SUM                   
136700                                                                          
136800       MOVE TODAYS-PERIOD            TO PER-IX                            
136900       MOVE WS-PERTAB-START-VV-1(PER-IX)                                  
137000                                     TO WEEK-IX                           
137100                                                                          
137200       PERFORM UNTIL WEEK-IX > +52                                        
137300                                                                          
137400         MOVE ZERO                   TO WS-KVOI-OTH                       
137500         PERFORM UNTIL WEEK-IX >                                          
137600                       WS-PERTAB-END-VV-1 (PER-IX)                        
137700                                                                          
137800           COMPUTE WS-KVOI-OTH = WS-KVOI-OTH                              
137900                               + AAR-KVOI-PROG(WEEK-IX)                   
138000                               + AAR-KVOI-DIV (WEEK-IX)                   
138100                               + AAR-KVOI-SATS(WEEK-IX)                   
138110                               + AAR-KVOI-REFILL (WEEK-IX)                
138200           ADD +1                    TO WEEK-IX                           
138300         END-PERFORM                                                      
138400                                                                          
138500         ADD WS-KVOI-OTH             TO WS-KVOI-OTH-TOT(MOD-IX)           
138600                                        WS-YEAR-OTH-SUM                   
138700                                                                          
138800         ADD +1                      TO MOD-IX                            
138900                                        PER-IX                            
139000       END-PERFORM                                                        
139100                                                                          
139200       ADD WS-YEAR-OTH-SUM           TO WS-YEAR-OTH-TOT(02)               
139300     ELSE                                                                 
139400       ADD +13                       TO MOD-IX                            
139500       SUBTRACT TODAYS-PERIOD      FROM MOD-IX                            
139600     END-IF                                                               
139700                                                                          
139800************* PRESENT YEAR *********                                      
139900     MOVE ZERO                       TO WS-KVOI-OTH                       
140000                                        WS-YEAR-OTH-SUM                   
140100                                                                          
140200     COMPUTE W-TIAAAA = WS-TODAYS-TIAAAA                                  
140300     MOVE W-TIAAAA (3:2)             TO MOD-YEAR (3)                      
140400     PERFORM IMS-GU-WDL811                                                
140500                                                                          
140600     IF SEGMENT-FOUND                                                     
140700       MOVE +1                       TO PER-IX                            
140800       MOVE WS-PERTAB-START-VV-0 (PER-IX)                                 
140900                                     TO WEEK-IX                           
141000                                                                          
141100       IF TODAYS-PERIOD > 1                                               
141200                                                                          
141300         PERFORM UNTIL WEEK-IX >                                          
141400                         WS-PERTAB-END-VV-0 (TODAYS-PERIOD - 1)           
141500                                                                          
141600           MOVE ZERO                 TO WS-KVOI-OTH                       
141700           PERFORM UNTIL WEEK-IX >                                        
141800                         WS-PERTAB-END-VV-0 (PER-IX)                      
141900                                                                          
142000             COMPUTE WS-KVOI-OTH = WS-KVOI-OTH                            
142100                                 + AAR-KVOI-PROG(WEEK-IX)                 
142200                                 + AAR-KVOI-DIV (WEEK-IX)                 
142300                                 + AAR-KVOI-SATS(WEEK-IX)                 
142310                                 + AAR-KVOI-REFILL (WEEK-IX)              
142400             ADD +1                  TO WEEK-IX                           
142500           END-PERFORM                                                    
142600                                                                          
142700           ADD WS-KVOI-OTH           TO WS-KVOI-OTH-TOT (MOD-IX)          
142800                                        WS-YEAR-OTH-SUM                   
142900                                                                          
143000           ADD +1                    TO MOD-IX                            
143100                                        PER-IX                            
143200                                                                          
143300         END-PERFORM                                                      
143400                                                                          
143500       END-IF                                                             
143600                                                                          
143700       MOVE ZERO                     TO WS-KVOI-OTH                       
143800       PERFORM UNTIL WEEK-IX >                                            
143900                     WS-PERTAB-END-VV-0 (PER-IX)                          
144000                                                                          
144100         COMPUTE WS-KVOI-OTH = WS-KVOI-OTH                                
144200                             + AAR-KVOI-PROG(WEEK-IX)                     
144300                             + AAR-KVOI-DIV (WEEK-IX)                     
144400                             + AAR-KVOI-SATS(WEEK-IX)                     
144410                             + AAR-KVOI-REFILL (WEEK-IX)                  
144500         ADD +1                      TO WEEK-IX                           
144600       END-PERFORM                                                        
144700                                                                          
144800       ADD WS-KVOI-OTH               TO WS-KVOI-INNEV-OTH-SUM             
144900                                        WS-YEAR-OTH-SUM                   
145000                                                                          
145100       ADD WS-YEAR-OTH-SUM           TO WS-YEAR-OTH-TOT(03)               
145200     END-IF                                                               
145300     .                                                                    
145400                                                                          
145500 FE-READ-SHOW-WDD311 SECTION.                                             
145600     MOVE 'FE-SHOW-WDD311  ' TO CURRENT-SECTION                           
145700                                                                          
145800     PERFORM IMS-GU-WDD311                                                
145900     IF SEGMENT-FOUND                                                     
146000        MOVE TEXT-BEART     TO MOD-BEART                                  
146100     ELSE                                                                 
146200        MOVE 'UNKNOWN'      TO MOD-BEART                                  
146300     END-IF                                                               
146400     .                                                                    
146500                                                                          
146600 G-CHECK-INPUT SECTION.                                                   
146700     MOVE 'G-CHECK-INPUT   ' TO CURRENT-SECTION                           
146800                                                                          
146900                                                                          
147000     MOVE YES  TO INDATA-SW                                               
147100     IF MID-INPUT = ALL '+'                                               
147200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
147300       CALL WMEDKONV USING MED-WMEDAREA                                   
147400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
147500       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
147600       MOVE NOO TO INDATA-SW                                              
147700     ELSE                                                                 
147800                                                                          
147900       PERFORM GA-CHECK-DC-FTG-USER                                       
148000                                                                          
148100       IF INDATA-OK                                                       
148200         IF MID-KVPB-DC-IN NOT = ALL '+'                                  
148300           INSPECT MID-KVPB-DC-IN REPLACING LEADING SPACES                
148400                                   BY ZERO                                
148500           MOVE MID-KVPB-DC-IN         TO DEC-IDFRIDATA                   
148600           MOVE 6                      TO DEC-KVHELTAL                    
148700           MOVE 1                      TO DEC-KVDECIMAL                   
148800                                                                          
148900           CALL WDECEDIT USING DEC-WDECAREA                               
149000                                                                          
149100           IF DEC-KDSVAR-OK                                               
149200              MOVE DEC-IDEDITDATA      TO WS-KVPB-DC-IN                   
149300                                          MOD-KVPB-DC-IN                  
149400              MOVE MFS-NUM-FIELD-OK    TO MOD-KVPB-DC-IN-ATTR             
149500           ELSE                                                           
149600              MOVE MFS-ERASE-FIELD     TO MOD-KVPB-DC-IN                  
149700              MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPB-DC-IN-ATTR             
149800              MOVE NOO                 TO INDATA-SW                       
149900           END-IF                                                         
150000         END-IF                                                           
150100                                                                          
150110         IF MID-KVPBREOI-DC-IN NOT = ALL '+'                              
150120           INSPECT MID-KVPBREOI-DC-IN REPLACING LEADING SPACES            
150130                                   BY ZERO                                
150140           MOVE MID-KVPBREOI-DC-IN     TO DEC-IDFRIDATA                   
150150           MOVE 6                      TO DEC-KVHELTAL                    
150160           MOVE 1                      TO DEC-KVDECIMAL                   
150170                                                                          
150180           CALL WDECEDIT USING DEC-WDECAREA                               
150190                                                                          
150191           IF DEC-KDSVAR-OK                                               
150192              MOVE DEC-IDEDITDATA      TO WS-KVPBREOI-DC-IN               
150193                                          MOD-KVPBREOI-DC-IN              
150194              MOVE MFS-NUM-FIELD-OK    TO MOD-KVPBREOI-DC-IN-ATTR         
150195           ELSE                                                           
150196              MOVE MFS-ERASE-FIELD     TO MOD-KVPBREOI-DC-IN              
150197              MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPBREOI-DC-IN-ATTR         
150198              MOVE NOO                 TO INDATA-SW                       
150199           END-IF                                                         
150200         END-IF                                                           
150201                                                                          
150210         IF MID-TIREFMPB-IN NOT = ALL '+'                                 
150300           MOVE MID-TIREFMPB-IN        TO MOD-TIREFMPB-IN                 
150400           IF MID-TIREFMPB-IN NOT NUMERIC                                 
150500              MOVE MFS-NUM-FIELD-WRONG TO MOD-TIREFMPB-IN-ATTR            
150600              MOVE NOO                 TO INDATA-SW                       
150700           ELSE                                                           
150800             IF MID-TIREFMPB-IN > ZERO                                    
150900                MOVE 'AAMMDD'            TO DAT-KDDATFORM                 
151000                MOVE MID-TIREFMPB-IN     TO DAT-I-TIDATUM                 
151100                                                                          
151200                CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM           
151300                                    DAT-O-TIDATUM DAT-KDSVAR              
151400                                                                          
151500                IF DAT-KDSVAR-OK                                          
151600                   MOVE MFS-NUM-FIELD-OK TO MOD-TIREFMPB-IN-ATTR          
151700                ELSE                                                      
151800                   MOVE MFS-NUM-FIELD-WRONG                               
151900                                         TO MOD-TIREFMPB-IN-ATTR          
152000                   MOVE NOO              TO INDATA-SW                     
152100                END-IF                                                    
152200             ELSE                                                         
152300                MOVE MFS-NUM-FIELD-OK TO MOD-TIREFMPB-IN-ATTR             
152400             END-IF                                                       
152500           END-IF                                                         
152600         END-IF                                                           
152700                                                                          
152800         IF MID-DAREFESC-IN NOT = ALL '+'                                 
152900           MOVE MID-DAREFESC-IN        TO MOD-DAREFESC-IN                 
153000           IF MID-DAREFESC-IN NOT NUMERIC                                 
153100              MOVE MFS-NUM-FIELD-WRONG TO MOD-DAREFESC-IN-ATTR            
153200              MOVE NOO                 TO INDATA-SW                       
153300           ELSE                                                           
153400             IF MID-DAREFESC-IN > ZERO                                    
153500               MOVE 'AAMMDD'            TO DAT-KDDATFORM                  
153600               MOVE MID-DAREFESC-IN     TO DAT-I-TIDATUM                  
153700                                                                          
153800               CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM            
153900                                   DAT-O-TIDATUM DAT-KDSVAR               
154000                                                                          
154100               IF DAT-KDSVAR-OK                                           
154200                  MOVE MFS-NUM-FIELD-OK TO MOD-DAREFESC-IN-ATTR           
154300               ELSE                                                       
154400                  MOVE MFS-NUM-FIELD-WRONG                                
154500                                        TO MOD-DAREFESC-IN-ATTR           
154600                  MOVE NOO              TO INDATA-SW                      
154700               END-IF                                                     
154800             ELSE                                                         
154900               MOVE MFS-NUM-FIELD-OK TO MOD-DAREFESC-IN-ATTR              
155000             END-IF                                                       
155100           END-IF                                                         
155200         END-IF                                                           
155300                                                                          
155400         IF MID-KVPB-PLAN-IN NOT = ALL '+'                                
155500         OR MID-DAPBPLAN-IN NOT = ALL '+'                                 
155600           IF MID-DAPBPLAN-IN = ZERO                                      
155700             CONTINUE                                                     
155800           ELSE                                                           
155900             IF MID-KVPB-PLAN-IN = ALL '+'                                
156000               MOVE MFS-ERASE-FIELD      TO MOD-KVPB-PLAN-IN              
156100               MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVPB-PLAN-IN-ATTR         
156200               MOVE NOO                  TO INDATA-SW                     
156300             ELSE                                                         
156400               INSPECT MID-KVPB-PLAN-IN REPLACING LEADING SPACES          
156500                                        BY ZERO                           
156600               MOVE MID-KVPB-PLAN-IN     TO DEC-IDFRIDATA                 
156700               MOVE 6                    TO DEC-KVHELTAL                  
156800               MOVE 1                    TO DEC-KVDECIMAL                 
156900                                                                          
157000               CALL WDECEDIT USING DEC-WDECAREA                           
157100                                                                          
157200               IF DEC-KDSVAR-OK                                           
157300                  MOVE DEC-IDEDITDATA    TO WS-KVPB-PLAN-IN               
157400                                            MOD-KVPB-PLAN-IN              
157500                  MOVE MFS-NUM-FIELD-OK  TO MOD-KVPB-PLAN-IN-ATTR         
157600               ELSE                                                       
157700                  MOVE MFS-ERASE-FIELD   TO MOD-KVPB-PLAN-IN              
157800                  MOVE MFS-NUM-FIELD-WRONG                                
157900                                         TO MOD-KVPB-PLAN-IN-ATTR         
158000                  MOVE NOO               TO INDATA-SW                     
158100               END-IF                                                     
158200             END-IF                                                       
158300           END-IF                                                         
158400                                                                          
158500           MOVE MID-DAPBPLAN-IN        TO MOD-DAPBPLAN-IN                 
158600           IF MID-DAPBPLAN-IN = ALL '+'                                   
158700           OR MID-DAPBPLAN-IN NOT NUMERIC                                 
158800             MOVE MFS-NUM-FIELD-WRONG  TO MOD-DAPBPLAN-IN-ATTR            
158900             MOVE NOO                  TO INDATA-SW                       
159000           ELSE                                                           
159100             IF MID-DAPBPLAN-IN = ZERO                                    
159200                MOVE MFS-NUM-FIELD-OK TO MOD-DAPBPLAN-IN-ATTR             
159300             ELSE                                                         
159400               IF MID-DAPBPLAN-IN NOT > WS-TODAYS-AAMMDD                  
159500                 MOVE MFS-NUM-FIELD-WRONG  TO MOD-DAPBPLAN-IN-ATTR        
159600                 MOVE NOO                  TO INDATA-SW                   
159700               ELSE                                                       
159800                 MOVE 'AAMMDD'            TO DAT-KDDATFORM                
159900                 MOVE MID-DAPBPLAN-IN     TO DAT-I-TIDATUM                
160000                                                                          
160100                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
160200                                     DAT-O-TIDATUM DAT-KDSVAR             
160300                                                                          
160400                 IF DAT-KDSVAR-OK                                         
160500                    MOVE MFS-NUM-FIELD-OK TO MOD-DAPBPLAN-IN-ATTR         
160600                 ELSE                                                     
160700                    MOVE MFS-NUM-FIELD-WRONG                              
160800                                          TO MOD-DAPBPLAN-IN-ATTR         
160900                    MOVE NOO              TO INDATA-SW                    
161000                 END-IF                                                   
161100               END-IF                                                     
161200             END-IF                                                       
161300           END-IF                                                         
161400         END-IF                                                           
161500         IF INDATA-WRONG                                                  
161600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
161700           CALL WMEDKONV USING MED-WMEDAREA                               
161800           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
161900           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
162000         END-IF                                                           
162100       ELSE                                                               
162200         MOVE ERR-NOT-AUTHORIZED   TO MED-IDMFSFEL                        
162300         CALL WMEDKONV USING MED-WMEDAREA                                 
162400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
162500         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
162600       END-IF                                                             
162700     END-IF                                                               
162800     .                                                                    
162900                                                                          
163000 GA-CHECK-DC-FTG-USER SECTION.                                            
163100                                                                          
163200     PERFORM IMS-GU-WDB601                                                
163300     IF SEGMENT-FOUND                                                     
163400        IF  DCS-NDC-CN                                                    
163500        OR (DCS-NDC-NA AND DCS-USA)                                       
163600           MOVE MSGI-IDFTG          TO WS-IDFTG                           
163700           IF (DCS-NDC-CN AND IDFTG-CN)                                   
163800           OR (DCS-NDC-NA AND IDFTG-US)                                   
163900           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
164000              CONTINUE                                                    
164100           ELSE                                                           
164200              MOVE NOO           TO INDATA-SW                             
164300           END-IF                                                         
164400        ELSE                                                              
164500           MOVE NOO              TO INDATA-SW                             
164600        END-IF                                                            
164700     ELSE                                                                 
164800        MOVE NOO                 TO INDATA-SW                             
164900     END-IF                                                               
165000                                                                          
165100     IF INDATA-WRONG                                                      
165200        MOVE ERR-NOT-AUTHORIZED  TO MED-IDMFSFEL                          
165300        CALL WMEDKONV         uSING MED-WMEDAREA                          
165400        MOVE MED-TEMFSFEL        TO MOD-TEMFSFEL                          
165500        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
165600     END-IF                                                               
165700     .                                                                    
165800     EJECT                                                                
165900 H-UPDATE SECTION.                                                        
166000     MOVE 'H-UPDATE        ' TO CURRENT-SECTION                           
166100                                                                          
166200     PERFORM IMS-GHU-WDK711                                               
166300     IF SEGMENT-FOUND                                                     
166400       IF MID-KVPB-DC-IN NOT = ALL '+'                                    
166500         MOVE WS-KVPB-DC-IN         TO SLAG-KVPB-REF                      
166510                                       SLAG-KVPB-HIST                     
166600                                       MOD-KVPB-SEP-DC                    
166700         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVPB-SEP-DC-ATTR               
166800                                                                          
166900         MOVE WS-TODAYS-AAMMDD      TO SLAG-TIREFMPB                      
167000                                       MOD-TIREFMPB                       
167100         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TIREFMPB-ATTR                  
167200       END-IF                                                             
167300                                                                          
167310       IF MID-KVPBREOI-DC-IN NOT = ALL '+'                                
167320         MOVE WS-KVPBREOI-DC-IN     TO SLAG-KVPBREOI                      
167321                                       SLAG-KVPBREOI-HIST                 
167330                                       MOD-KVPBREOI-DC                    
167340         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVPBREOI-DC-ATTR               
167350                                                                          
167360         MOVE WS-TODAYS-AAMMDD      TO SLAG-TIPBREOI                      
167390       END-IF                                                             
167391                                                                          
167400       IF MID-TIREFMPB-IN NOT = ALL '+'                                   
167500         MOVE MID-TIREFMPB-IN       TO SLAG-TIREFMPB                      
167600                                       MOD-TIREFMPB                       
167700         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-TIREFMPB-ATTR                  
167800       END-IF                                                             
167900                                                                          
168000       IF MID-DAREFESC-IN NOT = ALL '+'                                   
168100         MOVE MID-DAREFESC-IN       TO WS-TEMP-AAMMDD                     
168200                                       MOD-DAREFESC                       
168300         MOVE WS-TEMP-AAAAMMDD      TO SLAG-DAREFESC                      
168400         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-DAREFESC-ATTR                  
168500       END-IF                                                             
168600                                                                          
168700       PERFORM IMS-REPL-WDK711                                            
168800                                                                          
168900       IF MID-KVPB-PLAN-IN NOT = ALL '+'                                  
169000       AND MID-DAPBPLAN-IN NOT = ALL '+'                                  
169100       AND MID-DAPBPLAN-IN > ZERO                                         
169200         PERFORM IMS-GHU-WDK722                                           
169300         MOVE WS-KVPB-PLAN-IN       TO XLAG-KVPB-PLAN                     
169400                                       MOD-KVPB-PLAN                      
169500         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVPB-PLAN-ATTR                 
169600                                                                          
169700         MOVE MID-DAPBPLAN-IN       TO WS-TEMP-AAMMDD                     
169800                                       MOD-DAPBPLAN                       
169900         MOVE WS-TEMP-AAAAMMDD      TO XLAG-DAPBPLAN                      
170000         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-DAPBPLAN-ATTR                  
170100         IF SEGMENT-FOUND                                                 
170200           PERFORM IMS-REPL-WDK722                                        
170300         ELSE                                                             
170400           PERFORM S3-INSERT-WDK722                                       
170500         END-IF                                                           
170600       ELSE                                                               
170700         IF MID-DAPBPLAN-IN = ZERO                                        
170800           PERFORM IMS-GHU-WDK722                                         
170900           MOVE ZERO                  TO XLAG-KVPB-PLAN                   
171000                                         MOD-KVPB-PLAN                    
171100           MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVPB-PLAN-ATTR               
171200                                                                          
171300           MOVE MID-DAPBPLAN-IN       TO MOD-DAPBPLAN                     
171400                                                                          
171500           MOVE ZERO                  TO XLAG-DAPBPLAN                    
171600           MOVE MFS-ADD-HILIGHT-FIELD TO MOD-DAPBPLAN-ATTR                
171700           IF SEGMENT-FOUND                                               
171800             PERFORM IMS-REPL-WDK722                                      
171900           ELSE                                                           
172000             PERFORM S3-INSERT-WDK722                                     
172100           END-IF                                                         
172200         END-IF                                                           
172300       END-IF                                                             
172400                                                                          
172500       MOVE INF-UPDATED  TO MED-IDMFSINF                                  
172600       CALL WMEDKONV USING MED-WMEDAREA                                   
172700       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
172800       PERFORM MFS-ERASE-FIELD-IN                                         
172900     ELSE                                                                 
173000       MOVE ERR-UPDATE-NOT-ALLOWED                                        
173100                         TO MED-IDMFSFEL                                  
173200       CALL WMEDKONV USING MED-WMEDAREA                                   
173300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
173400       PERFORM MFS-ERASE-FIELD-IN                                         
173500     END-IF                                                               
173600     .                                                                    
173700                                                                          
173800 MFS-ERASE-FIELD-OUT SECTION.                                             
173900                                                                          
174000*    --- ALLA UTDATA-FÄLT                                                 
174100     MOVE MFS-ERASE-FIELD          TO MOD-WEEK-IN-PERIOD                  
174200                                      MOD-KVOI-DC-TOT                     
174300                                      MOD-KVOT-OH-TOT                     
174400                                      MOD-KVOI-OTH-TOT                    
174500                                      MOD-LATE-12-DC                      
174600                                      MOD-LATE-12-OH                      
174700                                      MOD-LATE-12-OTH                     
174800                                      MOD-LATE-6-DC                       
174900                                      MOD-LATE-6-OH                       
175000                                      MOD-LATE-6-OTH                      
175100                                      MOD-AVER-12-DC                      
175200                                      MOD-AVER-12-OTH                     
175300                                      MOD-AVER-6-DC                       
175400                                      MOD-AVER-6-OTH                      
175500                                      MOD-KVPB-SEP-DC                     
175510                                      MOD-KVPBREOI-DC                     
175600                                      MOD-KVPB-SEP-OTH                    
175700                                      MOD-TIREFMPB                        
175800                                      MOD-DAREFESC                        
175900                                      MOD-KVPB-PLAN                       
176000                                      MOD-DAPBPLAN                        
176100                                      MOD-KVPB-TREND                      
176200                                                                          
176300     MOVE +1                       TO IX1                                 
176400     PERFORM UNTIL IX1 > 12                                               
176500       MOVE MFS-ERASE-FIELD        TO MOD-TIAARP (IX1)                    
176600                                      MOD-KVVIPER (IX1)                   
176700                                      MOD-KVOI-DC (IX1)                   
176800                                      MOD-KVOT-OH (IX1)                   
176900                                      MOD-KVOI-OTH (IX1)                  
177000       IF IX1 < 4                                                         
177100         MOVE MFS-ERASE-FIELD      TO MOD-YEAR (IX1)                      
177200                                      MOD-YEAR-DC (IX1)                   
177300                                      MOD-YEAR-OH (IX1)                   
177400                                      MOD-YEAR-OTH (IX1)                  
177500       END-IF                                                             
177600       ADD +1                      TO IX1                                 
177700     END-PERFORM                                                          
177800     .                                                                    
177900                                                                          
178000 MFS-ERASE-FIELD-IN SECTION.                                              
178100                                                                          
178200*    --- ALLA INDATA-FÄLT                                                 
178300     MOVE MFS-ERASE-FIELD          TO MOD-KVPB-DC-IN                      
178310                                      MOD-KVPBREOI-DC-IN                  
178400                                      MOD-TIREFMPB-IN                     
178500                                      MOD-DAREFESC-IN                     
178600                                      MOD-KVPB-PLAN-IN                    
178700                                      MOD-DAPBPLAN-IN                     
178800     .                                                                    
178900                                                                          
179000 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
179100                                                                          
179200*    --- ALLA INDATA-FÄLT                                                 
179300     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KVPB-DC-IN                      
179310                                      MOD-KVPBREOI-DC-IN                  
179400                                      MOD-TIREFMPB-IN                     
179500                                      MOD-DAREFESC-IN                     
179600                                      MOD-KVPB-PLAN-IN                    
179700                                      MOD-DAPBPLAN-IN                     
179800     .                                                                    
179900                                                                          
180000 MFS-CLOSE-FIELD-IN       SECTION.                                        
180100                                                                          
180200*    --- ALLA INDATA-FÄLT                                                 
180300     MOVE MFS-CLOSE-FIELD          TO MOD-KVPB-DC-IN-ATTR                 
180310                                      MOD-KVPBREOI-DC-IN-ATTR             
180400                                      MOD-TIREFMPB-IN-ATTR                
180500                                      MOD-DAREFESC-IN-ATTR                
180600                                      MOD-KVPB-PLAN-IN-ATTR               
180700                                      MOD-DAPBPLAN-IN-ATTR                
180800     .                                                                    
180900                                                                          
181000 S1-SECURITY-CHECK SECTION.                                               
181100     MOVE 'S1-SECURITY-CHEC' TO CURRENT-SECTION                           
181200                                                                          
181300*    --- CHECK IF USER HAS ACCESS TO SEE PART-INFO                        
181400     PERFORM IMS-GU-WDK601                                                
181500     IF  SEGMENT-FOUND                                                    
181600       MOVE ART-IDLEVNR            TO WS-IDLEVNR-8                        
181700       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
181800       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
181900*        --- AUTHORIZED USER                                              
182000         SET PASSED-SECURITY-CHECK TO TRUE                                
182100       ELSE                                                               
182200*        --- USER NOT AUTHORIZED                                          
182300           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
182400           CALL WMEDKONV USING MED-WMEDAREA                               
182500           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
182600       END-IF                                                             
182700     ELSE                                                                 
182800*       -- PART NOT FOUND IN K6                                           
182900        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
183000        CALL WMEDKONV USING MED-WMEDAREA                                  
183100        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
183200        PERFORM MFS-ERASE-FIELD-OUT                                       
183300        PERFORM MFS-ERASE-FIELD-IN                                        
183400     END-IF                                                               
183500     .                                                                    
183600                                                                          
183700 S2-DATE SECTION.                                                         
183800     MOVE 'S2-DATE         ' TO CURRENT-SECTION                           
183900                                                                          
184000     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
184100                                                                          
184200     CALL WDATKONV  USING  DAT-KDDATFORM                                  
184300                           DAT-I-TIDATUM                                  
184400                           DAT-O-TIDATUM                                  
184500                           DAT-KDSVAR                                     
184600                                                                          
184700     MOVE DAT-TISEKEL        TO WS-TODAYS-CENTURY                         
184800     MOVE DAT-TIAA           TO WS-TODAYS-AA                              
184900     MOVE DAT-TIAA           TO TODAYS-AA                                 
185000     MOVE DAT-TIVV           TO TODAYS-WEEK                               
185100     MOVE DAT-TIRP           TO TODAYS-PERIOD                             
185200     MOVE 'AARP  '           TO DAT-KDDATFORM                             
185300     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
185400                                                                          
185500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
185600                         DAT-O-TIDATUM DAT-KDSVAR                         
185700                                                                          
185800     IF DAT-KDSVAR-OK                                                     
185900****   CALCULATE WEEKS IN CURRENT PERIOD                                  
186000                                                                          
186100       COMPUTE WS-WEEK-IN-PER = TODAYS-WEEK - DAT-TIVV + 1                
186200       MOVE ':'              TO WS-COLON                                  
186300       MOVE DAT-KVVIPER      TO WS-KVVIPER                                
186400                                                                          
186500     ELSE                                                                 
186600         STRING ' ERROR FROM DATE ROUTINE WDATKONV ' STATUS-WS            
186700         DELIMITED BY SIZE INTO ERROR-TEXT                                
186800         CALL FELLOG                                                      
186900     END-IF                                                               
187000     .                                                                    
187100                                                                          
187200 S3-INSERT-WDK722 SECTION.                                                
187300     MOVE 'S3-INSERT-WDK722' TO CURRENT-SECTION                           
187400                                                                          
187500     MOVE ALL '+'            TO WDK7-W005WDK7                             
187600     MOVE 'WDK722'           TO WDK7-IDSEGM                               
187700     MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                          
187800     MOVE W-IDDC             TO WDK7-IDDC-KFB                             
187900                                                                          
188000     MOVE WS-KVPB-PLAN-IN    TO WDK7-KVPB-PLAN                            
188100     MOVE WS-TEMP-AAAAMMDD   TO WDK7-DAPBPLAN                             
188200                                                                          
188300     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
188400                                       WDK7-PCB                           
188500     .                                                                    
188600                                                                          
188700* --- IMS SECTIONS ---                                                    
188800                                                                          
188900 IMS-GET-MSG SECTION.                                                     
189000                                                                          
189100     MOVE '  QC' TO GOOD-STATUSCODES                                      
189200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
189300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
189400     PERFORM IMS-STATUSCHECK                                              
189500     .                                                                    
189600                                                                          
189700 IMS-INSERT-MSG SECTION.                                                  
189800                                                                          
189900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
190000     MOVE SPACE TO GOOD-STATUSCODES                                       
190100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
190200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
190300     PERFORM IMS-STATUSCHECK                                              
190400     .                                                                    
190500                                                                          
190600 IMS-GU-WDB601 SECTION.                                                   
190700     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
190800                                                                          
190900     MOVE SPACE               TO ALL-SSA                                  
191000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
191100          DELIMITED BY SIZE INTO SSA1                                     
191200     MOVE '  GE'              TO GOOD-STATUSCODES                         
191300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
191400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
191500     PERFORM IMS-STATUSCHECK                                              
191600     .                                                                    
191700                                                                          
191800 IMS-GU-WDK601 SECTION.                                                   
191900     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
192000                                                                          
192100     MOVE SPACE               TO ALL-SSA                                  
192200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
192300          DELIMITED BY SIZE INTO SSA1                                     
192400     MOVE '  GE'              TO GOOD-STATUSCODES                         
192500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
192600     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
192700     PERFORM IMS-STATUSCHECK                                              
192800     .                                                                    
192900                                                                          
193000 IMS-GNP-WDK611 SECTION.                                                  
193100     MOVE 'IMS-GNP-WDK611   ' TO CURRENT-IMS-SECTION                      
193200                                                                          
193300     MOVE SPACE               TO ALL-SSA                                  
193400     MOVE 'WDK611  '          TO SSA1                                     
193500     MOVE '  GE'              TO GOOD-STATUSCODES                         
193600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
193700     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
193800     PERFORM IMS-STATUSCHECK                                              
193900     .                                                                    
194000                                                                          
194100 IMS-GNP-WDK629 SECTION.                                                  
194200     MOVE 'IMS-GNP-WDK629   ' TO CURRENT-IMS-SECTION                      
194300                                                                          
194400     MOVE SPACE               TO ALL-SSA                                  
194500     MOVE 'WDK629  '          TO SSA1                                     
194600     MOVE '  GE'              TO GOOD-STATUSCODES                         
194700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
194800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
194900     PERFORM IMS-STATUSCHECK                                              
195000     .                                                                    
195100                                                                          
195200 IMS-GU-WDK701 SECTION.                                                   
195300     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
195400                                                                          
195500     MOVE SPACE               TO ALL-SSA                                  
195600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
195700          DELIMITED BY SIZE INTO SSA1                                     
195800     MOVE '  GE'              TO GOOD-STATUSCODES                         
195900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
196000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
196100     PERFORM IMS-STATUSCHECK                                              
196200     .                                                                    
196300                                                                          
196400 IMS-GU-WDK711 SECTION.                                                   
196500     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
196600                                                                          
196700     MOVE SPACE               TO ALL-SSA                                  
196800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
196900          DELIMITED BY SIZE INTO SSA1                                     
197000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
197100          DELIMITED BY SIZE INTO SSA2                                     
197200     MOVE '  GE'              TO GOOD-STATUSCODES                         
197300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
197400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
197500     PERFORM IMS-STATUSCHECK                                              
197600     .                                                                    
197700                                                                          
197800 IMS-GHU-WDK711 SECTION.                                                  
197900     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
198000                                                                          
198100     MOVE SPACE               TO ALL-SSA                                  
198200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
198300          DELIMITED BY SIZE INTO SSA1                                     
198400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
198500          DELIMITED BY SIZE INTO SSA2                                     
198600     MOVE '  GE'              TO GOOD-STATUSCODES                         
198700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
198800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
198900     PERFORM IMS-STATUSCHECK                                              
199000     .                                                                    
199100                                                                          
199200 IMS-Gnp-WDK711 SECTION.                                                  
199300     MOVE 'IMS-Gnp-WDK711  ' TO CURRENT-IMS-SECTION                       
199400                                                                          
199500     MOVE SPACE               TO ALL-SSA                                  
199600     STRING 'WDK711  (IDDCREF  =' W-IDDCREF-X ')'                         
199700          DELIMITED BY SIZE INTO SSA1                                     
199800     MOVE '  GE'              TO GOOD-STATUSCODES                         
199900     CALL CBLTDLI USING Gnp WDK7-PCB DLI-IO-WDK711 SSA1                   
200000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
200100     PERFORM IMS-STATUSCHECK                                              
200200     .                                                                    
200300                                                                          
200400 IMS-REPL-WDK711 SECTION.                                                 
200500     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
200600                                                                          
200700     MOVE SPACE            TO ALL-SSA                                     
200800     MOVE '  '             TO GOOD-STATUSCODES                            
200900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
201000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUSCHECK                                              
201200     .                                                                    
201300                                                                          
201400 IMS-GU-WDK722 SECTION.                                                   
201500     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
201600                                                                          
201700     MOVE SPACE               TO ALL-SSA                                  
201800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
201900          DELIMITED BY SIZE INTO SSA1                                     
202000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
202100          DELIMITED BY SIZE INTO SSA2                                     
202200     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
202300          DELIMITED BY SIZE INTO SSA3                                     
202400     MOVE '  GE'              TO GOOD-STATUSCODES                         
202500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
202600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
202700     PERFORM IMS-STATUSCHECK                                              
202800     .                                                                    
202900                                                                          
203000 IMS-GHU-WDK722 SECTION.                                                  
203100     MOVE 'IMS-GHU-WDK722  ' TO CURRENT-IMS-SECTION                       
203200                                                                          
203300     MOVE SPACE               TO ALL-SSA                                  
203400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
203500          DELIMITED BY SIZE INTO SSA1                                     
203600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
203700          DELIMITED BY SIZE INTO SSA2                                     
203800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
203900          DELIMITED BY SIZE INTO SSA3                                     
204000     MOVE '  GE'              TO GOOD-STATUSCODES                         
204100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
204200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
204300     PERFORM IMS-STATUSCHECK                                              
204400     .                                                                    
204500                                                                          
204600 IMS-REPL-WDK722 SECTION.                                                 
204700     MOVE 'IMS-REPL-WDK722 ' TO CURRENT-IMS-SECTION                       
204800                                                                          
204900     MOVE SPACE            TO ALL-SSA                                     
205000     MOVE '  '             TO GOOD-STATUSCODES                            
205100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
205200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSCHECK                                              
205400     .                                                                    
205500                                                                          
205600 IMS-GU-WDL711 SECTION.                                                   
205700     MOVE 'IMS-GU-WDL711   ' TO CURRENT-IMS-SECTION                       
205800                                                                          
205900     MOVE SPACE               TO ALL-SSA                                  
206000     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
206100          DELIMITED BY SIZE INTO SSA1                                     
206200     STRING 'WDL711  (IDDC     =' W-IDDC-L7-X ')'                         
206300          DELIMITED BY SIZE INTO SSA2                                     
206400     MOVE '  GE'              TO GOOD-STATUSCODES                         
206500     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
206600     MOVE WDL7-STATUS-CODE    TO STATUS-WS                                
206700     PERFORM IMS-STATUSCHECK                                              
206800     .                                                                    
206900                                                                          
207000 IMS-GU-WDL411 SECTION.                                                   
207100     MOVE 'IMS-GU-WDL411   ' TO CURRENT-IMS-SECTION                       
207200                                                                          
207300     MOVE SPACE               TO ALL-SSA                                  
207400     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
207500          DELIMITED BY SIZE INTO SSA1                                     
207600     STRING 'WDL411  (IDDC     =' W-IDDC-L7-X ')'                         
207700          DELIMITED BY SIZE INTO SSA2                                     
207800     MOVE '  GE'              TO GOOD-STATUSCODES                         
207900     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-WDL411 SSA1 SSA2               
208000     MOVE WDL4-STATUS-CODE    TO STATUS-WS                                
208100     PERFORM IMS-STATUSCHECK                                              
208200     .                                                                    
208300                                                                          
208400 IMS-GU-WDL811  SECTION.                                                  
208500     MOVE 'IMS-GU-WDL811   ' TO CURRENT-IMS-SECTION                       
208600                                                                          
208700     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
208800          DELIMITED BY SIZE INTO SSA1                                     
208900     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
209000          DELIMITED BY SIZE INTO SSA2                                     
209100     MOVE '  GE'           TO GOOD-STATUSCODES                            
209200     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
209300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSCHECK                                              
209500     .                                                                    
209600                                                                          
209700 IMS-GU-WDD311 SECTION.                                                   
209800     MOVE 'IMS-GU-WDD311   ' TO CURRENT-IMS-SECTION                       
209900                                                                          
210000     MOVE SPACE               TO ALL-SSA                                  
210100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
210200          DELIMITED BY SIZE INTO SSA1                                     
210300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
210400          DELIMITED BY SIZE INTO SSA2                                     
210500     MOVE '  GE'              TO GOOD-STATUSCODES                         
210600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
210700     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
210800     PERFORM IMS-STATUSCHECK                                              
210900     .                                                                    
211000                                                                          
211100 IMS-STATUSCHECK SECTION.                                                 
211200                                                                          
211300     SET STATUS-IX TO 1                                                   
211400     SEARCH GOOD-STATUS                                                   
211500       AT END                                                             
211600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
211700         DELIMITED BY SIZE INTO ERROR-TEXT                                
211800         CALL FELLOG                                                      
211900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
212000         CONTINUE                                                         
212100     END-SEARCH                                                           
212200     .                                                                    
