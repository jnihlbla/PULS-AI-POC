000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3018100.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   14/10/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NES.REMANVIEWORDER                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        NES SCREEN TO DISPLAY ALL CORE PARTS ASSOCIATED TO A             
001100*        REMANUFACTURER WHICH ARE READY TO BE ORDERED.                    
001200*        ALSO START W371S1 TO ORDER THE PARTS.                            
001300*                                                                         
001400*        THE PROGRAM READS     WDD3                                       
001500*        THE PROGRAM READS     WDK7                                       
001600*        THE PROGRAM READS     WDA9                                       
001700*        THE PROGRAM UPDATES   WDR2                                       
001800*        THE PROGRAM READS     WDR4                                       
001900*        THE PROGRAM READS     WDD9                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W30181T                                             
002300*        REQUEST:     W30181I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    W30181O1                                            
002700*                                                                         
002800*    CHANGE LOG:                                                          
002900*                                                                         
003000*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003100*      ----------------------------------------------------------         
003200*      14/10/28 - REDDY RAHUL     - INITIAL VERSION.                      
003300*                                   E'TRACKER 10179385                    
003400*                                                                         
003500*      15/11/25 - REDDY RAHUL     - ADD CALLOFFS AND ARREARS.             
003600*                                   E'TRACKER 10251640                    
003700*                                                                         
003800*                                                                         
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800     SKIP3                                                                
004900 FILE SECTION.                                                            
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200 77  IDPGM                       PIC X(08)   VALUE 'W3018100'.            
005300 77  WS-TRANS                    PIC X(04)   VALUE '3181'.                
005400                                                                          
005500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005700 77  CURRENT-DB-SEC              PIC X(32) VALUE SPACE.                   
005800 77  KDRC-DISPLAY                PIC Z(5).                                
005900                                                                          
006000 77  YES                         PIC X       VALUE 'Y'.                   
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NOO                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  IX                          PIC 9(5)    VALUE ZERO.                  
006500                                                                          
006600 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006700     88  KEYS-OK                             VALUE 'Y'.                   
006800     88  KEYS-WRONG                          VALUE 'N'.                   
006900                                                                          
007000 77  ACTION-SW                   PIC X       VALUE SPACE.                 
007100     88  VIEW-ORDER                          VALUE 'V'.                   
007200     88  SEND-ORDER                          VALUE 'E'.                   
007300                                                                          
007400 01  ALL-SPACE.                                                           
007500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
007600                                                                          
007700 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
007800 01  WS-IDORDER                  PIC 9(7)    VALUE ZERO.                  
007900 01  WS-KVAVBART                 PIC 9(7)    VALUE ZERO.                  
008000 01  WS-TIREGTID                 PIC S9(7)   COMP-3 VALUE ZERO.           
008100                                                                          
008200 01  WS-PARTS-TO-ORDER           PIC 9(8)    VALUE ZERO.                  
008300 01  WS-KVLS-REM                 PIC S9(7)   VALUE ZERO.                  
008400 01  WS-KVADV-REM                PIC 9(7)    VALUE ZERO.                  
008500 01  WS-IDARTNR-BYT              PIC S9(9) COMP-3 VALUE ZERO.             
008600 01  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
008700 01  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
008800 01  WS-IDDISTR-NUM-4            PIC 9(4)    VALUE ZERO.                  
008900 01  WS-KVBEART-CORE-NUM         PIC 9(7)    VALUE ZERO.                  
009000                                                                          
009100 01  WS-IDARTNR                  PIC 9(9) VALUE ZERO.                     
009200 01  FILLER REDEFINES WS-IDARTNR.                                         
009300   03  FILLER                    PIC 9(5).                                
009400   03  WS-ARTSIFFRA              PIC 9(1).                                
009500     88  ART-0                   VALUE 6.                                 
009600     88  ART-1                   VALUE 4  7.                              
009700     88  ART-2                   VALUE 5  8.                              
009800     88  ART-3                   VALUE 9.                                 
009900   03  FILLER                    PIC 9(3).                                
010000                                                                          
010100 01  WS-KVAVROP                  PIC S9(7) COMP-3 VALUE ZERO.             
010200 01  WS-KVAVROP-GAM              PIC S9(7) COMP-3 VALUE ZERO.             
010300                                                                          
010400 01  WS-CURR-WEEK                PIC 9(6)    VALUE ZERO.                  
010500 01  FILLER REDEFINES WS-CURR-WEEK.                                       
010600     03  WS-CURR-WEEK-CC         PIC 9(2).                                
010700     03  WS-CURR-WEEK-AAVV       PIC 9(4).                                
010800                                                                          
010900 01  WS-END-WEEK                 PIC 9(6)    VALUE ZERO.                  
011000 01  FILLER REDEFINES WS-END-WEEK.                                        
011100     03  WS-END-WEEK-CC          PIC 9(2).                                
011200     03  WS-END-WEEK-AAVV        PIC 9(4).                                
011300                                                                          
011400     EJECT                                                                
011500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011600 01  GENERAL-SUBPROGRAMS.                                                 
011700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
012400     EJECT                                                                
012500*    --- PARAMETERS TO W005INIT                                           
012600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012700     SKIP3                                                                
012800*01 -COPY WMSGINIT                                                        
012900*    --- PARAMETERS TO WDATKONV                                           
013000                                                                          
013100*01  -COPY WDATAREA                                                       
013200     EJECT                                                                
013300*    --- PARAMETERS TO W009VADD                                           
013400                                                                          
013500 01  W009VADD-AAVV               PIC S9(5)   COMP-3.                      
013600 01  W009VADD-ANTAL              PIC S9(3)   COMP-3.                      
013700     EJECT                                                                
013800                                                                          
013900*    --- PARAMETERS TO ABEND                                              
014000                                                                          
014100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014400     SKIP3                                                                
014500 01  MESSAGE-CODES.                                                       
014600     03  ROUTINE-STARTED         PIC X(80)                                
014700               VALUE 'ROUTINE W371S1 STARTED'.                            
014800                                                                          
014900     EJECT                                                                
015000                                                                          
015100*01  -COPY WWDCKONS                                                       
015200     EJECT                                                                
015300                                                                          
015400 01  FILLER                      PIC X(8)  VALUE 'SOP     '.              
015500*01  -COPY WMSGSOP                                                        
015600     EJECT                                                                
015700                                                                          
015800 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
015900*01  FILLER -COPY WWDIS134    -RED TEST-IDDISTR.                          
016000     EJECT                                                                
016100                                                                          
016200 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
016300*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
016400*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
016500     EJECT                                                                
016600                                                                          
016700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016800     SKIP3                                                                
016900*01  -COPY WZ01SUB                                                        
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017200     SKIP3                                                                
017300 01  REQU-AREA.                                                           
017400*    03  -COPY WMIDPRE2                                                   
017500*    03  -COPY W30181I1                                                   
017600     EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017800     SKIP3                                                                
017900 01  RESP-AREA.                                                           
018000*    03  -COPY WMODPRE2                                                   
018100*    03  -COPY W30181O1                                                   
018200     EJECT                                                                
018300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
018400*                                                                         
018500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018600     SKIP3                                                                
018700 01  KEYS-FOR-DLI.                                                        
018800     03  W-IDARTNR-X.                                                     
018900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019000                                                                          
019100     03  W-IDARTNR-K6-X.                                                  
019200         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-IDSKYLT-X.                                                     
019500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019600                                                                          
019700     03  W-IDDC-X.                                                        
019800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019900                                                                          
020000     03  W-IDDISTR-X.                                                     
020100         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
020200                                                                          
020300     03  W-WDGXKEY-3151-X.                                                
020400         05  W-3151-IDHTYP       PIC X(4)    VALUE '3151'.                
020500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
020600     03  W-W3152KY-X.                                                     
020700         05  W-3152-IDUSER       PIC X(8)    VALUE SPACE.                 
020800         05  W-3152-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
020900     03  W-W3154KY-X.                                                     
021000         05  W-IDARTNRO          PIC S9(9)   VALUE ZERO COMP-3.           
021100     03  W-WDGXKEY-3161-X.                                                
021200         05  W-3161-IDHTYP       PIC X(4)    VALUE '3161'.                
021300         05  W-3161-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
021400         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
021500     03  W-WDD901KY-X.                                                    
021600         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
021700         05  W-IDDC-D9           PIC X(2)    VALUE '11'.                  
021800     03  W-IDLEVNR-X.                                                     
021900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
022000                                                                          
022100     SKIP2                                                                
022200*    --- STATUS-KOD FRÅN IMS                                              
022300 01  STATUS-WS                   PIC XX.                                  
022400     88  SEGMENT-FOUND                       VALUE '  '.                  
022500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
022600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022700     SKIP2                                                                
022800 01  GOOD-STATUSCODES.                                                    
022900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023000     SKIP3                                                                
023100 01  SSA1                        PIC X(64).                               
023200 01  SSA2                        PIC X(64).                               
023300 01  SSA3                        PIC X(64).                               
023400     EJECT                                                                
023500     EJECT                                                                
023600*    --- WORK-AREAS FOR DB2-SECTIONS                                      
023700*                                                                         
023800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024000     SKIP3                                                                
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
024300 01  DB2-WS.                                                              
024400     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
024500         88  ROW-FOUND                       VALUE  000.                  
024600         88  ROW-NOTFOUND                    VALUE  100.                  
024700         88  RESOURCE-UNAVAILABLE            VALUE  904.                  
024800     03  GOOD-SQLCODECODES.                                               
024900         05  GOOD-SQLCODE OCCURS 5                                        
025000           INDEXED BY SQLCODE-IX PIC  9(3).                               
025100*                                                                         
025200     EJECT                                                                
025300*    --- DB2 HOST-COPYTEXT                                                
025400 01  FILLER                      PIC X(16)   VALUE 'DB2-WS'.              
025500*01  -COPY BYART -PRE BYART-                                              
025600     EJECT                                                                
025700     SKIP3                                                                
025800*    --- DB2 DCL                                                          
025900 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
026000       EXEC SQL INCLUDE BYART END-EXEC.                                   
026100*    SKIP3                                                                
026200*    --- IMS FUNCTION CODES                                               
026300*01  -COPY W0003                                                          
026400     EJECT                                                                
026500                                                                          
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
026700 01  DLI-IO-WDD311.                                                       
026800*    03  -COPY WDD311                                                     
026900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
027000 01  DLI-IO-WDK711.                                                       
027100*    03  -COPY WDK711                                                     
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
027300 01  DLI-IO-WDA901.                                                       
027400*    03  -COPY WDA901                                                     
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
027600 01  DLI-IO-WDA911.                                                       
027700*    03  -COPY WDA911                                                     
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3151'.                    
027900 01  DLI-IO-WDGX3151.                                                     
028000*    03  -COPY WDGX01                                                     
028100     EJECT                                                                
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3152'.                    
028300 01  DLI-IO-WDGX3152.                                                     
028400*    03  -COPY WDGX3152                                                   
028500     EJECT                                                                
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3154'.                    
028700 01  DLI-IO-WDGX3154.                                                     
028800*    03  -COPY WDGX3154                                                   
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
029000 01  DLI-IO-WDGX3161.                                                     
029100*    03  -COPY WDGX3161                                                   
029200     EJECT                                                                
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
029400 01  DLI-IO-WDGX3162.                                                     
029500*    03  -COPY WDGX3162                                                   
029600     EJECT                                                                
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
029800 01  DLI-IO-WDK601.                                                       
029900*    03  -COPY WDK601                                                     
030000     EJECT                                                                
030100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
030200 01  DLI-IO-WDD902.                                                       
030300*    03  -COPY WDD902                                                     
030400     EJECT                                                                
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
030600 01  DLI-IO-WDD905.                                                       
030700*    03  -COPY WDD905                                                     
030800     EJECT                                                                
030900 LINKAGE SECTION.                                                         
031000*01  -COPY W0009   -PRE MSG-                                              
031100                                                                          
031200*01  -COPY W0009   -PRE ALT-                                              
031300                                                                          
031400*01  -COPY W0008  -PRE USEA-                                              
031500     05  FILLER                  PIC X.                                   
031600                                                                          
031700*01  -COPY W0008  -PRE WDD3-                                              
031800     05  FILLER                  PIC X.                                   
031900                                                                          
032000*01  -COPY W0008  -PRE WDK7-                                              
032100     05  FILLER                  PIC X.                                   
032200                                                                          
032300*01  -COPY W0008  -PRE WDA9-                                              
032400     05  FILLER                  PIC X.                                   
032500                                                                          
032600*01  -COPY W0008  -PRE 3151-                                              
032700     05  FILLER                  PIC X.                                   
032800                                                                          
032900*01  -COPY W0008  -PRE 3161-                                              
033000     05  FILLER                  PIC X.                                   
033100                                                                          
033200*01  -COPY W0008  -PRE WDD9-                                              
033300     05  FILLER                  PIC X.                                   
033400                                                                          
033500*01  -COPY W0008  -PRE WDK6-                                              
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDD3-PCB              
033900     WDK7-PCB WDA9-PCB 3151-PCB 3161-PCB WDD9-PCB WDK6-PCB .              
034000 MAIN SECTION.                                                            
034100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDD3-PCB              
034200     WDK7-PCB WDA9-PCB 3151-PCB 3161-PCB WDD9-PCB WDK6-PCB .              
034300                                                                          
034400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
034500     IF SUB-KDRC = 0                                                      
034600       PERFORM A-INIT                                                     
034700       PERFORM B-CHECK-KEYS                                               
034800       IF KEYS-OK                                                         
034900         IF SEND-ORDER                                                    
035000           PERFORM H-SEND-ORDER                                           
035100         END-IF                                                           
035200         IF VIEW-ORDER                                                    
035300           PERFORM F-READ-SHOW-INFO                                       
035400         END-IF                                                           
035500       END-IF                                                             
035600       PERFORM S02-RETURN-RESPONSE                                        
035700     END-IF                                                               
035800                                                                          
035900     PERFORM Z-FINIT                                                      
036000     MOVE ZERO                   TO RETURN-CODE                           
036100     GOBACK                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 A-INIT SECTION.                                                          
036500                                                                          
036600     MOVE ALL '+'                TO MOD-W30181O1                          
036700     INITIALIZE MOD-MODPREF                                               
036800     MOVE MID-KDPGMACT           TO MOD-KDPGMACT                          
036900     MOVE MID-KDDIASTATE         TO MOD-KDDIASTATE                        
037000     MOVE SPACE                  TO MOD-TEWEBERR                          
037100                                    MOD-TEWEBINF                          
037200     MOVE MID-IDDISTR            TO MOD-IDDISTR                           
037300     MOVE ZERO                   TO MOD-KVRADER                           
037400                                                                          
037500     IF MID-IDSPRAK = 'SV'                                                
037600       MOVE 'S'                  TO W-IDSKYLT                             
037700     ELSE                                                                 
037800       MOVE 'GB'                 TO W-IDSKYLT                             
037900     END-IF                                                               
038000                                                                          
038100     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
038200     CALL WDATKONV            USING DAT-KDDATFORM                         
038300                                    DAT-I-TIDATUM                         
038400                                    DAT-O-TIDATUM                         
038500                                    DAT-KDSVAR                            
038600                                                                          
038700     MOVE DAT-TIAAVV-GRP         TO WS-CURR-WEEK-AAVV                     
038800     MOVE DAT-TISEKEL            TO WS-CURR-WEEK-CC                       
038900                                    WS-END-WEEK-CC                        
039000                                                                          
039100     MOVE WS-CURR-WEEK-AAVV      TO W009VADD-AAVV                         
039200     MOVE +8                     TO W009VADD-ANTAL                        
039300     CALL W009VADD            USING W009VADD-AAVV                         
039400                                    W009VADD-ANTAL                        
039500     MOVE W009VADD-AAVV          TO WS-END-WEEK-AAVV                      
039600                                                                          
039700                                                                          
039800     .                                                                    
039900     EJECT                                                                
040000 B-CHECK-KEYS SECTION.                                                    
040100                                                                          
040200     MOVE ALL '+'             TO MSGI-WMSGINIT                            
040300     MOVE '001'               TO MSGI-KDCALL                              
040400     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
040500     MOVE MID-IDUSER          TO MSGI-IDUSER                              
040600     MOVE '3181'              TO MSGI-IDTRANS                             
040700     MOVE MID-IDDISTR         TO MSGI-IDDISTR                             
040800                                                                          
040900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041000                                                                          
041100     MOVE YES                    TO KEYS-SW                               
041200                                                                          
041300     MOVE MID-IDDISTR            TO WS-IDDISTR-NUM                        
041400                                                                          
041500     EVALUATE MID-KDPGMACT                                                
041600       WHEN 'V'                                                           
041700         SET VIEW-ORDER          TO TRUE                                  
041800       WHEN 'E'                                                           
041900         SET SEND-ORDER          TO TRUE                                  
042000       WHEN OTHER                                                         
042100         MOVE NOO                TO KEYS-SW                               
042200     END-EVALUATE                                                         
042300     .                                                                    
042400     EJECT                                                                
042500 F-READ-SHOW-INFO SECTION.                                                
042600                                                                          
042700     MOVE MID-IDUSER             TO W-3152-IDUSER                         
042800     MOVE WS-IDDISTR-NUM         TO W-3152-IDDISTR                        
042900                                    W-IDDISTR                             
043000                                                                          
043100     PERFORM IMS-GU-WDGX3152                                              
043200     IF SEGMENT-FOUND                                                     
043300       MOVE +1                   TO IX                                    
043400                                                                          
043500       PERFORM IMS-GNP-WDGX3154                                           
043600       PERFORM                                                            
043700         UNTIL SEGMENT-MISSING                                            
043800         MOVE 3154-IDARTNR-OBJ   TO W-IDARTNR                             
043900                                    WS-IDARTNR                            
044000                                    W-IDARTNRO                            
044100                                    WS-IDARTNR-NUM                        
044200                                    TEST-IDARTNR                          
044300                                                                          
044400         IF BYT01-BYTES                                                   
044500           IF BYT16-RADIO                                                 
044600             MOVE 3              TO WS-ARTSIFFRA                          
044700           ELSE                                                           
044800             IF ART-0                                                     
044900               MOVE 0            TO WS-ARTSIFFRA                          
045000             ELSE                                                         
045100               IF ART-1                                                   
045200                 MOVE 1          TO WS-ARTSIFFRA                          
045300               ELSE                                                       
045400                 IF ART-2                                                 
045500                   MOVE 2        TO WS-ARTSIFFRA                          
045600                 ELSE                                                     
045700                   IF ART-3                                               
045800                     MOVE 3      TO WS-ARTSIFFRA                          
045900                   END-IF                                                 
046000                 END-IF                                                   
046100               END-IF                                                     
046200             END-IF                                                       
046300           END-IF                                                         
046400         END-IF                                                           
046500                                                                          
046600         MOVE WS-IDARTNR         TO WS-IDARTNR-BYT                        
046700                                    W-IDARTNR-K6                          
046800                                    W-IDARTNR-D9                          
046900                                                                          
047000         PERFORM DB2-SELECT-BYART                                         
047100         IF ROW-FOUND                                                     
047200           MOVE BYART-KVBYTPKO   TO MOD-KVBYTPKO (IX)                     
047300         END-IF                                                           
047400         MOVE WS-IDARTNR-NUM     TO MOD-IDARTNR-OBJ (IX)                  
047500         MOVE 3154-KVBEART       TO MOD-KVBEART-CORE (IX)                 
047600                                                                          
047700         PERFORM IMS-GU-WDD3-BSEQ-BENA11                                  
047800         IF SEGMENT-FOUND                                                 
047900           MOVE TEXT-BEART       TO MOD-BEART (IX)                        
048000         END-IF                                                           
048100                                                                          
048200         MOVE MSGI-IDDC          TO W-IDDC                                
048300         PERFORM IMS-GU-WDK711                                            
048400         IF SEGMENT-FOUND                                                 
048500           MOVE SLAG-KVLS        TO MOD-KVLS-91 (IX)                      
048600         ELSE                                                             
048700           MOVE ZERO             TO MOD-KVLS-91 (IX)                      
048800         END-IF                                                           
048900                                                                          
049000         PERFORM IMS-GU-WDA901                                            
049100         IF SEGMENT-FOUND                                                 
049200           MOVE ZERO             TO WS-KVLS-REM                           
049300                                    WS-KVADV-REM                          
049400                                    WS-DAREGDAT                           
049500                                    WS-IDORDER                            
049600                                    WS-KVAVBART                           
049700                                    WS-TIREGTID                           
049800           PERFORM IMS-GNP-WDA911                                         
049900           IF SEGMENT-FOUND                                               
050000             MOVE UPD-IDDISTR    TO TEST-IDDISTR                          
050100             IF DIS134-BYTESREN-WEB                                       
050200               ADD UPD-KVLS-REM  TO WS-KVLS-REM                           
050300               MOVE UPD-IDDISTR  TO W-3161-IDDISTR                        
050400               PERFORM FA-READ-3161                                       
050500             END-IF                                                       
050600           END-IF                                                         
050700                                                                          
050800           MOVE WS-KVLS-REM      TO MOD-KVLS-REM (IX)                     
050900           MOVE WS-KVADV-REM     TO MOD-KVANTAL-PREADV (IX)               
051000           IF WS-IDORDER > ZERO                                           
051100             MOVE WS-DAREGDAT    TO MOD-DAORDDAT (IX)                     
051200             MOVE WS-IDORDER     TO MOD-IDORDER  (IX)                     
051300             MOVE WS-KVAVBART    TO MOD-KVBEART  (IX)                     
051400           ELSE                                                           
051500             MOVE ALL-SPACE      TO MOD-DAORDDAT (IX)                     
051600                                    MOD-IDORDER  (IX)                     
051700                                    MOD-KVBEART  (IX)                     
051800           END-IF                                                         
051900         END-IF                                                           
052000                                                                          
052100         PERFORM FB-GET-CALLOFFS-ARREARS                                  
052200                                                                          
052300         ADD +1                  TO IX                                    
052400                                                                          
052500         PERFORM IMS-GNP-WDGX3154                                         
052600       END-PERFORM                                                        
052700       COMPUTE IX = IX - 1                                                
052800       MOVE IX                   TO MOD-KVRADER                           
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 FA-READ-3161 SECTION.                                                    
053300                                                                          
053400     PERFORM IMS-GU-WDGX3161                                              
053500     IF SEGMENT-FOUND                                                     
053600       PERFORM                                                            
053700         UNTIL SEGMENT-MISSING                                            
053800         PERFORM IMS-GNP-WDGX3162                                         
053900         IF SEGMENT-FOUND                                                 
054000* GET ADVICED QTY                                                         
054100           IF 3162-IDARTNR = UPB-IDARTNR AND                              
054200              3162-IDUSER  = SPACE                                        
054300             ADD 3162-KVAVIS     TO WS-KVADV-REM                          
054400                                                                          
054500           END-IF                                                         
054600* GET THE LATEST ORDER DETAILS FOR THE GIVEN REMAN(IDDISTR)               
054700           IF 3162-IDARTNR = UPB-IDARTNR AND                              
054800              UPD-IDDISTR  = WS-IDDISTR-NUM                               
054900             IF WS-DAREGDAT = ZERO AND                                    
055000                3162-IDUSER > SPACE                                       
055100               MOVE 3162-DAREGDAT                                         
055200                                 TO WS-DAREGDAT                           
055300               MOVE 3162-IDORDER TO WS-IDORDER                            
055400               MOVE 3162-KVAVBART                                         
055500                                 TO WS-KVAVBART                           
055600               MOVE 3162-TIREGTID                                         
055700                                 TO WS-TIREGTID                           
055800             ELSE                                                         
055900               IF 3162-IDUSER > SPACE AND                                 
056000                  (3162-DAREGDAT > WS-DAREGDAT OR                         
056100                  (3162-DAREGDAT = WS-DAREGDAT AND                        
056200                   3162-TIREGTID > WS-TIREGTID))                          
056300                 MOVE 3162-DAREGDAT                                       
056400                                 TO WS-DAREGDAT                           
056500                 MOVE 3162-IDORDER                                        
056600                                 TO WS-IDORDER                            
056700                 MOVE 3162-KVAVBART                                       
056800                                 TO WS-KVAVBART                           
056900                 MOVE 3162-TIREGTID                                       
057000                                 TO WS-TIREGTID                           
057100               END-IF                                                     
057200             END-IF                                                       
057300           END-IF                                                         
057400         END-IF                                                           
057500       END-PERFORM                                                        
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 FB-GET-CALLOFFS-ARREARS SECTION.                                         
058000                                                                          
058100     MOVE ZERO                   TO WS-KVAVROP                            
058200                                    WS-KVAVROP-GAM                        
058300                                                                          
058400     PERFORM IMS-GU-WDK601                                                
058500     IF SEGMENT-FOUND                                                     
058600       MOVE ART-IDLEVNR          TO W-IDLEVNR                             
058700       PERFORM IMS-GU-WDD902                                              
058800       IF SEGMENT-FOUND                                                   
058900         PERFORM IMS-GNP-WDD905                                           
059000         PERFORM                                                          
059100           UNTIL SEGMENT-MISSING                                          
059200           IF KDAVROP = 2                                                 
059300             IF DAAVROP-AVS < WS-CURR-WEEK                                
059400*               ARREARS, LESS THAN CURRENT WEEK                           
059500               COMPUTE WS-KVAVROP-GAM = WS-KVAVROP-GAM + KVAVROP          
059600             ELSE                                                         
059700               IF DAAVROP-AVS >  WS-CURR-WEEK AND                         
059800                  DAAVROP-AVS <= WS-END-WEEK                              
059900*                 CALL-OFFS, NEXT WEEK UNTIL 8 WEEKS                      
060000                 COMPUTE WS-KVAVROP = WS-KVAVROP + KVAVROP                
060100               END-IF                                                     
060200             END-IF                                                       
060300           END-IF                                                         
060400           PERFORM IMS-GNP-WDD905                                         
060500         END-PERFORM                                                      
060600       END-IF                                                             
060700     END-IF                                                               
060800     MOVE WS-KVAVROP             TO MOD-KVAVROP     (IX)                  
060900     MOVE WS-KVAVROP-GAM         TO MOD-KVAVROP-GAM (IX)                  
061000     .                                                                    
061100     EJECT                                                                
061200 H-SEND-ORDER SECTION.                                                    
061300                                                                          
061400     MOVE MID-IDUSER             TO W-3152-IDUSER                         
061500     MOVE WS-IDDISTR-NUM         TO W-3152-IDDISTR                        
061600                                    WS-IDDISTR-NUM-4                      
061700                                                                          
061800     MOVE '3181'                 TO MSGSOP-IDTRANS                        
061900     MOVE '1'                    TO MSGSOP-KDMFSFOR                       
062000     MOVE 'O'                    TO MSGSOP-KDSOPFUNK                      
062100     MOVE 'W371S1'               TO MSGSOP-IDPROCESS                      
062200     STRING 'IDUSER-OREG(' MID-IDUSER                                     
062300            ') IDDISTR(' WS-IDDISTR-NUM-4                                 
062400            ')'                                                           
062500             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
062600     PERFORM IMS-ISRT-ALT-MSG                                             
062700                                                                          
062800     PERFORM IMS-GHU-WDGX3152                                             
062900     IF SEGMENT-FOUND                                                     
063000       MOVE JA                   TO 3152-FLKLAR                           
063100       PERFORM IMS-REPL-WDGX3152                                          
063200     END-IF                                                               
063300     MOVE ROUTINE-STARTED        TO MOD-TEWEBINF                          
063400                                                                          
063500     .                                                                    
063600     EJECT                                                                
063700 Z-FINIT SECTION.                                                         
063800     CONTINUE                                                             
063900     .                                                                    
064000     EJECT                                                                
064100*    --- DISPATCHER SECTIONS                                              
064200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
064300                                                                          
064400     MOVE 'GETARG'               TO SUB-KDFUNC                            
064500     MOVE 'CARPARTS.NES.REMANVIEWORDER'      TO SUB-ADDISPABS             
064600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
064700                                                                          
064800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
064900                                                                          
065000     IF SUB-KDRC > 0                                                      
065100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
065200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
065300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
065400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
065500     END-IF                                                               
065600     .                                                                    
065700     SKIP3                                                                
065800 S02-RETURN-RESPONSE SECTION.                                             
065900                                                                          
066000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
066100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
066200                                                                          
066300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
066400                                                                          
066500     IF SUB-KDRC > 0                                                      
066600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
066700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
066800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
066900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300     EJECT                                                                
067400 IMS-ISRT-ALT-MSG SECTION.                                                
067500                                                                          
067600     MOVE SPACE                  TO GOOD-STATUSCODES                      
067700     CALL CBLTDLI USING ISRT ALT-PCB MSGSOP-WMSGSOP                       
067800     MOVE ALT-STATUS-CODE        TO STATUS-WS                             
067900     PERFORM IMS-STATUSCHECK                                              
068000     .                                                                    
068100     EJECT                                                                
068200 IMS-GU-WDD3-BSEQ-BENA11   SECTION.                                       
068300                                                                          
068400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
068500             DELIMITED BY SIZE INTO SSA1                                  
068600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
068700             DELIMITED BY SIZE INTO SSA2                                  
068800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
068900     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
069000     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
069100     PERFORM IMS-STATUSCHECK                                              
069200     .                                                                    
069300     EJECT                                                                
069400 IMS-GU-WDK711 SECTION.                                                   
069500                                                                          
069600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
069700             DELIMITED BY SIZE INTO SSA1                                  
069800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
069900             DELIMITED BY SIZE INTO SSA2                                  
070000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
070100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
070200     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
070300     PERFORM IMS-STATUSCHECK                                              
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-GU-WDK601 SECTION.                                                   
070700                                                                          
070800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
070900             DELIMITED BY SIZE INTO SSA1                                  
071000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
071100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
071200     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
071300     PERFORM IMS-STATUSCHECK                                              
071400     .                                                                    
071500     EJECT                                                                
071600 IMS-GU-WDD902 SECTION.                                                   
071700                                                                          
071800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
071900             DELIMITED BY SIZE INTO SSA1                                  
072000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
072100             DELIMITED BY SIZE INTO SSA2                                  
072200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
072300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
072400     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
072500     PERFORM IMS-STATUSCHECK                                              
072600     .                                                                    
072700     EJECT                                                                
072800 IMS-GNP-WDD905 SECTION.                                                  
072900                                                                          
073000     MOVE 'WDD905  '             TO SSA1                                  
073100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
073200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
073300     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
073400     PERFORM IMS-STATUSCHECK                                              
073500     .                                                                    
073600     EJECT                                                                
073700 IMS-GU-WDA901 SECTION.                                                   
073800                                                                          
073900     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-X ')'                         
074000             DELIMITED BY SIZE INTO SSA1                                  
074100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
074200     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
074300     MOVE WDA9-STATUS-CODE       TO STATUS-WS                             
074400     PERFORM IMS-STATUSCHECK                                              
074500     .                                                                    
074600     EJECT                                                                
074700 IMS-GNP-WDA911 SECTION.                                                  
074800                                                                          
074900     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-X ')'                         
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
075200     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
075300     MOVE WDA9-STATUS-CODE       TO STATUS-WS                             
075400     PERFORM IMS-STATUSCHECK                                              
075500     .                                                                    
075600     EJECT                                                                
075700                                                                          
075800 IMS-GU-WDGX3152 SECTION.                                                 
075900                                                                          
076000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
076100             DELIMITED BY SIZE INTO SSA1                                  
076200     STRING 'WDGX3152(KY3152   =' W-W3152KY-X ')'                         
076300             DELIMITED BY SIZE INTO SSA2                                  
076400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
076500     CALL CBLTDLI USING GU  3151-PCB DLI-IO-WDGX3152 SSA1 SSA2            
076600     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
076700     PERFORM IMS-STATUSCHECK                                              
076800     .                                                                    
076900     EJECT                                                                
077000 IMS-GHU-WDGX3152 SECTION.                                                
077100                                                                          
077200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
077300             DELIMITED BY SIZE INTO SSA1                                  
077400     STRING 'WDGX3152(KY3152   =' W-W3152KY-X ')'                         
077500             DELIMITED BY SIZE INTO SSA2                                  
077600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
077700     CALL CBLTDLI USING GHU 3151-PCB DLI-IO-WDGX3152 SSA1 SSA2            
077800     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
077900     PERFORM IMS-STATUSCHECK                                              
078000     .                                                                    
078100     EJECT                                                                
078200 IMS-REPL-WDGX3152 SECTION.                                               
078300                                                                          
078400     MOVE 'WDGX3152 '            TO SSA1                                  
078500     MOVE '  '                   TO GOOD-STATUSCODES                      
078600     CALL CBLTDLI USING REPL 3151-PCB DLI-IO-WDGX3152 SSA1                
078700     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
078800     PERFORM IMS-STATUSCHECK                                              
078900     .                                                                    
079000     EJECT                                                                
079100 IMS-GNP-WDGX3154 SECTION.                                                
079200                                                                          
079300     MOVE 'WDGX3154 '            TO SSA1                                  
079400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
079500     CALL CBLTDLI USING GNP 3151-PCB DLI-IO-WDGX3154 SSA1                 
079600     MOVE 3151-STATUS-CODE       TO STATUS-WS                             
079700     PERFORM IMS-STATUSCHECK                                              
079800     .                                                                    
079900     EJECT                                                                
080000 IMS-GU-WDGX3161 SECTION.                                                 
080100                                                                          
080200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3161-X ')'                    
080300             DELIMITED BY SIZE INTO SSA1                                  
080400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
080500     CALL CBLTDLI USING GU 3161-PCB DLI-IO-WDGX3161 SSA1                  
080600     MOVE 3161-STATUS-CODE       TO STATUS-WS                             
080700     PERFORM IMS-STATUSCHECK                                              
080800     .                                                                    
080900     EJECT                                                                
081000 IMS-GNP-WDGX3162 SECTION.                                                
081100                                                                          
081200     MOVE 'WDGX3162 '            TO SSA1                                  
081300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
081400     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
081500     MOVE 3161-STATUS-CODE       TO STATUS-WS                             
081600     PERFORM IMS-STATUSCHECK                                              
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-STATUSCHECK SECTION.                                                 
082000                                                                          
082100     SET STATUS-IX               TO 1                                     
082200     SEARCH GOOD-STATUS                                                   
082300       AT END                                                             
082400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
082500             DELIMITED BY SIZE INTO ERROR-TEXT                            
082600         CALL FELLOG                                                      
082700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
082800         CONTINUE                                                         
082900     END-SEARCH                                                           
083000     .                                                                    
083100     EJECT                                                                
083200 DB2-SELECT-BYART SECTION.                                                
083300     MOVE 'DB2-SELECT-BYART'     TO CURRENT-DB-SEC                        
083400                                                                          
083500     MOVE 000100                 TO GOOD-SQLCODECODES                     
083600     EXEC SQL SELECT                                                      
083700                  KVBYTPKO                                                
083800              INTO                                                        
083900                  :BYART-KVBYTPKO                                         
084000              FROM BYART                                                  
084100             WHERE IDARTNR_BYT = :WS-IDARTNR-BYT                          
084200     END-EXEC                                                             
084300     MOVE SQLCODE                TO SQLCODE-WS                            
084400     PERFORM DB2-STATUS-KONTROLL                                          
084500     .                                                                    
084600     EJECT                                                                
084700 DB2-STATUS-KONTROLL SECTION.                                             
084800                                                                          
084900     SET SQLCODE-IX              TO 1                                     
085000     SEARCH GOOD-SQLCODE                                                  
085100       AT END                                                             
085200         STRING ' INVALID RETURN CODE FROM DB2: ' SQLCODE-WS              
085300             DELIMITED BY SIZE INTO ERROR-TEXT                            
085400         CALL FELLOG                                                      
085500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
085600         CONTINUE                                                         
085700     END-SEARCH                                                           
085800     .                                                                    
