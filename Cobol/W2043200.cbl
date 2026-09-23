000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2043200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   12/10/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TREND AND PB ADJUSTMENTS.                                        
000900*                                                                         
001000*        THE PROGRAM READS     WDK6                                       
001100*        THE PROGRAM UPDATES   WDK7                                       
001200*        THE PROGRAM READS     WDD3                                       
001300*        THE PROGRAM UPDATES   WDG3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W2T432                                              
001700*        MID:         W2I43201                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W2O43201                                            
002100*                                                                         
002101* CHANGE LOG:                                                             
002110* 2015-04-22   E'TRACKER 10130993                                         
002120*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
002130*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002810 77  IDPGM                       PIC X(08)   VALUE 'W2043200'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003410 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
003420 77  WS-TIPBJUST                 PIC 9(4)    VALUE ZERO.                  
003500                                                                          
003600 77  IX                          PIC S9(4)   VALUE +0   COMP SYNC.        
003700                                                                          
003800 77  DATUM                       PIC 9(6)    VALUE ZERO.                  
003900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004200     88  INDATA-OK                           VALUE 'Y'.                   
004300     88  INDATA-WRONG                        VALUE 'N'.                   
004400                                                                          
004500 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004600     88  KEYS-OK                             VALUE 'Y'.                   
004700     88  KEYS-WRONG                          VALUE 'N'.                   
005200                                                                          
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  OWN-MID                             VALUE '2432'.                
005500     88  GOOD-MID                            VALUE '2431' '2432'          
005600                                                   '2433' '2434'          
005700                                                   '2435' '2436'          
005800                                                   '2437' '2438'          
005900                                                   '2439'.                
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200 01  WS-INPUT.                                                            
006300     05  WS-KVPB-TREND-IN        PIC X(8).                                
006400     05  WS-KVVECKOR-TREND-IN    PIC X(2).                                
006500         88 WS-KVVECKOR-TREND-IN-GOOD        VALUE '01' THRU '52'.        
006600     05  WS-KVPB-JUST1-IN        PIC X(8).                                
006700     05  WS-TIPBJUST-1-IN        PIC X(4).                                
006800     05  WS-KVPB-JUST2-IN        PIC X(8).                                
006900     05  WS-TIPBJUST-2-IN        PIC X(4).                                
007000     EJECT                                                                
007100 01  WS-TIAAVV                   PIC 9(4).                                
007200 01  WS-DAREFESC                 PIC 9(8).                                
007300 01  FILLER REDEFINES WS-DAREFESC.                                        
007400     05  FILLER                  PIC 9(2).                                
007500     05  WS-DAREFESC-YYMMDD      PIC 9(6).                                
007600     EJECT                                                                
007610 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
007620*01 -COPY WWIDFTG                                                         
007630     EJECT                                                                
007700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007800 01  GENERAL-SUBPROGRAMS.                                                 
007900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008500     EJECT                                                                
008600*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008700*01 -COPY WMEDAREA                                                        
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009500     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
009600     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
009700     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
009800     03  ERR-VALID-PB-0          PIC X(3)    VALUE '163'.                 
009900     03  ERR-TREND-MAKES-PB-LT-0 PIC X(3)    VALUE '162'.                 
009910     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
010000     EJECT                                                                
010100*01  -COPY WDATAREA                                                       
010200     EJECT                                                                
010300*01  -COPY WDECAREA                                                       
010400     EJECT                                                                
010500*01  -COPY WY2000W3                                                       
010600     EJECT                                                                
010700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011000     SKIP3                                                                
011100*01 -COPY WMSGINIT                                                        
011200     EJECT                                                                
011300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011600     SKIP3                                                                
011700*01  MID -COPY W2I43201                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012000     SKIP3                                                                
012100*01  -COPY WMSGAREA                                                       
012200     EJECT                                                                
012300     03  MOD REDEFINES MSG-AREA.                                          
012400*      05  -COPY W2O43201                                                 
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012700     SKIP3                                                                
012800*01  -COPY WMFSAREA                                                       
012900     EJECT                                                                
013000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  KEYS-FOR-DLI.                                                        
013500     03  W-IDARTNR-X.                                                     
013600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013700     03  W-KDSEGKEY-X.                                                    
013800         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
013900     03  W-IDSKYLT-X.                                                     
014000         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
014100     03  W-2203KEY-X.                                                     
014200         05  W-IDHTYP-2203       PIC X(4)    VALUE '2203'.                
014300         05  W-IDDC-X.                                                    
014400             07  W-IDDC          PIC X(2)    VALUE SPACE.                 
014500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
014600     SKIP2                                                                
014700*    --- STATUS CODES FROM IMS                                            
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FOUND                       VALUE '  '.                  
015000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015200     SKIP2                                                                
015300 01  GOOD-STATUSCODES.                                                    
015400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(64).                               
015700 01  SSA2                        PIC X(64).                               
015800 01  SSA3                        PIC X(64).                               
015900     EJECT                                                                
016000*    --- IMS FUNCTION CODES                                               
016100*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*    ---  DLI INPUT-OUTPUT AREA                                           
016400                                                                          
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016600 01  DLI-IO-WDK601.                                                       
016700*    03  -COPY WDK601                                                     
016800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
016900 01  DLI-IO-WDB601.                                                       
017000*    03  -COPY WDB601                                                     
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
017200 01  DLI-IO-WDK701.                                                       
017300*    03  -COPY WDK701                                                     
017400     EJECT                                                                
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
017600 01  DLI-IO-WDK711.                                                       
017700*    03  -COPY WDK711                                                     
017800     EJECT                                                                
017900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
018000 01  DLI-IO-WDK722.                                                       
018100*    03  -COPY WDK722                                                     
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
018300 01  DLI-IO-WDD311.                                                       
018400*    03  -COPY WDD311                                                     
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
018600 01  DLI-IO-WDGX2204.                                                     
018700*    03  -COPY WDGX2204                                                   
018800     EJECT                                                                
018900 LINKAGE SECTION.                                                         
019000*01  -COPY W0009   -PRE MSG-                                              
019100*01  -COPY W0008   -PRE WDP7-                                             
019200     05  FILLER                  PIC X.                                   
019300                                                                          
019400*01  -COPY W0008  -PRE WDK6-                                              
019500     05  FILLER                  PIC X.                                   
019600                                                                          
019700*01  -COPY W0008  -PRE WDB6-                                              
019800     05  FILLER                  PIC X.                                   
019900                                                                          
020000*01  -COPY W0008  -PRE WDK7-                                              
020100     05  FILLER                  PIC X.                                   
020200                                                                          
020300*01  -COPY W0008  -PRE WDD3B-                                             
020400     05  FILLER                  PIC X.                                   
020500                                                                          
020600*01  -COPY W0008  -PRE WDG3-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDB6-PCB             
021000     WDK7-PCB WDD3B-PCB WDG3-PCB.                                         
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDB6-PCB             
021300     WDK7-PCB WDD3B-PCB WDG3-PCB.                                         
021400                                                                          
021500     PERFORM IMS-GET-MSG                                                  
021600     IF SEGMENT-FOUND                                                     
021700       PERFORM A-INIT                                                     
021800       PERFORM B-CHECK-KEYS                                               
021900       IF KEYS-OK                                                         
022000         IF MFS-UPDATE                                                    
022100           PERFORM G-CHECK-INPUT                                          
022200           IF INDATA-OK                                                   
022300             PERFORM H-UPDATE                                             
022400           END-IF                                                         
022500         ELSE                                                             
022600           IF MFS-FIRST                                                   
022700             PERFORM C-FIRST-PAGE                                         
022800           ELSE                                                           
022900             PERFORM E-SAME-PAGE                                          
023000           END-IF                                                         
023100         END-IF                                                           
023200         PERFORM F-READ-SHOW-INFO                                         
023300       END-IF                                                             
023400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O43201 + 4                      
023500       PERFORM IMS-INSERT-MSG                                             
023600     END-IF                                                               
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 A-INIT SECTION.                                                          
024300                                                                          
024400     IF MSG-DOUBLE-TRANSACTIONS                                           
024500       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
024600                                 TO MID-W2I43201                          
024700       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
024800       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
024900     ELSE                                                                 
025000       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
025100                                 TO MID-W2I43201                          
025200       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
025300       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
025400     END-IF                                                               
025500                                                                          
025600     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
025700     MOVE MSG-IDPFK              TO MFS-IDPFK                             
025800     MOVE MFS-IDTRANS            TO W-IDTRANS                             
025900                                                                          
026000     MOVE LOW-VALUE              TO MSG-AREA                              
026100     MOVE 'W2O432N1'             TO MFS-IDMOD                             
026200     MOVE '2432'                 TO MOD-IDTRANS                           
026300     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL                          
026400                                    MOD-TEMFSINF                          
026500                                                                          
026600     IF OWN-MID OR HELP-MID                                               
026700       CONTINUE                                                           
026800     ELSE                                                                 
026900       MOVE SPACE                TO MFS-KDTRTYP                           
027000       MOVE '7'                  TO MFS-IDPFK                             
027100     END-IF                                                               
027200                                                                          
027300     ACCEPT DATUM              FROM DATE                                  
027400     .                                                                    
027500     EJECT                                                                
027600 B-CHECK-KEYS SECTION.                                                    
027700                                                                          
027800     MOVE ALL '+'                TO MSGI-WMSGINIT                         
027900     MOVE '001'                  TO MSGI-KDCALL                           
028000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
028100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
028200     MOVE '2432'                 TO MSGI-IDTRANS                          
028300     IF GOOD-MID                                                          
028400       MOVE MID-IDARTNR-IN       TO MSGI-IDARTNR                          
028500       MOVE MID-IDDC-IN          TO MSGI-IDDC-KEY                         
028600     END-IF                                                               
028700     CALL W005INIT            USING MSGI-WMSGINIT                         
028800                                    WDP7-PCB                              
028900                                                                          
029000*    - LANGUAGE TO BE USED BY MEDKONV                                     
029100     MOVE 'GB'                   TO MED-IDSKYLT                           
029200                                                                          
029300     MOVE YES                    TO KEYS-SW                               
029400                                                                          
029500                                                                          
029600*    -- CHECK OF IDARTNR                                                  
029700     MOVE MFS-ERASE-FIELD        TO MOD-IDARTNR-IN                        
029800                                                                          
029900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
030000       MOVE '7'                  TO MFS-IDPFK                             
030100       MOVE SPACE                TO MFS-KDTRTYP                           
030200     END-IF                                                               
030210     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
030310     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
030400     IF WS-IDARTNR NUMERIC AND                                            
030500        WS-IDARTNR > ZERO                                                 
030600       MOVE WS-IDARTNR         TO W-IDARTNR                               
030700       PERFORM IMS-GU-WDK601                                              
030800       IF SEGMENT-FOUND                                                   
030900         MOVE ART-REKSIFFR       TO MOD-REKSIFFR                          
031000         MOVE '-'                TO MOD-DASH                              
031100       ELSE                                                               
031200         MOVE ERR-PART-MISSING   TO MED-IDMFSFEL                          
031300         MOVE NOO                TO KEYS-SW                               
031400       END-IF                                                             
031500     ELSE                                                                 
031600       MOVE NOO                  TO KEYS-SW                               
031700       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
031800     END-IF                                                               
031900                                                                          
032000                                                                          
032100*    -- CHECK OF IDDC                                                     
032200     MOVE MFS-ERASE-FIELD        TO MOD-IDDC-IN                           
032300                                                                          
032400     IF MID-IDDC-IN NOT = ALL '+'                                         
032500       MOVE '7'                  TO MFS-IDPFK                             
032600       MOVE SPACE                TO MFS-KDTRTYP                           
032700     END-IF                                                               
032800                                                                          
032900     IF MSGI-IDDC-KEY = ALL '+' OR SPACE                                  
033000       MOVE MSGI-IDDC            TO W-IDDC                                
033100     ELSE                                                                 
033200       MOVE MSGI-IDDC-KEY        TO W-IDDC                                
033300     END-IF                                                               
033400                                                                          
033500     PERFORM IMS-GU-WDB601                                                
033600     IF SEGMENT-FOUND AND                                                 
033700       (DCS-NDC-CN OR                                                     
033710       (DCS-NDC-NA AND DCS-USA))                                          
033800       CONTINUE                                                           
033900     ELSE                                                                 
034000       MOVE NOO                  TO KEYS-SW                               
034100       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
034200     END-IF                                                               
034300                                                                          
034400     IF GOOD-MID OR KEYS-OK                                               
034410       MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                              
034420       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
034600       MOVE W-IDDC               TO MOD-IDDC-UT                           
034700     ELSE                                                                 
034800       MOVE MFS-ERASE-FIELD      TO MOD-IDARTNR-UT                        
034900                                    MOD-IDDC-UT                           
035000     END-IF                                                               
035100                                                                          
035200     IF KEYS-WRONG                                                        
035300       CALL WMEDKONV          USING MED-WMEDAREA                          
035400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
035500       PERFORM MFS-ERASE-FIELD-IN                                         
035600       PERFORM MFS-ERASE-FIELD-OUT                                        
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 C-FIRST-PAGE SECTION.                                                    
036100                                                                          
036200     PERFORM MFS-ERASE-FIELD-IN                                           
036300     .                                                                    
036400     EJECT                                                                
036500 E-SAME-PAGE SECTION.                                                     
036600                                                                          
036700     IF OWN-MID OR HELP-MID                                               
036800       IF MID-INPUT = ALL '+' AND                                         
036900          MID-KDTECKEN-TREND-IN = '?'                                     
037000         PERFORM MFS-ERASE-FIELD-IN                                       
037100       ELSE                                                               
037200         MOVE INF-PRESS-PF11     TO MED-IDMFSINF                          
037300         CALL WMEDKONV        USING MED-WMEDAREA                          
037400         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
037500         PERFORM EA-MID-INDATA-TO-MOD                                     
037600       END-IF                                                             
037700     ELSE                                                                 
037800       PERFORM MFS-ERASE-FIELD-IN                                         
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 EA-MID-INDATA-TO-MOD SECTION.                                            
038300                                                                          
038400     IF MID-KDTECKEN-TREND-IN NOT = '?'                                   
038500       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
038600                                 TO MOD-KDTECKEN-TREND-IN                 
038700       MOVE MFS-ADD-READ-FIELD   TO MOD-KDTECKEN-TREND-IN-ATTR            
038800     ELSE                                                                 
038900       MOVE MFS-ERASE-FIELD      TO MOD-KDTECKEN-TREND-IN                 
039000     END-IF                                                               
039100                                                                          
039200     IF MID-KVPB-TREND-IN NOT = ALL '+'                                   
039300       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
039400                                 TO MOD-KVPB-TREND-IN                     
039500       MOVE MFS-ADD-READ-FIELD   TO MOD-KVPB-TREND-IN-ATTR                
039600     ELSE                                                                 
039700       MOVE MFS-ERASE-FIELD      TO MOD-KVPB-TREND-IN                     
039800     END-IF                                                               
039900                                                                          
040000     IF MID-KVVECKOR-TREND-IN NOT = ALL '+'                               
040100       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
040200                                 TO MOD-KVVECKOR-TREND-IN                 
040300       MOVE MFS-ADD-READ-FIELD   TO MOD-KVVECKOR-TREND-IN-ATTR            
040400     ELSE                                                                 
040500       MOVE MFS-ERASE-FIELD      TO MOD-KVVECKOR-TREND-IN                 
040600     END-IF                                                               
040700                                                                          
040800     IF MID-KVPB-JUST1-IN NOT = ALL '+'                                   
040900       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
041000                                 TO MOD-KVPB-JUST1-IN                     
041100       MOVE MFS-ADD-READ-FIELD   TO MOD-KVPB-JUST1-IN-ATTR                
041200     ELSE                                                                 
041300       MOVE MFS-ERASE-FIELD      TO MOD-KVPB-JUST1-IN                     
041400     END-IF                                                               
041500                                                                          
041600     IF MID-TIPBJUST-1-IN NOT = ALL '+'                                   
041700       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
041800                                 TO MOD-TIPBJUST-1-IN                     
041900       MOVE MFS-ADD-READ-FIELD   TO MOD-TIPBJUST-1-IN-ATTR                
042000     ELSE                                                                 
042100       MOVE MFS-ERASE-FIELD      TO MOD-TIPBJUST-1-IN                     
042200     END-IF                                                               
042300                                                                          
042400     IF MID-KVPB-JUST2-IN NOT = ALL '+'                                   
042500       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
042600                                 TO MOD-KVPB-JUST2-IN                     
042700       MOVE MFS-ADD-READ-FIELD   TO MOD-KVPB-JUST2-IN-ATTR                
042800     ELSE                                                                 
042900       MOVE MFS-ERASE-FIELD      TO MOD-KVPB-JUST2-IN                     
043000     END-IF                                                               
043100                                                                          
043200     IF MID-TIPBJUST-2-IN NOT = ALL '+'                                   
043300       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
043400                                 TO MOD-TIPBJUST-2-IN                     
043500       MOVE MFS-ADD-READ-FIELD   TO MOD-TIPBJUST-2-IN-ATTR                
043600     ELSE                                                                 
043700       MOVE MFS-ERASE-FIELD      TO MOD-TIPBJUST-2-IN                     
043800     END-IF                                                               
043900                                                                          
044000     .                                                                    
044100     EJECT                                                                
044200 F-READ-SHOW-INFO SECTION.                                                
044300                                                                          
044400     PERFORM IMS-GU-WDK711                                                
044500                                                                          
044600     IF SEGMENT-MISSING                                                   
044700       MOVE ERR-KEYS-MISSING     TO MED-IDMFSFEL                          
044800       CALL WMEDKONV          USING MED-WMEDAREA                          
044900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
045000       PERFORM MFS-ERASE-FIELD-OUT                                        
045100     ELSE                                                                 
045200       IF SLAG-IDDC-REF NOT = SPACE                                       
045300         MOVE ERR-REFILL-PART    TO MED-IDMFSFEL                          
045400         CALL WMEDKONV        USING MED-WMEDAREA                          
045500         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
045600         PERFORM MFS-ERASE-FIELD-OUT                                      
045700       ELSE                                                               
045800         PERFORM FA-READ-BASICDATA                                        
045900       END-IF                                                             
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 FA-READ-BASICDATA SECTION.                                               
046400                                                                          
046500     PERFORM IMS-GU-WDK722                                                
046600     IF SEGMENT-MISSING                                                   
046700       MOVE ERR-KEYS-MISSING     TO MED-IDMFSFEL                          
046800       CALL WMEDKONV          USING MED-WMEDAREA                          
046900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
047000       PERFORM MFS-ERASE-FIELD-OUT                                        
047100     ELSE                                                                 
047200       MOVE SLAG-KVPB-REF        TO MOD-KVPB-REF                          
047210       MOVE SLAG-TIREFMPB        TO MOD-TIREFMPB                          
047300       MOVE SLAG-DAREFESC        TO WS-DAREFESC                           
047400       MOVE WS-DAREFESC-YYMMDD   TO MOD-DAREFESC                          
047500       MOVE XLAG-KVPB-TREND      TO MOD-KVPB-TREND-UT                     
047600       MOVE XLAG-KVVECKOR-TREND  TO MOD-KVVECKOR-TREND-UT                 
047700       MOVE XLAG-TIDATUM-TREND   TO MOD-TIDATUM-TREND                     
047800       MOVE XLAG-KVPB-JUST1      TO MOD-KVPB-JUST1-UT                     
047810       IF XLAG-TIPBJUST-1 = +0                                            
047820          MOVE 'YYWW'            TO MOD-TIPBJUST-1-UT                     
047830       ELSE                                                               
047900          MOVE XLAG-TIPBJUST-1   TO WS-TIPBJUST                           
047901          MOVE WS-TIPBJUST       TO MOD-TIPBJUST-1-UT                     
047910       END-IF                                                             
048000       MOVE XLAG-KVPB-JUST2      TO MOD-KVPB-JUST2-UT                     
048010       IF XLAG-TIPBJUST-2 = +0                                            
048020          MOVE 'YYWW'            TO MOD-TIPBJUST-2-UT                     
048030       ELSE                                                               
048100          MOVE XLAG-TIPBJUST-2   TO WS-TIPBJUST                           
048101          MOVE WS-TIPBJUST       TO MOD-TIPBJUST-2-UT                     
048110       END-IF                                                             
048200     END-IF                                                               
048300                                                                          
048400     PERFORM IMS-GU-WDD3B                                                 
048500     IF SEGMENT-FOUND                                                     
048600       MOVE TEXT-BEART           TO MOD-BEART-ENG                         
048700     ELSE                                                                 
048800       MOVE SPACE                TO MOD-BEART-ENG                         
048900     END-IF                                                               
049000                                                                          
049100     .                                                                    
049200     EJECT                                                                
049300 G-CHECK-INPUT SECTION.                                                   
049400                                                                          
049500     MOVE YES                    TO INDATA-SW                             
049600     IF MID-INPUT = ALL '+' AND                                           
049700        MID-KDTECKEN-TREND-IN = '?'                                       
049800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
049900       CALL WMEDKONV          USING MED-WMEDAREA                          
050000       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
050100       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
050200       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
050300       MOVE NOO                  TO INDATA-SW                             
050400     ELSE                                                                 
050410       PERFORM GA-AUTH-USER-CHECK                                         
050420       IF INDATA-OK                                                       
050500         PERFORM IMS-GHU-WDK711                                           
050600         IF SEGMENT-MISSING                                               
050700           MOVE NOO                TO INDATA-SW                           
050800           MOVE ERR-KEYS-MISSING   TO MED-IDMFSFEL                        
050900           CALL WMEDKONV        USING MED-WMEDAREA                        
051000           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
051100           PERFORM MFS-ERASE-FIELD-OUT                                    
051200         ELSE                                                             
051300           IF SLAG-IDDC-REF NOT = SPACE                                   
051400             MOVE NOO              TO INDATA-SW                           
051500             MOVE ERR-REFILL-PART  TO MED-IDMFSFEL                        
051600             CALL WMEDKONV      USING MED-WMEDAREA                        
051700             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
051800           ELSE                                                           
051900             PERFORM IMS-GHU-WDK722                                       
052000             IF SEGMENT-MISSING                                           
052100               MOVE NOO                TO INDATA-SW                       
052200               MOVE ERR-KEYS-MISSING   TO MED-IDMFSFEL                    
052300               CALL WMEDKONV        USING MED-WMEDAREA                    
052400               MOVE MED-MFSFEL         TO MOD-TEMFSFEL                    
052500               PERFORM MFS-ERASE-FIELD-OUT                                
052600             END-IF                                                       
052700           END-IF                                                         
052800         END-IF                                                           
052810       END-IF                                                             
052900     END-IF                                                               
053000                                                                          
053100     IF INDATA-OK                                                         
053200                                                                          
053300       IF MID-KVPB-TREND-IN NOT = ALL '+'                                 
053540         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
053550                                 TO MOD-KVPB-TREND-IN                     
053600         MOVE MID-KVPB-TREND-IN  TO DEC-IDFRIDATA                         
053700         MOVE 6                  TO DEC-KVHELTAL                          
053800         MOVE 1                  TO DEC-KVDECIMAL                         
053900         CALL WDECEDIT        USING DEC-WDECAREA                          
054000         IF DEC-KDSVAR-OK                                                 
054011          IF DEC-IDEDITDATA   = ZERO                                      
054012        AND XLAG-KVPB-TREND   > ZERO                                      
054013            MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-TREND-IN-ATTR               
054014            MOVE MFS-NUM-FIELD-OK TO MOD-KDTECKEN-TREND-IN-ATTR           
054015            MOVE +0               TO XLAG-KVPB-TREND                      
054016            MOVE +0               TO XLAG-KVVECKOR-TREND                  
054017            MOVE +0               TO XLAG-TIDATUM-TREND                   
054018          ELSE                                                            
054100           MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-TREND-IN-ATTR                
054200           IF MID-KDTECKEN-TREND-IN = '+' OR '-'                          
054300             MOVE MFS-NUM-FIELD-OK                                        
054400                                 TO MOD-KDTECKEN-TREND-IN-ATTR            
054500             IF MID-KDTECKEN-TREND-IN = '-'                               
054600               COMPUTE XLAG-KVPB-TREND                                    
054700                                  = -1 * DEC-IDEDITDATA                   
054800             ELSE                                                         
054900               MOVE DEC-IDEDITDATA                                        
055000                                 TO XLAG-KVPB-TREND                       
055100             END-IF                                                       
055200           ELSE                                                           
055300             MOVE MFS-NUM-FIELD-WRONG                                     
055400                                 TO MOD-KDTECKEN-TREND-IN-ATTR            
055500             MOVE NOO            TO INDATA-SW                             
055600           END-IF                                                         
055610          END-IF                                                          
055700         ELSE                                                             
055800           MOVE MFS-NUM-FIELD-WRONG                                       
055900                                 TO MOD-KVPB-TREND-IN-ATTR                
056000           MOVE NOO              TO INDATA-SW                             
056110         END-IF                                                           
056200       ELSE                                                               
056300         MOVE MFS-ERASE-FIELD    TO MOD-KVPB-TREND-IN                     
056400         IF MID-KDTECKEN-TREND-IN NOT = '?'                               
056500           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
056600                                 TO MOD-KDTECKEN-TREND-IN                 
056700           IF MID-KDTECKEN-TREND-IN = '+' OR '-'                          
056800             MOVE MFS-NUM-FIELD-OK                                        
056900                                 TO MOD-KDTECKEN-TREND-IN-ATTR            
057000             MOVE MFS-NUM-FIELD-WRONG                                     
057100                                 TO MOD-KVPB-TREND-IN-ATTR                
057200             MOVE NOO            TO INDATA-SW                             
057300           ELSE                                                           
057400             MOVE MFS-NUM-FIELD-WRONG                                     
057500                                 TO MOD-KDTECKEN-TREND-IN-ATTR            
057600             MOVE NOO            TO INDATA-SW                             
057700           END-IF                                                         
057800         ELSE                                                             
057900           MOVE MFS-ERASE-FIELD  TO MOD-KDTECKEN-TREND-IN                 
058000         END-IF                                                           
058100       END-IF                                                             
058200                                                                          
058300       IF MID-KVVECKOR-TREND-IN NOT = ALL '+'                             
058400         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
058500                                 TO MOD-KVVECKOR-TREND-IN                 
058600         MOVE MID-KVVECKOR-TREND-IN                                       
058700                                 TO WS-KVVECKOR-TREND-IN                  
058800         INSPECT WS-KVVECKOR-TREND-IN REPLACING LEADING SPACE             
058900                                                     BY ZERO              
059000         IF MID-KVVECKOR-TREND-IN = ZERO                                  
059001        AND XLAG-KVVECKOR-TREND   > ZERO                                  
059002            MOVE MFS-NUM-FIELD-OK   TO MOD-KVVECKOR-TREND-IN-ATTR         
059003            MOVE MID-KVVECKOR-TREND-IN                                    
059004                                    TO XLAG-KVVECKOR-TREND                
059009            MOVE +0                 TO XLAG-KVPB-TREND                    
059011            MOVE +0                 TO XLAG-TIDATUM-TREND                 
059012         ELSE                                                             
059020            IF WS-KVVECKOR-TREND-IN-GOOD                                  
059100              MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-TREND-IN-ATTR         
059200              MOVE WS-KVVECKOR-TREND-IN                                   
059300                                    TO XLAG-KVVECKOR-TREND                
059400            ELSE                                                          
059500              MOVE MFS-NUM-FIELD-WRONG                                    
059600                                    TO MOD-KVVECKOR-TREND-IN-ATTR         
059700              MOVE NOO              TO INDATA-SW                          
059800            END-IF                                                        
059810         END-IF                                                           
059900       ELSE                                                               
060000         MOVE MFS-ERASE-FIELD    TO MOD-KVVECKOR-TREND-IN                 
060100       END-IF                                                             
060200                                                                          
060440       IF MID-KVVECKOR-TREND-IN > ZERO                                    
060460       OR MID-KVPB-TREND-IN     > ZERO                                    
060500          MOVE DATUM              TO XLAG-TIDATUM-TREND                   
060610       END-IF                                                             
060700                                                                          
060800       IF MID-KVPB-JUST1-IN NOT = ALL '+'                                 
060900         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
061000                                 TO MOD-KVPB-JUST1-IN                     
061100         MOVE MID-KVPB-JUST1-IN  TO DEC-IDFRIDATA                         
061200         MOVE 6                  TO DEC-KVHELTAL                          
061300         MOVE 1                  TO DEC-KVDECIMAL                         
061400         CALL WDECEDIT        USING DEC-WDECAREA                          
061500         IF DEC-KDSVAR-OK                                                 
061600           MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-JUST1-IN-ATTR                
061700           MOVE DEC-IDEDITDATA   TO XLAG-KVPB-JUST1                       
061800         ELSE                                                             
061900           MOVE MFS-NUM-FIELD-WRONG                                       
062000                                 TO MOD-KVPB-JUST1-IN-ATTR                
062100           MOVE NOO              TO INDATA-SW                             
062200         END-IF                                                           
062300                                                                          
062400*        TIPBJUST-1 SHOULD ALSO BE ENTERED.                               
062500         IF MID-TIPBJUST-1-IN = ALL '+'                                   
062600           MOVE MFS-NUM-FIELD-WRONG                                       
062700                                 TO MOD-TIPBJUST-1-IN-ATTR                
062800           MOVE NOO              TO INDATA-SW                             
062900         END-IF                                                           
063000                                                                          
063100*        UPDATE NOT ALLOWED IF THIS CHECK FAILS.                          
063200         IF SLAG-KVPB-REF > ZERO                                          
063300           CONTINUE                                                       
063400         ELSE                                                             
063500           MOVE MFS-NUM-FIELD-WRONG                                       
063600                                 TO MOD-KVPB-JUST1-IN-ATTR                
063700           MOVE NOO              TO INDATA-SW                             
063800           MOVE ERR-VALID-PB-0   TO MED-IDMFSINF                          
063900         END-IF                                                           
064400       ELSE                                                               
064500         MOVE MFS-ERASE-FIELD    TO MOD-KVPB-JUST1-IN                     
064600       END-IF                                                             
064700                                                                          
064800       IF MID-TIPBJUST-1-IN NOT = ALL '+'                                 
064900         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
065000                                 TO MOD-TIPBJUST-1-IN                     
065100         MOVE MID-TIPBJUST-1-IN  TO WS-TIPBJUST-1-IN                      
065200         INSPECT WS-TIPBJUST-1-IN REPLACING LEADING SPACE BY ZERO         
065300                                                                          
065400         IF WS-TIPBJUST-1-IN = ALL ZERO                                   
065403           MOVE MFS-NUM-FIELD-OK   TO MOD-TIPBJUST-1-IN-ATTR              
065405           MOVE WS-TIPBJUST-1-IN   TO XLAG-TIPBJUST-1                     
065410         ELSE                                                             
065430           IF WS-TIPBJUST-1-IN NOT NUMERIC                                
065500             MOVE MFS-NUM-FIELD-WRONG                                     
065600                                   TO MOD-TIPBJUST-1-IN-ATTR              
065700             MOVE NOO              TO INDATA-SW                           
065800           ELSE                                                           
065900             MOVE MFS-NUM-FIELD-OK TO MOD-TIPBJUST-1-IN-ATTR              
066000             MOVE WS-TIPBJUST-1-IN TO DAT-I-TIDATUM                       
066100             MOVE 'AAVV'           TO DAT-KDDATFORM                       
066200             PERFORM S01-CALL-WDATKONV                                    
066300             IF DAT-KDSVAR-OK                                             
066400               MOVE WS-TIPBJUST-1-IN                                      
066500                                   TO TMP1-YYWW                           
066600               PERFORM GB-DATE-CHECK                                      
066700               IF TMP1-YYWW > TMP2-YYWW                                   
066800                 MOVE WS-TIPBJUST-1-IN                                    
066900                                   TO XLAG-TIPBJUST-1                     
067000               ELSE                                                       
067100                 MOVE MFS-NUM-FIELD-WRONG                                 
067200                                   TO MOD-TIPBJUST-1-IN-ATTR              
067300                 MOVE NOO          TO INDATA-SW                           
067400               END-IF                                                     
067500             ELSE                                                         
067600               MOVE MFS-NUM-FIELD-WRONG                                   
067700                                   TO MOD-TIPBJUST-1-IN-ATTR              
067800               MOVE NOO            TO INDATA-SW                           
067900             END-IF                                                       
068000           END-IF                                                         
068010         END-IF                                                           
068100       ELSE                                                               
068200         MOVE MFS-ERASE-FIELD    TO MOD-TIPBJUST-1-IN                     
068300       END-IF                                                             
068400                                                                          
068500       IF MID-KVPB-JUST2-IN NOT = ALL '+'                                 
068600         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
068700                                 TO MOD-KVPB-JUST2-IN                     
068800         MOVE MID-KVPB-JUST2-IN  TO DEC-IDFRIDATA                         
068900         MOVE 6                  TO DEC-KVHELTAL                          
069000         MOVE 1                  TO DEC-KVDECIMAL                         
069100         CALL WDECEDIT        USING DEC-WDECAREA                          
069200         IF DEC-KDSVAR-OK                                                 
069300           MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-JUST2-IN-ATTR                
069400           MOVE DEC-IDEDITDATA   TO XLAG-KVPB-JUST2                       
069500         ELSE                                                             
069600           MOVE MFS-NUM-FIELD-WRONG                                       
069700                                 TO MOD-KVPB-JUST2-IN-ATTR                
069800           MOVE NOO              TO INDATA-SW                             
069900         END-IF                                                           
070000                                                                          
070100*        TIPBJUST-2 SHOULD ALSO BE ENTERED.                               
070200         IF MID-TIPBJUST-2-IN = ALL '+'                                   
070300           MOVE MFS-NUM-FIELD-WRONG                                       
070400                                 TO MOD-TIPBJUST-2-IN-ATTR                
070500           MOVE NOO              TO INDATA-SW                             
070600         END-IF                                                           
070700                                                                          
070800*        UPDATE NOT ALLOWED IF THIS CHECK FAILS.                          
070900         IF SLAG-KVPB-REF > ZERO                                          
071000           CONTINUE                                                       
071100         ELSE                                                             
071200           MOVE MFS-NUM-FIELD-WRONG                                       
071300                                 TO MOD-KVPB-JUST2-IN-ATTR                
071400           MOVE NOO              TO INDATA-SW                             
071500           MOVE ERR-VALID-PB-0   TO MED-IDMFSINF                          
071600         END-IF                                                           
072100       ELSE                                                               
072200         MOVE MFS-ERASE-FIELD    TO MOD-KVPB-JUST2-IN                     
072300       END-IF                                                             
072400                                                                          
072500       IF MID-TIPBJUST-2-IN NOT = ALL '+'                                 
072600         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
072700                                   TO MOD-TIPBJUST-2-IN                   
072800         MOVE MID-TIPBJUST-2-IN    TO WS-TIPBJUST-2-IN                    
072900         INSPECT WS-TIPBJUST-2-IN REPLACING LEADING SPACE BY ZERO         
073000                                                                          
073100         IF WS-TIPBJUST-2-IN = ALL ZERO                                   
073101           MOVE MFS-NUM-FIELD-OK TO MOD-TIPBJUST-2-IN-ATTR                
073102         ELSE                                                             
073110           IF WS-TIPBJUST-2-IN NOT NUMERIC                                
073200             MOVE MFS-NUM-FIELD-WRONG                                     
073300                                   TO MOD-TIPBJUST-2-IN-ATTR              
073400             MOVE NOO              TO INDATA-SW                           
073500           ELSE                                                           
073600             MOVE MFS-NUM-FIELD-OK TO MOD-TIPBJUST-2-IN-ATTR              
073700             MOVE WS-TIPBJUST-2-IN TO DAT-I-TIDATUM                       
073800             MOVE 'AAVV'           TO DAT-KDDATFORM                       
073900             PERFORM S01-CALL-WDATKONV                                    
074000             IF DAT-KDSVAR-OK                                             
074100               MOVE WS-TIPBJUST-2-IN                                      
074200                                   TO TMP1-YYWW                           
074300               PERFORM GB-DATE-CHECK                                      
074410               IF TMP1-YYWW > TMP2-YYWW                                   
074500                 MOVE WS-TIPBJUST-2-IN                                    
074600                                   TO XLAG-TIPBJUST-2                     
074700               ELSE                                                       
074800                 MOVE MFS-NUM-FIELD-WRONG                                 
074900                                   TO MOD-TIPBJUST-2-IN-ATTR              
075000                 MOVE NOO          TO INDATA-SW                           
075100               END-IF                                                     
075200             ELSE                                                         
075300               MOVE MFS-NUM-FIELD-WRONG                                   
075400                                   TO MOD-TIPBJUST-2-IN-ATTR              
075500               MOVE NOO            TO INDATA-SW                           
075600             END-IF                                                       
075700           END-IF                                                         
075710         END-IF                                                           
075720       END-IF                                                             
075800                                                                          
075900       IF INDATA-OK                                                       
075910         IF MID-TIPBJUST-1-IN NOT = ALL '+'                               
075911            MOVE MID-TIPBJUST-1-IN TO TMP1-YYWW                           
075920         ELSE                                                             
075921            MOVE XLAG-TIPBJUST-1   TO TMP1-YYWW                           
075930         END-IF                                                           
075940         IF MID-TIPBJUST-2-IN NOT = ALL '+'                               
075950            MOVE MID-TIPBJUST-2-IN TO TMP2-YYWW                           
075960         ELSE                                                             
075970            MOVE XLAG-TIPBJUST-2   TO TMP2-YYWW                           
075980         END-IF                                                           
076200         PERFORM WY2000P3                                                 
076300         IF (TMP1-YYWW = ZERO AND TMP2-YYWW = ZERO)                       
076301         OR TMP2-YYWW = ZERO                                              
076302            CONTINUE                                                      
076303         ELSE                                                             
076304            IF TMP1-YYWW = ZERO                                           
076305           AND TMP2-YYWW > ZERO                                           
076306              MOVE NOO         TO INDATA-SW                               
076308              MOVE MFS-NUM-FIELD-WRONG                                    
076309                               TO MOD-TIPBJUST-1-IN-ATTR                  
076310            ELSE                                                          
076320              IF TMP2-YYWW > TMP1-YYWW                                    
076400                CONTINUE                                                  
076500              ELSE                                                        
076600                MOVE NOO       TO INDATA-SW                               
076610                IF MID-TIPBJUST-1-IN NOT = ALL '+'                        
076700                  MOVE MFS-NUM-FIELD-WRONG                                
076800                               TO MOD-TIPBJUST-1-IN-ATTR                  
076810                ELSE                                                      
076811                  MOVE MFS-NUM-FIELD-WRONG                                
076812                               TO MOD-TIPBJUST-2-IN-ATTR                  
076820                END-IF                                                    
076900              END-IF                                                      
076901            END-IF                                                        
076910         END-IF                                                           
077000       END-IF                                                             
077100     ELSE                                                                 
077200       MOVE MFS-ERASE-FIELD    TO MOD-TIPBJUST-2-IN                       
077300     END-IF                                                               
077310                                                                          
077400     IF INDATA-WRONG                                                      
077410       IF MED-IDMFSFEL = '405'                                            
077411          CALL WMEDKONV        USING MED-WMEDAREA                         
077412          MOVE MED-MFSFEL         TO MOD-TEMFSFEL                         
077413          MOVE MED-MFSINF         TO MOD-TEMFSINF                         
077414          PERFORM MFS-ERASE-FIELD-OUT                                     
077430       ELSE                                                               
077500          MOVE ERR-CORR-HILITE-FLDS                                       
077600                                  TO MED-IDMFSFEL                         
077700          CALL WMEDKONV        USING MED-WMEDAREA                         
077800          MOVE MED-MFSFEL         TO MOD-TEMFSFEL                         
077900          MOVE MED-MFSINF         TO MOD-TEMFSINF                         
078000          PERFORM MFS-DONT-TOUCH-FIELD-OUT                                
078010          PERFORM MFS-DONT-TOUCH-FIELD-IN                                 
078020       END-IF                                                             
078030     END-IF                                                               
078100                                                                          
079200     .                                                                    
079300     EJECT                                                                
079310 GA-AUTH-USER-CHECK SECTION.                                              
079340                                                                          
079391     PERFORM IMS-GU-WDB601                                                
079392     IF SEGMENT-FOUND                                                     
079393       IF DCS-NDC-CN                                                      
079394       OR (DCS-NDC-NA AND DCS-USA)                                        
079395          MOVE MSGI-IDFTG          TO WS-IDFTG                            
079396          IF (DCS-NDC-CN AND IDFTG-CN)                                    
079397          OR (DCS-NDC-NA AND IDFTG-US)                                    
079398          OR MSGI-IDFTG  = WC-IDFTG-PV                                    
079399             CONTINUE                                                     
079400          ELSE                                                            
079401            MOVE NOO                   TO INDATA-SW                       
079402            MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSFEL                    
079403          END-IF                                                          
079404       ELSE                                                               
079405         MOVE NOO                   TO INDATA-SW                          
079406         MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSFEL                       
079407       END-IF                                                             
079408     ELSE                                                                 
079409       MOVE NOO                     TO INDATA-SW                          
079410       MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSFEL                       
079414     END-IF                                                               
079416     .                                                                    
079417     EJECT                                                                
079418                                                                          
079420 GB-DATE-CHECK SECTION.                                                   
079500                                                                          
079600     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
079700     PERFORM S01-CALL-WDATKONV                                            
079800     IF DAT-KDSVAR-OK                                                     
079900       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV                             
080000       MOVE WS-TIAAVV            TO TMP2-YYWW                             
080100     END-IF                                                               
080200     PERFORM WY2000P3                                                     
080300     .                                                                    
080400     EJECT                                                                
080500 H-UPDATE SECTION.                                                        
080600                                                                          
080610     IF MID-TIPBJUST-1-IN = ALL ZERO                                      
080620        MOVE ZERO                TO XLAG-TIPBJUST-1                       
080630                                    XLAG-KVPB-JUST1                       
080640     END-IF                                                               
080650                                                                          
080660     IF MID-TIPBJUST-2-IN = ALL ZERO                                      
080670        MOVE ZERO                TO XLAG-TIPBJUST-2                       
080680                                    XLAG-KVPB-JUST2                       
080690     END-IF                                                               
080691                                                                          
080700     PERFORM IMS-REPL-WDK722                                              
080800                                                                          
081200     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
081300     CALL WMEDKONV            USING MED-WMEDAREA                          
081400     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
081500     PERFORM MFS-ERASE-FIELD-IN                                           
081600     .                                                                    
081700     EJECT                                                                
081800 S01-CALL-WDATKONV SECTION.                                               
081900                                                                          
082000     CALL WDATKONV            USING DAT-KDDATFORM                         
082100                                    DAT-I-TIDATUM                         
082200                                    DAT-O-TIDATUM                         
082300                                    DAT-KDSVAR                            
082400                                                                          
082500     .                                                                    
082600     EJECT                                                                
082700 MFS-ERASE-FIELD-OUT SECTION.                                             
082800                                                                          
082900*    --- ALLA UTDATA-FÄLT                                                 
083000     MOVE MFS-ERASE-FIELD        TO MOD-BEART-ENG                         
083100                                    MOD-KVPB-REF                          
083200                                    MOD-KVPB-TREND-UT                     
083300                                    MOD-DAREFESC                          
083400                                    MOD-KVVECKOR-TREND-UT                 
083500                                    MOD-TIDATUM-TREND                     
083600                                    MOD-KVPB-JUST1-UT                     
083700                                    MOD-TIPBJUST-1-UT                     
083800                                    MOD-KVPB-JUST2-UT                     
083900                                    MOD-TIPBJUST-2-UT                     
084000     .                                                                    
084100     SKIP3                                                                
084200 MFS-ERASE-FIELD-IN SECTION.                                              
084300                                                                          
084400*    --- ALLA INDATA-FÄLT                                                 
084500     MOVE MFS-ERASE-FIELD        TO MOD-KDTECKEN-TREND-IN                 
084600                                    MOD-KVPB-TREND-IN                     
084700                                    MOD-KVVECKOR-TREND-IN                 
084800                                    MOD-KVPB-JUST1-IN                     
084900                                    MOD-TIPBJUST-1-IN                     
085000                                    MOD-KVPB-JUST2-IN                     
085100                                    MOD-TIPBJUST-2-IN                     
085200     .                                                                    
085300     EJECT                                                                
085400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
085500                                                                          
085600*    --- ALLA UTDATA-FÄLT                                                 
085700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART-ENG                         
085800                                    MOD-KVPB-REF                          
085900                                    MOD-KVPB-TREND-UT                     
086000                                    MOD-DAREFESC                          
086100                                    MOD-KVVECKOR-TREND-UT                 
086200                                    MOD-TIDATUM-TREND                     
086300                                    MOD-KVPB-JUST1-UT                     
086400                                    MOD-TIPBJUST-1-UT                     
086500                                    MOD-KVPB-JUST2-UT                     
086600                                    MOD-TIPBJUST-2-UT                     
086700     .                                                                    
086800     SKIP3                                                                
086900 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
087000                                                                          
087100*    --- ALLA INDATA-FÄLT                                                 
087200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDTECKEN-TREND-IN                 
087300                                    MOD-KVPB-TREND-IN                     
087400                                    MOD-KVVECKOR-TREND-IN                 
087500                                    MOD-KVPB-JUST1-IN                     
087600                                    MOD-TIPBJUST-1-IN                     
087700                                    MOD-KVPB-JUST2-IN                     
087800                                    MOD-TIPBJUST-2-IN                     
087900     .                                                                    
088000     EJECT                                                                
088100* --- IMS SECTIONS ---                                                    
088200     SKIP3                                                                
088300 IMS-GET-MSG SECTION.                                                     
088400                                                                          
088500     MOVE '  QC'                 TO GOOD-STATUSCODES                      
088600     CALL CBLTDLI             USING GU                                    
088700                                    MSG-PCB                               
088800                                    MSG-IO-AREA                           
088900     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
089000     PERFORM IMS-STATUSCHECK                                              
089100     .                                                                    
089200     SKIP3                                                                
089300 IMS-INSERT-MSG SECTION.                                                  
089400                                                                          
089500*    IF MSGI-IDLAND-SPR = 'SE'                                            
089600*      MOVE '0'                  TO MFS-KDHUVOMR                          
089700*    END-IF                                                               
089800     MOVE LOW-VALUE              TO MSG-KDZ1                              
089900                                    MSG-KDZ2                              
090000     MOVE SPACE                  TO GOOD-STATUSCODES                      
090100     CALL CBLTDLI             USING ISRT                                  
090200                                    MSG-PCB                               
090300                                    MSG-IO-AREA                           
090400                                    MFS-IDMOD                             
090500     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
090600     PERFORM IMS-STATUSCHECK                                              
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-GU-WDB601 SECTION.                                                   
091000                                                                          
091100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
091200             DELIMITED BY SIZE INTO SSA1                                  
091300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
091400     CALL CBLTDLI             USING GU                                    
091500                                    WDB6-PCB                              
091600                                    DLI-IO-WDB601                         
091700                                    SSA1                                  
091800     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
091900     PERFORM IMS-STATUSCHECK                                              
092000     .                                                                    
092100     EJECT                                                                
092200 IMS-GU-WDK601 SECTION.                                                   
092300                                                                          
092400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092500             DELIMITED BY SIZE INTO SSA1                                  
092600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
092700     CALL CBLTDLI             USING GU                                    
092800                                    WDK6-PCB                              
092900                                    DLI-IO-WDK601                         
093000                                    SSA1                                  
093100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
093200     PERFORM IMS-STATUSCHECK                                              
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GU-WDK711 SECTION.                                                   
093600                                                                          
093700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
093800             DELIMITED BY SIZE INTO SSA1                                  
093900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
094000             DELIMITED BY SIZE INTO SSA2                                  
094100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
094200     CALL CBLTDLI             USING GU                                    
094300                                    WDK7-PCB                              
094400                                    DLI-IO-WDK711                         
094500                                    SSA1 SSA2                             
094600     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
094700     PERFORM IMS-STATUSCHECK                                              
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-GHU-WDK711 SECTION.                                                  
095100                                                                          
095200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
095300             DELIMITED BY SIZE INTO SSA1                                  
095400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
095500             DELIMITED BY SIZE INTO SSA2                                  
095600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
095700     CALL CBLTDLI             USING GHU                                   
095800                                    WDK7-PCB                              
095900                                    DLI-IO-WDK711                         
096000                                    SSA1 SSA2                             
096100     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
096200     PERFORM IMS-STATUSCHECK                                              
096300     .                                                                    
096400     EJECT                                                                
096500 IMS-GU-WDK722 SECTION.                                                   
096600                                                                          
096700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
096800             DELIMITED BY SIZE INTO SSA1                                  
096900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
097000             DELIMITED BY SIZE INTO SSA2                                  
097100     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
097200             DELIMITED BY SIZE INTO SSA3                                  
097300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
097400     CALL CBLTDLI             USING GU                                    
097500                                    WDK7-PCB                              
097600                                    DLI-IO-WDK722                         
097700                                    SSA1 SSA2 SSA3                        
097800     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
097900     PERFORM IMS-STATUSCHECK                                              
098000     .                                                                    
098100     SKIP3                                                                
098200 IMS-GHU-WDK722 SECTION.                                                  
098300                                                                          
098400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
098500             DELIMITED BY SIZE INTO SSA1                                  
098600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
098700             DELIMITED BY SIZE INTO SSA2                                  
098800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
098900             DELIMITED BY SIZE INTO SSA3                                  
099000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
099100     CALL CBLTDLI             USING GHU                                   
099200                                    WDK7-PCB                              
099300                                    DLI-IO-WDK722                         
099400                                    SSA1 SSA2 SSA3                        
099500     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
099600     PERFORM IMS-STATUSCHECK                                              
099700     .                                                                    
099800     SKIP3                                                                
099900 IMS-REPL-WDK722 SECTION.                                                 
100000                                                                          
100100     MOVE '  '                   TO GOOD-STATUSCODES                      
100200     CALL CBLTDLI             USING REPL                                  
100300                                    WDK7-PCB                              
100400                                    DLI-IO-WDK722                         
100500     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
100600     PERFORM IMS-STATUSCHECK                                              
100700     .                                                                    
100800     SKIP3                                                                
100900 IMS-GU-WDD3B SECTION.                                                    
101000                                                                          
101100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
101200             DELIMITED BY SIZE INTO SSA1                                  
101300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
101400             DELIMITED BY SIZE INTO SSA2                                  
101500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
101600     CALL CBLTDLI             USING GU                                    
101700                                    WDD3B-PCB                             
101800                                    DLI-IO-WDD311                         
101900                                    SSA1 SSA2                             
102000     MOVE WDD3B-STATUS-CODE       TO STATUS-WS                            
102100     PERFORM IMS-STATUSCHECK                                              
102200     .                                                                    
102300     EJECT                                                                
103800 IMS-STATUSCHECK SECTION.                                                 
103900                                                                          
104000     SET STATUS-IX               TO 1                                     
104100     SEARCH GOOD-STATUS                                                   
104200       AT END                                                             
104300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
104400             DELIMITED BY SIZE INTO ERROR-TEXT                            
104500         CALL FELLOG                                                      
104600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
104700         CONTINUE                                                         
104800     END-SEARCH                                                           
104900     .                                                                    
105000     EJECT                                                                
105100*    -COPY WY2000P3                                                       
105200     EJECT                                                                
