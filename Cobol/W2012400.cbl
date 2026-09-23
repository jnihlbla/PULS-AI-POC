000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2012400.                                                
000300 AUTHOR.         HAMMARIN BO.                                             
000400 DATE-WRITTEN.   APRIL 2008.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        E-TRACKER 5929265                                                
000900*                                                                         
001000*        PGM                                                              
001100*        - ADMINISTRATES TEMPORARY CHANGES OF REPLACEMENT RULES IN        
001200*                                                                         
001300*        THE PROGRAM UPDATES   WDD7                                       
001400*        THE PROGRAM READS     WDK6                                       
001500*        THE PROGRAM READS     WDK7                                       
001600*        THE PROGRAM READS     WDB6                                       
001700*        THE PROGRAM READS     WDL2                                       
001800*        THE PROGRAM READS     WDR5                                       
001900*                                                                         
002000*    INPUT.                                                               
002100*        TRANSACTION: W2T124                                              
002200*                     W2T124U                                             
002300*        MID:         W2I12401                                            
002400*                                                                         
002500*    OUTPUT.                                                              
002600*        MOD:         W2O12401                                            
002700*                                                                         
002800*                                                                         
002900*    2012-01-03 E'TRACKER: 10143271 CHINA WAREHOUSE PROJECT-1             
003000*                                                                         
003100                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W2012400'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  YES                         PIC X     VALUE 'J'.                     
004300 77  NOO                         PIC X     VALUE 'N'.                     
004400                                                                          
004500*    --- INDEX FOR SCROLL LINES                                           
004600 77  INDX                        PIC S9(4) VALUE +0   COMP SYNC.          
004700 77  MAX-INDX                    PIC S9(4) VALUE +8   COMP SYNC.          
004800 77  MAX-INDX-LOGG               PIC S9(4) VALUE +14  COMP SYNC.          
004900                                                                          
005000*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005100 77  INPUT-SW                    PIC X     VALUE 'J'.                     
005200     88  INPUT-OK                          VALUE 'J'.                     
005300     88  INPUT-WRONG                       VALUE 'N'.                     
005400                                                                          
005500 77  KEYS-SW                     PIC X     VALUE 'J'.                     
005600     88  KEYS-OK                           VALUE 'J'.                     
005700     88  KEYS-WRONG                        VALUE 'N'.                     
005800                                                                          
005900 77  BASICDATA-SW                PIC X     VALUE 'J'.                     
006000     88  BASICDATA-OK                      VALUE 'J'.                     
006100     88  BASICDATA-WRONG                   VALUE 'N'.                     
006200                                                                          
006300 77  SCROLLDATA-SW               PIC X     VALUE 'J'.                     
006400     88  SCROLLDATA-EXIST                  VALUE 'J'.                     
006500     88  SCROLLDATA-MISSING                VALUE 'N'.                     
006600                                                                          
006700 77  PART-EXIST-K7-SW            PIC X     VALUE 'J'.                     
006800     88  PART-EXIST-K7                     VALUE 'J'.                     
006900                                                                          
007000 77  DC-MISSING-K7-SW            PIC X     VALUE 'N'.                     
007100     88  DC-EXIST-K7                       VALUE 'N'.                     
007200     88  DC-MISSING-K7                     VALUE 'J'.                     
007300                                                                          
007400 77  PART-EXIST-D7-SW            PIC X     VALUE 'J'.                     
007500     88  PART-EXIST-D7                     VALUE 'J'.                     
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)  VALUE SPACE.                   
007800     88  OWN-MID                           VALUE '2124'.                  
007900     88  GOOD-MID                          VALUE '2121' '2122'            
008000                                                 '2123' '2124'            
008100                                                 '2125' '2126'            
008200                                                 '2127' '2128'            
008300                                                 '2129'.                  
008400     88  HELP-MID                          VALUE '0551'.                  
008500     EJECT                                                                
008600                                                                          
008700*    --- MISCELLANEOUS WORK FIELDS                                        
008800 77  SPAR-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.             
008900 77  SPAR-IDDC                   PIC  X(2)        VALUE SPACE.            
009000                                                                          
009100 77  WS-KVRO                     PIC S9(7) COMP-3 VALUE ZERO.             
009200 77  WS-SUROBEL                  PIC S9(7) COMP-3 VALUE ZERO.             
009300 77  WS-KVBEART                  PIC S9(7) COMP-3 VALUE ZERO.             
009310 77  WS-KVAKS                    PIC S9(7) COMP-3 VALUE ZERO.             
009400 77  WS-KVAKS-CDC                PIC S9(7) COMP-3 VALUE ZERO.             
009500 77  WS-KVAKS-XDC                PIC S9(7) COMP-3 VALUE ZERO.             
009600 77  WS-SUAKSV                   PIC S9(7) COMP-3 VALUE ZERO.             
009700 77  WS-SULV                     PIC S9(9) COMP-3 VALUE ZERO.             
009800 77  WS-SULV-TOT                 PIC S9(9) COMP-3 VALUE ZERO.             
009900                                                                          
010000 01  WS-TODAYS-DATE.                                                      
010100     03  FILLER                  PIC 9(2)  VALUE 20.                      
010200     03  WS-TODAYS-DATE-6        PIC 9(6).                                
010300 01  WS-DATE.                                                             
010400     03  WS-DATE-8               PIC 9(8).                                
010500 01  WS-DATE-DISP.                                                        
010600     03  WS-DATE-8-DISP          PIC 9(8).                                
010700 01  W-DATE.                                                              
010800     03  W-FILLER                PIC X(2).                                
010900     03  W-DATE-6                PIC 9(6).                                
011000 01  WS-TIAVIDAT                 PIC 9(6).                                
011100 01  WS-TIME.                                                             
011200     03  WS-TIME-9               PIC S9(9) COMP-3.                        
011300     03  WS-TIME-8               PIC 9(8).                                
011400     EJECT                                                                
011500                                                                          
011600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011700 01  GENERAL-SUBPROGRAMS.                                                 
011800     03  WMEDKONV                PIC X(8)  VALUE 'WMEDKONV'.              
011900     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
012000     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
012100     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
012200     03  WDATKONV                PIC X(8)  VALUE 'WDATKONV'.              
012300     EJECT                                                                
012400                                                                          
012500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
012600*01 -COPY WMEDAREA                                                        
012700                                                                          
012800 01  MESSAGE-CODES.                                                       
012900     03  ERR-PART-MISSING        PIC X(3)  VALUE '017'.                   
013000     03  ERR-CORR-HILITE-FLDS    PIC X(3)  VALUE '001'.                   
013100     03  INF-PRESS-PF11          PIC X(3)  VALUE '003'.                   
013200     03  ERR-PF11-AND-NO-DATA    PIC X(3)  VALUE '011'.                   
013300     03  INF-UPDATE-DONE         PIC X(3)  VALUE '101'.                   
013400     03  INF-FIRST-PAGE          PIC X(3)  VALUE '006'.                   
013500     03  INF-MORE-INFO-EXISTS    PIC X(3)  VALUE '105'.                   
013600     03  ERR-WRONG-KEY           PIC X(3)  VALUE '401'.                   
013610     03  INF-REFILL-PART         PIC X(3)  VALUE '434'.                   
013700     EJECT                                                                
013800                                                                          
013900*01  -COPY WDATAREA                                                       
014000     EJECT                                                                
014100                                                                          
014200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
014300*                                                                         
014400 01  FILLER                      PIC X(16) VALUE 'WMSGINIT'.              
014500                                                                          
014600*01 -COPY WMSGINIT                                                        
014700     EJECT                                                                
014800                                                                          
014900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
015000*                                                                         
015100 01  SAVE-AREA.                                                           
015200     03  SAVE-IDTRANS            PIC X(4)  VALUE '2124'.                  
015300     03  SAVE-IDDC-ENTER         PIC X(2).                                
015400     03  SAVE-IDDC-NEXT          PIC X(2).                                
015500     03  SAVE-IDARTNR-ENTER      PIC X(9).                                
015600     03  SAVE-IDARTNR-NEXT       PIC X(9).                                
015700     03  SAVE-BEKOM              PIC X(50).                               
015800     EJECT                                                                
015900                                                                          
016000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
016100*                                                                         
016200 01  FILLER                      PIC X(16) VALUE 'MID-AREA'.              
016300                                                                          
016400*01  MID -COPY W2I12401                                                   
016500     EJECT                                                                
016600                                                                          
016700 01  FILLER                      PIC X(16) VALUE 'MSG/MOD-AREA'.          
016800                                                                          
016900*01  -COPY WMSGAREA                                                       
017000     EJECT                                                                
017100                                                                          
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W2O12401                                                 
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC X(16) VALUE 'MFS-AREA'.              
017700                                                                          
017800*01  -COPY WMFSAREA                                                       
017900     EJECT                                                                
018000                                                                          
018100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
018200*                                                                         
018300 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
018400                                                                          
018500 01  KEYS-TO-DLI.                                                         
018600*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
018700     03  W-IDDC-MIN-X.                                                    
018800         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
018900     03  W-IDARTNR-X.                                                     
019000         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
019100     03  W-IDDC-X.                                                        
019200         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
019300     03  W-WDGXKEY-X.                                                     
019400         05  FILLER              PIC X(4)  VALUE '6327'.                  
019500         05  W-KDARBTYP-6327     PIC X(8)  VALUE 'ERS'.                   
019600         05  W-IDDC-6327         PIC X(2)  VALUE '11'.                    
019700         05  FILLER              PIC X(16) VALUE LOW-VALUE.               
019800     03  W-IDUSER-GODK-X.                                                 
019900         05  W-IDUSER-GODK       PIC X(8)  VALUE SPACE.                   
020000     03  W-IDPTYP-X.                                                      
020100       05  W-IDPTYP              PIC X(3)   VALUE '310'.                  
020200     03  W-WDD713KY-X.                                                    
020300         05  W-DAREGDAT          PIC 9(8)         VALUE 0.                
020400         05  W-TIKLOCK           PIC S9(9) COMP-3 VALUE 0.                
020500                                                                          
020600*    --- STATUS-CODE FROM IMS                                             
020700 01  STATUS-WS                   PIC XX.                                  
020800     88  SEGMENT-FOUND                     VALUE '  '.                    
020900     88  SEGMENT-MISSING                   VALUE 'GE'.                    
021000     88  END-OF-BASE                       VALUE 'GB'.                    
021100                                                                          
021200 01  GOOD-STATUSCODES.                                                    
021300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021400                                                                          
021500 01  SSA1                        PIC X(128).                              
021600 01  SSA2                        PIC X(128).                              
021700     EJECT                                                                
021800                                                                          
021900*    --- IMS FUNCTION CODES                                               
022000*01  -COPY W0003                                                          
022100     EJECT                                                                
022200                                                                          
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400                                                                          
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022600 01  DLI-IO-WDK601.                                                       
022700*    03  -COPY WDK601                                                     
022800                                                                          
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
023000 01  DLI-IO-WDK611.                                                       
023100*    03  -COPY WDK611                                                     
023200     EJECT                                                                
023300                                                                          
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
023500 01  DLI-IO-WDK701.                                                       
023600*    03  -COPY WDK701                                                     
023700                                                                          
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
023900 01  DLI-IO-WDK711.                                                       
024000*    03  -COPY WDK711                                                     
024100     EJECT                                                                
024200                                                                          
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
024400 01  DLI-IO-WDB601.                                                       
024500*    03  -COPY WDB601                                                     
024600     EJECT                                                                
024700                                                                          
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
024900 01  DLI-IO-WDL201.                                                       
025000*    03  -COPY WDL201                                                     
025100                                                                          
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
025300 01  DLI-IO-WDL211.                                                       
025400*    03  -COPY WDL211                                                     
025500                                                                          
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
025700 01  DLI-IO-WDL221.                                                       
025800*    03  -COPY WDL221                                                     
025900     EJECT                                                                
026000                                                                          
026100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501'.                      
026200 01  DLI-IO-WDR501.                                                       
026300*    03  -COPY WDGX6327                                                   
026400                                                                          
026500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX6328'.                    
026600 01  DLI-IO-WDGX6328.                                                     
026700*    03  -COPY WDGX6328                                                   
026800     EJECT                                                                
026900                                                                          
027000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
027100 01  DLI-IO-WDD701.                                                       
027200*    03  -COPY WDD701                                                     
027300                                                                          
027400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD713'.                      
027500 01  DLI-IO-WDD713.                                                       
027600*    03  -COPY WDD713                                                     
027700                                                                          
027800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD714'.                      
027900 01  DLI-IO-WDD714.                                                       
028000*    03  -COPY WDD714                                                     
028100     EJECT                                                                
028200                                                                          
028300 LINKAGE SECTION.                                                         
028400*01  -COPY W0009   -PRE MSG-                                              
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01  -COPY W0008   -PRE WDP7-                                             
028800     05  FILLER                  PIC X.                                   
028900                                                                          
029000*01  -COPY W0008   -PRE WDK6-                                             
029100     05  FILLER                  PIC X.                                   
029200                                                                          
029300*01  -COPY W0008   -PRE WDK7-                                             
029400     05  FILLER                  PIC X.                                   
029500                                                                          
029600*01  -COPY W0008   -PRE WDB6-                                             
029700     05  FILLER                  PIC X.                                   
029800                                                                          
029900*01  -COPY W0008   -PRE WDL2-                                             
030000     05  FILLER                  PIC X.                                   
030100                                                                          
030200*01  -COPY W0008   -PRE WDR5-                                             
030300     05  FILLER                  PIC X.                                   
030400                                                                          
030500*01  -COPY W0008   -PRE WDD7-                                             
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800                                                                          
030900 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
031000                                            WDB6-PCB WDL2-PCB             
031100                                            WDR5-PCB                      
031200                                            WDD7-PCB.                     
031300 MAIN SECTION.                                                            
031400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
031500                                            WDB6-PCB WDL2-PCB             
031600                                            WDR5-PCB                      
031700                                            WDD7-PCB.                     
031800                                                                          
031900     PERFORM IMS-GET-MSG                                                  
032000     IF SEGMENT-FOUND                                                     
032100       PERFORM A-INIT                                                     
032200       PERFORM B-CHECK-KEYS                                               
032300       IF KEYS-OK                                                         
032400         IF MFS-UPDATE                                                    
032500           PERFORM G-CHECK-INPUT                                          
032600           IF INPUT-OK                                                    
032700             PERFORM H-UPDATE                                             
032800           END-IF                                                         
032900         ELSE                                                             
033000           IF MFS-FIRST                                                   
033100             PERFORM C-FIRST-PAGE                                         
033200           ELSE                                                           
033300             IF MFS-NEXT                                                  
033400               PERFORM D-NEXT-PAGE                                        
033500             ELSE                                                         
033600               PERFORM E-SAME-PAGE                                        
033700             END-IF                                                       
033800           END-IF                                                         
033900         END-IF                                                           
034000         PERFORM F-READ-SHOW-INFO                                         
034100       END-IF                                                             
034200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O12401 + 4                      
034300       PERFORM IMS-INSERT-MSG                                             
034400     END-IF                                                               
034500                                                                          
034600     MOVE ZERO TO RETURN-CODE                                             
034700     GOBACK                                                               
034800     .                                                                    
034900     EJECT                                                                
035000                                                                          
035100 A-INIT SECTION.                                                          
035200     ACCEPT WS-TODAYS-DATE-6            FROM DATE                         
035300     MOVE   WS-TODAYS-DATE              TO WS-DATE-8                      
035400                                                                          
035500     IF MSG-DOUBLE-TRANSACTIONS                                           
035600       MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W2I12401                   
035700       MOVE MSG-IDTRANS-2               TO MFS-IDTRANS                    
035800       MOVE MSG-KDMFSFOR-2              TO MFS-KDMFSFOR                   
035900     ELSE                                                                 
036000       MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W2I12401                   
036100       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
036200       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
036300     END-IF                                                               
036400                                                                          
036500     MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                    
036600     MOVE MSG-IDPFK                     TO MFS-IDPFK                      
036700     MOVE MFS-IDTRANS                   TO W-IDTRANS                      
036800                                                                          
036900     MOVE LOW-VALUE                     TO MSG-AREA                       
037000     MOVE 'W2O124N1'                    TO MFS-IDMOD                      
037100     MOVE '2124'                        TO MOD-IDTRANS                    
037200     MOVE MFS-ERASE-FIELD               TO MOD-TEMFSFEL                   
037300                                           MOD-TEMFSINF                   
037400                                                                          
037500     IF OWN-MID OR HELP-MID                                               
037600       CONTINUE                                                           
037700     ELSE                                                                 
037800       MOVE SPACE                       TO MFS-KDTRTYP                    
037900       MOVE '7'                         TO MFS-IDPFK                      
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400 B-CHECK-KEYS SECTION.                                                    
038500     MOVE ALL '+'                       TO MSGI-WMSGINIT                  
038600     MOVE '001'                         TO MSGI-KDCALL                    
038700     MOVE MSG-LTERM-NAME                TO MSGI-IDLTERM-USER              
038800     MOVE MSG-SIGNON-USERID             TO MSGI-IDUSER                    
038900     MOVE '2124'                        TO MSGI-IDTRANS                   
039000     MOVE MID-IDARTNR-IN                TO MSGI-IDARTNR                   
039100                                                                          
039200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
039300     MOVE MSGI-SPAR-AREA                TO SAVE-AREA                      
039400                                                                          
039500*    - LANGUAGE TO BE USED BY MEDKONV                                     
039600     MOVE 'GB'                          TO MED-IDSKYLT                    
039700                                                                          
039800     MOVE YES                           TO KEYS-SW                        
039900                                                                          
040000*    -- CHECK OF IDARTNR                                                  
040100     MOVE MFS-ERASE-FIELD               TO MOD-IDARTNR-IN                 
040200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
040300       MOVE '7'                         TO MFS-IDPFK                      
040400       MOVE SPACE                       TO MFS-KDTRTYP                    
040500     END-IF                                                               
040600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
040700     IF MSGI-IDARTNR NUMERIC                                              
040800       MOVE MSGI-IDARTNR                TO W-IDARTNR                      
040900     ELSE                                                                 
041000       MOVE NOO                         TO KEYS-SW                        
041100     END-IF                                                               
041200                                                                          
041300     IF GOOD-MID OR KEYS-OK                                               
041400       MOVE W-IDARTNR                   TO MOD-IDARTNR-UT                 
041500     ELSE                                                                 
041600       MOVE MFS-ERASE-FIELD             TO MOD-IDARTNR-UT                 
041700     END-IF                                                               
041800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
041900                                                                          
042000     IF KEYS-WRONG                                                        
042100       MOVE ERR-WRONG-KEY               TO MED-IDMFSFEL                   
042200       CALL WMEDKONV USING MED-WMEDAREA                                   
042300       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
042400       PERFORM MFS-ERASE-FIELD-IN                                         
042500       PERFORM MFS-ERASE-FIELD-OUT                                        
042600     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000 C-FIRST-PAGE SECTION.                                                    
043100     MOVE SPACE                TO SAVE-BEKOM                              
043200     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
043300     CALL WMEDKONV USING MED-WMEDAREA                                     
043400     MOVE MED-MFSINF           TO MOD-TEMFSFEL                            
043500     PERFORM MFS-ERASE-FIELD-IN                                           
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 D-NEXT-PAGE SECTION.                                                     
044000     MOVE SPACE                TO SAVE-BEKOM                              
044100     IF SAVE-IDTRANS = '2124'                                             
044200       MOVE SAVE-IDDC-NEXT     TO W-IDDC-MIN                              
044300       INSPECT SAVE-IDARTNR-NEXT REPLACING LEADING SPACE BY ZERO          
044400       MOVE SAVE-IDARTNR-NEXT  TO W-IDARTNR                               
044500     ELSE                                                                 
044600       PERFORM MFS-ERASE-FIELD-IN                                         
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 E-SAME-PAGE SECTION.                                                     
045200     IF SAVE-IDTRANS = '2124' OR '0551'                                   
045300       MOVE SAVE-IDDC-ENTER    TO W-IDDC-MIN                              
045400       MOVE MID-IDARTNR-UT     TO SAVE-IDARTNR-ENTER                      
045500       INSPECT SAVE-IDARTNR-ENTER REPLACING LEADING SPACE BY ZERO         
045600       MOVE SAVE-IDARTNR-ENTER TO W-IDARTNR                               
045700       IF (MID-KDERSTMP   = ALL '+' OR SPACE) AND                         
045800          (MID-BEKOM      = ALL '+' OR SPACE)                             
045900         PERFORM MFS-ERASE-FIELD-IN                                       
046000         MOVE SPACE            TO SAVE-BEKOM                              
046100       ELSE                                                               
046200         MOVE INF-PRESS-PF11   TO MED-IDMFSINF                            
046300         CALL WMEDKONV USING MED-WMEDAREA                                 
046400         MOVE MED-MFSINF       TO MOD-TEMFSFEL                            
046500         PERFORM EA-MID-INPUT-TO-MOD                                      
046600         MOVE MID-BEKOM        TO SAVE-BEKOM                              
046700       END-IF                                                             
046800     ELSE                                                                 
046900       PERFORM MFS-ERASE-FIELD-IN                                         
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300                                                                          
047400 EA-MID-INPUT-TO-MOD SECTION.                                             
047500     IF MID-KDERSTMP NOT = ALL '+' AND SPACE                              
047600       MOVE MID-KDERSTMP       TO MOD-KDERSTMP                            
047700       MOVE MFS-ADD-READ-FIELD TO MOD-KDERSTMP-ATTR                       
047800     ELSE                                                                 
047900       MOVE MFS-ERASE-FIELD    TO MOD-KDERSTMP                            
048000     END-IF                                                               
048100                                                                          
048200     IF MID-BEKOM NOT = ALL '+' AND SPACE                                 
048300       MOVE MID-BEKOM          TO MOD-BEKOM                               
048400       MOVE MFS-ADD-READ-FIELD TO MOD-BEKOM-ATTR                          
048500     ELSE                                                                 
048600       MOVE MFS-ERASE-FIELD    TO MOD-BEKOM                               
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 F-READ-SHOW-INFO SECTION.                                                
049200     PERFORM FA-READ-BASICDATA                                            
049300                                                                          
049400* EDIT BASIC DATA                                                         
049500     IF BASICDATA-OK OR                                                   
049600        MED-IDMFSFEL = 'XXX'                                              
049700       MOVE CLAG-IDANSK             TO MOD-IDANSK                         
049800       MOVE CLAG-KVLS               TO MOD-KVLS                           
049900       COMPUTE WS-KVAKS-CDC = CLAG-KVAKS-PAV +                            
050000                              CLAG-KVAKS-CDC +                            
050100                              CLAG-KVAKS-T   -                            
050200                              WS-KVAKS       +                            
050210                              WS-KVBEART                                  
050300       END-COMPUTE                                                        
050400       MOVE WS-KVAKS-CDC            TO MOD-KVAKS                          
050500       MOVE WS-KVRO                 TO MOD-KVRO                           
050600       MOVE WS-SUROBEL              TO MOD-SUROBEL                        
050700       MOVE CLAG-KDERS              TO MOD-KDERS                          
050800                                                                          
050900       PERFORM IMS-GU-WDK701                                              
051000       IF SEGMENT-MISSING                                                 
051100         MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                       
051200         CALL WMEDKONV USING MED-WMEDAREA                                 
051300         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
051400         PERFORM MFS-ERASE-FIELD-OUT                                      
051500         MOVE 'N'                   TO PART-EXIST-K7-SW                   
051600       ELSE                                                               
051700                                                                          
051800         MOVE +1 TO INDX                                                  
051900         PERFORM FB-READ-SCROLLDATA                                       
052000         IF SCROLLDATA-EXIST                                              
052100           MOVE DCS-IDDC            TO SAVE-IDDC-ENTER                    
052200           MOVE W-IDARTNR           TO SAVE-IDARTNR-ENTER                 
052300         ELSE                                                             
052400           MOVE W-IDDC-MIN          TO SAVE-IDDC-ENTER                    
052500           MOVE W-IDARTNR           TO SAVE-IDARTNR-ENTER                 
052600         END-IF                                                           
052700                                                                          
052800* EDIT SCROLL DATA                                                        
052900         IF MED-IDMFSFEL = 'XXX'                                          
053000           MOVE 9 TO INDX                                                 
053100         END-IF                                                           
053200         PERFORM UNTIL INDX > MAX-INDX                                    
053300           IF SCROLLDATA-EXIST                                            
053400             MOVE DCS-IDDC          TO MOD-IDDC-RADER(INDX)               
053500             MOVE WS-KVAKS-XDC      TO MOD-KVAKS-PAV-RADER(INDX)          
053600             COMPUTE WS-SUAKSV = WS-KVAKS-XDC *                           
053700                                 CLAG-PRARTSTD                            
053800             END-COMPUTE                                                  
053900             MOVE WS-SUAKSV         TO MOD-SUAKSV-PAV-RADER(INDX)         
054000             IF WS-TIAVIDAT = ZERO                                        
054100               MOVE SPACE           TO MOD-TIAVIDAT-RADER(INDX)           
054200             ELSE                                                         
054300               MOVE WS-TIAVIDAT     TO MOD-TIAVIDAT-RADER(INDX)           
054400             END-IF                                                       
054500             MOVE SLAG-KVLS         TO MOD-KVLS-RADER(INDX)               
054600             COMPUTE WS-SULV = (SLAG-KVLS      +                          
054700                                SLAG-KVEFRS    +                          
054800                                SLAG-KVAKS-PAV +                          
054900                                SLAG-KVAKS-SDC +                          
055000                                WS-KVAKS-XDC)  *                          
055100                                CLAG-PRARTSTD                             
055200             END-COMPUTE                                                  
055300             MOVE WS-SULV           TO MOD-SULV-RADER(INDX)               
055400             ADD +1 TO INDX                                               
055500             PERFORM FB-READ-SCROLLDATA                                   
055600           ELSE                                                           
055700             IF DC-MISSING-K7                                             
055800               MOVE MFS-ERASE-FIELD TO MOD-IDDC-RADER(INDX)               
055900                                       MOD-KVAKS-PAV-RADER(INDX)          
056000                                       MOD-SUAKSV-PAV-RADER(INDX)         
056100                                       MOD-TIAVIDAT-RADER(INDX)           
056200                                       MOD-KVLS-RADER(INDX)               
056300                                       MOD-SULV-RADER(INDX)               
056400               ADD +1 TO INDX                                             
056500             ELSE                                                         
056600               PERFORM FB-READ-SCROLLDATA                                 
056700             END-IF                                                       
056800           END-IF                                                         
056900         END-PERFORM                                                      
057000       END-IF                                                             
057100                                                                          
057200* EDIT END-OF-PAGE DATA PART 1                                            
057300       IF PART-EXIST-K7                                                   
057400         IF DC-EXIST-K7 AND                                               
057500            MED-IDMFSFEL NOT = 'XXX'                                      
057600           MOVE 'Y'                  TO MOD-FLFORTS                       
057700         END-IF                                                           
057800                                                                          
057900* EDIT LOGG DATA                                                          
058000         MOVE YES                    TO PART-EXIST-D7-SW                  
058100         PERFORM IMS-GU-WDD701                                            
058200         IF SEGMENT-FOUND                                                 
058300           MOVE +1 TO INDX                                                
058400           PERFORM IMS-GNP-WDD713                                         
058500           PERFORM UNTIL INDX > MAX-INDX-LOGG                             
058600             IF SEGMENT-FOUND                                             
058700               COMPUTE WS-DATE-8-DISP = 99999999 -                        
058800                                        TMP-DAREGDAT-9KOMPL               
058900               END-COMPUTE                                                
059000               MOVE WS-DATE-8-DISP   TO W-DATE                            
059100               MOVE W-DATE-6         TO MOD-DAREGDAT-LOGG (INDX)          
059200               MOVE TMP-KDERSTMP     TO MOD-KDERSTMP-LOGG (INDX)          
059300               MOVE TMP-SULV         TO MOD-SULV-LOGG     (INDX)          
059400               MOVE TMP-IDUSER       TO MOD-IDUSER-LOGG   (INDX)          
059500               PERFORM IMS-GNP-WDD713                                     
059600             ELSE                                                         
059700               MOVE MFS-ERASE-FIELD  TO MOD-DAREGDAT-LOGG (INDX)          
059800                                        MOD-KDERSTMP-LOGG (INDX)          
059900                                        MOD-SULV-LOGG     (INDX)          
060000                                        MOD-IDUSER-LOGG   (INDX)          
060100             END-IF                                                       
060200             ADD +1 TO INDX                                               
060300           END-PERFORM                                                    
060400                                                                          
060500* EDIT END-OF-PAGE DATA PART 2                                            
060600           PERFORM IMS-GU-WDD701                                          
060700           PERFORM IMS-GNP-WDD714                                         
060800           IF SAVE-BEKOM > SPACE                                          
060900             MOVE SAVE-BEKOM         TO MOD-BEKOM                         
061000                                        MID-BEKOM                         
061100           ELSE                                                           
061200             IF SEGMENT-FOUND                                             
061300               MOVE NOT-BEKOM        TO MOD-BEKOM                         
061400             ELSE                                                         
061500               MOVE SPACE            TO MOD-BEKOM                         
061600             END-IF                                                       
061700           END-IF                                                         
061800           IF SEGMENT-FOUND                                               
061900             MOVE NOT-IDUSER         TO MOD-IDUSER                        
062000             MOVE NOT-DAREGDAT       TO W-DATE                            
062100             MOVE W-DATE-6           TO MOD-DAREGDAT                      
062200           ELSE                                                           
062300             MOVE SPACE              TO MOD-IDUSER                        
062400                                        MOD-DAREGDAT                      
062500           END-IF                                                         
062600         ELSE                                                             
062700           MOVE '999'                TO MED-IDMFSFEL                      
062800           MOVE 'SUPERSESSION INFO MISSING'                               
062900                                     TO MOD-TEMFSFEL                      
063000           PERFORM MFS-ERASE-FIELD-OUT                                    
063100           MOVE NOO                  TO PART-EXIST-D7-SW                  
063200         END-IF                                                           
063300       END-IF                                                             
063400                                                                          
063500* EDIT END-OF-PAGE DATA PART 3                                            
063600       IF MED-IDMFSFEL NOT = 'XXX'                                        
063700         MOVE DCS-IDDC               TO SPAR-IDDC                         
063800         IF PART-EXIST-K7                                                 
063900           PERFORM IMS-GU-WDK701                                          
064000           PERFORM S10-ACCUMULATE-VALUE                                   
064100           MOVE WS-SULV-TOT          TO MOD-SULV-TOT                      
064200         END-IF                                                           
064300       END-IF                                                             
064400                                                                          
064500       IF INPUT-OK AND PART-EXIST-D7 AND PART-EXIST-K7                    
064600         IF MOD-FLFORTS = 'Y' AND NOT                                     
064700            MFS-UPDATE                                                    
064800           MOVE SPAR-IDDC            TO SAVE-IDDC-NEXT                    
064900           MOVE W-IDARTNR            TO SAVE-IDARTNR-NEXT                 
065000           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
065100           CALL WMEDKONV USING MED-WMEDAREA                               
065200           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
065300         ELSE                                                             
065400           MOVE SPACE                TO SAVE-IDDC-NEXT                    
065500                                        SAVE-IDARTNR-NEXT                 
065600         END-IF                                                           
065700                                                                          
065800         MOVE '002'                  TO MSGI-KDCALL                       
065900         MOVE '2124'                 TO SAVE-IDTRANS                      
066000         MOVE SAVE-AREA              TO MSGI-SPAR-AREA                    
066100         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
066200       END-IF                                                             
066300     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 FA-READ-BASICDATA SECTION.                                               
066800     MOVE YES                        TO BASICDATA-SW                      
066900     MOVE ZERO                       TO WS-KVRO                           
067000                                        WS-KVAKS                          
067010                                        WS-KVBEART                        
067100                                                                          
067200     PERFORM IMS-GU-WDK601                                                
067300     IF SEGMENT-MISSING                                                   
067400       MOVE ERR-PART-MISSING         TO MED-IDMFSFEL                      
067500       CALL WMEDKONV USING MED-WMEDAREA                                   
067600       MOVE MED-MFSFEL               TO MOD-TEMFSFEL                      
067700       PERFORM MFS-ERASE-FIELD-OUT                                        
067800       MOVE NOO                      TO BASICDATA-SW                      
067900     ELSE                                                                 
068000       PERFORM IMS-GNP-WDK611                                             
068100       IF SEGMENT-FOUND                                                   
068110          IF CLAG-IDDC-REF NOT = SPACE                                    
068111             IF MED-IDMFSINF NOT = '101'                                  
068120                MOVE INF-REFILL-PART  TO MED-IDMFSINF                     
068130                CALL WMEDKONV     USING MED-WMEDAREA                      
068140                MOVE MED-MFSINF       TO MOD-TEMFSINF                     
068141             END-IF                                                       
068150          END-IF                                                          
068160                                                                          
068200          IF CLAG-KDERS NOT = 01 AND 04 AND 07                            
068300            MOVE 'XXX'                  TO MED-IDMFSFEL                   
068400            MOVE 'WRONG SUPERSESSION CODE'                                
068500                                        TO MOD-TEMFSFEL                   
068600            PERFORM MFS-ERASE-FIELD-OUT                                   
068700            MOVE NOO                    TO BASICDATA-SW                   
068800          END-IF                                                          
068900          MOVE CLAG-KVROS               TO WS-KVRO                        
069000          COMPUTE WS-SUROBEL = WS-KVRO       *                            
069100                                CLAG-PRARTSTD                             
069200          END-COMPUTE                                                     
069300          IF WS-KVRO = ZERO                                               
069400            MOVE 'XXX'                  TO MED-IDMFSFEL                   
069500            MOVE 'NO BACKORDER'         TO MOD-TEMFSFEL                   
069600            PERFORM MFS-ERASE-FIELD-OUT                                   
069700            MOVE NOO                    TO BASICDATA-SW                   
069800          END-IF                                                          
069801                                                                          
069810          IF CLAG-IDDC-REF NOT = SPACE                                    
069820             MOVE CLAG-KVBEART          TO WS-KVBEART                     
069830          END-IF                                                          
069840                                                                          
069900       ELSE                                                               
070000          MOVE 'XXX'                    TO MED-IDMFSFEL                   
070100          MOVE 'NO BACKORDER'           TO MOD-TEMFSFEL                   
070200          PERFORM MFS-ERASE-FIELD-OUT                                     
070300          MOVE NOO                      TO BASICDATA-SW                   
070400       END-IF                                                             
070500     END-IF                                                               
070600                                                                          
070700     IF BASICDATA-OK                                                      
070800       PERFORM IMS-GU-WDL201                                              
070900       IF SEGMENT-FOUND                                                   
071000         PERFORM IMS-GNP-WDL211                                           
071100         IF SEGMENT-FOUND                                                 
071200           PERFORM UNTIL SEGMENT-MISSING                                  
071300             PERFORM IMS-GNP-WDL221                                       
071400             IF SEGMENT-FOUND                                             
071500               PERFORM UNTIL SEGMENT-MISSING                              
071600                 IF MOT-IDDC = '11' AND                                   
071700                    MOT-KDRT = 7                                          
071800                   IF MOT-KVAVIS > 0                                      
071900                     COMPUTE WS-KVAKS = WS-KVAKS +                        
072000                                       (MOT-KVAVIS -                      
072100                                        MOT-KVANTMOT)                     
072300                     END-COMPUTE                                          
072301                   END-IF                                                 
072306                 END-IF                                                   
072500                 PERFORM IMS-GNP-WDL221                                   
072600               END-PERFORM                                                
072700             END-IF                                                       
072800             PERFORM IMS-GNP-WDL211                                       
072900           END-PERFORM                                                    
073000         END-IF                                                           
073100       END-IF                                                             
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500                                                                          
073600 FB-READ-SCROLLDATA SECTION.                                              
073700     MOVE NOO                TO SCROLLDATA-SW                             
073800     MOVE ZERO               TO WS-KVAKS-XDC                              
073900     MOVE NOO                TO DC-MISSING-K7-SW                          
074000                                                                          
074100     PERFORM IMS-GNP-WDK711-QUAL                                          
074200     IF SEGMENT-FOUND                                                     
074300       MOVE SLAG-IDDC        TO W-IDDC                                    
074400       PERFORM IMS-GU-WDB601                                              
074510       IF DCS-KDDC = 'S'                                                  
074600         PERFORM IMS-GU-WDL201                                            
074700         IF SEGMENT-FOUND                                                 
074800           MOVE ZERO         TO WS-TIAVIDAT                               
074900           PERFORM IMS-GNP-WDL221                                         
075000           PERFORM UNTIL SEGMENT-MISSING                                  
075100             IF MOT-IDDC    = '11'           AND                          
075200                MOT-IDPTYP  = '310'          AND                          
075300                MOT-IDLEVNR = DCS-IDLEVNR-DC AND                          
075400                MOT-KDRT    = 8                                           
075500               IF MOT-KVAVIS > 0                                          
075600                 COMPUTE WS-KVAKS-XDC = WS-KVAKS-XDC +                    
075700                                       (MOT-KVAVIS -                      
075800                                        MOT-KVANTMOT)                     
075900                 END-COMPUTE                                              
076000               END-IF                                                     
076100               IF MOT-TIAVIDAT > WS-TIAVIDAT                              
076200                 MOVE MOT-TIAVIDAT                                        
076300                             TO WS-TIAVIDAT                               
076400               END-IF                                                     
076500             END-IF                                                       
076600             PERFORM IMS-GNP-WDL221                                       
076700           END-PERFORM                                                    
076800         END-IF                                                           
076900                                                                          
077000         IF WS-KVAKS-XDC   > ZERO OR                                      
077100            SLAG-KVLS      > ZERO OR                                      
077200            SLAG-KVEFRS    > ZERO OR                                      
077300            SLAG-KVAKS-PAV > ZERO OR                                      
077400            SLAG-KVAKS-SDC > ZERO                                         
077500           MOVE YES          TO SCROLLDATA-SW                             
077600         END-IF                                                           
077700       END-IF                                                             
077800     ELSE                                                                 
077900       MOVE YES              TO DC-MISSING-K7-SW                          
078000     END-IF                                                               
078100     .                                                                    
078200     EJECT                                                                
078300                                                                          
078400 G-CHECK-INPUT SECTION.                                                   
078500     MOVE YES                         TO INPUT-SW                         
078600                                                                          
078700     IF SAVE-BEKOM > SPACE                                                
078800       MOVE SAVE-BEKOM TO MID-BEKOM                                       
078900     END-IF                                                               
079000                                                                          
079100     IF (MID-KDERSTMP = ALL '+' OR SPACE) AND                             
079200        (MID-BEKOM    = ALL '+' OR SPACE)                                 
079300       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
079400       CALL WMEDKONV USING MED-WMEDAREA                                   
079500       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
079600       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
079700       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
079800       MOVE NOO                       TO INPUT-SW                         
079900     ELSE                                                                 
080000                                                                          
080100       IF MID-KDERSTMP NOT = ALL '+' AND SPACE                            
080200         IF MID-KDERSTMP NOT = 'A' AND 'D' AND 'B'                        
080300           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDERSTMP-ATTR                
080400           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
080500           CALL WMEDKONV USING MED-WMEDAREA                               
080600           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
080700           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
080800           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
080900           MOVE NOO                   TO INPUT-SW                         
081000         END-IF                                                           
081100       END-IF                                                             
081200     END-IF                                                               
081300                                                                          
081400     IF INPUT-WRONG                                                       
081500       CONTINUE                                                           
081600     ELSE                                                                 
081700       PERFORM GB-LOGICAL-CONTROL-2                                       
081800       IF INPUT-OK                                                        
081900         PERFORM GA-LOGICAL-CONTROL-1                                     
082000       END-IF                                                             
082100     END-IF                                                               
082200                                                                          
082300     IF INPUT-WRONG                                                       
082400       MOVE MID-BEKOM                 TO SAVE-BEKOM                       
082500     ELSE                                                                 
082600       MOVE SPACE                     TO SAVE-BEKOM                       
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000                                                                          
083100 GA-LOGICAL-CONTROL-1 SECTION.                                            
083200     MOVE W-IDARTNR                      TO SPAR-IDARTNR                  
083300     PERFORM IMS-GU-WDK601                                                
083400     PERFORM IMS-GNP-WDK611                                               
083500     PERFORM IMS-GU-WDK701                                                
083600     PERFORM S10-ACCUMULATE-VALUE                                         
083700                                                                          
083800     IF MID-KDERSTMP = 'A'                                                
083900       MOVE '11'                         TO W-IDDC                        
084000       PERFORM IMS-GU-WDR501                                              
084100       IF SEGMENT-MISSING                                                 
084200         MOVE NOO                        TO INPUT-SW                      
084300       ELSE                                                               
084400         MOVE MSGI-IDUSER                TO W-IDUSER-GODK                 
084500         PERFORM IMS-GNP-WDGX6328                                         
084600         IF SEGMENT-MISSING                                               
084700           MOVE NOO                      TO INPUT-SW                      
084800         ELSE                                                             
084900           IF WS-SULV-TOT > 6328-SUBEL                                    
085000             MOVE NOO                    TO INPUT-SW                      
085100           END-IF                                                         
085200         END-IF                                                           
085300       END-IF                                                             
085400                                                                          
085500       IF INPUT-WRONG                                                     
085600         MOVE '999'                      TO MED-IDMFSFEL                  
085700         MOVE 'NOT AUTHORIZED TO ATTEST' TO MOD-TEMFSFEL                  
085800         MOVE MFS-ALPHA-FIELD-WRONG      TO MOD-KDERSTMP-ATTR             
085900         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
086000       END-IF                                                             
086100     END-IF                                                               
086200                                                                          
086300     IF INPUT-OK AND                                                      
086400        MID-KDERSTMP NOT = ALL '+' AND SPACE                              
086500       PERFORM IMS-GU-WDD701                                              
086600       COMPUTE W-DAREGDAT = 99999999 -                                    
086700                            WS-DATE-8                                     
086800       END-COMPUTE                                                        
086900       PERFORM IMS-GNP-WDD713-QUAL                                        
087000       IF SEGMENT-FOUND                                                   
087100         IF MID-KDERSTMP = 'A' AND                                        
087200            TMP-KDERSTMP = 'A' AND                                        
087300            TMP-DAREGDAT-9KOMPL = W-DAREGDAT                              
087400           MOVE '999'                    TO MED-IDMFSFEL                  
087500           MOVE 'ALREADY ACTIVATED'      TO MOD-TEMFSFEL                  
087600           MOVE MFS-ALPHA-FIELD-WRONG    TO MOD-KDERSTMP-ATTR             
087700           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
087800           MOVE NOO                      TO INPUT-SW                      
087900         ELSE                                                             
088000           IF (MID-KDERSTMP = 'D' OR 'B') AND                             
088100              (TMP-KDERSTMP = 'D')                                        
088200             MOVE '999'                  TO MED-IDMFSFEL                  
088300             MOVE 'ALREADY DEACTIVATED'  TO MOD-TEMFSFEL                  
088400             MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDERSTMP-ATTR             
088500             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
088600             MOVE NOO                    TO INPUT-SW                      
088700           END-IF                                                         
088800         END-IF                                                           
088900       END-IF                                                             
089000       IF INPUT-OK                                                        
089100         IF MID-KDERSTMP = 'B' OR 'D'                                     
089200           IF (SEGMENT-MISSING) OR                                        
089300              (SEGMENT-FOUND AND                                          
089400              (TMP-DAREGDAT-9KOMPL NOT = W-DAREGDAT))                     
089500             MOVE '999'                   TO MED-IDMFSFEL                 
089600             MOVE 'NOTHING TO DEACTIVATE' TO MOD-TEMFSFEL                 
089700             MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDERSTMP-ATTR            
089800             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
089900             MOVE NOO                     TO INPUT-SW                     
090000           END-IF                                                         
090100         END-IF                                                           
090200       END-IF                                                             
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 GB-LOGICAL-CONTROL-2 SECTION.                                            
090800     PERFORM IMS-GU-WDK601                                                
090900     IF SEGMENT-MISSING                                                   
091000       MOVE ERR-PART-MISSING            TO MED-IDMFSFEL                   
091100       CALL WMEDKONV USING MED-WMEDAREA                                   
091200       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
091300       PERFORM MFS-ERASE-FIELD-OUT                                        
091400       MOVE NOO                         TO INPUT-SW                       
091500     ELSE                                                                 
091600       MOVE ZERO TO WS-KVRO                                               
091700       PERFORM IMS-GNP-WDK611                                             
091800       IF SEGMENT-FOUND                                                   
091900          IF CLAG-KDERS NOT = 01 AND 04 AND 07                            
092000            MOVE 'XXX'                     TO MED-IDMFSFEL                
092100            MOVE 'WRONG SUPERSESSION CODE' TO MOD-TEMFSFEL                
092200            PERFORM MFS-ERASE-FIELD-OUT                                   
092300            MOVE NOO                       TO INPUT-SW                    
092400          END-IF                                                          
092500          IF CLAG-KVROS = ZERO                                            
092600            MOVE 'XXX'                     TO MED-IDMFSFEL                
092700            MOVE 'NO BACKORDER'            TO MOD-TEMFSFEL                
092800            PERFORM MFS-ERASE-FIELD-OUT                                   
092900            MOVE NOO                       TO INPUT-SW                    
093000          ELSE                                                            
093100            MOVE CLAG-KVROS                TO WS-KVRO                     
093200          END-IF                                                          
093300       ELSE                                                               
093400          MOVE 'XXX'                        TO MED-IDMFSFEL               
093500          MOVE 'NO BACKORDER'               TO MOD-TEMFSFEL               
093600          PERFORM MFS-ERASE-FIELD-OUT                                     
093700          MOVE NOO                          TO INPUT-SW                   
093800       END-IF                                                             
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200                                                                          
094300 H-UPDATE SECTION.                                                        
094400     PERFORM IMS-GHU-WDD701                                               
094500                                                                          
094600     IF SAVE-BEKOM > SPACE                                                
094700       MOVE SAVE-BEKOM TO MID-BEKOM                                       
094800     END-IF                                                               
094900                                                                          
095000     IF (MID-BEKOM NOT = ALL '+' AND SPACE)                               
095100       PERFORM IMS-GHNP-WDD714                                            
095200       IF SEGMENT-FOUND                                                   
095300         IF MID-BEKOM NOT = NOT-BEKOM                                     
095400           MOVE MSGI-IDUSER     TO NOT-IDUSER                             
095500           MOVE WS-DATE-8       TO W-DATE                                 
095600           MOVE W-DATE-6        TO NOT-DAREGDAT                           
095700           MOVE MID-BEKOM       TO NOT-BEKOM                              
095800           PERFORM IMS-REPL-WDD714                                        
095900           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
096000           CALL WMEDKONV USING MED-WMEDAREA                               
096100           MOVE MED-TEMFSINF    TO MOD-TEMFSINF                           
096200         END-IF                                                           
096300       ELSE                                                               
096400         MOVE MSGI-IDUSER       TO NOT-IDUSER                             
096500         MOVE WS-DATE-8         TO W-DATE                                 
096600         MOVE W-DATE-6          TO NOT-DAREGDAT                           
096700         MOVE MID-BEKOM         TO NOT-BEKOM                              
096800         PERFORM IMS-ISRT-WDD714                                          
096900         MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                           
097000         CALL WMEDKONV USING MED-WMEDAREA                                 
097100         MOVE MED-TEMFSINF      TO MOD-TEMFSINF                           
097200       END-IF                                                             
097300     END-IF                                                               
097400                                                                          
097500     IF MID-KDERSTMP NOT = ALL '+' AND SPACE                              
097600       COMPUTE TMP-DAREGDAT-9KOMPL = 99999999 -                           
097700                                     WS-DATE-8                            
097800       END-COMPUTE                                                        
097900       ACCEPT WS-TIME-8         FROM TIME                                 
098000       MOVE WS-TIME-8           TO WS-TIME-9                              
098100       COMPUTE TMP-TIKLOCK-9KOMPL = 999999999 -                           
098200                                    WS-TIME-9                             
098300       END-COMPUTE                                                        
098400       IF MID-KDERSTMP = 'B'                                              
098500         MOVE 'D'               TO TMP-KDERSTMP                           
098600       ELSE                                                               
098700         MOVE MID-KDERSTMP      TO TMP-KDERSTMP                           
098800       END-IF                                                             
098900       MOVE WS-SULV-TOT         TO TMP-SULV                               
099000       MOVE MSGI-IDUSER         TO TMP-IDUSER                             
099100       PERFORM IMS-ISRT-WDD713                                            
099200       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
099300       CALL WMEDKONV USING MED-WMEDAREA                                   
099400       MOVE MED-TEMFSINF        TO MOD-TEMFSINF                           
099500     END-IF                                                               
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900 S10-ACCUMULATE-VALUE SECTION.                                            
100000     MOVE ZERO               TO WS-SULV-TOT                               
100100     PERFORM IMS-GNP-WDK711-UNQUAL                                        
100200     PERFORM UNTIL SEGMENT-MISSING                                        
100300       MOVE SLAG-IDDC        TO W-IDDC                                    
100400       PERFORM IMS-GU-WDB601                                              
100510       IF DCS-KDDC = 'S'                                                  
100600         PERFORM IMS-GU-WDL201                                            
100700         IF SEGMENT-FOUND                                                 
100800           MOVE ZERO         TO WS-KVAKS-XDC                              
100900           PERFORM IMS-GNP-WDL221                                         
101000           PERFORM UNTIL SEGMENT-MISSING                                  
101100             IF MOT-IDDC    = '11'           AND                          
101200                MOT-IDPTYP  = '310'          AND                          
101300                MOT-IDLEVNR = DCS-IDLEVNR-DC AND                          
101400                MOT-KDRT    = 8                                           
101500               IF MOT-KVAVIS > 0                                          
101600                 COMPUTE WS-KVAKS-XDC = WS-KVAKS-XDC +                    
101700                                       (MOT-KVAVIS -                      
101800                                        MOT-KVANTMOT)                     
101900                 END-COMPUTE                                              
102000               END-IF                                                     
102100             END-IF                                                       
102200             PERFORM IMS-GNP-WDL221                                       
102300           END-PERFORM                                                    
102400         END-IF                                                           
102500                                                                          
102600         COMPUTE WS-SULV-TOT = WS-SULV-TOT     +                          
102700                             ((SLAG-KVLS       +                          
102800                               SLAG-KVEFRS     +                          
102900                               SLAG-KVAKS-PAV  +                          
103000                               SLAG-KVAKS-SDC  +                          
103100                               WS-KVAKS-XDC)   *                          
103200                               CLAG-PRARTSTD)                             
103300         END-COMPUTE                                                      
103400       END-IF                                                             
103500                                                                          
103600       PERFORM IMS-GNP-WDK711-UNQUAL                                      
103700     END-PERFORM                                                          
103800     .                                                                    
103900     EJECT                                                                
104000                                                                          
104100 MFS-ERASE-FIELD-OUT SECTION.                                             
104200*    --- ALL OUTPUT FIELDS                                                
104300     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-SPAR                              
104400                               MOD-IDARTNR-SPAR                           
104500                               MOD-KDERSTMP                               
104600                               MOD-IDANSK                                 
104700                               MOD-KVLS                                   
104800                               MOD-KVAKS                                  
104900                               MOD-KVRO                                   
105000                               MOD-SUROBEL                                
105100                               MOD-KDERS                                  
105200                                                                          
105300     MOVE +1 TO INDX                                                      
105400     PERFORM UNTIL INDX > MAX-INDX                                        
105500       MOVE MFS-ERASE-FIELD TO MOD-IDDC-RADER (INDX)                      
105600                               MOD-KVAKS-PAV-RADER (INDX)                 
105700                               MOD-SUAKSV-PAV-RADER (INDX)                
105800                               MOD-TIAVIDAT-RADER (INDX)                  
105900                               MOD-KVLS-RADER (INDX)                      
106000                               MOD-SULV-RADER (INDX)                      
106100       ADD +1 TO INDX                                                     
106200     END-PERFORM                                                          
106300                                                                          
106400     MOVE MFS-ERASE-FIELD   TO MOD-FLFORTS                                
106500                               MOD-SULV-TOT                               
106600                               MOD-BEKOM                                  
106700                                                                          
106800     MOVE +1 TO INDX                                                      
106900     PERFORM UNTIL INDX > MAX-INDX-LOGG                                   
107000       MOVE MFS-ERASE-FIELD TO MOD-DAREGDAT-LOGG (INDX)                   
107100                               MOD-KDERSTMP-LOGG (INDX)                   
107200                               MOD-SULV-LOGG (INDX)                       
107300                               MOD-IDUSER-LOGG (INDX)                     
107400       ADD +1 TO INDX                                                     
107500     END-PERFORM                                                          
107600     .                                                                    
107700                                                                          
107800 MFS-ERASE-FIELD-IN SECTION.                                              
107900*    --- ALL INPUT FIELDS                                                 
108000     MOVE MFS-ERASE-FIELD   TO MOD-KDERSTMP                               
108100                               MOD-BEKOM                                  
108200     .                                                                    
108300     EJECT                                                                
108400                                                                          
108500 MFS-DONT-TOUCH-FIELD-OUT SECTION.                                        
108600*    --- ALL INPUT FIELDS                                                 
108700*    --- INCL SCROLL KEYS AND SCROLLDATA                                  
108800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-UT                        
108900                                    MOD-KDERSTMP                          
109000                                    MOD-BEKOM                             
109100     .                                                                    
109200                                                                          
109300 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
109400*    --- ALL INPUT FIELDS                                                 
109500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-IN                        
109600                                    MOD-KDERSTMP                          
109700                                    MOD-BEKOM                             
109800     .                                                                    
109900     EJECT                                                                
110000                                                                          
110100* --- IMS SECTIONS ---                                                    
110200                                                                          
110300 IMS-GET-MSG SECTION.                                                     
110400     MOVE '  QC'          TO GOOD-STATUSCODES                             
110500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110700     PERFORM IMS-STATUSCHECK                                              
110800     .                                                                    
110900                                                                          
111000 IMS-INSERT-MSG SECTION.                                                  
111100     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
111200     MOVE SPACE           TO GOOD-STATUSCODES                             
111300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
111400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111500     PERFORM IMS-STATUSCHECK                                              
111600     .                                                                    
111700     EJECT                                                                
111800                                                                          
111900 IMS-GU-WDK601 SECTION.                                                   
112000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
112100          DELIMITED BY SIZE INTO SSA1                                     
112200     MOVE '  GE'           TO GOOD-STATUSCODES                            
112300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
112400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSCHECK                                              
112600     .                                                                    
112700                                                                          
112800 IMS-GNP-WDK611 SECTION.                                                  
112900     MOVE 'WDK611'         TO SSA1                                        
113000     MOVE '  GE'           TO GOOD-STATUSCODES                            
113100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
113200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSCHECK                                              
113400     .                                                                    
113500     EJECT                                                                
113600                                                                          
113700 IMS-GU-WDK701 SECTION.                                                   
113800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
113900          DELIMITED BY SIZE INTO SSA1                                     
114000     MOVE '  GE'           TO GOOD-STATUSCODES                            
114100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
114200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUSCHECK                                              
114400     .                                                                    
114500                                                                          
114600 IMS-GNP-WDK711-QUAL SECTION.                                             
114700     STRING 'WDK711  (IDDC    =>' W-IDDC-MIN-X ')'                        
114800          DELIMITED BY SIZE INTO SSA1                                     
114900     MOVE '  GE'           TO GOOD-STATUSCODES                            
115000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
115100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
115200     PERFORM IMS-STATUSCHECK                                              
115300     .                                                                    
115400                                                                          
115500 IMS-GNP-WDK711-UNQUAL SECTION.                                           
115600     MOVE 'WDK711'         TO SSA1                                        
115700     MOVE '  GE'           TO GOOD-STATUSCODES                            
115800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
115900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSCHECK                                              
116100     .                                                                    
116200     EJECT                                                                
116300                                                                          
116400 IMS-GU-WDB601 SECTION.                                                   
116500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     MOVE '  '             TO GOOD-STATUSCODES                            
116800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
116900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
117000     PERFORM IMS-STATUSCHECK                                              
117100     .                                                                    
117200     EJECT                                                                
117300                                                                          
117400 IMS-GU-WDL201 SECTION.                                                   
117500     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
117600          DELIMITED BY SIZE INTO SSA1                                     
117700     MOVE '  GE'           TO GOOD-STATUSCODES                            
117800     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
117900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSCHECK                                              
118100     .                                                                    
118200                                                                          
118300 IMS-GNP-WDL211 SECTION.                                                  
118400     MOVE 'WDL211'         TO SSA1                                        
118500     MOVE '  GE'           TO GOOD-STATUSCODES                            
118600     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
118700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
118800     PERFORM IMS-STATUSCHECK                                              
118900     .                                                                    
119000                                                                          
119100 IMS-GNP-WDL221 SECTION.                                                  
119200     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
119300             DELIMITED BY SIZE INTO SSA1                                  
119400     MOVE '  GE' TO GOOD-STATUSCODES                                      
119500     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1                   
119600     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
119700     PERFORM IMS-STATUSCHECK                                              
119800     .                                                                    
119900                                                                          
120000 IMS-GU-WDR501 SECTION.                                                   
120100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
120200          DELIMITED BY SIZE INTO SSA1                                     
120300     MOVE '  GE'           TO GOOD-STATUSCODES                            
120400     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501 SSA1                    
120500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
120600     PERFORM IMS-STATUSCHECK                                              
120700     .                                                                    
120800                                                                          
120900 IMS-GNP-WDGX6328 SECTION.                                                
121000     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
121100          DELIMITED BY SIZE INTO SSA1                                     
121200     MOVE '  GE'           TO GOOD-STATUSCODES                            
121300     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX6328 SSA1                 
121400     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
121500     PERFORM IMS-STATUSCHECK                                              
121600     .                                                                    
121700     EJECT                                                                
121800                                                                          
121900 IMS-GU-WDD701 SECTION.                                                   
122000     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
122100          DELIMITED BY SIZE INTO SSA1                                     
122200     MOVE '  GE'           TO GOOD-STATUSCODES                            
122300     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
122400     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
122500     PERFORM IMS-STATUSCHECK                                              
122600     .                                                                    
122700                                                                          
122800 IMS-GHU-WDD701 SECTION.                                                  
122900     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
123000          DELIMITED BY SIZE INTO SSA1                                     
123100     MOVE '  '             TO GOOD-STATUSCODES                            
123200     CALL CBLTDLI USING GHU WDD7-PCB DLI-IO-WDD701 SSA1                   
123300     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
123400     PERFORM IMS-STATUSCHECK                                              
123500     .                                                                    
123600                                                                          
123700 IMS-GNP-WDD713 SECTION.                                                  
123800     MOVE 'WDD713'         TO SSA1                                        
123900     MOVE '  GE'           TO GOOD-STATUSCODES                            
124000     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD713 SSA1                   
124100     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSCHECK                                              
124300     .                                                                    
124400                                                                          
124500 IMS-GNP-WDD713-QUAL SECTION.                                             
124600     STRING 'WDD713  (WDD713KY>=' W-WDD713KY-X ')'                        
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     MOVE '  GE'           TO GOOD-STATUSCODES                            
124900     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD713 SSA1                   
125000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
125100     PERFORM IMS-STATUSCHECK                                              
125200     .                                                                    
125300                                                                          
125400 IMS-ISRT-WDD713 SECTION.                                                 
125500     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
125600          DELIMITED BY SIZE INTO SSA1                                     
125700     MOVE 'WDD713  '       TO SSA2                                        
125800     MOVE '  '             TO GOOD-STATUSCODES                            
125900     CALL CBLTDLI USING ISRT WDD7-PCB DLI-IO-WDD713 SSA1 SSA2             
126000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
126100     PERFORM IMS-STATUSCHECK                                              
126200     .                                                                    
126300                                                                          
126400 IMS-GNP-WDD714 SECTION.                                                  
126500     MOVE 'WDD714'         TO SSA1                                        
126600     MOVE '  GE'           TO GOOD-STATUSCODES                            
126700     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD714 SSA1                   
126800     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSCHECK                                              
127000     .                                                                    
127100                                                                          
127200 IMS-GHNP-WDD714 SECTION.                                                 
127300     MOVE 'WDD714'         TO SSA1                                        
127400     MOVE '  GE'           TO GOOD-STATUSCODES                            
127500     CALL CBLTDLI USING GHNP WDD7-PCB DLI-IO-WDD714 SSA1                  
127600     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
127700     PERFORM IMS-STATUSCHECK                                              
127800     .                                                                    
127900                                                                          
128000 IMS-REPL-WDD714 SECTION.                                                 
128100     MOVE '  '             TO GOOD-STATUSCODES                            
128200     CALL CBLTDLI USING REPL WDD7-PCB DLI-IO-WDD714 SSA1                  
128300     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
128400     PERFORM IMS-STATUSCHECK                                              
128500     .                                                                    
128600                                                                          
128700 IMS-ISRT-WDD714 SECTION.                                                 
128800     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE 'WDD714  '       TO SSA2                                        
129100     MOVE '  '             TO GOOD-STATUSCODES                            
129200     CALL CBLTDLI USING ISRT WDD7-PCB DLI-IO-WDD714 SSA1 SSA2             
129300     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUSCHECK                                              
129500     .                                                                    
129600     EJECT                                                                
129700                                                                          
129800 IMS-STATUSCHECK SECTION.                                                 
129900     SET STATUS-IX TO 1                                                   
130000     SEARCH GOOD-STATUS                                                   
130100       AT END                                                             
130200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
130300         DELIMITED BY SIZE INTO ERROR-TEXT                                
130400         CALL FELLOG                                                      
130500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
130600         CONTINUE                                                         
130700     END-SEARCH                                                           
130800     .                                                                    
