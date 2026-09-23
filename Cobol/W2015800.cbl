000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2015800.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   20/11/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    DETAILED FORECAST INFORMATION                                        
000900*                                                                         
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSACTION: W2T158                                              
001300*        MID:         W2I15801                                            
001400*                                                                         
001500*    OUTDATA.                                                             
001600*        MOD:         W2O15801                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400 77  IDPGM                       PIC X(08)   VALUE 'W2015800'.            
002500 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
002600 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
002700 77  W-DATUM                     PIC 9(06)   VALUE ZERO.                  
002800                                                                          
002900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400                                                                          
003500*    --- INDEX FOR SCROLL LINES                                           
003600 77  INDX                        PIC S9(4)  VALUE +0  COMP SYNC.          
003700 77  MAX-INDX                    PIC S9(4)  VALUE +51 COMP SYNC.          
003800                                                                          
003900 77  IX1                         PIC S9(4)  VALUE +0  COMP SYNC.          
004000 77  MAX-IX1                     PIC S9(4)  VALUE +5  COMP SYNC.          
004100 77  IX2                         PIC S9(4)  VALUE +0  COMP SYNC.          
004200 77  MAX-IX2                     PIC S9(4)  VALUE +14 COMP SYNC.          
004300                                                                          
004400*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004500                                                                          
004600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004700     88  KEYS-OK                             VALUE 'J'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004900                                                                          
005000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005100     88  OWN-MID                             VALUE '2158'.                
005200     88  2128-MID                            VALUE '2128'.                
005300     88  GOOD-MID                            VALUE '2121' '2122'          
005400                                                   '2123' '2124'          
005500                                                   '2125' '2126'          
005600                                                   '2127' '2128'.         
005700     88  HELP-MID                            VALUE '0551'.                
005800     EJECT                                                                
005900 01  ARBETS8AELT.                                                         
006000     03  ENDAST-SDCBEHOV         PIC  X(2)   VALUE '10'.                  
006100     03  ENDAST-NDCBEHOV         PIC  X(2)   VALUE '15'.                  
006200     03  W-TIAAVV-NUM            PIC   9(4)  VALUE ZERO.                  
006300     03  W-TIAAVV                PIC  S9(5)  VALUE ZERO  COMP-3.          
006400     03  W-TIAAVV-IN             PIC   9(4)  VALUE ZERO.                  
006500     03  W-ANTAL-VECKOR          PIC   9(3)   VALUE ZERO COMP-3.          
006600                                                                          
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  W22222                  PIC X(8)    VALUE 'W22222'.              
007000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
007100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     EJECT                                                                
007700 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
007800*    ---PARAMETRAR TILL DATKONV                                           
007900*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008100*    *************************************                                
008200*    **  LINK-AREA                      **                                
008300*    **  BEHOVSTABELL                   **                                
008400*    *************************************                                
008500*01  AREA  -COPY W222L222   -PRE LINK-.                                   
008600     EJECT                                                                
008700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008800*01 -COPY WMEDAREA                                                        
008900     SKIP3                                                                
009000 01  MESSAGE-CODES.                                                       
009100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009200     03  INF-ONLY-FUTURE-DATE    PIC X(3)    VALUE '363'.                 
009300     03  INF-MAX-ONE-YEAR-FUTURE PIC X(3)    VALUE '364'.                 
009400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009600     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
009700     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
009800     EJECT                                                                
009900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010200     SKIP3                                                                
010300*01 -COPY WMSGINIT                                                        
010400     EJECT                                                                
010500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010600*                                                                         
010700 01  SAVE-AREA.                                                           
010800     03  SAVE-IDTRANS             PIC X(4)   VALUE '2158'.                
010900     03  SAVE-IDARTNR-ENTER       PIC X(9)   VALUE SPACE.                 
011000     03  SAVE-IDDC-ENTER          PIC X(02)  VALUE SPACE.                 
011100     03  SAVE-IDARTNR-NEXT        PIC X(9)   VALUE SPACE.                 
011200     03  SAVE-IDDC-NEXT           PIC X(02)  VALUE SPACE.                 
011300     EJECT                                                                
011400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011700     SKIP3                                                                
011800*01  MID -COPY W2I15801                                                   
011900     EJECT                                                                
012000                                                                          
012100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012200     SKIP3                                                                
012300*01  -COPY WMSGAREA                                                       
012400     EJECT                                                                
012500     03  MOD REDEFINES MSG-AREA.                                          
012600*      05  -COPY W2O15801                                                 
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012900     SKIP3                                                                
013000*01  -COPY WMFSAREA                                                       
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16) VALUE 'WWDCKONS'.              
013300*      --- VALID IDDC CODES                                               
013400*                                                                         
013500*01    -COPY WWDCKONS                                                     
013600                                                                          
013700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000     SKIP3                                                                
014100 01  KEYS-FOR-DLI.                                                        
014200*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
014300                                                                          
014400     03  W-IDARTNR-X.                                                     
014500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014600     03  W-IDDC-MIN-X.                                                    
014700         05  W-IDDC-MIN          PIC  X(2)   VALUE SPACE.                 
014800     03  W-IDDC-MAX-X.                                                    
014900         05  W-IDDC-MAX          PIC  X(2)   VALUE SPACE.                 
015000     03  W-IDDC-REF-X.                                                    
015100         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
015200                                                                          
015300*    --- STATUS CODES FROM IMS                                            
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FOUND                       VALUE '  '.                  
015600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015800     88  SEGMENT-END                         VALUE 'GB'.                  
015900     SKIP2                                                                
016000 01  GOOD-STATUSCODES.                                                    
016100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(128).                              
016400 01  SSA2                        PIC X(128).                              
016500     EJECT                                                                
016600*    --- IMS FUNCTION CODES                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK601'.         
017100 01  DLI-IO-WDK601.                                                       
017200*    03 -COPY WDK601                                                      
017300     EJECT                                                                
017400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK611'.         
017500 01  DLI-IO-WDK611.                                                       
017600*    03 -COPY WDK611                                                      
017700     EJECT                                                                
017800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK701'.         
017900 01  DLI-IO-WDK701.                                                       
018000*    03 -COPY WDK701                                                      
018100     EJECT                                                                
018200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK711'.         
018300 01  DLI-IO-WDK711.                                                       
018400*    03 -COPY WDK711                                                      
018500     EJECT                                                                
018600 LINKAGE SECTION.                                                         
018700*01  -COPY W0009   -PRE MSG-                                              
018800                                                                          
018900*01  -COPY W0008   -PRE USEA-                                             
019000     05  FILLER                  PIC X.                                   
019100*01  -COPY W0008   -PRE WDK6-                                             
019200     05  FILLER                  PIC X.                                   
019300*01  -COPY W0008   -PRE WDK7-                                             
019400     05  FILLER                  PIC X.                                   
019500 01  W222-WDK6-PCB               PIC X.                                   
019600 01  W222-WDK7-PCB               PIC X.                                   
019700 01  W222-ARTM-PCB               PIC X.                                   
019800 01  W222-2501-PCB               PIC X.                                   
019900 01  W222-WDB6R-PCB              PIC X.                                   
020000 01  W222-WDK7R-PCB              PIC X.                                   
020100 01  W222-WDB6-PCB               PIC X.                                   
020200 01  W222-WDD7-PCB               PIC X.                                   
020300 01  W222-WDK7E-PCB              PIC X.                                   
020400 01  W222-UTIL-WDK6-PCB          PIC X.                                   
020500 01  W222-UTIL-WDK7-PCB          PIC X.                                   
020600 01  W222-UTIL-WDB6-PCB          PIC X.                                   
020700 01  W222-UTUP-WDK7-PCB          PIC X.                                   
020800 01  W222-UTUP-WDB6-PCB          PIC X.                                   
020900 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
021000 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
021100 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
021200                                                                          
021300                                                                          
021400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
021500                                   W222-WDK6-PCB                          
021600                                   W222-WDK7-PCB  W222-ARTM-PCB           
021700                                   W222-2501-PCB  W222-WDB6R-PCB          
021800                                   W222-WDK7R-PCB W222-WDB6-PCB           
021900                                   W222-WDD7-PCB  W222-WDK7E-PCB          
022000                                   W222-UTIL-WDK6-PCB                     
022100                                   W222-UTIL-WDK7-PCB                     
022200                                   W222-UTIL-WDB6-PCB                     
022300                                   W222-UTUP-WDK7-PCB                     
022400                                   W222-UTUP-WDB6-PCB                     
022500                                   W222-UTUP-UTIL-WDK6-PCB                
022600                                   W222-UTUP-UTIL-WDK7-PCB                
022700                                   W222-UTUP-UTIL-WDB6-PCB                
022800                                   .                                      
022900                                                                          
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
023200                                   W222-WDK6-PCB                          
023300                                   W222-WDK7-PCB  W222-ARTM-PCB           
023400                                   W222-2501-PCB  W222-WDB6R-PCB          
023500                                   W222-WDK7R-PCB W222-WDB6-PCB           
023600                                   W222-WDD7-PCB  W222-WDK7E-PCB          
023700                                   W222-UTIL-WDK6-PCB                     
023800                                   W222-UTIL-WDK7-PCB                     
023900                                   W222-UTIL-WDB6-PCB                     
024000                                   W222-UTUP-WDK7-PCB                     
024100                                   W222-UTUP-WDB6-PCB                     
024200                                   W222-UTUP-UTIL-WDK6-PCB                
024300                                   W222-UTUP-UTIL-WDK7-PCB                
024400                                   W222-UTUP-UTIL-WDB6-PCB                
024500                                   .                                      
024600                                                                          
024700     PERFORM IMS-GET-MSG                                                  
024800     IF SEGMENT-FOUND                                                     
024900       PERFORM A-INIT                                                     
025000       PERFORM B-CHECK-KEYS                                               
025100       IF KEYS-OK                                                         
025200          PERFORM F-READ-SHOW-INFO                                        
025300       END-IF                                                             
025400                                                                          
025500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O15801 + 4                      
025600       PERFORM IMS-INSERT-MSG                                             
025700                                                                          
025800     END-IF                                                               
025900                                                                          
026000     MOVE ZERO TO RETURN-CODE                                             
026100     GOBACK                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 A-INIT SECTION.                                                          
026500     MOVE 'A-INIT                  '  TO CURRENT-SECTION                  
026600                                                                          
026700     IF MSG-DOUBLE-TRANSACTIONS                                           
026800       MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W2I15801                   
026900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
027000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
027100     ELSE                                                                 
027200       MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W2I15801                   
027300       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
027400       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
027500     END-IF                                                               
027600                                                                          
027700     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
027800     MOVE MSG-IDPFK              TO MFS-IDPFK                             
027900     MOVE MFS-IDTRANS            TO W-IDTRANS                             
028000                                                                          
028100     MOVE LOW-VALUE              TO MSG-AREA                              
028200     MOVE 'W2O158N1'             TO MFS-IDMOD                             
028300     MOVE '2158'                 TO MOD-IDTRANS                           
028400     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL                          
028500                                    MOD-TEMFSINF                          
028600                                                                          
028700     IF OWN-MID OR HELP-MID                                               
028800       CONTINUE                                                           
028900     ELSE                                                                 
029000       MOVE SPACE                TO MFS-KDTRTYP                           
029100       MOVE '7'                  TO MFS-IDPFK                             
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 B-CHECK-KEYS SECTION.                                                    
029600     MOVE 'B-CHECK-KEYS            '  TO CURRENT-SECTION                  
029700                                                                          
029800     MOVE ALL '+'            TO MSGI-WMSGINIT                             
029900     MOVE '001'              TO MSGI-KDCALL                               
030000     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
030100     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
030200     MOVE '2158'             TO MSGI-IDTRANS                              
030300     IF OWN-MID                                                           
030400     OR (2128-MID AND MID-IDARTNR-IN NOT = ALL '+')                       
030500        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
030600        MOVE MID-TIAAVV-IN   TO MSGI-TIAAVV                               
030700     END-IF                                                               
030800                                                                          
030900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031000     IF (MSGI-SPAR-AREA (1:4) = '2158')                                   
031100     OR (2128-MID AND MID-IDARTNR-IN NOT = ALL '+')                       
031200        MOVE MSGI-SPAR-AREA TO SAVE-AREA                                  
031300     END-IF                                                               
031400                                                                          
031500*    - LANGUAGE TO BE USED BY MEDKONV                                     
031600     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
031700                                                                          
031800     MOVE YES                TO KEYS-SW                                   
031900                                                                          
032000*    -- KONTROLL AV IDARTNR                                               
032100     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
032200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
032300        MOVE '7'             TO MFS-IDPFK                                 
032400        MOVE SPACE           TO MFS-KDTRTYP                               
032500     END-IF                                                               
032600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
032700     IF MSGI-IDARTNR NUMERIC                                              
032800        MOVE MSGI-IDARTNR    TO W-IDARTNR                                 
032900     ELSE                                                                 
033000        MOVE NOO             TO KEYS-SW                                   
033100     END-IF                                                               
033200                                                                          
033300*    -- KONTROLL AV TIAAVV                                                
033400     MOVE MFS-RENSA-FAELT    TO MOD-TIAAVV-IN                             
033500     IF MID-TIAAVV-IN NOT = ALL '+'                                       
033600        MOVE '7'             TO MFS-IDPFK                                 
033700        MOVE SPACE           TO MFS-KDTRTYP                               
033800     END-IF                                                               
033900     IF MSGI-TIAAVV NUMERIC                                               
034000        MOVE MSGI-TIAAVV     TO W-TIAAVV-IN                               
034100     ELSE                                                                 
034200        MOVE NOO             TO KEYS-SW                                   
034300     END-IF                                                               
034400                                                                          
034500     IF GOOD-MID OR OWN-MID OR KEYS-OK                                    
034600       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
034700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZEROES BY SPACE           
034800       MOVE MSGI-TIAAVV      TO MOD-TIAAVV-UT                             
034900     ELSE                                                                 
035000       MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR-UT                            
035100                                MOD-TIAAVV-UT                             
035200     END-IF                                                               
035300                                                                          
035400     IF KEYS-WRONG                                                        
035500       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
035600       CALL WMEDKONV      USING MED-WMEDAREA                              
035700       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 F-READ-SHOW-INFO SECTION.                                                
036200     MOVE 'F-READ-SHOW-INFO        '  TO CURRENT-SECTION                  
036300                                                                          
036400     MOVE +0                        TO IX1                                
036500                                       IX2                                
036600                                                                          
036700     MOVE 'IDAG'                    TO DAT-KDDATFORM                      
036800                                                                          
036900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
037000                         DAT-O-TIDATUM DAT-KDSVAR                         
037100                                                                          
037200     IF DAT-KDSVAR-OK                                                     
037300        MOVE DAT-TIAAVV-GRP         TO W-TIAAVV-NUM                       
037400        MOVE W-TIAAVV-NUM           TO LINK-TIAAVV-AKTUELL                
037500                                       LINK-TIBEHOV-START                 
037600        MOVE 1                      TO W-ANTAL-VECKOR                     
037700        CALL W009VADD USING LINK-TIBEHOV-START                            
037800                            W-ANTAL-VECKOR                                
037900        MOVE DAT-TID                TO LINK-TID-AKTUELL                   
038000     ELSE                                                                 
038100        CALL FELLOG                                                       
038200     END-IF                                                               
038300                                                                          
038400     PERFORM IMS-GU-WDK601                                                
038500     IF SEGMENT-MISSING                                                   
038600       MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                       
038700       CALL WMEDKONV             USING MED-WMEDAREA                       
038800       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
038900       PERFORM MFS-RENSA-FAELT-UT                                         
039000     ELSE                                                                 
039100       PERFORM IMS-GNP-WDK611                                             
039200       IF SEGMENT-MISSING                                                 
039300         MOVE ERR-PART-MISSING      TO MED-IDMFSFEL                       
039400         CALL WMEDKONV           USING MED-WMEDAREA                       
039500         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
039600         PERFORM MFS-RENSA-FAELT-UT                                       
039700       ELSE                                                               
039800         PERFORM IMS-GU-WDK701                                            
039900         IF SEGMENT-FOUND                                                 
040000           PERFORM FA-VECKONR                                             
040100           IF CLAG-FLREFILL = YES                                         
040200             MOVE '1A'              TO W-IDDC-MIN                         
040300             MOVE '39'              TO W-IDDC-MAX                         
040400             MOVE ENDAST-SDCBEHOV   TO LINK-KDBEHOV                       
040500             PERFORM FB-CREATE-OUTPUT                                     
040600           END-IF                                                         
040700                                                                          
040800           MOVE '4A'                TO W-IDDC-MIN                         
040900           MOVE '89'                TO W-IDDC-MAX                         
041000           MOVE ENDAST-NDCBEHOV     TO LINK-KDBEHOV                       
041100           PERFORM FB-CREATE-OUTPUT                                       
041200         END-IF                                                           
041300       END-IF                                                             
041400     END-IF                                                               
041500                                                                          
041600     MOVE '002'                     TO MSGI-KDCALL                        
041700     MOVE '2158'                    TO SAVE-IDTRANS                       
041800     MOVE SAVE-AREA                 TO MSGI-SPAR-AREA                     
041900     CALL W005INIT               USING MSGI-WMSGINIT USEA-PCB             
042000     .                                                                    
042100     EJECT                                                                
042200                                                                          
042300 FA-VECKONR        SECTION.                                               
042400     MOVE 'FA-VECKONR '  TO CURRENT-SECTION                               
042500                                                                          
042600* DESSUTOM - TODAYS DATE                                                  
042700     IF W-TIAAVV-NUM = W-TIAAVV-IN                                        
042800       CONTINUE                                                           
042900     ELSE                                                                 
043000       IF W-TIAAVV-IN < W-TIAAVV-NUM                                      
043100         MOVE INF-ONLY-FUTURE-DATE                                        
043200                                 TO MED-IDMFSFEL                          
043300         CALL WMEDKONV        USING MED-WMEDAREA                          
043400         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
043500       ELSE                                                               
043600* TODAYS DATE + 1                                                         
043700         MOVE LINK-TIBEHOV-START TO W-TIAAVV                              
043800                                    W-TIAAVV-NUM                          
043900                                                                          
044000         MOVE +1                 TO INDX                                  
044100         PERFORM UNTIL INDX > MAX-INDX                                    
044200            OR W-TIAAVV-NUM = W-TIAAVV-IN                                 
044300* NEXT WEEK - W009VADD                                                    
044400           ADD +1                TO INDX                                  
044500           MOVE 1                TO W-ANTAL-VECKOR                        
044600           CALL W009VADD      USING W-TIAAVV                              
044700                                    W-ANTAL-VECKOR                        
044800           MOVE W-TIAAVV         TO W-TIAAVV-NUM                          
044900         END-PERFORM                                                      
045000         IF INDX > MAX-INDX                                               
045100           MOVE INF-MAX-ONE-YEAR-FUTURE                                   
045200                                 TO MED-IDMFSFEL                          
045300                                                                          
045400           MOVE +0               TO INDX                                  
045500           CALL WMEDKONV      USING MED-WMEDAREA                          
045600           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
045700         END-IF                                                           
045800       END-IF                                                             
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 FB-CREATE-OUTPUT SECTION.                                                
046300     MOVE 'FB-CREATE-OUTPUT ' TO CURRENT-SECTION                          
046400                                                                          
046500     MOVE WC-CDC-SE            TO W-IDDC-REF                              
046600     PERFORM IMS-GNP-WDK711                                               
046700     PERFORM UNTIL SEGMENT-MISSING                                        
046800       IF ART-KDERS-UTG = +0                                              
046900         MOVE ART-IDARTNR      TO LINK-IDARTNR                            
047000         MOVE SLAG-IDDC        TO LINK-IDDC                               
047100         MOVE INDX             TO LINK-KVVECKOR-BEHOV                     
047200         MOVE NOO              TO LINK-FLINKLDIRLEV                       
047300         CALL W22222 USING LINK-AREA                                      
047400                           W222-WDK6-PCB  W222-WDK7-PCB                   
047500                           W222-ARTM-PCB  W222-2501-PCB                   
047600                           W222-WDB6R-PCB                                 
047700                           W222-WDK7R-PCB                                 
047800                           W222-WDB6-PCB  W222-WDD7-PCB                   
047900                           W222-WDK7E-PCB                                 
048000                           W222-UTIL-WDK6-PCB                             
048100                           W222-UTIL-WDK7-PCB                             
048200                           W222-UTIL-WDB6-PCB                             
048300                           W222-UTUP-WDK7-PCB                             
048400                           W222-UTUP-WDB6-PCB                             
048500                           W222-UTUP-UTIL-WDK6-PCB                        
048600                           W222-UTUP-UTIL-WDK7-PCB                        
048700                           W222-UTUP-UTIL-WDB6-PCB                        
048800                                                                          
048900         IF LINK-ANROP-OK                                                 
049000                                                                          
049100           IF W-TIAAVV-IN           = DAT-TIAAVV-GRP                      
049200          AND LINK-KVBEHOV-DESSUTOM > +0                                  
049300             MOVE +1                         TO IX1                       
049400             ADD  +1                         TO IX2                       
049500             MOVE LINK-IDDC                  TO MOD-IDDC                  
049600                                               (IX1, IX2)                 
049700             MOVE LINK-KVBEHOV-DESSUTOM      TO MOD-KVPB                  
049800                                               (IX1, IX2)                 
049900           END-IF                                                         
050000                                                                          
050100           IF INDX > +0                                                   
050200             IF W-TIAAVV-NUM = W-TIAAVV-IN                                
050300                                                                          
050400               IF LINK-KVBEHOV-VECKA(INDX) > +0                           
050500                 IF IX2 = +0 OR MAX-IX2                                   
050600                   ADD +1                    TO IX1                       
050700                   MOVE +0                   TO IX2                       
050800                 END-IF                                                   
050900                 ADD +1                      TO IX2                       
051000                 MOVE LINK-IDDC              TO MOD-IDDC                  
051100                                               (IX1, IX2)                 
051200                 MOVE LINK-KVBEHOV-VECKA(INDX) TO MOD-KVPB                
051300                                               (IX1, IX2)                 
051400               END-IF                                                     
051500             END-IF                                                       
051600           END-IF                                                         
051700         ELSE                                                             
051800           PERFORM MFS-RENSA-FAELT-UT                                     
051900         END-IF                                                           
052000       END-IF                                                             
052100       PERFORM IMS-GNP-WDK711                                             
052200     END-PERFORM                                                          
052300     .                                                                    
052400     EJECT                                                                
052500 MFS-RENSA-FAELT-UT SECTION.                                              
052600                                                                          
052700*    --- ALLA UTDATA-FÄLT                                                 
052800     MOVE 1                   TO IX1                                      
052900     PERFORM UNTIL IX1 > MAX-IX1                                          
053000       MOVE 1                 TO IX2                                      
053100       PERFORM UNTIL IX2 > MAX-IX2                                        
053200         MOVE MFS-RENSA-FAELT TO MOD-IDDC (IX1, IX2)                      
053300                                 MOD-KVPB (IX1, IX2)                      
053400         ADD 1                TO IX2                                      
053500       END-PERFORM                                                        
053600       ADD 1                  TO IX1                                      
053700     END-PERFORM                                                          
053800     .                                                                    
053900* --- IMS SECTIONS ---                                                    
054000     SKIP3                                                                
054100 IMS-GET-MSG SECTION.                                                     
054200                                                                          
054300     MOVE '  QC' TO GOOD-STATUSCODES                                      
054400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
054500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054600     PERFORM IMS-STATUSCHECK                                              
054700     .                                                                    
054800     SKIP3                                                                
054900 IMS-INSERT-MSG SECTION.                                                  
055000                                                                          
055100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055200     MOVE SPACE TO GOOD-STATUSCODES                                       
055300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
055400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055500     PERFORM IMS-STATUSCHECK                                              
055600     .                                                                    
055700     EJECT                                                                
055800 IMS-GU-WDK601 SECTION.                                                   
055900     MOVE 'IMS-GNP-WDK611 '  TO DBS-SECTION                               
056000                                                                          
056100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
056200          DELIMITED BY SIZE INTO SSA1                                     
056300     MOVE '  GE'              TO GOOD-STATUSCODES                         
056400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
056500     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
056600     PERFORM IMS-STATUSCHECK                                              
056700     .                                                                    
056800                                                                          
056900 IMS-GNP-WDK611 SECTION.                                                  
057000     MOVE 'IMS-GNP-WDK611 '  TO DBS-SECTION                               
057100                                                                          
057200     MOVE 'WDK611   '         TO SSA1                                     
057300     MOVE '  GE'              TO GOOD-STATUSCODES                         
057400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
057500     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
057600     PERFORM IMS-STATUSCHECK                                              
057700     .                                                                    
057800     EJECT                                                                
057900 IMS-GU-WDK701      SECTION.                                              
058000     MOVE 'IMS-GU-WDK701 ' TO DBS-SECTION                                 
058100                                                                          
058200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
058300            DELIMITED BY SIZE INTO SSA1                                   
058400     MOVE '  GE'                TO GOOD-STATUSCODES                       
058500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
058600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
058700     PERFORM IMS-STATUSCHECK                                              
058800     .                                                                    
058900     SKIP3                                                                
059000 IMS-GNP-WDK711  SECTION.                                                 
059100     MOVE 'IMS-GNP-WDK711 ' TO DBS-SECTION                                
059200                                                                          
059300     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
059400                    '&IDDC    <=' W-IDDC-MAX-X                            
059500                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
059600          DELIMITED BY SIZE INTO SSA1                                     
059700     MOVE '  GE'              TO GOOD-STATUSCODES                         
059800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
059900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
060000     PERFORM IMS-STATUSCHECK                                              
060100     .                                                                    
060200     SKIP3                                                                
060300 IMS-STATUSCHECK SECTION.                                                 
060400                                                                          
060500     SET STATUS-IX TO 1                                                   
060600     SEARCH GOOD-STATUS                                                   
060700       AT END                                                             
060800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
060900         DELIMITED BY SIZE INTO ERROR-TEXT                                
061000         CALL FELLOG                                                      
061100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
061200         CONTINUE                                                         
061300     END-SEARCH                                                           
061400     .                                                                    
