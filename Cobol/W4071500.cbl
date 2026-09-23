000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4071500.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   95/08/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        QUESTION ON RETURN PERMIT.                                       
001000*        KEYS THAT HAVE TO BE FILLED IN: DISTR/CUST/REPORT.NO             
001100*        READS DATABASE WLKREE AND SHOWS INFO.                            
001200*                                                                         
001300*        THE PROGRAM READS     WLKREE (WDA2)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W4T715                                              
001700*        MID:         W4I71501                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W4O71501                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4071500'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FOR SCROLL LINES                                           
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +1200 COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004100 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
004200 77  IDARTNR-WS                  PIC 9(8)   VALUE ZERO.                   
004300 77  IDRADNR-WS                  PIC 9(4)   VALUE ZERO.                   
004400 77  WS-TIRETANK                 PIC S9(6)  VALUE ZERO  COMP-3.           
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004600                                                                          
004700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004800     88  KEYS-OK                             VALUE 'Y'.                   
004900     88  KEYS-WRONG                          VALUE 'N'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  OWN-MID                             VALUE '4715'.                
005300     88  GOOD-MID                            VALUE '4711' '4712'          
005400                                                   '4713' '4714'          
005500                                                   '4715' '4716'          
005600                                                   '472D'.                
005700     88  HELP-MID                            VALUE '0551'.                
005800     88  4737-MID                            VALUE '4737'.                
005900     EJECT                                                                
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERAL-SUBPROGRAM.                                                  
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
006700     EJECT                                                                
006800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006900*01 -COPY WMEDAREA                                                        
007000                                                                          
007100 01  MESSAGE-CODES.                                                       
007200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007500     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
007600     03  ERR-NO-UPDATE           PIC X(3)    VALUE '777'.                 
007700     03  ERR-LINE-MISSING        PIC X(3)    VALUE '005'.                 
007800     EJECT                                                                
007900*    ---  LÄNKAREA TILL W418OKOD                                          
008000 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
008100                                                                          
008200*01 -COPY W418OKOD           -PRE OKOD-.                                  
008300     EJECT                                                                
008400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700                                                                          
008800*01 -COPY WMSGINIT                                                        
008900                                                                          
009000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300                                                                          
009400*01  MID -COPY W4I71501                                                   
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700                                                                          
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W4O71501                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400                                                                          
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010800*                                                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100                                                                          
011200 01  KEYS-TO-DLI.                                                         
011300     03  W-IDLEVANM-X.                                                    
011400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011600         05  W-IDRAPPNR          PIC 9(7).                                
011700     03  W-WDA2KEY-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
012000                                                                          
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FOUND                       VALUE '  '.                  
012400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012600                                                                          
012700 01  GOOD-STATUSCODES.                                                    
012800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900                                                                          
013000 01  SSA1                        PIC X(64).                               
013100 01  SSA2                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNCTION CODES                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013800                                                                          
013900 01  DLI-IO-AREA.                                                         
014000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
014100                                                                          
014200     03  WLKREE01 REDEFINES IO-AREA.                                      
014300*        05  -COPY WDA201                                                 
014400                                                                          
014500     03  WLKREE11 REDEFINES IO-AREA.                                      
014600*        05  -COPY WDA211                                                 
014700     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900                                                                          
015000*01  -COPY W0009   -PRE MSG-                                              
015100*01  -COPY W0008   -PRE USEA-                                             
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400*01  -COPY W0008  -PRE KREE-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION  USING MSG-PCB                                        
015800                           USEA-PCB                                       
015900                           KREE-PCB.                                      
016000     ENTRY 'DLITCBL' USING MSG-PCB                                        
016100                           USEA-PCB                                       
016200                           KREE-PCB.                                      
016300                                                                          
016400     PERFORM IMS-GET-MSG                                                  
016500     IF SEGMENT-FOUND                                                     
016600       PERFORM A-INIT                                                     
016700       PERFORM B-CHECK-KEYS                                               
016800       IF KEYS-OK                                                         
016900         IF MFS-FIRST                                                     
017000           PERFORM C-FIRST-PAGE                                           
017100         ELSE                                                             
017200           IF MFS-NEXT                                                    
017300             PERFORM D-NEXT-PAGE                                          
017400           ELSE                                                           
017500             PERFORM E-SAME-PAGE                                          
017600           END-IF                                                         
017700         END-IF                                                           
017800         PERFORM F-READ-SHOW-INFO                                         
017900       END-IF                                                             
018000       MOVE MAX-MOD-LENGTH TO MSG-KVLL                                    
018100       PERFORM IMS-INSERT-MSG                                             
018200     END-IF                                                               
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     IF MSG-DOUBLE-TRANSACTIONS                                           
019100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I71501                 
019200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I71501                  
019600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019800     END-IF                                                               
019900                                                                          
020000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020300                                                                          
020400     MOVE LOW-VALUE TO MSG-AREA                                           
020500     MOVE 'W4O71501' TO MFS-IDMOD                                         
020600     MOVE '4715' TO MOD-IDTRANS                                           
020700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
020800                                                                          
020900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71501 + 4                        
021000                                                                          
021100     IF OWN-MID OR HELP-MID                                               
021200       CONTINUE                                                           
021300     ELSE                                                                 
021400       MOVE SPACE TO MFS-KDTRTYP                                          
021500       MOVE '7' TO MFS-IDPFK                                              
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 B-CHECK-KEYS SECTION.                                                    
022000                                                                          
022100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022200     MOVE '001'             TO MSGI-KDCALL                                
022300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022400     MOVE '4715'            TO MSGI-IDTRANS                               
022500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022600     IF GOOD-MID                                                          
022700       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
022800       MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                           
022900       MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                           
023000       IF MID-IDARTNR-IN  = ALL '+'                                       
023100          MOVE '+++++++++'     TO MSGI-IDARTNR                            
023200       ELSE                                                               
023300          MOVE MID-IDARTNR-IN     TO WS-IDARTNR                           
023400          MOVE WS-IDARTNR         TO MSGI-IDARTNR                         
023500       END-IF                                                             
023600       MOVE MID-IDRADNR-IN     TO MSGI-IDRADNR                            
023700     END-IF                                                               
023800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023900                                                                          
024000     IF MSGI-IDLAND-SPR = 'GB'                                            
024100       MOVE 'GB'                TO MED-IDSKYLT                            
024200     ELSE                                                                 
024300       MOVE 'S '                TO MED-IDSKYLT                            
024400     END-IF                                                               
024500                                                                          
024600     MOVE YES TO KEYS-SW                                                  
024700                                                                          
024800     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
024900                             MOD-IDKUNDNR-IN                              
025000                             MOD-IDRAPPNR-IN                              
025100                             MOD-IDARTNR-IN                               
025200                             MOD-IDRADNR-IN                               
025300                                                                          
025400                                                                          
025500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
025600       MOVE '7'         TO MFS-IDPFK                                      
025700       MOVE SPACE       TO MFS-KDTRTYP                                    
025800     END-IF                                                               
025900                                                                          
026000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
026100       MOVE '7'         TO MFS-IDPFK                                      
026200       MOVE SPACE       TO MFS-KDTRTYP                                    
026300     END-IF                                                               
026400                                                                          
026500     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
026600       MOVE '7'         TO MFS-IDPFK                                      
026700       MOVE SPACE       TO MFS-KDTRTYP                                    
026800     END-IF                                                               
026900                                                                          
027000     MOVE MSGI-IDDISTR  TO W-IDDISTR                                      
027100     MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                     
027200     MOVE MSGI-IDRAPPNR TO W-IDRAPPNR                                     
027300     MOVE MSGI-IDRADNR                    TO IDRADNR-WS                   
027400     MOVE MSGI-IDARTNR (2:8)              TO IDARTNR-WS                   
027500                                                                          
027600     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
027700       MOVE '7'                  TO MFS-IDPFK                             
027800       MOVE SPACE                TO MFS-KDTRTYP                           
027900       IF 4737-MID                                                        
028000          IF IDARTNR-WS  NOT NUMERIC                                      
028100             MOVE ZERO           TO IDARTNR-WS                            
028200          END-IF                                                          
028300       END-IF                                                             
028400     ELSE                                                                 
028500       IF GOOD-MID                                                        
028600          MOVE MID-IDARTNR-UT             TO IDARTNR-WS                   
028700       END-IF                                                             
028800       IF 4737-MID                                                        
028900          IF IDARTNR-WS  NOT NUMERIC                                      
029000             MOVE ZERO           TO IDARTNR-WS                            
029100          END-IF                                                          
029200       END-IF                                                             
029300     END-IF                                                               
029400                                                                          
029500     IF MID-IDRADNR-IN NOT = ALL '+'                                      
029600       MOVE '7'                  TO MFS-IDPFK                             
029700       MOVE SPACE                TO MFS-KDTRTYP                           
029800       IF 4737-MID                                                        
029900          MOVE ZERO              TO IDRADNR-WS                            
030000       END-IF                                                             
030100     ELSE                                                                 
030200       IF GOOD-MID                                                        
030300          MOVE MID-IDRADNR-UT             TO IDRADNR-WS                   
030400       END-IF                                                             
030500     END-IF                                                               
030600                                                                          
030700     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
030800     INSPECT IDRADNR-WS REPLACING LEADING SPACE BY ZERO                   
030900                                                                          
031000     IF IDARTNR-WS  NOT NUMERIC                                           
031100       MOVE NOO                  TO KEYS-SW                               
031200     ELSE                                                                 
031300       MOVE IDARTNR-WS         TO W-IDARTNR                               
031400       MOVE IDRADNR-WS         TO W-IDRADNR                               
031500     END-IF                                                               
031600                                                                          
031700     IF IDRADNR-WS NOT NUMERIC                                            
031800       MOVE NOO                  TO KEYS-SW                               
031900     END-IF                                                               
032000                                                                          
032100     IF KEYS-OK                                                           
032200       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
032300       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
032400       MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                       
032500       MOVE IDARTNR-WS           TO MOD-IDARTNR-UT                        
032600       MOVE IDRADNR-WS           TO MOD-IDRADNR-UT                        
032700     ELSE                                                                 
032800       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                             
032900                               MOD-IDKUNDNR-UT                            
033000                               MOD-IDRAPPNR-UT                            
033100                               MOD-IDARTNR-UT                             
033200                               MOD-IDRADNR-UT                             
033300     END-IF                                                               
033400                                                                          
033500     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
033600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
033700     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
033800     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
033900     INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE              
034000     IF MOD-IDKUNDNR-UT          = SPACE                                  
034100        MOVE '     0'           TO MOD-IDKUNDNR-UT                        
034200     END-IF                                                               
034300                                                                          
034400     IF KEYS-WRONG                                                        
034500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
034600       CALL WMEDKONV USING MED-WMEDAREA                                   
034700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034800       PERFORM MFS-ERASE-FIELD-IN                                         
034900       PERFORM MFS-ERASE-FIELD-OUT                                        
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 C-FIRST-PAGE SECTION.                                                    
035400                                                                          
035500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
035600     CALL WMEDKONV USING MED-WMEDAREA                                     
035700     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
035800                                                                          
035900     PERFORM MFS-ERASE-FIELD-IN                                           
036000     .                                                                    
036100     EJECT                                                                
036200 D-NEXT-PAGE SECTION.                                                     
036300                                                                          
036400     IF OWN-MID                                                           
036500       MOVE MID-IDARTNR-NEXT      TO W-IDARTNR                            
036600       MOVE MID-IDRADNR-NEXT      TO W-IDRADNR                            
036700     ELSE                                                                 
036800       MOVE IDARTNR-WS            TO W-IDARTNR                            
036900       MOVE IDRADNR-WS            TO W-IDRADNR                            
037000       PERFORM MFS-ERASE-FIELD-IN                                         
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 E-SAME-PAGE SECTION.                                                     
037500                                                                          
037600     IF GOOD-MID OR HELP-MID                                              
037700       MOVE MID-IDARTNR-ENTER     TO W-IDARTNR                            
037800       MOVE MID-IDRADNR-ENTER     TO W-IDRADNR                            
037900     ELSE                                                                 
038000       MOVE ZERO                  TO W-IDARTNR                            
038100                                     W-IDRADNR                            
038200       PERFORM MFS-ERASE-FIELD-IN                                         
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 F-READ-SHOW-INFO SECTION.                                                
038700                                                                          
038800     PERFORM IMS-GET-KREE01                                               
038900                                                                          
039000     IF SEGMENT-MISSING                                                   
039100        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
039200        CALL WMEDKONV USING MED-WMEDAREA                                  
039300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039400        PERFORM MFS-ERASE-FIELD-OUT                                       
039500     ELSE                                                                 
039600       IF ANM-DARETILL > 0                                                
039700          MOVE ANM-DARETILL(3:6) TO MOD-TIRETILL                          
039800       ELSE                                                               
039900          MOVE MFS-RENSA-FAELT   TO MOD-TIRETILL                          
040000       END-IF                                                             
040100       MOVE ANM-DARETANK(3:6)    TO WS-TIRETANK                           
040200       MOVE +1 TO INDX                                                    
040300                                                                          
040400       PERFORM IMS-GNP-KREE11                                             
040500       IF SEGMENT-FOUND                                                   
040600         MOVE LEV-IDARTNR        TO MOD-IDARTNR-ENTER                     
040700         MOVE LEV-IDRADNR        TO MOD-IDRADNR-ENTER                     
040800       ELSE                                                               
040900         MOVE ZERO               TO MOD-IDARTNR-ENTER                     
041000                                    MOD-IDRADNR-ENTER                     
041100                                    MOD-IDARTNR-NEXT                      
041200                                    MOD-IDRADNR-NEXT                      
041300         MOVE ZERO               TO MOD-IDANSTNR-RET                      
041400       END-IF                                                             
041500                                                                          
041600       PERFORM UNTIL INDX > MAX-INDX                                      
041700         IF SEGMENT-FOUND                                                 
041800           MOVE LEV-IDARTNR      TO MOD-IDARTNR    (INDX)                 
041900           MOVE LEV-IDRADNR      TO MOD-IDRADNR    (INDX)                 
042000           MOVE LEV-KDANMORS     TO MOD-KDANMORS   (INDX)                 
042100           MOVE LEV-KVLEVANM-BEKR TO MOD-KVLEVANM-BEKR (INDX)             
042200           MOVE LEV-KVLEVANM     TO MOD-KVLEVANM   (INDX)                 
042300           IF LEV-IDLOPNRM > +0                                           
042400             MOVE WS-TIRETANK    TO MOD-TIRETANK   (INDX)                 
042500           ELSE                                                           
042600             MOVE ZERO           TO MOD-TIRETANK   (INDX)                 
042700           END-IF                                                         
042800           MOVE LEV-TIINLINL     TO MOD-TIINLINL   (INDX)                 
042900           MOVE LEV-KVRETINL     TO MOD-KVRETINL   (INDX)                 
043000           MOVE LEV-KVRETINL-SKR TO MOD-KVRETINL-SKR (INDX)               
043100           MOVE LEV-KVAVV-KVANT  TO MOD-KVAVV-KVANT (INDX)                
043200           MOVE LEV-KVAVV-KVAL   TO MOD-KVAVV-KVAL (INDX)                 
043300           IF LEV-FLTEXT         = JA                                     
043400             IF MSGI-IDLAND-SPR = 'GB'                                    
043500               MOVE YES         TO MOD-FLTEXT   (INDX)                    
043600             ELSE                                                         
043700               MOVE LEV-FLTEXT  TO MOD-FLTEXT   (INDX)                    
043800             END-IF                                                       
043900           ELSE                                                           
044000             MOVE LEV-FLTEXT    TO MOD-FLTEXT   (INDX)                    
044100           END-IF                                                         
044110           IF INDX = 1                                                    
044200             IF LEV-IDANSTNR-ILIU > ZERO                                  
044410               MOVE LEV-IDANSTNR-ILIU TO MOD-IDANSTNR-RET                 
044500             ELSE                                                         
044501               IF LEV-IDANSTNR-RET > ZERO                                 
044502                 MOVE LEV-IDANSTNR-RET TO MOD-IDANSTNR-RET                
044503               END-IF                                                     
044504             END-IF                                                       
044510           END-IF                                                         
044600                                                                          
044700*--- ANROPA KONTROLL AV ORSAKSKODER                                       
044800           MOVE LEV-KDANMORS    TO OKOD-KDANMORS                          
044900           CALL W418OKOD USING OKOD-W418OKOD                              
045000           IF OKOD-FL-RETILL = 'J' OR                                     
045100              OKOD-FL-INTERNUPPACKNING = 'J'                              
045200             IF LEV-KDKREBEH = 'ANN'                                      
045300               MOVE 'XA'         TO MOD-FLRETILL   (INDX)                 
045400             ELSE                                                         
045500               MOVE 'X '         TO MOD-FLRETILL   (INDX)                 
045600             END-IF                                                       
045700           ELSE                                                           
045800             IF LEV-KDKREBEH = 'ANN'                                      
045900               MOVE ' A'         TO MOD-FLRETILL   (INDX)                 
046000             ELSE                                                         
046100               MOVE '  '         TO MOD-FLRETILL   (INDX)                 
046200             END-IF                                                       
046300           END-IF                                                         
046400                                                                          
046500           PERFORM IMS-GNP-KREE11                                         
046600         ELSE                                                             
046700           MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR    (INDX)                 
046800                                    MOD-KDANMORS   (INDX)                 
046900                                    MOD-KDANMORS   (INDX)                 
047000                                    MOD-KVLEVANM-BEKR (INDX)              
047100                                    MOD-KVLEVANM   (INDX)                 
047200                                    MOD-TIRETANK   (INDX)                 
047300                                    MOD-TIINLINL   (INDX)                 
047400                                    MOD-KVRETINL   (INDX)                 
047500                                    MOD-KVRETINL-SKR (INDX)               
047600                                    MOD-KVAVV-KVANT (INDX)                
047700                                    MOD-KVAVV-KVAL (INDX)                 
047800                                    MOD-FLTEXT     (INDX)                 
047900         END-IF                                                           
048000         ADD 1 TO INDX                                                    
048100       END-PERFORM                                                        
048200                                                                          
048300       IF SEGMENT-FOUND                                                   
048400         MOVE LEV-IDARTNR        TO MOD-IDARTNR-NEXT                      
048500         MOVE LEV-IDRADNR        TO MOD-IDRADNR-NEXT                      
048600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
048700         CALL WMEDKONV USING MED-WMEDAREA                                 
048800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
048900       ELSE                                                               
049000         MOVE ZERO             TO MOD-IDARTNR-NEXT                        
049100                                  MOD-IDRADNR-NEXT                        
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 MFS-ERASE-FIELD-OUT SECTION.                                             
049700                                                                          
049800*    --- ALLA UTDATA-FÄLT                                                 
049900*    --- INCL. SCROLL KEYS                                                
050000     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
050100                             MOD-IDKUNDNR-UT                              
050200                             MOD-IDRAPPNR-UT                              
050300                             MOD-IDARTNR-UT                               
050400                             MOD-IDRADNR-UT                               
050500                             MOD-IDARTNR-ENTER                            
050600                             MOD-IDARTNR-NEXT                             
050700                             MOD-IDRADNR-ENTER                            
050800                             MOD-IDRADNR-NEXT                             
050900     .                                                                    
051000 MFS-ERASE-FIELD-IN SECTION.                                              
051100                                                                          
051200*    --- ALLA INDATA-FÄLT                                                 
051300     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
051400                             MOD-IDKUNDNR-IN                              
051500                             MOD-IDRAPPNR-IN                              
051600                             MOD-IDARTNR-IN                               
051700                             MOD-IDRADNR-IN                               
051800     .                                                                    
051900     EJECT                                                                
052000* --- IMS SECTIONS ---                                                    
052100                                                                          
052200 IMS-GET-MSG SECTION.                                                     
052300                                                                          
052400     MOVE '  QC' TO GOOD-STATUSCODES                                      
052500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
052600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052700     PERFORM IMS-STATUSCHECK                                              
052800     .                                                                    
052900                                                                          
053000 IMS-INSERT-MSG SECTION.                                                  
053100                                                                          
053200     IF MSGI-IDLAND-SPR = 'GB'                                            
053300       MOVE 'N' TO MFS-KDHUVOMR                                           
053400     END-IF                                                               
053500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
053600     MOVE SPACE TO GOOD-STATUSCODES                                       
053700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
053800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053900     PERFORM IMS-STATUSCHECK                                              
054000     .                                                                    
054100     EJECT                                                                
054200 IMS-GET-KREE01 SECTION.                                                  
054300                                                                          
054400     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
054500          DELIMITED BY SIZE INTO SSA1                                     
054600     MOVE '  GE' TO GOOD-STATUSCODES                                      
054700     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
054800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
054900     PERFORM IMS-STATUSCHECK                                              
055000     .                                                                    
055100     EJECT                                                                
055200 IMS-GNP-KREE11 SECTION.                                                  
055300                                                                          
055400     STRING 'WLKREE11(WDA211KY>=' W-WDA2KEY-X ')'                         
055500          DELIMITED BY SIZE INTO SSA1                                     
055600     MOVE '  GE' TO GOOD-STATUSCODES                                      
055700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
055800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSCHECK                                              
056000     .                                                                    
056100     EJECT                                                                
056200 IMS-STATUSCHECK SECTION.                                                 
056300                                                                          
056400     SET STATUS-IX TO 1                                                   
056500     SEARCH GOOD-STATUS                                                   
056600       AT END                                                             
056700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
056800         DELIMITED BY SIZE INTO ERROR-TEXT                                
056900         CALL FELLOG                                                      
057000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
057100         CONTINUE                                                         
057200     END-SEARCH                                                           
057300     .                                                                    
