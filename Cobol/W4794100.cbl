000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4794100.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   MARCH 2009.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        TO CREATE REPORTS BASED ON THE DISTRICT NUMBER FROM              
001000*        W47940 AND SEND THEM VIA DISTRIBUTION PRINT                      
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U1000 - D&P ERROR                                                
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL                                                      
002400     SELECT W47940                     ASSIGN TO W47941D1.                
002500     SKIP2                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W47940                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W4794001  -L.                                                  
003500     SKIP3                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W4794100'.            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  SAVE-IDDISTR                PIC 9(5)    VALUE ZERO.                  
004200 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
004300 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  WS-TEST-CNT                 PIC S9(3)   VALUE ZERO.                  
004600 77  WS-TXT-MSG1                 PIC X(50)   VALUE SPACE.                 
004700 77  WS-TXT-MSG2                 PIC X(82)  VALUE SPACE.                  
004800                                                                          
004900 77  W47940-EOF-SW               PIC X       VALUE 'N'.                   
005000     88  END-OF-W47940                       VALUE 'Y'.                   
005100     EJECT                                                                
005200*                                                                         
005300 01  HDR-AREA.                                                            
005400*    03  -COPY WZ01REQU                                                   
005500*    03  -COPY WZ04HDR                                                    
005600     EJECT                                                                
005700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
005800 01  SEND-AREA.                                                           
005900*    03  -COPY WZ01SEND                                                   
006000     EJECT                                                                
006100 01  SEND-RAD-STYRTECKEN.                                                 
006200*    03  STYRTECKEN-RAD          PIC X.                                   
006300     03  SEND-RAD                PIC X(121)  VALUE SPACE.                 
006400*    --- CONTROL CHARACTERS                                               
006500 01  WS-SKIP1                    PIC X       VALUE ' '.                   
006600 01  WS-SKIP2                    PIC X       VALUE '0'.                   
006700 01  WS-SKIP3                    PIC X       VALUE '-'.                   
006800 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
006900     EJECT                                                                
007000                                                                          
007100 01  W-VIMSID.                                                            
007200     03  W-IMSID                 PIC X(4)    VALUE SPACE.                 
007300     03  FILLER                  PIC X(4)    VALUE SPACE.                 
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
007600                                                                          
007700     EJECT                                                                
007800*    --- LISTLAYOUT                                                       
007900 01  LISTA.                                                               
008000     03  RUBRIK-1.                                                        
008100         05  FILLER       PIC X       VALUE SPACE.                        
008200         05  FILLER       PIC X(18)   VALUE 'ORDERS IN STATUS E'.         
008300         05  FILLER       PIC X(3)    VALUE SPACE.                        
008400         05  FILLER       PIC X(8)    VALUE 'DISTRICT'.                   
008500         05  FILLER       PIC X       VALUE SPACE.                        
008600         05  RUB1-IDDISTR PIC ZZZ9    VALUE ZERO.                         
008700         05  FILLER       PIC X(3)    VALUE SPACE.                        
008800                                                                          
008900     03  RUBRIK-2.                                                        
009000         05  FILLER           PIC X(4)    VALUE SPACE.                    
009100         05  RUB2-IDKUNDNR    PIC X(08)   VALUE 'DealerNo'.               
009200         05  FILLER           PIC X(2)    VALUE SPACE.                    
009300         05  RUB2-TIREGDAT    PIC X(07)   VALUE 'RegDate'.                
009400         05  FILLER           PIC X(2)    VALUE SPACE.                    
009500         05  RUB2-IDORDER     PIC X(7)    VALUE 'OrderNo'.                
009600         05  FILLER           PIC X(2)    VALUE SPACE.                    
009700         05  RUB2-KDORDKL     PIC X(2)    VALUE 'Cl'.                     
009800         05  FILLER           PIC X(2)    VALUE SPACE.                    
009900         05  RUB2-IDSYSTEM    PIC X(6)    VALUE 'System'.                 
010000         05  FILLER           PIC X(2)    VALUE SPACE.                    
010100         05  RUB2-IDPTYP      PIC X(4)    VALUE 'Type'.                   
010200         05  FILLER           PIC X(2)    VALUE SPACE.                    
010300         05  RUB2-IDUSER      PIC X(07)   VALUE 'RegResp'.                
010400         05  FILLER           PIC X(3)    VALUE SPACE.                    
010500         05  RUB2-CAUSE       PIC X(05)   VALUE 'Cause'.                  
010600         05  FILLER           PIC X(16)   VALUE SPACE.                    
010700         05  RUB2-IDDC        PIC X(2)    VALUE 'DC'.                     
010800                                                                          
010900     03  RAD.                                                             
011000         05  FILLER           PIC X(4)    VALUE SPACE.                    
011100         05  RAD-IDKUNDNR     PIC ZZZZZZ9.                                
011200         05  FILLER           PIC X(3)    VALUE SPACE.                    
011300         05  RAD-TIREGDAT     PIC ZZZZZZ.                                 
011400         05  FILLER           PIC X(3)    VALUE SPACE.                    
011500         05  RAD-IDORDER      PIC ZZZZZZ9.                                
011600         05  FILLER           PIC X(2)    VALUE SPACE.                    
011700         05  RAD-KDORDKL      PIC 9.                                      
011800         05  FILLER           PIC X(3)    VALUE SPACE.                    
011900         05  RAD-IDSYSTEM     PIC X(4).                                   
012000         05  FILLER           PIC X(4)    VALUE SPACE.                    
012100         05  RAD-IDPTYP       PIC X(3).                                   
012200         05  FILLER           PIC X(3)    VALUE SPACE.                    
012300         05  RAD-IDUSER       PIC X(8).                                   
012400         05  FILLER           PIC X(2)    VALUE SPACE.                    
012500         05  RAD-CAUSE        PIC X(18).                                  
012600         05  FILLER           PIC X(3)    VALUE SPACE.                    
012700         05  RAD-IDDC         PIC X(2)    VALUE SPACE.                    
012800                                                                          
012900                                                                          
013000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013100 01  FILLER REDEFINES DAGENS-DATUM.                                       
013200     03  DAGENS-AA               PIC 9(2).                                
013300     03  DAGENS-MM               PIC 9(2).                                
013400     03  DAGENS-DD               PIC 9(2).                                
013500     EJECT                                                                
013600 01  GENERAL-SUBPROGRAMS.                                                 
013700*                                                                         
013800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014100     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
014200     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
014300     SKIP2                                                                
014400*    --- PARAMETERS TO ABEND                                              
014500                                                                          
014600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014900     SKIP2                                                                
015000 01  ERRTEXT.                                                             
015100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
015200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL POSTSUM                                          
015500*                                                                         
015600*01  -COPY W0005   -PRE  POSTSUM-                                         
015700     EJECT                                                                
015800 01  IN-AREA-START               PIC X(24)   VALUE                        
015900                                 'IN-AREA-START  '.                       
016000     SKIP2                                                                
016100*01  AREA -COPY W4794001    -PRE IN-                                      
016200     EJECT                                                                
016300                                                                          
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600*01  -COPY W0009            -PRE MSG-                                     
016700                                                                          
016800*01  -COPY W0009            -PRE DISTRDOC-                                
016900     EJECT                                                                
017000 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
017100 MAIN SECTION.                                                            
017200     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
017300                                                                          
017400     PERFORM A-INIT                                                       
017500                                                                          
017600     PERFORM S01-LAS-W47940                                               
017700                                                                          
017800     PERFORM UNTIL END-OF-W47940                                          
017900       MOVE IN-IDDISTR  TO SAVE-IDDISTR                                   
018000                           WS-IDDISTR                                     
018100       PERFORM S90-SEND-OPEN                                              
018200       PERFORM S90-PUT-DAP-START                                          
018300                                                                          
018400       PERFORM B-INIT-IDDISTR                                             
018500       PERFORM C-PRINT-HEAD                                               
018600                                                                          
018700       PERFORM UNTIL END-OF-W47940 OR                                     
018800          SAVE-IDDISTR NOT = IN-IDDISTR                                   
018900         PERFORM D-RAD-DATA                                               
019000         PERFORM S01-LAS-W47940                                           
019100       END-PERFORM                                                        
019200       PERFORM E-PRINT-TEXT-MSG                                           
019300       PERFORM S90-SEND-CLOSE                                             
019400*****  STOP SENDING MAILS IF THE COUNTER EXCEEDS 10                       
019500*****  IN TEST ENVIRONMENTS                                               
019600       ADD 1 TO WS-TEST-CNT                                               
019700       IF W-IMSID NOT = 'QASE'                                            
019800         IF WS-TEST-CNT >= 10                                             
019900           SET END-OF-W47940 TO TRUE                                      
020000         END-IF                                                           
020100       END-IF                                                             
020200     END-PERFORM                                                          
020300                                                                          
020400     PERFORM Z-FINIT                                                      
020500                                                                          
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 A-INIT SECTION.                                                          
021100                                                                          
021200     OPEN INPUT  W47940                                                   
021300     ACCEPT DAGENS-DATUM FROM DATE                                        
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500                                                                          
021600     CALL VIMSID            USING W-IMSID                                 
021700     IF W-IMSID(1:3) = 'IMG'                                              
021800       MOVE 'QASE' TO W-IMSID                                             
021900     END-IF                                                               
022000     IF W-IMSID(1:3) = 'IMP'                                              
022100       MOVE 'DEVE' TO W-IMSID                                             
022200     END-IF                                                               
022300     IF W-IMSID(1:3) = 'IMY'                                              
022400       MOVE 'IGRT' TO W-IMSID                                             
022500     END-IF                                                               
022600     IF W-IMSID(1:3) = 'IMD'                                              
022700       MOVE 'XDEV' TO W-IMSID                                             
022800     END-IF                                                               
022900     IF W-IMSID(1:3) = 'IMB'                                              
023000       MOVE 'ACPT' TO W-IMSID                                             
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 B-INIT-IDDISTR SECTION.                                                  
023500                                                                          
023600     MOVE IN-IDDISTR                 TO RUB1-IDDISTR                      
023700     .                                                                    
023800     EJECT                                                                
023900 C-PRINT-HEAD SECTION.                                                    
024000                                                                          
024100     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
024200     MOVE SPACE                      TO SEND-RAD                          
024300     PERFORM S90-PUT-DOC-LINE                                             
024400     MOVE IN-IDDISTR                 TO RUB1-IDDISTR                      
024500     MOVE RUBRIK-1                   TO SEND-RAD                          
024600     PERFORM S90-PUT-DOC-LINE                                             
024700                                                                          
024800     MOVE SPACE                      TO SEND-RAD                          
024900     PERFORM S90-PUT-DOC-LINE                                             
025000     MOVE RUBRIK-2                   TO SEND-RAD                          
025100     PERFORM S90-PUT-DOC-LINE                                             
025200                                                                          
025300     MOVE SPACE                      TO SEND-RAD                          
025400     PERFORM S90-PUT-DOC-LINE                                             
025500                                                                          
025600     .                                                                    
025700     EJECT                                                                
025800 D-RAD-DATA SECTION.                                                      
025900                                                                          
026000     INITIALIZE RAD.                                                      
026100                                                                          
026200     MOVE IN-IDKUNDNR     TO RAD-IDKUNDNR                                 
026300     MOVE IN-TIREGDAT     TO RAD-TIREGDAT                                 
026400     MOVE IN-IDORDNR7     TO RAD-IDORDER                                  
026500     MOVE IN-KDORDKL      TO RAD-KDORDKL                                  
026600     MOVE IN-IDSYSTEM     TO RAD-IDSYSTEM                                 
026700     MOVE IN-IDPTYP       TO RAD-IDPTYP                                   
026800     MOVE IN-IDUSER       TO RAD-IDUSER                                   
026900     MOVE IN-IDDC         TO RAD-IDDC                                     
027000                                                                          
027100     IF IN-IDPTYP = '001'                                                 
027200       MOVE 'ORDER LINE        '      TO RAD-CAUSE                        
027300     ELSE                                                                 
027400       IF IN-IDPTYP = '002'                                               
027500          MOVE 'BACK ORDER        '   TO RAD-CAUSE                        
027600       ELSE                                                               
027700         IF IN-IDPTYP = '003'                                             
027800            MOVE 'ORDER CONFIRMATION' TO RAD-CAUSE                        
027900         ELSE                                                             
028000           IF IN-IDPTYP = '004'                                           
028100             MOVE 'PRINT LINE       ' TO RAD-CAUSE                        
028200           END-IF                                                         
028300         END-IF                                                           
028400       END-IF                                                             
028500     END-IF                                                               
028600                                                                          
028700     MOVE RAD TO SEND-RAD                                                 
028800     PERFORM S90-PUT-DOC-LINE                                             
028900     .                                                                    
029000     EJECT                                                                
029100 E-PRINT-TEXT-MSG SECTION.                                                
029200                                                                          
029300     MOVE SPACE                      TO SEND-RAD                          
029400     PERFORM S90-PUT-DOC-LINE                                             
029500                                                                          
029600     MOVE SPACE TO WS-TXT-MSG1                                            
029700     STRING 'This is an automatic mail from the PULS system.'             
029800     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
029900     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
030000     PERFORM S90-PUT-DOC-LINE                                             
030100                                                                          
030200     MOVE SPACE                      TO SEND-RAD                          
030300     PERFORM S90-PUT-DOC-LINE                                             
030400                                                                          
030500     MOVE SPACE TO WS-TXT-MSG1                                            
030600     STRING 'There is unfinished order/orders from your order e'          
030700     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
030800                                                                          
030900     MOVE SPACE TO WS-TXT-MSG2                                            
031000     STRING WS-TXT-MSG1  'ntry process which needs to be'                 
031100     DELIMITED BY SIZE INTO WS-TXT-MSG2                                   
031200     MOVE WS-TXT-MSG2 TO SEND-RAD                                         
031300     PERFORM S90-PUT-DOC-LINE                                             
031400                                                                          
031500     MOVE SPACE TO WS-TXT-MSG1                                            
031600     STRING 'started or cancelled, (lines in status E).'                  
031700     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
031800     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
031900     PERFORM S90-PUT-DOC-LINE                                             
032000                                                                          
032100     MOVE SPACE                      TO SEND-RAD                          
032200     PERFORM S90-PUT-DOC-LINE                                             
032300                                                                          
032400     MOVE SPACE TO WS-TXT-MSG1                                            
032500     STRING 'To make corrections, choose screen as:'                      
032600     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
032700     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
032800     PERFORM S90-PUT-DOC-LINE                                             
032900                                                                          
033000     MOVE SPACE TO WS-TXT-MSG1                                            
033100     STRING '   * 4213 if System in the report is 4211'                   
033200     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
033300     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
033400     PERFORM S90-PUT-DOC-LINE                                             
033500                                                                          
033600     MOVE SPACE TO WS-TXT-MSG1                                            
033700     STRING '   * 4223 if System in the report is 4221'                   
033800     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
033900     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
034000     PERFORM S90-PUT-DOC-LINE                                             
034100                                                                          
034200     MOVE SPACE TO WS-TXT-MSG1                                            
034300     STRING '   * 4233 if System in the report is 4231'                   
034400     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
034500     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
034600     PERFORM S90-PUT-DOC-LINE                                             
034700                                                                          
034800     MOVE SPACE TO WS-TXT-MSG1                                            
034900     STRING '   * 4243 if System in the report is 4241'                   
035000     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
035100     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
035200     PERFORM S90-PUT-DOC-LINE                                             
035300                                                                          
035400     MOVE SPACE                      TO SEND-RAD                          
035500     PERFORM S90-PUT-DOC-LINE                                             
035600                                                                          
035700     MOVE SPACE TO WS-TXT-MSG1                                            
035800     STRING 'Specify district, customer and order number on the'          
035900     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
036000                                                                          
036100     MOVE SPACE TO WS-TXT-MSG2                                            
036200     STRING WS-TXT-MSG1  ' screen and press enter.'                       
036300     DELIMITED BY SIZE INTO WS-TXT-MSG2                                   
036400     MOVE WS-TXT-MSG2 TO SEND-RAD                                         
036500     PERFORM S90-PUT-DOC-LINE                                             
036600                                                                          
036700     MOVE SPACE                      TO SEND-RAD                          
036800     PERFORM S90-PUT-DOC-LINE                                             
036900                                                                          
037000     MOVE SPACE TO WS-TXT-MSG1                                            
037100     STRING '- If you get information: 701 ORDERN SAKNAS / 701'           
037200     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
037300                                                                          
037400     MOVE SPACE TO WS-TXT-MSG2                                            
037500     STRING WS-TXT-MSG1  'ORDER MISSING, the order is'                    
037600     DELIMITED BY SIZE INTO WS-TXT-MSG2                                   
037700     MOVE WS-TXT-MSG2 TO SEND-RAD                                         
037800     PERFORM S90-PUT-DOC-LINE                                             
037900                                                                          
038000     MOVE SPACE TO WS-TXT-MSG1                                            
038100     STRING '  missing & you do not need to take further action.'         
038200     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
038300     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
038400     PERFORM S90-PUT-DOC-LINE                                             
038500                                                                          
038600     MOVE SPACE                      TO SEND-RAD                          
038700     PERFORM S90-PUT-DOC-LINE                                             
038800                                                                          
038900     MOVE SPACE TO WS-TXT-MSG1                                            
039000     STRING '- To keep or cancel the order, do one of:'                   
039100     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
039200     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
039300     PERFORM S90-PUT-DOC-LINE                                             
039400                                                                          
039500     MOVE SPACE TO WS-TXT-MSG1                                            
039600     STRING '   * If you want the order, press enter.'                    
039700     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
039800     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
039900     PERFORM S90-PUT-DOC-LINE                                             
040000                                                                          
040100     MOVE SPACE TO WS-TXT-MSG1                                            
040200     STRING '   * If you want to cancel the order, put J/Y to A'          
040300     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
040400                                                                          
040500     MOVE SPACE TO WS-TXT-MSG2                                            
040600     STRING WS-TXT-MSG1 'nullation/Cancellation on the'                   
040700     DELIMITED BY SIZE INTO WS-TXT-MSG2                                   
040800     MOVE WS-TXT-MSG2 TO SEND-RAD                                         
040900     PERFORM S90-PUT-DOC-LINE                                             
041000                                                                          
041100     MOVE SPACE TO WS-TXT-MSG1                                            
041200     STRING '     screen, press enter.'                                   
041300     DELIMITED BY SIZE INTO WS-TXT-MSG1                                   
041400     MOVE WS-TXT-MSG1 TO SEND-RAD                                         
041500     PERFORM S90-PUT-DOC-LINE                                             
041600                                                                          
042900     .                                                                    
043000     EJECT                                                                
043100                                                                          
043200 Z-FINIT SECTION.                                                         
043300                                                                          
043400     CLOSE W47940                                                         
043500     MOVE 'S' TO POSTSUM-OPKOD                                            
043600     CALL POSTSUM USING POSTSUM-PARM                                      
043700     .                                                                    
043800     EJECT                                                                
043900 S01-LAS-W47940 SECTION.                                                  
044000     READ W47940 INTO IN-AREA                                             
044100     AT END                                                               
044200        MOVE HIGH-VALUE TO IN-AREA                                        
044300        SET END-OF-W47940 TO TRUE                                         
044400     NOT AT END                                                           
044500        MOVE 'W47940'   TO POSTSUM-FDNAMN                                 
044600        MOVE 'W47941D1' TO POSTSUM-DDNAMN2                                
044700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
044800        CALL POSTSUM USING POSTSUM-PARM                                   
044900     END-READ                                                             
045000     .                                                                    
045100     EJECT                                                                
045200 S90-SEND-OPEN SECTION.                                                   
045300                                                                          
045400     MOVE 'OPEN'                        TO SEND-KDFUNC                    
045500     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
045600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045700                         SEND-OPEN-AREA                                   
045800     IF SEND-KDRC > 0                                                     
045900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
046000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
046100       DELIMITED BY SIZE INTO ERRTEXT                                     
046200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 S90-PUT-DAP-START SECTION.                                               
046700                                                                          
046800     MOVE 1                       TO REQU-IDMSGVER                        
046900     MOVE SPACE                   TO REQU-KDPGMACT                        
047000     MOVE SPACE                   TO REQU-IDUSER                          
047100     MOVE 'ORDER-STATUS-E'        TO HDR-IDOUTTYPE                        
047200     MOVE SPACE                   TO HDR-IDOUTREC                         
047300                                     HDR-IDLIST                           
047400     MOVE WS-IDDISTR              TO HDR-IDOUTREC (1:4)                   
047500                                     HDR-IDLIST                           
047600     MOVE 'PUT'                   TO SEND-KDFUNC                          
047700     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
047800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
047900                         SEND-KVDLEN                                      
048000                         HDR-AREA                                         
048100     IF SEND-KDRC > ZERO                                                  
048200       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
048300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
048400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
048500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 S90-PUT-DOC-LINE SECTION.                                                
049000                                                                          
049100     MOVE 'PUT'                           TO SEND-KDFUNC                  
049200*                     -- UTAN STYRTECKEN:                                 
049300     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
049400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049500                         SEND-KVDLEN                                      
049600*                     -- UTAN STYRTECKEN:                                 
049700                         SEND-RAD                                         
049800     IF SEND-KDRC > ZERO                                                  
049900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
050000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
050100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
050200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 S90-SEND-CLOSE SECTION.                                                  
050700                                                                          
050800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
050900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
051000                                                                          
051100     IF SEND-KDRC > 0                                                     
051200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
051300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
051400       DELIMITED BY SIZE INTO ERRTEXT                                     
051500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
