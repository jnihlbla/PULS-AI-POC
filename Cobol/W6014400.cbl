000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014400.                                                
000400*AUTHOR.         LARS THELL / UMESH JAIN                                  
000500*DATE-WRITTEN.   92/06/25. / NOV 2011                                     
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        RAPPORTERA PÅ PARTI. INLAGT, AVVIKELSE                           
001100*                                                                         
001200*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T144, W6T144U            
001300*        AND W6T144V                                                      
001400*                                                                         
001500*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
001600*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6014410 WHICH             
001700*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
001800*                                                                         
001900*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
002000*        EXISTS - W6W14400 (TRANSACTIONS W6T144, W6T144U &                
002100*        W6T144V)                                                         
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T144                                              
002500*        MID:         W6I14401                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O14401                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W6014400'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
004800 77  MAX-REQU-INDX               PIC S9(4)  VALUE +50   COMP SYNC.        
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +707  COMP SYNC.        
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300                                                                          
005400 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005500 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005600 77  WS-IDLOPNRM                 PIC X(8)    VALUE SPACE.                 
005700                                                                          
005800 01  WS-IDDC-LOCAL.                                                       
005900     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
006000     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
006100     03  FILLER                  PIC X(1)   VALUE SPACE.                  
006200                                                                          
006300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006400     88  EGEN-MID                            VALUE '6144'.                
006500     88  GODK-MID                            VALUE '6143' '6144'          
006600                                                   '6145'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006900 01  SAVE-AREA.                                                           
007000     03  SAVE-IDTRANS            PIC X(4)    VALUE '6144'.                
007100     03  SAVE-IDRADNR-ENTER      PIC 9(5).                                
007200     03  SAVE-IDRADNR-NEXT       PIC 9(5).                                
007300                                                                          
007400*      --- VALID IDDC CODES                                               
007500*                                                                         
007600*01    -COPY WWDC99                                                       
007700       EJECT                                                              
007800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007900 01  GENERELLA-SUBPROGRAM.                                                
008000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008400     03  W6014410                PIC X(8)    VALUE 'W6014410'.            
008500     EJECT                                                                
008600*01 -COPY WMSGINIT                                                        
008700     SKIP3                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*01 -COPY WMEDAREA                                                        
009000     SKIP3                                                                
009100 01 WS-IDMSG-ERROR         PIC X(3).                                      
009200    88 WRONG-KEY           VALUE '022'.                                   
009300    88 CORR-HILITE-FLDS    VALUE '020'.                                   
009400    88 DIVERSE             VALUE '354'.                                   
009500    88 PF11-AND-NO-DATA    VALUE '014'.                                   
009600    88 UPDATE-NOT-ALLOWED  VALUE '007'.                                   
009700    88 MISSING             VALUE '027'.                                   
009800    88 INVALID-FIELD       VALUE '023'.                                   
009900    88 QUALITY             VALUE '357'.                                   
010000    88 CHECK-QUAL-FIRST    VALUE '355'.                                   
010100    88 CONTROL-NOT-COMPL   VALUE '356'.                                   
010200    88 PLACE-MISSING       VALUE '313'.                                   
010300    88 PLACE-MISSING-SVS   VALUE '314'.                                   
010400    88 WEIGHT-MISSING      VALUE '310'.                                   
010500    88 VOLUME-MISSING      VALUE '311'.                                   
010600    88 ORIGIN-MISSING      VALUE '312'.                                   
010700                                                                          
010800 01 WS-IDMSG-INFO          PIC X(3).                                      
010900    88 DEV-VALUE-TOO-HIGH  VALUE '359'.                                   
011000    88 FIRST-PAGE          VALUE '010'.                                   
011100    88 PRESS-PF11          VALUE '013'.                                   
011200    88 NO-MORE-LINES       VALUE '316'.                                   
011300    88 UPDATE-DONE         VALUE '001'.                                   
011400    88 MORE-LINES          VALUE '011'.                                   
011500                                                                          
011600 01  MESSAGE-CODES.                                                       
011700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012000     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '056'.                 
012100     03  ERR-MISSING             PIC X(3)    VALUE '010'.                 
012200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012300     03  ERR-NO-UPDATE           PIC X(3)    VALUE '034'.                 
012400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012600     03  ERR-DIVERSE             PIC X(3)    VALUE '182'.                 
012700     03  ERR-QUALITY             PIC X(3)    VALUE '189'.                 
012800     03  ERR-RTYP-3-OR-77        PIC X(3)    VALUE '227'.                 
012900     03  ERR-CHECK-QUAL-FIRST    PIC X(3)    VALUE '228'.                 
013000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013100     03  ERR-PLACE-MISSING       PIC X(3)    VALUE '764'.                 
013200     03  ERR-PLACE-MISSING-SVS   PIC X(3)    VALUE '???'.                 
013300     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
013400     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
013500     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
013600     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
013700     03  ERR-CONTROL-NOT-COMPL   PIC X(3)    VALUE '215'.                 
013800     03  INF-DEV-VALUE-TOO-HIGH  PIC X(3)    VALUE '349'.                 
013900     EJECT                                                                
014000******************************************************************        
014100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014400     SKIP3                                                                
014500*01  MID -COPY W6I14401                                                   
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014800     SKIP3                                                                
014900*01  -COPY WMSGAREA                                                       
015000     EJECT                                                                
015100     03  MOD REDEFINES MSG-AREA.                                          
015200*      05  -COPY W6O14401                                                 
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015500     SKIP3                                                                
015600*01  -COPY WMFSAREA                                                       
015700     EJECT                                                                
015800******************************************************************        
015900*                                                                         
016000*                AREOR FÖR ANROP TILL W6014410                            
016100*                                                                         
016200 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
016300 01  REQU-AREA.                                                           
016400     03 -COPY WZ01REQU                                                    
016500     03 -COPY W60144I1                                                    
016600 77  MAX-KVRADER               PIC S9(4) COMP VALUE +08.                  
016700                                                                          
016800 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
016900 01  RESP-AREA.                                                           
017000     03 -COPY WZ01RESP                                                    
017100     03 -COPY W60144O1                                                    
017200                                                                          
017300******************************************************************        
017400     EJECT                                                                
017500*    --- STATUS-KOD FRÅN IMS                                              
017600 01  STATUS-WS                   PIC XX.                                  
017700     88  SEGMENT-FINNS                       VALUE '  '.                  
017800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018000     SKIP2                                                                
018100 01  GODK-STATUSKODER.                                                    
018200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018300     SKIP3                                                                
018400 01  SSA1                        PIC X(96).                               
018500 01  SSA2                        PIC X(64).                               
018600     EJECT                                                                
018700*    --- IMS FUNKTIONSKODER                                               
018800*01  -COPY W0003                                                          
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100                                                                          
019200*01  -COPY W0009   -PRE MSG-                                              
019300 01  ALT1-PCB                 PIC X.                                      
019400 01  DISP-PCB                 PIC X.                                      
019500*01  -COPY W0008  -PRE USEA-                                              
019600     05  FILLER               PIC X.                                      
019700 01  INLA1-PCB                PIC X.                                      
019800 01  INLA2-PCB                PIC X.                                      
019900 01  INLC-PCB                 PIC X.                                      
020000 01  PLAA-PCB                 PIC X.                                      
020100 01  ARTD-PCB                 PIC X.                                      
020200 01  ARTS-PCB                 PIC X.                                      
020300 01  UPFA-PCB                 PIC X.                                      
020400 01  ARTC-PCB                 PIC X.                                      
020500 01  WDB6-PCB                 PIC X.                                      
020600                                                                          
020700**  PCB'ER FÖR SUBPGM WITHIN W6014410                                     
020800 01  PMRK-INLB-PCB               PIC X.                                   
020900 01  PMRK-INLC-PCB               PIC X.                                   
021000 01  PMRK-PLAA-PCB               PIC X.                                   
021100 01  STYR-HANA-PCB               PIC X.                                   
021200 01  STYR-PLAA-PCB               PIC X.                                   
021300 01  KOM-KOMA-PCB                PIC X.                                   
021400                                                                          
021500     EJECT                                                                
021600 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
021700                           INLA1-PCB                                      
021800                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
021900                           ARTS-PCB                                       
022000                           UPFA-PCB ARTC-PCB WDB6-PCB                     
022100                           PMRK-INLB-PCB PMRK-INLC-PCB                    
022200                           PMRK-PLAA-PCB                                  
022300                           STYR-HANA-PCB                                  
022400                           STYR-PLAA-PCB                                  
022500                           KOM-KOMA-PCB.                                  
022600                                                                          
022700     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
022800                           INLA1-PCB                                      
022900                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
023000                           ARTS-PCB                                       
023100                           UPFA-PCB ARTC-PCB WDB6-PCB                     
023200                           PMRK-INLB-PCB PMRK-INLC-PCB                    
023300                           PMRK-PLAA-PCB                                  
023400                           STYR-HANA-PCB                                  
023500                           STYR-PLAA-PCB                                  
023600                           KOM-KOMA-PCB.                                  
023700                                                                          
023800     PERFORM IMS-GET-MSG                                                  
023900     IF SEGMENT-FINNS                                                     
024000       PERFORM A-INIT                                                     
024100       PERFORM B-INIT-KEYS                                                
024200       PERFORM C-INIT-REQU                                                
024300       IF MFS-UPDATE                                                      
024400         SET REQU-UPDATE TO TRUE                                          
024500         MOVE SAVE-IDRADNR-ENTER  TO REQU-IDRADNR-START                   
024600       ELSE                                                               
024700         IF MFS-UPD-V                                                     
024800           SET REQU-UPD-V TO TRUE                                         
024900           MOVE SAVE-IDRADNR-ENTER  TO REQU-IDRADNR-START                 
025000         ELSE                                                             
025100           IF MFS-FIRST                                                   
025200             SET REQU-FIRST TO TRUE                                       
025300             PERFORM MFS-RENSA-FAELT-IN                                   
025400             PERFORM MFS-RENSA-FAELT-UT                                   
025500             MOVE ZERO                 TO REQU-IDRADNR-START              
025600           ELSE                                                           
025700             IF MFS-NEXT                                                  
025800               SET REQU-NEXT  TO TRUE                                     
025900               MOVE SAVE-IDRADNR-NEXT   TO REQU-IDRADNR-START             
026000             ELSE                                                         
026100               SET REQU-QUERY TO TRUE                                     
026200               MOVE SAVE-IDRADNR-ENTER  TO REQU-IDRADNR-START             
026300             END-IF                                                       
026400           END-IF                                                         
026500         END-IF                                                           
026600       END-IF                                                             
026700       PERFORM F-BUSINESS-LOGIC-W6014410                                  
026800*                                                                         
026900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027000       PERFORM IMS-INSERT-MSG                                             
027100     END-IF                                                               
027200                                                                          
027300     MOVE ZERO TO RETURN-CODE                                             
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800     MOVE SPACE                TO MED-IDMFSINF                            
027900                                                                          
028000     IF MSG-DUBBLA-TRANSKODER                                             
028100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14401                 
028200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028400     ELSE                                                                 
028500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14401                  
028600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028800     END-IF                                                               
028900                                                                          
029000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
029100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029300                                                                          
029400     MOVE LOW-VALUE TO MSG-AREA                                           
029500     MOVE 'W6O144N1' TO MFS-IDMOD                                         
029600     MOVE '6144' TO MOD-IDTRANS                                           
029700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029800                                                                          
029900     IF EGEN-MID OR HELP-MID                                              
030000       CONTINUE                                                           
030100     ELSE                                                                 
030200       MOVE SPACE TO MFS-KDTRTYP                                          
030300       MOVE '7' TO MFS-IDPFK                                              
030400     END-IF                                                               
030500     IF MID-IDLOPNRM-IN NOT = ALL '+' OR                                  
030600        MID-IDLEVNR-KOLLI-IN NOT = ALL '+' OR                             
030700        MID-IDOKOLLI-IN NOT = ALL '+' OR                                  
030800        MID-IDDC-IN NOT = ALL '+'                                         
030900       MOVE SPACE TO MFS-KDTRTYP                                          
031000       MOVE '7' TO MFS-IDPFK                                              
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400*                                                                         
031500 B-INIT-KEYS SECTION.                                                     
031600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
031700     MOVE '001'                  TO MSGI-KDCALL                           
031800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
031900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
032000     MOVE '6144'                 TO MSGI-IDTRANS                          
032100     IF EGEN-MID                                                          
032200       MOVE MID-IDLOPNRM-IN(2:8) TO MSGI-IDLOPNRM                         
032300       MOVE MID-IDLEVNR-KOLLI-IN TO MSGI-IDLEVNR                          
032400       MOVE MID-IDOKOLLI-IN      TO MSGI-IDOKOLLI                         
032500       IF MID-IDLEVNR-KOLLI-IN NOT = ALL '+' OR                           
032600          MID-IDOKOLLI-IN NOT = ALL '+'                                   
032700         MOVE ZEROES             TO MSGI-IDLOPNRM                         
032800       END-IF                                                             
032900       MOVE MID-IDDC-IN          TO MSGI-IDDC                             
033000     ELSE                                                                 
033100       MOVE ALL '+'              TO MID-IDLOPNRM-IN                       
033200                                    MID-IDLEVNR-KOLLI-IN                  
033300                                    MID-IDOKOLLI-IN                       
033400                                    MID-IDDC-IN                           
033500     END-IF                                                               
033600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033700     MOVE MSGI-SPAR-AREA         TO SAVE-AREA                             
033800     IF MSGI-IDLAND-SPR = 'GB'                                            
033900       MOVE +2                   TO SPRAK-IX                              
034000       MOVE 'GB '                TO MED-IDSKYLT                           
034100     ELSE                                                                 
034200       MOVE +1                   TO SPRAK-IX                              
034300       MOVE 'S  '                TO MED-IDSKYLT                           
034400     END-IF                                                               
034500     PERFORM BA-INIT-NYCKLAR                                              
034600     PERFORM BB-INIT-IDDC                                                 
034700     MOVE MFS-RENSA-FAELT        TO MOD-IDLOPNRM-IN                       
034800                                    MOD-IDLEVNR-KOLLI-IN                  
034900                                    MOD-IDOKOLLI-IN                       
035000                                    MOD-IDDC-IN                           
035100     .                                                                    
035200     EJECT                                                                
035300*                                                                         
035400 BA-INIT-NYCKLAR SECTION.                                                 
035500     MOVE MSGI-IDLOPNRM          TO WS-IDLOPNRM                           
035600     INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                  
035700                                                                          
035800     MOVE ZEROS                  TO REQU-IDLOPNRM-KEY                     
035900     IF WS-IDLOPNRM  NUMERIC                                              
036000       MOVE WS-IDLOPNRM          TO REQU-IDLOPNRM-KEY(2:8)                
036100     END-IF                                                               
036200                                                                          
036300     MOVE REQU-IDLOPNRM-KEY      TO MOD-IDLOPNRM-UT                       
036400     INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE              
036500                                                                          
036600     MOVE MSGI-IDLEVNR           TO WS-IDLEVNR-KOLLI                      
036700                                    MOD-IDLEVNR-KOLLI-UT                  
036800     MOVE MSGI-IDOKOLLI          TO WS-IDOKOLLI                           
036900                                    MOD-IDOKOLLI-UT                       
037000                                                                          
037100     INSPECT WS-IDOKOLLI  REPLACING LEADING SPACE BY ZERO                 
037200     IF WS-IDLEVNR-KOLLI NOT = SPACE  AND                                 
037300        WS-IDOKOLLI   NUMERIC                                             
037400       MOVE WS-IDOKOLLI          TO REQU-IDOKOLLI-KEY                     
037500       MOVE MSGI-IDLEVNR         TO REQU-IDLEVNR-KOLLI-KEY                
037600     ELSE                                                                 
037700       MOVE ZEROS                TO WS-IDOKOLLI                           
037800       MOVE SPACE                TO WS-IDLEVNR-KOLLI                      
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 BB-INIT-IDDC        SECTION.                                             
038300     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-IN                           
038400     IF MID-IDDC-IN = ALL '+'                                             
038500       MOVE MSGI-IDDC            TO WS-IDDC                               
038600                                    REQU-IDDC-KEY                         
038700                                    MOD-IDDC-UT                           
038800     ELSE                                                                 
038900       MOVE MID-IDDC-IN          TO WS-IDDC                               
039000                                    REQU-IDDC-KEY                         
039100                                    MOD-IDDC-UT                           
039200       MOVE '7'                  TO MFS-IDPFK                             
039300       MOVE SPACE                TO MFS-KDTRTYP                           
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 C-INIT-REQU   SECTION.                                                   
039800                                                                          
039900     MOVE MAX-KVRADER            TO REQU-KVRADER                          
040000*                                                                         
040100     MOVE MID-IDANSTNR           TO REQU-IDANSTNR                         
040200     MOVE +1                     TO INDX                                  
040300     PERFORM UNTIL INDX > MAX-INDX                                        
040400       MOVE MID-KDCMDVAL-RAD(INDX)                                        
040500                                 TO REQU-KDCMDVAL-LINE(INDX)              
040600       MOVE MID-KVINLART-UPD(INDX)                                        
040700                                 TO REQU-KVINLART-UPD-LINE (INDX)         
040800       MOVE MID-ADINLOMR-NXT-UPD(INDX)                                    
040900                              TO REQU-ADINLOMR-NXT-UPD-LINE(INDX)         
041000       MOVE MID-IDRADNR-RAD(INDX)                                         
041100                                 TO REQU-IDRADNR-LINE(INDX)               
041200       ADD +1                    TO INDX                                  
041300     END-PERFORM                                                          
041400     PERFORM UNTIL INDX > MAX-REQU-INDX                                   
041500       MOVE ALL '+'              TO REQU-KDCMDVAL-LINE(INDX)              
041600                                    REQU-KVINLART-UPD-LINE (INDX)         
041700                                 REQU-ADINLOMR-NXT-UPD-LINE(INDX)         
041800                                    REQU-IDRADNR-LINE(INDX)               
041900       ADD +1 TO INDX                                                     
042000     END-PERFORM                                                          
042100                                                                          
042200     IF MSGI-IDLAND-SPR = 'SE'                                            
042300       MOVE 'SV'                 TO REQU-IDSPRAK                          
042400     ELSE                                                                 
042500       MOVE 'EN'                 TO REQU-IDSPRAK                          
042600     END-IF                                                               
042700                                                                          
042800     .                                                                    
042900     EJECT                                                                
043000 F-BUSINESS-LOGIC-W6014410    SECTION.                                    
043100                                                                          
043200     CALL W6014410 USING                                                  
043300          REQU-AREA RESP-AREA MAX-KVRADER                                 
043400          MSG-PCB ALT1-PCB DISP-PCB                                       
043500          INLA1-PCB                                                       
043600          INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB                            
043700          ARTS-PCB                                                        
043800          UPFA-PCB ARTC-PCB WDB6-PCB                                      
043900          PMRK-INLB-PCB PMRK-INLC-PCB                                     
044000          PMRK-PLAA-PCB                                                   
044100          STYR-HANA-PCB                                                   
044200          STYR-PLAA-PCB                                                   
044300          KOM-KOMA-PCB                                                    
044400                                                                          
044500     PERFORM FA-SET-MSG-AND-HILIGHT                                       
044600     PERFORM FB-MOVE-RESP-TO-MOD                                          
044700     MOVE '002'               TO MSGI-KDCALL                              
044800     MOVE '6144'              TO MSGI-IDTRANS                             
044900     MOVE '6144'              TO SAVE-IDTRANS                             
045000     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
045100     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
045200     MOVE SAVE-AREA           TO MSGI-SPAR-AREA                           
045300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 FA-SET-MSG-AND-HILIGHT   SECTION.                                        
045800                                                                          
045900     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
046000     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
046100                                                                          
046200     IF WRONG-KEY                                                         
046300       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
046400     END-IF                                                               
046500                                                                          
046600     IF DIVERSE                                                           
046700       MOVE ERR-DIVERSE           TO MED-IDMFSFEL                         
046800     END-IF                                                               
046900                                                                          
047000     IF PF11-AND-NO-DATA                                                  
047100       MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                         
047200     END-IF                                                               
047300                                                                          
047400     IF UPDATE-NOT-ALLOWED                                                
047500       MOVE ERR-NO-UPDATE         TO MED-IDMFSFEL                         
047600     END-IF                                                               
047700                                                                          
047800     IF UPDATE-NOT-ALLOWED                                                
047900       MOVE ERR-NO-UPDATE         TO MED-IDMFSFEL                         
048000     END-IF                                                               
048100                                                                          
048200     IF MISSING                                                           
048300       MOVE ERR-MISSING           TO MED-IDMFSFEL                         
048400     END-IF                                                               
048500                                                                          
048600     IF CORR-HILITE-FLDS                                                  
048700       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
048800     END-IF                                                               
048900                                                                          
049000     IF INVALID-FIELD                                                     
049100       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
049200     END-IF                                                               
049300                                                                          
049400     IF QUALITY                                                           
049500       MOVE ERR-QUALITY           TO MED-IDMFSFEL                         
049600     END-IF                                                               
049700                                                                          
049800     IF CHECK-QUAL-FIRST                                                  
049900       MOVE ERR-CHECK-QUAL-FIRST  TO MED-IDMFSFEL                         
050000     END-IF                                                               
050100                                                                          
050200     IF CONTROL-NOT-COMPL                                                 
050300       MOVE ERR-CONTROL-NOT-COMPL TO MED-IDMFSFEL                         
050400     END-IF                                                               
050500                                                                          
050600     IF PLACE-MISSING                                                     
050700       MOVE ERR-PLACE-MISSING     TO MED-IDMFSFEL                         
050800     END-IF                                                               
050900                                                                          
051000     IF PLACE-MISSING-SVS                                                 
051100       MOVE ERR-PLACE-MISSING-SVS TO MED-IDMFSFEL                         
051200     END-IF                                                               
051300                                                                          
051400     IF WEIGHT-MISSING                                                    
051500       MOVE ERR-WEIGHT-MISSING    TO MED-IDMFSFEL                         
051600     END-IF                                                               
051700                                                                          
051800     IF VOLUME-MISSING                                                    
051900       MOVE ERR-VOLUME-MISSING    TO MED-IDMFSFEL                         
052000     END-IF                                                               
052100                                                                          
052200     IF ORIGIN-MISSING                                                    
052300       MOVE ERR-ORIGIN-MISSING    TO MED-IDMFSFEL                         
052400     END-IF                                                               
052500                                                                          
052600     CALL WMEDKONV USING MED-WMEDAREA                                     
052700     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                                 
052800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
052900     PERFORM MFS-ROER-EJ-FAELT-IN                                         
053000*                                                                         
053100     IF FIRST-PAGE                                                        
053200       MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                           
053300     END-IF                                                               
053400                                                                          
053500     IF MORE-LINES                                                        
053600       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
053700     END-IF                                                               
053800                                                                          
053900     IF NO-MORE-LINES                                                     
054000       MOVE INF-NO-MORE-LINES   TO MED-IDMFSINF                           
054100     END-IF                                                               
054200                                                                          
054300     IF PRESS-PF11                                                        
054400       MOVE INF-PRESS-PF11      TO MED-IDMFSINF                           
054500     END-IF                                                               
054600                                                                          
054700     IF DEV-VALUE-TOO-HIGH                                                
054800       MOVE INF-DEV-VALUE-TOO-HIGH  TO MED-IDMFSINF                       
054900     END-IF                                                               
055000                                                                          
055100     IF UPDATE-DONE                                                       
055200       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
055300     END-IF                                                               
055400                                                                          
055500     CALL WMEDKONV USING MED-WMEDAREA                                     
055600     MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                   
055700                                                                          
055800     .                                                                    
055900     EJECT                                                                
056000                                                                          
056100 FB-MOVE-RESP-TO-MOD      SECTION.                                        
056200     MOVE +1 TO INDX                                                      
056300     MOVE RESP-IDRADNR-START    TO SAVE-IDRADNR-ENTER                     
056400     MOVE RESP-IDRADNR-NEXT     TO SAVE-IDRADNR-NEXT                      
056500*                                                                         
056600     IF RESP-IDLOPNRM NOT = SPACES AND                                    
056700        RESP-IDLOPNRM > ZERO                                              
056800       MOVE RESP-IDLOPNRM        TO MOD-IDLOPNRM-UT                       
056900       INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE            
057000                                                                          
057100       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-KOLLI-UT                  
057200                                    MOD-IDOKOLLI-UT                       
057300       MOVE ALL '+'              TO MSGI-WMSGINIT                         
057400       MOVE '001'                TO MSGI-KDCALL                           
057500       MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                           
057600       MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                     
057700       MOVE '6144'               TO MSGI-IDTRANS                          
057800       MOVE RESP-IDLOPNRM(2:8)   TO MSGI-IDLOPNRM                         
057900       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
058000     ELSE                                                                 
058100       MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-UT                       
058200       MOVE REQU-IDOKOLLI-KEY    TO MOD-IDOKOLLI-UT                       
058300       INSPECT MOD-IDOKOLLI-UT                                            
058400         REPLACING LEADING ZERO BY SPACE                                  
058500       MOVE REQU-IDLEVNR-KOLLI-KEY                                        
058600                                 TO MOD-IDLEVNR-KOLLI-UT                  
058700     END-IF                                                               
058800*                                                                         
058900     IF RESP-IDARTNR = SPACES                                             
059000       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                           
059100     ELSE                                                                 
059200       IF RESP-IDARTNR = ALL '+'                                          
059300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                           
059400       ELSE                                                               
059500         MOVE RESP-IDARTNR       TO MOD-IDARTNR                           
059600       END-IF                                                             
059700     END-IF                                                               
059800*                                                                         
059900     IF RESP-KVAVIS = SPACES                                              
060000       MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS                            
060100     ELSE                                                                 
060200       IF RESP-KVAVIS = ALL '+'                                           
060300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS                            
060400       ELSE                                                               
060500         MOVE RESP-KVAVIS        TO MOD-KVAVIS                            
060600       END-IF                                                             
060700     END-IF                                                               
060800*                                                                         
060900     IF RESP-BEART = SPACES                                               
061000       MOVE MFS-RENSA-FAELT      TO MOD-BEART                             
061100     ELSE                                                                 
061200       IF RESP-BEART = ALL '+'                                            
061300         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                             
061400       ELSE                                                               
061500         MOVE RESP-BEART         TO MOD-BEART                             
061600       END-IF                                                             
061700     END-IF                                                               
061800*                                                                         
061900     IF RESP-KDSORT = SPACES                                              
062000       MOVE MFS-RENSA-FAELT      TO MOD-KDSORT                            
062100     ELSE                                                                 
062200       IF RESP-KDSORT = ALL '+'                                           
062300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDSORT                            
062400       ELSE                                                               
062500         MOVE RESP-KDSORT        TO MOD-KDSORT                            
062600       END-IF                                                             
062700     END-IF                                                               
062800*                                                                         
062900     IF RESP-BEFT  = SPACES                                               
063000       MOVE MFS-RENSA-FAELT      TO MOD-BEFT                              
063100     ELSE                                                                 
063200       IF RESP-BEFT  = ALL '+'                                            
063300         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEFT                              
063400       ELSE                                                               
063500         MOVE RESP-BEFT          TO MOD-BEFT                              
063600       END-IF                                                             
063700     END-IF                                                               
063800*                                                                         
063900     IF RESP-KVRAPP  = SPACES                                             
064000       MOVE MFS-RENSA-FAELT      TO MOD-KVRAPP                            
064100     ELSE                                                                 
064200       IF RESP-KVRAPP  = ALL '+'                                          
064300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVRAPP                            
064400       ELSE                                                               
064500         MOVE RESP-KVRAPP        TO MOD-KVRAPP                            
064600       END-IF                                                             
064700     END-IF                                                               
064800*                                                                         
064900     IF RESP-BEFARLIG-TEXT = SPACES                                       
065000       MOVE MFS-RENSA-FAELT      TO MOD-BEFARLIG-TEXT                     
065100     ELSE                                                                 
065200       IF RESP-BEFARLIG-TEXT  = ALL '+'                                   
065300         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEFARLIG-TEXT                     
065400       ELSE                                                               
065500         MOVE RESP-BEFARLIG-TEXT TO MOD-BEFARLIG-TEXT                     
065600       END-IF                                                             
065700     END-IF                                                               
065800*                                                                         
065900     IF RESP-ADINLOMR-NXT  = SPACES                                       
066000       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-NXT                      
066100     ELSE                                                                 
066200       IF RESP-ADINLOMR-NXT  = ALL '+'                                    
066300         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-NXT                      
066400       ELSE                                                               
066500         MOVE RESP-ADINLOMR-NXT  TO MOD-ADINLOMR-NXT                      
066600       END-IF                                                             
066700     END-IF                                                               
066800*                                                                         
066900     IF RESP-ADLAGOMR = SPACES                                            
067000       MOVE MFS-RENSA-FAELT      TO MOD-ADLAGOMR                          
067100     ELSE                                                                 
067200       IF RESP-ADLAGOMR = ALL '+'                                         
067300         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADLAGOMR                          
067400       ELSE                                                               
067500         MOVE RESP-ADLAGOMR      TO MOD-ADLAGOMR                          
067600       END-IF                                                             
067700     END-IF                                                               
067800*                                                                         
067900     IF RESP-ADGANG  = SPACES                                             
068000       MOVE MFS-RENSA-FAELT      TO MOD-ADGANG                            
068100     ELSE                                                                 
068200       IF RESP-ADGANG  = ALL '+'                                          
068300         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGANG                            
068400       ELSE                                                               
068500         MOVE RESP-ADGANG        TO MOD-ADGANG                            
068600       END-IF                                                             
068700     END-IF                                                               
068800*                                                                         
068900     IF RESP-ADPLATS = SPACES                                             
069000       MOVE MFS-RENSA-FAELT      TO MOD-ADPLATS                           
069100     ELSE                                                                 
069200       IF RESP-ADPLATS = ALL '+'                                          
069300         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADPLATS                           
069400       ELSE                                                               
069500         MOVE RESP-ADPLATS       TO MOD-ADPLATS                           
069600       END-IF                                                             
069700     END-IF                                                               
069800*                                                                         
069900     MOVE +1 TO INDX                                                      
070000     PERFORM UNTIL INDX > 3                                               
070100       IF RESP-ADBUFFOMR (INDX) = SPACES                                  
070200         MOVE MFS-RENSA-FAELT    TO MOD-ADBUFFOMR (INDX)                  
070300       ELSE                                                               
070400         IF RESP-ADBUFFOMR (INDX) = ALL '+'                               
070500           MOVE MFS-ROER-EJ-FAELT     TO MOD-ADBUFFOMR  (INDX)            
070600         ELSE                                                             
070700           MOVE RESP-ADBUFFOMR (INDX) TO MOD-ADBUFFOMR  (INDX)            
070800         END-IF                                                           
070900       END-IF                                                             
071000*                                                                         
071100       IF RESP-ADBUFFGANG (INDX) = SPACES                                 
071200          MOVE MFS-RENSA-FAELT   TO MOD-ADBUFFGANG (INDX)                 
071300       ELSE                                                               
071400        IF RESP-ADBUFFGANG (INDX) = ALL '+'                               
071500          MOVE MFS-ROER-EJ-FAELT      TO MOD-ADBUFFGANG (INDX)            
071600        ELSE                                                              
071700          MOVE RESP-ADBUFFGANG (INDX) TO MOD-ADBUFFGANG (INDX)            
071800        END-IF                                                            
071900       END-IF                                                             
072000*                                                                         
072100       IF RESP-ADBUFFPL (INDX) = SPACES                                   
072200          MOVE MFS-RENSA-FAELT   TO MOD-ADBUFFPL (INDX)                   
072300       ELSE                                                               
072400         IF RESP-ADBUFFPL (INDX) = ALL '+'                                
072500           MOVE MFS-ROER-EJ-FAELT      TO MOD-ADBUFFPL   (INDX)           
072600         ELSE                                                             
072700           MOVE RESP-ADBUFFPL (INDX)   TO MOD-ADBUFFPL   (INDX)           
072800         END-IF                                                           
072900       END-IF                                                             
073000       ADD +1  TO INDX                                                    
073100     END-PERFORM                                                          
073200*                                                                         
073300     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADINLOMR-NXT-ATTR                  
073400     MOVE MFS-ROER-EJ-FAELT     TO MOD-ADINLOMR-NXT                       
073500*                                                                         
073600     MOVE RESP-IDANSTNR-ATTR  TO MOD-IDANSTNR-ATTR                        
073700     IF RESP-IDANSTNR = SPACES                                            
073800       MOVE MFS-RENSA-FAELT   TO MOD-IDANSTNR                             
073900     ELSE                                                                 
074000       IF RESP-IDANSTNR = ALL '+'                                         
074100         MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                           
074200       ELSE                                                               
074300         MOVE RESP-IDANSTNR     TO MOD-IDANSTNR                           
074400       END-IF                                                             
074500     END-IF                                                               
074600*                                                                         
074700     MOVE +1 TO INDX                                                      
074800     PERFORM UNTIL INDX > RESP-KVRADER                                    
074900       MOVE RESP-KDCMDVAL-RAD-ATTR (INDX)                                 
075000                                TO MOD-KDCMDVAL-RAD-ATTR (INDX)           
075100       IF RESP-KDCMDVAL-RAD (INDX) = SPACES                               
075200         MOVE MFS-RENSA-FAELT   TO MOD-KDCMDVAL-RAD (INDX)                
075300       ELSE                                                               
075400         IF RESP-KDCMDVAL-RAD (INDX) = ALL '+'                            
075500           MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD (INDX)              
075600         ELSE                                                             
075700           MOVE RESP-KDCMDVAL-RAD (INDX)                                  
075800                                  TO MOD-KDCMDVAL-RAD (INDX)              
075900         END-IF                                                           
076000       END-IF                                                             
076100*                                                                         
076200       MOVE RESP-KVINLART-UPD-ATTR (INDX)                                 
076300                                TO MOD-KVINLART-UPD-ATTR (INDX)           
076400       IF RESP-KVINLART-UPD (INDX) = SPACES                               
076500         MOVE MFS-RENSA-FAELT   TO MOD-KVINLART-UPD (INDX)                
076600       ELSE                                                               
076700         IF RESP-KVINLART-UPD (INDX) = ALL '+'                            
076800           MOVE MFS-ROER-EJ-FAELT TO MOD-KVINLART-UPD (INDX)              
076900         ELSE                                                             
077000           MOVE RESP-KVINLART-UPD (INDX)                                  
077100                                  TO MOD-KVINLART-UPD (INDX)              
077200         END-IF                                                           
077300       END-IF                                                             
077400*                                                                         
077500       MOVE RESP-ADINLOMR-NXT-UPD-ATTR(INDX)                              
077600                             TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)           
077700       IF RESP-ADINLOMR-NXT-UPD (INDX) = SPACES                           
077800         MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR-NXT-UPD (INDX)            
077900       ELSE                                                               
078000         IF RESP-ADINLOMR-NXT-UPD (INDX) = ALL '+'                        
078100           MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-NXT-UPD (INDX)          
078200         ELSE                                                             
078300           MOVE RESP-ADINLOMR-NXT-UPD (INDX)                              
078400                                  TO MOD-ADINLOMR-NXT-UPD (INDX)          
078500         END-IF                                                           
078600       END-IF                                                             
078700*                                                                         
078800       IF RESP-IDRADNR-RAD (INDX) = SPACES                                
078900          MOVE MFS-RENSA-FAELT   TO RESP-IDRADNR-RAD (INDX)               
079000       ELSE                                                               
079100        IF RESP-IDRADNR-RAD (INDX) = ALL '+'                              
079200          MOVE MFS-ROER-EJ-FAELT        TO MOD-IDRADNR-RAD (INDX)         
079300        ELSE                                                              
079400          MOVE RESP-IDRADNR-RAD (INDX)  TO MOD-IDRADNR-RAD (INDX)         
079500        END-IF                                                            
079600       END-IF                                                             
079700*                                                                         
079800       IF RESP-KVINLART-RAD (INDX)  = SPACES                              
079900          MOVE MFS-RENSA-FAELT   TO RESP-KVINLART-RAD (INDX)              
080000       ELSE                                                               
080100        IF RESP-KVINLART-RAD (INDX)  = ALL '+'                            
080200         MOVE MFS-ROER-EJ-FAELT        TO MOD-KVINLART-RAD (INDX)         
080300        ELSE                                                              
080400         MOVE RESP-KVINLART-RAD (INDX) TO MOD-KVINLART-RAD (INDX)         
080500        END-IF                                                            
080600       END-IF                                                             
080700*                                                                         
080800       IF RESP-ADINLOMR-RAD (INDX) = SPACES                               
080900          MOVE MFS-RENSA-FAELT   TO RESP-ADINLOMR-RAD (INDX)              
081000       ELSE                                                               
081100        IF RESP-ADINLOMR-RAD (INDX) = ALL '+'                             
081200         MOVE MFS-ROER-EJ-FAELT        TO MOD-ADINLOMR-RAD (INDX)         
081300        ELSE                                                              
081400         MOVE RESP-ADINLOMR-RAD (INDX) TO MOD-ADINLOMR-RAD (INDX)         
081500        END-IF                                                            
081600       END-IF                                                             
081700*                                                                         
081800       IF RESP-KDINLSTA-RAD (INDX) = SPACES                               
081900          MOVE MFS-RENSA-FAELT   TO RESP-KDINLSTA-RAD (INDX)              
082000       ELSE                                                               
082100        IF RESP-KDINLSTA-RAD (INDX) = ALL '+'                             
082200         MOVE MFS-ROER-EJ-FAELT        TO MOD-KDINLSTA-RAD (INDX)         
082300        ELSE                                                              
082400         MOVE RESP-KDINLSTA-RAD (INDX) TO MOD-KDINLSTA-RAD (INDX)         
082500        END-IF                                                            
082600       END-IF                                                             
082700*                                                                         
082800       IF RESP-IDLEVNR-KOLLI-RAD(INDX) = SPACES                           
082900          MOVE MFS-RENSA-FAELT   TO RESP-IDLEVNR-KOLLI-RAD(INDX)          
083000       ELSE                                                               
083100        IF RESP-IDLEVNR-KOLLI-RAD(INDX) = ALL '+'                         
083200         MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLEVNR-KOLLI-RAD(INDX)          
083300        ELSE                                                              
083400         MOVE RESP-IDLEVNR-KOLLI-RAD(INDX)                                
083500                                  TO MOD-IDLEVNR-KOLLI-RAD(INDX)          
083600        END-IF                                                            
083700       END-IF                                                             
083800*                                                                         
083900       IF RESP-IDOKOLLI-RAD (INDX) = SPACES                               
084000          MOVE MFS-RENSA-FAELT   TO RESP-IDOKOLLI-RAD (INDX)              
084100       ELSE                                                               
084200        IF RESP-IDOKOLLI-RAD (INDX) = ALL '+'                             
084300         MOVE MFS-ROER-EJ-FAELT   TO MOD-IDOKOLLI-RAD (INDX)              
084400        ELSE                                                              
084500         MOVE RESP-IDOKOLLI-RAD (INDX)                                    
084600                                  TO MOD-IDOKOLLI-RAD (INDX)              
084700        END-IF                                                            
084800       END-IF                                                             
084900*                                                                         
085000       IF RESP-KVINLART-VOR-RAD (INDX)  = SPACES                          
085100          MOVE MFS-RENSA-FAELT   TO  RESP-KVINLART-VOR-RAD (INDX)         
085200       ELSE                                                               
085300        IF RESP-KVINLART-VOR-RAD (INDX)  = ALL '+'                        
085400         MOVE MFS-ROER-EJ-FAELT   TO MOD-KVINLART-VOR-RAD (INDX)          
085500        ELSE                                                              
085600         MOVE RESP-KVINLART-VOR-RAD (INDX)                                
085700                                  TO MOD-KVINLART-VOR-RAD (INDX)          
085800        END-IF                                                            
085900       END-IF                                                             
086000*                                                                         
086100       ADD +1 TO INDX                                                     
086200     END-PERFORM                                                          
086300                                                                          
086400*                                                                         
086500* CLOSE/CLEAR THE REMAINING LINES ON THE SCREEN                           
086600     PERFORM UNTIL INDX    >  MAX-INDX                                    
086700       MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-RAD-ATTR(INDX)               
086800                                MOD-KVINLART-UPD-ATTR(INDX)               
086900                                MOD-ADINLOMR-NXT-UPD-ATTR(INDX)           
087000                                                                          
087100       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-RAD  (INDX)                    
087200                               MOD-KVINLART-RAD (INDX)                    
087300                               MOD-ADINLOMR-RAD (INDX)                    
087400                               MOD-KDINLSTA-RAD (INDX)                    
087500                               MOD-IDLEVNR-KOLLI-RAD(INDX)                
087600                               MOD-IDOKOLLI-RAD (INDX)                    
087700                               MOD-KVINLART-VOR-RAD(INDX)                 
087800       ADD +1 TO INDX                                                     
087900     END-PERFORM                                                          
088000                                                                          
088100     IF UPDATE-DONE                                                       
088200       PERFORM MFS-FORM-ATTR                                              
088300       PERFORM MFS-RENSA-FAELT-IN                                         
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700                                                                          
088800 MFS-RENSA-FAELT-IN SECTION.                                              
088900                                                                          
089000     MOVE MFS-RENSA-FAELT      TO MOD-IDANSTNR                            
089100**                                                                        
089200**                                                                        
089300     PERFORM MFS-RENSA-RAD-FAELT-IN                                       
089400     .                                                                    
089500     SKIP2                                                                
089600 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
089700                                                                          
089800     MOVE +1                   TO INDX                                    
089900     PERFORM UNTIL INDX        >  MAX-INDX                                
090000         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD    (INDX)              
090100                                  MOD-KVINLART-UPD    (INDX)              
090200                                  MOD-ADINLOMR-NXT-UPD(INDX)              
090300         ADD +1                TO INDX                                    
090400     END-PERFORM                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 MFS-RENSA-FAELT-UT  SECTION.                                             
090800     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                             
090900                                  MOD-KVAVIS                              
091000                                  MOD-BEART                               
091100                                  MOD-KDSORT                              
091200                                  MOD-BEFT                                
091300                                  MOD-KVRAPP                              
091400                                  MOD-BEFARLIG-TEXT                       
091500                                  MOD-ADINLOMR-NXT                        
091600                                  MOD-ADLAGOMR                            
091700                                  MOD-ADGANG                              
091800                                  MOD-ADPLATS                             
091900                                                                          
092000     MOVE +1                    TO INDX                                   
092100     PERFORM UNTIL INDX         >  3                                      
092200         MOVE MFS-RENSA-FAELT   TO MOD-ADBUFFOMR  (INDX)                  
092300                                   MOD-ADBUFFGANG (INDX)                  
092400                                   MOD-ADBUFFPL   (INDX)                  
092500         ADD +1                 TO INDX                                   
092600     END-PERFORM                                                          
092700     PERFORM MFS-RENSA-FAELT-RAD-UT                                       
092800     .                                                                    
092900     EJECT                                                                
093000 MFS-RENSA-FAELT-RAD-UT   SECTION.                                        
093100                                                                          
093200     MOVE +1                    TO INDX                                   
093300     PERFORM UNTIL INDX         >  MAX-INDX                               
093400         MOVE MFS-RENSA-FAELT   TO MOD-IDRADNR-RAD      (INDX)            
093500                                   MOD-KVINLART-RAD     (INDX)            
093600                                   MOD-ADINLOMR-RAD     (INDX)            
093700                                   MOD-KDINLSTA-RAD     (INDX)            
093800                                   MOD-IDLEVNR-KOLLI-RAD(INDX)            
093900                                   MOD-IDOKOLLI-RAD     (INDX)            
094000                                   MOD-KVINLART-VOR-RAD (INDX)            
094100         ADD +1                 TO INDX                                   
094200     END-PERFORM                                                          
094300     .                                                                    
094400     EJECT                                                                
094500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
094600     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR                             
094700                                  MOD-KVAVIS                              
094800                                  MOD-BEART                               
094900                                  MOD-KDSORT                              
095000                                  MOD-BEFT                                
095100                                  MOD-KVRAPP                              
095200                                  MOD-BEFARLIG-TEXT                       
095300                                  MOD-ADINLOMR-NXT                        
095400                                  MOD-ADLAGOMR                            
095500                                  MOD-ADGANG                              
095600                                  MOD-ADPLATS                             
095700                                                                          
095800     MOVE +1                    TO INDX                                   
095900     PERFORM UNTIL INDX         >  3                                      
096000         MOVE MFS-ROER-EJ-FAELT TO MOD-ADBUFFOMR  (INDX)                  
096100                                   MOD-ADBUFFGANG (INDX)                  
096200                                   MOD-ADBUFFPL   (INDX)                  
096300         ADD +1                 TO INDX                                   
096400     END-PERFORM                                                          
096500     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
096600     .                                                                    
096700     EJECT                                                                
096800 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
096900                                                                          
097000     MOVE +1                    TO INDX                                   
097100     PERFORM UNTIL INDX         >  MAX-INDX                               
097200         MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-RAD      (INDX)            
097300                                   MOD-KVINLART-RAD     (INDX)            
097400                                   MOD-ADINLOMR-RAD     (INDX)            
097500                                   MOD-KDINLSTA-RAD     (INDX)            
097600                                   MOD-IDLEVNR-KOLLI-RAD(INDX)            
097700                                   MOD-IDOKOLLI-RAD     (INDX)            
097800                                   MOD-KVINLART-VOR-RAD (INDX)            
097900         ADD +1                 TO INDX                                   
098000     END-PERFORM                                                          
098100     .                                                                    
098200     EJECT                                                                
098300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
098400                                                                          
098500     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANSTNR                            
098600     PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                     
098700     .                                                                    
098800     SKIP2                                                                
098900 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
099000                                                                          
099100     MOVE +1                    TO INDX                                   
099200     PERFORM UNTIL INDX         >  MAX-INDX                               
099300         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD    (INDX)             
099400                                   MOD-KVINLART-UPD    (INDX)             
099500                                   MOD-ADINLOMR-NXT-UPD(INDX)             
099600         ADD +1                 TO INDX                                   
099700     END-PERFORM                                                          
099800     .                                                                    
099900     EJECT                                                                
100000 MFS-FORM-ATTR SECTION.                                                   
100100                                                                          
100200     MOVE MFS-FORMATETS-ATTR TO MOD-IDANSTNR-ATTR                         
100300     PERFORM MFS-FORM-ATTR-RAD                                            
100400     .                                                                    
100500     SKIP2                                                                
100600 MFS-FORM-ATTR-RAD        SECTION.                                        
100700                                                                          
100800     MOVE +1                   TO INDX                                    
100900     PERFORM UNTIL INDX        >  MAX-INDX                                
101000         MOVE MFS-FORMATETS-ATTR                                          
101100                               TO MOD-KDCMDVAL-RAD-ATTR    (INDX)         
101200                                  MOD-KVINLART-UPD-ATTR    (INDX)         
101300                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
101400         ADD +1                TO INDX                                    
101500     END-PERFORM                                                          
101600     .                                                                    
101700     EJECT                                                                
101800* --- IMS SEKTIONER ---                                                   
101900     SKIP3                                                                
102000 IMS-GET-MSG SECTION.                                                     
102100                                                                          
102200     MOVE '  QC' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     SKIP3                                                                
102800 IMS-INSERT-MSG SECTION.                                                  
102900                                                                          
103000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
103100       MOVE '0' TO MFS-KDHUVOMR                                           
103200     END-IF                                                               
103300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103400     MOVE SPACE TO GODK-STATUSKODER                                       
103500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900     EJECT                                                                
104000 IMS-STATUSKONTROLL SECTION.                                              
104100                                                                          
104200     SET STATUS-IX TO 1                                                   
104300     SEARCH GODK-STATUS                                                   
104400       AT END                                                             
104500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
104600         DELIMITED BY SIZE INTO FELTEXT                                   
104700         CALL FELLOG                                                      
104800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104900         CONTINUE                                                         
105000     END-SEARCH                                                           
105100     .                                                                    
