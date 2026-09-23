000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4066800.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   97/01/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER HÄNDELSEBAS OCH KONTROLLERAR INMATNING. OM INDATA          
001000*        OK SÅ STARTAS ETT BAKGRUNDS-MPP SOM SKRIVER 'BILL OF             
001100*        LADING' FÖR NDC:ERNA.                                            
001200*                                                                         
001300*        PROGRAMMET LÄSER      WL4463 (WDR4)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T668                                              
001700*        MID:         W4I66801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O66801                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W4066800'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  IX                          PIC S9(4)   VALUE +0 COMP SYNC.          
003700 77  INDX                        PIC 9(4)    VALUE ZERO.                  
003800 77  MAX-INDX                    PIC S9(4)   VALUE +42 COMP SYNC.         
003900 77  MAX-KVRADER                 PIC S9(4)   COMP VALUE +42.              
004000 77  WS-DATUM                    PIC X(6)    VALUE ZERO.                  
004100     EJECT                                                                
004200                                                                          
004300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004400     88  EGEN-MID                            VALUE '4668'.                
004500     88  GODK-MID                            VALUE '4661' '4662'          
004600                                                   '4663' '4664'          
004700                                                   '4665' '4666'          
004800                                                   '4667' '4668'          
004900                                                   '4669'.                
005000     88  HELP-MID                            VALUE '0551'.                
005100     EJECT                                                                
005200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005300 01  GENERELLA-SUBPROGRAM.                                                
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005700     03  W4066810                PIC X(8)    VALUE 'W4066810'.            
005800     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
005900     EJECT                                                                
006000     SKIP3                                                                
006100 01  ALL-PLUS.                                                            
006200     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
006300 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
006400*01  -COPY WORKAREA                                                       
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
006700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006800*                                                                         
006900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007000     SKIP3                                                                
007100*01 -COPY WMSGINIT                                                        
007200     SKIP3                                                                
007300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007600     SKIP3                                                                
007700*01  MID -COPY W4I66801                                                   
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008000     SKIP3                                                                
008100*01  -COPY WMSGAREA                                                       
008200     EJECT                                                                
008300     03  MOD REDEFINES MSG-AREA.                                          
008400*      05  -COPY W4O66801                                                 
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008700     SKIP3                                                                
008800*01  -COPY WMFSAREA                                                       
008900     EJECT                                                                
009000 01  REQU-AREA.                                                           
009100*    03 -COPY WZ01REQU                                                    
009200*    03 -COPY W40668I1                                                    
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009500 01  RESP-AREA.                                                           
009600*    03 -COPY WZ01RESP                                                    
009700*    03 -COPY W40668O1                                                    
009800 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
009900     SKIP3                                                                
010000*01  -COPY WL01MCNV                                                       
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700     EJECT                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01  -COPY W0009   -PRE MSG-                                              
012600     EJECT                                                                
012700*01  -COPY W0009   -PRE ALT-USA-                                          
012800     EJECT                                                                
012900*01  -COPY W0009   -PRE ALT-JAP-                                          
013000     EJECT                                                                
013100*01  -COPY W0009   -PRE ALT-AUS-                                          
013200     EJECT                                                                
013300*01  -COPY W0008   -PRE USEA-                                             
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600*01  -COPY W0008  -PRE 4463-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013810*01  -COPY W0008  -PRE WDB6-                                              
013820     05  FILLER                  PIC X.                                   
013830     EJECT                                                                
013900 PROCEDURE DIVISION  USING MSG-PCB ALT-USA-PCB ALT-JAP-PCB                
014000                           ALT-AUS-PCB     USEA-PCB 4463-PCB              
014010                           WDB6-PCB.                                      
014100     ENTRY 'DLITCBL' USING MSG-PCB ALT-USA-PCB ALT-JAP-PCB                
014200                           ALT-AUS-PCB     USEA-PCB 4463-PCB              
014210                           WDB6-PCB.                                      
014300                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-INIT-KEYS                                                
014800       IF MFS-UPDATE                                                      
014900         SET REQU-UPDATE  TO TRUE                                         
015000       ELSE                                                               
015100         IF MFS-FIRST                                                     
015200           SET REQU-FIRST TO TRUE                                         
015300           PERFORM MFS-RENSA-FAELT-IN                                     
015400         ELSE                                                             
015500           SET REQU-QUERY TO TRUE                                         
015600           PERFORM E-SAMMA-SIDA                                           
015700         END-IF                                                           
015800       END-IF                                                             
015900                                                                          
016000       PERFORM F-CALL-BIZ-LOGIC-W4066810                                  
016100                                                                          
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O66801 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700                                                                          
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66801                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66801                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W4O66801' TO MFS-IDMOD                                         
018900     MOVE '4668' TO MOD-IDTRANS                                           
019000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 B-INIT-KEYS SECTION.                                                     
020100                                                                          
020200     MOVE ALL '+'             TO MSGI-WMSGINIT                            
020300     MOVE '001'               TO MSGI-KDCALL                              
020400     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
020500     MOVE '4668'              TO MSGI-IDTRANS                             
020600     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020800                                                                          
020900     IF GODK-MID                                                          
021000       IF MID-TIDATUM-IN NOT = ALL '+'                                    
021100         MOVE MID-TIDATUM-IN  TO WS-DATUM                                 
021200       ELSE                                                               
021300         IF MID-TIDATUM-UT = ZERO                                         
021400           MOVE MSGI-TILOKDAT TO WS-DATUM                                 
021500         END-IF                                                           
021600       END-IF                                                             
021700     END-IF                                                               
021800                                                                          
021900     MOVE MFS-RENSA-FAELT     TO MOD-TIDATUM-IN                           
022000                                                                          
022100     IF EGEN-MID                                                          
022200       IF MID-TIDATUM-IN = ALL '+'                                        
022300         MOVE MID-TIDATUM-UT  TO WS-DATUM                                 
022400         INSPECT WS-DATUM REPLACING LEADING SPACE BY ZERO                 
022500       ELSE                                                               
022600         MOVE MID-TIDATUM-IN  TO WS-DATUM                                 
022700         MOVE '7'             TO MFS-IDPFK                                
022800         MOVE SPACE           TO MFS-KDTRTYP                              
022900       END-IF                                                             
023000     ELSE                                                                 
023100       MOVE MSGI-TILOKDAT     TO WS-DATUM                                 
023200       MOVE '7'               TO MFS-IDPFK                                
023300       MOVE SPACE             TO MFS-KDTRTYP                              
023400     END-IF                                                               
023500                                                                          
023600     IF WS-DATUM NUMERIC                                                  
023700       IF WS-DATUM = ZERO                                                 
023800         MOVE MSGI-TIREGDAT   TO WS-DATUM                                 
023900       END-IF                                                             
024000     END-IF                                                               
024100                                                                          
024300     MOVE WS-DATUM            TO MOD-TIDATUM-UT                           
024400                                 REQU-TIDATUM-KEY                         
024500     MOVE MSGI-IDDC           TO REQU-IDDC-KEY                            
025000     .                                                                    
025100     EJECT                                                                
025200 E-SAMMA-SIDA SECTION.                                                    
025300                                                                          
025400     IF EGEN-MID OR HELP-MID                                              
025500       CONTINUE                                                           
025600     ELSE                                                                 
025700       PERFORM MFS-LAES-IN-IGEN                                           
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 F-CALL-BIZ-LOGIC-W4066810  SECTION.                                      
026200                                                                          
026300     PERFORM FA-INIT-REQU                                                 
026400     CALL W4066810 USING REQU-AREA  RESP-AREA  MAX-KVRADER                
026500                         ALT-USA-PCB ALT-JAP-PCB                          
026600                         ALT-AUS-PCB 4463-PCB WDB6-PCB                    
026700                                                                          
026800     PERFORM FB-SET-MSG-HIGHLIGHT                                         
026900     PERFORM FC-MOVE-RESP-TO-MOD                                          
027000     .                                                                    
027100     EJECT                                                                
027200 FA-INIT-REQU SECTION.                                                    
027300                                                                          
027400     MOVE MAX-KVRADER           TO REQU-KVRADER                           
027500     MOVE +1                    TO INDX                                   
027600     PERFORM UNTIL INDX >  MAX-INDX                                       
027700       MOVE MID-KDSVAR(INDX)    TO REQU-KDSVAR-LINE(INDX)                 
027800       MOVE MID-IDTRPTNR(INDX)  TO REQU-IDTRPTNR-LINE(INDX)               
027900       MOVE MID-IDLBBET(INDX)   TO REQU-IDLBBET-LINE(INDX)                
028000       ADD +1                   TO INDX                                   
028100     END-PERFORM                                                          
028200                                                                          
028300     PERFORM UNTIL INDX >  MAX-KVRADER                                    
028400       MOVE ALL-PLUS            TO REQU-KDSVAR-LINE(INDX)                 
028500                                   REQU-IDTRPTNR-LINE(INDX)               
028600                                   REQU-IDLBBET-LINE(INDX)                
028700       ADD +1                   TO INDX                                   
028800     END-PERFORM                                                          
028900                                                                          
029000     MOVE '101'                 TO REQU-IDMSGVER                          
029100     MOVE MSGI-IDUSER           TO REQU-IDUSER                            
029200     .                                                                    
029300 FB-SET-MSG-HIGHLIGHT     SECTION.                                        
029400                                                                          
029500     MOVE RESP-IDMSG-ERROR      TO MCNV-IDMSG-ERROR                       
029600     MOVE RESP-IDMSG-INFO       TO MCNV-IDMSG-INFO                        
029700     MOVE RESP-IDELMT-ERROR     TO MCNV-IDELMT-ERROR                      
029800     MOVE MSGI-IDSPRAK          TO MCNV-IDSPRAK                           
029900                                                                          
030000     CALL WL01MCNV USING MCNV-AREA                                        
030100     MOVE MCNV-MFSINF           TO MOD-TEMFSINF                           
030200     MOVE MCNV-MFSFEL           TO MOD-TEMFSFEL                           
030300     .                                                                    
030400     EJECT                                                                
030500                                                                          
030600 FC-MOVE-RESP-TO-MOD SECTION.                                             
030700                                                                          
030800     MOVE +1                       TO INDX                                
030900     PERFORM UNTIL INDX > RESP-KVRADER                                    
031000                                                                          
031100       MOVE RESP-KDSVAR-LINE-ATTR(INDX)                                   
031200                                   TO MOD-KDSVAR-ATTR(INDX)               
031300                                                                          
031400       IF RESP-KDSVAR-LINE(INDX) = SPACE                                  
031500         MOVE MFS-ERASE-FIELD      TO MOD-KDSVAR(INDX)                    
031600       ELSE                                                               
031700         IF RESP-KDSVAR-LINE(INDX) = ALL '+'                              
031800           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
031900                                   TO MOD-KDSVAR(INDX)                    
032000         ELSE                                                             
032100           MOVE RESP-KDSVAR-LINE(INDX)                                    
032200                                   TO MOD-KDSVAR(INDX)                    
032300         END-IF                                                           
032400       END-IF                                                             
032500                                                                          
032600       IF RESP-IDTRPTNR-LINE(INDX) = SPACE                                
032700         MOVE MFS-ERASE-FIELD      TO MOD-IDTRPTNR(INDX)                  
032800       ELSE                                                               
032900         IF RESP-IDTRPTNR-LINE(INDX) = ALL '+'                            
033000           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
033100                                   TO MOD-IDTRPTNR(INDX)                  
033200         ELSE                                                             
033300           MOVE RESP-IDTRPTNR-LINE(INDX)                                  
033400                                   TO MOD-IDTRPTNR(INDX)                  
033500         END-IF                                                           
033600       END-IF                                                             
033700                                                                          
033800       IF RESP-IDLBBET-LINE(INDX) = SPACE                                 
033900         MOVE MFS-ERASE-FIELD      TO MOD-IDLBBET(INDX)                   
034000       ELSE                                                               
034100         IF RESP-IDLBBET-LINE(INDX) = ALL '+'                             
034200           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
034300                                   TO MOD-IDLBBET(INDX)                   
034400         ELSE                                                             
034500           MOVE RESP-IDLBBET-LINE(INDX)                                   
034600                                   TO MOD-IDLBBET(INDX)                   
034700         END-IF                                                           
034800       END-IF                                                             
034900                                                                          
035000       ADD +1                      TO INDX                                
035100     END-PERFORM                                                          
035200* CLEAR THE REMAINING LINES ON THE SCREEN                                 
035300     PERFORM UNTIL INDX    >  MAX-KVRADER                                 
035400       MOVE MFS-ERASE-FIELD  TO MOD-KDSVAR(INDX)                          
035500                                MOD-IDTRPTNR(INDX)                        
035600                                MOD-IDLBBET(INDX)                         
035710       MOVE MFS-STAENG-FAELT TO MOD-KDSVAR-ATTR(INDX)                     
035800       ADD +1                TO INDX                                      
035900     END-PERFORM                                                          
036000     .                                                                    
036100     EJECT                                                                
036200 MFS-RENSA-FAELT-UT SECTION.                                              
036300                                                                          
036400*    --- ALLA UTDATA-FÄLT                                                 
036500     MOVE +1 TO INDX                                                      
036600     PERFORM UNTIL INDX > MAX-INDX                                        
036700       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
036800       ADD +1 TO INDX                                                     
036900     END-PERFORM                                                          
037000     .                                                                    
037100     SKIP3                                                                
037200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
037300                                                                          
037400*    --- ALLA UTDATA-FÄLT                                                 
037500     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR    (INDX)                         
037600                             MOD-IDTRPTNR  (INDX)                         
037700                             MOD-IDLBBET   (INDX)                         
037800     .                                                                    
037900     EJECT                                                                
038000 MFS-RENSA-FAELT-IN SECTION.                                              
038100                                                                          
038200*    --- ALLA UTDATA-FÄLT                                                 
038300     MOVE +1 TO INDX                                                      
038400     PERFORM UNTIL INDX > MAX-INDX                                        
038500       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
038600       ADD +1 TO INDX                                                     
038700     END-PERFORM                                                          
038800     .                                                                    
038900     SKIP3                                                                
039000 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
039100                                                                          
039200*    --- ALLA UTDATA-FÄLT                                                 
039300     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR    (INDX)                         
039400     .                                                                    
039500     EJECT                                                                
039600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
039700                                                                          
039800*    --- ALLA UTDATA-FÄLT                                                 
039900     MOVE +1 TO INDX                                                      
040000     PERFORM UNTIL INDX > MAX-INDX                                        
040100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
040200       ADD +1 TO INDX                                                     
040300     END-PERFORM                                                          
040400     .                                                                    
040500     EJECT                                                                
040600 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
040700                                                                          
040800*    --- ALLA UTDATA-FÄLT                                                 
040900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR    (INDX)                       
041000                               MOD-IDTRPTNR (INDX)                        
041100                               MOD-IDLBBET (INDX)                         
041200     .                                                                    
041300     EJECT                                                                
041400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
041500                                                                          
041600*    --- ALLA UTDATA-FÄLT                                                 
041700     MOVE +1 TO INDX                                                      
041800     PERFORM UNTIL INDX > MAX-INDX                                        
041900       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
042000       ADD +1 TO INDX                                                     
042100     END-PERFORM                                                          
042200     .                                                                    
042300     EJECT                                                                
042400 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
042500                                                                          
042600*    --- ALLA UTDATA-FÄLT                                                 
042700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR    (INDX)                       
042800     .                                                                    
042900     EJECT                                                                
043000 MFS-LAES-IN-IGEN SECTION.                                                
043100                                                                          
043200*    --- ALLA INDATA-FÄLT                                                 
043300     MOVE +1 TO INDX                                                      
043400     PERFORM UNTIL INDX > MAX-INDX                                        
043500       IF MID-KDSVAR (INDX) NOT = ALL '+'                                 
043600         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSVAR-ATTR (INDX)             
043700       END-IF                                                             
043800       ADD +1 TO INDX                                                     
043900     END-PERFORM                                                          
044000     .                                                                    
044100     EJECT                                                                
044200* --- IMS SEKTIONER ---                                                   
044300     SKIP3                                                                
044400 IMS-GET-MSG SECTION.                                                     
044500                                                                          
044600     MOVE '  QC' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
044800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044900     PERFORM IMS-STATUSKONTROLL                                           
045000     .                                                                    
045100     SKIP3                                                                
045200 IMS-INSERT-MSG SECTION.                                                  
045300                                                                          
045400     IF ENGLISH-TEXT                                                      
045500       MOVE 'N' TO MFS-KDHUVOMR                                           
045600     END-IF                                                               
045700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
045800     MOVE SPACE TO GODK-STATUSKODER                                       
045900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
046000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046100     PERFORM IMS-STATUSKONTROLL                                           
046200     .                                                                    
046300     EJECT                                                                
046400 IMS-STATUSKONTROLL SECTION.                                              
046500                                                                          
046600     SET STATUS-IX TO 1                                                   
046700     SEARCH GODK-STATUS                                                   
046800       AT END                                                             
046900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047000         DELIMITED BY SIZE INTO FELTEXT                                   
047100         CALL FELLOG                                                      
047200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047300         CONTINUE                                                         
047400     END-SEARCH                                                           
047500     .                                                                    
