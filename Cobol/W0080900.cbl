000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W0080900.                                                
001000*AUTHOR.         THOMAS NILSSON.                                          
001100*DATE-WRITTEN.   JULI  87.                                                
001200                                                                          
001400                                                                          
001500*    FUNKTION.                                                            
001600*        PROGRAM - FÖR UPPDATERING AV LEV.VILLKOR PÅ WDGX-4735            
001700                                                                          
001800*    INDATA.                                                              
001900*        TRANSAKTION: W0T809                                              
002000*        MID:         W0I80901                                            
002100                                                                          
002200*    UTDATA.                                                              
002300*        MOD:         W0O80901                                            
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W0080900'.            
003700 77    JA                        PIC X       VALUE 'J'.                   
003800 77    NEJ                       PIC X       VALUE 'N'.                   
003900 77    KDLEVVIL-WS               PIC X       VALUE SPACE.                 
004000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77    SPRAK-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +0   COMP SYNC.        
004300 77    ANTAL-TEXTRADER           PIC S9(2)   VALUE +6   COMP SYNC.        
004400 77    WS-IDTRANS                PIC X(4).                                
004500       88  WS-GODKAEND-BILD                  VALUE '0809'.                
004600 77    UPPDAT-WS                 PIC X.                                   
004700       88  NYUPPLAEGG                        VALUE 'N'.                   
004800       88  AENDRA                            VALUE 'Ä' 'C'.               
004900       88  BORTTAG                           VALUE 'B' 'D'.               
005000     EJECT                                                                
005010                                                                          
005020 01  DYNAMISKA-SUBPROGRAM.                                                
005030   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005040   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005050                                                                          
005100 01    NYCKLAR-TILL-DLI.                                                  
005200   03    W-KDLEVVIL-X.                                                    
005300     05    W-IDHTYP              PIC X(4)    VALUE '4735'.                
005400     05    W-KDLEVVIL            PIC S9(3)   VALUE ZERO  COMP-3.          
005500     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
005600     SKIP3                                                                
005700 01    MEDDELANDE.                                                        
005800   03    FEL1.                                                            
005900      05 FILLER                  PIC X(40)                                
006000         VALUE 'FEL NYCKEL'.                                              
006100      05 FILLER                  PIC X(40)                                
006200         VALUE 'WRONG KEY '.                                              
006300   03    FILLER                  REDEFINES FEL1.                          
006400      05 FEL-1                   PIC X(40)   OCCURS 2.                    
006500                                                                          
006600   03    FEL2.                                                            
006700      05 FILLER                  PIC X(40)                                
006800         VALUE 'INMATNINGSFÄLT FEL IFYLLT'.                               
006900      05 FILLER                  PIC X(40)                                
007000         VALUE 'INPUT FIELD NOT CORRECT'.                                 
007100   03    FILLER                  REDEFINES FEL2.                          
007200      05 FEL-2                   PIC X(40)   OCCURS 2.                    
007300                                                                          
007400   03    FEL3.                                                            
007500      05 FILLER                  PIC X(40)                                
007600         VALUE 'LEVVILLKOR FINNS EJ REGISTRERAT'.                         
007700      05 FILLER                  PIC X(40)                                
007800         VALUE 'TERMS OF DEL. NOT REGISTERED'.                            
007900   03    FILLER                  REDEFINES FEL3.                          
008000      05 FEL-3                   PIC X(40)   OCCURS 2.                    
008100                                                                          
008200   03    FEL4.                                                            
008300      05 FILLER                  PIC X(40)                                
008400         VALUE 'LEVVILLKOR TEXT EJ IFYLLT'.                               
008500      05 FILLER                  PIC X(40)                                
008600         VALUE 'TERMS OF DEL. TEXT NOT REGISTERED'.                       
008700   03    FILLER                  REDEFINES FEL4.                          
008800      05 FEL-4                   PIC X(40)   OCCURS 2.                    
008900                                                                          
009000   03    FEL5.                                                            
009100      05 FILLER                  PIC X(40)                                
009200         VALUE 'LEVVILLKOR FINNS REDAN REGISTRERAT'.                      
009300      05 FILLER                  PIC X(40)                                
009400         VALUE 'TERMS OF DEL. ALREADY REGISTERED'.                        
009500   03    FILLER                  REDEFINES FEL5.                          
009600      05 FEL-5                   PIC X(40)   OCCURS 2.                    
009700                                                                          
009800   03    MEDD1.                                                           
009900      05 FILLER                  PIC X(40)                                
010000         VALUE 'LEVVILLKOR UPPDATERAT'.                                   
010100      05 FILLER                  PIC X(40)                                
010200         VALUE 'TERMS OF DEL. UPDATED'.                                   
010300   03    FILLER                  REDEFINES MEDD1.                         
010400      05 MEDD-1                  PIC X(40)   OCCURS 2.                    
010500                                                                          
010600   03    MEDD2.                                                           
010700      05 FILLER                  PIC X(40)                                
010800         VALUE 'LEVVILLKOR REGISTRERAT'.                                  
010900      05 FILLER                  PIC X(40)                                
011000         VALUE 'TERMS OF DEL. REGISTERED'.                                
011100   03    FILLER                  REDEFINES MEDD2.                         
011200      05 MEDD-2                  PIC X(40)   OCCURS 2.                    
011300                                                                          
011400   03    MEDD3.                                                           
011500      05 FILLER                  PIC X(40)                                
011600         VALUE 'LEVVILLKOR BORTTAGET'.                                    
011700      05 FILLER                  PIC X(40)                                
011800         VALUE 'TERMS OF DEL. DELETED'.                                   
011900   03    FILLER                  REDEFINES MEDD3.                         
012000      05 MEDD-3                  PIC X(40)   OCCURS 2.                    
012100                                                                          
012200     EJECT                                                                
012300******************************************************************        
012400*                                                                         
012500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012600*                                                                         
012700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
012800     SKIP3                                                                
012900*01    MID -COPY W0I80901 -PRE MID-.                                      
013100     EJECT                                                                
013200*01    -COPY WMSGAREA                                                     
013400     EJECT                                                                
013500*  03    MOD -COPY W0O80901 -PRE MOD- -RED MSG-AREA.                      
013700     EJECT                                                                
013800*01    -COPY WMFSAREA                                                     
014000     EJECT                                                                
014100******************************************************************        
014200*                                                                         
014300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*                                                                         
014500 01    IMS-WS.                                                            
014600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014700     SKIP3                                                                
014800*                        **** STATUS-KOD FRÅN IMS                         
014900   03    STATUS-WS               PIC XX.                                  
015000     88    SEGMENT-FINNS                     VALUE '  '.                  
015100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
015200     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
015300     SKIP3                                                                
015400   03    GODK-STATUSKODER.                                                
015500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015600     SKIP3                                                                
015700 01    SSA1                      PIC X(64).                               
015800 01    SSA2                      PIC X(64).                               
015900     EJECT                                                                
016000*                            IMS FUNKTIONSKODER                           
016100*01    -COPY W0003                                                        
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16) VALUE 'DLI-AREA'.              
016500*                            DLI INPUT-OUTPUT AREA                        
016600 01    DLI-IO-AREA.                                                       
016700   03    IO-AREA                 PIC X(250)  VALUE SPACE.                 
016800     SKIP3                                                                
016900*  03    WL473501 -COPY WDGX473E   -RED IO-AREA.                          
017100     EJECT                                                                
017200*  03    WL473511 -COPY WDGX4735   -RED IO-AREA.                          
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600*01    -COPY W0009     -PRE MSG-                                          
017800     EJECT                                                                
017900*01    -COPY W0008     -PRE WDGX-                                         
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300 PROCEDURE DIVISION USING MSG-PCB WDGX-PCB.                               
018400     ENTRY 'DLITCBL' USING MSG-PCB WDGX-PCB.                              
018500                                                                          
018600     PERFORM IMS-GET-MSG                                                  
018700                                                                          
018800     IF SEGMENT-FINNS                                                     
018900       PERFORM A-INIT-SPARA-INPUT                                         
019000       IF WS-GODKAEND-BILD                                                
019100         IF KDLEVVIL-WS NUMERIC                                           
019200           IF MFS-UPDATE                                                  
019300             IF AENDRA                                                    
019400               PERFORM       B-REPLACE-LEVVIL                             
019500             ELSE                                                         
019600               EVALUATE TRUE                                              
019700               WHEN NYUPPLAEGG                                            
019800                 PERFORM       C-INSERT-LEVVIL                            
019900               WHEN BORTTAG                                               
020000                 PERFORM       D-DELETE-LEVVIL                            
020100                WHEN OTHER                                                
020200                 MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                    
020300                 MOVE MFS-RENSA-FAELT TO MOD-UPPDAT-UT                    
020400               END-EVALUATE                                               
020500             END-IF                                                       
020600           ELSE                                                           
020700             PERFORM S01-GET-LEVVIL                                       
020800           END-IF                                                         
020900         ELSE                                                             
021000           MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                          
021100           PERFORM F-ROER-EJ-LEVVIL                                       
021200         END-IF                                                           
021300       ELSE                                                               
021400         MOVE MFS-RENSA-FAELT TO MOD-KDLEVVIL-UT                          
021500                                 MOD-UPPDAT-UT                            
021600       END-IF                                                             
021700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
021800       PERFORM IMS-INSERT-MSG                                             
021900     END-IF                                                               
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT-SPARA-INPUT SECTION.                                              
022500                                                                          
022600     IF MSG-DUBBLA-TRANSKODER                                             
022700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80901                 
022800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023000       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
023100     ELSE                                                                 
023200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80901                  
023300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023500       MOVE ' ' TO MFS-KDTRTYP                                            
023600     END-IF                                                               
023700     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023710     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W0O80901 + 4                  
023800     IF ENGLISH-TEXT                                                      
023900       MOVE +2 TO SPRAK-IX                                                
024000     ELSE                                                                 
024100       MOVE +1 TO SPRAK-IX                                                
024200     END-IF                                                               
024300     IF MID-KDLEVVIL-IN = ALL '+'                                         
024400       MOVE MID-KDLEVVIL-UT TO KDLEVVIL-WS                                
024500     ELSE                                                                 
024600       MOVE MID-KDLEVVIL-IN TO KDLEVVIL-WS                                
024700       MOVE ' ' TO MFS-KDTRTYP                                            
024800     END-IF                                                               
024900     IF MID-UPPDAT-IN = '+'                                               
025000       MOVE MID-UPPDAT-UT TO UPPDAT-WS                                    
025100     ELSE                                                                 
025200       MOVE MID-UPPDAT-IN TO UPPDAT-WS                                    
025300       MOVE ' ' TO MFS-KDTRTYP                                            
025400     END-IF                                                               
025500     MOVE LOW-VALUE TO MSG-AREA                                           
025600     MOVE 'W0O80901' TO MFS-IDMOD                                         
025700     MOVE '0809' TO MOD-IDTRANS                                           
025800     IF KDLEVVIL-WS NUMERIC                                               
025900       MOVE KDLEVVIL-WS   TO W-KDLEVVIL                                   
026000     END-IF                                                               
026100     MOVE KDLEVVIL-WS     TO MOD-KDLEVVIL-UT                              
026200     MOVE UPPDAT-WS       TO MOD-UPPDAT-UT                                
026300     PERFORM F-ROER-EJ-LEVVIL                                             
026400     MOVE MFS-RENSA-FAELT TO MOD-KDLEVVIL-IN                              
026500                             MOD-UPPDAT-IN                                
026600                             MOD-TEMFSFEL                                 
026700                             MOD-TEMFSINF                                 
026800     .                                                                    
026900     EJECT                                                                
027000 B-REPLACE-LEVVIL SECTION.                                                
027100                                                                          
027200     PERFORM IMS-GET-LEVVILTEXT                                           
027300                                                                          
027400     IF SEGMENT-FINNS                                                     
027800       MOVE 1 TO INDX                                                     
027900       PERFORM UNTIL INDX > ANTAL-TEXTRADER                               
028100         IF MID-BELEVVIL(INDX) = ALL '+'                                  
028200           CONTINUE                                                       
028300         ELSE                                                             
028400           MOVE MID-BELEVVIL(INDX) TO LEVVIL-BELEVVIL(INDX)               
028500           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
028600           MOD-BELEVVIL-ATTR(INDX)                                        
028700         END-IF                                                           
028800         ADD 1 TO INDX                                                    
028900       END-PERFORM                                                        
029000       PERFORM IMS-REPLACE-LEVVIL                                         
029100       MOVE MEDD-1 (SPRAK-IX) TO MOD-TEMFSINF                             
029300       PERFORM F-ROER-EJ-LEVVIL                                           
029400     ELSE                                                                 
029500       MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                              
029600       PERFORM E-RENSA-LEVVIL                                             
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 C-INSERT-LEVVIL SECTION.                                                 
030100                                                                          
030200     MOVE W-KDLEVVIL-X TO WL473501                                        
030300     PERFORM IMS-INSERT-LEVVILKOD                                         
030400                                                                          
030500     IF SEGMENT-FINNS-REDAN                                               
030600       PERFORM S01-GET-LEVVIL                                             
030700       MOVE FEL-5 (SPRAK-IX) TO MOD-TEMFSFEL                              
030800     ELSE                                                                 
030900       IF MID-RAD(1) = ALL '+'                                            
031000         PERFORM IMS-GET-LEVVILKOD                                        
031100         PERFORM IMS-DELETE-LEVVIL                                        
031200         MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                            
031300         MOVE MFS-ADD-SAETT-CURSOR TO MOD-BELEVVIL-ATTR(1)                
031400         PERFORM E-RENSA-LEVVIL                                           
031500       ELSE                                                               
031600         MOVE 1 TO INDX                                                   
031700         MOVE SPACE TO LEVVIL-WDGX4735                                    
031800         MOVE '1'   TO LEVVIL-KDSEGKEY                                    
031900         PERFORM UNTIL INDX > ANTAL-TEXTRADER                             
032100           IF MID-BELEVVIL(INDX) = ALL '+'                                
032200             CONTINUE                                                     
032300           ELSE                                                           
032400             MOVE MID-BELEVVIL(INDX) TO LEVVIL-BELEVVIL(INDX)             
032500             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
032600             MOD-BELEVVIL-ATTR(INDX)                                      
032700           END-IF                                                         
032800           ADD 1 TO INDX                                                  
032900         END-PERFORM                                                      
033000         PERFORM IMS-INSERT-LEVVILTEXT                                    
033100         MOVE MEDD-2 (SPRAK-IX) TO MOD-TEMFSINF                           
033200         PERFORM F-ROER-EJ-LEVVIL                                         
033300       END-IF                                                             
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 D-DELETE-LEVVIL  SECTION.                                                
033800                                                                          
033900     PERFORM IMS-GET-LEVVILKOD                                            
034000                                                                          
034100     IF SEGMENT-FINNS                                                     
034200       PERFORM IMS-DELETE-LEVVIL                                          
034300       MOVE MEDD-3 (SPRAK-IX) TO MOD-TEMFSINF                             
034400       PERFORM E-RENSA-LEVVIL                                             
034500     ELSE                                                                 
034600       MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                              
034700       PERFORM F-ROER-EJ-LEVVIL                                           
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 E-RENSA-LEVVIL SECTION.                                                  
035200                                                                          
035300     MOVE 1 TO INDX                                                       
035400     PERFORM UNTIL INDX > ANTAL-TEXTRADER                                 
035600       MOVE MFS-RENSA-FAELT TO MOD-BELEVVIL(INDX)                         
035700       ADD 1 TO INDX                                                      
035800     END-PERFORM                                                          
035900     .                                                                    
036000     EJECT                                                                
036100 F-ROER-EJ-LEVVIL SECTION.                                                
036200                                                                          
036300     MOVE 1 TO INDX                                                       
036400     PERFORM UNTIL INDX > ANTAL-TEXTRADER                                 
036600       MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVVIL(INDX)                       
036700       ADD 1 TO INDX                                                      
036800     END-PERFORM                                                          
036900     .                                                                    
037000     EJECT                                                                
037100 S01-GET-LEVVIL SECTION.                                                  
037200                                                                          
037300     PERFORM IMS-GET-LEVVILTEXT                                           
037400     IF SEGMENT-FINNS                                                     
037500       MOVE 1 TO INDX                                                     
037600       PERFORM UNTIL INDX > ANTAL-TEXTRADER                               
037800         MOVE LEVVIL-BELEVVIL(INDX) TO MOD-BELEVVIL(INDX)                 
037900         ADD 1 TO INDX                                                    
038000       END-PERFORM                                                        
038100     ELSE                                                                 
038200       MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                              
038300       PERFORM E-RENSA-LEVVIL                                             
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700* IMS SEKTIONER                                                           
038800     SKIP3                                                                
038900 IMS-GET-MSG SECTION.                                                     
039000                                                                          
039100     MOVE '  QC' TO GODK-STATUSKODER                                      
039200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     SKIP3                                                                
039700 IMS-INSERT-MSG SECTION.                                                  
039800                                                                          
039900     IF ENGLISH-TEXT                                                      
040000       MOVE 'N' TO MFS-KDHUVOMR                                           
040100     END-IF                                                               
040200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040300     MOVE SPACE TO GODK-STATUSKODER                                       
040400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-GET-LEVVILKOD SECTION.                                               
041000                                                                          
041100     STRING 'WL473501(WDGXKEY  =' W-KDLEVVIL-X ')'                        
041200            DELIMITED BY SIZE INTO SSA1                                   
041300     MOVE '  GE' TO GODK-STATUSKODER                                      
041400     CALL CBLTDLI USING GHU WDGX-PCB DLI-IO-AREA SSA1                     
041500     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
041600     PERFORM IMS-STATUSKONTROLL                                           
041700     .                                                                    
041800     SKIP3                                                                
041900 IMS-GET-LEVVILTEXT SECTION.                                              
042000                                                                          
042100     STRING 'WL473501(WDGXKEY  =' W-KDLEVVIL-X ')'                        
042200            DELIMITED BY SIZE INTO SSA1                                   
042300     MOVE 'WL473511 ' TO SSA2                                             
042400     MOVE '  GE' TO GODK-STATUSKODER                                      
042500     CALL CBLTDLI USING GHU WDGX-PCB DLI-IO-AREA SSA1 SSA2                
042600     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
042700     PERFORM IMS-STATUSKONTROLL                                           
042800     .                                                                    
042900     EJECT                                                                
043000 IMS-INSERT-LEVVILKOD SECTION.                                            
043100                                                                          
043200     MOVE 'WL473501 ' TO SSA1                                             
043300     MOVE '  II' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING ISRT WDGX-PCB DLI-IO-AREA SSA1                    
043500     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900 IMS-INSERT-LEVVILTEXT SECTION.                                           
044000                                                                          
044100     STRING 'WL473501(WDGXKEY  =' W-KDLEVVIL-X ')'                        
044200            DELIMITED BY SIZE INTO SSA1                                   
044300     MOVE 'WL473511 ' TO SSA2                                             
044400     MOVE '  II' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING ISRT WDGX-PCB DLI-IO-AREA SSA1 SSA2               
044600     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     SKIP3                                                                
045000 IMS-REPLACE-LEVVIL SECTION.                                              
045100                                                                          
045200     MOVE '  ' TO GODK-STATUSKODER                                        
045300     CALL CBLTDLI USING REPL WDGX-PCB DLI-IO-AREA                         
045400     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700     SKIP3                                                                
045800 IMS-DELETE-LEVVIL SECTION.                                               
045900                                                                          
046000     MOVE '  ' TO GODK-STATUSKODER                                        
046100     CALL CBLTDLI USING DLET WDGX-PCB DLI-IO-AREA                         
046200     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     .                                                                    
046500     EJECT                                                                
046600 IMS-STATUSKONTROLL SECTION.                                              
046700                                                                          
046800     SET STATUS-IX TO 1                                                   
046900     SEARCH GODK-STATUS                                                   
046910       AT END                                                             
046920         CALL FELLOG                                                      
047000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
047100     END-SEARCH                                                           
047200     .                                                                    
