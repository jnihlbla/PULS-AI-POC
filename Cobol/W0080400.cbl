000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W0080400.                                                
001000*AUTHOR.         ANDERS RYDéN                                             
001100*DATE-WRITTEN.   AUG   84.                                                
001200                                                                          
001400                                                                          
001500*    FUNKTION.                                                            
001600*        PROGRAM - FÖR UPPDATERING AV FRAKTKODER PÅ WDGX-4732             
001700                                                                          
001800*    INDATA.                                                              
001900*        TRANSAKTION: W0T804                                              
002000*        MID:         W0I80401                                            
002100                                                                          
002200*    UTDATA.                                                              
002300*        MOD:         W0O80401                                            
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W0080400'.            
003700 77    JA                        PIC X       VALUE 'J'.                   
003800 77    NEJ                       PIC X       VALUE 'N'.                   
003900 77    KDFRAKT-WS                PIC X(3)    VALUE SPACE.                 
004000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77    SPRAK-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +0   COMP SYNC.        
004300 77    ANTAL-TEXTRADER           PIC S9(2)   VALUE +6   COMP SYNC.        
004400 77    WS-IDTRANS                PIC X(4).                                
004500       88  WS-GODKAEND-BILD                  VALUE '0804'.                
004600 77    UPPDAT-WS                 PIC X.                                   
004700       88  AENDRA                            VALUE 'Ä' 'C'.               
004800       88  BORTTAG                           VALUE 'B' 'D'.               
004900       88  NYUPPLAEGG                        VALUE 'N'.                   
005000     SKIP3                                                                
005100     EJECT                                                                
005110                                                                          
005120 01  DYNAMISKA-SUBPROGRAM.                                                
005130   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005140   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005150                                                                          
005200 01    NYCKLAR-TILL-DLI.                                                  
005300   03    W-KDFRAKT-X.                                                     
005400     05    W-IDHTYP              PIC X(4)    VALUE '4732'.                
005500     05    W-KDFRAKT             PIC S9(3)   VALUE ZERO  COMP-3.          
005600     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
005700     SKIP3                                                                
005800 01    MEDDELANDE.                                                        
005900   03    FEL1.                                                            
006000      05 FILLER                  PIC X(40)                                
006100         VALUE 'FEL NYCKEL'.                                              
006200      05 FILLER                  PIC X(40)                                
006300         VALUE 'WRONG KEY '.                                              
006400   03    FILLER                  REDEFINES FEL1.                          
006500      05 FEL-1                   PIC X(40)   OCCURS 2.                    
006600                                                                          
006700   03    FEL2.                                                            
006800      05 FILLER                  PIC X(40)                                
006900         VALUE 'INMATNINGSFÄLT FEL IFYLLT'.                               
007000      05 FILLER                  PIC X(40)                                
007100         VALUE 'INPUT FIELD NOT CORRECT'.                                 
007200   03    FILLER                  REDEFINES FEL2.                          
007300      05 FEL-2                   PIC X(40)   OCCURS 2.                    
007400                                                                          
007500   03    FEL3.                                                            
007600      05 FILLER                  PIC X(40)                                
007700         VALUE 'FRAKTKOD FINNS EJ REGISTRERAD'.                           
007800      05 FILLER                  PIC X(40)                                
007900         VALUE 'FREIGHT CODE NOT REGISTERED '.                            
008000   03    FILLER                  REDEFINES FEL3.                          
008100      05 FEL-3                   PIC X(40)   OCCURS 2.                    
008200                                                                          
008300   03    FEL4.                                                            
008400      05 FILLER                  PIC X(40)                                
008500         VALUE 'FRAKTTEXT EJ IFYLLD'.                                     
008600      05 FILLER                  PIC X(40)                                
008700         VALUE 'FREIGHT TEXT NOT REGISTRED'.                              
008800   03    FILLER                  REDEFINES FEL4.                          
008900      05 FEL-4                   PIC X(40)   OCCURS 2.                    
009000                                                                          
009100   03    FEL5.                                                            
009200      05 FILLER                  PIC X(40)                                
009300         VALUE 'FRAKTKOD FINNS REDAN REGISTRERAD'.                        
009400      05 FILLER                  PIC X(40)                                
009500         VALUE 'FREIGHT CODE ALREADY REGISTERED'.                         
009600   03    FILLER                  REDEFINES FEL5.                          
009700      05 FEL-5                   PIC X(40)   OCCURS 2.                    
009800                                                                          
009900   03    MEDD1.                                                           
010000      05 FILLER                  PIC X(40)                                
010100         VALUE 'FRAKTTEXT UPPDATERAD'.                                    
010200      05 FILLER                  PIC X(40)                                
010300         VALUE 'FREIGHT TEXT UPDATED'.                                    
010400   03    FILLER                  REDEFINES MEDD1.                         
010500      05 MEDD-1                  PIC X(40)   OCCURS 2.                    
010600                                                                          
010700   03    MEDD2.                                                           
010800      05 FILLER                  PIC X(40)                                
010900         VALUE 'FRAKTKOD REGISTRERAD'.                                    
011000      05 FILLER                  PIC X(40)                                
011100         VALUE 'FREIGHT CODE REGISTERED'.                                 
011200   03    FILLER                  REDEFINES MEDD2.                         
011300      05 MEDD-2                  PIC X(40)   OCCURS 2.                    
011400                                                                          
011500   03    MEDD3.                                                           
011600      05 FILLER                  PIC X(40)                                
011700         VALUE 'FRAKTKOD BORTTAGEN'.                                      
011800      05 FILLER                  PIC X(40)                                
011900         VALUE 'FREIGHT CODE DELETED'.                                    
012000   03    FILLER                  REDEFINES MEDD3.                         
012100      05 MEDD-3                  PIC X(40)   OCCURS 2.                    
012200                                                                          
012300     EJECT                                                                
012400******************************************************************        
012500*                                                                         
012600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012700*                                                                         
012800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
012900     SKIP3                                                                
013000*01    MID -COPY W0I80401 -PRE MID-.                                      
013200     EJECT                                                                
013300*01    -COPY WMSGAREA                                                     
013500     EJECT                                                                
013600*  03    MOD -COPY W0O80401 -PRE MOD- -RED MSG-AREA.                      
013800     EJECT                                                                
013900*01    -COPY WMFSAREA                                                     
014100     EJECT                                                                
014200******************************************************************        
014300*                                                                         
014400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600 01    IMS-WS.                                                            
014700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014800     SKIP3                                                                
014900*                        **** STATUS-KOD FRÅN IMS                         
015000   03    STATUS-WS               PIC XX.                                  
015100     88    SEGMENT-FINNS                     VALUE '  '.                  
015200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
015300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
015400     SKIP3                                                                
015500   03    GODK-STATUSKODER.                                                
015600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015700     SKIP3                                                                
015800 01    SSA1                      PIC X(64).                               
015900 01    SSA2                      PIC X(64).                               
016000     EJECT                                                                
016100*                            IMS FUNKTIONSKODER                           
016200*01    -COPY W0003                                                        
016400     EJECT                                                                
016500*                            DLI INPUT-OUTPUT AREA                        
016600 01    DLI-IO-AREA.                                                       
016700   03    IO-AREA                 PIC X(140)  VALUE SPACE.                 
016800     SKIP3                                                                
016900*  03    WL473201 -COPY WDGX473B   -RED IO-AREA.                          
017100     EJECT                                                                
017200*  03    WL473211 -COPY WDGX4732   -RED IO-AREA.                          
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
019100         IF KDFRAKT-WS NUMERIC                                            
019200           IF MFS-UPDATE                                                  
019300             IF AENDRA                                                    
019400               PERFORM       B-REPLACE-FRAKTTEXT                          
019500             ELSE                                                         
019600               EVALUATE TRUE                                              
019700               WHEN NYUPPLAEGG                                            
019800                 PERFORM       C-INSERT-FRAKTKOD                          
019900               WHEN BORTTAG                                               
020000                 PERFORM       D-DELETE-FRAKTKOD                          
020100                WHEN OTHER                                                
020200                 MOVE FEL-2 (SPRAK-IX) TO MOD-MESSAGE-RAD1                
020300                 MOVE MFS-RENSA-FAELT TO MOD-UPPDAT-UT                    
020400               END-EVALUATE                                               
020500             END-IF                                                       
020600           ELSE                                                           
020700             PERFORM S01-GET-FRAKTTEXT                                    
020800           END-IF                                                         
020900         ELSE                                                             
021000           MOVE FEL-1 (SPRAK-IX) TO MOD-MESSAGE-RAD1                      
021100           PERFORM F-ROER-EJ-FRAKTTEXT                                    
021200         END-IF                                                           
021300       ELSE                                                               
021400         MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-UT                           
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
022700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80401                 
022800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023000       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
023100     ELSE                                                                 
023200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80401                  
023300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023500       MOVE ' ' TO MFS-KDTRTYP                                            
023600     END-IF                                                               
023610     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W0O80401 + 4                  
023700     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023800     IF ENGLISH-TEXT                                                      
023900       MOVE +2 TO SPRAK-IX                                                
024000     ELSE                                                                 
024100       MOVE +1 TO SPRAK-IX                                                
024200     END-IF                                                               
024300     IF MID-KDFRAKT-IN = ALL '+'                                          
024400       MOVE MID-KDFRAKT-UT TO KDFRAKT-WS                                  
024500       INSPECT KDFRAKT-WS REPLACING LEADING SPACE BY ZERO                 
024600     ELSE                                                                 
024700       MOVE MID-KDFRAKT-IN TO KDFRAKT-WS                                  
024800       MOVE ' ' TO MFS-KDTRTYP                                            
024900     END-IF                                                               
025000     IF MID-UPPDAT-IN = '+'                                               
025100       MOVE MID-UPPDAT-UT TO UPPDAT-WS                                    
025200     ELSE                                                                 
025300       MOVE MID-UPPDAT-IN TO UPPDAT-WS                                    
025400       MOVE ' ' TO MFS-KDTRTYP                                            
025500     END-IF                                                               
025600     MOVE LOW-VALUE TO MSG-AREA                                           
025700     MOVE 'W0O80401' TO MFS-IDMOD                                         
025800     MOVE '0804' TO MOD-IDTRANS                                           
025900     IF KDFRAKT-WS NUMERIC                                                
026000       MOVE KDFRAKT-WS   TO W-KDFRAKT                                     
026100     END-IF                                                               
026200     MOVE KDFRAKT-WS      TO MOD-KDFRAKT-UT                               
026300     MOVE UPPDAT-WS       TO MOD-UPPDAT-UT                                
026400     PERFORM F-ROER-EJ-FRAKTTEXT                                          
026500     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
026600     MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-IN                               
026700                             MOD-UPPDAT-IN                                
026800                             MOD-MESSAGE-RAD1                             
026900                             MOD-MESSAGE-RAD23                            
027000     .                                                                    
027100     EJECT                                                                
027200 B-REPLACE-FRAKTTEXT SECTION.                                             
027300                                                                          
027400     PERFORM IMS-GET-FRAKTTEXT                                            
027500                                                                          
027600     IF SEGMENT-FINNS                                                     
027700       IF MID-FRAKTTEXT-GRUPP = ALL '+'                                   
027800         MOVE MEDD-1 (SPRAK-IX) TO MOD-MESSAGE-C1                         
027900       ELSE                                                               
028000         MOVE 1 TO INDX                                                   
028100         PERFORM UNTIL INDX > ANTAL-TEXTRADER                             
028300           IF MID-FRAKTTEXT(INDX) = ALL '+'                               
028400             CONTINUE                                                     
028500           ELSE                                                           
028600             MOVE MID-FRAKTTEXT(INDX) TO FRAKT-BEFRAKT(INDX)              
028700             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
028800             MOD-FRAKTTEXT-ATTR(INDX)                                     
028900           END-IF                                                         
029000           ADD 1 TO INDX                                                  
029100         END-PERFORM                                                      
029200         PERFORM IMS-REPLACE-FRAKTTEXT                                    
029300         MOVE MEDD-1 (SPRAK-IX) TO MOD-MESSAGE-C1                         
029400       END-IF                                                             
029500       PERFORM F-ROER-EJ-FRAKTTEXT                                        
029600     ELSE                                                                 
029700       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
029800       PERFORM E-RENSA-FRAKTTEXT                                          
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 C-INSERT-FRAKTKOD SECTION.                                               
030300                                                                          
030400     MOVE W-KDFRAKT-X TO WL473201                                         
030500     PERFORM IMS-INSERT-FRAKTKOD                                          
030600                                                                          
030700     IF SEGMENT-FINNS-REDAN                                               
030800       PERFORM S01-GET-FRAKTTEXT                                          
030900       MOVE FEL-5 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
031000     ELSE                                                                 
031100       IF MID-FRAKTTEXT-GRUPP = ALL '+'                                   
031200         PERFORM IMS-GET-FRAKTKOD                                         
031300         PERFORM IMS-DELETE-FRAKTKOD                                      
031400         MOVE FEL-4 (SPRAK-IX) TO MOD-MESSAGE-RAD1                        
031500         MOVE MFS-ADD-SAETT-CURSOR TO MOD-FRAKTTEXT-ATTR(1)               
031600         PERFORM E-RENSA-FRAKTTEXT                                        
031700       ELSE                                                               
031800         MOVE SPACE TO FRAKT-WDGX4732                                     
031900         MOVE '1' TO FRAKT-KDSEGKEY                                       
032000         MOVE 1 TO INDX                                                   
032100         PERFORM UNTIL INDX > ANTAL-TEXTRADER                             
032300           IF MID-FRAKTTEXT(INDX) = ALL '+'                               
032400             CONTINUE                                                     
032500           ELSE                                                           
032600             MOVE MID-FRAKTTEXT(INDX) TO FRAKT-BEFRAKT(INDX)              
032700             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
032800             MOD-FRAKTTEXT-ATTR(INDX)                                     
032900           END-IF                                                         
033000           ADD 1 TO INDX                                                  
033100         END-PERFORM                                                      
033200         PERFORM IMS-INSERT-FRAKTTEXT                                     
033300         MOVE MEDD-2 (SPRAK-IX) TO MOD-MESSAGE-C1                         
033400         PERFORM F-ROER-EJ-FRAKTTEXT                                      
033500       END-IF                                                             
033600     END-IF                                                               
033700     CONTINUE.                                                            
033800     EJECT                                                                
033900 D-DELETE-FRAKTKOD SECTION.                                               
034000                                                                          
034100     PERFORM IMS-GET-FRAKTKOD                                             
034200                                                                          
034300     IF SEGMENT-FINNS                                                     
034400       PERFORM IMS-DELETE-FRAKTKOD                                        
034500       MOVE MEDD-3 (SPRAK-IX) TO MOD-MESSAGE-C1                           
034600       PERFORM E-RENSA-FRAKTTEXT                                          
034700     ELSE                                                                 
034800       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
034900       PERFORM F-ROER-EJ-FRAKTTEXT                                        
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 E-RENSA-FRAKTTEXT SECTION.                                               
035400                                                                          
035500     MOVE 1 TO INDX                                                       
035600     PERFORM UNTIL INDX > ANTAL-TEXTRADER                                 
035800       MOVE MFS-RENSA-FAELT TO MOD-FRAKTTEXT(INDX)                        
035900       ADD 1 TO INDX                                                      
036000     END-PERFORM                                                          
036100     .                                                                    
036200     SKIP3                                                                
036300 F-ROER-EJ-FRAKTTEXT SECTION.                                             
036400                                                                          
036500     MOVE 1 TO INDX                                                       
036600     PERFORM UNTIL INDX > ANTAL-TEXTRADER                                 
036800       MOVE MFS-ROER-EJ-FAELT TO MOD-FRAKTTEXT(INDX)                      
036900       ADD 1 TO INDX                                                      
037000     END-PERFORM                                                          
037100     .                                                                    
037200     EJECT                                                                
037300 S01-GET-FRAKTTEXT SECTION.                                               
037400                                                                          
037500     PERFORM IMS-GET-FRAKTTEXT                                            
037600     IF SEGMENT-FINNS                                                     
037700       MOVE 1 TO INDX                                                     
037800       PERFORM UNTIL INDX > ANTAL-TEXTRADER                               
038000         MOVE FRAKT-BEFRAKT(INDX) TO MOD-FRAKTTEXT(INDX)                  
038100         ADD 1 TO INDX                                                    
038200       END-PERFORM                                                        
038300     ELSE                                                                 
038400       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
038500       PERFORM E-RENSA-FRAKTTEXT                                          
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900* IMS SEKTIONER                                                           
039000     SKIP3                                                                
039100 IMS-GET-MSG SECTION.                                                     
039200                                                                          
039300     MOVE '  QC' TO GODK-STATUSKODER                                      
039400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039600     PERFORM IMS-STATUSKONTROLL                                           
039700     .                                                                    
039800     SKIP3                                                                
039900 IMS-INSERT-MSG SECTION.                                                  
040000                                                                          
040100     IF ENGLISH-TEXT                                                      
040200       MOVE 'N' TO MFS-KDHUVOMR                                           
040300     END-IF                                                               
040400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040500     MOVE SPACE TO GODK-STATUSKODER                                       
040600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000     EJECT                                                                
041100 IMS-GET-FRAKTKOD SECTION.                                                
041200                                                                          
041300     STRING 'WL473201(WDGXKEY  =' W-KDFRAKT-X ')'                         
041400            DELIMITED BY SIZE INTO SSA1                                   
041500     MOVE '  GE' TO GODK-STATUSKODER                                      
041600     CALL CBLTDLI USING GHU WDGX-PCB DLI-IO-AREA SSA1                     
041700     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     SKIP3                                                                
042100 IMS-GET-FRAKTTEXT SECTION.                                               
042200                                                                          
042300     STRING 'WL473201(WDGXKEY  =' W-KDFRAKT-X ')'                         
042400            DELIMITED BY SIZE INTO SSA1                                   
042500     MOVE 'WL473211 ' TO SSA2                                             
042600     MOVE '  GE' TO GODK-STATUSKODER                                      
042700     CALL CBLTDLI USING GHU WDGX-PCB DLI-IO-AREA SSA1 SSA2                
042800     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     .                                                                    
043100     EJECT                                                                
043200 IMS-INSERT-FRAKTKOD SECTION.                                             
043300                                                                          
043400     MOVE 'WL473201 ' TO SSA1                                             
043500     MOVE '  II' TO GODK-STATUSKODER                                      
043600     CALL CBLTDLI USING ISRT WDGX-PCB DLI-IO-AREA SSA1                    
043700     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
043800     PERFORM IMS-STATUSKONTROLL                                           
043900     .                                                                    
044000     SKIP3                                                                
044100 IMS-INSERT-FRAKTTEXT SECTION.                                            
044200                                                                          
044300     STRING 'WL473201(WDGXKEY  =' W-KDFRAKT-X ')'                         
044400            DELIMITED BY SIZE INTO SSA1                                   
044500     MOVE 'WL473211 ' TO SSA2                                             
044600     MOVE '  II' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING ISRT WDGX-PCB DLI-IO-AREA SSA1 SSA2               
044800     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
044900     PERFORM IMS-STATUSKONTROLL                                           
045000     .                                                                    
045100     SKIP3                                                                
045200 IMS-REPLACE-FRAKTTEXT SECTION.                                           
045300                                                                          
045400     MOVE '  ' TO GODK-STATUSKODER                                        
045500     CALL CBLTDLI USING REPL WDGX-PCB DLI-IO-AREA                         
045600     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     .                                                                    
045900     SKIP3                                                                
046000 IMS-DELETE-FRAKTKOD SECTION.                                             
046100                                                                          
046200     MOVE '  ' TO GODK-STATUSKODER                                        
046300     CALL CBLTDLI USING DLET WDGX-PCB DLI-IO-AREA                         
046400     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700     EJECT                                                                
046800 IMS-STATUSKONTROLL SECTION.                                              
046900                                                                          
047000     SET STATUS-IX TO 1                                                   
047100     SEARCH GODK-STATUS                                                   
047110       AT END                                                             
047120         CALL FELLOG                                                      
047200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
047300     END-SEARCH                                                           
047500     .                                                                    
