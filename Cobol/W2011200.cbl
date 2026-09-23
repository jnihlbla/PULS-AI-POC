000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W2011200.                                        
000300 AUTHOR.                 IDK BS.                                          
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.           JANUARI 1979.                                    
000600     REMARKS.                                                             
000700*        FUNKTION. TP-PROGRAM FRÅGE.                                      
000800*                    P R I S - I N F O L E V.                             
000900*        INDATA.                                                          
001000*            TRANSAKTION: W2T112                                          
001100*            MID:     W2I11201                                            
001200*        UTDATA.                                                          
001300*            MOD:     W2O11201                                            
001400*        SUBPROGRAM.                                                      
001500*            FELLOG                                                       
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP3                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77      IDLEVNR-WS      PIC X(5)    VALUE SPACE.                         
002500 77      INDX            PIC S9(9)   VALUE +0    COMP SYNC.               
002600 77      MAX-LINE        PIC S9(9)   VALUE +0    COMP SYNC.               
002700 77      JA              PIC X       VALUE 'J'.                           
002800 77      NEJ             PIC X       VALUE 'N'.                           
002900 77      W-ANT           PIC S9(4)   VALUE +0    COMP SYNC.               
003000 77      MOD-MAX-LANGD   PIC S9(4)   VALUE +460  COMP SYNC.               
003100 01  WS-TITULF.                                                           
003200     03  FILLER          PIC X(1) VALUE SPACE.                            
003300     03  WS-TITULF-NUM   PIC 9(6).                                        
003400 01  WS-KDVALLEV         PIC 9(3) VALUE ZERO.                             
003500 01  WS-KDVALISO         PIC X(3) VALUE SPACE.                            
003600     SKIP3                                                                
003700 01      W-IDLEVNR-X.                                                     
003800   03    W-IDLEVNR       PIC X(5)    VALUE SPACE.                         
003900 01      W-IDLAND-X.                                                      
004000   03    W-IDLAND        PIC X(2)    VALUE SPACE.                         
004100     SKIP3                                                                
004200 01      MEDDELANDEN.                                                     
004300   03    FEL1            PIC X(28)                                        
004400                    VALUE 'SUPPLY NUMBER IS NOT NUMERIC'.                 
004500   03    FEL2            PIC X(36)                                        
004600                    VALUE 'THIS SUPPLIER IS NOT IN THE DATABASE'.         
004700   03    MED1            PIC X(25)                                        
004800                    VALUE 'FOR MORE RATES PRESS SEND'.                    
004900     SKIP3                                                                
005000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005100*                                                                         
005200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
005300     SKIP3                                                                
005400*01 -COPY WMSGINIT                                                        
005500     SKIP3                                                                
005600 01  FILLER                      PIC X(8)    VALUE 'W553LVAL'.            
005700*01 -COPY W553LVAL                                                        
005800     EJECT                                                                
005900 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
006000*01 -COPY WWIDFTG                                                         
006100     EJECT                                                                
006200 01  FILLER                      PIC X(16)   VALUE 'SORT-AREA'.           
006300                                                                          
006400 01  GENERELLA-SUBPROGRAM.                                                
006500   03 CBLTDLI            PIC X(8)    VALUE 'CBLTDLI '.                    
006600   03 FELLOG             PIC X(8)    VALUE 'FELLOG  '.                    
006700   03 W005INIT           PIC X(8)    VALUE 'W005INIT'.                    
006800                                                                          
006900 01  W-DATUM.                                                             
007000   03  W-AA              PIC 9(2).                                        
007100   03  W-MM              PIC 9(2).                                        
007200   03  W-DD              PIC 9(2).                                        
007300 01  W-DATUM-N REDEFINES W-DATUM PIC 9(6).                                
007400     EJECT                                                                
007500*                        ****    TP-AREOR                                 
007600     SKIP3                                                                
007700*01      MID -COPY W2I11201 -PRE MID-.                                    
007800     SKIP3                                                                
007900*01      -COPY WMSGAREA                                                   
008000     SKIP3                                                                
008100*  03    MOD -COPY W2O11201 -PRE MOD- -RED MSG-AREA.                      
008200     EJECT                                                                
008300*01  -COPY WMFSAREA                                                       
008400     EJECT                                                                
008500******************************************************************        
008600*****                                                                     
008700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*****                                                                     
008900 01  IMS-WS.                                                              
009000   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
009100     SKIP3                                                                
009200*****                    **** STATUS-KOD FRÅN IMS                         
009300   03    STATUS-WS       PIC XX.                                          
009400         88  SEGMENT-FINNS       VALUE '  '.                              
009500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
009600     SKIP3                                                                
009700   03    GODK-STATUSKODER.                                                
009800     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01      SSA1            PIC X(32).                                       
010100 01      SSA2            PIC X(32).                                       
010200     SKIP3                                                                
010300*                            IMS FUNKTIONSKODER                           
010400*01      -COPY W0003                                                      
010500     EJECT                                                                
010600*                            DLI INPUT-OUTPUT AREA                        
010700 01      DLI-IO-AREA     PIC X(300)  VALUE SPACE.                         
010800     SKIP3                                                                
010900*01      WLLEVA01 -COPY WDF101 -PRE LEVA-   -RED DLI-IO-AREA.             
011000     EJECT                                                                
011100*01      WLLEVA11 -COPY WDF102 -PRE LEVA-   -RED DLI-IO-AREA.             
011200     EJECT                                                                
011300*01  WLLEVA12 -COPY WDF103 -PRE LEVA- -RED DLI-IO-AREA.                   
011400     EJECT                                                                
011500*01  WLLEVA13 -COPY WDF104 -PRE LEVA- -RED DLI-IO-AREA.                   
011600     EJECT                                                                
011700*01  WLLEVA14 -COPY WDF106 -PRE LEVA- -RED DLI-IO-AREA.                   
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000*01  -COPY W0009     -PRE MSG-                                            
012100     SKIP3                                                                
012200*01  -COPY W0008     -PRE USEA-                                           
012300         05  FILLER           PIC X.                                      
012400     EJECT                                                                
012500*01  -COPY W0008     -PRE LEVA-                                           
012600         05  FILLER           PIC X.                                      
012700     EJECT                                                                
012800 PROCEDURE DIVISION USING MSG-PCB USEA-PCB LEVA-PCB.                      
012900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LEVA-PCB.                     
013000     PERFORM IMS-GET-MSG                                                  
013100     IF SEGMENT-FINNS                                                     
013200       IF MSG-DUBBLA-TRANSKODER                                           
013300         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11201               
013400         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
013500         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
013600         MOVE ZERO TO MID-ANT                                             
013700       ELSE                                                               
013800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11201                
013900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
014000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
014100       END-IF                                                             
014200       MOVE ALL '+'         TO MSGI-WMSGINIT                              
014300       MOVE '001'           TO MSGI-KDCALL                                
014400       MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                          
014500       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
014600       MOVE '2112'          TO MSGI-IDTRANS                               
014700       IF MID-IDLEVNR1 NOT = ALL '+'                                      
014800          MOVE MID-IDLEVNR1 TO MSGI-IDLEVNR                               
014900       END-IF                                                             
015000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
015100       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
015200                               WS-IDFTG                                   
015300       IF MID-IDLEVNR1 = ALL '+'                                          
015400         MOVE MSGI-IDLEVNR TO IDLEVNR-WS                                  
015500       ELSE                                                               
015600         MOVE MID-IDLEVNR1 TO IDLEVNR-WS                                  
015700         MOVE ZERO TO MID-ANT                                             
015800       END-IF                                                             
015900       MOVE '2112' TO MOD-TRANS-NUMMER                                    
016000       MOVE 'W2O112N1' TO MFS-IDMOD                                       
016100       MOVE IDLEVNR-WS TO MOD-IDLEVNR-UT                                  
016200       IF  MID-ANT NUMERIC                                                
016300       AND MFS-IDTRANS = '2112'                                           
016400           MOVE MID-ANT TO MOD-ANT                                        
016500           MOVE MID-ANT TO W-ANT                                          
016600       ELSE                                                               
016700           MOVE ZERO    TO MOD-ANT                                        
016800           MOVE ZERO    TO W-ANT                                          
016900       END-IF                                                             
017000       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN MOD-MESSAGE                 
017100       MOVE LOW-VALUE TO MOD-AREA                                         
017200       PERFORM AA-LAS-LEVERANTOR                                          
017300       MOVE MOD-MAX-LANGD TO MSG-KVLL                                     
017400       PERFORM IMS-INSERT-MSG                                             
017500     END-IF                                                               
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 AA-LAS-LEVERANTOR  SECTION.                                              
018100     MOVE MFS-BLANKA-UT-FAELT TO MOD-MESSAGE-BOTTOM                       
018200     MOVE +10      TO MAX-LINE                                            
018300     MOVE +1 TO INDX                                                      
018400                                                                          
018500     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
018600     PERFORM IMS-GET-LEV                                                  
018700     IF SEGMENT-FINNS                                                     
018800                                                                          
018900        SET MOD-RAB-INDX TO INDX                                          
019000        PERFORM UNTIL MOD-RAB-INDX NOT < MAX-LINE                         
019100          MOVE MFS-BLANKA-UT-FAELT TO MOD-RABRADER (MOD-RAB-INDX)         
019200          SET MOD-RAB-INDX UP BY +1                                       
019300        END-PERFORM                                                       
019400                                                                          
019500        SET MOD-LARMGR-INDX TO INDX                                       
019600        PERFORM UNTIL MOD-LARMGR-INDX NOT < MAX-LINE                      
019700          IF MOD-ANT EQUAL ZERO                                           
019800              MOVE MFS-BLANKA-UT-FAELT                                    
019900                            TO MOD-LARMGRRADER (MOD-LARMGR-INDX)          
020000          ELSE                                                            
020100              MOVE MFS-ROER-EJ-FAELT                                      
020200                            TO MOD-LARMGRRADER (MOD-LARMGR-INDX)          
020300          END-IF                                                          
020400          SET MOD-LARMGR-INDX UP BY +1                                    
020500        END-PERFORM                                                       
020600                                                                          
020700        IF MOD-ANT NOT = ZERO                                             
020800          MOVE MFS-ROER-EJ-FAELT TO MOD-BELEV                             
020900        END-IF                                                            
021000                                                                          
021100        IF  MOD-ANT EQUAL ZERO                                            
021200          IF IDFTG-CN                                                     
021300            MOVE 'CN' TO W-IDLAND                                         
021400          ELSE                                                            
021410            IF IDFTG-US                                                   
021420              MOVE 'US' TO W-IDLAND                                       
021430            ELSE                                                          
021500              MOVE 'SE' TO W-IDLAND                                       
021600            END-IF                                                        
021610          END-IF                                                          
021700          PERFORM IMS-GETNEXT-TULL                                        
021800          IF SEGMENT-FINNS                                                
021900            MOVE LEVA-TULL-KDVALLEV  TO MOD-KDVALLEV                      
022000                                        WS-KDVALLEV                       
022100            MOVE LEVA-TULL-RETULF-2  TO MOD-RETULF-2                      
022200            MOVE LEVA-TULL-RETULF-1  TO MOD-RETULF-1                      
022300            IF LEVA-TULL-TITULF > ZERO                                    
022400               MOVE LEVA-TULL-TITULF TO WS-TITULF-NUM                     
022500               MOVE WS-TITULF        TO MOD-TITULF                        
022600            ELSE                                                          
022700               MOVE SPACE            TO MOD-TITULF                        
022800            END-IF                                                        
022900          END-IF                                                          
023000        END-IF                                                            
023100                                                                          
023200        MOVE SPACE              TO WS-KDVALISO                            
023300        MOVE +1                 TO VAL-IX                                 
023400        PERFORM VAL-IX-MAX TIMES                                          
023500          IF WS-KDVALLEV = VAL-KDVALUTA(VAL-IX)                           
023600            MOVE VAL-KDVALISO(VAL-IX) TO WS-KDVALISO                      
023700          END-IF                                                          
023800          ADD +1                TO VAL-IX                                 
023900        END-PERFORM                                                       
024000        IF WS-KDVALISO = SPACE                                            
024100          MOVE MFS-RENSA-FAELT  TO MOD-KDVALISO                           
024200        ELSE                                                              
024300          MOVE WS-KDVALISO      TO MOD-KDVALISO                           
024400        END-IF                                                            
024500                                                                          
024600        MOVE +0 TO INDX                                                   
024700        SET MOD-RAB-INDX TO INDX                                          
024800        PERFORM IMS-GETNEXT-RABATT                                        
024900        PERFORM UNTIL (SEGMENT-SAKNAS)                                    
025000        OR (MOD-RAB-INDX = MAX-LINE)                                      
025100            ADD +1 TO INDX                                                
025200            IF  INDX GREATER MOD-ANT                                      
025300              SET MOD-RAB-INDX UP BY +1                                   
025400              MOVE LEVA-KDRAB TO MOD-KDRAB (MOD-RAB-INDX)                 
025500              MOVE LEVA-RERAB TO MOD-RERAB (MOD-RAB-INDX)                 
025600            END-IF                                                        
025700            PERFORM IMS-GETNEXT-RABATT                                    
025800        END-PERFORM                                                       
025900                                                                          
026000        IF  SEGMENT-FINNS                                                 
026100          MOVE MED1 TO MOD-MESSAGE-BOTTOM                                 
026200          ADD MAX-LINE TO MOD-ANT                                         
026300        END-IF                                                            
026400                                                                          
026500        IF  W-ANT = ZERO                                                  
026600          MOVE +0 TO INDX                                                 
026700          SET MOD-LARMGR-INDX TO INDX                                     
026800          PERFORM IMS-GETNEXT-LARM                                        
026900          PERFORM UNTIL SEGMENT-SAKNAS                                    
027000            SET MOD-LARMGR-INDX UP BY 1                                   
027100            MOVE LEVA-KDPRODGRP TO MOD-KDPRODGRP (MOD-LARMGR-INDX)        
027200            MOVE LEVA-KRLARMG   TO MOD-KRLARMG (MOD-LARMGR-INDX)          
027300            MOVE LEVA-RELARMG   TO MOD-RELARMG (MOD-LARMGR-INDX)          
027400            PERFORM IMS-GETNEXT-LARM                                      
027500          END-PERFORM                                                     
027600                                                                          
027700          PERFORM IMS-GETNEXT-ADR                                         
027800          IF SEGMENT-FINNS                                                
027900            MOVE LEVA-ADR-BELEV TO MOD-BELEV                              
028000          END-IF                                                          
028100        END-IF                                                            
028200      ELSE                                                                
028300        PERFORM MFS-RENSA-BILD                                            
028400        MOVE FEL2 TO MOD-MESSAGE                                          
028500      END-IF                                                              
028600     .                                                                    
028700     EJECT                                                                
028800 MFS-RENSA-BILD SECTION.                                                  
028900     MOVE MFS-RENSA-FAELT TO MOD-ANT                                      
029000                             MOD-BELEV                                    
029100                             MOD-KDVALLEV                                 
029200                             MOD-RETULF-1                                 
029300                             MOD-RETULF-2                                 
029400                             MOD-TITULF                                   
029500                                                                          
029600     SET MOD-RAB-INDX    TO +1                                            
029700     SET MOD-LARMGR-INDX TO +1                                            
029800     PERFORM UNTIL MOD-RAB-INDX > MAX-LINE                                
029900       MOVE MFS-RENSA-FAELT TO MOD-KDRAB (MOD-RAB-INDX)                   
030000                               MOD-RERAB (MOD-RAB-INDX)                   
030100                               MOD-KDPRODGRP (MOD-LARMGR-INDX)            
030200                               MOD-KRLARMG (MOD-LARMGR-INDX)              
030300                               MOD-RELARMG (MOD-LARMGR-INDX)              
030400       SET MOD-RAB-INDX    UP BY +1                                       
030500       SET MOD-LARMGR-INDX UP BY +1                                       
030600     END-PERFORM                                                          
030700     .                                                                    
030800     EJECT                                                                
030900* IMS SEKTIONER                                                           
031000     SKIP3                                                                
031100 IMS-GET-MSG SECTION.                                                     
031200     MOVE '  QC' TO GODK-STATUSKODER                                      
031300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031700     SKIP3                                                                
031800 IMS-INSERT-MSG SECTION.                                                  
031900     IF SWEDISH-TEXT                                                      
032000        MOVE '0' TO MFS-KDHUVOMR                                          
032100     END-IF                                                               
032200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032300     MOVE SPACE TO GODK-STATUSKODER                                       
032400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032600     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800     SKIP3                                                                
032900     EJECT                                                                
033000 IMS-GET-LEV SECTION.                                                     
033100     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
033200            DELIMITED BY SIZE INTO SSA1                                   
033300     MOVE '  GE' TO GODK-STATUSKODER                                      
033400     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
033500     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
033600     PERFORM IMS-STATUSKONTROLL                                           
033700     .                                                                    
033800     SKIP3                                                                
033900 IMS-GETNEXT-TULL SECTION.                                                
034000     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
034100            DELIMITED BY SIZE INTO SSA1                                   
034200     MOVE '  GE' TO GODK-STATUSKODER                                      
034300     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA SSA1                     
034400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
034500     PERFORM IMS-STATUSKONTROLL                                           
034600     .                                                                    
034700     SKIP3                                                                
034800 IMS-GETNEXT-RABATT SECTION.                                              
034900     MOVE 'WLLEVA12' TO SSA1                                              
035000     MOVE '  GE' TO GODK-STATUSKODER                                      
035100     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA SSA1                     
035200     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     SKIP3                                                                
035600 IMS-GETNEXT-LARM SECTION.                                                
035700     MOVE 'WLLEVA13' TO SSA1                                              
035800     MOVE '  GE' TO GODK-STATUSKODER                                      
035900     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA SSA1                     
036000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
036100     PERFORM IMS-STATUSKONTROLL                                           
036200     .                                                                    
036300     EJECT                                                                
036400 IMS-GETNEXT-ADR SECTION.                                                 
036500     MOVE 'WLLEVA14' TO SSA1                                              
036600     MOVE '  GE' TO GODK-STATUSKODER                                      
036700     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA SSA1                     
036800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
036900     PERFORM IMS-STATUSKONTROLL                                           
037000     SKIP3                                                                
037100     .                                                                    
037200     EJECT                                                                
037300 IMS-STATUSKONTROLL SECTION.                                              
037400     SET STATUS-IX TO 1                                                   
037500     SEARCH GODK-STATUS                                                   
037600       AT END CALL FELLOG                                                 
037700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
037800     END-SEARCH                                                           
037900     .                                                                    
