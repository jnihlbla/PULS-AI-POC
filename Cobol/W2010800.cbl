000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2010800.                                                
000300*AUTHOR.         P-O WOLLHAG - MULTIDATA.                                 
000400*DATE-WRITTEN.   DEC-78.                                                  
000500*    FUNKTION.   TP-PROGRAM FÖR FRÅGA PÅ INLEVERANSHISTORIKREGISTR        
000600*    INDATA.                                                              
000700*        TRANSAKTION: W2T108                                              
000800*        MID:         W2I10801                                            
000900*    UTDATA.                                                              
001000*        MOD:         W2O10801                                            
001100*    SUBPROGRAM:                                                          
001200*        FELLOG                                                           
001300*                                                                         
001400*   ÄNDRINGAR:                                                            
001500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
001700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
001800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
001900*                                                                         
002000*                                                                         
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM               PIC X(8)    VALUE 'W2010800'.                    
003000 77      JA              PIC X(1)    VALUE 'J'.                           
003100 77      NEJ             PIC X(1)    VALUE 'N'.                           
003200 77      IDARTNR-WS      PIC X(9)    VALUE ZERO.                          
003300                                                                          
003400 77      BEH-SW          PIC X(1)    VALUE 'N'.                           
003500   88      POSTEN-SKALL-BEHANDLAS    VALUE 'J'.                           
003600                                                                          
003700 77  SECURITY-SW         PIC X       VALUE 'N'.                           
003800     88  PASSED-SECURITY-CHECK       VALUE 'J'.                           
003900     88  BLOCKED-SECURITY-CHECK      VALUE 'N'.                           
004000                                                                          
004100 77      MAX-LINE-ANT    PIC S9(9)   VALUE +13   COMP SYNC.               
004200 77      RADIX-C1        PIC S9(9)   VALUE ZERO  COMP-3.                  
004300 77      RADIX-C2        PIC S9(9)   VALUE ZERO  COMP-3.                  
004400 77      SPALT-IX        PIC S9(9)   VALUE ZERO  COMP-3.                  
004500 77      WS-IDLEVNR-8    PIC X(8)    VALUE SPACE.                         
004600     SKIP3                                                                
004700                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005200                                                                          
005300 01      NYCKLAR-TILL-DLI.                                                
005400   03    W-IDARTNR-X.                                                     
005500     05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
005600   03    W-DAINLEV-X.                                                     
005700     05  W-DAINLEV       PIC 9(16)   VALUE ZERO.                          
005800                                                                          
005900 01      SPAR-IDDC       PIC X(2)    VALUE SPACE.                         
006000                                                                          
006100 01      MEDDELANDE.                                                      
006200   03    FEL-1           PIC X(26)   VALUE 'ARTIKELNUMMER EJ NUMER        
006300-    'ISKT'.                                                              
006400   03    FEL-2           PIC X(37)   VALUE 'ARTIKELN FINNS EJ I HI        
006500-    'STORIKREGISTRET'.                                                   
006600   03    FEL-3           PIC X(20)   VALUE 'OBEHÖRIG ANVÄNDARE  '.        
006700                                                                          
006800   03    MEDDELANDE-1    PIC X(46)   VALUE 'FLER TRANSAKTIONER FIN        
006900-                                      'NS PÅ AKTUELLT ARTIKELNR'.        
007000   03    FEL-4           PIC X(40) VALUE 'ARTIKEL EJ REGISTRERAD'.        
007100                                                                          
007200     EJECT                                                                
007300*01    -COPY WWLEV04                                                      
007400     EJECT                                                                
007500*      --- VALID IDDC CODES                                               
007600*                                                                         
007700*01    -COPY WWDC99                                                       
007800     EJECT                                                                
007900******************************************************************        
008000*                                                                         
008100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008200*                                                                         
008300 01      FILLER          PIC X(16)   VALUE 'MFS-WS          '.            
008400     SKIP3                                                                
008500*    -COPY W2I10801 -PRE MID-.                                            
008600     EJECT                                                                
008700*    -COPY WMSGAREA                                                       
008800     EJECT                                                                
008900*    03 W2O10801 -COPY W2O10801 -PRE MOD- -RED MSG-AREA.                  
009000     EJECT                                                                
009100*    -COPY WMFSAREA                                                       
009200     EJECT                                                                
009300*                    ****    PARAMETRAR TILL W005INIT                     
009400*    -COPY WMSGINIT                                                       
009500     EJECT                                                                
009600******************************************************************        
009700*             ARBETS-AREOR TILL IMS-SEKTIONERNA                           
009800*                                                                         
009900 01      IMS-WS.                                                          
010000   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
010100     SKIP3                                                                
010200*****                    **** STATUS-KOD FRÅN IMS                         
010300   03    STATUS-WS       PIC XX.                                          
010400         88  SEGMENT-FINNS       VALUE '  '.                              
010500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
010600     SKIP3                                                                
010700   03    GODK-STATUSKODER.                                                
010800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01      SSA1            PIC X(64).                                       
011100 01      SSA2            PIC X(64).                                       
011200 01      SSA3            PIC X(64).                                       
011300     EJECT                                                                
011400*                            IMS FUNKTIONSKODER                           
011500*01      -COPY W0003                                                      
011600     EJECT                                                                
011700*                            DLI INPUT-OUTPUT AREA                        
011800 01  FILLER                      PIC X(24) VALUE 'DLI-IO-AREA'.           
011900 01  DLI-IO-AREA.                                                         
012000   03  IO-AREA                   PIC X(150)  VALUE SPACE.                 
012100     SKIP3                                                                
012200*  03  WLINLE01 -COPY WDL201              -RED IO-AREA.                   
012300     EJECT                                                                
012400*  03  WLINLE11 -COPY WDL211              -RED IO-AREA.                   
012500     EJECT                                                                
012600*  03  WLINLE21 -COPY WDL221              -RED IO-AREA.                   
012700     EJECT                                                                
012800*  03  WLINLE22 -COPY WDL222              -RED IO-AREA.                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(24) VALUE 'DLI-IO-WDK601'.         
013100 01  DLI-IO-WDK601.                                                       
013200*  03    -COPY WDK601.                                                    
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009     -PRE MSG-                                            
013600     EJECT                                                                
013700*    -COPY W0008     -PRE USEA-                                           
013800         05  FILLER           PIC X.                                      
013900     EJECT                                                                
014000*    -COPY W0008     -PRE WLINLE-.                                        
014100         05  WLINLE-KEY-FB-IDARTNR   PIC S9(9)  COMP-3.                   
014200         05  WLINLE-KEY-FB-DAINLEV   PIC 9(16).                           
014300     EJECT                                                                
014400*    -COPY W0008     -PRE WDK6-                                           
014500         05  FILLER           PIC X.                                      
014600     EJECT                                                                
014700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
014800                                  WLINLE-PCB WDK6-PCB.                    
014900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
015000                                   WLINLE-PCB WDK6-PCB.                   
015100                                                                          
015200     PERFORM IMS-GET-MSG                                                  
015300     IF SEGMENT-FINNS                                                     
015400       PERFORM A-INIT-SPARA-INPUT                                         
015500                                                                          
015600       IF IDARTNR-WS NOT NUMERIC                                          
015700         MOVE FEL-1 TO MOD-MESSAGE-RAD1                                   
015800       ELSE                                                               
015900         MOVE IDARTNR-WS TO W-IDARTNR                                     
016000         PERFORM SEC-URITY                                                
016100                                                                          
016200         IF PASSED-SECURITY-CHECK                                         
016300           MOVE MID-IDINLEV TO W-DAINLEV                                  
016400           IF MID-IDINLEV = 0                                             
016500             PERFORM S1-LAES-FRAN-BORJAN                                  
016600           ELSE                                                           
016700             IF MID-IDINLEV (1:1) = 0                                     
016800               MOVE 8       TO W-DAINLEV (1:1)                            
016900             ELSE                                                         
017000               MOVE 7       TO W-DAINLEV (1:1)                            
017100             END-IF                                                       
017200             PERFORM S2-LAES-FRAN-AKT-POSITION                            
017300           END-IF                                                         
017400           IF SEGMENT-SAKNAS                                              
017500             MOVE FEL-2 TO MOD-MESSAGE-RAD1                               
017600           ELSE                                                           
017700             PERFORM UNTIL                                                
017800              NOT ( SEGMENT-FINNS AND MOD-IDINLEV = ZERO )                
017900               PERFORM B-REDIGERA-RAD                                     
018000               PERFORM S3-LAES-NASTA-INLEV                                
018100             END-PERFORM                                                  
018200             IF MOD-IDINLEV > ZERO                                        
018300               MOVE MEDDELANDE-1 TO MOD-MESSAGE-RAD23                     
018400             END-IF                                                       
018500           END-IF                                                         
018600         ELSE                                                             
018700           IF MOD-MESSAGE-RAD1 = FEL-4                                    
018800*            --- ARTIKEL SAKNAS PÅ WDK6                                   
018900             CONTINUE                                                     
019000           ELSE                                                           
019100*            --- OBEHÖRIG ANVÄNDARE                                       
019200             MOVE FEL-3 TO MOD-MESSAGE-RAD1                               
019300           END-IF                                                         
019400         END-IF                                                           
019500       END-IF                                                             
019600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10801 + 4                      
019700       PERFORM IMS-ISRT-MSG                                               
019800     END-IF                                                               
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     CONTINUE.                                                            
020200     EJECT                                                                
020300 A-INIT-SPARA-INPUT SECTION.                                              
020400     IF MSG-DUBBLA-TRANSKODER                                             
020500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10801                 
020600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
020700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020800       MOVE +0 TO MID-IDINLEV                                             
020900     ELSE                                                                 
021000       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I10801                   
021100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
021200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021300     END-IF                                                               
021400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
021500     MOVE '001'             TO MSGI-KDCALL                                
021600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021800     MOVE '2108'            TO MSGI-IDTRANS                               
021900     IF MFS-IDTRANS = '2108'                                              
022000     OR (MID-IDARTNR-IN NUMERIC                                           
022100     AND MID-IDARTNR-IN > ZERO)                                           
022200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
022300     END-IF                                                               
022400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022500     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
022600     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
022700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022800       MOVE +0          TO MID-IDINLEV                                    
022900     END-IF                                                               
023000     IF MID-IDINLEV NOT NUMERIC                                           
023100       MOVE +0          TO MID-IDINLEV                                    
023200     END-IF                                                               
023300     IF MFS-IDTRANS NOT = '2108'                                          
023400       MOVE +0          TO MID-IDINLEV                                    
023500     END-IF                                                               
023600     MOVE SPACE      TO MSG-AREA                                          
023700     MOVE 'W2O10801' TO MFS-IDMOD                                         
023800     MOVE '2108'     TO MOD-IDTRANS                                       
023900     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
024000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
024100     MOVE ZERO TO MOD-IDINLEV                                             
024200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN MOD-MESSAGE-RAD1              
024300                             MOD-MESSAGE-RAD23                            
024400     MOVE +1 TO RADIX-C1                                                  
024500     MOVE +1 TO RADIX-C2                                                  
024600     CONTINUE.                                                            
024700     EJECT                                                                
024800 B-REDIGERA-RAD SECTION.                                                  
024900     MOVE JA TO BEH-SW                                                    
025000     IF MOT-IDPTYP = 'R31' OR '310' OR 'R32' OR 'R34' OR 'R30'            
025100       IF MOT-IDPTYP = 'R34'                                              
025200         IF DIR-KDRT = +8 OR +77                                          
025300           MOVE NEJ TO BEH-SW                                             
025400         ELSE                                                             
025500           MOVE DIR-IDDC         TO SPAR-IDDC                             
025600         END-IF                                                           
025700       ELSE                                                               
025800         IF MOT-KDRT = +8 OR +77                                          
025900           MOVE NEJ TO BEH-SW                                             
026000         ELSE                                                             
026100           MOVE MOT-IDDC         TO SPAR-IDDC                             
026200         END-IF                                                           
026300       END-IF                                                             
026400     ELSE                                                                 
026500       IF MOT-IDPTYP = 'R40'                                              
026600         MOVE NEJ TO BEH-SW                                               
026700       ELSE                                                               
026800         MOVE DIR-IDDC         TO SPAR-IDDC                               
026900       END-IF                                                             
027000     END-IF                                                               
027100     IF POSTEN-SKALL-BEHANDLAS                                            
027200       MOVE SPAR-IDDC TO WS-IDDC                                          
027300       IF CDC-SE                                                          
027400         MOVE +1 TO SPALT-IX                                              
027500         IF MOT-IDPTYP = ('R31' OR '310' OR 'R30') AND                    
027600                                      MOT-KVANTMOT > 0                    
027700           IF RADIX-C1 < MAX-LINE-ANT                                     
027800             PERFORM BA-REDIGERA-C1-RAD                                   
027900             PERFORM BB-REDIGERA-C1-P32                                   
028000           ELSE                                                           
028100             MOVE WLINLE-KEY-FB-DAINLEV (2:15) TO MOD-IDINLEV             
028200           END-IF                                                         
028300         ELSE                                                             
028400           IF RADIX-C1 < MAX-LINE-ANT + 1                                 
028500             PERFORM BA-REDIGERA-C1-RAD                                   
028600           ELSE                                                           
028700             MOVE WLINLE-KEY-FB-DAINLEV (2:15) TO MOD-IDINLEV             
028800           END-IF                                                         
028900         END-IF                                                           
029000       ELSE                                                               
029100        IF CDC-TR                                                         
029200         MOVE +2 TO SPALT-IX                                              
029300         IF MOT-IDPTYP = ('R31' OR '310' OR 'R30') AND                    
029400                                      MOT-KVANTMOT > 0                    
029500           IF RADIX-C2 < MAX-LINE-ANT                                     
029600             PERFORM BC-REDIGERA-C2-RAD                                   
029700             PERFORM BD-REDIGERA-C2-P32                                   
029800           ELSE                                                           
029900             MOVE WLINLE-KEY-FB-DAINLEV (2:15) TO MOD-IDINLEV             
030000           END-IF                                                         
030100         ELSE                                                             
030200           IF RADIX-C2 < MAX-LINE-ANT + 1                                 
030300             PERFORM BC-REDIGERA-C2-RAD                                   
030400           ELSE                                                           
030500             MOVE WLINLE-KEY-FB-DAINLEV (2:15) TO MOD-IDINLEV             
030600           END-IF                                                         
030700         END-IF                                                           
030800        END-IF                                                            
030900       END-IF                                                             
031000     END-IF                                                               
031100     CONTINUE.                                                            
031200     EJECT                                                                
031300 BA-REDIGERA-C1-RAD SECTION.                                              
031400     IF MOT-IDPTYP = 'R33' OR 'R34'                                       
031500       MOVE DIR-TIAVSDAT     TO MOD-TIAVSDAT                              
031600                                 (RADIX-C1, SPALT-IX)                     
031700       MOVE DIR-IDAVINR      TO MOD-IDAVINR                               
031800                                 (RADIX-C1, SPALT-IX)                     
031900       MOVE DIR-KVAVIS       TO MOD-KVAVIS                                
032000                                 (RADIX-C1, SPALT-IX)                     
032100       MOVE DIR-IDLEVNR      TO MOD-IDLEVNR                               
032200                                 (RADIX-C1, SPALT-IX)                     
032300       MOVE DIR-IDPTYP       TO MOD-IDPTYP                                
032400                                 (RADIX-C1, SPALT-IX)                     
032500       MOVE DIR-KDRT         TO MOD-KDRT                                  
032600                                 (RADIX-C1, SPALT-IX)                     
032800       MOVE DIR-IDLEVNR      TO LEV04-IDLEVNR                             
032810       IF LEV04-REFNR                                                     
032820         MOVE DIR-IDSUPREF (3:8)                                          
032821                             TO MOD-IDAVINR                               
032822                                 (RADIX-C1, SPALT-IX)                     
032830       END-IF                                                             
032831*                                                                         
032840       ADD +1 TO RADIX-C1                                                 
032900     ELSE                                                                 
033000       IF MOT-IDPTYP = 'R31' OR '310' OR 'R32' OR 'R30'                   
033100         MOVE MOT-TIAVIDAT TO MOD-TIAVSDAT                                
033200                                 (RADIX-C1, SPALT-IX)                     
033300         IF MOT-IDFS > SPACE                                              
033400            MOVE MOT-IDFS    TO MOD-IDFS                                  
033500                                 (RADIX-C1, SPALT-IX)                     
033600         ELSE                                                             
033700            MOVE MOT-IDAVINR TO MOD-IDAVINR                               
033800                                 (RADIX-C1, SPALT-IX)                     
033900         END-IF                                                           
034000         MOVE MOT-IDLEVNR TO MOD-IDLEVNR                                  
034100                                 (RADIX-C1, SPALT-IX)                     
034200         MOVE MOT-IDPTYP       TO MOD-IDPTYP                              
034300                                 (RADIX-C1, SPALT-IX)                     
034400         MOVE MOT-KDRT         TO MOD-KDRT                                
034500                                 (RADIX-C1, SPALT-IX)                     
034600         IF MOT-IDPTYP = 'R32'                                            
034700           MOVE MOT-KVANTMOT TO MOD-KVAVIS                                
034800                                 (RADIX-C1, SPALT-IX)                     
034900         ELSE                                                             
035000           MOVE MOT-KVAVIS TO MOD-KVAVIS                                  
035100                                 (RADIX-C1, SPALT-IX)                     
035200         END-IF                                                           
035300         ADD +1 TO RADIX-C1                                               
035400       END-IF                                                             
035500     END-IF                                                               
035600     CONTINUE.                                                            
035700     EJECT                                                                
035800 BB-REDIGERA-C1-P32 SECTION.                                              
035900                                                                          
036000     MOVE 'P32'           TO MOD-IDPTYP                                   
036100                             (RADIX-C1, SPALT-IX)                         
036200     MOVE MOT-KVANTMOT    TO MOD-KVAVIS                                   
036300                             (RADIX-C1, SPALT-IX)                         
036400     ADD +1 TO RADIX-C1                                                   
036500     CONTINUE.                                                            
036600     EJECT                                                                
036700 BC-REDIGERA-C2-RAD SECTION.                                              
036800     IF MOT-IDPTYP = 'R33' OR 'R34'                                       
036900       MOVE DIR-TIAVSDAT     TO MOD-TIAVSDAT                              
037000                                 (RADIX-C2, SPALT-IX)                     
037100       MOVE DIR-IDAVINR      TO MOD-IDAVINR                               
037200                                 (RADIX-C2, SPALT-IX)                     
037300       MOVE DIR-KVAVIS       TO MOD-KVAVIS                                
037400                                 (RADIX-C2, SPALT-IX)                     
037500       MOVE DIR-IDLEVNR      TO MOD-IDLEVNR                               
037600                                 (RADIX-C2, SPALT-IX)                     
037700       MOVE DIR-IDPTYP       TO MOD-IDPTYP                                
037800                                 (RADIX-C2, SPALT-IX)                     
037900       MOVE DIR-KDRT         TO MOD-KDRT                                  
038000                                 (RADIX-C2, SPALT-IX)                     
038020       MOVE DIR-IDLEVNR      TO LEV04-IDLEVNR                             
038030       IF LEV04-REFNR                                                     
038040         MOVE DIR-IDSUPREF (3:8)                                          
038050                             TO MOD-IDAVINR                               
038060                                 (RADIX-C1, SPALT-IX)                     
038070       END-IF                                                             
038080*                                                                         
038100       ADD +1 TO  RADIX-C2                                                
038200     ELSE                                                                 
038300       IF MOT-IDPTYP = 'R31' OR '310' OR 'R32' OR 'R30'                   
038400         MOVE MOT-TIAVIDAT TO MOD-TIAVSDAT                                
038500                                 (RADIX-C2, SPALT-IX)                     
038600         IF MOT-IDFS > SPACE                                              
038700            MOVE MOT-IDFS    TO MOD-IDFS                                  
038800                                 (RADIX-C2, SPALT-IX)                     
038900         ELSE                                                             
039000            MOVE MOT-IDAVINR TO MOD-IDAVINR                               
039100                                 (RADIX-C2, SPALT-IX)                     
039200         END-IF                                                           
039300         MOVE MOT-IDLEVNR TO MOD-IDLEVNR                                  
039400                                 (RADIX-C2, SPALT-IX)                     
039500         MOVE MOT-IDPTYP       TO MOD-IDPTYP                              
039600                                 (RADIX-C2, SPALT-IX)                     
039700         MOVE MOT-KDRT         TO MOD-KDRT                                
039800                                 (RADIX-C2, SPALT-IX)                     
039900         IF MOT-IDPTYP = 'R32'                                            
040000           MOVE MOT-KVANTMOT TO MOD-KVAVIS                                
040100                                 (RADIX-C2, SPALT-IX)                     
040200         ELSE                                                             
040300           MOVE MOT-KVAVIS TO MOD-KVAVIS                                  
040400                                 (RADIX-C2, SPALT-IX)                     
040500         END-IF                                                           
040600         ADD +1 TO  RADIX-C2                                              
040700       END-IF                                                             
040800     END-IF                                                               
040900     CONTINUE.                                                            
041000     EJECT                                                                
041100 BD-REDIGERA-C2-P32 SECTION.                                              
041200                                                                          
041300     MOVE 'P32'           TO MOD-IDPTYP                                   
041400                             (RADIX-C2, SPALT-IX)                         
041500     MOVE MOT-KVANTMOT    TO MOD-KVAVIS                                   
041600                             (RADIX-C2, SPALT-IX)                         
041700     ADD +1 TO  RADIX-C2                                                  
041800     CONTINUE.                                                            
041900     EJECT                                                                
042000 S1-LAES-FRAN-BORJAN    SECTION.                                          
042100                                                                          
042200     PERFORM IMS-GU-ART-INL-OKVAL                                         
042300     IF SEGMENT-FINNS                                                     
042400       PERFORM IMS-GNP-OKVAL                                              
042500     END-IF                                                               
042600     CONTINUE.                                                            
042700     EJECT                                                                
042800 S2-LAES-FRAN-AKT-POSITION  SECTION.                                      
042900                                                                          
043000     PERFORM IMS-GU-ART-INL-KVAL                                          
043100     IF SEGMENT-FINNS                                                     
043200       PERFORM IMS-GNP-OKVAL                                              
043300     END-IF                                                               
043400     CONTINUE.                                                            
043500     EJECT                                                                
043600 S3-LAES-NASTA-INLEV    SECTION.                                          
043700                                                                          
043800     PERFORM IMS-GNP-INL-OKVAL                                            
043900     IF SEGMENT-FINNS                                                     
044000       PERFORM IMS-GNP-OKVAL                                              
044100     END-IF                                                               
044200     CONTINUE.                                                            
044300     EJECT                                                                
044400                                                                          
044500 SEC-URITY SECTION.                                                       
044600     SKIP2                                                                
044700*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
044800     PERFORM IMS-GU-K601                                                  
044900     IF  SEGMENT-FINNS                                                    
045000       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
045100       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
045200       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
045300*        --- BEHÖRIG USER                                                 
045400         SET PASSED-SECURITY-CHECK TO TRUE                                
045500       ELSE                                                               
045600*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
045700         CONTINUE                                                         
045800       END-IF                                                             
045900     ELSE                                                                 
046000       MOVE FEL-4  TO MOD-MESSAGE-RAD1                                    
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400*                                                                         
046500*-----------------------------------------------------------              
046600*    I M S - SEKTIONER                                                    
046700*-----------------------------------------------------------              
046800*                                                                         
046900 IMS-GET-MSG SECTION.                                                     
047000     MOVE '  QC' TO GODK-STATUSKODER                                      
047100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
047200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     CONTINUE.                                                            
047500     SKIP3                                                                
047600 IMS-ISRT-MSG SECTION.                                                    
047700                                                                          
047800     IF MSGI-IDLAND-SPR = 'GB'                                            
047900        MOVE 'N' TO MFS-KDHUVOMR                                          
048000     END-IF                                                               
048100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
048200     MOVE SPACE TO GODK-STATUSKODER                                       
048300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
048400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     CONTINUE.                                                            
048700     EJECT                                                                
048800 IMS-GU-ART-INL-OKVAL SECTION.                                            
048900                                                                          
049000     STRING 'WLINLE01*P(IDARTNR  =' W-IDARTNR-X ')'                       
049100             DELIMITED BY SIZE INTO SSA1                                  
049200     MOVE   'WLINLE11'   TO         SSA2                                  
049300     MOVE '  GE' TO GODK-STATUSKODER                                      
049400     CALL CBLTDLI USING GU WLINLE-PCB DLI-IO-AREA SSA1 SSA2               
049500     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
049600     PERFORM IMS-STATUSKONTROLL                                           
049700     CONTINUE.                                                            
049800     SKIP3                                                                
049900 IMS-GNP-OKVAL            SECTION.                                        
050000                                                                          
050100     MOVE '  ' TO GODK-STATUSKODER                                        
050200     CALL CBLTDLI USING GNP WLINLE-PCB DLI-IO-AREA                        
050300     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     CONTINUE.                                                            
050600     EJECT                                                                
050700 IMS-GU-ART-INL-KVAL     SECTION.                                         
050800                                                                          
050900     STRING 'WLINLE01*P(IDARTNR  =' W-IDARTNR-X ')'                       
051000             DELIMITED BY SIZE INTO SSA1                                  
051100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
051200             DELIMITED BY SIZE INTO SSA2                                  
051300     MOVE '  ' TO GODK-STATUSKODER                                        
051400     CALL CBLTDLI USING GU WLINLE-PCB DLI-IO-AREA SSA1 SSA2               
051500     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
051600     PERFORM IMS-STATUSKONTROLL                                           
051700     CONTINUE.                                                            
051800     EJECT                                                                
051900 IMS-GNP-INL-OKVAL SECTION.                                               
052000                                                                          
052100     MOVE 'WLINLE11'     TO         SSA1                                  
052200     MOVE '  GE' TO GODK-STATUSKODER                                      
052300     CALL CBLTDLI USING GNP WLINLE-PCB DLI-IO-AREA SSA1                   
052400     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     CONTINUE.                                                            
052700     SKIP3                                                                
052800 IMS-GU-K601 SECTION.                                                     
052900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
053000            DELIMITED BY SIZE INTO SSA1                                   
053100     MOVE '  GE' TO GODK-STATUSKODER                                      
053200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
053300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     EJECT                                                                
053700 IMS-STATUSKONTROLL SECTION.                                              
053800     SET STATUS-IX TO 1                                                   
053900     SEARCH GODK-STATUS                                                   
054000       AT END                                                             
054100         CALL FELLOG                                                      
054200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
054300     END-SEARCH                                                           
054400     .                                                                    
