000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                W9010200.                                     
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 17:30:49.                         
000800*AUTHOR.                    URBAN ZACKRISSON                              
000900*DATE-COMPILED.                                                           
001000*DATE-WRITTEN.              OKT  -84.                                     
001100*REMARKS.                                                                 
001200*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
001300*                ERSÄTTNINGAR. ANGIVEN ARTIKEL 'ERSÄTTES AV'              
001400*    INDATA.                                                              
001500*        TRANSAKTION: W9T102                                              
001600*        MID:         W9I10201                                            
001700*    UTDATA.                                                              
001800*        MOD:         W9O10201                                            
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400*    -COPY WY2000W9                                                       
002500     SKIP3                                                                
002600 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
002700 77      JA              PIC X       VALUE 'J'.                           
002800 77      NEJ             PIC X       VALUE 'N'.                           
002900 77      SW-VISAS        PIC X       VALUE 'J'.                           
003000 77      MAX-MOD-LENGD   PIC S9(4)   VALUE +707  COMP SYNC.               
003100 77      MAX-ANT-BILD-RADER                                               
003200                         PIC S9(9)   VALUE +27   COMP SYNC.               
003300 77      INDX            PIC S9(9)   VALUE +0    COMP SYNC.               
003400 77      W-KDERS         PIC S9(3)   VALUE +0    COMP-3.                  
003500 77      DAGENS-VECKA    PIC S9(4)   VALUE +0    COMP-3.                  
003600 77      VECKOR-TILL-PUBLISERING                                          
003700                         PIC 9(6)    VALUE ZERO.                          
003800 77      INLEVERANS-VECKA                                                 
003900                         PIC S9(4)   VALUE +0    COMP-3.                  
004000     SKIP3                                                                
004100 01      NYCKLAR-TILL-DLI.                                                
004200   03      W-IDARTNR-X.                                                   
004300     05      W-IDARTNR   PIC S9(9)   VALUE ZERO  COMP-3.                  
004400   03      W-IDKORTNR-X.                                                  
004500     05      W-IDKORTNR  PIC S9(3)   VALUE ZERO  COMP-3.                  
004600     EJECT                                                                
004700 01      W-TIFINLV       PIC S9(5).                                       
004800                                                                          
004900 01      FILLER  REDEFINES W-TIFINLV.                                     
005000   03    W-TIFINLV-AAR   PIC 9(2).                                        
005100   03    W-TIFINLV-VECKA PIC 9(2).                                        
005200   03    W-TIFINLV-DAG   PIC 9(1).                                        
005300     SKIP3                                                                
005400 01      DYNAMISKA-SUBPROGRAM.                                            
005500   03      FILLER        PIC X(16)   VALUE 'SUBPROGRAM     '.             
005600   03      WDATKONV      PIC X(8)    VALUE 'WDATKONV'.                    
005700   03      FELLOG        PIC X(8)    VALUE 'FELLOG'.                      
005800   03      CBLTDLI       PIC X(8)    VALUE 'CBLTDLI'.                     
005900   03      W005INIT      PIC X(8)    VALUE 'W005INIT'.                    
006000     SKIP3                                                                
006100     EJECT                                                                
006200 01      MEDDELANDEN.                                                     
006300                                                                          
006400   03    W-FEL-1.                                                         
006500     05    FILLER        PIC X(26)   VALUE                                
006600                             'ARTIKELNUMMER EJ NUMERISKT'.                
006700     05    FILLER        PIC X(26)   VALUE                                
006800                             'PARTNUMBER NOT NUMERIC    '.                
006900   03    FILLER REDEFINES W-FEL-1.                                        
007000     05    FEL-1         PIC X(26) OCCURS 2.                              
007100                                                                          
007200   03    W-FEL-2.                                                         
007300     05    FILLER        PIC X(27)   VALUE                                
007400                             'ARTIKELN ERSÄTTES EJ       '.               
007500     05    FILLER        PIC X(27)   VALUE                                
007600                             'THIS PART IS NOT SUPERSEDED'.               
007700   03    FILLER REDEFINES W-FEL-2.                                        
007800     05    FEL-2         PIC X(27) OCCURS 2.                              
007900                                                                          
008000   03    W-FEL-4.                                                         
008100     05    FILLER        PIC X(19)   VALUE                                
008200                             'ARTIKELN HAR UTGÅTT'.                       
008300     05    FILLER        PIC X(19)   VALUE                                
008400                             'NOT STORED ANYMORE '.                       
008500   03    FILLER REDEFINES W-FEL-4.                                        
008600     05    FEL-4         PIC X(19) OCCURS 2.                              
008700                                                                          
008800   03    W-MED-1.                                                         
008900     05    FILLER        PIC X(34)   VALUE                                
009000                             'MER INFORMATION FINNS, TRYCK ENTER'.        
009100     05    FILLER        PIC X(34)   VALUE                                
009200                             'FOR MORE INFORMATION, PRESS ENTER '.        
009300   03    FILLER REDEFINES W-MED-1.                                        
009400     05    MED-1         PIC X(34) OCCURS 2.                              
009500                                                                          
009600     EJECT                                                                
009700 01  FILLER              PIC X(16)   VALUE '  WDATAREA      '.            
009800*01  WDATAREA  -COPY WDATAREA                                             
009900     EJECT                                                                
010000*                        ****  PARAMETRAR TILL W005INIT                   
010100 01  FILLER              PIC X(16)   VALUE '  WMSGINIT      '.            
010200*01  -COPY WMSGINIT                                                       
010300     EJECT                                                                
010400******************************************************************        
010500*                                                                         
010600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
010700*                                                                         
010800 01      FILLER          PIC X(16)   VALUE 'MFS-WS'.                      
010900     SKIP3                                                                
011000*01      MID -COPY W9I10201 -PRE MID-.                                    
011100     SKIP3                                                                
011200*01      -COPY WMSGAREA                                                   
011300     SKIP3                                                                
011400*  03    MOD -COPY W9O10201 -PRE MOD- -RED MSG-AREA.                      
011500     EJECT                                                                
011600*01  -COPY WMFSAREA.                                                      
011700     EJECT                                                                
011800******************************************************************        
011900*                                                                         
012000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200 01  IMS-WS.                                                              
012300   03    FILLER          PIC X(16)   VALUE 'IMS-WS    '.                  
012400     SKIP3                                                                
012500*****                    **** STATUS-KOD FRÅN IMS                         
012600   03    STATUS-WS       PIC XX.                                          
012700         88  SEGMENT-FINNS       VALUE '  '.                              
012800         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
012900     SKIP3                                                                
013000   03    GODK-STATUSKODER.                                                
013100     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01      SSA1            PIC X(32).                                       
013400 01      SSA2            PIC X(32).                                       
013500     EJECT                                                                
013600*                            IMS FUNKTIONSKODER                           
013700*01      -COPY W0003                                                      
013800     EJECT                                                                
013900*                            DLI INPUT-OUTPUT AREA                        
014000 01      DLI-IO-AREA.                                                     
014100   03    IO-AREA         PIC X(900)  VALUE SPACE.                         
014200     SKIP3                                                                
014300*03      WLARTC01 -COPY WDK601               -RED IO-AREA.                
014400     EJECT                                                                
014500*03      WLARTC11 -COPY WDK611               -RED IO-AREA.                
014600     EJECT                                                                
014700*03      WLERSA01 -COPY WDD701 -PRE ERSATT-  -RED IO-AREA.                
014800     EJECT                                                                
014900*03      WLERSA11 -COPY WDD702 -PRE TILLK-   -RED IO-AREA.                
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200*01  -COPY W0009     -PRE MSG-                                            
015300     SKIP3                                                                
015400*01  -COPY W0008     -PRE USEA-                                           
015500         05  FILLER           PIC X.                                      
015600     SKIP3                                                                
015700*01  -COPY W0008     -PRE ARTC-                                           
015800         05  FILLER           PIC X.                                      
015900     SKIP3                                                                
016000*01  -COPY W0008     -PRE ERSA-                                           
016100         05  FILLER           PIC X.                                      
016200     EJECT                                                                
016300 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
016400                                   ARTC-PCB ERSA-PCB.                     
016500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
016600                                   ARTC-PCB ERSA-PCB.                     
016700                                                                          
016800     PERFORM IMS-GET-MSG                                                  
016900                                                                          
017000     IF SEGMENT-FINNS                                                     
017100       PERFORM A-SPARA-NYCKLAR-OCH-INIT                                   
017200                                                                          
017300       IF IDARTNR-WS NOT NUMERIC                                          
017400         MOVE FEL-1 (INDX) TO MOD-MESSAGE-RAD1                            
017500       ELSE                                                               
017600         MOVE IDARTNR-WS TO W-IDARTNR                                     
017700         PERFORM IMS-GET-ERSATT-ROT                                       
017800                                                                          
017900         IF SEGMENT-SAKNAS                                                
018000           PERFORM B-ERSATTES-EJ                                          
018100         ELSE                                                             
018200           MOVE ERSATT-DIERS-ERS TO MOD-DIERS-ERS                         
018300           PERFORM IMS-GET-ARTIKEL-ROT                                    
018400                                                                          
018500           IF SEGMENT-FINNS                                               
018600             PERFORM C-LAS-KOLLA-VILLKOR                                  
018700                                                                          
018800             IF SW-VISAS = NEJ                                            
018900               PERFORM B-ERSATTES-EJ                                      
019000             ELSE                                                         
019100               MOVE MID-IDKORTNR-ANT TO W-IDKORTNR                        
019200               PERFORM IMS-GET-TILLK-SEG                                  
019300               SET MOD-IX TO +1                                           
019400                                                                          
019500               PERFORM UNTIL                                              
019600                NOT ( MOD-IX NOT > MAX-ANT-BILD-RADER + 1 )               
019700                 IF SEGMENT-FINNS                                         
019800                                                                          
019900                   IF MOD-IX = MAX-ANT-BILD-RADER + 1                     
020000                     MOVE MED-1 (INDX) TO MOD-MESSAGE-RAD23               
020100                     MOVE 'GE' TO ERSA-STATUS-CODE                        
020200                     MOVE TILLK-IDKORTNR TO MOD-IDKORTNR-ANT              
020300                   ELSE                                                   
020400                     PERFORM D-FLYTTA-TILL-MODRAD                         
020500                     PERFORM IMS-GET-TILLK-SEG                            
020600                   END-IF                                                 
020700                 ELSE                                                     
020800                   EVALUATE TRUE                                          
020900                   WHEN MOD-IX NOT = MAX-ANT-BILD-RADER + 1               
021000                     MOVE MFS-RENSA-FAELT TO MOD-RAD (MOD-IX)             
021100                   END-EVALUATE                                           
021200                 END-IF                                                   
021300                 SET MOD-IX UP BY +1                                      
021400                                                                          
021500               END-PERFORM                                                
021600             END-IF                                                       
021700           END-IF                                                         
021800         END-IF                                                           
021900       END-IF                                                             
022000       PERFORM IMS-INSERT-MSG                                             
022100                                                                          
022200     END-IF                                                               
022300     MOVE ZERO TO RETURN-CODE                                             
022400                                                                          
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-SPARA-NYCKLAR-OCH-INIT SECTION.                                        
022900                                                                          
023000     IF MSG-DUBBLA-TRANSKODER                                             
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I10201                 
023200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023400       MOVE ZERO TO MID-IDKORTNR-ANT                                      
023500     ELSE                                                                 
023600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I10201                  
023700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023900     END-IF                                                               
024000                                                                          
024100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
024200     MOVE '001'             TO MSGI-KDCALL                                
024300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024400     MOVE '9102'            TO MSGI-IDTRANS                               
024500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024600     IF MFS-IDTRANS = '9102'                                              
024700       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
024800       IF MID-IDARTNR-IN = ALL '+' OR SPACE                               
024900         CONTINUE                                                         
025000       ELSE                                                               
025100         MOVE ZERO TO MID-IDKORTNR-ANT                                    
025200       END-IF                                                             
025300     ELSE                                                                 
025400       IF MID-IDARTNR-IN NUMERIC                                          
025500       AND MID-IDARTNR-IN > ZERO                                          
025600           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
025700       END-IF                                                             
025800     END-IF                                                               
025900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026000     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
026100     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400     MOVE 'W9O10201' TO MFS-IDMOD                                         
026500     MOVE '9102' TO MOD-IDTRANS                                           
026600     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
026700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
026800     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
026900                                                                          
027000     IF SWEDISH-TEXT                                                      
027100       MOVE +1 TO INDX                                                    
027200     ELSE                                                                 
027300       MOVE +2 TO INDX                                                    
027400     END-IF                                                               
027500     IF (MID-IDKORTNR-ANT NOT NUMERIC)                                    
027600     OR (MFS-IDTRANS NOT = '9102')                                        
027700       MOVE ZERO TO MID-IDKORTNR-ANT                                      
027800     END-IF                                                               
027900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
028000                      MOD-MESSAGE-RAD1                                    
028100                      MOD-MESSAGE-RAD23                                   
028200                                                                          
028300     MOVE JA TO SW-VISAS                                                  
028400     MOVE ZERO TO W-KDERS                                                 
028500                  DAGENS-VECKA                                            
028600                  VECKOR-TILL-PUBLISERING                                 
028700                  INLEVERANS-VECKA                                        
028800     .                                                                    
028900     EJECT                                                                
029000 B-ERSATTES-EJ SECTION.                                                   
029100                                                                          
029200     MOVE FEL-2 (INDX) TO MOD-MESSAGE-RAD1                                
029300                                                                          
029400     MOVE MFS-RENSA-FAELT TO MOD-KDERS                                    
029500     MOD-TIERSDAT                                                         
029600     MOD-DIERS-ERS                                                        
029700     MOVE SPACE TO MOD-STRECK MOD-REKSIFFR                                
029800     SET MOD-IX TO +1                                                     
029900     PERFORM UNTIL                                                        
030000      ( MOD-IX > MAX-ANT-BILD-RADER )                                     
030100       MOVE MFS-RENSA-FAELT TO MOD-RAD (MOD-IX)                           
030200       SET MOD-IX UP BY +1                                                
030300     END-PERFORM                                                          
030400     .                                                                    
030500     EJECT                                                                
030600 C-LAS-KOLLA-VILLKOR SECTION.                                             
030700                                                                          
030800     MOVE ART-TIFINLV     TO W-TIFINLV                                    
030900     PERFORM CA-DATUM-BERAKNING                                           
031000     IF VECKOR-TILL-PUBLISERING < 2                                       
031100       MOVE NEJ TO SW-VISAS                                               
031200     ELSE                                                                 
031300       MOVE DAT-TIAAMMDD      TO MOD-TIERSDAT                             
031400       MOVE ART-KDERS-UTG     TO MOD-KDERS                                
031500                                 W-KDERS                                  
031600       MOVE '-'               TO MOD-STRECK                               
031700       MOVE ART-REKSIFFR      TO MOD-REKSIFFR                             
031800                                                                          
031900       PERFORM IMS-GET-ARTC11                                             
032000       IF SEGMENT-FINNS                                                   
032100         MOVE CLAG-KDERS TO W-KDERS                                       
032200                            MOD-KDERS                                     
032300         IF CLAG-KDUART = 'M' OR 'S'                                      
032400           MOVE NEJ TO SW-VISAS                                           
032500         ELSE                                                             
032600           IF W-KDERS = +52 OR < 21                                       
032700              MOVE NEJ TO SW-VISAS                                        
032800           ELSE                                                           
032900              IF W-KDERS = +29                                            
033000                 MOVE FEL-4 (INDX) TO MOD-MESSAGE-RAD1                    
033100              END-IF                                                      
033200           END-IF                                                         
033300         END-IF                                                           
033400       END-IF                                                             
033500     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 CA-DATUM-BERAKNING SECTION.                                              
033900                                                                          
034000     MOVE 'IDAG  '  TO DAT-KDDATFORM                                      
034100                                                                          
034200     CALL WDATKONV USING                                                  
034300          DAT-KDDATFORM,                                                  
034400          DAT-I-TIDATUM,                                                  
034500          DAT-O-TIDATUM,                                                  
034600          DAT-KDSVAR                                                      
034700                                                                          
034800     IF DAT-KDSVAR-OK                                                     
034900                                                                          
035000       MOVE DAT-TIAA      TO TMP1-YY                                      
035100       MOVE W-TIFINLV-AAR TO TMP2-YY                                      
035200       PERFORM WY2000P9                                                   
035300                                                                          
035400       MULTIPLY TMP1-YY BY 52 GIVING DAGENS-VECKA                         
035500       ADD DAT-TIVV TO DAGENS-VECKA                                       
035600                                                                          
035700       MULTIPLY TMP2-YY BY 52 GIVING INLEVERANS-VECKA                     
035800       ADD W-TIFINLV-VECKA TO INLEVERANS-VECKA                            
035900                                                                          
036000       SUBTRACT DAGENS-VECKA FROM INLEVERANS-VECKA                        
036100                GIVING VECKOR-TILL-PUBLISERING                            
036200     ELSE                                                                 
036300       CALL FELLOG                                                        
036400     END-IF                                                               
036500     MOVE 'AAVVD '  TO DAT-KDDATFORM                                      
036600     MOVE ART-TIERSDAT TO DAT-I-TIDATUM                                   
036700                                                                          
036800     CALL WDATKONV USING                                                  
036900          DAT-KDDATFORM,                                                  
037000          DAT-I-TIDATUM,                                                  
037100          DAT-O-TIDATUM,                                                  
037200          DAT-KDSVAR                                                      
037300     .                                                                    
037400                                                                          
037500*    IF DAT-KDSVAR-FEL                                                    
037600*        CALL  FELLOG                                                     
037700*    ENDIF                                                                
037800     EJECT                                                                
037900 D-FLYTTA-TILL-MODRAD SECTION.                                            
038000                                                                          
038100     IF TILLK-FLTEXT = NEJ                                                
038200       MOVE TILLK-IDARTNR-TILLK TO MOD-IDARTNR-TILLK (MOD-IX)             
038300       MOVE TILLK-DIERS-TILLK   TO MOD-DIERS-TILLK (MOD-IX)               
038400     ELSE                                                                 
038500       MOVE TILLK-BEERS       TO MOD-RAD (MOD-IX)                         
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900* IMS SEKTIONER                                                           
039000     SKIP3                                                                
039100 IMS-GET-MSG SECTION.                                                     
039200     MOVE '  QC' TO GODK-STATUSKODER                                      
039300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039500     PERFORM IMS-STATUSKONTROLL                                           
039600     .                                                                    
039700     SKIP3                                                                
039800 IMS-INSERT-MSG SECTION.                                                  
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
040900 IMS-GET-ARTIKEL-ROT SECTION.                                             
041000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
041100            DELIMITED BY SIZE INTO SSA1                                   
041200     MOVE '  GE' TO GODK-STATUSKODER                                      
041300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
041400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     .                                                                    
041700     SKIP3                                                                
041800 IMS-GET-ARTC11 SECTION.                                                  
041900     MOVE 'WLARTC11 ' TO SSA1                                             
042000     MOVE '  GE' TO GODK-STATUSKODER                                      
042100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
042200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042300     PERFORM IMS-STATUSKONTROLL                                           
042400     .                                                                    
042500     EJECT                                                                
042600 IMS-GET-ERSATT-ROT SECTION.                                              
042700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
042800            DELIMITED BY SIZE INTO SSA1                                   
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
043100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     SKIP3                                                                
043500 IMS-GET-TILLK-SEG SECTION.                                               
043600     STRING 'WLERSA11(IDKORTNR>=' W-IDKORTNR-X ')'                        
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     MOVE '  GE' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
044000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     EJECT                                                                
044400 IMS-STATUSKONTROLL SECTION.                                              
044500     SET STATUS-IX TO 1                                                   
044600     SEARCH GODK-STATUS AT END CALL FELLOG                                
044700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
044800     END-SEARCH                                                           
044900     .                                                                    
045000     EJECT                                                                
045100*    -COPY WY2000P9                                                       
