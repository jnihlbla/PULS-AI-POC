000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                W9010300.                                     
000800*AUTHOR.                    URBAN ZACKRISSON.                             
000900*DATE-COMPILED.                                                           
001000*DATE-WRITTEN.              OKT  -84.                                     
001200*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
001300*                ERSÄTTNINGAR. ANGIVEN ARTIKEL 'ERSÄTTER'                 
001400*    INDATA.                                                              
001500*        TRANSAKTION: W9T103                                              
001600*        MID:         W9I10301                                            
001700*    UTDATA.                                                              
001800*        MOD:         W9O10301                                            
001900*    SUBPROGRAM.                                                          
002000*        FELLOG                                                           
002100*        WDATKONV                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 WORKING-STORAGE SECTION.                                                 
002710*    -COPY WY2000W9                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(8)   VALUE 'W9010300'.             
003300     SKIP3                                                                
003400 77      IDARTNR-WS              PIC X(9)   VALUE SPACE.                  
003500 77      JA                      PIC X(1)   VALUE 'J'.                    
003600 77      NEJ                     PIC X(1)   VALUE 'N'.                    
003700 77      SW-VISAS                PIC X(1)   VALUE 'J'.                    
003800 77      SW-ERSATTER             PIC X(1)   VALUE 'N'.                    
003900 77      MAX-MOD-LENGD           PIC S9(4)  VALUE +1030 COMP SYNC.        
004000 77      MAX-ANT-BILD-RADER      PIC S9(9)  VALUE +13   COMP SYNC.        
004100 77      INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77      W-KDERS                 PIC S9(3)  VALUE +0    COMP-3.           
004300 77      DAGENS-VECKA            PIC S9(4)  VALUE +0    COMP-3.           
004400 77      VECKOR-TILL-PUBLISERING PIC 9(6)   VALUE ZERO.                   
004500 77      INLEVERANS-VECKA        PIC S9(4)  VALUE +0    COMP-3.           
004600     SKIP3                                                                
004700 01      W-TIFINLV               PIC S9(5).                               
004800 01      FILLER  REDEFINES W-TIFINLV.                                     
004900   03    W-TIFINLV-AAR           PIC 9(2).                                
005000   03    W-TIFINLV-VECKA         PIC 9(2).                                
005100   03    W-TIFINLV-DAG           PIC 9(1).                                
005200     EJECT                                                                
005300 01    NYCKLAR-TILL-DLI.                                                  
005400                                                                          
005500   03    W-IDARTNR-X.                                                     
005600     05    W-IDARTNR             PIC S9(9)  VALUE ZERO  COMP-3.           
005700   03    W-ERSATT-IDARTNR-X.                                              
005800     05    W-ERSATT-IDARTNR      PIC S9(9)  VALUE ZERO  COMP-3.           
006100   03    W-IDARTNR-ERS-X.                                                 
006200     05    W-IDARTNR-ERS         PIC S9(9)  VALUE ZERO  COMP-3.           
006300   03    W-IDKORTNR-X.                                                    
006400     05    W-IDKORTNR            PIC S9(3)  VALUE ZERO  COMP-3.           
006500     SKIP3                                                                
006600 01    DYNAMISKA-SUBPROGRAM.                                              
006700   03    FILLER                  PIC X(16)  VALUE 'SUBPROGRAM'.           
006800   03    WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
006830   03    CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
006840   03    FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
006841   03    W005INIT                PIC X(8)   VALUE 'W005INIT'.             
006850                                                                          
006900     EJECT                                                                
007000 01    MEDDELANDEN.                                                       
007100                                                                          
007200   03    W-FEL-1.                                                         
007300     05    FILLER        PIC X(26)   VALUE                                
007400                             'ARTIKELNUMMER EJ NUMERISKT'.                
007500     05    FILLER        PIC X(26)   VALUE                                
007600                             'PARTNUMBER NOT NUMERIC    '.                
007700   03    FILLER REDEFINES W-FEL-1.                                        
007800     05    FEL-1         PIC X(26) OCCURS 2.                              
007900                                                                          
008000   03    W-FEL-2.                                                         
008100     05    FILLER        PIC X(27)   VALUE                                
008200                             'ARTIKELN ERSÄTTER EJ       '.               
008300     05    FILLER        PIC X(27)   VALUE                                
008400                             'THIS IS NO SUPERSEDING PART'.               
008500   03    FILLER REDEFINES W-FEL-2.                                        
008600     05    FEL-2         PIC X(27) OCCURS 2.                              
008700                                                                          
008800   03    W-MED-1.                                                         
008900     05    FILLER        PIC X(34)   VALUE                                
009000                             'MER INFORMATION FINNS, TRYCK ENTER'.        
009100     05    FILLER        PIC X(34)   VALUE                                
009200                             'FOR MORE INFORMATION, PRESS ENTER '.        
009300   03    FILLER REDEFINES W-MED-1.                                        
009400     05    MED-1         PIC X(34) OCCURS 2.                              
009500                                                                          
009600   03    W-MED-2.                                                         
009700     05    FILLER        PIC X(32)   VALUE                                
009800                             '    EJ ENSAM TILLKOMMANDE      '.           
009900     05    FILLER        PIC X(32)   VALUE                                
010000                             'NOT THE ONLY SUPERSEDING PART NO'.          
010100   03    FILLER REDEFINES W-MED-2.                                        
010200     05    MED-2         PIC X(32) OCCURS 2.                              
010300                                                                          
010400     EJECT                                                                
010500 01      FILLER          PIC X(16)   VALUE 'WDATAREA'.                    
010600*01      RTDATAREA  -COPY WDATAREA.                                       
010700     EJECT                                                                
010701*                         ****  PARAMETRAR TILL W005INIT                  
010710 01      FILLER          PIC X(16)   VALUE 'WMSGINIT'.                    
010730*01      -COPY WMSGINIT                                                   
010800     EJECT                                                                
010900******************************************************************        
011000*                                                                         
011100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011200*                                                                         
011300 01      FILLER          PIC X(16)   VALUE 'MFS-WS'.                      
011400     SKIP3                                                                
011500*01      MID -COPY W9I10301 -PRE MID-.                                    
011700     SKIP3                                                                
011800*01      -COPY WMSGAREA                                                   
012000     SKIP3                                                                
012100*  03    MOD -COPY W9O10301 -PRE MOD- -RED MSG-AREA.                      
012300     EJECT                                                                
012400*01  -COPY WMFSAREA.                                                      
012600     EJECT                                                                
012700******************************************************************        
012800*                                                                         
012900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013000*                                                                         
013100 01  IMS-WS.                                                              
013200   03    FILLER          PIC X(16)   VALUE 'IMS-WS'.                      
013300     SKIP3                                                                
013400*****                    **** STATUS-KOD FRÅN IMS                         
013500   03    STATUS-WS       PIC XX.                                          
013600         88  SEGMENT-FINNS       VALUE '  '.                              
013700         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
013800     SKIP3                                                                
013900   03    GODK-STATUSKODER.                                                
014000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01      SSA1            PIC X(64).                                       
014300 01      SSA2            PIC X(64).                                       
014400     EJECT                                                                
014500*                            IMS FUNKTIONSKODER                           
014600*01      -COPY W0003                                                      
014800     EJECT                                                                
014900*                            DLI INPUT-OUTPUT AREA                        
015000 01  DLI-IO-AREA.                                                         
015100     03  IO-AREA         PIC X(900)  VALUE SPACE.                         
015200     SKIP3                                                                
015900*    03  WLARTC01 -COPY WDK601               -RED IO-AREA.                
016100     EJECT                                                                
016200*    03  WLARTC11 -COPY WDK611               -RED IO-AREA.                
016400     EJECT                                                                
016800     03  FILLER REDEFINES IO-AREA.                                        
016900*        05  WLERSA11 -COPY WDD702 -PRE TILLK-.                           
017100     EJECT                                                                
017200*        05  WLERSA01 -COPY WDD701 -PRE ERSATT-.                          
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600*01  -COPY W0009     -PRE MSG-                                            
017800     SKIP3                                                                
017900*01  -COPY W0008     -PRE USEA-                                           
018100         05  FILLER           PIC X.                                      
018110     SKIP3                                                                
018300*01  -COPY W0008     -PRE ARTC-                                           
018500         05  FILLER           PIC X.                                      
018600     SKIP3                                                                
018700*01  -COPY W0008     -PRE ERSA-                                           
018900         05  FILLER           PIC X.                                      
019000     EJECT                                                                
019100 PROCEDURE DIVISION USING MSG-PCB USEA-PCB ARTC-PCB ERSA-PCB.             
019200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB ERSA-PCB.            
019300                                                                          
019400     PERFORM IMS-GET-MSG                                                  
019500                                                                          
019600     IF SEGMENT-FINNS                                                     
019700       PERFORM A-SPARA-NYCKLAR-OCH-INIT                                   
019800                                                                          
019900       IF IDARTNR-WS NOT NUMERIC                                          
020000         MOVE FEL-1 (INDX) TO MOD-MESSAGE-RAD1                            
020100       ELSE                                                               
020200         MOVE IDARTNR-WS TO W-IDARTNR                                     
020300         PERFORM IMS-GET-ERSATTANDE-ARTIKEL-ROT                           
020400                                                                          
020500         IF SEGMENT-FINNS                                                 
020600           MOVE '-' TO MOD-STRECK                                         
020700           MOVE ART-REKSIFFR TO MOD-REKSIFFR                              
020800         END-IF                                                           
020900         MOVE MID-IDKORTNR-ANT TO W-IDKORTNR                              
021000         MOVE MID-IDARTNR-ERS-ANT TO W-IDARTNR-ERS                        
021100         PERFORM IMS-GET-UNIK-TILLK-SEG                                   
021200                                                                          
021300         IF SEGMENT-SAKNAS                                                
021400           MOVE FEL-2 (INDX) TO MOD-MESSAGE-RAD1                          
021500         ELSE                                                             
021600           SET MOD-IX TO +1                                               
021700                                                                          
021800           PERFORM UNTIL                                                  
021900            NOT ( MOD-IX NOT > MAX-ANT-BILD-RADER + 1 )                   
022000             IF SEGMENT-FINNS                                             
022100                                                                          
022200               IF MOD-IX = MAX-ANT-BILD-RADER + 1                         
022300                 PERFORM B-STOPPA-LASNING                                 
022400                 SET MOD-IX UP BY +1                                      
022500               ELSE                                                       
022600                 PERFORM C-FLYTTA-TILL-MODRAD                             
022700                 PERFORM D-LAS-KOLLA-VILLKOR                              
022800                                                                          
022900                 IF SW-VISAS = JA                                         
023000                   SET MOD-IX UP BY +1                                    
023100                   MOVE JA TO SW-ERSATTER                                 
023200                 END-IF                                                   
023300                 PERFORM IMS-GET-TILLK-SEG                                
023400               END-IF                                                     
023500             ELSE                                                         
023600               IF MOD-IX NOT = MAX-ANT-BILD-RADER + 1                     
023700                 MOVE MFS-RENSA-FAELT TO MOD-RAD (MOD-IX)                 
023800               END-IF                                                     
023900               SET MOD-IX UP BY +1                                        
024000             END-IF                                                       
024100           END-PERFORM                                                    
024200           IF SW-ERSATTER = NEJ                                           
024300             MOVE FEL-2 (INDX) TO MOD-MESSAGE-RAD1                        
024400           END-IF                                                         
024500           IF MOD-IDARTNR (1) = MFS-RENSA-FAELT                           
024600             MOVE SPACE TO MOD-STRECK MOD-REKSIFFR                        
024700           END-IF                                                         
024800         END-IF                                                           
024900       END-IF                                                             
025000     END-IF                                                               
025100     PERFORM IMS-INSERT-MSG                                               
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     CONTINUE.                                                            
025600     EJECT                                                                
025700 A-SPARA-NYCKLAR-OCH-INIT SECTION.                                        
025800                                                                          
025900     IF MSG-DUBBLA-TRANSKODER                                             
026000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I10301                 
026100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026300       MOVE ZERO TO MID-IDKORTNR-ANT                                      
026400       MOVE ZERO TO MID-IDARTNR-ERS-ANT                                   
026500     ELSE                                                                 
026600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I10301                  
026700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026900     END-IF                                                               
026910                                                                          
026920     MOVE ALL '+' TO MSGI-WMSGINIT                                        
026930     MOVE '001'             TO MSGI-KDCALL                                
026940     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026941     MOVE '9103'            TO MSGI-IDTRANS                               
026942     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026950     IF MFS-IDTRANS = '9103'                                              
026951       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
026960       IF MID-IDARTNR-IN = ALL '+' OR SPACE                               
026970         CONTINUE                                                         
026980       ELSE                                                               
026990         MOVE ZERO TO MID-IDKORTNR-ANT                                    
026991         MOVE ZERO TO MID-IDARTNR-ERS-ANT                                 
026992       END-IF                                                             
026993     ELSE                                                                 
026994       MOVE ZERO TO MID-IDKORTNR-ANT                                      
026995       MOVE ZERO TO MID-IDARTNR-ERS-ANT                                   
026996       IF MID-IDARTNR-IN NUMERIC                                          
026998       AND MID-IDARTNR-IN > ZERO                                          
026999           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
027000       END-IF                                                             
027001     END-IF                                                               
027002     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027003     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
027004     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
027005                                                                          
027800     MOVE LOW-VALUE TO MSG-AREA                                           
027900     MOVE 'W9O10301' TO MFS-IDMOD                                         
028000     MOVE '9103' TO MOD-IDTRANS                                           
028100     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
028200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
028300     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
028400                                                                          
028500     IF SWEDISH-TEXT                                                      
028600       MOVE +1 TO INDX                                                    
028700     ELSE                                                                 
028800       MOVE +2 TO INDX                                                    
028900     END-IF                                                               
029000     IF (MID-IDKORTNR-ANT NOT NUMERIC)                                    
029100     OR (MID-IDARTNR-ERS-ANT NOT NUMERIC)                                 
029200     OR (MFS-IDTRANS NOT = '9103')                                        
029300       MOVE ZERO TO MID-IDKORTNR-ANT MID-IDARTNR-ERS-ANT                  
029400     END-IF                                                               
029500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029600                             MOD-MESSAGE-RAD1                             
029700                             MOD-MESSAGE-RAD23                            
029800                                                                          
029900     MOVE JA  TO SW-VISAS                                                 
030000     MOVE NEJ TO SW-ERSATTER                                              
030100     MOVE ZERO TO W-KDERS                                                 
030200                  DAGENS-VECKA                                            
030300                  VECKOR-TILL-PUBLISERING                                 
030400                  INLEVERANS-VECKA                                        
030500     CONTINUE.                                                            
030600     EJECT                                                                
030700 B-STOPPA-LASNING SECTION.                                                
030800                                                                          
030900     MOVE MED-1 (INDX) TO MOD-MESSAGE-RAD23                               
031000     MOVE 'GE' TO ERSA-STATUS-CODE                                        
031100     MOVE TILLK-IDKORTNR TO MOD-IDKORTNR-ANT                              
031200     MOVE ERSATT-IDARTNR TO MOD-IDARTNR-ERS-ANT                           
031300     CONTINUE.                                                            
031400                                                                          
031500     EJECT                                                                
031600 C-FLYTTA-TILL-MODRAD SECTION.                                            
031700                                                                          
031800     MOVE ERSATT-DIERS-ERS     TO MOD-DIERS-ERS (MOD-IX)                  
031900     MOVE ERSATT-IDARTNR       TO MOD-IDARTNR   (MOD-IX)                  
032000                                  W-ERSATT-IDARTNR                        
032100                                                                          
032200     IF ERSATT-KVKORT > 1                                                 
032300       MOVE MED-2 (INDX)     TO MOD-TEXT-KOMMENTAR (MOD-IX)               
032400     ELSE                                                                 
032500       MOVE MFS-RENSA-FAELT  TO MOD-TEXT-KOMMENTAR (MOD-IX)               
032600     END-IF                                                               
032700     MOVE TILLK-DIERS-TILLK    TO MOD-DIERS-TILLK (MOD-IX)                
032800     CONTINUE.                                                            
032900                                                                          
033000     EJECT                                                                
033100 D-LAS-KOLLA-VILLKOR SECTION.                                             
033200                                                                          
033210     MOVE JA TO SW-VISAS                                                  
033300     PERFORM IMS-GET-ARTIKEL-ROT                                          
033400                                                                          
033500     IF SEGMENT-FINNS                                                     
033600       MOVE ART-TIFINLV TO W-TIFINLV                                      
033700       PERFORM DA-DATUM-BERAKNING                                         
033800                                                                          
033900       IF VECKOR-TILL-PUBLISERING < 2                                     
034000         MOVE NEJ TO SW-VISAS                                             
034100       ELSE                                                               
034200         MOVE ART-KDERS-UTG TO MOD-KDERS (MOD-IX)                         
034300                                   W-KDERS                                
034400         PERFORM IMS-GET-ARTC11                                           
034500                                                                          
034600         IF SEGMENT-FINNS                                                 
034700           IF CLAG-KDUART = 'M' OR 'S'                                    
034800             MOVE NEJ TO SW-VISAS                                         
034900           ELSE                                                           
035400             MOVE CLAG-KDERS TO W-KDERS                                   
035500             IF W-KDERS = 29 OR 52 OR < 21                                
035600                MOVE NEJ TO SW-VISAS                                      
035700             ELSE                                                         
035800                MOVE CLAG-KDERS TO MOD-KDERS (MOD-IX)                     
036200                IF CLAG-PRARTSTD     = 0                                  
036300                  MOVE NEJ TO SW-VISAS                                    
036400                END-IF                                                    
036600            END-IF                                                        
036800           END-IF                                                         
036900         END-IF                                                           
037000       END-IF                                                             
037100     END-IF                                                               
037200     CONTINUE.                                                            
037300     EJECT                                                                
037400                                                                          
037500 DA-DATUM-BERAKNING SECTION.                                              
037600                                                                          
037700     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
037800                                                                          
037900     CALL WDATKONV USING DAT-KDDATFORM                                    
038000     DAT-I-TIDATUM                                                        
038100     DAT-O-TIDATUM                                                        
038200     DAT-KDSVAR                                                           
038300                                                                          
038400     IF DAT-KDSVAR-OK                                                     
038410       MOVE DAT-TIAA      TO TMP1-YY                                      
038420       MOVE W-TIFINLV-AAR TO TMP2-YY                                      
038430       PERFORM WY2000P9                                                   
038440                                                                          
038500       MULTIPLY TMP1-YY  BY 52 GIVING DAGENS-VECKA                        
038600       ADD DAT-TIVV TO DAGENS-VECKA                                       
038700                                                                          
038800       MULTIPLY TMP2-YY  BY 52 GIVING INLEVERANS-VECKA                    
038900       ADD W-TIFINLV-VECKA TO INLEVERANS-VECKA                            
039000                                                                          
039100       SUBTRACT DAGENS-VECKA FROM INLEVERANS-VECKA                        
039200                GIVING VECKOR-TILL-PUBLISERING                            
039300     ELSE                                                                 
039400       CALL  FELLOG                                                       
039500     END-IF                                                               
039600     CONTINUE.                                                            
039700     EJECT                                                                
039800* IMS SEKTIONER                                                           
039900     SKIP3                                                                
040000 IMS-GET-MSG SECTION.                                                     
040100     SKIP2                                                                
040200     MOVE '  QC' TO GODK-STATUSKODER                                      
040300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040500     PERFORM IMS-STATUSKONTROLL                                           
040600     CONTINUE.                                                            
040700     SKIP3                                                                
040800 IMS-INSERT-MSG SECTION.                                                  
040900                                                                          
041000     IF ENGLISH-TEXT                                                      
041100       MOVE 'N' TO MFS-KDHUVOMR                                           
041200     END-IF                                                               
041300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041400     MOVE SPACE TO GODK-STATUSKODER                                       
041500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     CONTINUE.                                                            
041900     EJECT                                                                
042000 IMS-GET-ERSATTANDE-ARTIKEL-ROT SECTION.                                  
042100                                                                          
042200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042300            DELIMITED BY SIZE INTO SSA1                                   
042400     MOVE '  GE' TO GODK-STATUSKODER                                      
042500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
042600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042700     PERFORM IMS-STATUSKONTROLL                                           
042800     CONTINUE.                                                            
042900     EJECT                                                                
043000 IMS-GET-ARTIKEL-ROT SECTION.                                             
043100                                                                          
043200     STRING 'WLARTC01(IDARTNR  =' W-ERSATT-IDARTNR-X ')'                  
043300            DELIMITED BY SIZE INTO SSA1                                   
043400     MOVE '  GE' TO GODK-STATUSKODER                                      
043500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
043600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     CONTINUE.                                                            
043900     SKIP3                                                                
044000 IMS-GET-ARTC11 SECTION.                                                  
044100                                                                          
044200     MOVE 'WLARTC11 ' TO SSA1                                             
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
044500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     CONTINUE.                                                            
046000     EJECT                                                                
046100 IMS-GET-UNIK-TILLK-SEG SECTION.                                          
046200                                                                          
046300     STRING 'WLERSA11*D(WDD7ASEQ =' W-IDARTNR-X                           
046400                      '&IDKORTNR=>' W-IDKORTNR-X ')'                      
046500            DELIMITED BY SIZE INTO SSA1                                   
046600     STRING 'WLERSA01(IDARTNR =>' W-IDARTNR-ERS-X ')'                     
046700            DELIMITED BY SIZE INTO SSA2                                   
046800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
046900     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1 SSA2                 
047000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     CONTINUE.                                                            
047300     SKIP3                                                                
047400 IMS-GET-TILLK-SEG SECTION.                                               
047500                                                                          
047600     STRING 'WLERSA11*D(WDD7ASEQ =' W-IDARTNR-X ')'                       
047700            DELIMITED BY SIZE INTO SSA1                                   
047800     MOVE 'WLERSA01 ' TO SSA2                                             
047900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
048000     CALL CBLTDLI USING GN ERSA-PCB DLI-IO-AREA SSA1 SSA2                 
048100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     CONTINUE.                                                            
048400     EJECT                                                                
049600 IMS-STATUSKONTROLL SECTION.                                              
049700     SET STATUS-IX TO 1                                                   
049800     SEARCH GODK-STATUS                                                   
049810       AT END                                                             
049820         CALL FELLOG                                                      
049900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
050000     END-SEARCH                                                           
050100     CONTINUE                                                             
050200            CONTINUE.                                                     
050300 IMS-STATUS-KONTROLL-EXIT. EXIT.                                          
050400     CONTINUE.                                                            
050410     EJECT                                                                
050500*    -COPY WY2000P9                                                       
