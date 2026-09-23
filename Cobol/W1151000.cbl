000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1151000.                                    
000300 AUTHOR.                     JANNE MELANDER                               
000400 DATE-WRITTEN.               JULI 1988.                                   
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900*    BMP MED CHECK-POINT (150 UPPDAT) C.E. 93-05-07                       
001000*                                                                         
001100*    LÄSER HÄNDELSE-TRANS SKAPAD AV ON-LINE PGM W10113                    
001200*    HÄNDELSETRANSEN = WLXXAW11(1115-ROT, 1116-BARNET)"WDGX1116"          
001300*    PROGRAMMET RIVER ELLER LÄGGER UPP ARTIKLAR I BASLAGER, BEROEN        
001400*    PÅ HÄNDELSETRANSENS ERSÄTTNINGS-KOD, SAMT HUR DE ERSÄTTANDE          
001500*    ARTIKLARNA FÖRHÅLLER SIG TILL TILL DEN ERSATTA ARTIKELN.             
001600*                                                                         
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400 WORKING-STORAGE SECTION.                                                 
002500     SKIP2                                                                
002600*    -COPY WY2000W1                                                       
002700     SKIP3                                                                
002800 77  PROGRAM-NAMN            PIC X(8) VALUE 'W1151000'.                   
002900*- - - - - - - - - - - - - - - - - KONSTANTER.                            
003000 77  JA                      PIC X       VALUE 'J'.                       
003100 77  NEJ                     PIC X       VALUE 'N'.                       
003200*- - - - - - - - - - - - - - - - -CHECK-POINT.                            
003300 77  CHKP-ID                 PIC X(8)    VALUE 'W1151000'.                
003400 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
003500 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
003600 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
003700 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
003800 77  CHKP-RAK                PIC S9(9)   VALUE +0 COMP SYNC.              
003900 77  CHKP-MAX                PIC S9(3)   VALUE +350 COMP SYNC.            
004000*                                                                         
004100 77  TEXT-SW                 PIC X       VALUE 'J'.                       
004200 77  SAMMA-PROJ-SW           PIC X       VALUE 'N'.                       
004300 77  SPAR-IDPROJ             PIC X(4)    VALUE SPACE.                     
004400 77  WS-DAGDAT               PIC  9(7)   VALUE ZERO.                      
004500 77  WS-RIVN-DAT             PIC  9(7)   VALUE ZERO.                      
004600 77  WS-1124-TIPROJSTO       PIC S9(7)   VALUE ZERO COMP-3.               
004700 77  WS-BORT-ANTAL-KVBASL    PIC S9(7)   COMP-3 VALUE ZERO.               
004800* - - - --                                                                
004900 77  RKOD-ABEND              PIC S9(4)   VALUE +16  COMP SYNC.            
005000 77  ANTAL-RAKNARE           PIC S9(9)   VALUE +0   COMP SYNC.            
005100 77  MARKET-DLET             PIC X(12)   VALUE 'MARKET-DLET='.            
005200 77  SW-MARKET-DLET          PIC X       VALUE '0'.                       
005300     88 NO-MARKETS-DELETED   VALUE '0'.                                   
005400     88 MARKET-DELETED       VALUE '1'.                                   
005500     88 ALL-MARKETS-DELETED  VALUE '2'.                                   
005600                                                                          
005700*01  -COPY WWPRODSL                                                       
005800                                                                          
005900*      --- VALID IDDC CODES                                               
006000*                                                                         
006100*01    -COPY WWDCKONS                                                     
006200*01    -COPY WWDC99                                                       
006300       EJECT                                                              
006400*- - - - - - - - - - - - - - - - - DYNAMISKA-SUBPROGRAM.                  
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600     03 WORKDAY             PIC X(8)    VALUE 'WORKDAY '.                 
006700     03 CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                  
006800     03 FELLOG              PIC X(8)    VALUE 'FELLOG'.                   
006900     03 ABEND               PIC X(8)    VALUE 'ABEND'.                    
007000     EJECT                                                                
007100*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
007200 01  IMS-WS.                                                              
007300     03  FILLER              PIC X(8)   VALUE 'IMS-WS  '.                 
007400*                                                                         
007500*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
007600     03 W-IDARTNR-X.                                                      
007700        05 W-IDARTNR         PIC S9(9)  COMP-3 VALUE ZERO.                
007800                                                                          
007900     03 W-KDBASLM-X.                                                      
008000        05 W-KDBASLM         PIC X(6)          VALUE SPACE.               
008100                                                                          
008200     03 W-1115-KEY-X.                                                     
008300        05 FILLER            PIC  X(4)         VALUE '1115'.              
008400        05 FILLER            PIC X(26)         VALUE LOW-VALUE.           
008500                                                                          
008600     03 W-1123-KEY-X.                                                     
008700        05 FILLER            PIC  X(4)         VALUE '1123'.              
008800        05 W-1123-KDPRODSL   PIC  S9(3)        COMP-3 VALUE ZERO.         
008900        05 W-1123-IDPROJ     PIC  X(4)         VALUE SPACE.               
009000        05 FILLER            PIC  X(20)        VALUE LOW-VALUE.           
009100                                                                          
009200     03 W-1126-KEY-X.                                                     
009300        05 W-1126-KDBASLM    PIC  X(6)        VALUE SPACE.                
009400        05 FILLER            PIC  X(9)        VALUE LOW-VALUE.            
009500                                                                          
009600*- - - - - - - - - - - - - - - - - VARIABLER TILL W009VADD                
009700 01   W009VADD-DATUM         PIC S9(5) COMP-3.                            
009800 01   W009VADD-ANTAL         PIC S9(5) COMP-3.                            
009900                                                                          
010000     EJECT                                                                
010100 01  FILLER               PIC X(16)   VALUE  'WORKDAY '.                  
010200*01      -COPY WORKAREA                                                   
010300     EJECT                                                                
010400*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
010500   03    STATUS-WS      PIC XX.                                           
010600     88  SEGMENT-FINNS             VALUE '  '.                            
010700     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
010800     88  BASEN-SLUT                VALUE 'GB'.                            
010900     88  IMS-EJ-OK                 VALUE 'XD'.                            
011000*                                                                         
011100*                                                                         
011200   03    SSA1           PIC X(64).                                        
011300   03    SSA2           PIC X(64).                                        
011400*                                                                         
011500   03    GODK-STATUSKODER.                                                
011600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     EJECT                                                                
011800*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
011900*01      -COPY W0003.                                                     
012000     EJECT                                                                
012100*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
012200 01  DLI-IO-AREA.                                                         
012300     03 IO-AREA             PIC X(32).                                    
012400*    03 AREA     -COPY WDGX1116 -PRE XXAW-   -RED IO-AREA.                
012500     EJECT                                                                
012600 01  DLI-IO-AREA2.                                                        
012700     03 IO-AREA2            PIC X(900).                                   
012800*    03 AREA     -COPY WDD201   -PRE ARTG-   -RED IO-AREA2.               
012900     EJECT                                                                
013000*    03 AREA     -COPY WDD211   -PRE ARTG11- -RED IO-AREA2.               
013100     EJECT                                                                
013200*    03 WLARTC01 -COPY WDK601                -RED IO-AREA2.               
013300     EJECT                                                                
013400*    03 WLARTC11 -COPY WDK611                -RED IO-AREA2.               
013500     EJECT                                                                
013600 01  DLI-IO-AREA3.                                                        
013700     03 IO-AREA3            PIC X(32).                                    
013800*    03 WLERSA11 -COPY WDD702 -PRE TILLK-  -RED IO-AREA3.                 
013900     EJECT                                                                
014000 01  DLI-IO-AREA4.                                                        
014100     03 IO-AREA4            PIC X(96).                                    
014200*    03 WLXXAP11 -COPY WDGX1124              -RED IO-AREA4.               
014300     EJECT                                                                
014400*    03 WLXXAP12 -COPY WDGX1126              -RED IO-AREA4.               
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700     SKIP2                                                                
014800*    -COPY W0009 -PRE MSG-.                                               
014900     EJECT                                                                
015000*    -COPY W0008 -PRE XXAW-.                                              
015100          05  FILLER         PIC X.                                       
015200     EJECT                                                                
015300*    -COPY W0008 -PRE ARTG-.                                              
015400          05  FILLER         PIC X.                                       
015500     EJECT                                                                
015600*    -COPY W0008 -PRE XXAP-.                                              
015700          05  FILLER         PIC X.                                       
015800     EJECT                                                                
015900*    -COPY W0008 -PRE ERSA-.                                              
016000          05  FILLER         PIC X.                                       
016100     EJECT                                                                
016200*    -COPY W0008 -PRE ARTC-.                                              
016300          05  FILLER         PIC X.                                       
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB XXAW-PCB ARTG-PCB                      
016600                                   XXAP-PCB ERSA-PCB ARTC-PCB.            
016700     ENTRY 'DLITCBL' USING MSG-PCB XXAW-PCB ARTG-PCB                      
016800                                   XXAP-PCB ERSA-PCB ARTC-PCB.            
016900     PERFORM A-INIT                                                       
017000     MOVE +1 TO CHKP-RAK                                                  
017100     MOVE +0 TO ANTAL-RAKNARE                                             
017200     PERFORM IMS-GET-WLXXAW11-GHNP                                        
017300     PERFORM UNTIL SEGMENT-SAKNAS                                         
017400       IF XXAW-1116-KDERS-NEW > ZERO                                      
017500         MOVE XXAW-1116-TIREGDAT   TO TMP1-YYMMDD                         
017600         MOVE WS-RIVN-DAT          TO TMP2-YYMMDD                         
017700         PERFORM WY2000P1                                                 
017800         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
017900           MOVE XXAW-1116-IDARTNR TO W-IDARTNR                            
018000           IF XXAW-1116-KDERS-NEW = 09 OR 52                              
018100***                               19 OCH 29 FÖREKOMMER EJ PÅ XXAW         
018200             PERFORM B-KONTROLL-PROJSTO-TIBASL-RIV                        
018300           ELSE                                                           
018400             PERFORM IMS-GHU-ARTG01                                       
018500             MOVE     ARTG-ART-IDPROJ   TO SPAR-IDPROJ                    
018600             PERFORM C-LAES-ALLA-TILLKOMMANDE-ART                         
018700*****  FIX START ***********                                              
018800             IF SEGMENT-FINNS                                             
018900               IF TEXT-SW = JA                                            
019000                 PERFORM B-KONTROLL-PROJSTO-TIBASL-RIV                    
019100               ELSE                                                       
019200                 IF SAMMA-PROJ-SW = JA                                    
019300                   PERFORM B-KONTROLL-PROJSTO-TIBASL-RIV                  
019400                 END-IF                                                   
019500               END-IF                                                     
019600             END-IF                                                       
019700*****  FIX STOPP ***********                                              
019800           END-IF                                                         
019900           PERFORM IMS-DLET-WLXXAW11                                      
020000           ADD +1 TO CHKP-RAK                                             
020100           ADD +1 TO ANTAL-RAKNARE                                        
020200         ELSE                                                             
020300           CONTINUE                                                       
020400***        TRANSEN SKALL LIGGA PÅ H-REGISTRET I 5 DAGAR INNAN             
020500***        RIVNINGEN GÖRS.                                                
020600         END-IF                                                           
020700       ELSE                                                               
020800         MOVE XXAW-1116-IDARTNR TO W-IDARTNR                              
020900         PERFORM A-KONTROLL-KDERS-ZERO                                    
021000         PERFORM IMS-DLET-WLXXAW11                                        
021100         ADD +1 TO CHKP-RAK                                               
021200         ADD +1 TO ANTAL-RAKNARE                                          
021300       END-IF                                                             
021400       IF CHKP-RAK > CHKP-MAX                                             
021500         PERFORM IMS-CHECKPOINT                                           
021600         MOVE +1 TO CHKP-RAK                                              
021700         PERFORM IMS-GU-XXAW01                                            
021800       END-IF                                                             
021900                                                                          
022000       PERFORM IMS-GET-WLXXAW11-GHNP                                      
022100     END-PERFORM                                                          
022200     DISPLAY ANTAL-RAKNARE ' POSTER HAR BEHANDLATS'                       
022300                                                                          
022400     PERFORM Z-FINIT                                                      
022500     MOVE ZERO               TO RETURN-CODE                               
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000                                                                          
023100*****************************************************                     
023200*   HÄMTAR IN DAGENS DATUM SAMT                                           
023300*   LÄSER ROTEN PÅ HÄNDELSEBASEN                                          
023400*****************************************************                     
023500                                                                          
023600     ACCEPT WS-DAGDAT FROM DATE                                           
023700     PERFORM IMS-RESTART                                                  
023800     PERFORM IMS-GU-XXAW01                                                
023900                                                                          
024000*    SKALL SUBTRAHERA 5 ARB DAGAR FRÅN DAGENSDATUM                        
024100*    FÖR ATT MAN SKALL KUNNA SE VILKA ERSÄTTNINGAR                        
024200*    SOM SKALL BEHANDLAS.                                                 
024300*    OBSERVERA:  GÄLLER INTE VID RIVNING AV ERSÄTTNING                    
024400                                                                          
024500     MOVE 003                TO WORK-KDCALL                               
024600     MOVE WC-CDC-SE          TO WORK-IDDC                                 
024700     MOVE 5                  TO WORK-KVWORKD                              
024800     MOVE WS-DAGDAT          TO WORK-TIAAMMDD-TOM                         
024900*                                                                         
025000     CALL WORKDAY USING      WORK-KDCALL                                  
025100                             WORK-DATE-AREA                               
025200                             WORK-KDSVAR                                  
025300*                                                                         
025400     IF WORK-KDSVAR-OK                                                    
025500        MOVE WORK-TIAAMMDD-FOM TO WS-RIVN-DAT                             
025600     ELSE                                                                 
025700        CALL ABEND USING RKOD-ABEND                                       
025800        DISPLAY 'WORK-KDSVAR ÄR INTE OKEY  '                              
025900     END-IF                                                               
026000     EJECT                                                                
026100     .                                                                    
026200     EJECT                                                                
026300 A-KONTROLL-KDERS-ZERO        SECTION.                                    
026400*****************************************************                     
026500* SÄTTER ARTG01-TIBASL = +1 (BASLAGER MARKNADS KNYTTID)                   
026600* OM ARTG01-TIBASL = +1 ARTIKELN LÄGGS UPP PÅ BASLAGERBEREDARKÖ           
026700*****************************************************                     
026800*                                                                         
026900     PERFORM IMS-GU-ARTC01                                                
027000     IF SEGMENT-FINNS                                                     
027100       MOVE ART-KDPRODSL TO TEST-KDPRODSL                                 
027200       IF KDPRODSL-VOLVO-UTAN-EMB                                         
027300         PERFORM IMS-GNP-F-ARTC11                                         
027400         IF ( CLAG-FLLSRDEL = 'J' )                                       
027500           AND ( CLAG-KDUART = SPACE )                                    
027600             PERFORM IMS-GHU-ARTG01                                       
027700             IF ARTG-ART-DABASL = ZERO                                    
027800               MOVE +1           TO ARTG-ART-DABASL                       
027900               PERFORM IMS-REPL-ARTG01                                    
028000               ADD +1 TO CHKP-RAK                                         
028100             END-IF                                                       
028200         END-IF                                                           
028300       END-IF                                                             
028400     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 B-KONTROLL-PROJSTO-TIBASL-RIV SECTION.                                   
028800                                                                          
028900***************************************************************           
029000* ÄR 1124-TIPROJSTO(GEN.PROJ-STOPP) < DAGENS-DATUM  GÖRS INGET            
029100* FÖR DÅ FIXAR ORDERSYSTEMET DET SOM SKALL GÖRAS.                         
029200* LIKASÅ KOLLAS ATT..                                                     
029300* 1126-TIPROJSTO(MARKN.PROJ-STOPP) < DAGENS-DATUM PÅ ALLA                 
029400* MARKNADSSEGMENT I PROJEKTBASEN ANNARS RIVS ARTIKEL                      
029500*****************************************************************         
029600                                                                          
029700     MOVE XXAW-1116-IDARTNR TO W-IDARTNR                                  
029800     PERFORM IMS-GHU-ARTG01                                               
029900     IF SEGMENT-FINNS                                                     
030000*****  FIX IF                                                             
030100     MOVE ARTG-ART-IDPROJ   TO W-1123-IDPROJ                              
030200     MOVE ZERO              TO W-1123-KDPRODSL                            
030300     MOVE ARTG-ART-KDPRODSL TO TEST-KDPRODSL                              
030400     PERFORM IMS-GU-ARTC01                                                
030500     IF SEGMENT-FINNS                                                     
030600        MOVE ART-KDPRODSL TO TEST-KDPRODSL                                
030700     END-IF                                                               
030800     IF KDPRODSL-VOLVO-UTAN-EMB                                           
030900           MOVE +11       TO W-1123-KDPRODSL                              
031000     END-IF                                                               
031100     PERFORM IMS-GET-PROJ-ROT                                             
031200**   -                                                                    
031300     IF SEGMENT-FINNS                                                     
031400       PERFORM IMS-GNP-PROJ-1124                                          
031500       MOVE 1124-TIPROJSTO TO WS-1124-TIPROJSTO                           
031600                                                                          
031700       PERFORM IMS-GHU-ARTG01                                             
031800       IF ARTG-ART-DABASL = +1                                            
031900         DISPLAY 'ART EJ MARKNADS-KNUTEN ÄN, '                            
032000                 'TA NER FRÅN BASL-BER-KÖ'                                
032100         MOVE ZERO  TO  ARTG-ART-DABASL                                   
032200         PERFORM IMS-REPL-ARTG01                                          
032300         ADD +1 TO CHKP-RAK                                               
032400       ELSE                                                               
032500         IF ARTG-ART-DABASL > +1                                          
032600***                                   ARTIKELN ÄR MARKNADS-KNUTEN         
032700           PERFORM BA-BEHANDLA-MARKNADSSEGMENT                            
032800           IF NO-MARKETS-DELETED                                          
032900             DISPLAY 'INGET MARKNADS-SEGMENT BORTTAGET OCH'               
033000                     ' XXAW-KDERS-NEW= ' XXAW-1116-KDERS-NEW              
033100           ELSE                                                           
033200             IF ALL-MARKETS-DELETED                                       
033300               MOVE WS-1124-TIPROJSTO   TO TMP1-YYMMDD                    
033400               MOVE WS-DAGDAT           TO TMP2-YYMMDD                    
033500               PERFORM WY2000P1                                           
033600               IF TMP1-YYMMDD > TMP2-YYMMDD                               
033700               OR XXAW-1116-KDERS-NEW =  52                               
033800                 DISPLAY 'ALLA MARKNADS-SEGM BORTTAGNA. '                 
033900                         'NOLLAR TIBASL, STOMREG, KVBASL'                 
034000                 PERFORM IMS-GHU-ARTG01                                   
034100                 MOVE ZERO   TO ARTG-ART-DABASL                           
034200                                ARTG-ART-TISTOMREG                        
034300                                ARTG-ART-KVBASL                           
034400                 PERFORM IMS-REPL-ARTG01                                  
034500                 ADD +1 TO CHKP-RAK                                       
034600               ELSE                                                       
034700                 DISPLAY 'DETTA BORDE INTE INTRÄFFA !!'                   
034800                 DISPLAY 'INGET KOMMER ATT GÖRAS,     '                   
034900                 DISPLAY 'MEN ALLA MARKNADER ÄR BORTA OCH'                
035000                      ' 1124-TIPROJSTO = ' WS-1124-TIPROJSTO              
035100                 DISPLAY '. XXAW-KDERS-NEW= ' XXAW-1116-KDERS-NEW         
035200               END-IF                                                     
035300             ELSE                                                         
035400***                   DET FINNS MARKNAD(ER) KVAR PÅ NYPON(WDD211)         
035500***                                    ENDAST JUSTERING AV KVABSL         
035600               PERFORM IMS-GHU-ARTG01                                     
035700               DISPLAY 'MARKNADER FINNS KVAR. JUSTERAR KVBASL'            
035800               SUBTRACT WS-BORT-ANTAL-KVBASL FROM ARTG-ART-KVBASL         
035900               PERFORM IMS-REPL-ARTG01                                    
036000               ADD +1 TO CHKP-RAK                                         
036100             END-IF                                                       
036200           END-IF                                                         
036300         END-IF                                                           
036400       END-IF                                                             
036500     ELSE                                                                 
036600       PERFORM IMS-GHU-ARTG01                                             
036700       IF ARTG-ART-DABASL = +1                                            
036800         MOVE ZERO TO ARTG-ART-DABASL                                     
036900         DISPLAY 'PROJ SAKNAS. TAS NER FRÅN BASL-BER-KÖ'                  
037000         PERFORM IMS-REPL-ARTG01                                          
037100         ADD +1 TO CHKP-RAK                                               
037200       ELSE                                                               
037300         IF ARTG-ART-DABASL > +1                                          
037400           MOVE ZERO TO ARTG-ART-DABASL                                   
037500                        ARTG-ART-TISTOMREG                                
037600                        ARTG-ART-KVBASL                                   
037700           DISPLAY 'PROJ SAKNAS. ART MARKNADSKNUTEN'                      
037800           DISPLAY '** TAS NER FRÅN MARKNADSKÖN OCH'                      
037900           DISPLAY '** NOLL TILL KVBASL OCH TISTOMREG.'                   
038000           DISPLAY '** ALLA MARKNADS-SEGM BORT PÅ WDD2'                   
038100           PERFORM IMS-REPL-ARTG01                                        
038200           ADD +1 TO CHKP-RAK                                             
038300           PERFORM IMS-GHNP-F-ARTG11                                      
038400           PERFORM UNTIL SEGMENT-SAKNAS                                   
038500              PERFORM IMS-DLET-ARTG11                                     
038600              ADD +1 TO CHKP-RAK                                          
038700              PERFORM IMS-GHNP-ARTG11-NEXT                                
038800           END-PERFORM                                                    
038900         END-IF                                                           
039000       END-IF                                                             
039100     END-IF                                                               
039200     END-IF                                                               
039300**************** FIX END-IF                                               
039400     .                                                                    
039500     EJECT                                                                
039600 BA-BEHANDLA-MARKNADSSEGMENT   SECTION.                                   
039700                                                                          
039800     MOVE ZERO TO WS-BORT-ANTAL-KVBASL                                    
039900     SET  NO-MARKETS-DELETED  TO TRUE                                     
040000                                                                          
040100     PERFORM IMS-GHNP-F-ARTG11                                            
040200     PERFORM UNTIL NOT SEGMENT-FINNS                                      
040300       MOVE ARTG11-ART-KDBASLM TO W-1126-KDBASLM                          
040400                                  W-KDBASLM                               
040500       PERFORM IMS-GNP-PROJ-1126-KVAL                                     
040600       IF SEGMENT-FINNS                                                   
040700         IF XXAW-1116-KDERS-NEW =  52                                     
040800           DISPLAY '52-MÄRKT !    DELETE AV ' W-KDBASLM                   
040900           PERFORM IMS-DLET-ARTG11                                        
041000           ADD +1 TO CHKP-RAK                                             
041100           SET  MARKET-DELETED  TO TRUE                                   
041200         ELSE                                                             
041300           MOVE 1126-TIPROJSTO   TO TMP1-YYMMDD                           
041400           MOVE WS-DAGDAT        TO TMP2-YYMMDD                           
041500           PERFORM WY2000P1                                               
041600           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
041700             ADD ARTG11-ART-KVBASLM TO WS-BORT-ANTAL-KVBASL               
041800             DISPLAY '1126-TIPROJSTO > DAGENS-DAT !   '                   
041900                     'DELETE AV ' W-KDBASLM                               
042000             PERFORM IMS-DLET-ARTG11                                      
042100             ADD +1 TO CHKP-RAK                                           
042200             SET  MARKET-DELETED  TO TRUE                                 
042300           ELSE                                                           
042400             IF 1126-TIPROJSTO = ZERO                                     
042500               MOVE WS-1124-TIPROJSTO   TO TMP1-YYMMDD                    
042600               MOVE WS-DAGDAT           TO TMP2-YYMMDD                    
042700               PERFORM WY2000P1                                           
042800               IF TMP1-YYMMDD > TMP2-YYMMDD                               
042900                 ADD ARTG11-ART-KVBASLM TO WS-BORT-ANTAL-KVBASL           
043000                 DISPLAY '1124-TIPROJSTO > DAGENS-DAT !   '               
043100                       'DELETE AV ' W-KDBASLM                             
043200                 PERFORM IMS-DLET-ARTG11                                  
043300                 ADD +1 TO CHKP-RAK                                       
043400                 SET  MARKET-DELETED  TO TRUE                             
043500               END-IF                                                     
043600             END-IF                                                       
043700           END-IF                                                         
043800         END-IF                                                           
043900       ELSE                                                               
044000         CONTINUE                                                         
044100**               MARKNAD SAKNAS PÅ PROJ-BASEN MEN FINNS PÅ NYPON          
044200**                             SKALL MAN RENSA DÅ ELLER ?  /J.M.          
044300       END-IF                                                             
044400       PERFORM IMS-GHNP-F-ARTG11-KVAL-STORRE                              
044500***                                          NÄSTA NYPON-MARKNAD          
044600     END-PERFORM                                                          
044700     PERFORM IMS-GHNP-F-ARTG11                                            
044800     IF SEGMENT-SAKNAS                                                    
044900       SET  ALL-MARKETS-DELETED  TO TRUE                                  
045000       DISPLAY 'ALLA WDD211-SEGMENT BORTTAGNA'                            
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 C-LAES-ALLA-TILLKOMMANDE-ART SECTION.                                    
045500                                                                          
045600*****************************************************                     
045700* LÄSER ALLA TILLKOMMANDE ARTIKLAR OCH                                    
045800* SER EFTER OM DE ÄR TEXT-ARTIKLAR.                                       
045900* ÄR DET INTE EN TEXT-ARTIKEL TITTAR MAN EFTER IFALL                      
046000* DEN TILLKOMMANDE ARTIKELN LIGGER UNDER SAMMA                            
046100* IDPROJ   SOM DEN ERSATTA ARTIKELN.                                      
046200*****************************************************                     
046300                                                                          
046400     MOVE JA TO TEXT-SW                                                   
046500     MOVE NEJ TO SAMMA-PROJ-SW                                            
046600     PERFORM IMS-GET-ERSATT-ART-WDD701                                    
046700     IF SEGMENT-FINNS                                                     
046800********** FIX IF                                                         
046900     PERFORM IMS-GET-TILLK-ART-WDD7                                       
047000     PERFORM UNTIL SEGMENT-SAKNAS                                         
047100        IF TILLK-FLTEXT = JA                                              
047200           CONTINUE                                                       
047300        ELSE                                                              
047400           MOVE NEJ TO TEXT-SW                                            
047500           MOVE TILLK-IDARTNR-TILLK TO W-IDARTNR                          
047600           PERFORM IMS-GHU-ARTG01                                         
047700           IF SEGMENT-FINNS                                               
047800              IF ARTG-ART-IDPROJ = SPAR-IDPROJ                            
047900                 MOVE JA TO SAMMA-PROJ-SW                                 
048000              END-IF                                                      
048100           END-IF                                                         
048200        END-IF                                                            
048300        PERFORM IMS-GET-TILLK-ART-WDD7                                    
048400     END-PERFORM                                                          
048500     END-IF                                                               
048600*********** ,FIX END-IF                                                   
048700     .                                                                    
048800     EJECT                                                                
048900                                                                          
049000 Z-FINIT SECTION.                                                         
049100     CONTINUE                                                             
049200     .                                                                    
049300     EJECT                                                                
049400******* I M S   S E C T I O N  ***                                        
049500*                                                                         
049600 IMS-RESTART SECTION.                                                     
049700     SKIP2                                                                
049800     MOVE SPACE TO MSG-IO-AREA                                            
049900     MOVE '  ' TO GODK-STATUSKODER                                        
050000     CALL CBLTDLI USING XRST MSG-PCB                                      
050100                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
050200                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     SKIP3                                                                
050600     .                                                                    
050700 IMS-CHECKPOINT SECTION.                                                  
050800     SKIP2                                                                
050900     MOVE CHKP-ID TO MSG-IO-AREA                                          
051000     MOVE '  XD' TO GODK-STATUSKODER                                      
051100     CALL CBLTDLI USING CHKP MSG-PCB                                      
051200                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
051300                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
051400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051500     PERFORM IMS-STATUSKONTROLL                                           
051600     IF IMS-EJ-OK                                                         
051700       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
051800       CALL FELLOG                                                        
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 IMS-GU-XXAW01 SECTION.                                                   
052300*                                                                         
052400     STRING 'WLXXAW01(WDGXKEY  =' W-1115-KEY-X ')'                        
052500         DELIMITED BY SIZE INTO SSA1                                      
052600     MOVE '  '   TO GODK-STATUSKODER                                      
052700     CALL CBLTDLI USING GU XXAW-PCB DLI-IO-AREA SSA1                      
052800     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
052900     PERFORM IMS-STATUSKONTROLL                                           
053000     SKIP2                                                                
053100     .                                                                    
053200     EJECT                                                                
053300 IMS-GET-WLXXAW11-GHNP SECTION.                                           
053400*                                                                         
053500     MOVE 'WLXXAW11 ' TO SSA1                                             
053600     MOVE '  GE'  TO GODK-STATUSKODER                                     
053700     CALL CBLTDLI USING GHNP XXAW-PCB DLI-IO-AREA SSA1                    
053800     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     SKIP2                                                                
054100     .                                                                    
054200     EJECT                                                                
054300 IMS-DLET-WLXXAW11     SECTION.                                           
054400*                                                                         
054500     MOVE '  '  TO GODK-STATUSKODER                                       
054600     CALL CBLTDLI USING DLET XXAW-PCB DLI-IO-AREA                         
054700     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     SKIP2                                                                
055000     .                                                                    
055100     EJECT                                                                
055200 IMS-GHU-ARTG01          SECTION.                                         
055300*                                                                         
055400     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
055500         DELIMITED BY SIZE INTO SSA1                                      
055600     MOVE '  GE'     TO GODK-STATUSKODER                                  
055700     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
055800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     SKIP2                                                                
056100     .                                                                    
056200     EJECT                                                                
056300 IMS-REPL-ARTG01         SECTION.                                         
056400*                                                                         
056500     MOVE '  '   TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                        
056700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSKONTROLL                                           
056900     SKIP2                                                                
057000     .                                                                    
057100     EJECT                                                                
057200 IMS-GHNP-F-ARTG11 SECTION.                                               
057300*                                                                         
057400     MOVE 'WLARTG11*F ' TO SSA1                                           
057500     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
057600     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
057700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUSKONTROLL                                           
057900     .                                                                    
058000                                                                          
058100 IMS-GHNP-F-ARTG11-KVAL-STORRE SECTION.                                   
058200*                                                                         
058300     STRING 'WLARTG11*F(KDBASLM  >' W-KDBASLM-X ')'                       
058400         DELIMITED BY SIZE INTO SSA1                                      
058500     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
058600     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
058700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000                                                                          
059100 IMS-GHNP-ARTG11-NEXT SECTION.                                            
059200*                                                                         
059300     MOVE 'WLARTG11 ' TO SSA1                                             
059400     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
059500     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
059600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     SKIP2                                                                
059900     .                                                                    
060000     EJECT                                                                
060100 IMS-DLET-ARTG11 SECTION.                                                 
060200*                                                                         
060300     MOVE '  '   TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA2                        
060500     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     SKIP2                                                                
060800     .                                                                    
060900     EJECT                                                                
061000 IMS-GET-PROJ-ROT SECTION.                                                
061100*                                                                         
061200     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
061300         DELIMITED BY SIZE INTO SSA1                                      
061400     MOVE '  GE'   TO GODK-STATUSKODER                                    
061500     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA2 SSA1                     
061600     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
061700     PERFORM IMS-STATUSKONTROLL                                           
061800     SKIP2                                                                
061900     .                                                                    
062000     EJECT                                                                
062100 IMS-GNP-PROJ-1124 SECTION.                                               
062200*                                                                         
062300     MOVE 'WLXXAP11 ' TO SSA1                                             
062400     MOVE '  '   TO GODK-STATUSKODER                                      
062500     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA4 SSA1                    
062600     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900                                                                          
063000 IMS-GNP-PROJ-1126-KVAL SECTION.                                          
063100*                                                                         
063200     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
063300         DELIMITED BY SIZE INTO SSA1                                      
063400     MOVE '  GE'   TO GODK-STATUSKODER                                    
063500     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA4 SSA1                    
063600     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-GET-ERSATT-ART-WDD701 SECTION.                                       
064100*                                                                         
064200     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
064300         DELIMITED BY SIZE INTO SSA1                                      
064400     MOVE '  GE' TO GODK-STATUSKODER                                      
064500     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA3 SSA1                     
064600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
064700     PERFORM IMS-STATUSKONTROLL                                           
064800     SKIP2                                                                
064900     .                                                                    
065000     EJECT                                                                
065100 IMS-GET-TILLK-ART-WDD7  SECTION.                                         
065200*                                                                         
065300     MOVE 'WLERSA11 ' TO SSA1                                             
065400     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
065500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA3 SSA1                    
065600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     SKIP2                                                                
065900     .                                                                    
066000     EJECT                                                                
066100 IMS-GU-ARTC01 SECTION.                                                   
066200*                                                                         
066300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
066400         DELIMITED BY SIZE INTO SSA1                                      
066500     MOVE '  GE'   TO GODK-STATUSKODER                                    
066600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
066700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     SKIP2                                                                
067000     .                                                                    
067100     EJECT                                                                
067200 IMS-GNP-F-ARTC11          SECTION.                                       
067300*                                                                         
067400     MOVE 'WLARTC11*F' TO SSA1                                            
067500     MOVE '  '   TO GODK-STATUSKODER                                      
067600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
067700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067800     PERFORM IMS-STATUSKONTROLL                                           
067900     SKIP2                                                                
068000     .                                                                    
068100     EJECT                                                                
068200 IMS-STATUSKONTROLL SECTION.                                              
068300*                                                                         
068400     SET STATUS-IX TO 1                                                   
068500     SEARCH GODK-STATUS AT END CALL FELLOG                                
068600         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
068700         CONTINUE                                                         
068800     END-SEARCH                                                           
068900     .                                                                    
069000     EJECT                                                                
069100*    -COPY WY2000P1                                                       
