000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1150500.                                    
000300 AUTHOR.                     GUNNEL ERIKSSON                              
000400 DATE-WRITTEN.               JUNI 1988.                                   
000500     SKIP2                                                                
000600*    REMARKS.                                               *             
000700*                                                           *             
000800*    FUNKTION.                                              *             
000900*                                                           *             
001000*    LÄSER WDD201 MED SB.                                   *             
001100*    PLOCKAR UT VISSA POSTER FÖR LIST-FIL                   *             
001200*    LÄSER ÄVEN WLBENA, WLARTC SAMT WLXXAP-BASERNA          *             
001300*    DESSA BASER LÄSES MED DL/1                             *             
001400*                                                           *             
001500*                                                           *             
001600*                                                           *             
001700*                                                           *             
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SELECT  KORTFIL    ASSIGN UT-S-W11505D1.                             
002300*            *** INPUT-FILE ***                                           
002400*                                                                         
002500     SELECT  UTFIL      ASSIGN UT-S-W11505D2.                             
002600*            *** OUTPUT-FILE ***                                          
002700*                                                                         
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  KORTFIL                                                              
003300     LABEL RECORDS STANDARD                                               
003400     RECORDING      F                                                     
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700*01  KORTPOST  -COPY W1150501   -L.                                       
003800                                                                          
003900 FD  UTFIL                                                                
004000     LABEL RECORDS STANDARD                                               
004100     RECORDING      F                                                     
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400*01  UTPOST  -COPY W1150601     -L.                                       
004500                                                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900*    -COPY WY2000W1                                                       
005000     SKIP3                                                                
005100 77  PROGRAM-NAMN            PIC X(8) VALUE 'W1150500'.                   
005200*- - - - - - - - - - - - - - - - - KONSTANTER.                            
005300 77  JA                      PIC X       VALUE 'J'.                       
005400 77  NEJ                     PIC X       VALUE 'N'.                       
005500 77  SW-KORTFIL-EOF          PIC X       VALUE 'N'.                       
005600 77  SW-SKRIV                PIC X       VALUE 'J'.                       
005700 77  INDX-A                  PIC S9(9)   VALUE +0 COMP SYNC.              
005800 77  INDX-B                  PIC S9(9)   VALUE +0 COMP SYNC.              
005900                                                                          
006000*- - - - - - - - - - - - - - - - - DATUM-FAELT                            
006100 01  DAGENS-DATUM             PIC 9(6) VALUE ZERO.                        
006200*- - - - - - - - - - - - - - - - - DYNAMISKA-SUB-PGM                      
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
006500     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
006600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006700     EJECT                                                                
006800*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
006900*01  POST              -COPY W1150501   -PRE IN-                          
007000*                                                                         
007100     EJECT                                                                
007200*01  AREA              -COPY W1150601   -PRE UT-                          
007300*                                                                         
007400*01  POST              -COPY W1150601   -PRE SPAR-                        
007500*                                                                         
007600 01  SPAR-KVBLKIT                  PIC S9(3)  COMP-3 VALUE ZERO.          
007700                                                                          
007800 01  SPAR-GRUPP-IDDISTR.                                                  
007900     03  WS-SPAR-IDDISTR OCCURS 6     PIC S9(5) COMP-3.                   
008000*                                                                         
008100 01  SPAR-GRUPP-REBLFORD.                                                 
008200     03  WS-SPAR-REBLFORD  OCCURS 6     PIC S9(3) COMP-3.                 
008300*                                                                         
008400 01  SPAR-GRUPP-KVBASLMD.                                                 
008500     03  WS-SPAR-KVBASLMD  OCCURS 6     PIC S9(7) COMP-3.                 
008600*                                                                         
008700 01  SPAR-GRUPP-PRIS-AKTUELL-ART.                                         
008800     03  WS-SPAR-SUM-AKTUELL-ART    OCCURS 6                              
008900                                   PIC S9(9)V9(2) COMP-3.                 
009000     EJECT                                                                
009100*01            -COPY WWKDDEAL    -PRE GODK-                               
009200                                                                          
009300     EJECT                                                                
009400*01            -COPY WWPRODSL                                             
009500                                                                          
009600     EJECT                                                                
009700*01            -COPY W0005      -PRE POSTSUM-                             
009800                                                                          
009900     EJECT                                                                
010000 01    IMS-WS.                                                            
010100   03  FILLER           PIC X(8)   VALUE 'IMS-WS  '.                      
010200*                                                                         
010300*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
010400   03 W-IDARTNR-X.                                                        
010500      05 W-IDARTNR         PIC S9(9)  COMP-3 VALUE ZERO.                  
010600                                                                          
010700   03 W-IDSKYLT-X.                                                        
010800      05 W-IDSKYLT         PIC  X(3)         VALUE SPACE.                 
010900                                                                          
011000   03 W-KDNOTTYP-X.                                                       
011100      05 W-KDNOTTYP        PIC S9     COMP-3    VALUE +6.                 
011200                                                                          
011300   03 W-1123-KEY-X.                                                       
011400      05 FILLER            PIC  X(4)            VALUE '1123'.             
011500      05 W-1123-KDPRODSL   PIC S9(3)   COMP-3   VALUE ZERO.               
011600      05 W-1123-IDPROJ     PIC X(4)             VALUE SPACE.              
011700      05 FILLER            PIC X(20)            VALUE LOW-VALUE.          
011800                                                                          
011900   03 W-1126-KEY-X.                                                       
012000      05 W-1126-KDBASLM    PIC X(6)             VALUE SPACE.              
012100      05 FILLER            PIC X(09)            VALUE LOW-VALUE.          
012200                                                                          
012300   03 W-IDARTNR-PRIS-X.                                                   
012400      05 W-IDARTNR-PRIS    PIC S9(9)  COMP-3 VALUE ZERO.                  
012500                                                                          
012600   03 W-WDD7A1KY-MIN.                                                     
012700      05 W-IDARTNR-MIN     PIC S9(9)    COMP-3 VALUE ZERO.                
012800      05 FILLER            PIC S9(9)    COMP-3 VALUE ZERO.                
012900      05 FILLER            PIC S9(3)    COMP-3 VALUE ZERO.                
013000                                                                          
013100   03 W-WDD7A1KY-MAX.                                                     
013200      05 W-IDARTNR-MAX     PIC S9(9)    COMP-3 VALUE ZERO.                
013300      05 FILLER     PIC S9(9)    COMP-3 VALUE +999999999.                 
013400      05 FILLER     PIC S9(3)    COMP-3 VALUE +999.                       
013500                                                                          
013600*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
013700   03    STATUS-WS      PIC XX.                                           
013800     88  SEGMENT-FINNS             VALUE '  '.                            
013900     88  BASEN-SLUT                VALUE 'GB'.                            
014000*                                                                         
014100*                                                                         
014200   03    SSA1           PIC X(64).                                        
014300   03    SSA2           PIC X(64).                                        
014400*                                                                         
014500   03    GODK-STATUSKODER.                                                
014600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     EJECT                                                                
014800*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
014900*01      -COPY W0003                                                      
015000     EJECT                                                                
015100*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
015200 01  DLI-IO-AREA1.                                                        
015300     03 IO-AREA1            PIC X(600).                                   
015400*                                                                         
015500*    03 AREA  -COPY WDD201  -PRE WDD201-  -RED IO-AREA1.                  
015600     EJECT                                                                
015700*    03 AREA  -COPY WDD211  -PRE WDD211-  -RED IO-AREA1.                  
015800     EJECT                                                                
015900 01  DLI-IO-AREA.                                                         
016000     03 IO-AREA             PIC X(600).                                   
016100*    03       -COPY WDK601                   -RED IO-AREA.                
016200     EJECT                                                                
016300*    03       -COPY WDK625                   -RED IO-AREA.                
016400     EJECT                                                                
016500*    03 AREA  -COPY WDD311     -PRE BENA11-  -RED IO-AREA.                
016600     EJECT                                                                
016700*    03 AREA  -COPY WDD7A1     -PRE ERSB01-  -RED IO-AREA.                
016800     EJECT                                                                
016900 01  DLI-IO-AREA2.                                                        
017000     03 IO-AREA2            PIC X(100).                                   
017100*                                                                         
017200*    03 AREA  -COPY WDGX1123   -PRE XXAP01-  -RED IO-AREA2.               
017300     EJECT                                                                
017400*    03 AREA  -COPY WDGX1126   -PRE XXAP12-  -RED IO-AREA2.               
017500     EJECT                                                                
017600 01  DLI-IO-AREA3.                                                        
017700     03 IO-AREA3            PIC X(900).                                   
017800*                                                                         
017900*    03       -COPY WDK611                   -RED IO-AREA3.               
018000     EJECT                                                                
018100 LINKAGE SECTION.                                                         
018200*    -COPY W0008 -PRE WDD2-                                               
018300          05  FILLER         PIC XX.                                      
018400*    -COPY W0008 -PRE ARTG-                                               
018500          05  FILLER         PIC XX.                                      
018600     EJECT                                                                
018700*    -COPY W0008 -PRE ARTC-                                               
018800          05  FILLER         PIC XX.                                      
018900     EJECT                                                                
019000*    -COPY W0008 -PRE XXAP-                                               
019100          05  FILLER         PIC XX.                                      
019200*    -COPY W0008 -PRE BENA-                                               
019300          05  FILLER         PIC XX.                                      
019400     EJECT                                                                
019500*    -COPY W0008 -PRE ERSB-                                               
019600          05  FILLER         PIC XX.                                      
019700     EJECT                                                                
019800 PROCEDURE DIVISION  USING WDD2-PCB ARTG-PCB ARTC-PCB                     
019900                           XXAP-PCB BENA-PCB ERSB-PCB.                    
020000     ENTRY 'DLITCBL' USING WDD2-PCB ARTG-PCB ARTC-PCB                     
020100                           XXAP-PCB BENA-PCB ERSB-PCB.                    
020200     PERFORM A-INIT                                                       
020300     PERFORM IMS-GET-WDD2                                                 
020400     PERFORM UNTIL BASEN-SLUT                                             
020500         EVALUATE  WDD2-SEG-NAME-FB                                       
020600            WHEN  'WDD201  '                                              
020700               PERFORM S02-RENSA-SPAR-POST                                
020800               MOVE JA TO SW-SKRIV                                        
020900               IF WDD201-ART-DABASL > 1                                   
021000                 IF IN-001-IDPROJ = WDD201-ART-IDPROJ                     
021100                   MOVE WDD201-ART-IDARTNR    TO                          
021200                                          SPAR-IDARTNR                    
021300                                          W-IDARTNR                       
021400                                          W-IDARTNR-MIN                   
021500                                          W-IDARTNR-MAX                   
021600                                          W-IDARTNR-PRIS                  
021700                   MOVE WDD201-ART-TISTOMREG  TO SPAR-TISTOMREG           
021800                   MOVE WDD201-ART-IDPROJ     TO SPAR-IDPROJ              
021900                   MOVE WDD201-ART-TEARTNOT-BASL  TO                      
022000                                       SPAR-TEARTNOT-BASL                 
022100                   PERFORM B-KTR-PRIS                                     
022200                 ELSE                                                     
022300                   MOVE NEJ TO SW-SKRIV                                   
022400                 END-IF                                                   
022500               ELSE                                                       
022600                 MOVE NEJ TO SW-SKRIV                                     
022700               END-IF                                                     
022800            WHEN   'WDD211'                                               
022900               IF SW-SKRIV = JA                                           
023000                 IF IN-001-KDBASLM = WDD211-ART-KDBASLM                   
023100                   MOVE WDD211-ART-TISTOMREG   TO TMP1-YYMMDD             
023200                   MOVE SPAR-TISTOMREG         TO TMP2-YYMMDD             
023300                   PERFORM WY2000P1                                       
023400                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
023500                     MOVE WDD211-ART-TISTOMREG TO SPAR-TISTOMREG          
023600                   END-IF                                                 
023700                   PERFORM C-KTR-KDDEALER-MARK-NOT                        
023800                   IF SW-SKRIV = JA                                       
023900                     PERFORM D-KTR-MARK-QTY                               
024000                     IF SW-SKRIV = JA                                     
024100                       IF XXAP12-1126-IDDISTR(2) > ZERO                   
024200                         MOVE +1 TO INDX-A                                
024300                         PERFORM UNTIL INDX-A > 6                         
024400                           IF WDD211-ART-KVBASLMD(INDX-A) > ZERO          
024500                             MOVE +8 TO INDX-A                            
024600                           END-IF                                         
024700                           ADD +1 TO INDX-A                               
024800                         END-PERFORM                                      
024900                         IF INDX-A = +9                                   
025000                           PERFORM E-INTE-PROCENT-FORDEL                  
025100                         ELSE                                             
025200                           PERFORM F-PROCENT-FORDEL                       
025300                         END-IF                                           
025400                       ELSE                                               
025500                         COMPUTE                                          
025600                         SPAR-KVBASLM = WDD211-ART-KVBASLMD(1) +          
025700                                        SPAR-KVBLKIT *                    
025800                                        WDD211-ART-KVBASLKIT              
025900                                                                          
026000                         MOVE WDD211-ART-KVBASLMD(1) TO                   
026100                         SPAR-KVBASLMD                                    
026200                         MOVE WDD211-ART-KVBASLKIT TO                     
026300                         SPAR-KVBASLKIT                                   
026400*------------------------SUMMA PER ARTIKLEL ---------------               
026500                         COMPUTE                                          
026600                         SPAR-SUM-AKTUELL-ART =                           
026700                         SPAR-PRIS-AKTUELL-ART * SPAR-KVBASLM             
026800*-----------------------------------------------------------------        
026900                       END-IF                                             
027000                       IF SW-SKRIV = JA                                   
027100                         PERFORM IMS-GU-ARTC01                            
027200                         IF SEGMENT-FINNS                                 
027300                           PERFORM G-KTR-PRODSL                           
027400                           IF SW-SKRIV = JA                               
027500                             PERFORM H-KTR-IDFKNGRP-KDBPSR                
027600                             IF SW-SKRIV = JA                             
027700                               MOVE ART-TIFINLV TO                        
027800                                                 SPAR-TIFINLV             
027900                               MOVE ART-REKSIFFR TO                       
028000                                                 SPAR-REKSIFFR            
028100                               PERFORM IMS-GNP-ARTC25                     
028200                               IF SEGMENT-FINNS                           
028300                                 MOVE NOT-TEARTNOT TO                     
028400                                                 SPAR-TEARTNOT            
028500                               END-IF                                     
028600                               PERFORM IMS-GET-BENA11                     
028700                               MOVE BENA11-TEXT-BEART TO                  
028800                                                 SPAR-BEART               
028900                               PERFORM I-PREP-UTPOST                      
029000                             END-IF                                       
029100                           END-IF                                         
029200                         END-IF                                           
029300                       END-IF                                             
029400                     END-IF                                               
029500                   END-IF                                                 
029600                 END-IF                                                   
029700               END-IF                                                     
029800         END-EVALUATE                                                     
029900         PERFORM IMS-GET-WDD2                                             
030000     END-PERFORM                                                          
030100     PERFORM Z-FINIT                                                      
030200     MOVE ZERO               TO RETURN-CODE                               
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 A-INIT SECTION.                                                          
030700                                                                          
030800     OPEN INPUT KORTFIL                                                   
030900     OPEN OUTPUT UTFIL                                                    
031000                                                                          
031100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
031200                                                                          
031300     PERFORM S01-LAES-KORTFIL                                             
031400                                                                          
031500     MOVE +11              TO W-1123-KDPRODSL                             
031600     MOVE IN-001-IDPROJ    TO W-1123-IDPROJ                               
031700     MOVE IN-001-KDBASLM   TO W-1126-KDBASLM                              
031800                                                                          
031900     MOVE IN-001-IDSKYLT   TO W-IDSKYLT                                   
032000                                                                          
032100     PERFORM IMS-GU-XXAP12                                                
032200                                                                          
032300     IF XXAP12-1126-IDDISTR(2) > ZERO                                     
032400       MOVE +1 TO INDX-A                                                  
032500       PERFORM UNTIL INDX-A > 6                                           
032600         IF XXAP12-1126-IDDISTR(INDX-A) = ZERO                            
032700           MOVE ZERO TO WS-SPAR-REBLFORD(INDX-A)                          
032800         ELSE                                                             
032900           MOVE XXAP12-1126-REBLFORD(INDX-A) TO                           
033000           WS-SPAR-REBLFORD(INDX-A)                                       
033100         END-IF                                                           
033200         ADD +1 TO INDX-A                                                 
033300       END-PERFORM                                                        
033400     ELSE                                                                 
033500       MOVE XXAP12-1126-KVBLKIT TO SPAR-KVBLKIT                           
033600       MOVE XXAP12-1126-IDDISTR(1) TO WS-SPAR-IDDISTR(1)                  
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 B-KTR-PRIS   SECTION.                                                    
034100                                                                          
034200*------------KTR OM PRIS FINNS PÅ AKTUELL ARTIKEL                         
034300     PERFORM IMS-GET-ARTC11                                               
034400     IF SEGMENT-FINNS                                                     
034500        MOVE CLAG-PRARTBTO-EXP TO                                         
034600                    SPAR-PRARTBTO-EXP-CP                                  
034700                    SPAR-PRIS-AKTUELL-ART                                 
034800     ELSE                                                                 
034900        MOVE ZERO TO SPAR-PRIS-AKTUELL-ART                                
035000     END-IF                                                               
035100                                                                          
035200*------------KTR OM JÄMFÖRBART ELLER ERSATT ARTIKEL FINNS                 
035300                                                                          
035400     IF WDD201-ART-IDARTNR-MOTSV = ZERO                                   
035500       PERFORM IMS-GU-ERSB01                                              
035600       IF SEGMENT-FINNS                                                   
035700         MOVE ERSB01-ERS-IDARTNR TO                                       
035800                        SPAR-IDARTNR-MOTSV                                
035900                        W-IDARTNR-PRIS                                    
036000       ELSE                                                               
036100         MOVE ZERO TO   SPAR-IDARTNR-MOTSV                                
036200                        W-IDARTNR-PRIS                                    
036300       END-IF                                                             
036400     ELSE                                                                 
036500       MOVE WDD201-ART-IDARTNR-MOTSV   TO                                 
036600                        SPAR-IDARTNR-MOTSV                                
036700                        W-IDARTNR-PRIS                                    
036800     END-IF                                                               
036900                                                                          
037000                                                                          
037100*-----------HÄMTER REKSIFFRA TILL EV JMB EL. TILLK ARTNR                  
037200                                                                          
037300     PERFORM IMS-GU-ARTC01-REKS-MOTSV                                     
037400     IF SEGMENT-FINNS                                                     
037500        MOVE ART-REKSIFFR TO SPAR-REKS-MOTSV                              
037600        MOVE '-'          TO SPAR-M-STRECK                                
037700                                                                          
037800*------------KTR OM PRIS FINNS PÅ JMB. TILLK ART FINNS                    
037900        PERFORM IMS-GNP-ARTC11                                            
038000        IF SEGMENT-FINNS                                                  
038100           MOVE CLAG-PRARTBTO-EXP TO                                      
038200                       SPAR-PRARTBTO-EXP-CP                               
038300        ELSE                                                              
038400           MOVE ZERO TO SPAR-PRARTBTO-EXP-CP                              
038500        END-IF                                                            
038600                                                                          
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 C-KTR-KDDEALER-MARK-NOT SECTION.                                         
039100                                                                          
039200     MOVE IN-001-KDDEALER TO GODK-KDDEALER                                
039300                                                                          
039400     IF GODK-KDDEALER-VAERDEN                                             
039500       IF IN-001-KDDEALER = ' '                                           
039600         MOVE WDD211-ART-KDBASLM TO SPAR-KDBASLM                          
039700         MOVE WDD211-ART-KDDEALER TO SPAR-KDDEALER                        
039800       ELSE                                                               
039900         IF IN-001-KDDEALER = 'A'                                         
040000             IF WDD211-ART-KDDEALER = 'A' OR 'B' OR 'C' OR                
040100                                      'D' OR 'E'                          
040200                MOVE WDD211-ART-KDBASLM TO SPAR-KDBASLM                   
040300                MOVE WDD211-ART-KDDEALER TO SPAR-KDDEALER                 
040400             ELSE                                                         
040500               MOVE NEJ TO SW-SKRIV                                       
040600             END-IF                                                       
040700         ELSE                                                             
040800           IF IN-001-KDDEALER = 'B'                                       
040900             IF WDD211-ART-KDDEALER = 'B' OR 'C' OR 'D' OR 'E'            
041000               MOVE WDD211-ART-KDBASLM TO SPAR-KDBASLM                    
041100               MOVE WDD211-ART-KDDEALER TO SPAR-KDDEALER                  
041200             ELSE                                                         
041300               MOVE NEJ TO SW-SKRIV                                       
041400             END-IF                                                       
041500           ELSE                                                           
041600             IF IN-001-KDDEALER = 'C'                                     
041700                IF WDD211-ART-KDDEALER = 'C' OR 'D' OR 'E'                
041800                   MOVE WDD211-ART-KDBASLM TO SPAR-KDBASLM                
041900                   MOVE WDD211-ART-KDDEALER TO SPAR-KDDEALER              
042000                ELSE                                                      
042100                  MOVE NEJ TO SW-SKRIV                                    
042200                END-IF                                                    
042300             ELSE                                                         
042400                IF IN-001-KDDEALER = 'D'                                  
042500                   IF WDD211-ART-KDDEALER = 'D' OR 'E'                    
042600                      MOVE WDD211-ART-KDBASLM TO SPAR-KDBASLM             
042700                      MOVE WDD211-ART-KDDEALER TO SPAR-KDDEALER           
042800                   ELSE                                                   
042900                     MOVE NEJ TO SW-SKRIV                                 
043000                   END-IF                                                 
043100                ELSE                                                      
043200                   IF IN-001-KDDEALER = 'E'                               
043300                      IF WDD211-ART-KDDEALER = 'E'                        
043400                         MOVE WDD211-ART-KDBASLM TO                       
043500                                             SPAR-KDBASLM                 
043600                         MOVE WDD211-ART-KDDEALER TO                      
043700                                             SPAR-KDDEALER                
043800                      ELSE                                                
043900                        MOVE NEJ TO SW-SKRIV                              
044000                      END-IF                                              
044100                   END-IF                                                 
044200                END-IF                                                    
044300             END-IF                                                       
044400           END-IF                                                         
044500         END-IF                                                           
044600       END-IF                                                             
044700     ELSE                                                                 
044800       MOVE NEJ TO SW-SKRIV                                               
044900     END-IF                                                               
045000                                                                          
045100     IF SW-SKRIV = JA                                                     
045200       IF IN-001-MARK-NOT = 'Y' OR 'N' OR SPACE                           
045300         IF IN-001-MARK-NOT = ' '                                         
045400           CONTINUE                                                       
045500         ELSE                                                             
045600           IF IN-001-MARK-NOT = 'Y'                                       
045700             IF WDD211-ART-TEARTNOT-MARK = SPACE                          
045800               MOVE NEJ TO SW-SKRIV                                       
045900             ELSE                                                         
046000              MOVE WDD211-ART-TEARTNOT-MARK TO SPAR-TEARTNOT-BASL         
046100             END-IF                                                       
046200           ELSE                                                           
046300             IF IN-001-MARK-NOT = 'N'                                     
046400               IF WDD211-ART-TEARTNOT-MARK = SPACE                        
046500                 CONTINUE                                                 
046600               ELSE                                                       
046700                 MOVE NEJ TO SW-SKRIV                                     
046800               END-IF                                                     
046900             END-IF                                                       
047000           END-IF                                                         
047100         END-IF                                                           
047200       ELSE                                                               
047300         MOVE NEJ TO SW-SKRIV                                             
047400       END-IF                                                             
047500     END-IF                                                               
047600                                                                          
047700     .                                                                    
047800     EJECT                                                                
047900 D-KTR-MARK-QTY SECTION.                                                  
048000                                                                          
048100     IF IN-001-MARK-QTY = SPACE                                           
048200       CONTINUE                                                           
048300     ELSE                                                                 
048400       IF IN-001-MARK-QTY = 'Y'                                           
048500******************DATUM.REG ELLER RIVEN FRÅN KÖ  =  KVANTITET LAGD        
048600         IF WDD211-ART-TIBASLM > ZERO                                     
048700           IF WDD211-ART-KVBASLM > ZERO                                   
048800              CONTINUE                                                    
048900           ELSE                                                           
049000              MOVE NEJ TO SW-SKRIV                                        
049100           END-IF                                                         
049200         ELSE                                                             
049300           MOVE NEJ TO SW-SKRIV                                           
049400         END-IF                                                           
049500       ELSE                                                               
049600         IF IN-001-MARK-QTY = 'N'                                         
049700***************RIVEN FRÅN KÖN,  UTAN KVANTET                              
049800           IF WDD211-ART-TIBASLM = +2                                     
049900             IF WDD211-ART-KVBASLM = ZERO                                 
050000                CONTINUE                                                  
050100             ELSE                                                         
050200                MOVE NEJ TO SW-SKRIV                                      
050300             END-IF                                                       
050400           ELSE                                                           
050500***************LIGGER PÅ KÖ,MED ELLER UTAN KVANTITET                      
050600             IF WDD211-ART-TIBASLM = ZERO                                 
050700                CONTINUE                                                  
050800             ELSE                                                         
050900***************DATUM REG. = UPPDAT-KVANT/UPPDATERAD NOLLKVANTITET         
051000                MOVE NEJ TO SW-SKRIV                                      
051100             END-IF                                                       
051200           END-IF                                                         
051300         ELSE                                                             
051400            MOVE NEJ TO SW-SKRIV                                          
051500         END-IF                                                           
051600       END-IF                                                             
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 E-INTE-PROCENT-FORDEL SECTION.                                           
052100     SKIP2                                                                
052200     MOVE +1 TO INDX-A                                                    
052300*-----------------------------SÖKER SPECIELLT DISTRIKT------------        
052400     IF IN-001-IDDISTR > ZERO                                             
052500       PERFORM UNTIL INDX-A > 6                                           
052600            MOVE ZERO TO                                                  
052700            WS-SPAR-IDDISTR(INDX-A)                                       
052800            WS-SPAR-KVBASLMD(INDX-A)                                      
052900            WS-SPAR-SUM-AKTUELL-ART(INDX-A)                               
053000         ADD +1 TO INDX-A                                                 
053100       END-PERFORM                                                        
053200                                                                          
053300       MOVE +1 TO INDX-A                                                  
053400       PERFORM UNTIL INDX-A > 6                                           
053500         IF IN-001-IDDISTR = WDD211-ART-IDDISTR(INDX-A)                   
053600            MOVE WDD211-ART-IDDISTR(INDX-A) TO                            
053700            WS-SPAR-IDDISTR(1)                                            
053800            MOVE WDD211-ART-KVBASLMD(INDX-A) TO                           
053900            WS-SPAR-KVBASLMD(1)                                           
054000            COMPUTE                                                       
054100            WS-SPAR-SUM-AKTUELL-ART(1) =                                  
054200            SPAR-PRIS-AKTUELL-ART * WS-SPAR-KVBASLMD(1)                   
054300         ELSE                                                             
054400            MOVE ZERO TO                                                  
054500            WS-SPAR-IDDISTR(INDX-A)                                       
054600            WS-SPAR-KVBASLMD(INDX-A)                                      
054700            WS-SPAR-SUM-AKTUELL-ART(INDX-A)                               
054800         END-IF                                                           
054900         ADD +1 TO INDX-A                                                 
055000       END-PERFORM                                                        
055100       IF WS-SPAR-IDDISTR(1) > ZERO                                       
055200         MOVE WDD211-ART-KVBASLM TO SPAR-KVBASLM                          
055300       ELSE                                                               
055400         MOVE NEJ TO SW-SKRIV                                             
055500       END-IF                                                             
055600     ELSE                                                                 
055700*------------------------------ALLA DISTRIKT----------------------        
055800       PERFORM UNTIL INDX-A > 6                                           
055900         IF  WDD211-ART-IDDISTR(INDX-A) = ZERO                            
056000           MOVE  +0  TO  WS-SPAR-IDDISTR(INDX-A)                          
056100                         WS-SPAR-KVBASLMD(INDX-A)                         
056200                         WS-SPAR-SUM-AKTUELL-ART(INDX-A)                  
056300         ELSE                                                             
056400           MOVE WDD211-ART-IDDISTR(INDX-A) TO                             
056500           WS-SPAR-IDDISTR(INDX-A)                                        
056600           MOVE WDD211-ART-KVBASLMD(INDX-A)TO                             
056700           WS-SPAR-KVBASLMD(INDX-A)                                       
056800           COMPUTE                                                        
056900           WS-SPAR-SUM-AKTUELL-ART(INDX-A) =                              
057000           SPAR-PRIS-AKTUELL-ART * WS-SPAR-KVBASLMD(INDX-A)               
057100         END-IF                                                           
057200         ADD +1 TO INDX-A                                                 
057300       END-PERFORM                                                        
057400       MOVE WDD211-ART-KVBASLM TO SPAR-KVBASLM                            
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800 F-PROCENT-FORDEL SECTION.                                                
057900     SKIP2                                                                
058000     MOVE +1 TO INDX-A                                                    
058100*---------------------------SPECIELLT DISTRIKT SÖKT                       
058200     IF IN-001-IDDISTR > ZERO                                             
058300       PERFORM UNTIL INDX-A > 6                                           
058400            MOVE ZERO TO                                                  
058500            WS-SPAR-IDDISTR(INDX-A)                                       
058600            WS-SPAR-KVBASLMD(INDX-A)                                      
058700            WS-SPAR-SUM-AKTUELL-ART(INDX-A)                               
058800         ADD +1 TO INDX-A                                                 
058900       END-PERFORM                                                        
059000                                                                          
059100       MOVE +1 TO INDX-A                                                  
059200       PERFORM UNTIL INDX-A > 6                                           
059300         IF IN-001-IDDISTR = WDD211-ART-IDDISTR(INDX-A)                   
059400            MOVE WDD211-ART-IDDISTR(INDX-A) TO WS-SPAR-IDDISTR(1)         
059500            COMPUTE                                                       
059600            WS-SPAR-KVBASLMD(1) = WDD211-ART-KVBASLM *                    
059700                     WS-SPAR-REBLFORD(INDX-A) / 100 + 0.5                 
059800            COMPUTE                                                       
059900            WS-SPAR-SUM-AKTUELL-ART(1) =                                  
060000            SPAR-PRIS-AKTUELL-ART * WS-SPAR-KVBASLMD(1)                   
060100         ELSE                                                             
060200            MOVE ZERO TO                                                  
060300            WS-SPAR-IDDISTR(INDX-A)                                       
060400            WS-SPAR-KVBASLMD(INDX-A)                                      
060500            WS-SPAR-SUM-AKTUELL-ART(INDX-A)                               
060600         END-IF                                                           
060700         ADD +1 TO INDX-A                                                 
060800       END-PERFORM                                                        
060900       IF WS-SPAR-IDDISTR(1) > ZERO                                       
061000         MOVE WDD211-ART-KVBASLM TO SPAR-KVBASLM                          
061100       ELSE                                                               
061200         MOVE NEJ TO SW-SKRIV                                             
061300       END-IF                                                             
061400     ELSE                                                                 
061500*-----------------------ALLA DISTRIKT VISAS ----------------------        
061600       PERFORM UNTIL INDX-A > 6                                           
061700         IF  WDD211-ART-IDDISTR(INDX-A) = ZERO                            
061800           MOVE ZERO TO  WS-SPAR-IDDISTR(INDX-A)                          
061900                         WS-SPAR-KVBASLMD(INDX-A)                         
062000                         WS-SPAR-SUM-AKTUELL-ART(INDX-A)                  
062100         ELSE                                                             
062200           MOVE WDD211-ART-IDDISTR(INDX-A) TO                             
062300           WS-SPAR-IDDISTR(INDX-A)                                        
062400           COMPUTE                                                        
062500           WS-SPAR-KVBASLMD(INDX-A) = WDD211-ART-KVBASLM *                
062600                       WS-SPAR-REBLFORD(INDX-A) / 100 + 0.5               
062700           COMPUTE                                                        
062800           WS-SPAR-SUM-AKTUELL-ART(INDX-A) =                              
062900           SPAR-PRIS-AKTUELL-ART * WS-SPAR-KVBASLMD(INDX-A)               
063000         END-IF                                                           
063100         ADD +1 TO INDX-A                                                 
063200       END-PERFORM                                                        
063300       MOVE WDD211-ART-KVBASLM TO SPAR-KVBASLM                            
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 G-KTR-PRODSL SECTION.                                                    
063800                                                                          
063900     IF IN-001-KDPRODSL = ZERO OR ART-KDPRODSL                            
064000                                                                          
064100*-------------------- OM IN-001-KDPRODSL ÄR IFYLLT INNEBÄR DET            
064200*-------------------- ATT ENDAST DET PRODUKTSLAGET SKALL LISTAS.          
064300*-------------------- ANNARS SKALL ALLA GOOD PRODUCT SLAG LISTAS          
064400                                                                          
064500        MOVE ART-KDPRODSL TO TEST-KDPRODSL   SPAR-KDPRODSL                
064600        IF KDPRODSL-VOLVO-UTAN-EMB                                        
064700           CONTINUE                                                       
064800        ELSE                                                              
064900           MOVE NEJ TO SW-SKRIV                                           
065000        END-IF                                                            
065100     ELSE                                                                 
065200        MOVE NEJ TO SW-SKRIV                                              
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 H-KTR-IDFKNGRP-KDBPSR SECTION.                                           
065700     IF  IN-001-IDFKNGRP-1 = ZERO                                         
065800     AND IN-001-IDFKNGRP-2 = ZERO                                         
065900       MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                                 
066000     ELSE                                                                 
066100       IF IN-001-IDFKNGRP-1 > ZERO                                        
066200         IF IN-001-IDFKNGRP-2 > ZERO                                      
066300           IF ART-IDFKNGRP  < IN-001-IDFKNGRP-1 OR                        
066400              ART-IDFKNGRP  > IN-001-IDFKNGRP-2                           
066500              MOVE NEJ TO SW-SKRIV                                        
066600           ELSE                                                           
066700              MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                          
066800           END-IF                                                         
066900         ELSE                                                             
067000           IF ART-IDFKNGRP = IN-001-IDFKNGRP-1                            
067100             MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                           
067200           ELSE                                                           
067300             MOVE NEJ TO SW-SKRIV                                         
067400           END-IF                                                         
067500         END-IF                                                           
067600       ELSE                                                               
067700         IF ART-IDFKNGRP > IN-001-IDFKNGRP-2                              
067800           MOVE NEJ TO SW-SKRIV                                           
067900         ELSE                                                             
068000           MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                             
068100         END-IF                                                           
068200       END-IF                                                             
068300     END-IF                                                               
068400     EJECT                                                                
068500*    ARTC11                                                               
068600     PERFORM IMS-GNP-ARTC11                                               
068700                                                                          
068800     IF  IN-001-KDBPSR-1 = ZERO                                           
068900     AND IN-001-KDBPSR-2 = ZERO                                           
069000       MOVE CLAG-KDBPSR TO SPAR-KDBPSR                                    
069100     ELSE                                                                 
069200       IF IN-001-KDBPSR-1 > ZERO                                          
069300         IF IN-001-KDBPSR-2 > ZERO                                        
069400           IF CLAG-KDBPSR  < IN-001-KDBPSR-1 OR                           
069500              CLAG-KDBPSR  > IN-001-KDBPSR-2                              
069600              MOVE NEJ TO SW-SKRIV                                        
069700           ELSE                                                           
069800              MOVE CLAG-KDBPSR TO SPAR-KDBPSR                             
069900           END-IF                                                         
070000         ELSE                                                             
070100           IF CLAG-KDBPSR = IN-001-KDBPSR-1                               
070200             MOVE CLAG-KDBPSR TO SPAR-KDBPSR                              
070300           ELSE                                                           
070400             MOVE NEJ TO SW-SKRIV                                         
070500           END-IF                                                         
070600         END-IF                                                           
070700       ELSE                                                               
070800         IF CLAG-KDBPSR > IN-001-KDBPSR-2                                 
070900           MOVE NEJ TO SW-SKRIV                                           
071000         ELSE                                                             
071100           MOVE CLAG-KDBPSR TO SPAR-KDBPSR                                
071200         END-IF                                                           
071300       END-IF                                                             
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 I-PREP-UTPOST SECTION.                                                   
071800                                                                          
071900     IF XXAP12-1126-IDDISTR(2) > ZERO                                     
072000       MOVE +1 TO INDX-A                                                  
072100       PERFORM UNTIL INDX-A > +6                                          
072200         IF WS-SPAR-IDDISTR(INDX-A) = ZERO                                
072300           MOVE +6 TO INDX-A                                              
072400         ELSE                                                             
072500           MOVE WS-SPAR-IDDISTR(INDX-A)  TO SPAR-IDDISTR                  
072600           MOVE WS-SPAR-KVBASLMD(INDX-A) TO SPAR-KVBASLMD                 
072700           MOVE WS-SPAR-SUM-AKTUELL-ART(INDX-A) TO                        
072800                                     SPAR-SUM-AKTUELL-ART                 
072900           PERFORM IA-SKRIV-UTPOST                                        
073000         END-IF                                                           
073100         ADD +1 TO INDX-A                                                 
073200       END-PERFORM                                                        
073300     ELSE                                                                 
073400       MOVE WS-SPAR-IDDISTR(1)  TO SPAR-IDDISTR                           
073500       PERFORM IA-SKRIV-UTPOST                                            
073600     END-IF                                                               
073700     .                                                                    
073800 IA-SKRIV-UTPOST SECTION.                                                 
073900                                                                          
074000     PERFORM IAA-PREP-SEQ-FALT                                            
074100     MOVE '-'                  TO SPAR-STRECK                             
074200     MOVE SPAR-POST TO UT-AREA                                            
074300     WRITE UTPOST FROM UT-AREA                                            
074400                                                                          
074500     MOVE 'W11505'   TO POSTSUM-FDNAMN                                    
074600     MOVE 'W11505D2' TO POSTSUM-DDNAMN2                                   
074700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
074800     CALL POSTSUM USING POSTSUM-PARM                                      
074900     .                                                                    
075000     EJECT                                                                
075100 IAA-PREP-SEQ-FALT SECTION.                                               
075200                                                                          
075300     MOVE +1 TO INDX-B                                                    
075400     PERFORM UNTIL INDX-B > 4                                             
075500       IF IN-001-SEQUENCE(INDX-B) = 'IDARTNR'                             
075600         MOVE SPAR-IDARTNR TO SPAR-SEQUENCE(INDX-B)                       
075700       ELSE                                                               
075800         IF IN-001-SEQUENCE(INDX-B) = 'IDFKNGRP'                          
075900           MOVE SPAR-IDFKNGRP TO SPAR-SEQUENCE(INDX-B)                    
076000         ELSE                                                             
076100           IF IN-001-SEQUENCE(INDX-B) = 'TISTOMREG'                       
076200             MOVE SPAR-TISTOMREG TO SPAR-SEQUENCE(INDX-B)                 
076300           ELSE                                                           
076400             IF IN-001-SEQUENCE(INDX-B) = 'KDPRODSL'                      
076500               MOVE SPAR-KDPRODSL TO SPAR-SEQUENCE(INDX-B)                
076600             ELSE                                                         
076700               MOVE SPACE TO SPAR-SEQUENCE(INDX-B)                        
076800             END-IF                                                       
076900           END-IF                                                         
077000         END-IF                                                           
077100       END-IF                                                             
077200       ADD +1 TO INDX-B                                                   
077300     END-PERFORM                                                          
077400     .                                                                    
077500     EJECT                                                                
077600 S01-LAES-KORTFIL SECTION.                                                
077700     SKIP1                                                                
077800     READ KORTFIL INTO IN-POST                                            
077900     MOVE 'KORTFIL' TO POSTSUM-FDNAMN                                     
078000     MOVE 'W1150501D1' TO POSTSUM-DDNAMN2                                 
078100     MOVE SPACE        TO POSTSUM-TRANSTYP                                
078200     CALL POSTSUM USING POSTSUM-PARM                                      
078300     .                                                                    
078400     EJECT                                                                
078500 S02-RENSA-SPAR-POST SECTION.                                             
078600                                                                          
078700     MOVE SPACE TO SPAR-POST                                              
078800                                                                          
078900                                                                          
079000     MOVE ZERO TO  SPAR-IDDISTR                                           
079100                   SPAR-IDFKNGRP                                          
079200                   SPAR-IDARTNR                                           
079300                   SPAR-REKSIFFR                                          
079400                   SPAR-KDBPSR                                            
079500                   SPAR-TISTOMREG                                         
079600                   SPAR-KVBASLMD                                          
079700                   SPAR-KVBASLKIT                                         
079800                   SPAR-KVBASLM                                           
079900                   SPAR-TIFINLV                                           
080000                   SPAR-IDARTNR-MOTSV                                     
080100                   SPAR-PRARTBTO-EXP-CP                                   
080200                   W-IDARTNR-PRIS                                         
080300                   SPAR-PRIS-AKTUELL-ART                                  
080400                   SPAR-SUM-AKTUELL-ART                                   
080500                   SPAR-REKS-MOTSV                                        
080600     .                                                                    
080700 Z-FINIT SECTION.                                                         
080800     SKIP2                                                                
080900     MOVE 'S' TO POSTSUM-OPKOD                                            
081000     CALL POSTSUM USING POSTSUM-PARM                                      
081100                                                                          
081200     CLOSE KORTFIL                                                        
081300             UTFIL                                                        
081400     EJECT                                                                
081500     .                                                                    
081600******* I M S   S E C T I O N  ***                                        
081700 IMS-GET-WDD2 SECTION.                                                    
081800*                                                                         
081900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
082000     CALL CBLTDLI USING GN WDD2-PCB DLI-IO-AREA1                          
082100     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     SKIP2                                                                
082400     .                                                                    
082500 IMS-GU-XXAP12 SECTION.                                                   
082600*                                                                         
082700     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
082800         DELIMITED BY SIZE INTO SSA1                                      
082900     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
083000         DELIMITED BY SIZE INTO SSA2                                      
083100     MOVE '  '   TO GODK-STATUSKODER                                      
083200     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA2 SSA1 SSA2                
083300     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
083400     PERFORM IMS-STATUSKONTROLL                                           
083500     SKIP2                                                                
083600     .                                                                    
083700     EJECT                                                                
083800 IMS-GET-ARTC11 SECTION.                                                  
083900*                                                                         
084000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-PRIS-X ')'                    
084100         DELIMITED BY SIZE INTO SSA1                                      
084200     MOVE   'WLARTC11 '      TO SSA2                                      
084300     MOVE '  GE'     TO GODK-STATUSKODER                                  
084400     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA3 SSA1 SSA2               
084500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     SKIP2                                                                
084800     .                                                                    
084900     EJECT                                                                
085000 IMS-GU-ARTC01 SECTION.                                                   
085100*                                                                         
085200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
085300         DELIMITED BY SIZE INTO SSA1                                      
085400     MOVE '  GE'   TO GODK-STATUSKODER                                    
085500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
085600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     SKIP2                                                                
085900     .                                                                    
086000 IMS-GU-ARTC01-REKS-MOTSV SECTION.                                        
086100*                                                                         
086200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-PRIS-X ')'                    
086300         DELIMITED BY SIZE INTO SSA1                                      
086400     MOVE '  GE'   TO GODK-STATUSKODER                                    
086500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
086600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     SKIP2                                                                
086900     .                                                                    
087000     EJECT                                                                
087100 IMS-GNP-ARTC11 SECTION.                                                  
087200*                                                                         
087300     MOVE 'WLARTC11 ' TO SSA1                                             
087400     MOVE '  GE'   TO GODK-STATUSKODER                                    
087500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA3 SSA1                    
087600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     SKIP2                                                                
087900     .                                                                    
088000     EJECT                                                                
088100 IMS-GNP-ARTC25 SECTION.                                                  
088200*                                                                         
088300     MOVE   'WLARTC11 ' TO SSA1                                           
088400     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
088500         DELIMITED BY SIZE INTO SSA2                                      
088600     MOVE '  GE'   TO GODK-STATUSKODER                                    
088700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
088800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088900     PERFORM IMS-STATUSKONTROLL                                           
089000     SKIP2                                                                
089100     .                                                                    
089200     EJECT                                                                
089300 IMS-GET-BENA11 SECTION.                                                  
089400*                                                                         
089500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
089600         DELIMITED BY SIZE INTO SSA1                                      
089700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
089800         DELIMITED BY SIZE INTO SSA2                                      
089900     MOVE '  '     TO GODK-STATUSKODER                                    
090000     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA SSA1 SSA2                
090100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
090200     PERFORM IMS-STATUSKONTROLL                                           
090300     SKIP2                                                                
090400     .                                                                    
090500     EJECT                                                                
090600 IMS-GU-ERSB01 SECTION.                                                   
090700*                                                                         
090800     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
090900                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
091000         DELIMITED BY SIZE INTO SSA1                                      
091100     MOVE '  GE'     TO GODK-STATUSKODER                                  
091200     CALL CBLTDLI USING GU  ERSB-PCB DLI-IO-AREA SSA1                     
091300     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     SKIP2                                                                
091600     .                                                                    
091700     EJECT                                                                
091800 IMS-STATUSKONTROLL SECTION.                                              
091900*                                                                         
092000     SET STATUS-IX TO 1                                                   
092100     SEARCH GODK-STATUS AT END CALL FELLOG                                
092200         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
092300         CONTINUE                                                         
092400     END-SEARCH                                                           
092500     .                                                                    
092600     EJECT                                                                
092700     EJECT                                                                
092800*    -COPY WY2000P1                                                       
