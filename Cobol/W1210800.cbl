000100     SKIP2                                                                
000200 ID DIVISION.                                                             
000300*                                                                         
000400 PROGRAM-ID.             W1210800.                                        
000500*AUTHOR.                 BODIL LINDAHL.                                   
000600*DATE-WRITTEN.           JUNI 1986.                                       
000700                                                                          
000800                                                                          
000900*  FUNKTION: PROGRAMMET LÄSER W12102-FELFIL MED POSTER FRÅN W12102        
001000*            SOM 'VÄNTAR PÅ NAMNLEX' ELLER ÄR 'RS-UNIKA'.                 
001100*            BEREDARE HÄMTAS FRÅN WDK6 OCH POSTERNA SORTERAS.             
001200*            AV POSTERNA SKAPAS EN LISTA.                                 
001300*                                                                         
001400*  ÄNDRING:  ARTIKLAR MED DEFINITIV ERSÄTTNINGSKOD SKALL EJ KOMMA         
001500*            MED PÅ LISTAN. / C.E. 93-01-13                               
001600*                                                                         
001700                                                                          
001800                                                                          
001900*    SUBPROGRAM:                                                          
002000*            DATKONV                                                      
002100*            W009VADD                                                     
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900                                                                          
003000*- - - - - - - - - - - - - - INFIL:                                       
003100*                            - - FELMARKERADE POSTER I WDD3               
003200     SELECT  W12102-FELFIL            ASSIGN TO W12108D1.                 
003300                                                                          
003400*- - - - - - - - - - - - - - SORTFIL:                                     
003500     SELECT  SORTFIL                  ASSIGN TO W12108DS.                 
003600                                                                          
003700*- - - - - - - - - - - - - - LISTA                                        
003800     SELECT  W12108-001               ASSIGN TO W12108D2.                 
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W12102-FELFIL                                                        
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  POST     -COPY W12102      -L.                                       
004900                                                                          
005000 SD  SORTFIL                                                              
005100                .                                                         
005200*01  POST     -COPY W12106      -PRE SORT-                                
005300                                                                          
005400 FD  W12108-001                                                           
005500     RECORDING V                                                          
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP2                                                                
005800 01  LISTPOST                   PIC X(124).                               
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100*    -COPY WY2000W2                                                       
006200     SKIP3                                                                
006300 77  IDPGM                      PIC X(8)     VALUE 'W1210800'.            
006400     SKIP2                                                                
006500*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006600                                                                          
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000     SKIP2                                                                
007100*- - - - - - - - - - - - - -  END-OF-FILE SWITCHAR                        
007200 77  W12102-EOF                  PIC X       VALUE 'N'.                   
007300 77  SORTFIL-EOF                 PIC X       VALUE 'N'.                   
007400     SKIP2                                                                
007500*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
007600                                                                          
007700 77  PRINT-RADER-MAX             PIC 9(2)    VALUE 40.                    
007800 77  PRINT-RADER-RAKNARE         PIC 9(2)    VALUE 41.                    
007900 77  SID-RAKNARE                 PIC 9(4)    VALUE ZERO.                  
008000 77  OLD-IDBERED                 PIC 9(3)    VALUE ZERO.                  
008100                                                                          
008200 01  WS-DAGENS-DATUM-AAVVD       PIC 9(5).                                
008300 01  WS-DAGENS-DATUM             PIC 9(6).                                
008400                                                                          
008500 01  VECKOR.                                                              
008600     03  AAVVD                   PIC 9(5).                                
008700     03  FILLER REDEFINES AAVVD.                                          
008800         05  AAVV                PIC 9(4).                                
008900         05  D                   PIC 9(1).                                
009000                                                                          
009100 01  WS-TIUPPDAT                 PIC 9(6).                                
009200                                                                          
009300***** VARIABLER TILL W009VADD                                             
009400 01  W009VADD-DATUM              PIC S9(5) COMP-3.                        
009500 01  W009VADD-ANTAL              PIC S9(3) COMP-3.                        
009600                                                                          
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
009900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
010000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010100   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010200   03  W009VADD                  PIC X(8)    VALUE 'W009VADD'.            
010300     EJECT                                                                
010400*01  -COPY WDATAREA                                                       
010500     EJECT                                                                
010600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
010700                                                                          
010800*01  -COPY W0005       -PRE POSTSUM-.                                     
010900     EJECT                                                                
011000 01  IN-AREA.                                                             
011100*03  AREA1                        -COPY W12102      -PRE IN-              
011200     EJECT                                                                
011300 01  SORT-AREA.                                                           
011400*03  SORT-AREA1                   -COPY W12106      -PRE SORTWS-          
011500     EJECT                                                                
011600 01  LISTPOST-WS.                                                         
011700     05  FILLER                  PIC X(3).                                
011800     05  LIST-IDBERED            PIC 9(3).                                
011900     05  FILLER                  PIC X(13).                               
012000     05  LIST-IDARTNR            PIC Z(8)9.                               
012100     05  FILLER                  PIC X(2).                                
012200     05  LIST-KDERS              PIC Z9(2).                               
012300     05  FILLER                  PIC X(3).                                
012400     05  LIST-BEART              PIC X(25).                               
012500     05  FILLER                  PIC X(5).                                
012600     05  LIST-KDFEL              PIC X(43).                               
012700     05  FILLER                  PIC X(11).                               
012800     EJECT                                                                
012900 01  LISTRUBRIK1.                                                         
013000     05  FILLER                  PIC X(3).                                
013100     05  LISTRUBRIK1-NR          PIC X(10)  VALUE                         
013200     'W12108-001'.                                                        
013300     05  FILLER                  PIC X(31).                               
013400     05  LISTRUBRIK1-RUBTEXT     PIC X(22)  VALUE                         
013600                     'NYA ARTIKELBENÄMNINGAR'.                            
013700     05  FILLER                  PIC X(1).                                
013800     05  LISTRUBRIK1-DATUM.                                               
013900         10  LISTRUBRIK1-DATTEXT PIC X(6)   VALUE                         
014000         'DATUM '.                                                        
014100         10  LISTRUBRIK1-DAT     PIC X(6).                                
014200     05  FILLER                  PIC X(10).                               
014300     05  LISTRUBRIK1-SIDA.                                                
014400         10  LISTRUBRIK1-SIDTEXT PIC X(5)  VALUE                          
014500         'SIDA '.                                                         
014600         10  LISTRUBRIK1-SIDNR   PIC Z(3)9.                               
014700                                                                          
014800 01  LISTRUBRIK2.                                                         
014900     05  FILLER              PIC X(3).                                    
015000     05  FILLER              PIC X(14)  VALUE 'BEREDARENUMMER'.           
015100     05  FILLER              PIC X(2).                                    
015200     05  FILLER              PIC X(12)  VALUE 'ARTIKELNR.'.               
015300     05  FILLER              PIC X(2)   VALUE 'EK'.                       
015400     05  FILLER              PIC X(3).                                    
015500     05  FILLER              PIC X(16)  VALUE 'ARTIKELBENÄMNING'.         
015600     05  FILLER              PIC X(68).                                   
015700     EJECT                                                                
015800 01  NYCKLAR-TILL-DLI.                                                    
015900   03  W-IDARTNR-X.                                                       
016000     05  W-IDARTNR               PIC S9(9)                COMP-3.         
016100     EJECT                                                                
016200*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
016300*                                                                         
016400 01  IMS-WS.                                                              
016500   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
016600     SKIP3                                                                
016700*                            *** STATUSKOD FRÅN IMS                       
016800   03  STATUS-WS                 PIC XX.                                  
016900     88  SEGMENT-FINNS                       VALUE '  '.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     SKIP3                                                                
017200   03  GODK-STATUSKODER.                                                  
017300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500   03  SSA1                      PIC X(64).                               
017600   03  SSA2                      PIC X(64).                               
017700     EJECT                                                                
017800*01  -COPY W0003                                                          
017900     EJECT                                                                
018000 01  DLI-IO-AREA.                                                         
018100   03  IO-AREA-1                PIC X(110)  VALUE SPACE.                  
018200*  03  ARTC01 -COPY WDK601                    -RED IO-AREA-1.             
018300     EJECT                                                                
018400   03  IO-AREA-2                PIC X(900)  VALUE SPACE.                  
018500*  03  ARTC11 -COPY WDK611                    -RED IO-AREA-2.             
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800     SKIP3                                                                
018900*01  -COPY W0008    -PRE ARTC-                                            
019000   05  FILLER  PIC X.                                                     
019100     EJECT                                                                
019200 PROCEDURE DIVISION USING ARTC-PCB .                                      
019300     ENTRY 'DLITCBL' USING ARTC-PCB .                                     
019400                                                                          
019500 STYR SECTION.                                                            
019600     SKIP3                                                                
019700     PERFORM A-INIT                                                       
019800                                                                          
019900     SORT SORTFIL ASCENDING KEY SORT-SORT-IDBERED                         
020000                                SORT-SORT-IDARTNR                         
020100          INPUT  PROCEDURE B-INPUT                                        
020200          OUTPUT PROCEDURE C-OUTPUT                                       
020300                                                                          
020400     IF SORT-RETURN NOT = ZERO                                            
020500       DISPLAY IDPGM                                                      
020600       ' RETURKOD FRÅN SORT STÖRRE ÄN NOLL'                               
020700       CALL FELLOG                                                        
020800     END-IF                                                               
020900     PERFORM Z-FINIT                                                      
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500     SKIP3                                                                
021600     OPEN INPUT W12102-FELFIL                                             
021700                                                                          
021800     OPEN OUTPUT W12108-001                                               
021900                                                                          
022000     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
022100     MOVE WS-DAGENS-DATUM TO LISTRUBRIK1-DAT                              
022200     .                                                                    
022300     EJECT                                                                
022400 B-INPUT SECTION.                                                         
022500     SKIP3                                                                
022600     PERFORM S01-LAES-W12102-FELFIL                                       
022700                                                                          
022800     PERFORM UNTIL W12102-EOF = JA                                        
022900       IF IN-BENAEMNING-KDFEL = 1                                         
023000         PERFORM BA-KOLLA-TIUPPDAT                                        
023100         MOVE AAVVD                   TO TMP1-YYWWD                       
023200         MOVE WS-DAGENS-DATUM-AAVVD   TO TMP2-YYWWD                       
023300         PERFORM WY2000P2                                                 
023400         IF TMP1-YYWWD > TMP2-YYWWD OR                                    
023500            TMP1-YYWWD = TMP2-YYWWD                                       
023600           PERFORM S03-LAES-ARTC                                          
023700           IF ART-KDERS-UTG > +0                                          
023800             CONTINUE                                                     
023900           ELSE                                                           
024000             IF CLAG-KDERS   > +20                                        
024100               CONTINUE                                                   
024200             ELSE                                                         
024300               MOVE CLAG-IDBERED  TO SORTWS-SORT-IDBERED                  
024400               MOVE IN-BENAEMNING-IDARTNR TO SORTWS-SORT-IDARTNR          
024500               MOVE CLAG-KDERS    TO SORTWS-SORT-KDERS                    
024600               MOVE IN-BENAEMNING-BEART TO SORTWS-SORT-BEART              
024700               MOVE IN-BENAEMNING-KDFEL TO SORTWS-SORT-KDFEL              
024800                                                                          
024900               RELEASE SORT-POST FROM SORT-AREA                           
025000             END-IF                                                       
025100           END-IF                                                         
025200         END-IF                                                           
025300       END-IF                                                             
025400       PERFORM S01-LAES-W12102-FELFIL                                     
025500     END-PERFORM                                                          
025600     .                                                                    
025700     EJECT                                                                
025800 BA-KOLLA-TIUPPDAT SECTION.                                               
025900     SKIP3                                                                
026000     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
026100     CALL WDATKONV USING DAT-KDDATFORM                                    
026200                         DAT-I-TIDATUM                                    
026300                         DAT-O-TIDATUM                                    
026400                         DAT-KDSVAR                                       
026500     MOVE DAT-TIAAVVD TO WS-DAGENS-DATUM-AAVVD                            
026600                                                                          
026700     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
026800     MOVE IN-BENAEMNING-TIUPPDAT TO WS-TIUPPDAT                           
026900     MOVE WS-TIUPPDAT TO DAT-I-TIDATUM                                    
027000                                                                          
027100     CALL WDATKONV USING DAT-KDDATFORM                                    
027200                         DAT-I-TIDATUM                                    
027300                         DAT-O-TIDATUM                                    
027400                         DAT-KDSVAR                                       
027500                                                                          
027600     MOVE DAT-TIAAVVD TO AAVVD                                            
027700     MOVE AAVV        TO W009VADD-DATUM                                   
027800     MOVE +4          TO W009VADD-ANTAL                                   
027900     CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                    
028000     MOVE W009VADD-DATUM TO AAVV                                          
028100     .                                                                    
028200     EJECT                                                                
028300 C-OUTPUT SECTION.                                                        
028400     SKIP3                                                                
028500     PERFORM S04-RETURN-SORTFIL                                           
028600                                                                          
028700     PERFORM UNTIL SORTFIL-EOF = JA                                       
028800       IF SORTWS-SORT-KDFEL = 1                                           
028900         IF  ( PRINT-RADER-RAKNARE < PRINT-RADER-MAX )                    
029000         AND ( OLD-IDBERED = SORTWS-SORT-IDBERED )                        
029100           PERFORM CB-SKRIV-PRINT-POST-NYRAD                              
029200         ELSE                                                             
029300           PERFORM CA-SKRIV-PRINT-POST-NYSIDA                             
029400         END-IF                                                           
029500       END-IF                                                             
029600       PERFORM S04-RETURN-SORTFIL                                         
029700     END-PERFORM                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 CA-SKRIV-PRINT-POST-NYSIDA SECTION.                                      
030100     SKIP3                                                                
030200     MOVE ZERO                 TO PRINT-RADER-RAKNARE                     
030300     ADD 1                     TO SID-RAKNARE                             
030400                                                                          
030500     MOVE SID-RAKNARE          TO LISTRUBRIK1-SIDNR                       
030600     MOVE LISTRUBRIK1          TO LISTPOST-WS                             
030700     WRITE LISTPOST FROM LISTPOST-WS                                      
030800     ADD 1                     TO PRINT-RADER-RAKNARE                     
030900                                                                          
031000     MOVE LISTRUBRIK2          TO LISTPOST-WS                             
031100     WRITE LISTPOST FROM LISTPOST-WS                                      
031200     ADD 2                     TO PRINT-RADER-RAKNARE                     
031300                                                                          
031400     MOVE SPACE                TO LISTPOST-WS                             
031500                                                                          
031600     MOVE SORTWS-SORT-IDBERED  TO LIST-IDBERED                            
031700                                  OLD-IDBERED                             
031800     MOVE SORTWS-SORT-IDARTNR  TO LIST-IDARTNR                            
031900     MOVE SORTWS-SORT-BEART    TO LIST-BEART                              
032000                                                                          
032100     WRITE LISTPOST FROM LISTPOST-WS                                      
032200     ADD 2              TO PRINT-RADER-RAKNARE                            
032300     .                                                                    
032400     EJECT                                                                
032500 CB-SKRIV-PRINT-POST-NYRAD SECTION.                                       
032600     SKIP3                                                                
032700     MOVE SORTWS-SORT-IDBERED  TO LIST-IDBERED                            
032800                                  OLD-IDBERED                             
032900     MOVE SORTWS-SORT-IDARTNR  TO LIST-IDARTNR                            
033000     MOVE SORTWS-SORT-BEART    TO LIST-BEART                              
033100                                                                          
033200     WRITE LISTPOST FROM LISTPOST-WS                                      
033300     ADD 2              TO PRINT-RADER-RAKNARE                            
033400     .                                                                    
033500     EJECT                                                                
033600 S01-LAES-W12102-FELFIL SECTION.                                          
033700     SKIP3                                                                
033800     READ W12102-FELFIL INTO IN-AREA                                      
033900        AT END MOVE JA TO W12102-EOF                                      
034000     END-READ                                                             
034100                                                                          
034200     IF W12102-EOF = NEJ                                                  
034300       MOVE 'W12108'   TO POSTSUM-FDNAMN                                  
034400       MOVE 'W12108DD' TO POSTSUM-DDNAMN2                                 
034500       CALL POSTSUM USING POSTSUM-PARM                                    
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 S03-LAES-ARTC SECTION.                                                   
035000     SKIP3                                                                
035100     MOVE IN-BENAEMNING-IDARTNR TO W-IDARTNR                              
035200                                                                          
035300     PERFORM IMS-GET-ARTC01                                               
035400     PERFORM IMS-GET-ARTC11                                               
035500                                                                          
035600     IF SEGMENT-SAKNAS                                                    
035700       MOVE ZERO             TO CLAG-IDBERED                              
035800       IF ART-KDERS-UTG > ZERO                                            
035900         MOVE ZERO           TO CLAG-KDERS                                
036000       ELSE                                                               
036100***      DETTA BORDE INTE INTRÄFFA, OM BASEN ÄR OK.                       
036200         MOVE +18            TO CLAG-KDERS                                
036300***      18 ÄR INGEN RIKTIG ERSÄTTNINGSKOD, MEN ARTIKELN LISTAS.          
036400       END-IF                                                             
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 S04-RETURN-SORTFIL SECTION.                                              
036900     SKIP3                                                                
037000     RETURN SORTFIL INTO SORT-AREA  AT END                                
037100     MOVE JA TO SORTFIL-EOF                                               
037200     END-RETURN                                                           
037300     .                                                                    
037400     EJECT                                                                
037500 Z-FINIT   SECTION.                                                       
037600     SKIP3                                                                
037700     CLOSE  W12102-FELFIL                                                 
037800     CLOSE W12108-001                                                     
037900*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA POSTER               
038000     MOVE 'S' TO POSTSUM-OPKOD                                            
038100     CALL POSTSUM USING POSTSUM-PARM                                      
038200     .                                                                    
038300* IMS SECTIONER                                                           
038400     SKIP3                                                                
038500 IMS-GET-ARTC01 SECTION.                                                  
038600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
038700            DELIMITED BY SIZE INTO SSA1                                   
038800     MOVE '  '   TO GODK-STATUSKODER                                      
038900     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-1 SSA1                        
039000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
039100     PERFORM IMS-STATUSKONTROLL                                           
039200     .                                                                    
039300     SKIP3                                                                
039400 IMS-GET-ARTC11 SECTION.                                                  
039500     MOVE 'WLARTC11 ' TO SSA1                                             
039600     MOVE 'GE  '   TO GODK-STATUSKODER                                    
039700     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA-2 SSA1                       
039800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     EJECT                                                                
040200 IMS-STATUSKONTROLL SECTION.                                              
040300     SET STATUS-IX TO 1                                                   
040400     SEARCH GODK-STATUS AT END CALL FELLOG                                
040500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040600         CONTINUE                                                         
040700     END-SEARCH                                                           
040800      .                                                                   
040900     EJECT                                                                
041000*    -COPY WY2000P2                                                       
