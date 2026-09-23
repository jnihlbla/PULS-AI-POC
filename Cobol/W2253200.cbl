000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2253200.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   90/11/15.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MATCHAR UTVAL FRÅN WDM4 MED ORDERRADER FRÅN WDA5                 
001100*        (NERLÄST PÅ FIL). MATCHADE RADER SKRIVS PÅ UTFIL.                
001200*        HÄMTAR LEVERANTÖRSNUMMER FRÅN WDK601 OM DET BEHÖVS.              
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- URVAL FRÅN WDM4 (TPO 1-6)                                  
002800     SELECT W22530                     ASSIGN TO W22532D1.                
002900     SKIP2                                                                
003000*          --- ORDERRADER FRÅN WDA5                                       
003100     SELECT W44061                     ASSIGN TO W22532D2.                
003200     SKIP2                                                                
003300*          --- MATCHADE RADER                                             
003400     SELECT W22532                     ASSIGN TO W22532D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W22530                                                               
004100     LABEL RECORD    STANDARD                                             
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W22530          -L                                             
004600                                                                          
004700     SKIP3                                                                
004800 FD  W44061                                                               
004900     LABEL RECORD    STANDARD                                             
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300*01  -COPY W44060          -L                                             
005400                                                                          
005500     SKIP3                                                                
005600 FD  W22532                                                               
005700     LABEL RECORD    STANDARD                                             
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY W22532     -PRE W22532- -L                                
006200                                                                          
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500     SKIP2                                                                
006600*    -COPY WY2000W1                                                       
006700     SKIP3                                                                
006800 77  IDPGM                       PIC X(8)    VALUE 'W2253200'.            
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100 77  INDX                        PIC S9(3)   VALUE +0   COMP SYNC.        
007200 77  ART-IX                      PIC S9(3)   VALUE +0   COMP SYNC.        
007300 77  MAX-INDX                    PIC S9(3)   VALUE +100 COMP SYNC.        
007400 77  ANT-URVAL                   PIC S9(3)   VALUE +0   COMP SYNC.        
007500                                                                          
007600 77  W22530-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W22530                       VALUE 'J'.                   
007800 77  W44061-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W44061                       VALUE 'J'.                   
008000                                                                          
008100 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
008200     88  TRAFF                               VALUE 'J'.                   
008300     EJECT                                                                
008400 01  DAGENS-DATUM.                                                        
008500     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
008600     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
008700     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
008800     EJECT                                                                
008900 01  ARBETSFAELT.                                                         
009000     03  WS-TIAAVVD.                                                      
009100         05  WS-TIAAVV           PIC S9(4)   VALUE ZERO.                  
009200         05  FILLER              PIC S9.                                  
009300 01  URVALSTABELL.                                                        
009400     03  URVAL   OCCURS 200.                                              
009500       05  URVALS-ID.                                                     
009600         07  URV-IDUSER           PIC X(8).                               
009700         07  URV-TIREGDAT         PIC 9(7).                               
009800         07  URV-TIREGTID         PIC 9(7).                               
009900       05  URV-IDANSK-FOM         PIC 9(3).                               
010000       05  URV-IDANSK-TOM         PIC 9(3).                               
010100       05  URV-KDSORT1            PIC 9.                                  
010200       05  URV-IDLEVNR            PIC X(5).                               
010300       05  URV-KDPRODSL           PIC 9(3).                               
010400       05  URV-IDDISTR-FOM        PIC 9(5).                               
010500       05  URV-IDDISTR-TOM        PIC 9(5).                               
010600       05  URV-KDTPOTYP-FOM       PIC 9.                                  
010700       05  URV-KDTPOTYP-TOM       PIC 9.                                  
010800       05  URV-TITPO-FOM          PIC 9(7).                               
010900       05  URV-TITPO-TOM          PIC 9(7).                               
011000       05  URV-IDARTNR OCCURS 200 PIC 9(9).                               
011100     EJECT                                                                
011200*                                                                         
011300*01    -COPY WWPRODSL                                                     
011400*                                                                         
011500*      --- VALID IDDC CODES                                               
011600*                                                                         
011700*01    -COPY WWDC99                                                       
011800       EJECT                                                              
011900 01  DYNAMISKA-SUBPROGRAM.                                                
012000*                                                                         
012100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     SKIP2                                                                
012700*    --- PARAMETRAR TILL ABEND                                            
012800                                                                          
012900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005      -PRE  POSTSUM-                                      
013500     EJECT                                                                
013600*01  -COPY WDATAREA                                                       
013700     EJECT                                                                
013800*    --- NOLL-AREA FÖR NOLLNING AV UTPOST                                 
013900*01  AREA -COPY W22532          -PRE NOLL-                                
014000     EJECT                                                                
014100 01  IN-AREA-START               PIC X(24)   VALUE                        
014200                                               'IN-AREA-START  '.         
014300     SKIP2                                                                
014400                                                                          
014500*01  AREA -COPY W22530         -PRE IN-                                   
014600     EJECT                                                                
014700 01  WDA5-AREA-START             PIC X(24)   VALUE                        
014800                                               'WDA5-AREA-START'.         
014900     SKIP2                                                                
015000*01  AREA -COPY W44060         -PRE WDA5-                                 
015100     EJECT                                                                
015200 01  UT-AREA-START               PIC X(24)   VALUE                        
015300                                               'UT-AREA-START  '.         
015400     SKIP2                                                                
015500 01  UT-AREA.                                                             
015600     03  FILLER                  PIC X(585).                              
015700*01  FILLER -COPY W22532          -PRE UT-   -RED  UT-AREA                
015800     EJECT                                                                
015900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016200     SKIP3                                                                
016300 01  NYCKLAR-TILL-DL1.                                                    
016400     03  W-IDARTNR-X.                                                     
016500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016600     SKIP2                                                                
016700*    --- STATUS-KOD FRÅN IMS                                              
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FINNS                       VALUE '  '.                  
017000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017300     SKIP2                                                                
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    --- DLI INPUT-OUTPUT AREA                                            
018300 01  FILLER                     PIC X(16) VALUE 'DL1-IO-AREA-01'.         
018400     SKIP3                                                                
018500 01  DLI-IO-AREA-01.                                                      
018600     03  IO-AREA-01             PIC X(150)  VALUE SPACE.                  
018700     SKIP3                                                                
018800     03  WLARTC01 REDEFINES IO-AREA-01.                                   
018900         05  -COPY WDK601                                                 
019000     EJECT                                                                
019100 LINKAGE SECTION.                                                         
019200                                                                          
019300*01  -COPY W0008 -PRE ARTC-                                               
019400     05  FILLER             PIC X.                                        
019500     EJECT                                                                
019600 PROCEDURE DIVISION   USING ARTC-PCB.                                     
019700     ENTRY 'DLITCBL'  USING ARTC-PCB.                                     
019800     SKIP2                                                                
019900                                                                          
020000     PERFORM A-INIT                                                       
020100     PERFORM B-LAES-IN-URVAL-TILL-TABELL                                  
020200     PERFORM S02-LAES-W44061                                              
020300     PERFORM UNTIL END-OF-W44061                                          
020400       MOVE +1 TO INDX                                                    
020500       PERFORM UNTIL INDX > ANT-URVAL                                     
020600         PERFORM C-JFR-ORDERRAD-MED-URVAL                                 
020700         IF TRAFF                                                         
020800           PERFORM D-SKRIV-UT-RADEN                                       
020900         END-IF                                                           
021000         ADD +1 TO INDX                                                   
021100       END-PERFORM                                                        
021200       PERFORM S02-LAES-W44061                                            
021300     END-PERFORM                                                          
021400                                                                          
021500                                                                          
021600     PERFORM Z-FINIT                                                      
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     OPEN INPUT  W22530                                                   
022500                 W44061                                                   
022600                                                                          
022700     OPEN OUTPUT W22532                                                   
022800     SKIP2                                                                
022900     ACCEPT DAGENS-DATUM  FROM DATE                                       
023000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023100                                                                          
023200     INITIALIZE NOLL-AREA                                                 
023300     MOVE NOLL-AREA TO UT-AREA                                            
023400     .                                                                    
023500     EJECT                                                                
023600 B-LAES-IN-URVAL-TILL-TABELL SECTION.                                     
023700                                                                          
023800     PERFORM S01-LAES-W22530                                              
023900     MOVE +1 TO ANT-URVAL                                                 
024000     PERFORM UNTIL END-OF-W22530                                          
024100       MOVE IN-IDUSER       TO URV-IDUSER(ANT-URVAL)                      
024200       MOVE IN-TIREGDAT     TO URV-TIREGDAT(ANT-URVAL)                    
024300       MOVE IN-TIREGTID     TO URV-TIREGTID(ANT-URVAL)                    
024400       MOVE IN-IDANSK-FOM   TO URV-IDANSK-FOM(ANT-URVAL)                  
024500       MOVE IN-IDANSK-TOM   TO URV-IDANSK-TOM(ANT-URVAL)                  
024600       MOVE IN-KDSORT1      TO URV-KDSORT1(ANT-URVAL)                     
024700       MOVE IN-IDLEVNR      TO URV-IDLEVNR(ANT-URVAL)                     
024800       MOVE IN-KDPRODSL     TO URV-KDPRODSL(ANT-URVAL)                    
024900       MOVE IN-IDDISTR-FOM  TO URV-IDDISTR-FOM(ANT-URVAL)                 
025000       MOVE IN-IDDISTR-TOM  TO URV-IDDISTR-TOM(ANT-URVAL)                 
025100       MOVE IN-KDTPOTYP-FOM TO URV-KDTPOTYP-FOM(ANT-URVAL)                
025200       MOVE IN-KDTPOTYP-TOM TO URV-KDTPOTYP-TOM(ANT-URVAL)                
025300       MOVE IN-TITPO-FOM    TO URV-TITPO-FOM(ANT-URVAL)                   
025400       MOVE IN-TITPO-TOM    TO URV-TITPO-TOM(ANT-URVAL)                   
025500       MOVE +1 TO ART-IX                                                  
025600       PERFORM UNTIL ART-IX > MAX-INDX                                    
025700         MOVE IN-IDARTNR(ART-IX)                                          
025800                            TO URV-IDARTNR(ANT-URVAL, ART-IX)             
025900         ADD +1 TO ART-IX                                                 
026000       END-PERFORM                                                        
026100       ADD +1 TO ANT-URVAL                                                
026200       PERFORM S01-LAES-W22530                                            
026300     END-PERFORM                                                          
026400     .                                                                    
026500     EJECT                                                                
026600 C-JFR-ORDERRAD-MED-URVAL SECTION.                                        
026700                                                                          
026800     MOVE NEJ TO TRAFF-SW                                                 
026900     IF URV-IDARTNR(INDX, 1) > 0                                          
027000       MOVE +1 TO ART-IX                                                  
027100       PERFORM UNTIL ART-IX > MAX-INDX OR                                 
027200                     URV-IDARTNR(INDX, ART-IX) = ZERO                     
027300         IF URV-IDARTNR(INDX, ART-IX) = WDA5-RAD-IDARTNR                  
027400           MOVE WDA5-RAD-IDDC    TO WS-IDDC                               
027500           IF CDC                                                         
027600              MOVE JA TO TRAFF-SW                                         
027700           END-IF                                                         
027800         END-IF                                                           
027900         ADD +1 TO ART-IX                                                 
028000       END-PERFORM                                                        
028100     ELSE                                                                 
028200       MOVE JA TO TRAFF-SW                                                
028300     END-IF                                                               
028400                                                                          
028500     IF TRAFF                                                             
028600       IF (URV-IDANSK-FOM(INDX) = 0 AND                                   
028700           URV-IDANSK-TOM(INDX) = 0)                                      
028800       OR (WDA5-RAD-IDANSK >= URV-IDANSK-FOM(INDX) AND                    
028900           WDA5-RAD-IDANSK <= URV-IDANSK-TOM(INDX))                       
029000         IF URV-IDLEVNR(INDX) = SPACE                                     
029100         OR URV-IDLEVNR(INDX) = WDA5-RAD-IDLEVNR                          
029200           IF URV-KDPRODSL(INDX) = 0                                      
029300           OR URV-KDPRODSL(INDX) = WDA5-RAD-KDPRODSL                      
029400             IF (URV-IDDISTR-FOM(INDX) = 0 AND                            
029500                 URV-IDDISTR-TOM(INDX) = 0)                               
029600             OR (WDA5-RAD-IDDISTR >= URV-IDDISTR-FOM(INDX) AND            
029700                 WDA5-RAD-IDDISTR <= URV-IDDISTR-TOM(INDX))               
029800               IF (URV-KDTPOTYP-FOM(INDX) = 0 AND                         
029900                   URV-KDTPOTYP-TOM(INDX) = 0)                            
030000               OR (WDA5-RAD-KDTPOTYP >= URV-KDTPOTYP-FOM(INDX)            
030100                   AND                                                    
030200                   WDA5-RAD-KDTPOTYP <= URV-KDTPOTYP-TOM(INDX))           
030300                 MOVE WDA5-RAD-TITPO        TO TMP1-YYMMDD                
030400                 MOVE URV-TITPO-FOM(INDX)   TO TMP2-YYMMDD                
030500                 MOVE URV-TITPO-TOM(INDX)   TO TMP3-YYMMDD                
030600                 PERFORM WY2000Q1                                         
030700                 IF (TMP1-YYMMDD >= TMP2-YYMMDD AND                       
030800                     TMP1-YYMMDD <= TMP3-YYMMDD)                          
030900*                 ORDERRADEN PASSAR IN I URVALET, OK ATT SKRIVA           
031000                   CONTINUE                                               
031100                 ELSE                                                     
031200                   MOVE NEJ TO TRAFF-SW                                   
031300                 END-IF                                                   
031400               ELSE                                                       
031500                 MOVE NEJ TO TRAFF-SW                                     
031600               END-IF                                                     
031700             ELSE                                                         
031800               MOVE NEJ TO TRAFF-SW                                       
031900             END-IF                                                       
032000           ELSE                                                           
032100             MOVE NEJ TO TRAFF-SW                                         
032200           END-IF                                                         
032300         ELSE                                                             
032400           MOVE NEJ TO TRAFF-SW                                           
032500         END-IF                                                           
032600       ELSE                                                               
032700         MOVE NEJ TO TRAFF-SW                                             
032800       END-IF                                                             
032900     END-IF                                                               
033000                                                                          
033100     .                                                                    
033200     EJECT                                                                
033300 D-SKRIV-UT-RADEN SECTION.                                                
033400                                                                          
033500     MOVE URV-IDUSER(INDX)       TO UT-IDUSER                             
033600     MOVE URV-TIREGDAT(INDX)     TO UT-TIREGDAT                           
033700     MOVE URV-TIREGTID(INDX)     TO UT-TIREGTID                           
033800*    MOVE ZERO                   TO UT-URV-IDURVNR                        
033900     MOVE URV-IDANSK-FOM(INDX)   TO UT-URV-IDANSK-FOM                     
034000     MOVE URV-IDANSK-TOM(INDX)   TO UT-URV-IDANSK-TOM                     
034100     MOVE URV-KDSORT1(INDX)      TO UT-URV-KDSORT1                        
034200     MOVE URV-IDLEVNR(INDX)      TO UT-URV-IDLEVNR                        
034300     MOVE URV-KDPRODSL(INDX)     TO UT-URV-KDPRODSL                       
034400     MOVE URV-IDDISTR-FOM(INDX)  TO UT-URV-IDDISTR-FOM                    
034500     MOVE URV-IDDISTR-TOM(INDX)  TO UT-URV-IDDISTR-TOM                    
034600     MOVE SPACE                  TO UT-URV-KDBASLM-FOM                    
034700                                    UT-URV-KDBASLM-TOM                    
034800     MOVE URV-KDTPOTYP-FOM(INDX) TO UT-URV-KDTPOTYP-FOM                   
034900     MOVE URV-KDTPOTYP-TOM(INDX) TO UT-URV-KDTPOTYP-TOM                   
035000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
035100     MOVE URV-TITPO-FOM(INDX) TO DAT-I-TIDATUM                            
035200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
035300                         DAT-O-TIDATUM DAT-KDSVAR                         
035400     IF DAT-KDSVAR-OK                                                     
035500       MOVE DAT-TIAAVVD-GRP      TO WS-TIAAVVD                            
035600       MOVE WS-TIAAVV            TO UT-URV-TITPO-FOM                      
035700     ELSE                                                                 
035800       MOVE ZERO                 TO UT-URV-TITPO-FOM                      
035900     END-IF                                                               
036000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
036100     MOVE URV-TITPO-TOM(INDX) TO DAT-I-TIDATUM                            
036200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036300                         DAT-O-TIDATUM DAT-KDSVAR                         
036400     IF DAT-KDSVAR-OK                                                     
036500       MOVE DAT-TIAAVVD-GRP      TO WS-TIAAVVD                            
036600       MOVE WS-TIAAVV            TO UT-URV-TITPO-TOM                      
036700     ELSE                                                                 
036800       MOVE ZERO                 TO UT-URV-TITPO-TOM                      
036900     END-IF                                                               
037000     MOVE +1 TO ART-IX                                                    
037100     PERFORM UNTIL ART-IX > MAX-INDX                                      
037200       MOVE URV-IDARTNR(INDX, ART-IX) TO UT-URV-IDARTNR(ART-IX)           
037300       ADD +1 TO ART-IX                                                   
037400     END-PERFORM                                                          
037500     MOVE WDA5-RAD-IDANSK        TO UT-IDANSK                             
037600     IF WDA5-RAD-IDLEVNR = SPACE                                          
037700       MOVE WDA5-RAD-IDARTNR TO W-IDARTNR                                 
037800       PERFORM IMS-GET-ARTC01                                             
037900       MOVE ART-IDLEVNR         TO UT-IDLEVNR                             
038000     ELSE                                                                 
038100       MOVE WDA5-RAD-IDLEVNR       TO UT-IDLEVNR                          
038200     END-IF                                                               
038300     MOVE WDA5-RAD-IDARTNR       TO UT-IDARTNR                            
038400     MOVE WDA5-RAD-KDTPOTYP      TO UT-KDTPOTYP                           
038500     MOVE WDA5-RAD-KVART         TO UT-KVART                              
038600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
038700     MOVE WDA5-RAD-TITPO TO DAT-I-TIDATUM                                 
038800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
038900                         DAT-O-TIDATUM DAT-KDSVAR                         
039000     IF DAT-KDSVAR-OK                                                     
039100       MOVE DAT-TIAAVVD-GRP      TO WS-TIAAVVD                            
039200       MOVE WS-TIAAVV            TO UT-TITPO                              
039300     ELSE                                                                 
039400       MOVE ZERO                 TO UT-TITPO                              
039500     END-IF                                                               
039600     MOVE WDA5-RAD-IDDISTR       TO UT-IDDISTR                            
039700     MOVE SPACE                  TO UT-KDBASLM                            
039800     MOVE WDA5-RAD-IDORDNR5      TO UT-IDORDER                            
039900     MOVE URV-KDPRODSL(INDX)     TO TEST-KDPRODSL                         
040000     IF KDPRODSL-VOLVO-BIMA                                               
040100        PERFORM S11-SKRIV-W22532                                          
040200     END-IF                                                               
040300     MOVE NOLL-AREA TO UT-AREA                                            
040400     .                                                                    
040500     EJECT                                                                
040600 Z-FINIT SECTION.                                                         
040700     CLOSE W22530                                                         
040800           W44061                                                         
040900           W22532                                                         
041000     SKIP2                                                                
041100     MOVE 'S' TO POSTSUM-OPKOD                                            
041200     CALL POSTSUM USING POSTSUM-PARM                                      
041300     .                                                                    
041400     EJECT                                                                
041500 S01-LAES-W22530  SECTION.                                                
041600     SKIP2                                                                
041700     READ W22530 INTO IN-AREA                                             
041800     AT END                                                               
041900        SET END-OF-W22530 TO TRUE                                         
042000                                                                          
042100     NOT AT END                                                           
042200        MOVE 'W22530' TO POSTSUM-FDNAMN                                   
042300        MOVE 'W22532D1' TO POSTSUM-DDNAMN2                                
042400        CALL POSTSUM USING POSTSUM-PARM                                   
042500     END-READ                                                             
042600     .                                                                    
042700     EJECT                                                                
042800 S02-LAES-W44061  SECTION.                                                
042900     SKIP2                                                                
043000     READ W44061 INTO WDA5-AREA                                           
043100     AT END                                                               
043200        SET END-OF-W44061 TO TRUE                                         
043300                                                                          
043400     NOT AT END                                                           
043500        MOVE 'W44061' TO POSTSUM-FDNAMN                                   
043600        MOVE 'W22532D2' TO POSTSUM-DDNAMN2                                
043700        CALL POSTSUM USING POSTSUM-PARM                                   
043800     END-READ                                                             
043900     .                                                                    
044000     EJECT                                                                
044100 S11-SKRIV-W22532 SECTION.                                                
044200     SKIP2                                                                
044300     WRITE W22532-POST FROM UT-AREA                                       
044400                                                                          
044500     MOVE 'W22532 ' TO POSTSUM-FDNAMN                                     
044600     MOVE 'W22532D3' TO POSTSUM-DDNAMN2                                   
044700     CALL POSTSUM USING POSTSUM-PARM                                      
044800     .                                                                    
044900     EJECT                                                                
045000*S99-ABEND SECTION.                                                       
045100*                                                                         
045200*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
045300*    .                                                                    
045400* --- IMS-SEKTIONER ---                                                   
045500     SKIP3                                                                
045600 IMS-GET-ARTC01 SECTION.                                                  
045700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE '  ' TO GODK-STATUSKODER                                        
046000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
046100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-STATUSKONTROLL SECTION.                                              
046600     SKIP2                                                                
046700     SET STATUS-IX TO 1                                                   
046800     SEARCH GODK-STATUS                                                   
046900       AT END CALL FELLOG                                                 
047000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
047100     END-SEARCH                                                           
047200     .                                                                    
047300     EJECT                                                                
047400*    -COPY WY2000Q1                                                       
