000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2225600.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   00/03/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MATCHAR W01160 MED REGISTER FÖR KVPB-JUST                        
000900*        NYTT REGISTER SKAPAS                                             
001000*        UPPDATERINGAR AV WDK626 UT PÅ FIL FÖR BMP (W22206)               
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET LÄSER      WDK7                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- REGISTER KVPB-JUST IN                                      
002800     SELECT W22255                     ASSIGN TO W22256D1.                
002900     SKIP2                                                                
003000*          --- LAGERBAND                                                  
003100     SELECT W01160                     ASSIGN TO W22256D2.                
003200     SKIP2                                                                
003300*          --- REGISTER KVPB-JUST UT                                      
003400     SELECT W22256                     ASSIGN TO W22256D3.                
003500     SKIP2                                                                
003600*          --- POSTER FÖR UPPDAT WDK626                                   
003700     SELECT W22257                     ASSIGN TO W22256D4.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W22255                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W22255      -L.                                                
004800     SKIP3                                                                
004900 FD  W01160                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY W01160      -L.                                                
005400     SKIP3                                                                
005500 FD  W22256                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST -COPY W22255 -PRE  REGUT-  -L.                                  
006000     SKIP3                                                                
006100 FD  W22257                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  POST -COPY W22257 -PRE  UT-  -L.                                     
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006810*    -COPY WY2000W2                                                       
006900                                                                          
007000*    -- CHECKED BY WY2000                                                 
007100 77  IDPGM                       PIC X(8)    VALUE 'W2225600'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  W22255-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-W22255                       VALUE 'J'.                   
007700                                                                          
007800 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W01160                       VALUE 'J'.                   
008000                                                                          
008100*    -COPY W222JUST                                                       
008200                                                                          
008300 01  ARBETSFALT.                                                          
008400     03  WS-KVPB-TOT             PIC S9(6)V9(1) VALUE ZERO.               
008500     03  W-ANTAL-VECKOR          PIC S9(3)   VALUE ZERO  COMP-3.          
008600     EJECT                                                                
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009200                                                                          
009300 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-AAVV.                                        
009500     03  DAGENS-AAVV-AAR         PIC 9(2).                                
009600     03  DAGENS-AAVV-VV          PIC 9(2).                                
009700 01  DAGENS-AAVV-PACK            PIC S9(5)   VALUE ZERO COMP-3.           
009710                                                                          
009720 01  DAGENS-AAVVD-PLUS-2V        PIC 9(5)    VALUE ZERO.                  
009730 01  FILLER  REDEFINES DAGENS-AAVVD-PLUS-2V.                              
009740     03  DAGENS-AAVV-PLUS-2V-AAVV PIC 9(4).                               
009750     03  DAGENS-AAVV-PLUS-2V-D   PIC 9.                                   
009760                                                                          
009770 01  DAGENS-AAVV-PLUS-2V         PIC S9(5)   COMP-3 VALUE ZERO.           
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010800     SKIP2                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400     SKIP2                                                                
011500 01  FELTEXT.                                                             
011600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL DATKORT                                          
012000*                                                                         
012100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22256'.              
012200     SKIP2                                                                
012300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012400     SKIP2                                                                
012500*01  -COPY WDATKORT                                                       
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL POSTSUM                                          
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013100*01  -COPY WDATAREA                                                       
013200     EJECT                                                                
013300 01  IN-AREA-START               PIC X(24)   VALUE                        
013400                                 'IN-AREA-START  '.                       
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W22255     -PRE IN-                                       
013800     EJECT                                                                
013900 01  LAG-AREA-START              PIC X(24)   VALUE                        
014000                                 'LAG-AREA-START  '.                      
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY W01160     -PRE LAG-                                      
014400     EJECT                                                                
014500 01  REGUT-AREA-START            PIC X(24)   VALUE                        
014600                                 'REGUT-AREA-START  '.                    
014700     SKIP2                                                                
014800                                                                          
014900*01  AREA -COPY W22255     -PRE REGUT-                                    
015000     EJECT                                                                
015100 01  UT-AREA-START               PIC X(24)   VALUE                        
015200                                 'UT-AREA-START  '.                       
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W22257     -PRE UT-                                       
015600     EJECT                                                                
015700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400     03  W-KDSEGKEY-X.                                                    
016500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016600     03  W-IDDC-X.                                                        
016700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016800     SKIP2                                                                
016900*    --- STATUS-KOD FRÅN IMS                                              
017000 01  STATUS-WS                   PIC XX.                                  
017100     88  SEGMENT-FINNS                       VALUE '  '.                  
017200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     SKIP2                                                                
017500 01  GODK-STATUSKODER.                                                    
017600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017700     SKIP3                                                                
017800 01  SSA1                        PIC X(64).                               
017900 01  SSA2                        PIC X(64).                               
018000     EJECT                                                                
018100*    --- IMS FUNKTIONSKODER                                               
018200*01  -COPY W0003                                                          
018300     EJECT                                                                
018400*    ---  DLI INPUT-OUTPUT AREA                                           
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
018600 01  DLI-IO-WDK601.                                                       
018700*    03  -COPY WDK601                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
019000 01  DLI-IO-WDK611.                                                       
019100*    03  -COPY WDK611                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
019400 01  DLI-IO-WDK626.                                                       
019500*    03  -COPY WDK626                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
019800 01  DLI-IO-WDK701.                                                       
019900*    03  -COPY WDK701                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
020200 01  DLI-IO-WDK711.                                                       
020300*    03  -COPY WDK711                                                     
020400     EJECT                                                                
020500 LINKAGE SECTION.                                                         
020600                                                                          
020700                                                                          
020800*01  -COPY W0008  -PRE WDK6-                                              
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE WDK7-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400 PROCEDURE DIVISION  USING WDK6-PCB WDK7-PCB.                             
021500 MAIN SECTION.                                                            
021600     ENTRY 'DLITCBL' USING WDK6-PCB WDK7-PCB.                             
021700                                                                          
021800     PERFORM A-INIT                                                       
021900                                                                          
022000     PERFORM S01-LAES-W22255                                              
022100     PERFORM S02-LAES-W01160                                              
022200     PERFORM UNTIL END-OF-W01160                                          
022300       IF IN-IDARTNR = LAG-CLAG-IDARTNR                                   
022400          PERFORM B-TRAFF-IDARTNR-REG-LAG                                 
022500          PERFORM S01-LAES-W22255                                         
022600          PERFORM S02-LAES-W01160                                         
022700       ELSE                                                               
022800          IF IN-IDARTNR > LAG-CLAG-IDARTNR                                
022900             MOVE LAG-CLAG-IDPROJ TO WS-IDPROJ                            
023000             IF GODK-PROJEKT                                              
023100                PERFORM C-KOLL-LAGERBAND                                  
023200             END-IF                                                       
023300             PERFORM S02-LAES-W01160                                      
023400          ELSE                                                            
023500*         *  IN-IDARTNR < LAG-CLAG-IDARTNR                                
023600             DISPLAY ' ENDAST REGISTER ' IN-IDARTNR                       
023700             PERFORM D-REG-ARTIKEL-UTGATT-PA-LAG                          
023800             PERFORM S01-LAES-W22255                                      
023900          END-IF                                                          
024000       END-IF                                                             
024100     END-PERFORM                                                          
024200                                                                          
024300     PERFORM Z-FINIT                                                      
024400                                                                          
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 A-INIT SECTION.                                                          
025000                                                                          
025100     OPEN INPUT  W22255                                                   
025200                 W01160                                                   
025300                                                                          
025400     OPEN OUTPUT W22256                                                   
025500                 W22257                                                   
025600                                                                          
025700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025800     MOVE D-AAR       TO DAGENS-DATUM-AAR    DAGENS-AAVV-AAR              
025900     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
026000     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
026100     MOVE D-VECKA     TO DAGENS-AAVV-VV                                   
026200     MOVE DAGENS-AAVV TO DAGENS-AAVV-PACK                                 
026300     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
026400                                                                          
026410     MOVE DAGENS-AAVV TO DAGENS-AAVV-PLUS-2V                              
026420     MOVE 2           TO W-ANTAL-VECKOR                                   
026430     CALL W009VADD    USING DAGENS-AAVV-PLUS-2V W-ANTAL-VECKOR            
026440     MOVE DAGENS-AAVV-PLUS-2V TO DAGENS-AAVV-PLUS-2V-AAVV                 
026450                                                                          
026500*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
026600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
026700*    MOVE AAMMDD   TO DAT-I-TIDATUM                                       
026800*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026900*                    DAT-O-TIDATUM DAT-KDSVAR                             
027000*    IF DAT-KDSVAR-OK                                                     
027100*      ......                                                             
027200*    ELSE                                                                 
027300*      .........                                                          
027400*    END-IF                                                               
027500*------------------------                                                 
027600     .                                                                    
027700     EJECT                                                                
027800 B-TRAFF-IDARTNR-REG-LAG SECTION.                                         
027900                                                                          
028000*    MATCH ! ÄR VÄRDEN PÅ BAS = REG ?                                     
028100                                                                          
028200     MOVE LAG-CLAG-IDARTNR TO W-IDARTNR                                   
028300     PERFORM IMS-GET-WDK601                                               
028400     IF SEGMENT-FINNS                                                     
028410       MOVE ART-TIFINLV          TO TMP1-YYWWD                            
028420       MOVE DAGENS-AAVVD-PLUS-2V TO TMP2-YYWWD                            
028430       PERFORM WY2000P2                                                   
028500        PERFORM IMS-GET-WDK611                                            
028600        IF SEGMENT-FINNS                                                  
028700           PERFORM IMS-GET-WDK626                                         
028800           IF SEGMENT-FINNS                                               
028900             IF  JUST-KVPB-JUST (1) = IN-KVPB-JUST (1) AND                
029000                 JUST-KVPB-JUST (2) = IN-KVPB-JUST (2)                    
029100                                                                          
029200                 MOVE CLAG-IDPROJ TO WS-IDPROJ                            
029300                 IF GODK-PROJEKT AND CLAG-KDERS < 11 AND                  
029310                    TMP1-YYWWD < TMP2-YYWWD                               
029400                    PERFORM S20-BER-PBJUST                                
029500                    IF UT-KVPB-JUST (1) > ZERO OR                         
029600                       UT-KVPB-JUST (2) > ZERO                            
029700                       PERFORM S11-SKRIV-W22256                           
029800                       PERFORM S12-SKRIV-W22257                           
029810                    ELSE                                                  
029820                       IF IN-KVPB-JUST (1) > 0 OR                         
029830                          IN-KVPB-JUST (2) > 0                            
029840                           PERFORM S21-NOLLA-KVPB-JUST                    
029850                           PERFORM S12-SKRIV-W22257                       
029860                       END-IF                                             
029900                    END-IF                                                
030000                 ELSE                                                     
030100                    PERFORM S21-NOLLA-KVPB-JUST                           
030200                    PERFORM S12-SKRIV-W22257                              
030300                 END-IF                                                   
030400             END-IF                                                       
030500           ELSE                                                           
030501             DISPLAY 'SKALL EJ KUNNA HÄNDA ' W-IDARTNR                    
030510           END-IF                                                         
030600        END-IF                                                            
030710     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 C-KOLL-LAGERBAND        SECTION.                                         
031100                                                                          
031200*    FINNS EJ PÅ REG, MANUELL ? ELLER NY ?                                
031300                                                                          
031400     MOVE LAG-CLAG-IDARTNR TO W-IDARTNR                                   
031500     PERFORM IMS-GET-WDK601                                               
031600     IF SEGMENT-FINNS                                                     
031610       MOVE ART-TIFINLV          TO TMP1-YYWWD                            
031620       MOVE DAGENS-AAVVD-PLUS-2V TO TMP2-YYWWD                            
031630       PERFORM WY2000P2                                                   
031640       IF TMP1-YYWWD < TMP2-YYWWD                                         
031700        PERFORM IMS-GET-WDK611                                            
031800        IF SEGMENT-FINNS                                                  
031900           PERFORM IMS-GET-WDK626                                         
032000           IF SEGMENT-FINNS                                               
032100              IF JUST-KVPB-JUST (1) > ZERO OR                             
032200                 JUST-KVPB-JUST (2) > ZERO                                
032300*                * MANUELL                                                
032400                 CONTINUE                                                 
032500              ELSE                                                        
032600                 IF CLAG-KDERS < 11                                       
032700                    PERFORM S20-BER-PBJUST                                
032800                    IF UT-KVPB-JUST (1) > ZERO OR                         
032900                       UT-KVPB-JUST (2) > ZERO                            
033000                       PERFORM S11-SKRIV-W22256                           
033100                       PERFORM S12-SKRIV-W22257                           
033200                    END-IF                                                
033300                 END-IF                                                   
033400              END-IF                                                      
033500           ELSE                                                           
033600              IF CLAG-KDERS < 11                                          
033700                 PERFORM S20-BER-PBJUST                                   
033800                 IF UT-KVPB-JUST (1) > ZERO OR                            
033900                    UT-KVPB-JUST (2) > ZERO                               
034000                    PERFORM S11-SKRIV-W22256                              
034100                    PERFORM S12-SKRIV-W22257                              
034200                 END-IF                                                   
034300              END-IF                                                      
034400           END-IF                                                         
034500        END-IF                                                            
034600       END-IF                                                             
034610     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 D-REG-ARTIKEL-UTGATT-PA-LAG SECTION.                                     
035000                                                                          
035100*    ARTIKELN FINNS EJ LÄNGRE MED PÅ W01160 ???                           
035200*    OM VÄRDENA ÄR OFÖRÄNDRADE,BERÄKNA NYTT                               
035300*    (SKALL DETTA KUNNA HÄNDA ?)                                          
035400                                                                          
035500     MOVE IN-IDARTNR   TO W-IDARTNR                                       
035600     PERFORM IMS-GET-WDK601                                               
035700     IF SEGMENT-FINNS                                                     
035710       MOVE ART-TIFINLV          TO TMP1-YYWWD                            
035720       MOVE DAGENS-AAVVD-PLUS-2V TO TMP2-YYWWD                            
035730       PERFORM WY2000P2                                                   
035740       IF TMP1-YYWWD < TMP2-YYWWD                                         
035800        PERFORM IMS-GET-WDK611                                            
035900        IF SEGMENT-FINNS                                                  
036000           MOVE CLAG-IDPROJ TO WS-IDPROJ                                  
036100           IF GODK-PROJEKT AND CLAG-KDERS < 11                            
036200              PERFORM IMS-GET-WDK626                                      
036300              IF SEGMENT-FINNS                                            
036400                IF  JUST-KVPB-JUST (1) = IN-KVPB-JUST (1) AND             
036500                    JUST-KVPB-JUST (2) = IN-KVPB-JUST (2)                 
036600                    PERFORM S20-BER-PBJUST                                
036700                    IF UT-KVPB-JUST (1) > ZERO OR                         
036800                       UT-KVPB-JUST (2) > ZERO                            
036900                       PERFORM S11-SKRIV-W22256                           
037000                       PERFORM S12-SKRIV-W22257                           
037100                    END-IF                                                
037200                END-IF                                                    
037300              END-IF                                                      
037400           END-IF                                                         
037500        END-IF                                                            
037600       END-IF                                                             
037610     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 Z-FINIT SECTION.                                                         
038100                                                                          
038200     CLOSE W22255                                                         
038300           W01160                                                         
038400           W22256                                                         
038500           W22257                                                         
038600     SKIP2                                                                
038700     MOVE 'S' TO POSTSUM-OPKOD                                            
038800     CALL POSTSUM USING POSTSUM-PARM                                      
038900     .                                                                    
039000     EJECT                                                                
039100 S01-LAES-W22255  SECTION.                                                
039200     READ W22255 INTO IN-AREA                                             
039300     AT END                                                               
039400        MOVE 999999999    TO IN-IDARTNR                                   
039500        SET END-OF-W22255 TO TRUE                                         
039600                                                                          
039700     NOT AT END                                                           
039800        MOVE 'W22255'   TO POSTSUM-FDNAMN                                 
039900        MOVE 'W22256D1' TO POSTSUM-DDNAMN2                                
040000        MOVE 'IREG'     TO POSTSUM-TRANSTYP                               
040100        CALL POSTSUM USING POSTSUM-PARM                                   
040200     END-READ                                                             
040300     .                                                                    
040400     EJECT                                                                
040500 S02-LAES-W01160  SECTION.                                                
040600     READ W01160 INTO LAG-AREA                                            
040700     AT END                                                               
040800        MOVE 999999999    TO LAG-CLAG-IDARTNR                             
040900        SET END-OF-W01160 TO TRUE                                         
041000                                                                          
041100     NOT AT END                                                           
041200        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
041300        MOVE 'W22256D2' TO POSTSUM-DDNAMN2                                
041400        MOVE 'LAG'      TO POSTSUM-TRANSTYP                               
041500        CALL POSTSUM USING POSTSUM-PARM                                   
041600     END-READ                                                             
041700     .                                                                    
041800     EJECT                                                                
041900 S11-SKRIV-W22256 SECTION.                                                
042000                                                                          
042100     WRITE REGUT-POST FROM REGUT-AREA                                     
042200                                                                          
042300     MOVE 'REG '     TO POSTSUM-TRANSTYP                                  
042400     MOVE 'W22255'   TO POSTSUM-FDNAMN                                    
042500     MOVE 'W22256D3' TO POSTSUM-DDNAMN2                                   
042600     CALL POSTSUM USING POSTSUM-PARM                                      
042700     .                                                                    
042800     EJECT                                                                
042900 S12-SKRIV-W22257 SECTION.                                                
043000                                                                          
043100     WRITE UT-POST FROM UT-AREA                                           
043200                                                                          
043300     MOVE 'UPPD'     TO POSTSUM-TRANSTYP                                  
043400     MOVE 'W22257'   TO POSTSUM-FDNAMN                                    
043500     MOVE 'W22256D4' TO POSTSUM-DDNAMN2                                   
043600     CALL POSTSUM USING POSTSUM-PARM                                      
043700     .                                                                    
043800     EJECT                                                                
043900 S20-BER-PBJUST   SECTION.                                                
044000                                                                          
044100     MOVE W-IDARTNR TO UT-IDARTNR     REGUT-IDARTNR                       
044200     MOVE ZERO      TO WS-KVPB-TOT                                        
044300     PERFORM IMS-GET-WDK701                                               
044400     IF SEGMENT-FINNS                                                     
044500        PERFORM IMS-GET-WDK711                                            
044600        PERFORM UNTIL SEGMENT-SAKNAS                                      
044700           ADD SLAG-KVPB-REF TO WS-KVPB-TOT                               
044800           PERFORM IMS-GET-WDK711                                         
044900        END-PERFORM                                                       
045000     END-IF                                                               
045100     COMPUTE WS-KVPB-TOT = WS-KVPB-TOT    +                               
045200                           CLAG-KVPB-SEP  +                               
045300                           CLAG-KVPB-TPO  +                               
045400                           CLAG-KVPB-SATS                                 
045500     COMPUTE UT-KVPB-JUST (1) =  CLAG-KVPB-SEP +                          
045600                           0.05 * WS-KVPB-TOT                             
045700     COMPUTE UT-KVPB-JUST (2) =  CLAG-KVPB-SEP +                          
045800                           0.10 * WS-KVPB-TOT                             
045900     MOVE UT-KVPB-JUST (1) TO REGUT-KVPB-JUST (1)                         
046000     MOVE UT-KVPB-JUST (2) TO REGUT-KVPB-JUST (2)                         
046100                                                                          
046200     MOVE DAGENS-AAVV      TO UT-TIPBJUST (1)                             
046300     MOVE CLAG-KVVECKOR-FT TO W-ANTAL-VECKOR                              
046400     CALL W009VADD      USING UT-TIPBJUST (1) W-ANTAL-VECKOR              
046500     MOVE +1               TO W-ANTAL-VECKOR                              
046600     CALL W009VADD      USING UT-TIPBJUST (1) W-ANTAL-VECKOR              
046700     MOVE UT-TIPBJUST (1)  TO REGUT-TIPBJUST (1)                          
046800                                                                          
046900     MOVE UT-TIPBJUST (1)  TO UT-TIPBJUST (2)                             
047000     MOVE +12              TO W-ANTAL-VECKOR                              
047100     CALL W009VADD      USING UT-TIPBJUST (2) W-ANTAL-VECKOR              
047200     MOVE UT-TIPBJUST (2)  TO REGUT-TIPBJUST (2)                          
047210                                                                          
047220     IF REGUT-KVPB-JUST (1) NOT > ZERO                                    
047230        MOVE ZERO          TO REGUT-TIPBJUST (1)                          
047240                              UT-TIPBJUST(1)                              
047250     END-IF                                                               
047260     IF REGUT-KVPB-JUST (2) NOT > ZERO                                    
047270        MOVE ZERO          TO REGUT-TIPBJUST (2)                          
047280                              UT-TIPBJUST(2)                              
047290     END-IF                                                               
047300                                                                          
047400     MOVE CLAG-IDPROJ      TO REGUT-IDPROJ                                
047500     MOVE CLAG-KDERS       TO REGUT-KDERS                                 
047600     MOVE ART-KDPRODSL     TO REGUT-KDPRODSL                              
047700     MOVE CLAG-IDANSK      TO REGUT-IDANSK                                
047800     .                                                                    
047900     EJECT                                                                
048000 S21-NOLLA-KVPB-JUST SECTION.                                             
048100                                                                          
048200     MOVE W-IDARTNR TO UT-IDARTNR                                         
048300     MOVE ZERO      TO UT-KVPB-JUST (1)                                   
048400                       UT-KVPB-JUST (2)                                   
048500                       UT-TIPBJUST  (1)                                   
048600                       UT-TIPBJUST  (2)                                   
048700     .                                                                    
048800     EJECT                                                                
048900 S99-ABEND SECTION.                                                       
049000                                                                          
049100     SKIP2                                                                
049200     MOVE 'S' TO POSTSUM-OPKOD                                            
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     CALL ABEND USING RKOD-ABEND                                          
049500     .                                                                    
049600     EJECT                                                                
049700* --- IMS SEKTIONER ---                                                   
049800                                                                          
049900 IMS-GET-WDK601 SECTION.                                                  
050000                                                                          
050100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
050200          DELIMITED BY SIZE INTO SSA1                                     
050300     MOVE '  GE' TO GODK-STATUSKODER                                      
050400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
050500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
050800     EJECT                                                                
050900 IMS-GET-WDK611 SECTION.                                                  
051000                                                                          
051100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
051200          DELIMITED BY SIZE INTO SSA1                                     
051300     MOVE '  GE' TO GODK-STATUSKODER                                      
051400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
051500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
051600     PERFORM IMS-STATUSKONTROLL                                           
051700     .                                                                    
051800     EJECT                                                                
051900 IMS-GET-WDK626 SECTION.                                                  
052000                                                                          
052100     STRING 'WDK626    '                                                  
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE '  GE' TO GODK-STATUSKODER                                      
052400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK626 SSA1                   
052500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSKONTROLL                                           
052700     .                                                                    
052800     EJECT                                                                
052900 IMS-GET-WDK701 SECTION.                                                  
053000                                                                          
053100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
053200          DELIMITED BY SIZE INTO SSA1                                     
053300     MOVE '  GE' TO GODK-STATUSKODER                                      
053400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
053500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     .                                                                    
053800     EJECT                                                                
053900 IMS-GET-WDK711 SECTION.                                                  
054000                                                                          
054100*****STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
054200     STRING 'WDK711    '                                                  
054300          DELIMITED BY SIZE INTO SSA1                                     
054400     MOVE '  GE' TO GODK-STATUSKODER                                      
054500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
054600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
054700     PERFORM IMS-STATUSKONTROLL                                           
054800     .                                                                    
054900     EJECT                                                                
055000 IMS-STATUSKONTROLL SECTION.                                              
055100                                                                          
055200     SET STATUS-IX TO 1                                                   
055300     SEARCH GODK-STATUS                                                   
055400       AT END                                                             
055500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055600           DELIMITED BY SIZE INTO FELTEXT                                 
055700         DISPLAY FELTEXT                                                  
055800         CALL FELLOG                                                      
055900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056000         CONTINUE                                                         
056100     END-SEARCH                                                           
056200     .                                                                    
056300*    -COPY WY2000P2                                                       
