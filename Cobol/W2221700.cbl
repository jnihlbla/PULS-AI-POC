000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2221700.                                            
000300 AUTHOR.             ANN-MARIE DAGE/BODIL LINDAHL                         
000400 DATE-WRITTEN.       AUG  1990.                                           
000500                                                                          
000600     REMARKS.                                                             
000700*    FUNCTION.                                                            
000800*            PROGRAMMET LÄSER TRANSAKTIONS-FIL MED                        
000900*            UPPGIFT OM                                                   
001000*               - UPPDAT SATSSTRUKTURER M.A.P. ING.ARTIKLAR               
001100*                                              (2234-POST)                
001200*            PB-SATS RÄKNAS OM VARJE GÅNG FÖR INGÅENDE ARTIKLAR.          
001300*                                                                         
001400*    SUBPROGRAM:                                                          
001500*            DATKORT                                                      
001600*            POSTSUM                                                      
001700*            WDATKONV                                                     
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*                            *** 2234- SATSSTRUKT. UPPDATERAD  ***        
002300*                            *** INFIL                         ***        
002400     SELECT  W22203  ASSIGN  UT-S-W22217D1.                               
002500*                                                                         
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W22203                                                               
003100     RECORDING F                                                          
003200     BLOCK 0                                                              
003300     LABEL RECORD STANDARD.                                               
003400*01  -COPY W2222234              -L.                                      
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003800*    -COPY WY2000W3                                                       
003900     SKIP3                                                                
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200 77  TABIND                  PIC S9(9)   VALUE +0    COMP-3.              
004300 77  TABIND2                 PIC S9(9)   VALUE +0    COMP-3.              
004400 77  TABIND-MAX              PIC S9(9)   VALUE +1000 COMP-3.              
004500 77  AKTUELL-STRUKTUR        PIC X       VALUE 'J'.                       
004600 77  SPARA-IDARTNR           PIC S9(9)   VALUE +0    COMP-3.              
004700                                                                          
004800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
005500     SKIP3                                                                
005600 01  KONSTANTER.                                                          
005700     03  JA                  PIC X       VALUE 'J'.                       
005800     03  NEJ                 PIC X       VALUE 'N'.                       
005900     03  DELEATE             PIC X       VALUE 'D'.                       
006000                                                                          
006100 01  CHKP-VAR.                                                            
006200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
006300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
006400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
006500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
006600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006700 03  CHKP-MAX                    PIC S9(3)   VALUE +10.                   
006800                                                                          
006900 01  W-ARBETSFALT.                                                        
007000     03  W-DAGENS-DATUM-AAVV PIC 9(4).                                    
007100     03  W-DATUM REDEFINES W-DAGENS-DATUM-AAVV.                           
007200         05  W-DAGENS-DATUM-AA                                            
007300                             PIC 9(2).                                    
007400         05  W-DAGENS-DATUM-VV                                            
007500                             PIC 9(2).                                    
007600                                                                          
007700     03  WS-DAGENS-DATUM-KOLL.                                            
007800         05 WS-AAR           PIC 9(2).                                    
007900         05 WS-MAANAD        PIC 9(2).                                    
008000         05 WS-DAG           PIC 9(2).                                    
008100     03  WS-DAGENS-DATUM-NY REDEFINES WS-DAGENS-DATUM-KOLL                
008200                             PIC 9(6).                                    
008300     03  WS-DAGENS-DATUM     PIC S9(7) COMP-3.                            
008400     03  WS-KOLLVECKA        PIC 9(4) VALUE ZERO.                         
008500                                                                          
008600     03  WS-KVPB-TOT         PIC S9(6)V9(1)          COMP-3.              
008700     03  W-DIFF-KVPB         PIC S9(8)V9(3)          COMP-3.              
008800     03  W-PROGN-KVPB-SATS   PIC S9(6)V9(1)          COMP-3.              
008900     03  WS-KVPB-REF         PIC S9(6)V9(1)          COMP-3.              
009000     03  WS-KVPB-REF-TOT     PIC S9(6)V9(1)          COMP-3.              
009100     03  WS-FAKTOR           PIC S9(2)V9(1)          COMP-3.              
009200                                                                          
009300 01  WS-KDPRODSL             PIC 9(3).                                    
009400 01  FILLER REDEFINES WS-KDPRODSL.                                        
009500     03  FILLER              PIC 9(2).                                    
009600     03  WS-SISTA-SIFFRAN    PIC 9(1).                                    
009700                                                                          
009800 01  STR-TABELL.                                                          
009900     03  TAB-STR-IDARTNR     OCCURS 1000    PIC S9(9)  COMP-3.            
010000                                                                          
010100 77  W22203-EOF-SW           PIC X       VALUE 'N'.                       
010200     88  END-OF-W22203                   VALUE 'J'.                       
010300     EJECT                                                                
010400 01  DYNAMISKA-SUBPROGRAM.                                                
010500     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
010600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
010700     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
010800     EJECT                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000     03  W-IDARTNR-X.                                                     
011100        05  W-IDARTNR           PIC S9(9)               COMP-3.           
011200     03  W-IDLEVNR-X.                                                     
011300        05  W-IDLEVNR           PIC X(5)  VALUE '1002 '.                  
011400     03  W-WDJ1CSEQ-X.                                                    
011500        05  W-IDLEVNR-S         PIC X(5)  VALUE SPACE.                    
011600        05  W-BELEVART-S        PIC X(30)  VALUE SPACE.                   
011700        05  W-IDARTNR-S         PIC S9(9)  VALUE ZERO   COMP-3.           
011800                                                                          
011900*                            *** GENERELLA SUBRUTINER                     
012000 01      SUBPROGRAM.                                                      
012100   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
012200   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
012300   03    ABEND           PIC X(8)    VALUE 'ABEND   '.                    
012400     EJECT                                                                
012500*                            *** PARAMETRAR TILL DATKORT                  
012600 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22217'.                  
012700 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
012800*01  -COPY WDATKORT                                                       
012900     EJECT                                                                
013000*                            *** PARAMETRAR TILL POSTSUM                  
013100*01  -COPY W0005       -PRE POSTSUM-.                                     
013200     EJECT                                                                
013300*01  -COPY WDATAREA                                                       
013400     EJECT                                                                
013500*                            *************************************        
013600*                            ** AREA FÖR W22203 - 2234-POST     **        
013700*                            ** SATSSTRUKTUR UPPDATERAD         **        
013800*                            *************************************        
013900*01  AREA  -COPY W2222234    -PRE I2234-.                                 
014000     EJECT                                                                
014100******************************************************************        
014200*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
014300*                                                                         
014400 01      IMS-WS.                                                          
014500   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
014600                                                                          
014700*                            *** STATUSKOD FRÅN IMS                       
014800   03    STATUS-WS       PIC XX.                                          
014900     88  SEGMENT-FINNS               VALUE '  '.                          
015000     88  SEGMENT-UPPLAGT             VALUE '  '.                          
015100     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
015200     88  IMS-EJ-OK                   VALUE 'XD'.                          
015300     SKIP3                                                                
015400   03    SSA1            PIC X(96).                                       
015500   03    SSA2            PIC X(32).                                       
015600     SKIP3                                                                
015700   03    GODK-STATUSKODER.                                                
015800     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
015900     EJECT                                                                
016000*01      -COPY W0003                                                      
016100     EJECT                                                                
016200 01      DLI-IO-AREA     PIC X(500)  VALUE SPACE.                         
016300     SKIP3                                                                
016400*01  WLSATB01 -COPY WDJ101 -PRE SATB-    -RED DLI-IO-AREA.                
016500     EJECT                                                                
016600*01  WLSATB11 -COPY WDJ111 -PRE SATB-    -RED DLI-IO-AREA.                
016700     EJECT                                                                
016800 01  WDJ1CSEQ REDEFINES DLI-IO-AREA.                                      
016900*    03 WLSATE11       -COPY WDJ111 -PRE SATE-                            
017000     EJECT                                                                
017100*    03 WLSATE01       -COPY WDJ101 -PRE SATE-                            
017200     EJECT                                                                
017300 01      DLI-IO-AREA-2   PIC X(900)  VALUE SPACE.                         
017400     SKIP3                                                                
017500*01  WLARTC01 -COPY WDK601              -RED DLI-IO-AREA-2.               
017600     EJECT                                                                
017700*01  WLARTC11 -COPY WDK611              -RED DLI-IO-AREA-2.               
017800     EJECT                                                                
017900 01      DLI-IO-AREA-3   PIC X(600)  VALUE SPACE.                         
018000     SKIP3                                                                
018100*01  WLARTS01 -COPY WDK711              -RED DLI-IO-AREA-3.               
018200     EJECT                                                                
018300 LINKAGE SECTION.                                                         
018400*01      -COPY W0009     -PRE MSG-                                        
018500     EJECT                                                                
018600*01      -COPY W0008     -PRE ARTC-                                       
018700      05 FILLER          PIC X.                                           
018800     EJECT                                                                
018900*01      -COPY W0008     -PRE SATB-                                       
019000      05 FILLER          PIC X.                                           
019100     EJECT                                                                
019200*01      -COPY W0008     -PRE SATB2-                                      
019300      05 FILLER          PIC X.                                           
019400     EJECT                                                                
019500*01      -COPY W0008     -PRE SATE-                                       
019600      05 FILLER          PIC X.                                           
019700     EJECT                                                                
019800*01      -COPY W0008     -PRE ARTS-                                       
019900      05 FILLER          PIC X.                                           
020000     EJECT                                                                
020100 PROCEDURE DIVISION USING MSG-PCB  ARTC-PCB SATB-PCB SATB2-PCB            
020200                          SATE-PCB ARTS-PCB.                              
020300     SKIP3                                                                
020400     ENTRY 'DLITCBL' USING MSG-PCB  ARTC-PCB SATB-PCB SATB2-PCB           
020500                           SATE-PCB ARTS-PCB.                             
020600     SKIP3                                                                
020700     PERFORM A-INIT                                                       
020800                                                                          
020900     PERFORM UNTIL END-OF-W22203                                          
021000       PERFORM D-BEHANDLA-2234-TRANS                                      
021100                                                                          
021200       IF CHKP-ANT > CHKP-MAX                                             
021300         PERFORM X-TAG-CHECKPOINT                                         
021400       END-IF                                                             
021500                                                                          
021600       PERFORM S01-LAES-W22203-POST                                       
021700     END-PERFORM                                                          
021800     PERFORM E-AVSLUTA                                                    
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500******************************************************************        
022600*                                                                *        
022700*    ÖPPNAR FILER, LÄSER DATUMKORT OCH FÖRSTA POSTER PÅ TRANS-   *        
022800*    FILER.                                                      *        
022900*                                                                *        
023000******************************************************************        
023100                                                                          
023200     OPEN INPUT W22203                                                    
023300                                                                          
023400     PERFORM IMS-RESTART                                                  
023500                                                                          
023600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023700     MOVE D-AAR              TO W-DAGENS-DATUM-AA                         
023800                                WS-AAR                                    
023900     MOVE D-VECKA            TO W-DAGENS-DATUM-VV                         
024000     MOVE D-MAANAD           TO WS-MAANAD                                 
024100     MOVE D-DAG              TO WS-DAG                                    
024200     MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                           
024300                                                                          
024400                                                                          
024500     MOVE 'W22217' TO POSTSUM-PROGNAMN                                    
024600                                                                          
024700     PERFORM S01-LAES-W22203-POST                                         
024800     .                                                                    
024900     EJECT                                                                
025000 D-BEHANDLA-2234-TRANS SECTION.                                           
025100******************************************************************        
025200*                                                                *        
025300*    SATSSTRUKTUR UPPDATERAD (ING ARTIKEL)                       *        
025400*                                                                *        
025500******************************************************************        
025600     MOVE +1 TO TABIND                                                    
025700     PERFORM UNTIL TABIND > TABIND-MAX                                    
025800       MOVE ZERO TO TAB-STR-IDARTNR(TABIND)                               
025900       ADD +1 TO TABIND                                                   
026000     END-PERFORM                                                          
026100                                                                          
026200     MOVE ZERO TO W-PROGN-KVPB-SATS                                       
026300     MOVE ZERO TO WS-KVPB-TOT                                             
026400     MOVE ZERO TO TABIND                                                  
026500                                                                          
026600     IF I2234-KDISATS = DELEATE                                           
026700        MOVE I2234-IDARTNR-ING TO W-IDARTNR                               
026800        PERFORM S96-UPPDATERA-ARTC                                        
026900     ELSE                                                                 
027000                                                                          
027100        MOVE I2234-IDARTNR-ING TO W-IDARTNR-S                             
027200                                  W-IDARTNR                               
027300        MOVE SPACE             TO W-IDLEVNR-S                             
027400        MOVE SPACE             TO W-BELEVART-S                            
027500                                                                          
027600        PERFORM S97-KOLLA-ING-ART                                         
027700                                                                          
027800        PERFORM IMS-GET-SATB01-2                                          
027900        IF SEGMENT-FINNS                                                  
028000           PERFORM S98-LAGRA-I-STR-TAB                                    
028100           PERFORM S03-BEHANDLA-SATS-I-SATS                               
028200        END-IF                                                            
028300                                                                          
028400     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 E-AVSLUTA SECTION.                                                       
028800                                                                          
028900     CLOSE  W22203                                                        
029000                                                                          
029100     MOVE 'S' TO POSTSUM-OPKOD                                            
029200     CALL POSTSUM USING POSTSUM-PARM                                      
029300     EJECT                                                                
029400     .                                                                    
029500     EJECT                                                                
029600 S01-LAES-W22203-POST SECTION.                                            
029700                                                                          
029800     READ W22203 INTO I2234-AREA                                          
029900     AT END                                                               
030000        SET END-OF-W22203 TO TRUE                                         
030100     NOT AT END                                                           
030200        MOVE 'W22203'        TO POSTSUM-FDNAMN                            
030300        MOVE 'W22217D1'      TO POSTSUM-DDNAMN2                           
030400        MOVE I2234-IDHTYP    TO POSTSUM-TRANSTYP                          
030500        CALL POSTSUM USING POSTSUM-PARM                                   
030600     END-READ                                                             
030700     .                                                                    
030800     EJECT                                                                
030900 S03-BEHANDLA-SATS-I-SATS SECTION.                                        
031000******************************************************************        
031100*                                                                *        
031200*    PB-SATS PÅ INGÅENDE ARTIKLAR I SATS-I-SATS BERÄKNAS.        *        
031300*                                                                *        
031400******************************************************************        
031500     SKIP2                                                                
031600     MOVE +1 TO TABIND2                                                   
031700     IF TABIND > 0                                                        
031800       PERFORM UNTIL TABIND2 > TABIND                                     
031900         MOVE TAB-STR-IDARTNR(TABIND2) TO W-IDARTNR                       
032000         PERFORM IMS-GET-SATB01                                           
032100           IF SEGMENT-FINNS                                               
032200             PERFORM IMS-GET-SATB11                                       
032300             PERFORM UNTIL SEGMENT-SAKNAS                                 
032400               MOVE SATB-RAD-IDARTNR TO W-IDARTNR-S                       
032500                                        W-IDARTNR                         
032600               MOVE SPACE            TO W-IDLEVNR-S                       
032700               MOVE SPACE            TO W-BELEVART-S                      
032800                                                                          
032900               PERFORM S97-KOLLA-ING-ART                                  
033000                                                                          
033100               PERFORM IMS-GET-SATB01-2                                   
033200                                                                          
033300               IF SEGMENT-FINNS                                           
033400                  PERFORM S98-LAGRA-I-STR-TAB                             
033500               END-IF                                                     
033600               PERFORM IMS-GET-SATB11                                     
033700             END-PERFORM                                                  
033800           END-IF                                                         
033900           ADD +1 TO TABIND2                                              
034000         END-PERFORM                                                      
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 S96-UPPDATERA-ARTC SECTION.                                              
034500******************************************************************        
034600*                                                                *        
034700*    PB-SATS INGÅENDE ARTIKEL UPPDATERAS.                        *        
034800*                                                                *        
034900******************************************************************        
035000     SKIP2                                                                
035100     PERFORM IMS-GET-ARTC01                                               
035200     IF SEGMENT-FINNS                                                     
035300        IF ART-KDERS-UTG = 0                                              
035400           PERFORM IMS-GET-ARTC11                                         
035500           MOVE W-PROGN-KVPB-SATS TO CLAG-KVPB-SATS                       
035600           PERFORM IMS-REPL-ARTC                                          
035700           ADD +1 TO CHKP-ANT                                             
035800        END-IF                                                            
035900     END-IF                                                               
036000                                                                          
036100     MOVE ZERO TO W-PROGN-KVPB-SATS                                       
036200                  WS-KVPB-TOT                                             
036300     .                                                                    
036400     EJECT                                                                
036500 S97-KOLLA-ING-ART SECTION.                                               
036600******************************************************************        
036700*                                                                *        
036800*    KONTROLL OM SÖKT ARTIKEL INGÅR SOM AKTUELL RAD I AKTUELL    *        
036900*    1002-SATS. VID TRÄFF BERÄKNAS NYTT KVPB-SATS PÅ ARTIKELN    *        
037000*                                                                *        
037100******************************************************************        
037200     PERFORM IMS-GET-SATB-CSEQ-FIRST                                      
037300     PERFORM UNTIL SEGMENT-SAKNAS                                         
037400       IF SATE-STR-IDARTNR < 100000000                                    
037500         IF SATE-STR-TIBORT = 0                                           
037600           MOVE SATE-STR-IDARTNR TO W-IDARTNR                             
037700           PERFORM S972-KOLLA-ERSATTNING                                  
037800           IF AKTUELL-STRUKTUR = NEJ                                      
037900               CONTINUE                                                   
038000           ELSE                                                           
038100             PERFORM S99-LAS-KVPB-TOT                                     
038200             MOVE SATE-RAD-TISTADAT TO TMP1-YYMMDD                        
038300             MOVE WS-DAGENS-DATUM   TO TMP2-YYMMDD                        
038400             MOVE SATE-RAD-TISTODAT TO TMP3-YYMMDD                        
038500             PERFORM WY2000Q1                                             
038600             IF (    TMP1-YYMMDD <= TMP2-YYMMDD                           
038700                 AND TMP3-YYMMDD > TMP2-YYMMDD )                          
038800                PERFORM S971-RAKNA-OM-KVPBSATS                            
038900             ELSE                                                         
039000               IF SATE-RAD-KDISATS = 'N'                                  
039100                  MOVE SATE-RAD-TISTADAT TO DAT-I-TIDATUM                 
039200                  MOVE 'AAVV  '          TO DAT-KDDATFORM                 
039300                  CALL WDATKONV USING DAT-KDDATFORM                       
039400                                      DAT-I-TIDATUM                       
039500                                      DAT-O-TIDATUM                       
039600                                      DAT-KDSVAR                          
039700                  MOVE DAT-TIAAVV-GRP TO WS-KOLLVECKA                     
039800                  IF WS-KOLLVECKA = W-DAGENS-DATUM-AAVV                   
039900                     PERFORM S971-RAKNA-OM-KVPBSATS                       
040000                  END-IF                                                  
040100               ELSE                                                       
040200                  IF SATE-RAD-KDISATS = 'U'                               
040300                    MOVE SATE-RAD-TISTODAT TO DAT-I-TIDATUM               
040400                    MOVE 'AAVV  '          TO DAT-KDDATFORM               
040500                    CALL WDATKONV USING DAT-KDDATFORM                     
040600                                        DAT-I-TIDATUM                     
040700                                        DAT-O-TIDATUM                     
040800                                        DAT-KDSVAR                        
040900                    MOVE DAT-TIAAVV-GRP TO WS-KOLLVECKA                   
041000                    MOVE WS-KOLLVECKA           TO TMP1-YYWW              
041100                    MOVE W-DAGENS-DATUM-AAVV    TO TMP2-YYWW              
041200                    PERFORM WY2000P3                                      
041300                    IF TMP1-YYWW < TMP2-YYWW                              
041400                       PERFORM S971-RAKNA-OM-KVPBSATS                     
041500                    END-IF                                                
041600                  END-IF                                                  
041700               END-IF                                                     
041800             END-IF                                                       
041900           END-IF                                                         
042000         END-IF                                                           
042100       END-IF                                                             
042200       PERFORM IMS-GET-SATB-CSEQ-NEXT                                     
042300     END-PERFORM                                                          
042400                                                                          
042500     MOVE W-IDARTNR-S TO W-IDARTNR                                        
042600     PERFORM S96-UPPDATERA-ARTC                                           
042700     .                                                                    
042800     EJECT          .                                                     
042900 S971-RAKNA-OM-KVPBSATS SECTION.                                          
043000******************************************************************        
043100*                                                                *        
043200*    PB-TOT * RADENS ANTAL UPPDATERAR ARTIKELRADENS PB-SATS      *        
043300*                                                                *        
043400******************************************************************        
043500     SKIP2                                                                
043600     COMPUTE W-DIFF-KVPB ROUNDED = WS-KVPB-TOT                            
043700         * SATE-RAD-REANTPSA                                              
043800     ADD W-DIFF-KVPB TO W-PROGN-KVPB-SATS ROUNDED                         
043900                                                                          
044000     .                                                                    
044100     EJECT                                                                
044200 S972-KOLLA-ERSATTNING SECTION.                                           
044300******************************************************************        
044400*                                                                *        
044500*    KONTROLL OM ÖVERLIGGANDE STRUKTUR ÄR 01 11 04 14 09 19 -    *        
044600*    ERSATT                                                               
044700*    SAKNAS BESTÄLLNINGSREST BERÄKNAS INGET PB-SATS PÅ DESS      *        
044800*    INGÅENDE ARTIKLAR.                                          *        
044900******************************************************************        
045000     SKIP2                                                                
045100     MOVE JA TO AKTUELL-STRUKTUR                                          
045200     PERFORM IMS-GET-ARTC01                                               
045300     IF SEGMENT-FINNS                                                     
045400       MOVE ART-KDPRODSL TO WS-KDPRODSL                                   
045500       PERFORM IMS-GET-ARTC11                                             
045600       IF SEGMENT-FINNS                                                   
045700         IF CLAG-KDERS = 01 OR 04 OR 09 OR 11 OR 14 OR                    
045710                         17 OR 18 OR 19                                   
045800             MOVE NEJ TO AKTUELL-STRUKTUR                                 
045900         END-IF                                                           
046000       END-IF                                                             
046010     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 S98-LAGRA-I-STR-TAB SECTION.                                             
046400******************************************************************        
046500*                                                                *        
046600*    LAGRAR HITTAD STRUKTUR I TABELL.                            *        
046700*                                                                *        
046800******************************************************************        
046900                                                                          
047000     ADD  +1 TO TABIND                                                    
047100                                                                          
047200     MOVE SATB-STR-IDARTNR TO TAB-STR-IDARTNR(TABIND)                     
047300     .                                                                    
047400     EJECT                                                                
047500 S99-LAS-KVPB-TOT SECTION.                                                
047600******************************************************************        
047700*                                                                *        
047800*    PB-SEP + PB-SATS + PB-REF-SDC ADDERAS TILL PB-TOT           *        
047900*    FÖR ÖVERLIGGANDE STRUKTURNUMMER                             *        
048000*                                                                *        
048100******************************************************************        
048200     SKIP2                                                                
048300     IF SEGMENT-FINNS                                                     
048400        COMPUTE WS-KVPB-TOT = CLAG-KVPB-SATS +                            
048500             CLAG-KVPB-SEP                                                
048600     END-IF                                                               
048700                                                                          
048800     PERFORM IMS-GET-ARTS01                                               
048900     IF SEGMENT-FINNS                                                     
049000        PERFORM IMS-GET-ARTS11                                            
049100        PERFORM UNTIL SEGMENT-SAKNAS                                      
049200          ADD SLAG-KVPB-REF TO WS-KVPB-TOT                                
049300          PERFORM IMS-GET-ARTS11                                          
049400        END-PERFORM                                                       
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 X-TAG-CHECKPOINT   SECTION.                                              
049900                                                                          
050000     PERFORM IMS-CHECKPOINT                                               
050100     MOVE ZERO TO CHKP-ANT                                                
050200     .                                                                    
050300     EJECT                                                                
050400* IMS SECTIONER                                                           
050500     SKIP3                                                                
050600 IMS-GET-SATB01 SECTION.                                                  
050700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
050800            DELIMITED BY SIZE INTO SSA1                                   
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
051100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     SKIP3                                                                
051400     .                                                                    
051500 IMS-GET-SATB01-2 SECTION.                                                
051600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
051700            DELIMITED BY SIZE INTO SSA1                                   
051800     MOVE '  GE' TO GODK-STATUSKODER                                      
051900     CALL CBLTDLI USING GU SATB2-PCB DLI-IO-AREA SSA1                     
052000     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     SKIP3                                                                
052300     .                                                                    
052400 IMS-GET-SATB11 SECTION.                                                  
052500     MOVE 'WLSATB11 ' TO SSA1                                             
052600     MOVE '  GE' TO GODK-STATUSKODER                                      
052700     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
052800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
052900     PERFORM IMS-STATUSKONTROLL                                           
053000     SKIP3                                                                
053100     .                                                                    
053200 IMS-GET-SATB-CSEQ-FIRST SECTION.                                         
053300     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
053400            DELIMITED BY SIZE INTO SSA1                                   
053500     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
053600            DELIMITED BY SIZE INTO SSA2                                   
053700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
053800     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
053900     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
054000     PERFORM IMS-STATUSKONTROLL                                           
054100     .                                                                    
054200     EJECT                                                                
054300 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
054400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
054500            DELIMITED BY SIZE INTO SSA1                                   
054600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
054700            DELIMITED BY SIZE INTO SSA2                                   
054800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
054900     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
055000     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
055100     PERFORM IMS-STATUSKONTROLL                                           
055200     .                                                                    
055300     SKIP3                                                                
055400 IMS-GET-ARTC01 SECTION.                                                  
055500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
055600            DELIMITED BY SIZE INTO SSA1                                   
055700     MOVE '  GE' TO GODK-STATUSKODER                                      
055800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
055900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     SKIP3                                                                
056200     .                                                                    
056300 IMS-GET-ARTC11 SECTION.                                                  
056400     MOVE 'WLARTC11 ' TO SSA1                                             
056500     MOVE '  GE' TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-2 SSA1                  
056700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSKONTROLL                                           
056900     EJECT                                                                
057000     .                                                                    
057100 IMS-REPL-ARTC SECTION.                                                   
057200     MOVE '  '   TO GODK-STATUSKODER                                      
057300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-2                       
057400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     EJECT                                                                
057700     .                                                                    
057800 IMS-GET-ARTS01 SECTION.                                                  
057900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
058000            DELIMITED BY SIZE INTO SSA1                                   
058100     MOVE '  GE' TO GODK-STATUSKODER                                      
058200     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-3 SSA1                    
058300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     SKIP3                                                                
058600     .                                                                    
058700     SKIP3                                                                
058800 IMS-GET-ARTS11 SECTION.                                                  
058900     MOVE 'WLARTS11 ' TO SSA1                                             
059000     MOVE '  GE' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-3 SSA1                   
059200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-RESTART SECTION.                                                     
059700     SKIP2                                                                
059800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
059900     MOVE '  ' TO GODK-STATUSKODER                                        
060000     CALL CBLTDLI USING XRST MSG-PCB                                      
060100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
060200                        CHKP-AREA-LENGTH CHKP-AREA                        
060300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     EJECT                                                                
060700 IMS-CHECKPOINT SECTION.                                                  
060800     SKIP2                                                                
060900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
061000     MOVE '  XD' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING CHKP MSG-PCB                                      
061200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
061300                        CHKP-AREA-LENGTH CHKP-AREA                        
061400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061500     PERFORM IMS-STATUSKONTROLL                                           
061600                                                                          
061700     IF IMS-EJ-OK                                                         
061800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
061900       DISPLAY FELTEXT                                                    
062000       CALL FELLOG                                                        
062100     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400 IMS-STATUSKONTROLL SECTION.                                              
062500     SKIP2                                                                
062600     SET STATUS-IX TO 1                                                   
062700     SEARCH GODK-STATUS                                                   
062800       AT END                                                             
062900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063000           DELIMITED BY SIZE INTO FELTEXT                                 
063100         DISPLAY FELTEXT                                                  
063200         CALL FELLOG                                                      
063300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063400         CONTINUE                                                         
063500     END-SEARCH                                                           
063600     .                                                                    
063700     EJECT                                                                
063800*    -COPY WY2000Q1                                                       
063900     EJECT                                                                
064000*    -COPY WY2000P3                                                       
