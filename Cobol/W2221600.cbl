000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2221600.                                            
000300 AUTHOR.             ANN-MARIE DAGE/BODIL LINDAHL                         
000400 DATE-WRITTEN.       AUG  1990.                                           
000500                                                                          
000600     REMARKS.                                                             
000700*    FUNCTION.                                                            
000800*            PROGRAMMET LÄSER TVÅ TRANSAKTIONS-FILER MED                  
000900*            UPPGIFT OM                                                   
001000*               - UPPDAT PBSEP PER SATSARTIKEL (2202-POST)                
001100*            PB-SATS RÄKNAS OM VARJE GÅNG FÖR INGÅENDE ARTIKLAR.          
001200*                                                                         
001300*            -SEP -96   OMGJORT TILL BMP UTAN CHECKPOINT                  
001400*                       PROGRAMMET BRYTS EFTER 500 BEHANDLADE             
001500*                       INPOSTER OCH RESTERANDE POSTER SKRIVS             
001600*                       PÅ EN NY GENERATION AV INFILEN (FRONTEC)          
001700*    SUBPROGRAM:                                                          
001800*            DATKORT                                                      
001900*            POSTSUM                                                      
002000*            WDATKONV                                                     
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*                            *** 2202- PB-SEP FÖR SATSARTIKEL  ***        
002600*                            *** INFIL                                    
002700     SELECT  W22202    ASSIGN     W22216D1.                               
002800*                                                                         
002900*                                                                         
003000     SELECT  SORTFIL   ASSIGN     W22216DS.                               
003100*                                                                         
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22202                                                               
003700     RECORDING F                                                          
003800     BLOCK 0                                                              
003900     LABEL RECORD STANDARD.                                               
004000*01  -COPY W2222202 -PRE I2202-  -L.                                      
004100     SKIP3                                                                
004200 SD  SORTFIL.                                                             
004300 01  SORT-POST.                                                           
004400     03  SORT-IDARTNR        PIC S9(9)   COMP-3.                          
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800*    -COPY WY2000W3                                                       
004900     SKIP3                                                                
005000*    -COPY WY2000W1                                                       
005100     SKIP3                                                                
005200 77  TABIND                  PIC S9(9)   VALUE +0    COMP-3.              
005300 77  TABIND2                 PIC S9(9)   VALUE +0    COMP-3.              
005400 77  TABIND-MAX              PIC S9(9)   VALUE +1000 COMP-3.              
005500 77  AKTUELL-STRUKTUR        PIC X       VALUE 'J'.                       
005600 77  SPARA-IDARTNR           PIC S9(9)   VALUE +0    COMP-3.              
005700                                                                          
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
006500     SKIP3                                                                
006600 01  KONSTANTER.                                                          
006700     03  JA                  PIC X       VALUE 'J'.                       
006800     03  NEJ                 PIC X       VALUE 'N'.                       
006900     03  DELEATE             PIC X       VALUE 'D'.                       
007000                                                                          
007100 01  CHKP-VAR.                                                            
007200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
007500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
007600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
007700 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
007800                                                                          
007900 01  W-ARBETSFALT.                                                        
008000     03  W-DAGENS-DATUM-AAVV PIC 9(4).                                    
008100     03  W-DATUM REDEFINES W-DAGENS-DATUM-AAVV.                           
008200         05  W-DAGENS-DATUM-AA                                            
008300                             PIC 9(2).                                    
008400         05  W-DAGENS-DATUM-VV                                            
008500                             PIC 9(2).                                    
008600                                                                          
008700     03  WS-DAGENS-DATUM-KOLL.                                            
008800         05 WS-AAR           PIC 9(2).                                    
008900         05 WS-MAANAD        PIC 9(2).                                    
009000         05 WS-DAG           PIC 9(2).                                    
009100     03  WS-DAGENS-DATUM-NY REDEFINES WS-DAGENS-DATUM-KOLL                
009200                             PIC 9(6).                                    
009300     03  WS-DAGENS-DATUM     PIC S9(7) COMP-3.                            
009400     03  WS-KOLLVECKA        PIC 9(4) VALUE ZERO.                         
009500                                                                          
009600     03  WS-KVPB-TOT         PIC S9(6)V9(1)          COMP-3.              
009700     03  W-DIFF-KVPB         PIC S9(8)V9(3)          COMP-3.              
009800     03  W-PROGN-KVPB-SATS   PIC S9(6)V9(1)          COMP-3.              
009900     03  WS-KVPB-REF         PIC S9(6)V9(1)          COMP-3.              
010000     03  WS-KVPB-REF-TOT     PIC S9(6)V9(1)          COMP-3.              
010100     03  WS-FAKTOR           PIC S9(2)V9(1)          COMP-3.              
010200                                                                          
010300 01  SORTWS-AREA-START       PIC X(16)  VALUE 'SORTWS-AREA  '.            
010400                                                                          
010500 01  SORTWS-AREA.                                                         
010600     03  SORTWS-IDARTNR      PIC S9(9)  COMP-3.                           
010700     03  SORT-RETURN-X       PIC X(2)   VALUE SPACE.                      
010800                                                                          
010900 01  WS-KDPRODSL             PIC 9(3).                                    
011000 01  FILLER REDEFINES WS-KDPRODSL.                                        
011100     03  FILLER              PIC 9(2).                                    
011200     03  WS-SISTA-SIFFRAN    PIC 9(1).                                    
011300                                                                          
011400 01  STR-TABELL.                                                          
011500     03  TAB-STR-IDARTNR     OCCURS 1000    PIC S9(9)  COMP-3.            
011600                                                                          
011700 01  EOF-SWITCHAR.                                                        
011800     03  W22202-EOF          PIC X       VALUE 'N'.                       
011900     03  EOF-SORTFIL         PIC X       VALUE 'N'.                       
012000                                                                          
012100     EJECT                                                                
012200 01  DYNAMISKA-SUBPROGRAM.                                                
012300     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
012400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
012500     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
012600     EJECT                                                                
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-IDARTNR-X.                                                     
012900        05  W-IDARTNR           PIC S9(9)               COMP-3.           
013000     03  W-IDLEVNR-X.                                                     
013100        05  W-IDLEVNR           PIC X(5)  VALUE '1002 '.                  
013200     03  W-WDJ1CSEQ-X.                                                    
013300        05  W-IDLEVNR-S         PIC X(5)  VALUE SPACE.                    
013400        05  W-BELEVART-S        PIC X(30)  VALUE SPACE.                   
013500        05  W-IDARTNR-S         PIC S9(9)  VALUE ZERO   COMP-3.           
013600                                                                          
013700*                            *** GENERELLA SUBRUTINER                     
013800 01      SUBPROGRAM.                                                      
013900   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
014000   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
014100   03    ABEND           PIC X(8)    VALUE 'ABEND   '.                    
014200     EJECT                                                                
014300*                            *** PARAMETRAR TILL DATKORT                  
014400 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22216'.                  
014500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
014600*01  -COPY WDATKORT                                                       
014700     EJECT                                                                
014800*                            *** PARAMETRAR TILL POSTSUM                  
014900*01  -COPY W0005       -PRE POSTSUM-.                                     
015000     EJECT                                                                
015100*01  -COPY WDATAREA                                                       
015200     EJECT                                                                
015300*                            *************************************        
015400*                            ** AREA FÖR W22202 - 2202-POST     **        
015500*                            ** LEVERANSPLAN UPPDATERAD         **        
015600*                            *************************************        
015700*01  AREA  -COPY W2222202   -PRE I2202-.                                  
015800     EJECT                                                                
015900******************************************************************        
016000*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
016100*                                                                         
016200 01      IMS-WS.                                                          
016300   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
016400                                                                          
016500*                            *** STATUSKOD FRÅN IMS                       
016600   03    STATUS-WS       PIC XX.                                          
016700     88  SEGMENT-FINNS               VALUE '  '.                          
016800     88  SEGMENT-UPPLAGT             VALUE '  '.                          
016900     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
017000     88  IMS-EJ-OK                   VALUE 'XD'.                          
017100     SKIP3                                                                
017200   03    SSA1            PIC X(96).                                       
017300   03    SSA2            PIC X(32).                                       
017400     SKIP3                                                                
017500   03    GODK-STATUSKODER.                                                
017600     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
017700     EJECT                                                                
017800*01      -COPY W0003                                                      
017900     EJECT                                                                
018000 01      DLI-IO-AREA     PIC X(500)  VALUE SPACE.                         
018100     SKIP3                                                                
018200*01  WLSATB01 -COPY WDJ101 -PRE SATB-    -RED DLI-IO-AREA.                
018300     EJECT                                                                
018400*01  WLSATB11 -COPY WDJ111 -PRE SATB-    -RED DLI-IO-AREA.                
018500     EJECT                                                                
018600 01  WDJ1CSEQ REDEFINES DLI-IO-AREA.                                      
018700*    03 WLSATE11       -COPY WDJ111 -PRE SATE-                            
018800     EJECT                                                                
018900*    03 WLSATE01       -COPY WDJ101 -PRE SATE-                            
019000     EJECT                                                                
019100 01      DLI-IO-AREA-2   PIC X(900)  VALUE SPACE.                         
019200     SKIP3                                                                
019300*01  WLARTC01 -COPY WDK601              -RED DLI-IO-AREA-2.               
019400     EJECT                                                                
019500*01  WLARTC11 -COPY WDK611              -RED DLI-IO-AREA-2.               
019600     EJECT                                                                
019700 01      DLI-IO-AREA-3   PIC X(700)  VALUE SPACE.                         
019800     SKIP3                                                                
019900*01  WLARTS01 -COPY WDK711              -RED DLI-IO-AREA-3.               
020000     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020200                                                                          
020300*01  -COPY W0009         -PRE MSG-                                        
020400     EJECT                                                                
020500*01      -COPY W0008     -PRE ARTC-                                       
020600      05 FILLER          PIC X.                                           
020700     EJECT                                                                
020800*01      -COPY W0008     -PRE SATB-                                       
020900      05 FILLER          PIC X.                                           
021000     EJECT                                                                
021100*01      -COPY W0008     -PRE SATB2-                                      
021200      05 FILLER          PIC X.                                           
021300     EJECT                                                                
021400*01      -COPY W0008     -PRE SATE-                                       
021500      05 FILLER          PIC X.                                           
021600     EJECT                                                                
021700*01      -COPY W0008     -PRE ARTS-                                       
021800      05 FILLER          PIC X.                                           
021900     EJECT                                                                
022000 PROCEDURE DIVISION USING MSG-PCB ARTC-PCB SATB-PCB SATB2-PCB             
022100                          SATE-PCB ARTS-PCB.                              
022200     SKIP3                                                                
022300     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB SATB-PCB SATB2-PCB            
022400                           SATE-PCB ARTS-PCB.                             
022500     SKIP3                                                                
022600     PERFORM A-INIT                                                       
022700                                                                          
022800     IF W22202-EOF = NEJ                                                  
022900       SORT SORTFIL ON                                                    
023000           ASCENDING KEY  SORT-IDARTNR                                    
023100           DUPLICATES IN ORDER                                            
023200           INPUT PROCEDURE B-BEHANDLA-2202-TRANS                          
023300           OUTPUT PROCEDURE C-BEHANDLA-INGAENDE-ARTIKLAR                  
023400                                                                          
023500       IF SORT-RETURN NOT = 0                                             
023600         MOVE SORT-RETURN TO SORT-RETURN-X                                
023700         STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                    
023800             DELIMITED BY SIZE                                            
023900             INTO FELTEXT-STR                                             
024000         DISPLAY FELTEXT                                                  
024100         CALL FELLOG                                                      
024200       ELSE                                                               
024300         PERFORM E-AVSLUTA                                                
024700       END-IF                                                             
024710     END-IF                                                               
024711                                                                          
024712     MOVE ZERO TO RETURN-CODE                                             
024720     GOBACK                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 A-INIT SECTION.                                                          
025100******************************************************************        
025200*                                                                *        
025300*    ÖPPNAR FILER, LÄSER DATUMKORT OCH FÖRSTA POSTER PÅ TRANS-   *        
025400*    FILER.                                                      *        
025500*                                                                *        
025600******************************************************************        
025700                                                                          
025800     OPEN INPUT  W22202                                                   
025900                                                                          
026000     PERFORM IMS-RESTART                                                  
026100                                                                          
026200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026300     MOVE D-AAR              TO W-DAGENS-DATUM-AA                         
026400                                WS-AAR                                    
026500     MOVE D-VECKA            TO W-DAGENS-DATUM-VV                         
026600     MOVE D-MAANAD           TO WS-MAANAD                                 
026700     MOVE D-DAG              TO WS-DAG                                    
026800     MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                           
026900                                                                          
027000                                                                          
027100     MOVE 'W22216' TO POSTSUM-PROGNAMN                                    
027200                                                                          
027300     PERFORM S01-LAES-W22202-POST                                         
027400     .                                                                    
027500     EJECT                                                                
027600 B-BEHANDLA-2202-TRANS SECTION.                                           
027700******************************************************************        
027800*                                                                *        
027900*    PB-SEP UPPDATERAD PÅ SATSARTIKEL                            *        
028000*                                                                *        
028100******************************************************************        
028200                                                                          
028300     PERFORM UNTIL W22202-EOF = JA                                        
028400         MOVE I2202-IDARTNR-SATS TO W-IDARTNR                             
028500         PERFORM IMS-GET-SATB01                                           
028600         IF SEGMENT-FINNS AND SATB-STR-TIBORT = 0                         
028700           AND SATB-STR-IDLEVNR = '1002 '                                 
028800             PERFORM IMS-GET-SATB11                                       
028900             PERFORM UNTIL SEGMENT-SAKNAS                                 
029000                 MOVE SATB-RAD-IDARTNR TO SORTWS-IDARTNR                  
029100                 RELEASE SORT-POST FROM SORTWS-AREA                       
029200                 PERFORM IMS-GET-SATB11                                   
029300             END-PERFORM                                                  
029400         END-IF                                                           
029500         PERFORM S01-LAES-W22202-POST                                     
029600     END-PERFORM                                                          
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 C-BEHANDLA-INGAENDE-ARTIKLAR SECTION.                                    
030100     SKIP2                                                                
030200     PERFORM S04-SORT-RETURN                                              
030300                                                                          
030400     PERFORM UNTIL EOF-SORTFIL = JA                                       
030500                                                                          
030600       MOVE +1 TO TABIND                                                  
030700       PERFORM UNTIL TABIND > TABIND-MAX                                  
030800         MOVE ZERO TO TAB-STR-IDARTNR(TABIND)                             
030900         ADD +1 TO TABIND                                                 
031000       END-PERFORM                                                        
031100                                                                          
031200       MOVE +0 TO TABIND                                                  
031300                                                                          
031400       IF SORTWS-IDARTNR = SPARA-IDARTNR                                  
031500           CONTINUE                                                       
031600       ELSE                                                               
031700         MOVE SORTWS-IDARTNR TO SPARA-IDARTNR                             
031800                                                                          
031900         MOVE ZERO TO W-PROGN-KVPB-SATS                                   
032000         MOVE ZERO TO WS-KVPB-TOT                                         
032100                                                                          
032200         MOVE SORTWS-IDARTNR  TO W-IDARTNR                                
032300                               W-IDARTNR-S                                
032400         MOVE SPACE            TO W-IDLEVNR-S                             
032500         MOVE SPACE            TO W-BELEVART-S                            
032600                                                                          
032700         PERFORM S97-KOLLA-ING-ART                                        
032800                                                                          
032900         PERFORM IMS-GET-SATB01-2                                         
033000         IF SEGMENT-FINNS                                                 
033100             PERFORM S98-LAGRA-I-STR-TAB                                  
033200         END-IF                                                           
033300       END-IF                                                             
033400       PERFORM S03-BEHANDLA-SATS-I-SATS                                   
033500                                                                          
033600       IF CHKP-ANT > CHKP-MAX                                             
033700         PERFORM X-TAG-CHECKPOINT                                         
033800       END-IF                                                             
033900                                                                          
034000       PERFORM S04-SORT-RETURN                                            
034100                                                                          
034200     END-PERFORM                                                          
034300     .                                                                    
034400     EJECT                                                                
034500 E-AVSLUTA SECTION.                                                       
034600                                                                          
034700     CLOSE  W22202                                                        
034800                                                                          
034900     MOVE 'S' TO POSTSUM-OPKOD                                            
035000     CALL POSTSUM USING POSTSUM-PARM                                      
035100     .                                                                    
035200     EJECT                                                                
035300 S01-LAES-W22202-POST SECTION.                                            
035400                                                                          
035500     READ W22202 INTO I2202-AREA                                          
035600     AT END                                                               
035700         MOVE JA             TO W22202-EOF                                
035800     NOT AT END                                                           
035900         MOVE 'W22202'       TO POSTSUM-FDNAMN                            
036000         MOVE 'W22216D1'     TO POSTSUM-DDNAMN2                           
036100         MOVE I2202-IDHTYP   TO POSTSUM-TRANSTYP                          
036200         CALL POSTSUM USING POSTSUM-PARM                                  
036300     END-READ                                                             
036400     .                                                                    
036500     EJECT                                                                
036600 S03-BEHANDLA-SATS-I-SATS SECTION.                                        
036700******************************************************************        
036800*                                                                *        
036900*    PB-SATS PÅ INGÅENDE ARTIKLAR I SATS-I-SATS BERÄKNAS.        *        
037000*                                                                *        
037100******************************************************************        
037200     SKIP2                                                                
037300     MOVE +1 TO TABIND2                                                   
037400     IF TABIND > 0                                                        
037500       PERFORM UNTIL TABIND2 > TABIND                                     
037600                                                                          
037700         MOVE TAB-STR-IDARTNR(TABIND2) TO W-IDARTNR                       
037800         PERFORM IMS-GET-SATB01                                           
037900         IF SEGMENT-FINNS                                                 
038000            PERFORM IMS-GET-SATB11                                        
038100            PERFORM UNTIL SEGMENT-SAKNAS                                  
038200               MOVE SATB-RAD-IDARTNR TO W-IDARTNR-S                       
038300                                        W-IDARTNR                         
038400               MOVE SPACE            TO W-IDLEVNR-S                       
038500               MOVE SPACE            TO W-BELEVART-S                      
038600                                                                          
038700               PERFORM S97-KOLLA-ING-ART                                  
038800                                                                          
038900               PERFORM IMS-GET-SATB01-2                                   
039000                                                                          
039100               IF SEGMENT-FINNS                                           
039200                  PERFORM S98-LAGRA-I-STR-TAB                             
039300               END-IF                                                     
039400               PERFORM IMS-GET-SATB11                                     
039500            END-PERFORM                                                   
039600         END-IF                                                           
039700         ADD +1 TO TABIND2                                                
039800       END-PERFORM                                                        
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 S04-SORT-RETURN SECTION.                                                 
040300     SKIP2                                                                
040400                                                                          
040500     RETURN SORTFIL INTO SORTWS-AREA                                      
040600     AT END                                                               
040700         MOVE JA             TO EOF-SORTFIL                               
040800     NOT AT END                                                           
040900         MOVE 'SORTFIL'      TO POSTSUM-FDNAMN                            
041000         MOVE 'SORTFIL'      TO POSTSUM-DDNAMN2                           
041100         MOVE 'SORT'         TO POSTSUM-TRANSTYP                          
041200         CALL POSTSUM USING POSTSUM-PARM                                  
041300     END-RETURN                                                           
041400     .                                                                    
041500     EJECT                                                                
041600 S96-UPPDATERA-ARTC SECTION.                                              
041700******************************************************************        
041800*                                                                *        
041900*    PB-SATS INGÅENDE ARTIKEL UPPDATERAS.                        *        
042000*                                                                *        
042100******************************************************************        
042200     SKIP2                                                                
042300     PERFORM IMS-GET-ARTC01                                               
042400     IF SEGMENT-FINNS                                                     
042500        IF ART-KDERS-UTG = 0                                              
042600           PERFORM IMS-GET-ARTC11                                         
042700           MOVE W-PROGN-KVPB-SATS TO CLAG-KVPB-SATS                       
042800           PERFORM IMS-REPL-ARTC                                          
042900           ADD +1 TO CHKP-ANT                                             
043000        END-IF                                                            
043100     END-IF                                                               
043200                                                                          
043300     MOVE ZERO TO W-PROGN-KVPB-SATS                                       
043400                  WS-KVPB-TOT                                             
043500     .                                                                    
043600     EJECT                                                                
043700 S97-KOLLA-ING-ART SECTION.                                               
043800******************************************************************        
043900*                                                                *        
044000*    KONTROLL OM SÖKT ARTIKEL INGÅR SOM AKTUELL RAD I AKTUELL    *        
044100*    1002-SATS. VID TRÄFF BERÄKNAS NYTT KVPB-SATS PÅ ARTIKELN    *        
044200*                                                                *        
044300******************************************************************        
044400     PERFORM IMS-GET-SATB-CSEQ-FIRST                                      
044500     PERFORM UNTIL SEGMENT-SAKNAS                                         
044600       IF SATE-STR-IDARTNR < 100000000                                    
044700         IF SATE-STR-TIBORT = 0                                           
044800           MOVE SATE-STR-IDARTNR TO W-IDARTNR                             
044900           PERFORM S972-KOLLA-ERSATTNING                                  
045000           IF AKTUELL-STRUKTUR = NEJ                                      
045100               CONTINUE                                                   
045200           ELSE                                                           
045300             PERFORM S99-LAS-KVPB-TOT                                     
045400             MOVE SATE-RAD-TISTADAT TO TMP1-YYMMDD                        
045500             MOVE WS-DAGENS-DATUM   TO TMP2-YYMMDD                        
045600             MOVE SATE-RAD-TISTODAT TO TMP3-YYMMDD                        
045700             PERFORM WY2000Q1                                             
045800             IF (    TMP1-YYMMDD <= TMP2-YYMMDD                           
045900                 AND TMP3-YYMMDD > TMP2-YYMMDD )                          
046000                PERFORM S971-RAKNA-OM-KVPBSATS                            
046100             ELSE                                                         
046200               IF SATE-RAD-KDISATS = 'N'                                  
046300                  MOVE SATE-RAD-TISTADAT TO DAT-I-TIDATUM                 
046400                  MOVE 'AAVV  '          TO DAT-KDDATFORM                 
046500                  CALL WDATKONV USING DAT-KDDATFORM                       
046600                                      DAT-I-TIDATUM                       
046700                                      DAT-O-TIDATUM                       
046800                                      DAT-KDSVAR                          
046900                  MOVE DAT-TIAAVV-GRP TO WS-KOLLVECKA                     
047000                  IF WS-KOLLVECKA = W-DAGENS-DATUM-AAVV                   
047100                     PERFORM S971-RAKNA-OM-KVPBSATS                       
047200                  END-IF                                                  
047300               ELSE                                                       
047400                  IF SATE-RAD-KDISATS = 'U'                               
047500                    MOVE SATE-RAD-TISTODAT TO DAT-I-TIDATUM               
047600                    MOVE 'AAVV  '          TO DAT-KDDATFORM               
047700                    CALL WDATKONV USING DAT-KDDATFORM                     
047800                                        DAT-I-TIDATUM                     
047900                                        DAT-O-TIDATUM                     
048000                                        DAT-KDSVAR                        
048100                    MOVE DAT-TIAAVV-GRP TO WS-KOLLVECKA                   
048200                    MOVE WS-KOLLVECKA           TO TMP1-YYWW              
048300                    MOVE W-DAGENS-DATUM-AAVV    TO TMP2-YYWW              
048400                    PERFORM WY2000P3                                      
048500                    IF TMP1-YYWW < TMP2-YYWW                              
048600                       PERFORM S971-RAKNA-OM-KVPBSATS                     
048700                    END-IF                                                
048800                  END-IF                                                  
048900               END-IF                                                     
049000             END-IF                                                       
049100           END-IF                                                         
049200         END-IF                                                           
049300       END-IF                                                             
049400       PERFORM IMS-GET-SATB-CSEQ-NEXT                                     
049500     END-PERFORM                                                          
049600                                                                          
049700     MOVE W-IDARTNR-S TO W-IDARTNR                                        
049800     PERFORM S96-UPPDATERA-ARTC                                           
049900     .                                                                    
050000     EJECT                                                                
050100 S971-RAKNA-OM-KVPBSATS SECTION.                                          
050200******************************************************************        
050300*                                                                *        
050400*    PB-TOT * RADENS ANTAL UPPDATERAR ARTIKELRADENS PB-SATS      *        
050500*                                                                *        
050600******************************************************************        
050700     SKIP2                                                                
050800     COMPUTE W-DIFF-KVPB ROUNDED = WS-KVPB-TOT                            
050900         * SATE-RAD-REANTPSA                                              
051000     ADD W-DIFF-KVPB TO W-PROGN-KVPB-SATS ROUNDED                         
051100                                                                          
051200     .                                                                    
051300     EJECT                                                                
051400 S972-KOLLA-ERSATTNING SECTION.                                           
051500******************************************************************        
051600*                                                                *        
051700*    KONTROLL OM ÖVERLIGGANDE STRUKTUR ÄR 01 11 04 14 09 19 -    *        
051800*    ERSATT                                                               
051900*    SAKNAS BESTÄLLNINGSREST BERÄKNAS INGET PB-SATS PÅ DESS      *        
052000*    INGÅENDE ARTIKLAR.                                          *        
052100******************************************************************        
052200     SKIP2                                                                
052300     MOVE JA TO AKTUELL-STRUKTUR                                          
052400     PERFORM IMS-GET-ARTC01                                               
052500     MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
052600     PERFORM IMS-GET-ARTC11                                               
052700     IF SEGMENT-FINNS                                                     
052800       IF CLAG-KDERS = 01 OR 04 OR 09 OR 11 OR 14 OR                      
052810                       17 OR 18 OR 19                                     
052900           MOVE NEJ TO AKTUELL-STRUKTUR                                   
053000       END-IF                                                             
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 S98-LAGRA-I-STR-TAB SECTION.                                             
053500******************************************************************        
053600*                                                                *        
053700*    LAGRAR HITTAD STRUKTUR I TABELL.                            *        
053800*                                                                *        
053900******************************************************************        
054000                                                                          
054100     ADD  +1 TO TABIND                                                    
054200                                                                          
054300     MOVE SATB-STR-IDARTNR TO TAB-STR-IDARTNR(TABIND)                     
054400     .                                                                    
054500     EJECT                                                                
054600 S99-LAS-KVPB-TOT SECTION.                                                
054700******************************************************************        
054800*                                                                *        
054900*    PB-SEP + PB-SATS + PB-REF-SDC ADDERAS TILL PB-TOT           *        
055000*    FÖR ÖVERLIGGANDE STRUKTURNUMMER                             *        
055100*                                                                *        
055200******************************************************************        
055300     SKIP2                                                                
055400     IF SEGMENT-FINNS                                                     
055500        COMPUTE WS-KVPB-TOT = CLAG-KVPB-SATS +                            
055600             CLAG-KVPB-SEP                                                
055700     END-IF                                                               
055800                                                                          
055900     PERFORM IMS-GET-ARTS01                                               
056000     IF SEGMENT-FINNS                                                     
056100        PERFORM IMS-GET-ARTS11                                            
056200        PERFORM UNTIL SEGMENT-SAKNAS                                      
056300          ADD SLAG-KVPB-REF TO WS-KVPB-TOT                                
056400          PERFORM IMS-GET-ARTS11                                          
056500        END-PERFORM                                                       
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 X-TAG-CHECKPOINT   SECTION.                                              
057000                                                                          
057100     PERFORM IMS-CHECKPOINT                                               
057200     MOVE ZERO TO CHKP-ANT                                                
057300     .                                                                    
057400     EJECT                                                                
057500* IMS SECTIONER                                                           
057600     SKIP3                                                                
057700 IMS-GET-SATB01 SECTION.                                                  
057800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
057900            DELIMITED BY SIZE INTO SSA1                                   
058000     MOVE '  GE' TO GODK-STATUSKODER                                      
058100     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
058200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     SKIP3                                                                
058500     .                                                                    
058600 IMS-GET-SATB01-2 SECTION.                                                
058700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
058800            DELIMITED BY SIZE INTO SSA1                                   
058900     MOVE '  GE' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING GU SATB2-PCB DLI-IO-AREA SSA1                     
059100     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     SKIP3                                                                
059400     .                                                                    
059500 IMS-GET-SATB11 SECTION.                                                  
059600     MOVE 'WLSATB11 ' TO SSA1                                             
059700     MOVE '  GE' TO GODK-STATUSKODER                                      
059800     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
059900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
060100     SKIP3                                                                
060200     .                                                                    
060300 IMS-GET-SATB-CSEQ-FIRST SECTION.                                         
060400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
060500            DELIMITED BY SIZE INTO SSA1                                   
060600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
060700            DELIMITED BY SIZE INTO SSA2                                   
060800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
060900     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
061000     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
061100     PERFORM IMS-STATUSKONTROLL                                           
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
061500     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
061600            DELIMITED BY SIZE INTO SSA1                                   
061700     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
061800            DELIMITED BY SIZE INTO SSA2                                   
061900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
062000     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
062100     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     SKIP3                                                                
062500 IMS-GET-ARTC01 SECTION.                                                  
062600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
062700            DELIMITED BY SIZE INTO SSA1                                   
062800     MOVE '  GE' TO GODK-STATUSKODER                                      
062900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
063000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     SKIP3                                                                
063300     .                                                                    
063400 IMS-GET-ARTC11 SECTION.                                                  
063500     MOVE 'WLARTC11 ' TO SSA1                                             
063600     MOVE '  GE' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-2 SSA1                  
063800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     EJECT                                                                
064100     .                                                                    
064200 IMS-REPL-ARTC SECTION.                                                   
064300     MOVE '  '   TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-2                       
064500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     EJECT                                                                
064800     .                                                                    
064900 IMS-GET-ARTS01 SECTION.                                                  
065000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
065100            DELIMITED BY SIZE INTO SSA1                                   
065200     MOVE '  GE' TO GODK-STATUSKODER                                      
065300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-3 SSA1                    
065400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     SKIP3                                                                
065700     .                                                                    
065800 IMS-GET-ARTS11 SECTION.                                                  
065900     MOVE 'WLARTS11 ' TO SSA1                                             
066000     MOVE '  GE' TO GODK-STATUSKODER                                      
066100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-3 SSA1                   
066200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500     EJECT                                                                
066600 IMS-CHECKPOINT SECTION.                                                  
066700     SKIP2                                                                
066800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
066900     MOVE '  XD' TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING CHKP MSG-PCB                                      
067100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
067200                        CHKP-AREA-LENGTH CHKP-AREA                        
067300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067400     PERFORM IMS-STATUSKONTROLL                                           
067500                                                                          
067600     IF IMS-EJ-OK                                                         
067700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
067800       DISPLAY FELTEXT                                                    
067900       CALL FELLOG                                                        
068000     END-IF                                                               
068100     .                                                                    
068200     EJECT                                                                
068300 IMS-RESTART SECTION.                                                     
068400     SKIP2                                                                
068500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
068600     MOVE '  ' TO GODK-STATUSKODER                                        
068700     CALL CBLTDLI USING XRST MSG-PCB                                      
068800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
068900                        CHKP-AREA-LENGTH CHKP-AREA                        
069000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069100     PERFORM IMS-STATUSKONTROLL                                           
069200     .                                                                    
069300     EJECT                                                                
069400 IMS-STATUSKONTROLL SECTION.                                              
069500     SET STATUS-IX TO 1                                                   
069600     SEARCH GODK-STATUS AT END CALL FELLOG                                
069700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                 
069800     END-SEARCH                                                           
069900     .                                                                    
070000     EJECT                                                                
070100*    -COPY WY2000Q1                                                       
070200     EJECT                                                                
070300*    -COPY WY2000P3                                                       
