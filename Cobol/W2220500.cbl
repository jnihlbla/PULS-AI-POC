000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2220500.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   13/01/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        FAIR DISTRIBUTION FROM REFILL                                    
001000*        W22205  WEEKLY FILE EXTRACT FOR MODEL OF REQUIREMENTS            
001100*        W22205X WEEKLY FILE EXTRACT FOR MODEL OF REQUIREMENTS            
001200*        TO AZURE DATALAKE                                                
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- SYSIN FROM JCL                                             
002300     SELECT INDATA                     ASSIGN TO SYSIN.                   
002400     SKIP2                                                                
002500*          --- W01160 - WDK601 & WDK611                                   
002600     SELECT W01160                     ASSIGN TO W22205D1.                
002700     SKIP2                                                                
002800*          --- W22205 - WEEKLY FILE EXTRACT                               
002900     SELECT W22205                     ASSIGN TO W22205D2.                
003000     SKIP2                                                                
003100*          --- W22205X - WEEKLY FILE EXTRACT                              
003200     SELECT W22205X                    ASSIGN TO W22205D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  INDATA                                                               
003900     LABEL RECORD STANDARD                                                
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200 01  INPOST          PIC X(80).                                           
004300                                                                          
004400 FD  W01160                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  W01160-POST       -COPY W01160      -L.                              
004900     SKIP3                                                                
005000 FD  W22205                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  W22205-POST  -COPY W22205           -L.                              
005500                                                                          
005600 FD  W22205X                                                              
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  W22205X-POST  -COPY W22205X         -L.                              
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300*    -- CHECKED BY WY2000                                                 
006400     SKIP3                                                                
006500*    -COPY WY2000W3                                                       
006600     SKIP3                                                                
006700                                                                          
006800 77  FILLER                      PIC X(8)    VALUE 'WORKAREA'.            
006900 77  IDPGM                       PIC X(8)    VALUE 'W2220500'.            
007000 77  JA                          PIC X       VALUE 'J'.                   
007100 77  NEJ                         PIC X       VALUE 'N'.                   
007200     SKIP2                                                                
007300 01  ERROR-TEXT.                                                          
007400     03  FILLER                  PIC X(8)    VALUE 'ERRORTEX'.            
007500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
007800 01  INAREA.                                                              
007900     03  PARM-EXTR-FIL           PIC X(10).                               
008000     03  FILLER                  PIC X(70).                               
008100                                                                          
008200     SKIP2                                                                
008300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008400 77  LINK-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008500 77  INDX-TREND                  PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  MAX-INDX                    PIC S9(4)  VALUE +20   COMP SYNC.        
008700                                                                          
008800 77  INDX-BEHOV                  PIC S9(4)  VALUE +0    COMP SYNC.        
008900 77  MAX-INDX-BEHOV              PIC S9(4)  VALUE +156  COMP SYNC.        
009000*                                                                         
009100 77  CONST-KVVECKOR-BEHOV        PIC S9(4)  VALUE +19   COMP SYNC.        
009200     SKIP2                                                                
009300                                                                          
009400 77  PART-SW                  PIC X       VALUE 'J'.                      
009500     88  PART-OK                          VALUE 'J'.                      
009600     88  PART-WRONG                       VALUE 'N'.                      
009700                                                                          
009800*    --- ARBETSFÄLT                                                       
009900 01  ARBETSFAELT.                                                         
010000     03  ENDAST-SEPARATBEHOV     PIC  X(2)   VALUE '01'.                  
010100     03  ENDAST-SATSBEHOV        PIC  X(2)   VALUE '02'.                  
010200     03  ENDAST-LEVBEHOV         PIC  X(2)   VALUE '04'.                  
010300     03  ENDAST-SDCBEHOV         PIC  X(2)   VALUE '10'.                  
010400     03  ENDAST-NDCBEHOV         PIC  X(2)   VALUE '15'.                  
010500     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
010600                                 PIC  X(2)   VALUE '19'.                  
010700     03  W-TIAAVV-NUM            PIC   9(4)  VALUE ZERO.                  
010800     03  W-TIAAVV                PIC  S9(5)  VALUE ZERO  COMP-3.          
010900     03  W-TIME                  PIC   9(8)  VALUE ZERO.                  
011000     03  W-VECKO-SEP-BEHOV  PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
011100     03  W-DAG-SEP-BEHOV    PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
011200     03  W-ANTAL-VECKOR          PIC   9(3)   VALUE ZERO COMP-3.          
011300     03  W-KVDAGAR-KVAR          PIC   9(3)   VALUE ZERO COMP-3.          
011400     03  W-TIFINLV-AAVV          PIC S9(5)               COMP-3.          
011500     03  W-FAKTOR                PIC S9(1)V9(3)          COMP-3.          
011600     03  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                   
011700     03  SPAR-KVPB-TOT           PIC S9(8)V9(2) VALUE ZERO COMP-3.        
011800                                                                          
011900     03  WS-CURRENT-DATE.                                                 
012000         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
012100         05  FILLER              PIC 9(4)   VALUE ZERO.                   
012200         05  FILLER              PIC 9(6)   VALUE ZERO.                   
012300                                                                          
012400     03  FILLER REDEFINES WS-CURRENT-DATE.                                
012500*-----   INKLUSIVE SEKEL                                                  
012600         05  WS-DAGENS-DATUM     PIC 9(8).                                
012700         05  WS-DAGENS-TID.                                               
012800             07 WS-DAGENS-TIMME  PIC 9(2).                                
012900             07 WS-DAGENS-MINUT  PIC 9(2).                                
013000             07 WS-DAGENS-SEKUND PIC 9(2).                                
013100                                                                          
013200     03  WS-KVPB-TREND           PIC S9(6)V9.                             
013300     03  MAX-KVVECKOR-TREND      PIC S9(3)  VALUE ZERO.                   
013400                                                                          
013500 01      W-KVPB-TOT-TAB.                                                  
013600     03  W-KVPB-TOT         PIC  S9(8)V9(2)   COMP-3 OCCURS 20.           
013700*                                                                         
013800*                                                                         
013900*                                                                         
014000 01 FILLER                      PIC  X(16) VALUE 'ART-TAB'.               
014100******************************************************************        
014200*TABELL FÖR BEHOV 20 VECKOR FRAM FÖR EN ARTIKEL.                          
014300******************************************************************        
014400*                                                                         
014500 01 ART-TABLE.                                                            
014600    03 ART-TAB-PERIOD OCCURS 20.                                          
014700       05   TAB-IDARTNR          PIC X(9).                                
014800       05   TAB-TIAAVV           PIC 9(4).                                
014900       05   TAB-KVPB-SEP         PIC Z(5)9.9.                             
015000       05   TAB-KVPB-SATS        PIC Z(5)9.9.                             
015100       05   TAB-KVPB-TPO         PIC Z(5)9.9.                             
015200       05   TAB-KVPB-SDC         PIC Z(5)9.9.                             
015300       05   TAB-KVPB-NDC         PIC Z(5)9.9.                             
015400       05   TAB-KVPB-TREND       PIC -(6)9.9.                             
015500       05   TAB-KVPB-PLAN        PIC Z(6)9.9.                             
015600*                                                                         
015700 01    TAB-IX                    PIC S9(3) COMP-3.                        
015800 01    TAB-IX-MAX                PIC S9(3) COMP-3 VALUE +20.              
015900*                                                                         
016000******************************************************************        
016100*                                                                         
016200*      --- VALID IDDC CODES                                               
016300*                                                                         
016400*01    -COPY WWDCKONS                                                     
016500       EJECT                                                              
016600*                                                                         
016700*01    -COPY WWPRODSL                                                     
016800       EJECT                                                              
016900 77  INDATA-EOF                  PIC X       VALUE 'N'.                   
017000 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
017100     88  END-OF-W01160                       VALUE 'Y'.                   
017200     EJECT                                                                
017300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
017400 01  FILLER REDEFINES TODAYS-DATE.                                        
017500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
017600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
017700     03  TODAYS-DATE-DAY         PIC 9(2).                                
017800     EJECT                                                                
017900 01  GENERAL-SUBPROGRAMS.                                                 
018000*                                                                         
018100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
018500     03  W22222                  PIC X(8)    VALUE 'W22222'.              
018600     EJECT                                                                
018700*    *************************************                                
018800*    **  LINK-AREA                      **                                
018900*    **  BEHOVSTABELL                   **                                
019000*    *************************************                                
019100*01  AREA  -COPY W222L222   -PRE LINK-.                                   
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL POSTSUM                                          
019400*                                                                         
019500*01  -COPY W0005   -PRE  POSTSUM-                                         
019600     EJECT                                                                
019700*01  -COPY WDATAREA                                                       
019800     EJECT                                                                
019900* VARIABLES TO SUBPROGRAM W009VADD                                        
020000 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
020100 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
020200     EJECT                                                                
020300 01  CLAG-AREA-START             PIC X(24)   VALUE                        
020400                                             'CLAG-AREA-START'.           
020500     SKIP2                                                                
020600                                                                          
020700*01  AREA     -COPY W01160   -PRE IN-                                     
020800     EJECT                                                                
020900 01  OUT-AREA-START              PIC X(24)   VALUE                        
021000                                             'OUT-AREA-W22205'.           
021100     SKIP2                                                                
021200*01       -COPY W22205                                                    
021300*                                                                         
021400     SKIP2                                                                
021500 01  OUT2-AREA-START              PIC X(24)   VALUE                       
021600                                             'OUT2-AREA-W22205X'.         
021700     SKIP2                                                                
021800*01       -COPY W22205X                                                   
021900*                                                                         
022000     SKIP2                                                                
022100*    ---  DLI INPUT-OUTPUT AREA                                           
022200                                                                          
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600 01  W222-WDK6-PCB               PIC X.                                   
022700 01  W222-ARTS-PCB               PIC X.                                   
022800 01  W222-ARTM-PCB               PIC X.                                   
022900 01  W222-2501-PCB               PIC X.                                   
023000 01  W222-WDB6R-PCB              PIC X.                                   
023100 01  W222-WDK7R-PCB              PIC X.                                   
023200 01  W222-WDB6-PCB               PIC X.                                   
023300 01  W222-WDD7-PCB               PIC X.                                   
023400 01  W222-WDK7E-PCB              PIC X.                                   
023500 01  W222-UTIL-WDK6-PCB          PIC X.                                   
023600 01  W222-UTIL-WDK7-PCB          PIC X.                                   
023700 01  W222-UTIL-WDB6-PCB          PIC X.                                   
023800 01  W222-UTUP-WDK7-PCB          PIC X.                                   
023900 01  W222-UTUP-WDB6-PCB          PIC X.                                   
024000 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
024100 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
024200 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
024300     EJECT                                                                
024400                                                                          
024500 PROCEDURE DIVISION  USING W222-WDK6-PCB  W222-ARTS-PCB                   
024600                           W222-ARTM-PCB  W222-2501-PCB                   
024700                           W222-WDB6R-PCB W222-WDK7R-PCB                  
024800                           W222-WDB6-PCB  W222-WDD7-PCB                   
024900                           W222-WDK7E-PCB                                 
025000                           W222-UTIL-WDK6-PCB                             
025100                           W222-UTIL-WDK7-PCB                             
025200                           W222-UTIL-WDB6-PCB                             
025300                           W222-UTUP-WDK7-PCB                             
025400                           W222-UTUP-WDB6-PCB                             
025500                           W222-UTUP-UTIL-WDK6-PCB                        
025600                           W222-UTUP-UTIL-WDK7-PCB                        
025700                           W222-UTUP-UTIL-WDB6-PCB                        
025800                           .                                              
025900 MAIN SECTION.                                                            
026000     ENTRY 'DLITCBL' USING W222-WDK6-PCB  W222-ARTS-PCB                   
026100                           W222-ARTM-PCB  W222-2501-PCB                   
026200                           W222-WDB6R-PCB W222-WDK7R-PCB                  
026300                           W222-WDB6-PCB  W222-WDD7-PCB                   
026400                           W222-WDK7E-PCB                                 
026500                           W222-UTIL-WDK6-PCB                             
026600                           W222-UTIL-WDK7-PCB                             
026700                           W222-UTIL-WDB6-PCB                             
026800                           W222-UTUP-WDK7-PCB                             
026900                           W222-UTUP-WDB6-PCB                             
027000                           W222-UTUP-UTIL-WDK6-PCB                        
027100                           W222-UTUP-UTIL-WDK7-PCB                        
027200                           W222-UTUP-UTIL-WDB6-PCB                        
027300                           .                                              
027400                                                                          
027500     PERFORM A-INIT                                                       
027600     PERFORM S01-READ-W01160                                              
027700     PERFORM UNTIL END-OF-W01160                                          
027800                                                                          
027900       PERFORM B-LAES-WRITE-INFO                                          
028000                                                                          
028100       PERFORM S01-READ-W01160                                            
028200     END-PERFORM                                                          
028300                                                                          
028400                                                                          
028500     PERFORM Z-FINIT                                                      
028600                                                                          
028700     MOVE ZERO TO RETURN-CODE                                             
028800     GOBACK                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 A-INIT SECTION.                                                          
029200     SKIP2                                                                
029300                                                                          
029400     OPEN INPUT W01160                                                    
029500                                                                          
029600     OPEN OUTPUT W22205                                                   
029700                 W22205X                                                  
029800                                                                          
029900     OPEN INPUT INDATA                                                    
030000     READ INDATA NEXT RECORD INTO INAREA                                  
030100       AT END MOVE JA        TO INDATA-EOF                                
030200     END-READ                                                             
030300     CLOSE INDATA                                                         
030400                                                                          
030500     MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE                       
030600                                                                          
030700     MOVE 'IDAG'             TO DAT-KDDATFORM                             
030800                                                                          
030900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
031000                         DAT-O-TIDATUM DAT-KDSVAR                         
031100                                                                          
031200     IF DAT-KDSVAR-OK                                                     
031300        MOVE 1                   TO W-ANTAL-VECKOR                        
031400                                                                          
031500        MOVE DAT-TIAAVV-GRP      TO W-TIAAVV-NUM                          
031600        MOVE W-TIAAVV-NUM        TO LINK-TIAAVV-AKTUELL                   
031700                                    LINK-TIBEHOV-START                    
031800        CALL W009VADD USING    LINK-TIBEHOV-START  W-ANTAL-VECKOR         
031900        MOVE DAT-TID             TO LINK-TID-AKTUELL                      
032000        MOVE SPACE               TO LINK-IDDC                             
032100     ELSE                                                                 
032200        CALL FELLOG                                                       
032300     END-IF                                                               
032400                                                                          
032500     MOVE +1                        TO INDX                               
032600     PERFORM UNTIL INDX             >  MAX-INDX                           
032700        MOVE ZERO                   TO W-KVPB-TOT(INDX)                   
032800        ADD +1                      TO INDX                               
032900     END-PERFORM                                                          
033000                                                                          
033100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033200     .                                                                    
033300     EJECT                                                                
033400 B-LAES-WRITE-INFO SECTION.                                               
033500                                                                          
033600       PERFORM BA-CHECK-PART                                              
033700       IF PART-OK                                                         
033800         PERFORM BB-VECKONR                                               
033900         PERFORM BC-SEPARAT-BEHOV                                         
034000         PERFORM BD-SATS-BEHOV                                            
034100         PERFORM BE-TPO-BEHOV                                             
034200         PERFORM BF-SDC-BEHOV                                             
034300         PERFORM BG-NDC-BEHOV                                             
034400         PERFORM BI-PBPLAN                                                
034500         PERFORM BJ-TREND                                                 
034600         PERFORM BK-WRITE-TO-FILE                                         
034700       END-IF                                                             
034800     .                                                                    
034900     EJECT                                                                
035000 BA-CHECK-PART     SECTION.                                               
035100                                                                          
035200     MOVE NEJ       TO PART-SW                                            
035300                                                                          
035400     IF IN-CLAG-KDERS-UTG = 0                                             
035500       MOVE IN-CLAG-KDPRODSL     TO TEST-KDPRODSL                         
035600       IF KDPRODSL-VOLVO-BIMA                                             
035700                                                                          
035800         IF IN-CLAG-KDERS < 11                                            
035900           IF IN-CLAG-PRARTSTD > 0                                        
036000                                                                          
036100             IF (PARM-EXTR-FIL = 'EXTRFIL1' AND                           
036200                 IN-CLAG-KVPB-SEP > 0)                                    
036300             OR (PARM-EXTR-FIL = 'EXTRFIL2')                              
036400                                                                          
036500               PERFORM BAA-CLEAR-TABLE                                    
036600                                                                          
036700               MOVE IN-CLAG-IDARTNR TO LINK-IDARTNR                       
036800               MOVE JA           TO PART-SW                               
036900             ELSE                                                         
037000               MOVE NEJ          TO PART-SW                               
037100             END-IF                                                       
037200           END-IF                                                         
037300         END-IF                                                           
037400       END-IF                                                             
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 BAA-CLEAR-TABLE   SECTION.                                               
038000                                                                          
038100     MOVE +1                    TO TAB-IX                                 
038200     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
038300       MOVE IN-CLAG-IDARTNR     TO TAB-IDARTNR(TAB-IX)                    
038400       MOVE ZERO                TO TAB-TIAAVV (TAB-IX)                    
038500       MOVE ZERO                TO TAB-KVPB-SEP (TAB-IX)                  
038600       MOVE ZERO                TO TAB-KVPB-SATS (TAB-IX)                 
038700       MOVE ZERO                TO TAB-KVPB-TPO (TAB-IX)                  
038800       MOVE ZERO                TO TAB-KVPB-SDC (TAB-IX)                  
038900       MOVE ZERO                TO TAB-KVPB-NDC (TAB-IX)                  
039000       MOVE ZERO                TO TAB-KVPB-TREND(TAB-IX)                 
039100       MOVE ZERO                TO TAB-KVPB-PLAN (TAB-IX)                 
039200       ADD +1                   TO TAB-IX                                 
039300     END-PERFORM                                                          
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700 BB-VECKONR        SECTION.                                               
039800                                                                          
039900     MOVE 1                    TO W-ANTAL-VECKOR                          
040000     MOVE +1                   TO TAB-IX                                  
040100     MOVE W-TIAAVV-NUM         TO TAB-TIAAVV(TAB-IX)                      
040200     ADD +1                    TO TAB-IX                                  
040300     MOVE LINK-TIBEHOV-START   TO TAB-TIAAVV(TAB-IX)                      
040400                                  W-TIAAVV                                
040500                                                                          
040600     ADD +1                    TO TAB-IX                                  
040700     PERFORM UNTIL TAB-IX      >  MAX-INDX                                
040800                                                                          
040900        CALL W009VADD USING W-TIAAVV W-ANTAL-VECKOR                       
041000        MOVE W-TIAAVV          TO TAB-TIAAVV(TAB-IX)                      
041100        ADD +1                 TO TAB-IX                                  
041200     END-PERFORM                                                          
041300                                                                          
041400     .                                                                    
041500     EJECT                                                                
041600 BC-SEPARAT-BEHOV  SECTION.                                               
041700                                                                          
041800     PERFORM BCA-SEP-BEHOV-INNEV-VECKA                                    
041900     PERFORM BCB-SEP-BEHOV-OVRIGA-VECKOR                                  
042000                                                                          
042100     .                                                                    
042200     EJECT                                                                
042300 BCA-SEP-BEHOV-INNEV-VECKA  SECTION.                                      
042400                                                                          
042500     MOVE +1                   TO TAB-IX                                  
042600     ACCEPT W-TIME             FROM TIME                                  
042700     COMPUTE W-VECKO-SEP-BEHOV ROUNDED  =  IN-CLAG-KVPB-SEP / 4.33        
042800     COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =  W-VECKO-SEP-BEHOV / 5          
042900     DIVIDE IN-CLAG-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                   
043000     MOVE W-TIAAVV-NUM         TO TMP1-YYWW                               
043100     MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                               
043200     PERFORM WY2000P3                                                     
043300                                                                          
043400     IF  DAT-TID = 6 OR                                                   
043500         DAT-TID = 7 OR                                                   
043600        (DAT-TID = 5 AND W-TIME(1:4) > 1700)                              
043700     OR  TMP1-YYWW < TMP2-YYWW                                            
043800         MOVE ZERO              TO TAB-KVPB-SEP(TAB-IX)                   
043900     ELSE                                                                 
044000         COMPUTE W-KVDAGAR-KVAR =  5 - DAT-TID                            
044100         IF W-TIME(1:4)         <= 1700                                   
044200            ADD +1              TO W-KVDAGAR-KVAR                         
044300         END-IF                                                           
044400                                                                          
044500         MOVE +1                TO W-FAKTOR                               
044600         SUBTRACT IN-CLAG-REDIRLEV FROM W-FAKTOR                          
044700                                                                          
044800         COMPUTE TAB-KVPB-SEP(TAB-IX) = W-DAG-SEP-BEHOV *                 
044900                                      W-KVDAGAR-KVAR  *                   
045000                                      W-FAKTOR                            
045100                                                                          
045200         COMPUTE W-KVPB-TOT(TAB-IX) = (W-DAG-SEP-BEHOV *                  
045300                                       W-KVDAGAR-KVAR  *                  
045400                                       W-FAKTOR)         +                
045500                                       W-KVPB-TOT(TAB-IX)                 
045600     END-IF                                                               
045700                                                                          
045800     .                                                                    
045900     EJECT                                                                
046000 BCB-SEP-BEHOV-OVRIGA-VECKOR SECTION.                                     
046100                                                                          
046200     MOVE CONST-KVVECKOR-BEHOV  TO LINK-KVVECKOR-BEHOV                    
046300     MOVE ENDAST-SEPARATBEHOV   TO LINK-KDBEHOV                           
046400     MOVE NEJ                   TO LINK-FLINKLDIRLEV                      
046500     CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB             
046600                                 W222-ARTM-PCB  W222-2501-PCB             
046700                                 W222-WDB6R-PCB W222-WDK7R-PCB            
046800                                 W222-WDB6-PCB  W222-WDD7-PCB             
046900                                 W222-WDK7E-PCB                           
047000                                 W222-UTIL-WDK6-PCB                       
047100                                 W222-UTIL-WDK7-PCB                       
047200                                 W222-UTIL-WDB6-PCB                       
047300                                 W222-UTUP-WDK7-PCB                       
047400                                 W222-UTUP-WDB6-PCB                       
047500                                 W222-UTUP-UTIL-WDK6-PCB                  
047600                                 W222-UTUP-UTIL-WDK7-PCB                  
047700                                 W222-UTUP-UTIL-WDB6-PCB                  
047800                                                                          
047900     IF LINK-ANROP-FEL                                                    
048000        PERFORM S04-NOLLA-W22222                                          
048100     END-IF                                                               
048200                                                                          
048300     MOVE +2                             TO TAB-IX                        
048400     MOVE +1                             TO LINK-INDX                     
048500     PERFORM UNTIL TAB-IX                >  MAX-INDX                      
048600      MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                                  
048700        TO TAB-KVPB-SEP(TAB-IX)                                           
048800      ADD  LINK-KVBEHOV-VECKA(LINK-INDX)                                  
048900        TO W-KVPB-TOT(TAB-IX)                                             
049000      ADD +1                           TO TAB-IX                          
049100                                            LINK-INDX                     
049200     END-PERFORM                                                          
049300     .                                                                    
049400     EJECT                                                                
049500 BD-SATS-BEHOV  SECTION.                                                  
049600                                                                          
049700     IF IN-CLAG-FLIART             =  JA                                  
049800        MOVE CONST-KVVECKOR-BEHOV  TO LINK-KVVECKOR-BEHOV                 
049900        MOVE ENDAST-SATSBEHOV      TO LINK-KDBEHOV                        
050000        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
050100        CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB          
050200                                    W222-ARTM-PCB  W222-2501-PCB          
050300                                    W222-WDB6R-PCB W222-WDK7R-PCB         
050400                                    W222-WDB6-PCB  W222-WDD7-PCB          
050500                                    W222-WDK7E-PCB                        
050600                                    W222-UTIL-WDK6-PCB                    
050700                                    W222-UTIL-WDK7-PCB                    
050800                                    W222-UTIL-WDB6-PCB                    
050900                                    W222-UTUP-WDK7-PCB                    
051000                                    W222-UTUP-WDB6-PCB                    
051100                                    W222-UTUP-UTIL-WDK6-PCB               
051200                                    W222-UTUP-UTIL-WDK7-PCB               
051300                                    W222-UTUP-UTIL-WDB6-PCB               
051400        IF LINK-ANROP-FEL                                                 
051500           PERFORM S04-NOLLA-W22222                                       
051600        END-IF                                                            
051700                                                                          
051800        MOVE +1                     TO TAB-IX                             
051900        MOVE LINK-KVBEHOV-DESSUTOM  TO TAB-KVPB-SATS(TAB-IX)              
052000        ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(TAB-IX)                 
052100                                                                          
052200        MOVE +2                     TO TAB-IX                             
052300        MOVE +1                     TO LINK-INDX                          
052400        PERFORM UNTIL TAB-IX              >  MAX-INDX                     
052500          MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                              
052600            TO TAB-KVPB-SATS(TAB-IX)                                      
052700         ADD LINK-KVBEHOV-VECKA(LINK-INDX)                                
052800          TO W-KVPB-TOT(TAB-IX)                                           
052900         ADD +1                         TO TAB-IX                         
053000                                             LINK-INDX                    
053100        END-PERFORM                                                       
053200     ELSE                                                                 
053300        MOVE +1                     TO TAB-IX                             
053400        PERFORM UNTIL TAB-IX        >  MAX-INDX                           
053500           MOVE ZERO                TO TAB-KVPB-SATS(TAB-IX)              
053600           ADD +1                   TO TAB-IX                             
053700        END-PERFORM                                                       
053800     END-IF                                                               
053900                                                                          
054000     .                                                                    
054100     EJECT                                                                
054200 BE-TPO-BEHOV  SECTION.                                                   
054300                                                                          
054400     MOVE CONST-KVVECKOR-BEHOV     TO LINK-KVVECKOR-BEHOV                 
054500     MOVE ENDAST-LEVBEHOV          TO LINK-KDBEHOV                        
054600     MOVE NEJ                      TO LINK-FLINKLDIRLEV                   
054700     CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB             
054800                                 W222-ARTM-PCB  W222-2501-PCB             
054900                                 W222-WDB6R-PCB W222-WDK7R-PCB            
055000                                 W222-WDB6-PCB  W222-WDD7-PCB             
055100                                 W222-WDK7E-PCB                           
055200                                 W222-UTIL-WDK6-PCB                       
055300                                 W222-UTIL-WDK7-PCB                       
055400                                 W222-UTIL-WDB6-PCB                       
055500                                 W222-UTUP-WDK7-PCB                       
055600                                 W222-UTUP-WDB6-PCB                       
055700                                 W222-UTUP-UTIL-WDK6-PCB                  
055800                                 W222-UTUP-UTIL-WDK7-PCB                  
055900                                 W222-UTUP-UTIL-WDB6-PCB                  
056000     IF LINK-ANROP-FEL                                                    
056100        PERFORM S04-NOLLA-W22222                                          
056200     END-IF                                                               
056300                                                                          
056400     MOVE +1                        TO TAB-IX                             
056500     MOVE LINK-KVBEHOV-DESSUTOM     TO TAB-KVPB-TPO(TAB-IX)               
056600     ADD  LINK-KVBEHOV-DESSUTOM     TO W-KVPB-TOT(TAB-IX)                 
056700                                                                          
056800     MOVE +2                        TO TAB-IX                             
056900     MOVE +1                        TO LINK-INDX                          
057000                                                                          
057100     PERFORM UNTIL TAB-IX                >  MAX-INDX                      
057200       MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                                 
057300         TO TAB-KVPB-TPO(TAB-IX)                                          
057400       ADD LINK-KVBEHOV-VECKA(LINK-INDX) TO W-KVPB-TOT(TAB-IX)            
057500       ADD +1                           TO TAB-IX                         
057600                                            LINK-INDX                     
057700     END-PERFORM                                                          
057800     .                                                                    
057900     EJECT                                                                
058000 BF-SDC-BEHOV  SECTION.                                                   
058100                                                                          
058200     IF IN-CLAG-FLREFILL           =  JA                                  
058300        MOVE CONST-KVVECKOR-BEHOV  TO LINK-KVVECKOR-BEHOV                 
058400        MOVE ENDAST-SDCBEHOV       TO LINK-KDBEHOV                        
058500        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
058600        CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB          
058700                                    W222-ARTM-PCB  W222-2501-PCB          
058800                                    W222-WDB6R-PCB W222-WDK7R-PCB         
058900                                    W222-WDB6-PCB  W222-WDD7-PCB          
059000                                    W222-WDK7E-PCB                        
059100                                    W222-UTIL-WDK6-PCB                    
059200                                    W222-UTIL-WDK7-PCB                    
059300                                    W222-UTIL-WDB6-PCB                    
059400                                    W222-UTUP-WDK7-PCB                    
059500                                    W222-UTUP-WDB6-PCB                    
059600                                    W222-UTUP-UTIL-WDK6-PCB               
059700                                    W222-UTUP-UTIL-WDK7-PCB               
059800                                    W222-UTUP-UTIL-WDB6-PCB               
059900        IF LINK-ANROP-FEL                                                 
060000           PERFORM S04-NOLLA-W22222                                       
060100        END-IF                                                            
060200                                                                          
060300        MOVE +1                     TO TAB-IX                             
060400        MOVE LINK-KVBEHOV-DESSUTOM  TO TAB-KVPB-SDC(TAB-IX)               
060500        ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(TAB-IX)                 
060600                                                                          
060700        MOVE +2                            TO TAB-IX                      
060800        MOVE +1                            TO LINK-INDX                   
060900        PERFORM UNTIL TAB-IX               >  MAX-INDX                    
061000          MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                              
061100            TO TAB-KVPB-SDC(TAB-IX)                                       
061200          ADD LINK-KVBEHOV-VECKA(LINK-INDX)                               
061300           TO W-KVPB-TOT(TAB-IX)                                          
061400          ADD +1                          TO TAB-IX                       
061500                                              LINK-INDX                   
061600        END-PERFORM                                                       
061700     ELSE                                                                 
061800        MOVE +1                     TO TAB-IX                             
061900        PERFORM UNTIL TAB-IX        >  MAX-INDX                           
062000           MOVE ZERO                TO TAB-KVPB-SDC(TAB-IX)               
062100           ADD +1                   TO TAB-IX                             
062200        END-PERFORM                                                       
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 BG-NDC-BEHOV  SECTION.                                                   
062700                                                                          
062800      MOVE CONST-KVVECKOR-BEHOV    TO LINK-KVVECKOR-BEHOV                 
062900      MOVE ENDAST-NDCBEHOV         TO LINK-KDBEHOV                        
063000      MOVE NEJ                     TO LINK-FLINKLDIRLEV                   
063100      CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB            
063200                                  W222-ARTM-PCB  W222-2501-PCB            
063300                                  W222-WDB6R-PCB W222-WDK7R-PCB           
063400                                  W222-WDB6-PCB  W222-WDD7-PCB            
063500                                  W222-WDK7E-PCB                          
063600                                  W222-UTIL-WDK6-PCB                      
063700                                  W222-UTIL-WDK7-PCB                      
063800                                  W222-UTIL-WDB6-PCB                      
063900                                  W222-UTUP-WDK7-PCB                      
064000                                  W222-UTUP-WDB6-PCB                      
064100                                  W222-UTUP-UTIL-WDK6-PCB                 
064200                                  W222-UTUP-UTIL-WDK7-PCB                 
064300                                  W222-UTUP-UTIL-WDB6-PCB                 
064400      IF LINK-ANROP-FEL                                                   
064500         PERFORM S04-NOLLA-W22222                                         
064600      END-IF                                                              
064700                                                                          
064800      MOVE +1                       TO TAB-IX                             
064900      MOVE LINK-KVBEHOV-DESSUTOM    TO TAB-KVPB-NDC(TAB-IX)               
065000      ADD  LINK-KVBEHOV-DESSUTOM    TO W-KVPB-TOT(TAB-IX)                 
065100                                                                          
065200      MOVE +2                              TO TAB-IX                      
065300      MOVE +1                              TO LINK-INDX                   
065400      PERFORM UNTIL TAB-IX                 >  MAX-INDX                    
065500       MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                                 
065600         TO TAB-KVPB-NDC(TAB-IX)                                          
065700       ADD LINK-KVBEHOV-VECKA(LINK-INDX)                                  
065800        TO W-KVPB-TOT(TAB-IX)                                             
065900       ADD +1                            TO TAB-IX                        
066000                                            LINK-INDX                     
066100      END-PERFORM                                                         
066200     .                                                                    
066300     EJECT                                                                
066400 BI-PBPLAN  SECTION.                                                      
066500                                                                          
066600     IF ((IN-CLAG-DAPBPLAN > WS-DAGENS-DATUM OR                           
066700        IN-CLAG-DAPBPLAN = WS-DAGENS-DATUM)OR                             
066800       (IN-CLAG-DASEASON > WS-DAGENS-DATUM OR                             
066900        IN-CLAG-DASEASON = WS-DAGENS-DATUM))                              
067000        MOVE CONST-KVVECKOR-BEHOV  TO LINK-KVVECKOR-BEHOV                 
067100        MOVE SEP-SATS-TPO-LEV-SDC-NDC                                     
067200                                   TO LINK-KDBEHOV                        
067300        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
067400        CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-ARTS-PCB          
067500                                    W222-ARTM-PCB  W222-2501-PCB          
067600                                    W222-WDB6R-PCB W222-WDK7R-PCB         
067700                                    W222-WDB6-PCB  W222-WDD7-PCB          
067800                                    W222-WDK7E-PCB                        
067900                                    W222-UTIL-WDK6-PCB                    
068000                                    W222-UTIL-WDK7-PCB                    
068100                                    W222-UTIL-WDB6-PCB                    
068200                                    W222-UTUP-WDK7-PCB                    
068300                                    W222-UTUP-WDB6-PCB                    
068400                                    W222-UTUP-UTIL-WDK6-PCB               
068500                                    W222-UTUP-UTIL-WDK7-PCB               
068600                                    W222-UTUP-UTIL-WDB6-PCB               
068700        IF LINK-ANROP-FEL                                                 
068800           PERFORM S04-NOLLA-W22222                                       
068900        END-IF                                                            
069000                                                                          
069100        MOVE +1                     TO TAB-IX                             
069200                                                                          
069300        ACCEPT W-TIME               FROM TIME                             
069400      COMPUTE W-VECKO-SEP-BEHOV ROUNDED = IN-CLAG-KVPB-SEP / 4.33         
069500        COMPUTE W-DAG-SEP-BEHOV ROUNDED =  W-VECKO-SEP-BEHOV / 5          
069600                                                                          
069700        IF DAT-TID = 6 OR                                                 
069800            DAT-TID = 7 OR                                                
069900           (DAT-TID = 5 AND W-TIME(1:4) > 1700)                           
070000            CONTINUE                                                      
070100        ELSE                                                              
070200            COMPUTE W-KVDAGAR-KVAR = 5 - DAT-TID                          
070300            IF W-TIME(1:4)      <= 1700                                   
070400               ADD +1           TO W-KVDAGAR-KVAR                         
070500            END-IF                                                        
070600                                                                          
070700            MOVE +1 TO W-FAKTOR                                           
070800            SUBTRACT IN-CLAG-REDIRLEV FROM W-FAKTOR                       
070900            COMPUTE LINK-KVBEHOV-DESSUTOM ROUNDED =                       
071000                    LINK-KVBEHOV-DESSUTOM +                               
071100                                        (W-DAG-SEP-BEHOV *                
071200                                         W-KVDAGAR-KVAR  *                
071300                                         W-FAKTOR)                        
071400        END-IF                                                            
071500        MOVE LINK-KVBEHOV-DESSUTOM  TO TAB-KVPB-PLAN(TAB-IX)              
071600                                                                          
071700        MOVE +2                            TO TAB-IX                      
071800        MOVE +1                            TO LINK-INDX                   
071900        PERFORM UNTIL TAB-IX               >  MAX-INDX                    
072000          MOVE LINK-KVBEHOV-VECKA(LINK-INDX)                              
072100            TO TAB-KVPB-PLAN(TAB-IX)                                      
072200          ADD +1                          TO TAB-IX                       
072300                                              LINK-INDX                   
072400        END-PERFORM                                                       
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800 BJ-TREND   SECTION.                                                      
072900                                                                          
073000      IF IN-CLAG-KVPB-TREND NOT = ZERO                                    
073100         MOVE IN-CLAG-KVVECKOR-TREND TO MAX-KVVECKOR-TREND                
073200         MOVE +1                       TO TAB-IX                          
073300         MOVE +1                       TO INDX-TREND                      
073400         PERFORM UNTIL TAB-IX          >  MAX-INDX                        
073500            PERFORM BJA-TRENDBER                                          
073600            COMPUTE SPAR-KVPB-TOT ROUNDED =                               
073700               W-KVPB-TOT (TAB-IX) / 2                                    
073800            ADD    WS-KVPB-TREND   TO W-KVPB-TOT (TAB-IX)                 
073900*                ÄR DET RÄTT TECKEN ??????                                
074000            IF W-KVPB-TOT (TAB-IX) < SPAR-KVPB-TOT                        
074100               MOVE SPAR-KVPB-TOT TO W-KVPB-TOT (TAB-IX)                  
074200            END-IF                                                        
074300            ADD +1             TO TAB-IX                                  
074400            ADD +1             TO INDX-TREND                              
074500         END-PERFORM                                                      
074600      ELSE                                                                
074700         MOVE +1                       TO TAB-IX                          
074800         PERFORM UNTIL TAB-IX          >  MAX-INDX                        
074900            MOVE ZERO          TO TAB-KVPB-TREND (TAB-IX)                 
075000            ADD +1             TO TAB-IX                                  
075100         END-PERFORM                                                      
075200      END-IF                                                              
075300     .                                                                    
075400     EJECT                                                                
075500 BJA-TRENDBER SECTION.                                                    
075600                                                                          
075700******************************************************************        
075800*                                                                *        
075900*    BERÄKNING AV TRENDBEHOV                                     *        
076000*                                                                *        
076100******************************************************************        
076200                                                                          
076300     IF DAT-TID > 5 OR (DAT-TID = 5 AND WS-DAGENS-TIMME > 17)             
076400***     VECKOHELG ?                                                       
076500        IF INDX-TREND NOT < MAX-KVVECKOR-TREND                            
076600           MOVE MAX-KVVECKOR-TREND TO INDX-TREND                          
076700        END-IF                                                            
076800        COMPUTE WS-KVPB-TREND ROUNDED =                                   
076900               INDX-TREND * IN-CLAG-KVPB-TREND / 4.33                     
077000        END-COMPUTE                                                       
077100        MOVE       WS-KVPB-TREND       TO                                 
077200                TAB-KVPB-TREND (TAB-IX)                                   
077300     ELSE                                                                 
077400***     MITT I VECKAN                                                     
077500        IF INDX-TREND NOT < MAX-KVVECKOR-TREND                            
077600           MOVE MAX-KVVECKOR-TREND TO INDX-TREND                          
077700        END-IF                                                            
077800        COMPUTE WS-KVPB-TREND ROUNDED =                                   
077900               INDX-TREND * IN-CLAG-KVPB-TREND / 4.33                     
078000        END-COMPUTE                                                       
078100        IF TAB-IX = 1                                                     
078200           IF DAT-TID = 2                                                 
078300              COMPUTE WS-KVPB-TREND ROUNDED =                             
078400                      WS-KVPB-TREND * 0.80                                
078500           END-IF                                                         
078600           IF DAT-TID = 3                                                 
078700              COMPUTE WS-KVPB-TREND ROUNDED =                             
078800                      WS-KVPB-TREND * 0.60                                
078900           END-IF                                                         
079000           IF DAT-TID = 4                                                 
079100              COMPUTE WS-KVPB-TREND ROUNDED =                             
079200                      WS-KVPB-TREND * 0.40                                
079300           END-IF                                                         
079400           IF DAT-TID = 5                                                 
079500              COMPUTE WS-KVPB-TREND ROUNDED =                             
079600                      WS-KVPB-TREND * 0.20                                
079700           END-IF                                                         
079800        END-IF                                                            
079900        MOVE       WS-KVPB-TREND       TO                                 
080000                TAB-KVPB-TREND (TAB-IX)                                   
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 BK-WRITE-TO-FILE     SECTION.                                            
080500     MOVE +1                        TO TAB-IX                             
080600     PERFORM UNTIL TAB-IX           >  MAX-INDX                           
080700       MOVE TAB-IDARTNR    (TAB-IX) TO OUT-IDARTNR                        
080800       MOVE TAB-TIAAVV     (TAB-IX) TO OUT-TIAAVV                         
080900       MOVE TAB-KVPB-SEP   (TAB-IX) TO OUT-KVPB-SEP                       
081000       MOVE TAB-KVPB-SATS  (TAB-IX) TO OUT-KVPB-SATS                      
081100       MOVE TAB-KVPB-TPO   (TAB-IX) TO OUT-KVPB-TPO                       
081200       MOVE TAB-KVPB-SDC   (TAB-IX) TO OUT-KVPB-SDC                       
081300       MOVE TAB-KVPB-NDC   (TAB-IX) TO OUT-KVPB-NDC                       
081400       MOVE TAB-KVPB-TREND (TAB-IX) TO OUT-KVPB-TREND                     
081500       MOVE TAB-KVPB-PLAN  (TAB-IX) TO OUT-KVPB-PLAN                      
081600       PERFORM S11-WRITE-W22205                                           
081700                                                                          
081800       MOVE TAB-IDARTNR    (TAB-IX) TO OUT2-IDARTNR                       
081900       MOVE TAB-TIAAVV     (TAB-IX) TO OUT2-TIAAVV                        
082000       MOVE TAB-KVPB-SEP   (TAB-IX) TO OUT2-KVPB-SEP                      
082100       MOVE TAB-KVPB-SATS  (TAB-IX) TO OUT2-KVPB-SATS                     
082200       MOVE TAB-KVPB-TPO   (TAB-IX) TO OUT2-KVPB-TPO                      
082300       MOVE TAB-KVPB-SDC   (TAB-IX) TO OUT2-KVPB-SDC                      
082400       MOVE TAB-KVPB-NDC   (TAB-IX) TO OUT2-KVPB-NDC                      
082500       MOVE TAB-KVPB-TREND (TAB-IX) TO OUT2-KVPB-TREND                    
082600       MOVE TAB-KVPB-PLAN  (TAB-IX) TO OUT2-KVPB-PLAN                     
082700       PERFORM S12-WRITE-W22205X                                          
082800                                                                          
082900       ADD +1                       TO TAB-IX                             
083000     END-PERFORM                                                          
083100                                                                          
083200     .                                                                    
083300     EJECT                                                                
083400 BL-CLEAR-TABLE       SECTION.                                            
083500     MOVE +1                        TO TAB-IX                             
083600     PERFORM UNTIL TAB-IX           >  MAX-INDX                           
083700       MOVE ZERO                    TO TAB-IDARTNR   (TAB-IX)             
083800       MOVE ZERO                    TO TAB-TIAAVV    (TAB-IX)             
083900       MOVE ZERO                    TO TAB-KVPB-SEP  (TAB-IX)             
084000       MOVE ZERO                    TO TAB-KVPB-SATS (TAB-IX)             
084100       MOVE ZERO                    TO TAB-KVPB-TPO  (TAB-IX)             
084200       MOVE ZERO                    TO TAB-KVPB-SDC  (TAB-IX)             
084300       MOVE ZERO                    TO TAB-KVPB-NDC  (TAB-IX)             
084400       MOVE ZERO                    TO TAB-KVPB-TREND(TAB-IX)             
084500       MOVE ZERO                    TO TAB-KVPB-PLAN (TAB-IX)             
084600       ADD +1                       TO TAB-IX                             
084700     END-PERFORM                                                          
084800                                                                          
084900     .                                                                    
085000     EJECT                                                                
085100 Z-FINIT SECTION.                                                         
085200                                                                          
085300                                                                          
085400     CLOSE W01160                                                         
085500                                                                          
085600     CLOSE W22205                                                         
085700           W22205X                                                        
085800     SKIP2                                                                
085900     MOVE 'S' TO POSTSUM-OPKOD                                            
086000     CALL POSTSUM USING POSTSUM-PARM                                      
086100     .                                                                    
086200     EJECT                                                                
086300 S01-READ-W01160  SECTION.                                                
086400     SKIP2                                                                
086500     READ W01160 INTO IN-AREA                                             
086600     AT END                                                               
086700*       MOVE HIGH-VALUE TO CLAG-ID                                        
086800        SET END-OF-W01160 TO TRUE                                         
086900                                                                          
087000     NOT AT END                                                           
087100        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
087200        MOVE 'W22205D1' TO POSTSUM-DDNAMN2                                
087300        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
087400        CALL POSTSUM USING POSTSUM-PARM                                   
087500     END-READ                                                             
087600     .                                                                    
087700     EJECT                                                                
087800 S04-NOLLA-W22222 SECTION.                                                
087900                                                                          
088000     MOVE +0                    TO LINK-KVBEHOV-SUMMA                     
088100                                   LINK-KVBEHOV-DESSUTOM                  
088200                                   LINK-TIBEHOV-FIRST                     
088300                                                                          
088400     MOVE +1                    TO INDX-BEHOV                             
088500     PERFORM UNTIL INDX-BEHOV > MAX-INDX-BEHOV                            
088600       MOVE +0                  TO LINK-KVBEHOV-VECKA(INDX-BEHOV)         
088700       ADD +1                   TO INDX-BEHOV                             
088800     END-PERFORM                                                          
088900     .                                                                    
089000     EJECT                                                                
089100 S11-WRITE-W22205 SECTION.                                                
089200     SKIP2                                                                
089300     WRITE W22205-POST FROM OUT-W22205                                    
089400                                                                          
089500*    MOVE OUT-IDPTYP TO POSTSUM-TRANSTYP                                  
089600     MOVE 'W22205 '  TO POSTSUM-FDNAMN                                    
089700     MOVE 'W22205D2' TO POSTSUM-DDNAMN2                                   
089800     MOVE 'OUT-'     TO POSTSUM-TRANSTYP                                  
089900     CALL POSTSUM USING POSTSUM-PARM                                      
090000     .                                                                    
090100     EJECT                                                                
090200 S12-WRITE-W22205X SECTION.                                               
090300     SKIP2                                                                
090400     WRITE W22205X-POST FROM OUT2-W22205X                                 
090500                                                                          
090600     MOVE 'AZURE'     TO POSTSUM-TRANSTYP                                 
090700     MOVE 'W22205X'   TO POSTSUM-FDNAMN                                   
090800     MOVE 'W22205D3'  TO POSTSUM-DDNAMN2                                  
090900     MOVE 'OUT2-'     TO POSTSUM-TRANSTYP                                 
091000     CALL POSTSUM  USING POSTSUM-PARM                                     
091100     .                                                                    
091200     EJECT                                                                
091300*    -COPY WY2000P3                                                       
