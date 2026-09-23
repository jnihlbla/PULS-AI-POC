000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1118700.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/09/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDK7 OCH INFIL(W91042) OCH SKRIVER UTFIL(W11187)           
000900*                SKAPAR UTFIL MED ARTIKLAR SOM SKALL PUBLICERAS           
001000*                INOM DE NÄRMSTA MELLAN VECKA +8 OCH +9 FRAMÖVER          
001100*                ELLER OM DE HAR REGISTRERATS UNDER DEN GÅNGNA            
001200*                VECKAN                                                   
001300*                                                                         
001400*   ÄNDRING    97-12-08     JOHAN L                                       
001500*              FEL I URVAL RÄTTAT. TIDIGARE URVAL 8-10 VECKOR             
001600*              VAR FEL, RÄTT SKALL VARA 8-9 VECKOR !                      
001700*                                                                         
001800*   ÄNDRING    99-01-18     JOHAN L                                       
001900*              NU ÄR NDC 61 OCH 62 INKLUDERAT !                           
002000*                                                                         
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- INFIL                                                      
003100     SELECT W91042                     ASSIGN TO W11187D1.                
003200     SKIP2                                                                
003300*          --- UTFIL MED INTRESSANTA ARTIKLAR FÖR UTLISTAN                
003400     SELECT W11187                     ASSIGN TO W11187D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W91042                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W91042   -PRE INP-                                        
004500     SKIP3                                                                
004600 FD  W11187                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W11187   -PRE  UTP-                                       
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400*    -COPY WY2000W3                                                       
005500     SKIP3                                                                
005600*    -COPY WY2000W1                                                       
005700     SKIP3                                                                
005800 01  WS-TIAAVV                   PIC 9(4).                                
005900 01  WS-TIAAVV-START             PIC 9(4).                                
006000 01  WS-TIAAVV-SLUT              PIC 9(4).                                
006100 01  WS-TIAAVV-SLUT-FD           PIC 9(4).                                
006200                                                                          
006300 01  WS-TIFINLV                  PIC  9(4) COMP-3.                        
006400 01  WS-TIAAMMDD                 PIC  9(7).                               
006500                                                                          
006600 01  INNEV-AAVV.                                                          
006700   03 INNEV-AA                   PIC 9(2).                                
006800   03 INNEV-VV                   PIC 9(2).                                
006900                                                                          
007000 01  INNEV-AAVVD.                                                         
007100   03 INNEV-AAVV2                PIC 9(4).                                
007200   03 INNEV-D                    PIC 9.                                   
007300                                                                          
007400 01 INNEV-AAVVD-NUM              PIC 9(5).                                
007500                                                                          
007600 01 WS-SISTA-SIFFRAN.                                                     
007700     03 WS-KDERS                 PIC 9.                                   
007800                                                                          
007900 01 W-SUP                        PIC X VALUE SPACE.                       
008000 01 W-SUP-2                      PIC X VALUE SPACE.                       
008100                                                                          
008200 01  ARE-ALL-PARTS-FOUND         PIC X.                                   
008300     88  ALL-PARTS-ARE-FOUND                 VALUE 'J'.                   
008400     88  ALL-PARTS-NOT-FOUND-YET             VALUE 'N'.                   
008500     SKIP2                                                                
008600 77  IDPGM                       PIC X(8)    VALUE 'W1118700'.            
008700 77  JA                          PIC X       VALUE 'J'.                   
008800 77  NEJ                         PIC X       VALUE 'N'.                   
008900                                                                          
009000*    -COPY WWPRODSL                                                       
009100     SKIP3                                                                
009200                                                                          
009300 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
009400     88  END-OF-W91042                       VALUE 'J'.                   
009500     EJECT                                                                
009600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES DAGENS-DATUM.                                       
009800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010100     EJECT                                                                
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
011100     SKIP2                                                                
011200*    --- PARAMETRAR TILL ABEND                                            
011300                                                                          
011400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011700     SKIP2                                                                
011800 01  FELTEXT.                                                             
011900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012100     EJECT                                                                
012200*      --- VALID IDDC CODES                                               
012300*                                                                         
012400*01    -COPY WWDCKONS                                                     
012500*01    -COPY WWDC99                                                       
012600       EJECT                                                              
012700*    --- PARAMETRAR TILL DATKORT                                          
012800*                                                                         
012900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W11187'.              
013000     SKIP2                                                                
013100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013200     SKIP2                                                                
013300*01  -COPY WDATKORT                                                       
013400     EJECT                                                                
013500*    --- PARAMETRAR TILL POSTSUM                                          
013600*                                                                         
013700*01  -COPY W0005   -PRE  POSTSUM-                                         
013800     EJECT                                                                
013900*01  -COPY WDATAREA                                                       
014000     EJECT                                                                
014100* VARIABLER TILL SUBPROGRAM W009VADD                                      
014200 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
014300 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
014400     EJECT                                                                
014500 01  IN-AREA-START               PIC X(24)   VALUE                        
014600                                 'IN-AREA-START  '.                       
014700     SKIP2                                                                
014800                                                                          
014900*01  AREA -COPY W91042     -PRE IN-                                       
015000     EJECT                                                                
015100 01  UT-AREA-START               PIC X(24)   VALUE                        
015200                                 'UT-AREA-START  '.                       
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W11187     -PRE UT-                                       
015600     EJECT                                                                
015700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*                                                                         
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200*--------------------NYCKLAR TILL ANROP PÅ ARTS                           
016300 01  NYCKLAR-TILL-DLI-ARTS.                                               
016400     03  W-IDARTNR-S-X.                                                   
016500         05  W-IDARTNR-S         PIC S9(9)   COMP-3 VALUE ZEROES.         
016600     SKIP2                                                                
016700     03  W-IDDC-A-X.                                                      
016800         05  W-IDDC-MIN-X        PIC X(2)    VALUE '40'.                  
016900         05  W-IDDC-MAX-X        PIC X(2)    VALUE '59'.                  
017000     SKIP2                                                                
017100     03  W-IDDC-B-X.                                                      
017200         05  W-IDDC-B            PIC X(2)    VALUE SPACE.                 
017300*--------------------NYCKLAR TILL ANROP PÅ ERSB                           
017400 01  NYCKLAR-TILL-DLI-ERSB.                                               
017500     03  W-KEY-X.                                                         
017600         05  W-ERSB-MIN-X.                                                
017700             07 W-IDARTNR-MIN-X  PIC S9(9)   COMP-3 VALUE ZEROES.         
017800             07 FILLER           PIC X(7)    VALUE LOW-VALUE.             
017900         05  W-ERSB-MAX-X.                                                
018000             07 W-IDARTNR-MAX-X  PIC S9(9)   COMP-3 VALUE ZEROES.         
018100             07 FILLER           PIC X(7)    VALUE HIGH-VALUE.            
018200     SKIP2                                                                
018300*--------------------NYCKLAR TILL ANROP PÅ ARTC                           
018400 01  NYCKLAR-TILL-DLI-ARTC.                                               
018500     03  W-IDARTNR-C-X.                                                   
018600         05  W-IDARTNR-C         PIC S9(9)   COMP-3 VALUE ZEROES.         
018700     SKIP2                                                                
018800*    --- STATUS-KOD FRÅN IMS                                              
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019200     SKIP2                                                                
019300 01  GODK-STATUSKODER.                                                    
019400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019500     SKIP3                                                                
019600 01  SSA1                        PIC X(64).                               
019700 01  SSA2                        PIC X(64).                               
019800     EJECT                                                                
019900*    --- IMS FUNKTIONSKODER                                               
020000*01  -COPY W0003                                                          
020100     EJECT                                                                
020200*    ---  DLI INPUT-OUTPUT AREA                                           
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
020400 01  DLI-IO-WLARTS11.                                                     
020500*    03  -COPY WDK711  -PRE ARTS-                                         
020600     EJECT                                                                
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLERSB01'.                    
020800 01  DLI-IO-WLERSB01.                                                     
020900*    03  -COPY WDD7A1  -PRE ERSB-                                         
021000     EJECT                                                                
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
021200 01  DLI-IO-WLARTC01.                                                     
021300*    03  -COPY WDK601  -PRE ARTC-                                         
021400     EJECT                                                                
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
021600 01  DLI-IO-WLARTC11.                                                     
021700*    03  -COPY WDK611  -PRE ARTC-                                         
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000                                                                          
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE ARTS-                                              
022300     05  FILLER                  PIC X.                                   
022400*01  -COPY W0008  -PRE ERSB-                                              
022500     05  FILLER                  PIC X.                                   
022600*01  -COPY W0008  -PRE ARTC-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900 PROCEDURE DIVISION  USING ARTS-PCB ERSB-PCB ARTC-PCB.                    
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL' USING ARTS-PCB ERSB-PCB ARTC-PCB.                    
023200                                                                          
023300     PERFORM A-INIT                                                       
023400                                                                          
023500     PERFORM S01-LAES-W91042                                              
023600     PERFORM UNTIL END-OF-W91042                                          
023700       COMPUTE WS-TIFINLV = IN-TIFINLV / 10                               
023800                                                                          
023900       MOVE WS-TIFINLV       TO TMP1-YYWW                                 
024000       MOVE WS-TIAAVV-SLUT   TO TMP2-YYWW                                 
024100       PERFORM WY2000P3                                                   
024200       IF TMP1-YYWW  <= TMP2-YYWW                                         
024300         MOVE IN-TIREGDAT   TO TMP1-YYMMDD                                
024400         MOVE WS-TIAAMMDD   TO TMP2-YYMMDD                                
024500         PERFORM WY2000P1                                                 
024600         MOVE WS-TIFINLV        TO TMP1-YYWW                              
024700         MOVE WS-TIAAVV-SLUT-FD TO TMP2-YYWW                              
024800         PERFORM WY2000P3                                                 
024900         IF TMP1-YYMMDD > TMP2-YYMMDD OR                                  
025000            TMP1-YYWW   > TMP2-YYWW                                       
025100           MOVE IN-KDPRODSL  TO TEST-KDPRODSL                             
025200           IF KDPRODSL-BIMA                                               
025300             CONTINUE                                                     
025400           ELSE                                                           
025500             IF  IN-IDFKNGRP = 3955 AND IN-FLLSRDEL = 'N'                 
025600               CONTINUE                                                   
025700             ELSE                                                         
025800               IF NOT IN-IDPROJ = '9501' OR '9502' OR 'E695'              
025900                 MOVE NEJ    TO UT-FLJANEJ-US-CAN                         
026000                 MOVE NEJ    TO UT-FLJANEJ-JPN                            
026100                 MOVE NEJ    TO UT-FLJANEJ-AUS                            
026110                 MOVE IN-IDARTNR  TO  W-IDARTNR-S                         
026200                 PERFORM B-CHECK-WDK7-NA                                  
026300                 IF SEGMENT-SAKNAS                                        
026400                   MOVE JA   TO UT-FLJANEJ-US-CAN                         
026500                 END-IF                                                   
026600                 PERFORM B-CHECK-WDK7-JPN                                 
026700                 IF SEGMENT-SAKNAS                                        
026800                   MOVE JA   TO UT-FLJANEJ-JPN                            
026900                 END-IF                                                   
027000                 PERFORM B-CHECK-WDK7-AUS                                 
027100                 IF SEGMENT-SAKNAS                                        
027200                   MOVE JA   TO UT-FLJANEJ-AUS                            
027300                 END-IF                                                   
027400                 IF UT-FLJANEJ-US-CAN = JA   OR                           
027500                    UT-FLJANEJ-JPN    = JA   OR                           
027600                    UT-FLJANEJ-AUS    = JA                                
027700                   PERFORM C-TILLDELNING                                  
027800                   PERFORM D-ERSATTNINGS-KONTROLL                         
027900                   PERFORM S11-SKRIV-W11187                               
028000                 END-IF                                                   
028100               END-IF                                                     
028200             END-IF                                                       
028300           END-IF                                                         
028400         END-IF                                                           
028500       END-IF                                                             
028600       PERFORM S01-LAES-W91042                                            
028700     END-PERFORM                                                          
028800                                                                          
028900                                                                          
029000     PERFORM Z-FINIT                                                      
029100                                                                          
029200     MOVE ZERO TO RETURN-CODE                                             
029300     GOBACK                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 A-INIT SECTION.                                                          
029700                                                                          
029800     OPEN INPUT  W91042                                                   
029900                                                                          
030000     OPEN OUTPUT W11187                                                   
030100                                                                          
030200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
030300     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
030400     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
030500     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
030600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030700                                                                          
030800     MOVE D-AAR       TO INNEV-AA                                         
030900     MOVE D-VECKA     TO INNEV-VV                                         
031000     MOVE D-DAGNR     TO INNEV-D                                          
031100                                                                          
031200     MOVE INNEV-AAVV  TO WS-TIAAVV                                        
031300     MOVE INNEV-AAVV  TO WS-TIAAVV-START                                  
031400     MOVE INNEV-AAVV  TO WS-TIAAVV-SLUT                                   
031500     MOVE INNEV-AAVV  TO WS-TIAAVV-SLUT-FD                                
031600                                                                          
031700*    *** INNEVARANDE VECKA - 1 VECKA  (FÖREGÅENDE KÖRNING)                
031800     MOVE -1               TO ANTAL-VECKOR                                
031900     MOVE WS-TIAAVV-START  TO DATUM-AAVV                                  
032000     CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                          
032100     MOVE DATUM-AAVV      TO WS-TIAAVV-START                              
032200                                                                          
032300     MOVE WS-TIAAVV-START TO INNEV-AAVV2                                  
032400*    *** OMVANDLA TILL ÅÅMMDD                                             
032500     MOVE INNEV-AAVVD     TO INNEV-AAVVD-NUM                              
032600     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
032700     MOVE INNEV-AAVVD-NUM TO DAT-I-TIDATUM                                
032800                                                                          
032900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
033000                     DAT-O-TIDATUM DAT-KDSVAR                             
033100                                                                          
033200     IF DAT-KDSVAR-OK                                                     
033300        MOVE DAT-TIAAMMDD  TO WS-TIAAMMDD                                 
033400     ELSE                                                                 
033500        MOVE 'FEL FRÅN DATKONV' TO FELTEXT-STR                            
033600        PERFORM S99-ABEND                                                 
033700     END-IF                                                               
033800                                                                          
033900     MOVE 9  TO ANTAL-VECKOR                                              
034000     MOVE WS-TIAAVV-SLUT    TO DATUM-AAVV                                 
034100     CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                          
034200     MOVE DATUM-AAVV TO WS-TIAAVV-SLUT                                    
034300                                                                          
034400     MOVE 8 TO ANTAL-VECKOR                                               
034500     MOVE WS-TIAAVV-SLUT-FD TO DATUM-AAVV                                 
034600     CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                          
034700     MOVE DATUM-AAVV TO WS-TIAAVV-SLUT-FD                                 
034800     .                                                                    
034900     EJECT                                                                
035000 B-CHECK-WDK7-NA SECTION.                                                 
035100     PERFORM IMS-GET-ARTS-SLAG-NA                                         
035200     .                                                                    
035300     EJECT                                                                
035400 B-CHECK-WDK7-JPN SECTION.                                                
035600     MOVE WC-NDC-JP-61 TO  W-IDDC-B                                       
035700     PERFORM IMS-GET-ARTS-SLAG-6X                                         
035800     .                                                                    
035900     EJECT                                                                
036000 B-CHECK-WDK7-AUS SECTION.                                                
036200     MOVE WC-NDC-AU   TO  W-IDDC-B                                        
036300     PERFORM IMS-GET-ARTS-SLAG-6X                                         
036400     .                                                                    
036500     EJECT                                                                
036600 C-TILLDELNING SECTION.                                                   
036700     MOVE IN-IDPROJ        TO   UT-IDPROJ                                 
036800     MOVE IN-IDFKNGRP      TO   UT-IDFKNGRP                               
036900     MOVE IN-IDARTNR       TO   UT-IDARTNR                                
037000     MOVE IN-BEART(10)     TO   UT-BEART-USA                              
037100     MOVE IN-KDPRODSL      TO   UT-KDPRODSL                               
037200     MOVE IN-KDBPSR        TO   UT-KDBPSR                                 
037300     MOVE IN-TIFINLV       TO   UT-TIFINLV                                
037400     MOVE IN-IDARTNR-MOTSV TO   UT-IDARTNR-MOTSV                          
037500     MOVE IN-KDUART        TO   UT-KDUART                                 
037600     MOVE IN-KDFARLIG      TO   UT-KDFARLIG                               
037700     MOVE IN-FLLSRDEL      TO   UT-FLLSRDEL                               
037800     MOVE IN-PRARTSTD      TO   UT-PRARTSTD                               
037900     MOVE IN-KDPSLLOC      TO   UT-KDPSLLOC                               
038000     MOVE IN-KDERS         TO   UT-KDERS                                  
038100     MOVE IN-KDERS-UTG     TO   UT-KDERS-UTG                              
038200     .                                                                    
038300     EJECT                                                                
038400 D-ERSATTNINGS-KONTROLL SECTION.                                          
038500     IF IN-FLERS = 'J'                                                    
038600       MOVE SPACE TO W-SUP                                                
038700       PERFORM E-HITTA-ERSATTANDE-ARTIKEL                                 
038800       MOVE W-SUP  TO  UT-FLERS                                           
038900     ELSE                                                                 
039000       MOVE SPACE  TO  UT-FLERS                                           
039100     END-IF                                                               
039200     .                                                                    
039300 E-HITTA-ERSATTANDE-ARTIKEL SECTION.                                      
039400     MOVE IN-IDARTNR TO W-IDARTNR-MIN-X                                   
039500     MOVE IN-IDARTNR TO W-IDARTNR-MAX-X                                   
039600     PERFORM IMS-GET-ERSB-ERS                                             
039700                                                                          
039800     IF SEGMENT-FINNS                                                     
039900       MOVE NEJ TO ARE-ALL-PARTS-FOUND                                    
040000       PERFORM UNTIL ALL-PARTS-ARE-FOUND                                  
040100         IF SEGMENT-FINNS                                                 
040200           IF NOT IN-IDARTNR = ERSB-TILLK-IDARTNR                         
040300             MOVE JA TO ARE-ALL-PARTS-FOUND                               
040400           ELSE                                                           
040500             MOVE ERSB-ERS-IDARTNR TO W-IDARTNR-C-X                       
040600             PERFORM IMS-GET-ARTC-ART                                     
040700             IF ARTC-ART-KDERS-UTG > 0                                    
040800               MOVE ARTC-ART-KDERS-UTG TO WS-KDERS                        
040900               PERFORM F-WSKDERS-TILL-WSUP2                               
041000             ELSE                                                         
041100               PERFORM IMS-GET-ARTC-CLAG                                  
041200               IF SEGMENT-FINNS                                           
041300                 MOVE ARTC-CLAG-KDERS TO WS-KDERS                         
041400                 PERFORM F-WSKDERS-TILL-WSUP2                             
041500               END-IF                                                     
041600             END-IF                                                       
041700                                                                          
041800             IF W-SUP = SPACE                                             
041900               MOVE W-SUP-2 TO W-SUP                                      
042000             ELSE                                                         
042100               IF NOT W-SUP = W-SUP-2                                     
042200                 MOVE 'M' TO W-SUP                                        
042300               END-IF                                                     
042400             END-IF                                                       
042500           END-IF                                                         
042600         ELSE                                                             
042700           MOVE JA TO ARE-ALL-PARTS-FOUND                                 
042800         END-IF                                                           
042900                                                                          
043000         IF NOT ALL-PARTS-ARE-FOUND                                       
043100           PERFORM IMS-GET-NEXT-ERSB-ERS                                  
043200         END-IF                                                           
043300                                                                          
043400       END-PERFORM                                                        
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 F-WSKDERS-TILL-WSUP2 SECTION.                                            
043900* NEDANSTÅENDE EVALUATE-SATS SÄTTER W-SUP-2 TILL '*' OM WS-KDERS          
044000* HAR VÄRDET 1,2,3 ELLER 7, ANNARS (4,5,6 ELLER 8) SÄTTS 'V'              
044100                                                                          
044200     EVALUATE WS-KDERS                                                    
044300     WHEN 1 THRU 3                                                        
044400     WHEN 7                                                               
044500           MOVE "*" TO W-SUP-2                                            
044600     WHEN 4 THRU 6                                                        
044700     WHEN 8                                                               
044800           MOVE "V" TO W-SUP-2                                            
044900     END-EVALUATE                                                         
045000     .                                                                    
045100     EJECT                                                                
045200 Z-FINIT SECTION.                                                         
045300     CLOSE W91042                                                         
045400           W11187                                                         
045500     SKIP2                                                                
045600     MOVE 'S' TO POSTSUM-OPKOD                                            
045700     CALL POSTSUM USING POSTSUM-PARM                                      
045800     .                                                                    
045900     EJECT                                                                
046000 S01-LAES-W91042  SECTION.                                                
046100     READ W91042 INTO IN-AREA                                             
046200     AT END                                                               
046300        MOVE HIGH-VALUE TO IN-AREA                                        
046400        SET END-OF-W91042 TO TRUE                                         
046500                                                                          
046600     NOT AT END                                                           
046700        MOVE 'W91042' TO POSTSUM-FDNAMN                                   
046800        MOVE 'W11187D1' TO POSTSUM-DDNAMN2                                
046900        MOVE SPACE TO POSTSUM-TRANSTYP                                    
047000        CALL POSTSUM USING POSTSUM-PARM                                   
047100     END-READ                                                             
047200     .                                                                    
047300     EJECT                                                                
047400 S11-SKRIV-W11187 SECTION.                                                
047500                                                                          
047600     WRITE UTP-POST FROM UT-AREA                                          
047700                                                                          
047800     MOVE 'W11187' TO POSTSUM-FDNAMN                                      
047900     MOVE 'W11187D2' TO POSTSUM-DDNAMN2                                   
048000     CALL POSTSUM USING POSTSUM-PARM                                      
048100     .                                                                    
048200     EJECT                                                                
048300 S99-ABEND SECTION.                                                       
048400                                                                          
048500     SKIP2                                                                
048600     MOVE 'S' TO POSTSUM-OPKOD                                            
048700     CALL POSTSUM USING POSTSUM-PARM                                      
048800     CALL ABEND USING RKOD-ABEND                                          
048900     .                                                                    
049000     EJECT                                                                
049100* --- IMS SEKTIONER ---                                                   
049200     SKIP3                                                                
049300 IMS-GET-ARTS-SLAG-NA SECTION.                                            
049400                                                                          
049500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-S-X ')'                       
049600          DELIMITED BY SIZE INTO SSA1                                     
049700     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN-X                            
049800                    '&IDDC    =<' W-IDDC-MAX-X ')'                        
049900          DELIMITED BY SIZE INTO SSA2                                     
050000     MOVE '  GE' TO GODK-STATUSKODER                                      
050100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
050200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GET-ARTS-SLAG-6X SECTION.                                            
050700                                                                          
050800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-S-X ')'                       
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     STRING 'WLARTS11(IDDC     =' W-IDDC-B-X ')'                          
051100          DELIMITED BY SIZE INTO SSA2                                     
051200     MOVE '  GE' TO GODK-STATUSKODER                                      
051300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
051400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
051500     PERFORM IMS-STATUSKONTROLL                                           
051600     .                                                                    
051700     EJECT                                                                
051800 IMS-GET-ERSB-ERS SECTION.                                                
051900                                                                          
052000     STRING 'WLERSB01(WDD7A1KY>=' W-ERSB-MIN-X                            
052100                    '&WDD7A1KY=<' W-ERSB-MAX-X ')'                        
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE '  GE' TO GODK-STATUSKODER                                      
052400     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-WLERSB01 SSA1                  
052500     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSKONTROLL                                           
052700     .                                                                    
052800     EJECT                                                                
052900 IMS-GET-NEXT-ERSB-ERS SECTION.                                           
053000                                                                          
053100     MOVE 'WLERSB01' TO SSA1                                              
053200     MOVE '  GB' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-WLERSB01 SSA1                  
053400     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-GET-ARTC-ART  SECTION.                                               
053900                                                                          
054000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-C-X ')'                       
054100          DELIMITED BY SIZE INTO SSA1                                     
054200     MOVE '  ' TO GODK-STATUSKODER                                        
054300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
054400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUSKONTROLL                                           
054600     .                                                                    
054700     EJECT                                                                
054800 IMS-GET-ARTC-CLAG SECTION.                                               
054900                                                                          
055000     MOVE 'WLARTC11' TO SSA1                                              
055100     MOVE '  GE' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
055300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-STATUSKONTROLL SECTION.                                              
055800                                                                          
055900     SET STATUS-IX TO 1                                                   
056000     SEARCH GODK-STATUS                                                   
056100       AT END                                                             
056200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056300           DELIMITED BY SIZE INTO FELTEXT                                 
056400         DISPLAY FELTEXT                                                  
056500         CALL FELLOG                                                      
056600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056700         CONTINUE                                                         
056800     END-SEARCH                                                           
056900     .                                                                    
057000     EJECT                                                                
057100*    -COPY WY2000P1                                                       
057200     EJECT                                                                
057300*    -COPY WY2000P3                                                       
