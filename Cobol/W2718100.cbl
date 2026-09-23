000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2718100.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   DEC 2001.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET TAR FRAM DE ARTIKLAR DÄR FLREFBEO                     
001200*        SKA AKTIVERAS                                                    
001300*                                                                         
001400*        PROGRAM RUNS WEEKLY                                              
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDK7                                       
001700*                              WDL7                                       
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002100*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*                                                                         
003300     SELECT W27181                     ASSIGN TO W27181D1.                
003400*          --- UT-FIL                                                     
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000                                                                          
004100                                                                          
004200 FD  W27181                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W27181  -PRE UT-    -L.                                   
004700                                                                          
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100*    -COPY WY2000W1                                                       
005200     SKIP2                                                                
005300*    -COPY WY2000W3                                                       
005400     SKIP3                                                                
005500*    -COPY WY2000W2                                                       
005600     SKIP3                                                                
005700*    -COPY WWPRODSL                                                       
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(8)    VALUE 'W2718100'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006010 77  YES                         PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  STOPP                       PIC X       VALUE 'S'.                   
006300 77  AKTIV                       PIC X       VALUE 'A'.                   
006400                                                                          
006500*    --- INDEX SAMT MAX-INDEX                                             
006600 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006700 77  IX                          PIC 9(2)    VALUE ZERO.                  
006800                                                                          
006900*    --- SWITCHAR                                                         
007000 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
007100                                                                          
007200                                                                          
007300 01  ESCLOCK-SW                  PIC X       VALUE 'N'.                   
007400     88  PROGNOS-AER-LAAST                   VALUE 'J'.                   
007500     88  PROGNOS-AER-FAST                    VALUE 'A'.                   
007600     88  PROGNOS-EJ-LAAST                    VALUE 'N'.                   
007700                                                                          
007800 01  ORDERHIT-SW                 PIC X       VALUE 'J'.                   
007900     88  NO-ORDER-HITS                       VALUE 'N'.                   
008000                                                                          
008100*    --- ARBETSFÄLT                                                       
008200 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
008300 01  ARBETSFAELT.                                                         
008400     03  WS-ANTAL-KVOT           PIC 9(9)    VALUE ZERO.                  
008500     03  WS-TIFINLV              PIC S9(5)   VALUE ZERO.                  
008600     03  WS-FLEXCP1-REFBEO       PIC 9(1)    VALUE ZERO.                  
008700     03  WS-FLEXCP2-REFBEO       PIC 9(1)    VALUE ZERO.                  
008701     03  WS-FLEXCP4-REFBEO       PIC 9(1)    VALUE ZERO.                  
008702                                                                          
008703     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400* ARBETSFÄLT FÖR ATT KONTROLLERA PB-LSÅNING, ESC-LÅS MM.                  
009500 01  DA-DAGENS-DATUM.                                                     
009600     03  DAGENS-DATUM-20         PIC 9(2) VALUE 20.                       
009700     03  DAGENS-DATUM-6LONG      PIC 9(6).                                
009800 01  WS-DAPUBL.                                                           
009900     03  WS-DAPUBL-YEAR          PIC 9(4).                                
010000     03  WS-DAPUBL-MONTH         PIC 9(2).                                
010100     03  WS-DAPUBL-DAY           PIC 9(2).                                
010200 01  WS2-TIFINLV                 PIC 9(5).                                
010300 01  FILLER REDEFINES WS2-TIFINLV.                                        
010400     03  WS2-TIFINLV-AAVV        PIC 9(4).                                
010500     03  FILLER                  PIC 9(1).                                
010600 01  WS3-TIFINLV                 PIC 9(6).                                
010710 01  WS-PRARTBES                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
010800 01  WS-DAGENS-DAT-1YEAR         PIC 9(6).                                
010900*                                                                         
011000 01  WS-KDERS                    PIC 9(3) VALUE ZEROES.                   
011100                                                                          
011110*                                                                         
011120 01  WS-KLASS                    PIC 9(1)    VALUE ZERO.                  
011170*                                                                         
011200     EJECT                                                                
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400*                                                                         
011500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
012200     SKIP2                                                                
012300*    --- PARAMETRAR TILL ABEND                                            
012400                                                                          
012500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012700     SKIP2                                                                
012800 01  FELTEXT.                                                             
012900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL DATKORT                                          
013300*                                                                         
013400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27181'.              
013500     SKIP2                                                                
013600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013700     SKIP2                                                                
013800*01  -COPY WDATKORT                                                       
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL WDATKONV                                         
014500*                                                                         
014600*01  -COPY WDATAREA                                                       
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL W009VADD                                         
014900*                                                                         
015000 01  W009VADD-AREA.                                                       
015100     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
015200     03  ANTAL                   PIC S9(3) COMP-3.                        
015300*                                                                         
015330                                                                          
015400     EJECT                                                                
015500 01  UT-AREA-START              PIC X(24)   VALUE                         
015600                                 'UT-AREA-START  '.                       
015700     SKIP2                                                                
015800                                                                          
015900*01  AREA -COPY W27181     -PRE UT-                                       
016000     EJECT                                                                
016100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016200                                                                          
016300     SKIP3                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016500     SKIP3                                                                
016600 01  NYCKLAR-TILL-DLI.                                                    
016700     03  W-IDARTNR-X.                                                     
016800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016900                                                                          
017000     03  W-IDARTNR-US-X.                                                  
017100         05  W-IDARTNR-US        PIC S9(9)   VALUE ZERO COMP-3.           
017200                                                                          
017300     03  W-IDLAND-US-X.                                                   
017400         05  W-IDLAND-US         PIC X(2)    VALUE SPACE.                 
017500                                                                          
017600     03  W-IDDC-X.                                                        
017700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017800                                                                          
017900     03  W-IDDC-B6-X.                                                     
018000         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
018100                                                                          
018200     03  W-KDSEGKEY-X.                                                    
018300         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
018400                                                                          
018500     03  W-WDGXKEY-X.                                                     
018600          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
018700          05 W-IDDC-2501         PIC X(2)    VALUE SPACE.                 
018800          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
018900                                                                          
019000     03  W-IDREFTAB-X.                                                    
019001         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
019002*                                                                         
019003     SKIP2                                                                
019004*    --- STATUS-KOD FRÅN IMS                                              
019005 01  STATUS-WS                   PIC XX.                                  
019006     88  SEGMENT-FINNS                       VALUE '  '.                  
019007     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     SKIP3                                                                
019500 01  SSA1                        PIC X(64).                               
019600 01  SSA2                        PIC X(64).                               
019700     EJECT                                                                
019800*    --- IMS FUNKTIONSKODER                                               
019900*01  -COPY W0003                                                          
020000     EJECT                                                                
020100*    ---  DLI INPUT-OUTPUT AREA                                           
020200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
020300 01  DLI-IO-WDK601.                                                       
020400*    03  -COPY WDK601                                                     
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
020800 01  DLI-IO-WDK611.                                                       
020900*    03  -COPY WDK611                                                     
021000     EJECT                                                                
021100                                                                          
021200                                                                          
021300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
021400     SKIP3                                                                
021500 01  DLI-IO-AREA-K7.                                                      
021600     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
021700     SKIP3                                                                
021800     03  WDK701 REDEFINES IO-AREA-K7.                                     
021900*        05  -COPY WDK701                                                 
022000     SKIP3                                                                
022100     03  WDK711 REDEFINES IO-AREA-K7.                                     
022200*        05  -COPY WDK711                                                 
022300     EJECT                                                                
022400                                                                          
022500                                                                          
022600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
022700 01  DLI-IO-WDK712.                                                       
022800*    03  -COPY WDK712                                                     
022900     EJECT                                                                
023000                                                                          
023100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
023200 01  DLI-IO-WDL711.                                                       
023300*    03  -COPY WDL711                                                     
023400     EJECT                                                                
023500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023600 01   DLI-IO-AREA-B601.                                                   
023700*     03  -COPY WDB601                                                    
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL250111'.                    
024100 01  DLI-IO-WL250111.                                                     
024200*    03  -COPY WDGX2502                                                   
024201     EJECT                                                                
024202 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
024203                                                                          
024204 LINKAGE SECTION.                                                         
024300                                                                          
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE WDK7-                                              
024600     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
024700     EJECT                                                                
024800*01  -COPY W0008  -PRE WDK7US-                                            
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE WDK6-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008  -PRE WDK7X-                                             
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE WDL7-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008  -PRE WDL4-                                              
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE WDB6-                                              
026400     05  FILLER                  PIC X.                                   
026401*01  -COPY W0008  -PRE WDR2-                                              
026420     05  FILLER                  PIC X.                                   
026430                                                                          
026500     EJECT                                                                
026600 PROCEDURE DIVISION  USING WDK7-PCB WDK7US-PCB WDK6-PCB WDK7X-PCB         
026700                           WDL7-PCB WDL4-PCB WDB6-PCB WDR2-PCB.           
026800     ENTRY 'DLITCBL' USING WDK7-PCB WDK7US-PCB WDK6-PCB WDK7X-PCB         
026900                           WDL7-PCB WDL4-PCB WDB6-PCB WDR2-PCB.           
027100     PERFORM A-INIT                                                       
027200     PERFORM IMS-GN-WDK7                                                  
027300     PERFORM UNTIL SEGMENT-SAKNAS                                         
027400       EVALUATE WDK7-SEG-NAME-FB                                          
027500         WHEN 'WDK701  '                                                  
027600           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
027700         WHEN 'WDK711  '                                                  
027800                                                                          
027900           MOVE SLAG-IDDC    TO W-IDDC                                    
028000                                W-IDDC-B6                                 
028200*                                                                         
028300           IF SLAG-FLREFBEO = 'S'                                         
028700             CONTINUE                                                     
028800           ELSE                                                           
028801             PERFORM IMS-GU-WDB601                                        
028802*                                                                         
028900             MOVE DCS-FLEXCP1-REFBEO TO WS-FLEXCP1-REFBEO                 
029000             MOVE DCS-FLEXCP2-REFBEO TO WS-FLEXCP2-REFBEO                 
029001             MOVE DCS-FLEXCP4-REFBEO TO WS-FLEXCP4-REFBEO                 
029002*                                                                         
029003             IF (DCS-CHINA OR DCS-USA)                                    
029004             AND SLAG-IDDC-REF = SPACES                                   
029005               CONTINUE                                                   
030200             ELSE                                                         
030210                                                                          
030300               PERFORM IMS-GU-WDK601                                      
030400               MOVE ART-KDPRODSL TO TEST-KDPRODSL                         
030401               MOVE ART-TIFINLV TO WS-TIFINLV                             
030402                                                                          
030403               PERFORM IMS-GU-WDK611                                      
030404               IF SEGMENT-FINNS                                           
030405                  MOVE CLAG-KDERS  TO WS-KDERS                            
030406               END-IF                                                     
030407                                                                          
031807                                                                          
031808               IF SLAG-FLREFBEO = JA                                      
031809*                TO SET REFILL FLAG AS 'N'                                
031810                 PERFORM D-UPD-REFILL-FLAG-J-TO-N                         
031811               ELSE                                                       
031812                 IF ( KDPRODSL-WHEELS OR                                  
031813                       KDPRODSL-TOOLS OR                                  
031814                       KDPRODSL-EMB  OR                                   
031815                       KDPRODSL-BRANDON )  OR                             
031816                       ART-IDLEVNR = 'BQ8VA' OR                           
031817                       CLAG-KDUART = 'S'                                  
031818                       CONTINUE                                           
031819                 ELSE                                                     
031820                    IF SLAG-KDREFSTA = AKTIV                              
031830                    AND SLAG-FLREFBEO = NEJ                               
031831                    AND SLAG-FLREFILL = JA                                
031832                    AND SLAG-IDDC-REF > SPACES                            
031833                    AND ART-KDERS-UTG = 0                                 
031834                    AND WS-KDERS    < 20                                  
031835                       PERFORM S01-KOLLA-PROGNOS-LAASNING                 
031836                       IF PROGNOS-EJ-LAAST                                
031837*                         TO SET REFILL FLAG AS 'J'                       
031838                          PERFORM E-UPD-REFILL-FLAG-N-TO-J                
031839                       END-IF                                             
032400                    END-IF                                                
032410                 END-IF                                                   
032500               END-IF                                                     
032600             END-IF                                                       
032700           END-IF                                                         
032800                                                                          
032900       END-EVALUATE                                                       
033000       PERFORM IMS-GN-WDK7                                                
033100     END-PERFORM                                                          
033200                                                                          
033300     PERFORM Z-FINIT                                                      
033400                                                                          
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000                                                                          
034200                                                                          
040900 A-INIT SECTION.                                                          
041000                                                                          
041100     OPEN OUTPUT W27181                                                   
041200                                                                          
041300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
041400     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
041500     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
041600     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
041700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
041800     MOVE DAGENS-DATUM TO DAGENS-DATUM-6LONG                              
041900     COMPUTE WS-DAGENS-DAT-1YEAR =                                        
042000             DAGENS-DATUM-6LONG - 10000                                   
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
047110 D-UPD-REFILL-FLAG-J-TO-N  SECTION.                                       
047120                                                                          
047121*    TO GET NO. OF ORDER HITS                                             
047122       PERFORM S02-GET-ORDER-HITS                                         
047123                                                                          
047140**   VALIDATE REFO FLAGS FROM 4403                                        
047141     IF NO-ORDER-HITS                                                     
047142       CONTINUE                                                           
047143     ELSE                                                                 
047144       IF WS-ANTAL-KVOT < WS-FLEXCP4-REFBEO OR                            
047151          ( DCS-FLEXCP3-REFBEO = NEJ AND                                  
047160            KDPRODSL-ACC )                                                
047170                                                                          
047190         MOVE W-IDARTNR  TO UT-IDARTNR                                    
047191         MOVE SLAG-IDDC  TO UT-IDDC                                       
047192         MOVE 'N'        TO UT-FLREFBEO                                   
047193         PERFORM S06-CHECK-AUTOREF                                        
047194         PERFORM S12-SKRIV-W27181                                         
047195       ELSE                                                               
047196         PERFORM S20-GET-KLASS                                            
047197         IF WS-KLASS     > WS-FLEXCP2-REFBEO                              
047198            MOVE W-IDARTNR  TO UT-IDARTNR                                 
047199            MOVE SLAG-IDDC  TO UT-IDDC                                    
047200            MOVE 'N'        TO UT-FLREFBEO                                
047300            PERFORM S06-CHECK-AUTOREF                                     
047400            PERFORM S12-SKRIV-W27181                                      
047401         END-IF                                                           
047402       END-IF                                                             
047403     END-IF                                                               
047404     .                                                                    
047405                                                                          
047406 E-UPD-REFILL-FLAG-N-TO-J SECTION.                                        
047407                                                                          
047408*    TO GET NO. OF ORDER HITS                                             
047409       PERFORM S02-GET-ORDER-HITS                                         
047410                                                                          
047411*VALIDATE REFO FLAGS FROM 4403                                            
047412     IF WS-ANTAL-KVOT > WS-FLEXCP1-REFBEO                                 
047413        PERFORM S20-GET-KLASS                                             
047414        IF WS-KLASS      <= WS-FLEXCP2-REFBEO                             
047415           IF KDPRODSL-ACC                                                
047416              IF DCS-FLEXCP3-REFBEO = JA                                  
047417                 MOVE W-IDARTNR TO UT-IDARTNR                             
047418                 MOVE SLAG-IDDC TO UT-IDDC                                
047419                 MOVE JA        TO UT-FLREFBEO                            
047420                 PERFORM S06-CHECK-AUTOREF                                
047421                 PERFORM S12-SKRIV-W27181                                 
047422              END-IF                                                      
047423           ELSE                                                           
047424              MOVE W-IDARTNR TO UT-IDARTNR                                
047425              MOVE SLAG-IDDC TO UT-IDDC                                   
047426              MOVE JA        TO UT-FLREFBEO                               
047427              PERFORM S06-CHECK-AUTOREF                                   
047428              PERFORM S12-SKRIV-W27181                                    
047429           END-IF                                                         
047430        END-IF                                                            
047431     END-IF                                                               
047432     .                                                                    
047433                                                                          
047434 Z-FINIT SECTION.                                                         
047435                                                                          
047436     CLOSE W27181                                                         
047500                                                                          
047600     MOVE 'S' TO POSTSUM-OPKOD                                            
047700     CALL POSTSUM USING POSTSUM-PARM                                      
047800     .                                                                    
047900     EJECT                                                                
048000 S01-KOLLA-PROGNOS-LAASNING SECTION.                                      
048100                                                                          
048200     MOVE NEJ TO ESCLOCK-SW                                               
048300                                                                          
048400                                                                          
048500* KONTROLLERA OM PB ÄR MANUELLT LÅST                                      
048600     MOVE SLAG-TIREFMPB      TO TMP1-YYMMDD                               
048700     MOVE SLAG-TIPBREOI      TO TMP3-YYMMDD                               
048800     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
048900     PERFORM WY2000P1                                                     
049000     IF  (TMP1-YYMMDD > TMP2-YYMMDD)                                      
049100     OR  (TMP3-YYMMDD > TMP2-YYMMDD)                                      
049200        MOVE 'A' TO ESCLOCK-SW                                            
049300     ELSE                                                                 
049400*    OM INTE ART. ÄR MANUELLT LÅST SÅ KAN DEN VARA ESC-LÅST               
049500                                                                          
049600*    KONTROLLERA OM ARTIKLEN ÄR ESC-LÅST                                  
049700        IF SLAG-DAREFESC > ZERO                                           
049800        OR SLAG-DAREFESC-REOI > ZERO                                      
049900           IF SLAG-DAREFESC >= DA-DAGENS-DATUM                            
050000           OR SLAG-DAREFESC-REOI >= DA-DAGENS-DATUM                       
050100              MOVE JA TO ESCLOCK-SW                                       
050200           END-IF                                                         
050300        END-IF                                                            
050400                                                                          
050500*    KONTROLLERA OM PUBVECKA DC ÄR INOM 6 MÅNADER                         
050600        IF W-IDARTNR NOT = W-IDARTNR-US                                   
050700           MOVE W-IDARTNR    TO W-IDARTNR-US                              
050800           MOVE DCS-IDLANDX2 TO W-IDLAND-US                               
050900           PERFORM IMS-GU-WDK712                                          
051000           IF SEGMENT-SAKNAS                                              
051100              MOVE ZERO TO LART-DAPUBL                                    
051200           END-IF                                                         
051300        END-IF                                                            
051400        IF LART-DAPUBL > ZERO                                             
051500           PERFORM S03-DAPUBL-1YEAR                                       
051600           IF WS-DAPUBL >= DA-DAGENS-DATUM                                
051700              MOVE JA TO ESCLOCK-SW                                       
051800           END-IF                                                         
051900        END-IF                                                            
052000                                                                          
052100*    KONTROLLERA OM PUBVECKA CDC ÄR INOM 6 MÅNADER                        
052200        IF WS-TIFINLV > ZERO                                              
052300           IF WS-TIFINLV > 50000                                          
052400             MOVE ZERO       TO TMP1-YYMMDD                               
052500           ELSE                                                           
052600             PERFORM S04-WS-TIFINLV-6MONTHS                               
052700             MOVE WS3-TIFINLV TO TMP1-YYMMDD                              
052800           END-IF                                                         
052900           MOVE DAGENS-DATUM TO TMP2-YYMMDD                               
053000           IF  TMP1-YYMMDD > TMP2-YYMMDD                                  
053100              MOVE JA TO ESCLOCK-SW                                       
053200           END-IF                                                         
053300        END-IF                                                            
053400     END-IF                                                               
053500                                                                          
053600     .                                                                    
053700     EJECT                                                                
053800 S02-GET-ORDER-HITS SECTION.                                              
053900                                                                          
054000     INITIALIZE WS-ANTAL-KVOT                                             
054001                                                                          
054002     MOVE YES    TO ORDERHIT-SW                                           
054003     PERFORM IMS-GU-WDL711                                                
054004     IF SEGMENT-FINNS                                                     
054005                                                                          
054006        MOVE ZERO         TO WS-ANTAL-KVOT                                
054007        MOVE 1            TO IX                                           
054008                                                                          
054009        PERFORM UNTIL IX > 53                                             
054010           ADD DC-KVOT-RULL (IX)                                          
054011                          TO WS-ANTAL-KVOT                                
054012           ADD DC-KVOT-REF-RULL (IX)                                      
054013                          TO WS-ANTAL-KVOT                                
054014           ADD 1          TO IX                                           
054015        END-PERFORM                                                       
054016                                                                          
054017        MOVE 1            TO IX                                           
054018        PERFORM UNTIL IX > 5                                              
054019           ADD DC-KVOT-INNEV    (IX)                                      
054020                          TO WS-ANTAL-KVOT                                
054021           ADD DC-KVOT-PP-INNEV (IX)                                      
054022                          TO WS-ANTAL-KVOT                                
054023           ADD DC-KVOT-REF-INNEV (IX)                                     
054024                          TO WS-ANTAL-KVOT                                
054025           ADD 1          TO IX                                           
054026        END-PERFORM                                                       
054027     ELSE                                                                 
054028        MOVE NEJ   TO ORDERHIT-SW                                         
054029                                                                          
054030     END-IF                                                               
054031     .                                                                    
054032 S03-DAPUBL-1YEAR SECTION.                                                
054033                                                                          
054040     MOVE LART-DAPUBL TO WS-DAPUBL                                        
054100                                                                          
054200     IF WS-DAPUBL-MONTH = 01                                              
054300       MOVE 07 TO WS-DAPUBL-MONTH                                         
054400     ELSE                                                                 
054500       IF WS-DAPUBL-MONTH = 02                                            
054600         MOVE 08 TO WS-DAPUBL-MONTH                                       
054700       ELSE                                                               
054800         IF WS-DAPUBL-MONTH = 03                                          
054900           MOVE 09 TO WS-DAPUBL-MONTH                                     
055000         ELSE                                                             
055100           IF WS-DAPUBL-MONTH = 04                                        
055200             MOVE 10 TO WS-DAPUBL-MONTH                                   
055300           ELSE                                                           
055400             IF WS-DAPUBL-MONTH = 05                                      
055500               MOVE 11 TO WS-DAPUBL-MONTH                                 
055600             ELSE                                                         
055700               IF WS-DAPUBL-MONTH = 06                                    
055800                 MOVE 12 TO WS-DAPUBL-MONTH                               
055900               ELSE                                                       
056000                 IF WS-DAPUBL-MONTH = 07                                  
056100                   ADD +1 TO WS-DAPUBL-YEAR                               
056200                   MOVE 01 TO WS-DAPUBL-MONTH                             
056300                 ELSE                                                     
056400                   IF WS-DAPUBL-MONTH = 08                                
056500                     ADD +1 TO WS-DAPUBL-YEAR                             
056600                     MOVE 02 TO WS-DAPUBL-MONTH                           
056700                   ELSE                                                   
056800                     IF WS-DAPUBL-MONTH = 09                              
056900                       ADD +1 TO WS-DAPUBL-YEAR                           
057000                       MOVE 03 TO WS-DAPUBL-MONTH                         
057100                     ELSE                                                 
057200                       IF WS-DAPUBL-MONTH = 10                            
057300                         ADD +1 TO WS-DAPUBL-YEAR                         
057400                         MOVE 04 TO WS-DAPUBL-MONTH                       
057500                       ELSE                                               
057600                         IF WS-DAPUBL-MONTH = 11                          
057700                           ADD +1 TO WS-DAPUBL-YEAR                       
057800                           MOVE 05 TO WS-DAPUBL-MONTH                     
057900                         ELSE                                             
058000                           IF WS-DAPUBL-MONTH = 12                        
058100                             ADD +1 TO WS-DAPUBL-YEAR                     
058200                             MOVE 06 TO WS-DAPUBL-MONTH                   
058300                           END-IF                                         
058400                         END-IF                                           
058500                       END-IF                                             
058600                     END-IF                                               
058700                   END-IF                                                 
058800                 END-IF                                                   
058900               END-IF                                                     
059000             END-IF                                                       
059100           END-IF                                                         
059200         END-IF                                                           
059300       END-IF                                                             
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 S04-WS-TIFINLV-6MONTHS SECTION.                                          
059800*    ADDERA 6 MÅNADER TILL PUBVECKA CDC                                   
059900     MOVE WS-TIFINLV       TO WS2-TIFINLV                                 
060000     MOVE WS2-TIFINLV-AAVV TO DATUM-AAVV                                  
060100     MOVE +26              TO ANTAL                                       
060200     CALL W009VADD USING DATUM-AAVV ANTAL                                 
060300     MOVE DATUM-AAVV TO WS2-TIFINLV-AAVV                                  
060400                                                                          
060500*    RÄKNA OM PUBVECKA CDC TILL ÅÅMMDD                                    
060600     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
060700     MOVE WS2-TIFINLV        TO DAT-I-TIDATUM                             
060800                                                                          
060900     CALL WDATKONV USING DAT-KDDATFORM                                    
061000                         DAT-I-TIDATUM                                    
061100                         DAT-O-TIDATUM                                    
061200                         DAT-KDSVAR                                       
061300                                                                          
061400     IF DAT-KDSVAR-OK                                                     
061500       MOVE DAT-TIAAMMDD     TO WS3-TIFINLV                               
061600     ELSE                                                                 
061700       MOVE 'SVAR WDATKONV   I S04 SECTION'                               
061800                             TO FELTEXT-STR                               
061900       DISPLAY FELTEXT                                                    
062000       PERFORM S99-ABEND                                                  
062100     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
063900 S06-CHECK-AUTOREF SECTION.                                               
064000                                                                          
064100     IF  DCS-KDDC = 'S'                                                   
064200     AND WS-KDERS = 09                                                    
064300         IF SLAG-FLREFBEO > SPACES                                        
064400            MOVE STOPP   TO UT-FLREFBEO                                   
064500         END-IF                                                           
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900                                                                          
065000 S12-SKRIV-W27181 SECTION.                                                
065100                                                                          
065200     WRITE UT-POST FROM UT-AREA                                           
065300                                                                          
065400     MOVE 'W27181'   TO POSTSUM-FDNAMN                                    
065500     MOVE 'W27181D1' TO POSTSUM-DDNAMN2                                   
065600     CALL POSTSUM USING POSTSUM-PARM                                      
065700     .                                                                    
065800     EJECT                                                                
065810 S20-GET-KLASS     SECTION.                                               
065820                                                                          
065830     MOVE SLAG-IDDC     TO W-IDDC-2501                                    
065840     MOVE SLAG-IDREFTAB TO W-IDREFTAB                                     
065841     MOVE 0             TO WS-KLASS                                       
065842                                                                          
065850     PERFORM IMS-GU-2502                                                  
065851                                                                          
065852     IF NOT SEGMENT-FINNS                                                 
065853        MOVE ZERO       TO W-IDREFTAB                                     
065854        PERFORM IMS-GU-2502                                               
065855     END-IF                                                               
065856                                                                          
065857     IF SEGMENT-FINNS                                                     
065858       PERFORM S21-GET-BESPRIS                                            
065859                                                                          
065860       MOVE 1            TO IX                                            
065900                                                                          
065901       PERFORM UNTIL  2502-PRARTBES (IX) >= WS-PRARTBES                   
065902         ADD 1 TO IX                                                      
065903       END-PERFORM                                                        
065904       MOVE IX TO WS-KLASS                                                
066005                                                                          
066014     ELSE                                                                 
066015       MOVE '2502 SEGMENT ERROR' TO FELTEXT-STR                           
066016       DISPLAY FELTEXT                                                    
066017       PERFORM S99-ABEND                                                  
066018     END-IF                                                               
066019                                                                          
066047     .                                                                    
066048                                                                          
066049 S21-GET-BESPRIS  SECTION.                                                
066050                                                                          
066051     IF DCS-CHINA OR DCS-NDC-NA                                           
066052        MOVE W-IDARTNR    TO W-IDARTNR-US                                 
066053        MOVE DCS-IDLANDX2 TO W-IDLAND-US                                  
066054        PERFORM IMS-GU-WDK712                                             
066055        IF SEGMENT-SAKNAS                                                 
066056           MOVE ZERO TO LART-PRMATRL                                      
066057        END-IF                                                            
066058        MOVE LART-PRMATRL     TO WS-PRARTBES                              
066059     ELSE                                                                 
066060        MOVE CLAG-PRARTSTD    TO WS-PRARTBES                              
066061     END-IF                                                               
066062     .                                                                    
066070     EJECT                                                                
066071                                                                          
066072 S99-ABEND SECTION.                                                       
066073                                                                          
066100     MOVE 'S' TO POSTSUM-OPKOD                                            
066200     CALL POSTSUM USING POSTSUM-PARM                                      
066300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
066400     .                                                                    
066500     EJECT                                                                
066600* --- IMS SEKTIONER ---                                                   
066700     SKIP3                                                                
066710 IMS-GU-2502 SECTION.                                                     
066720     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
066730          DELIMITED BY SIZE INTO SSA1                                     
066740     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
066750          DELIMITED BY SIZE INTO SSA2                                     
066760     MOVE '  GE' TO GODK-STATUSKODER                                      
066770     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WL250111 SSA1 SSA2             
066780     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
066790     PERFORM IMS-STATUSKONTROLL                                           
066791     .                                                                    
066792     SKIP2                                                                
066800 IMS-GU-WDK601 SECTION.                                                   
066900                                                                          
067000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
067100          DELIMITED BY SIZE INTO SSA1                                     
067200     MOVE '  ' TO GODK-STATUSKODER                                        
067300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
068400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
068500     PERFORM IMS-STATUSKONTROLL                                           
068600     .                                                                    
068700     EJECT                                                                
068800 IMS-GU-WDK611 SECTION.                                                   
068900                                                                          
069000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
069100          DELIMITED BY SIZE INTO SSA1                                     
069200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
069300          DELIMITED BY SIZE INTO SSA2                                     
069400     MOVE '  GE' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
069600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     EJECT                                                                
070000                                                                          
070100 IMS-GN-WDK7 SECTION.                                                     
070200                                                                          
070300     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
070400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
070500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
070600     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     EJECT                                                                
070900 IMS-GU-WDK712 SECTION.                                                   
071000                                                                          
071100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-US-X ')'                      
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     STRING 'WDK712  (IDLAND   =' W-IDLAND-US-X ')'                       
071400          DELIMITED BY SIZE INTO SSA2                                     
071500     MOVE '  GE' TO GODK-STATUSKODER                                      
071600     CALL CBLTDLI USING GU WDK7US-PCB DLI-IO-WDK712 SSA1 SSA2             
071700     MOVE WDK7US-STATUS-CODE TO STATUS-WS                                 
071800     PERFORM IMS-STATUSKONTROLL                                           
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200 IMS-GU-WDL711 SECTION.                                                   
072300                                                                          
072400     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
072500          DELIMITED BY SIZE INTO SSA1                                     
072600     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
072700          DELIMITED BY SIZE INTO SSA2                                     
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
073000     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     EJECT                                                                
073400                                                                          
073500 IMS-GU-WDB601    SECTION.                                                
073600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
073700          DELIMITED BY SIZE INTO SSA1                                     
073800     MOVE '  ' TO GODK-STATUSKODER                                        
073900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
074000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
074100     PERFORM IMS-STATUSKONTROLL                                           
074200     .                                                                    
074300     EJECT                                                                
074400 IMS-STATUSKONTROLL SECTION.                                              
074500                                                                          
074600     SET STATUS-IX TO 1                                                   
074700     SEARCH GODK-STATUS                                                   
074800       AT END                                                             
074900         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
075000           DELIMITED BY SIZE INTO FELTEXT-STR                             
075100         DISPLAY FELTEXT                                                  
075200         CALL FELLOG                                                      
075300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075400         CONTINUE                                                         
075500     END-SEARCH                                                           
075600     .                                                                    
075700     EJECT                                                                
075800*    -COPY WY2000P1                                                       
