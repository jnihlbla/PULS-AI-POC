000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2728100.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   OKT 2015.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET TAR FRAM DE ARTIKLAR DÄR FLREFBEO                     
001200*        SKA AKTIVERAS FÖR CDC                                            
001300*                                                                         
001400*        PROGRAM RUNS WEEKLY                                              
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDK6                                       
001700*                              WDL8                                       
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
003300     SELECT W27281                     ASSIGN TO W27281D1.                
003400*          --- UT-FIL                                                     
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000                                                                          
004100                                                                          
004200 FD  W27281                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W27281  -PRE UT-    -L.                                   
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
005900 77  IDPGM                       PIC X(8)    VALUE 'W2728100'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006010 77  YES                         PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  AKTIV                       PIC X       VALUE 'A'.                   
006300 77  CDC-11                      PIC X(2)    VALUE '11'.                  
006400                                                                          
006500*    --- INDEX SAMT MAX-INDEX                                             
006600 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006700 77  IX                          PIC 9(2)    VALUE ZERO.                  
006800 77  IX-2                        PIC 9(2)    VALUE ZERO.                  
006900                                                                          
007000*    --- SWITCHAR                                                         
007100 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
007200                                                                          
007300                                                                          
007400 01  ESCLOCK-SW                  PIC X       VALUE 'N'.                   
007500     88  PROGNOS-AER-LAAST                   VALUE 'J'.                   
007600     88  PROGNOS-AER-FAST                    VALUE 'A'.                   
007700     88  PROGNOS-EJ-LAAST                    VALUE 'N'.                   
007800                                                                          
007810 01  ORDERHIT-SW                 PIC X       VALUE 'J'.                   
007820     88  NO-ORDER-HITS                       VALUE 'N'.                   
007830                                                                          
007900 01  SW-SALES-1YEAR              PIC X       VALUE 'J'.                   
008000     88  NO-SALE                             VALUE 'N'.                   
008100                                                                          
008200*    --- ARBETSFÄLT                                                       
008300 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
008400 01  ARBETSFAELT.                                                         
008500     03  WS-ANTAL-KVOT           PIC 9(9)    VALUE ZERO.                  
008600     03  WS-ART-KDERS-UTG        PIC 9(2)    VALUE ZERO.                  
008700     03  WS-ART-IDFKNGRP         PIC 9(4)    VALUE ZERO.                  
008800     03  WS-VECKA                PIC 9(2)    VALUE ZERO.                  
008900     03  WS-FLEXCP1-REFBEO       PIC 9(1)    VALUE ZERO.                  
009000     03  WS-FLEXCP2-REFBEO       PIC 9(1)    VALUE ZERO.                  
009100     03  WS-FLEXCP4-REFBEO       PIC 9(1)    VALUE ZERO.                  
009200     03  WS-KLASS                PIC 9(1)    VALUE ZERO.                  
009300                                                                          
009400     EJECT                                                                
009500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009600 01  FILLER REDEFINES DAGENS-DATUM.                                       
009700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010000     EJECT                                                                
010100* ARBETSFÄLT FÖR ATT KONTROLLERA PB-LSÅNING, ESC-LÅS MM.                  
010200 01  WS-TIPBLOCK-DATUM.                                                   
010300     03  TIPBLOCK-DATUM-20         PIC 9(2) VALUE 20.                     
010400     03  TIPBLOCK-DATUM-6LONG      PIC 9(6).                              
010500 01  DA-DAGENS-DATUM.                                                     
010600     03  DAGENS-DATUM-20         PIC 9(2) VALUE 20.                       
010700     03  DAGENS-DATUM-6LONG      PIC 9(6).                                
010800 01  WS-INNEV-AAR.                                                        
010900     03  INNEV-AAR-SEKEL         PIC 9(2) VALUE 20.                       
011000     03  INNEV-AAR-AAR           PIC 9(2).                                
011100 01  WS-AAR                      PIC 9(2).                                
011200 01  WS-DAPUBL.                                                           
011300     03  WS-DAPUBL-YEAR          PIC 9(4).                                
011400     03  WS-DAPUBL-MONTH         PIC 9(2).                                
011500     03  WS-DAPUBL-DAY           PIC 9(2).                                
011600 01  WS-TIPBDAT-AAMMDD           PIC 9(6).                                
011800 01  WS-DAGENS-DAT-1YEAR         PIC 9(6).                                
011801 01  WS-TIFINLV                  PIC S9(5)   VALUE ZERO.                  
011810 01  WS2-TIFINLV                 PIC 9(5).                                
011820 01  FILLER REDEFINES WS2-TIFINLV.                                        
011830     03  WS2-TIFINLV-AAVV        PIC 9(4).                                
011840     03  FILLER                  PIC 9(1).                                
011850 01  WS3-TIFINLV                 PIC 9(6).                                
011900                                                                          
012000     EJECT                                                                
012100 01  DYNAMISKA-SUBPROGRAM.                                                
012200*                                                                         
012300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
013000     SKIP2                                                                
013100*    --- PARAMETRAR TILL ABEND                                            
013200                                                                          
013300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013500     SKIP2                                                                
013600 01  FELTEXT.                                                             
013700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL DATKORT                                          
014100*                                                                         
014200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27281'.              
014300     SKIP2                                                                
014400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014500     SKIP2                                                                
014600*01  -COPY WDATKORT                                                       
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL POSTSUM                                          
014900*                                                                         
015000*01  -COPY W0005   -PRE  POSTSUM-                                         
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL WDATKONV                                         
015300*                                                                         
015400*01  -COPY WDATAREA                                                       
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL W009VADD                                         
015700*                                                                         
015800 01  W009VADD-AREA.                                                       
015900     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
016000     03  ANTAL                   PIC S9(3) COMP-3.                        
016100*                                                                         
016200     EJECT                                                                
016300*    -COPY WWDC99                                                         
016400     SKIP3                                                                
016500 01  UT-AREA-START              PIC X(24)   VALUE                         
016600                                 'UT-AREA-START  '.                       
016700     SKIP2                                                                
016800                                                                          
016900*01  AREA -COPY W27281     -PRE UT-                                       
017000     EJECT                                                                
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200                                                                          
017300     SKIP3                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900                                                                          
018000     03  W-TIAAAA-X.                                                      
018100         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
018200                                                                          
018300     03  W-IDARTNR-US-X.                                                  
018400         05  W-IDARTNR-US        PIC S9(9)   VALUE ZERO COMP-3.           
018500                                                                          
018600     03  W-IDLAND-US-X.                                                   
018700         05  W-IDLAND-US         PIC X(2)    VALUE SPACE.                 
018800                                                                          
018900     03  W-IDDC-X.                                                        
019000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019100                                                                          
019200     03  W-IDDC-2-X.                                                      
019300         05  W-IDDC-2            PIC X(2)    VALUE SPACE.                 
019400                                                                          
019500     03  W-IDDC-B6-X.                                                     
019600         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
019700                                                                          
019800     03  W-KDSEGKEY-X.                                                    
019900         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
020000                                                                          
020100     03  W-WDGXKEY-X.                                                     
020200          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
020300          05 W-IDDC-2501         PIC X(2)    VALUE SPACE.                 
020400          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
020500                                                                          
020600     03  W-IDREFTAB-X.                                                    
020700         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
020800*                                                                         
020900     SKIP2                                                                
021000*    --- STATUS-KOD FRÅN IMS                                              
021100 01  STATUS-WS                   PIC XX.                                  
021200     88  SEGMENT-FINNS                       VALUE '  '.                  
021300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021500     SKIP2                                                                
021600 01  GODK-STATUSKODER.                                                    
021700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021800     SKIP3                                                                
021900 01  SSA1                        PIC X(64).                               
022000 01  SSA2                        PIC X(64).                               
022100 01  SSA3                        PIC X(64).                               
022200     EJECT                                                                
022300*    --- IMS FUNKTIONSKODER                                               
022400*01  -COPY W0003                                                          
022500     EJECT                                                                
022600*    ---  DLI INPUT-OUTPUT AREA                                           
022700                                                                          
022800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
022900     SKIP3                                                                
023000                                                                          
023100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
023200 01  DLI-IO-WDK601.                                                       
023300*    03  -COPY WDK601                                                     
023400     EJECT                                                                
023500                                                                          
023600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
023700 01  DLI-IO-WDK611.                                                       
023800*    03  -COPY WDK611                                                     
023900     EJECT                                                                
024000                                                                          
024100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
024200 01  DLI-IO-WDK629.                                                       
024300*    03  -COPY WDK629                                                     
024400     EJECT                                                                
024500                                                                          
024600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
024700 01  DLI-IO-WDL711.                                                       
024800*    03  -COPY WDL711                                                     
024900     EJECT                                                                
025000                                                                          
025100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
025200 01  DLI-IO-WDK711.                                                       
025300*    03  -COPY WDK711                                                     
025400     EJECT                                                                
025500                                                                          
025600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
025700 01  DLI-IO-WDK712.                                                       
025800*    03  -COPY WDK712                                                     
025900     EJECT                                                                
026000                                                                          
026100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722'.                      
026200 01  DLI-IO-WDK722.                                                       
026300*    03  -COPY WDK722                                                     
026400     EJECT                                                                
026500                                                                          
026600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL801'.                      
026700 01  DLI-IO-WDL801.                                                       
026800*    03  -COPY WDL801   -PRE WDL8-                                        
026900     EJECT                                                                
027000                                                                          
027100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL811'.                      
027200 01  DLI-IO-WDL811.                                                       
027300*    03  -COPY WDL811                                                     
027400     EJECT                                                                
027500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027600 01   DLI-IO-AREA-B601.                                                   
027700*     03  -COPY WDB601                                                    
027800     EJECT                                                                
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL250111'.                    
028000 01  DLI-IO-WL250111.                                                     
028100*     03  -COPY WDGX2502                                                  
028200    EJECT                                                                 
028300                                                                          
028400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
028500                                                                          
028600 LINKAGE SECTION.                                                         
028700                                                                          
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE WDK7-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008  -PRE WDK6-                                              
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE WDL8X-                                             
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008  -PRE WDL8-                                              
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE WDL7-                                              
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008      -PRE WDB6-                                          
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008      -PRE WDR2-                                          
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000 PROCEDURE DIVISION  USING WDK7-PCB WDK6-PCB WDL8X-PCB                    
031100                           WDL8-PCB WDL7-PCB WDB6-PCB WDR2-PCB.           
031200     ENTRY 'DLITCBL' USING WDK7-PCB WDK6-PCB WDL8X-PCB                    
031300                           WDL8-PCB WDL7-PCB WDB6-PCB WDR2-PCB.           
031400                                                                          
031500     PERFORM A-INIT                                                       
031600     MOVE '11'               TO W-IDDC-B6                                 
031700     PERFORM IMS-GU-WDB601                                                
031701                                                                          
031707       MOVE DCS-FLEXCP1-REFBEO TO WS-FLEXCP1-REFBEO                       
031708       MOVE DCS-FLEXCP2-REFBEO TO WS-FLEXCP2-REFBEO                       
031709       MOVE DCS-FLEXCP4-REFBEO TO WS-FLEXCP4-REFBEO                       
031710                                                                          
031720      PERFORM IMS-GN-WDK629                                               
031730      PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                        
031740       PERFORM IMS-GNP-WDK611                                             
031750       PERFORM IMS-GNP-WDK601                                             
031760       MOVE ART-IDARTNR                  TO W-IDARTNR                     
031770       MOVE ART-KDPRODSL                 TO TEST-KDPRODSL                 
031771       MOVE ART-TIFINLV                  TO WS-TIFINLV                    
031780       MOVE CREF-IDDC-REF                TO WS-IDDC                       
031790                                                                          
031800                                                                          
031900*                                                                         
032000       IF CREF-FLREFBEO = 'S'                                             
032100         CONTINUE                                                         
032200       ELSE                                                               
032300         IF CREF-FLREFBEO = JA                                            
032400*          TO SET REFILL FLAG AS 'N'                                      
032500           PERFORM D-UPD-REFILL-FLAG-J-TO-N                               
032600*          PERFORM S05-SALES-1YEAR                                        
032700         ELSE                                                             
032800           IF ( KDPRODSL-WHEELS OR                                        
032900                 KDPRODSL-TOOLS OR                                        
033000                 KDPRODSL-EMB  OR                                         
033100                 KDPRODSL-BRANDON )  OR                                   
033200                 ART-IDLEVNR = 'BQ8VA' OR                                 
033300                 ART-IDLEVNR = '10987' OR                                 
033400                 CLAG-KDUART = 'S'                                        
033500             CONTINUE                                                     
033600           ELSE                                                           
033700             IF CREF-KDREFSTA = AKTIV                                     
033800             AND CREF-FLREFBEO = NEJ                                      
033900             AND CREF-FLREFILL = JA                                       
034000             AND CREF-IDDC-REF > SPACES                                   
034100             AND ART-KDERS-UTG = 0                                        
034200             AND CLAG-KDERS  < 20                                         
034300                PERFORM S01-KOLLA-PROGNOS-LAASNING                        
034400                IF PROGNOS-EJ-LAAST                                       
034500*                  TO SET REFILL FLAG AS 'J'                              
034600                   PERFORM E-UPD-REFILL-FLAG-N-TO-J                       
034700                END-IF                                                    
034800             END-IF                                                       
034900           END-IF                                                         
035000         END-IF                                                           
035100       END-IF                                                             
035200                                                                          
035300       PERFORM IMS-GN-WDK629                                              
035400     END-PERFORM                                                          
035500                                                                          
035600     PERFORM Z-FINIT                                                      
035700                                                                          
035800     MOVE ZERO TO RETURN-CODE                                             
035900     GOBACK                                                               
036000     .                                                                    
036100     EJECT                                                                
036200                                                                          
036300 A-INIT SECTION.                                                          
036400                                                                          
036500     OPEN OUTPUT W27281                                                   
036600                                                                          
036700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036800     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
036900                         INNEV-AAR-AAR                                    
037000                         WS-AAR                                           
037100     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
037200     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
037300     MOVE D-VECKA     TO WS-VECKA                                         
037400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037500     MOVE DAGENS-DATUM TO DAGENS-DATUM-6LONG                              
037600     COMPUTE WS-DAGENS-DAT-1YEAR =                                        
037700             DAGENS-DATUM-6LONG - 10000                                   
037800     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100                                                                          
038200 D-UPD-REFILL-FLAG-J-TO-N  SECTION.                                       
038300                                                                          
038400*    TO GET NO. OF ORDER HITS                                             
038500       PERFORM S02-GET-ORDER-HITS                                         
038600                                                                          
038610     IF NO-ORDER-HITS                                                     
038620       CONTINUE                                                           
038630     ELSE                                                                 
038700**   VALIDATE REFO FLAGS FROM 4403                                        
038800       IF WS-ANTAL-KVOT < WS-FLEXCP4-REFBEO OR                            
038900          ( DCS-FLEXCP3-REFBEO = NEJ AND                                  
039000            KDPRODSL-ACC )                                                
039100                                                                          
039200         MOVE W-IDARTNR  TO UT-IDARTNR                                    
039300         MOVE 'N'        TO UT-FLREFBEO                                   
039400         PERFORM S12-SKRIV-W27281                                         
039500       ELSE                                                               
039600         PERFORM S20-GET-KLASS                                            
039700         IF WS-KLASS     > WS-FLEXCP2-REFBEO                              
039800            MOVE W-IDARTNR  TO UT-IDARTNR                                 
039900            MOVE 'N'        TO UT-FLREFBEO                                
040000            PERFORM S12-SKRIV-W27281                                      
040100         END-IF                                                           
040200       END-IF                                                             
040210     END-IF                                                               
040300     .                                                                    
040400                                                                          
040500 E-UPD-REFILL-FLAG-N-TO-J SECTION.                                        
040600                                                                          
040700*    TO GET NO. OF ORDER HITS                                             
040800       PERFORM S02-GET-ORDER-HITS                                         
040900                                                                          
041000*VALIDATE REFO FLAGS FROM 4403                                            
041100     IF WS-ANTAL-KVOT > WS-FLEXCP1-REFBEO                                 
041200        PERFORM S20-GET-KLASS                                             
041300        IF WS-KLASS      <= WS-FLEXCP2-REFBEO                             
041400           IF KDPRODSL-ACC                                                
041500              IF DCS-FLEXCP3-REFBEO = JA                                  
041600                 MOVE W-IDARTNR TO UT-IDARTNR                             
041700                 MOVE JA        TO UT-FLREFBEO                            
041800                 PERFORM S12-SKRIV-W27281                                 
041900              END-IF                                                      
042000           ELSE                                                           
042100              MOVE W-IDARTNR TO UT-IDARTNR                                
042200              MOVE JA        TO UT-FLREFBEO                               
042300              PERFORM S12-SKRIV-W27281                                    
042400           END-IF                                                         
042500        END-IF                                                            
042600     END-IF                                                               
042700     .                                                                    
042800                                                                          
042900                                                                          
043000 Z-FINIT SECTION.                                                         
043100                                                                          
043200     CLOSE W27281                                                         
043300                                                                          
043400     MOVE 'S' TO POSTSUM-OPKOD                                            
043500     CALL POSTSUM USING POSTSUM-PARM                                      
043600     .                                                                    
043700     EJECT                                                                
043800 S01-KOLLA-PROGNOS-LAASNING SECTION.                                      
043900                                                                          
044000     MOVE NEJ TO ESCLOCK-SW                                               
044100                                                                          
044200                                                                          
044300* KONTROLLERA OM PB ÄR MANUELLT LÅST                                      
044400     IF CLAG-TIPBDAT > ZERO                                               
044500       IF CLAG-TIPBDAT < 50000                                            
044600         MOVE CLAG-TIPBDAT     TO DAT-I-TIDATUM                           
044700         MOVE 'AAVVD'          TO DAT-KDDATFORM                           
044800         CALL WDATKONV USING DAT-KDDATFORM                                
044900                             DAT-I-TIDATUM                                
045000                             DAT-O-TIDATUM                                
045100                             DAT-KDSVAR                                   
045200                                                                          
045300         IF DAT-KDSVAR-OK                                                 
045400            MOVE DAT-TIAAMMDD TO WS-TIPBDAT-AAMMDD                        
045500         ELSE                                                             
045600            MOVE 'FEL FRÅN WDATKONV I S01-' TO FELTEXT-STR                
045700            DISPLAY FELTEXT                                               
045800            PERFORM S99-ABEND                                             
045900         END-IF                                                           
046000       ELSE                                                               
046100         MOVE ZERO       TO WS-TIPBDAT-AAMMDD                             
046200       END-IF                                                             
046300     ELSE                                                                 
046400       MOVE ZERO         TO WS-TIPBDAT-AAMMDD                             
046500     END-IF                                                               
046600     MOVE WS-TIPBDAT-AAMMDD  TO TMP1-YYMMDD                               
046700     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
046800     PERFORM WY2000P1                                                     
046900     IF  (TMP1-YYMMDD > TMP2-YYMMDD)                                      
047000        MOVE 'A' TO ESCLOCK-SW                                            
047100     ELSE                                                                 
047200*    OM INTE ART. ÄR MANUELLT LÅST SÅ KAN DEN VARA ESC-LÅST               
047300                                                                          
047400*    KONTROLLERA OM ARTIKLEN ÄR ESC-LÅST                                  
047500        MOVE CLAG-TIPBLOCK TO TIPBLOCK-DATUM-6LONG                        
047600        IF TIPBLOCK-DATUM-6LONG > ZERO                                    
047700           IF WS-TIPBLOCK-DATUM >= DA-DAGENS-DATUM                        
047800              MOVE JA TO ESCLOCK-SW                                       
047900           END-IF                                                         
048000        END-IF                                                            
048100                                                                          
048200*    CHECK IF PUBWEEK SENDING DC IS WITHIN 1 YEAR                         
048300        MOVE W-IDARTNR       TO W-IDARTNR-US                              
048400        IF NDC-US                                                         
048500          MOVE 'US'          TO W-IDLAND-US                               
048600        END-IF                                                            
048700        IF NDC-CN                                                         
048800          MOVE 'CN'          TO W-IDLAND-US                               
048900        END-IF                                                            
049000        PERFORM IMS-GU-WDK712                                             
049100        IF SEGMENT-SAKNAS                                                 
049200           MOVE ZERO TO LART-DAPUBL                                       
049300        END-IF                                                            
049400        IF LART-DAPUBL > ZERO                                             
049500           PERFORM S03-DAPUBL-6MONTH                                      
049600           IF WS-DAPUBL >= DA-DAGENS-DATUM                                
049700              MOVE JA TO ESCLOCK-SW                                       
049800           END-IF                                                         
049900        END-IF                                                            
049910*    KONTROLLERA OM PUBVECKA CDC ÄR INOM 6 MÅNADER                        
049920        IF WS-TIFINLV > ZERO                                              
049930           IF WS-TIFINLV > 50000                                          
049940             MOVE ZERO       TO TMP1-YYMMDD                               
049950           ELSE                                                           
049960             PERFORM S04-WS-TIFINLV-6MONTHS                               
049970             MOVE WS3-TIFINLV TO TMP1-YYMMDD                              
049980           END-IF                                                         
049990           MOVE DAGENS-DATUM TO TMP2-YYMMDD                               
049991           IF  TMP1-YYMMDD > TMP2-YYMMDD                                  
049992              MOVE JA TO ESCLOCK-SW                                       
049993           END-IF                                                         
049994        END-IF                                                            
050000     END-IF                                                               
050100                                                                          
050200     IF ESCLOCK-SW = 'J' OR 'A'                                           
050300       CONTINUE                                                           
050400     ELSE                                                                 
050500       MOVE CREF-IDDC-REF   TO W-IDDC-2                                   
050600       PERFORM IMS-GU-WDK722                                              
050700       IF SEGMENT-FINNS                                                   
050800         IF XLAG-TIREFSTO-LOC = ZERO                                      
050900           CONTINUE                                                       
051000         ELSE                                                             
051100           IF XLAG-TIREFSTO-LOC <= DAGENS-DATUM-6LONG                     
051200             CONTINUE                                                     
051300           ELSE                                                           
051400             MOVE JA TO ESCLOCK-SW                                        
051500           END-IF                                                         
051600         END-IF                                                           
051700       END-IF                                                             
051800     END-IF                                                               
051900                                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 S02-GET-ORDER-HITS SECTION.                                              
052300                                                                          
052310     MOVE YES    TO ORDERHIT-SW                                           
052400     MOVE WS-AAR         TO INNEV-AAR-AAR                                 
052500     MOVE WS-INNEV-AAR   TO W-TIAAAA                                      
054000     INITIALIZE WS-ANTAL-KVOT                                             
054100                                                                          
054200     PERFORM IMS-GU-WDL811                                                
054300     IF SEGMENT-FINNS                                                     
054400       IF CREF-FLREFILL = JA                                              
054500                                                                          
054600          MOVE ZERO          TO WS-ANTAL-KVOT                             
054700          MOVE +1       TO IX                                             
054800                                                                          
054900          PERFORM UNTIL IX > 53                                           
055000             ADD AAR-KVOT-PROG (IX)                                       
055100                            TO WS-ANTAL-KVOT                              
055200             ADD AAR-KVOT-REFILL (IX)                                     
055300                            TO WS-ANTAL-KVOT                              
055400             ADD 1           TO IX                                        
055500          END-PERFORM                                                     
055600                                                                          
055700          COMPUTE INNEV-AAR-AAR = INNEV-AAR-AAR - 1                       
055800          MOVE WS-INNEV-AAR TO W-TIAAAA                                   
055900          PERFORM IMS-GU-WDL811                                           
056000          MOVE WS-VECKA      TO IX                                        
056100          IF SEGMENT-FINNS                                                
056200            PERFORM UNTIL IX > 53                                         
056300               ADD AAR-KVOT-PROG (IX)                                     
056400                              TO WS-ANTAL-KVOT                            
056500               ADD AAR-KVOT-REFILL (IX)                                   
056600                              TO WS-ANTAL-KVOT                            
056700               ADD 1         TO IX                                        
056800            END-PERFORM                                                   
056900          END-IF                                                          
057000       END-IF                                                             
057010     ELSE                                                                 
057020        MOVE NEJ   TO ORDERHIT-SW                                         
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400                                                                          
057500 S03-DAPUBL-6MONTH  SECTION.                                              
057600                                                                          
057700     MOVE LART-DAPUBL TO WS-DAPUBL                                        
057800                                                                          
057900     IF WS-DAPUBL-MONTH = 01                                              
058000       MOVE 07 TO WS-DAPUBL-MONTH                                         
058100     ELSE                                                                 
058200       IF WS-DAPUBL-MONTH = 02                                            
058300         MOVE 08 TO WS-DAPUBL-MONTH                                       
058400       ELSE                                                               
058500         IF WS-DAPUBL-MONTH = 03                                          
058600           MOVE 09 TO WS-DAPUBL-MONTH                                     
058700         ELSE                                                             
058800           IF WS-DAPUBL-MONTH = 04                                        
058900             MOVE 10 TO WS-DAPUBL-MONTH                                   
059000           ELSE                                                           
059100             IF WS-DAPUBL-MONTH = 05                                      
059200               MOVE 11 TO WS-DAPUBL-MONTH                                 
059300             ELSE                                                         
059400               IF WS-DAPUBL-MONTH = 06                                    
059500                 MOVE 12 TO WS-DAPUBL-MONTH                               
059600               ELSE                                                       
059700                 IF WS-DAPUBL-MONTH = 07                                  
059800                   ADD +1 TO WS-DAPUBL-YEAR                               
059900                   MOVE 01 TO WS-DAPUBL-MONTH                             
060000                 ELSE                                                     
060100                   IF WS-DAPUBL-MONTH = 08                                
060200                     ADD +1 TO WS-DAPUBL-YEAR                             
060300                     MOVE 02 TO WS-DAPUBL-MONTH                           
060400                   ELSE                                                   
060500                     IF WS-DAPUBL-MONTH = 09                              
060600                       ADD +1 TO WS-DAPUBL-YEAR                           
060700                       MOVE 03 TO WS-DAPUBL-MONTH                         
060800                     ELSE                                                 
060900                       IF WS-DAPUBL-MONTH = 10                            
061000                         ADD +1 TO WS-DAPUBL-YEAR                         
061100                         MOVE 04 TO WS-DAPUBL-MONTH                       
061200                       ELSE                                               
061300                         IF WS-DAPUBL-MONTH = 11                          
061400                           ADD +1 TO WS-DAPUBL-YEAR                       
061500                           MOVE 05 TO WS-DAPUBL-MONTH                     
061600                         ELSE                                             
061700                           IF WS-DAPUBL-MONTH = 12                        
061800                             ADD +1 TO WS-DAPUBL-YEAR                     
061900                             MOVE 06 TO WS-DAPUBL-MONTH                   
062000                           END-IF                                         
062100                         END-IF                                           
062200                       END-IF                                             
062300                     END-IF                                               
062400                   END-IF                                                 
062500                 END-IF                                                   
062600               END-IF                                                     
062700             END-IF                                                       
062800           END-IF                                                         
062900         END-IF                                                           
063000       END-IF                                                             
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063410 S04-WS-TIFINLV-6MONTHS SECTION.                                          
063420*    ADDERA 6 MÅNADER TILL PUBVECKA CDC                                   
063430     MOVE WS-TIFINLV       TO WS2-TIFINLV                                 
063440     MOVE WS2-TIFINLV-AAVV TO DATUM-AAVV                                  
063450     MOVE +26              TO ANTAL                                       
063460     CALL W009VADD USING DATUM-AAVV ANTAL                                 
063470     MOVE DATUM-AAVV TO WS2-TIFINLV-AAVV                                  
063480                                                                          
063490*    RÄKNA OM PUBVECKA CDC TILL ÅÅMMDD                                    
063491     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
063492     MOVE WS2-TIFINLV        TO DAT-I-TIDATUM                             
063493                                                                          
063494     CALL WDATKONV USING DAT-KDDATFORM                                    
063495                         DAT-I-TIDATUM                                    
063496                         DAT-O-TIDATUM                                    
063497                         DAT-KDSVAR                                       
063498                                                                          
063499     IF DAT-KDSVAR-OK                                                     
063500       MOVE DAT-TIAAMMDD     TO WS3-TIFINLV                               
063501     ELSE                                                                 
063502       MOVE 'SVAR WDATKONV   I S04 SECTION'                               
063503                             TO FELTEXT-STR                               
063504       DISPLAY FELTEXT                                                    
063505       PERFORM S99-ABEND                                                  
063506     END-IF                                                               
063507     .                                                                    
063508     EJECT                                                                
063509                                                                          
063510 S05-SALES-1YEAR SECTION.                                                 
063600     MOVE JA    TO SW-SALES-1YEAR                                         
063700     IF WS-DAGENS-DAT-1YEAR > CREF-TIREFEFT                               
063800       MOVE NEJ    TO SW-SALES-1YEAR                                      
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200                                                                          
064300 S12-SKRIV-W27281 SECTION.                                                
064400                                                                          
064500     WRITE UT-POST FROM UT-AREA                                           
065230                                                                          
065240     MOVE 'W27281'   TO POSTSUM-FDNAMN                                    
065250     MOVE 'W27281D1' TO POSTSUM-DDNAMN2                                   
065260     CALL POSTSUM USING POSTSUM-PARM                                      
065270     .                                                                    
065280     EJECT                                                                
065290 S20-GET-KLASS         SECTION.                                           
065300                                                                          
065400     MOVE CDC-11            TO W-IDDC-2501                                
065500     MOVE CREF-IDREFTAB TO W-IDREFTAB                                     
065600     MOVE 0                 TO WS-KLASS                                   
065700                                                                          
065800     PERFORM IMS-GU-2502                                                  
065900                                                                          
066000     IF NOT SEGMENT-FINNS                                                 
066100        MOVE ZERO           TO W-IDREFTAB                                 
066200        PERFORM IMS-GU-2502                                               
066300     END-IF                                                               
066400                                                                          
066500     IF SEGMENT-FINNS                                                     
066600       MOVE 1  TO IX                                                      
066700                                                                          
066800       PERFORM UNTIL 2502-PRARTBES (IX) >= CLAG-PRARTSTD                  
066900         ADD 1 TO IX                                                      
067000       END-PERFORM                                                        
067100       MOVE IX TO WS-KLASS                                                
067200     ELSE                                                                 
067300       MOVE '2502 SEGMENT ERROR' TO FELTEXT-STR                           
067400       DISPLAY FELTEXT                                                    
067500       PERFORM S99-ABEND                                                  
067600     END-IF                                                               
067700     .                                                                    
067800     EJECT                                                                
067900 S99-ABEND SECTION.                                                       
068000                                                                          
068100     MOVE 'S' TO POSTSUM-OPKOD                                            
068200     CALL POSTSUM USING POSTSUM-PARM                                      
068300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
068400     .                                                                    
068500     EJECT                                                                
068600* --- IMS SEKTIONER ---                                                   
068700     SKIP3                                                                
068800 IMS-GN-WDK629 SECTION.                                                   
068900     MOVE 'WDK629 '        TO SSA1                                        
069000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
069100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
069200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     SKIP3                                                                
069500     .                                                                    
069600     EJECT                                                                
069700 IMS-GNP-WDK611 SECTION.                                                  
069800     MOVE 'WDK611 '        TO SSA1                                        
069900     MOVE '  '             TO GODK-STATUSKODER                            
070000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
070100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     SKIP3                                                                
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-GNP-WDK601 SECTION.                                                  
070700     MOVE 'WDK601 '        TO SSA1                                        
070800     MOVE '  '             TO GODK-STATUSKODER                            
070900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
071000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     SKIP3                                                                
071300     .                                                                    
071400     EJECT                                                                
071500 IMS-GU-WDL711 SECTION.                                                   
071600                                                                          
071700     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
071800          DELIMITED BY SIZE INTO SSA1                                     
071900     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
072000          DELIMITED BY SIZE INTO SSA2                                     
072100     MOVE '  GE' TO GODK-STATUSKODER                                      
072200     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
072300     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
072400     PERFORM IMS-STATUSKONTROLL                                           
072500     .                                                                    
072600     EJECT                                                                
072700                                                                          
072800 IMS-GU-WDK711 SECTION.                                                   
072900                                                                          
073000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-US-X ')'                      
073100          DELIMITED BY SIZE INTO SSA1                                     
073200     STRING 'WDK711  (IDDC     =' W-IDDC-2-X ')'                          
073300          DELIMITED BY SIZE INTO SSA2                                     
073400     MOVE '  GE' TO GODK-STATUSKODER                                      
073500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
073600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 IMS-GU-WDK712 SECTION.                                                   
074200                                                                          
074300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-US-X ')'                      
074400          DELIMITED BY SIZE INTO SSA1                                     
074500     STRING 'WDK712  (IDLAND   =' W-IDLAND-US-X ')'                       
074600          DELIMITED BY SIZE INTO SSA2                                     
074700     MOVE '  GE' TO GODK-STATUSKODER                                      
074800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
074900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200     EJECT                                                                
075300                                                                          
075400 IMS-GU-WDK722 SECTION.                                                   
075500                                                                          
075600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-US-X ')'                      
075700          DELIMITED BY SIZE INTO SSA1                                     
075800     STRING 'WDK711  (IDDC     =' W-IDDC-2-X ')'                          
075900          DELIMITED BY SIZE INTO SSA2                                     
076000     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
076100          DELIMITED BY SIZE INTO SSA3                                     
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
076400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 IMS-GU-WDL811      SECTION.                                              
077000                                                                          
077100     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
077400          DELIMITED BY SIZE INTO SSA2                                     
077500     MOVE '  GE' TO GODK-STATUSKODER                                      
077600     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
077700     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     SKIP3                                                                
078100                                                                          
078200 IMS-GU-WDB601    SECTION.                                                
078300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
078400          DELIMITED BY SIZE INTO SSA1                                     
078500     MOVE '  ' TO GODK-STATUSKODER                                        
078600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
078700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     EJECT                                                                
079100 IMS-GU-2502 SECTION.                                                     
079200     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
079300          DELIMITED BY SIZE INTO SSA1                                     
079400     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
079500          DELIMITED BY SIZE INTO SSA2                                     
079600     MOVE '  GE' TO GODK-STATUSKODER                                      
079700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WL250111 SSA1 SSA2             
079800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
079900     PERFORM IMS-STATUSKONTROLL                                           
080000     .                                                                    
080100     SKIP2                                                                
080200 IMS-STATUSKONTROLL SECTION.                                              
080300                                                                          
080400     SET STATUS-IX TO 1                                                   
080500     SEARCH GODK-STATUS                                                   
080600       AT END                                                             
080700         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
080800           DELIMITED BY SIZE INTO FELTEXT-STR                             
080900         DISPLAY FELTEXT                                                  
081000         CALL FELLOG                                                      
081100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
081200         CONTINUE                                                         
081300     END-SEARCH                                                           
081400     .                                                                    
081500     EJECT                                                                
081600*    -COPY WY2000P1                                                       
