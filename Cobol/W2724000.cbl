000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2724000.                                                
000400*AUTHOR.         ARUP DATTA.                                              
000500*DATE-WRITTEN.   DEC 2016.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        CHECK ARTICLE FOR ACTIVATION                                     
001100*        WRITE ARTICLES MARKED FOR ACTIVATION TO UPD FILE                 
001200*                                                                         
001300*        PROGRAM READS         WDK6                                       
001400*                              WDL8                                       
001500*                              WDB6                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800*                                                                         
002900     SELECT W27240                     ASSIGN TO W27240D1.                
003000*          --- UT-FIL WITH PARTS FOR ACTIVATION                           
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500*                                                                         
003600 FD  W27240                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W27240  -PRE UT-    -L.                                   
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004500*    -COPY WY2000W3                                                       
004600     SKIP3                                                                
004700*    -COPY WWDC99                                                         
004800     SKIP3                                                                
004900 77  IDPGM                       PIC X(8)    VALUE 'W2724000'.            
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400     SKIP2                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900 01  WORK-AREA.                                                           
006000     03  FILLER                  PIC X(10)   VALUE 'WORK-AREA'.           
006100*                                                                         
006200     03  IX1                     PIC 9(2)    VALUE ZERO.                  
006300     03  MAX-IX1                 PIC 9(2)    VALUE 13.                    
006400     03  IX2                     PIC 9(2)    VALUE ZERO.                  
006500     03  MAX-IX2                 PIC 9(2)    VALUE 10.                    
006600     03  RP-INDX                 PIC 9(2)    VALUE ZERO.                  
006700     03  MAX-RP-INDX             PIC 9(2)    VALUE 12.                    
006800     03  INDX                    PIC 9(2)    VALUE ZERO.                  
006900     03  VV-INDX                 PIC 9(2)    VALUE ZERO.                  
007000     03  MAX-VV-INDX             PIC 9(2)    VALUE 53.                    
007100     03  TODAYS-DATE             PIC 9(6)    VALUE ZERO.                  
007200*                                                                         
007300 01  WS-TABLE-ARRAY.                                                      
007400     03 WS-TAB-WDB614 OCCURS 13.                                          
007500         10 WS-TAB-KVAKT         PIC S9(3)   COMP-3.                      
007600         10 WS-TAB-KVVECKOR-PUBV PIC S9(3)   COMP-3.                      
007700         10 WS-TAB-PRARTSTD      PIC 9(7).                                
007800         10 WS-TAB-VLARTNTO      PIC 9(8).                                
007900         10 WS-TAB-KVPB-SEP-REF  PIC S9(7)   COMP-3.                      
008000         10 WS-TAB-KDPRODSL OCCURS 11 TIMES PIC S9(3) COMP-3.             
008100                                                                          
008200 01  WS-VLARTNTO-JFR             PIC 9(8)V9(1) VALUE ZERO.                
008300 01  FILLER REDEFINES WS-VLARTNTO-JFR.                                    
008400     03  WS-VLARTNTO-HELTAL      PIC 9(8).                                
008500     03  WS-VLARTNTO-DECIMAL     PIC 9(1).                                
008600                                                                          
008700 01  WS-PRARTSTD-JFR             PIC 9(7)V9(2) VALUE ZERO.                
008800 01  FILLER REDEFINES WS-PRARTSTD-JFR.                                    
008900     03  WS-PRARTSTD-HELTAL      PIC 9(7).                                
009000     03  WS-PRARTSTD-DECIMAL     PIC 9(2).                                
009100                                                                          
009200 01  WS-KVVECKOR-PUBV            PIC 9(5)    VALUE ZERO.                  
009300                                                                          
009400 01  WS-TIFINLV-AAVVD            PIC 9(5)    VALUE ZERO.                  
009500                                                                          
009600 01  WS-INNEVARANDE-TIVV         PIC 9(2)    VALUE ZERO.                  
009700                                                                          
009800 01  WS-TISTOREF                 PIC S9(5)   VALUE ZERO COMP-3.           
009900                                                                          
010000 01  WS-TIREFSTA-6               PIC 9(6)    VALUE ZERO.                  
010100                                                                          
010200 01  WS-TIREFSTA-JFR             PIC 9(4)    VALUE ZERO.                  
010300 01  FILLER REDEFINES WS-TIREFSTA-JFR.                                    
010400     03 WS-TIREFSTA-JFR-AA       PIC 9(2).                                
010500     03 WS-TIREFSTA-JFR-VV       PIC 9(2).                                
010600                                                                          
010700 01  WS-TIKVOT-JFR               PIC 9(4)    VALUE ZERO.                  
010800 01  FILLER REDEFINES WS-TIKVOT-JFR.                                      
010900     03 WS-TIKVOT-TIAA           PIC 9(2).                                
011000     03 WS-TIKVOT-TIVV           PIC 9(2).                                
011100                                                                          
011200                                                                          
011300 77  PG-SW                       PIC X       VALUE 'N'.                   
011400     88  PG-FINNS                            VALUE 'J'.                   
011500                                                                          
011600 77  AKTIVERING-OK-SW            PIC X       VALUE 'N'.                   
011700     88  AKTIVERA-ART-OK                     VALUE 'J'.                   
011800                                                                          
011900 77  FLKTRL-AKT-SW               PIC X       VALUE ' '.                   
012000     88  FLKTRL-AKT-JA                       VALUE 'J'.                   
012100     88  FLKTRL-AKT-NEJ                      VALUE 'N'.                   
012200                                                                          
012300 01  WS-PER-AREA.                                                         
012400     03  WS-PERIODTABLE       OCCURS 12.                                  
012500         05   WS-TIAARP.                                                  
012600           07 WS-TIAARP-AA       PIC  9(2)   VALUE ZERO.                  
012700           07 WS-TIAARP-RP       PIC  9(2)   VALUE ZERO.                  
012800         05   WS-FORSTA-TIVV     PIC  9(2)   VALUE ZERO.                  
012900         05   WS-SISTA-TIVV      PIC  9(2)   VALUE ZERO.                  
013000                                                                          
013100     03  WS-PER-TAB-AR-1.                                                 
013200         05 WS-PERIODTABELL-AR-1 OCCURS 53.                               
013300            10 WS-KVOT-SUM-1     PIC S9(7)   VALUE ZERO.                  
013400                                                                          
013500     03  WS-PER-TAB-AR-0.                                                 
013600         05 WS-PERIODTABELL-AR-0 OCCURS 53.                               
013700            10 WS-KVOT-SUM-0     PIC S9(7)   VALUE ZERO.                  
013800                                                                          
013900     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
014000     03  FILLER REDEFINES DAGENS-TIAARP.                                  
014100         05 DAGENS-TIAA          PIC  9(2).                               
014200         05 DAGENS-TIRP          PIC  9(2).                               
014300                                                                          
014400     03  DAGENS-TIVV             PIC 9(2)    VALUE ZERO.                  
014500                                                                          
014600     03  DAGENS-VECKA            PIC 9(5)    VALUE ZERO.                  
014700     03  FILLER REDEFINES DAGENS-VECKA.                                   
014800         05 DAGENS-VECKA-AAVV    PIC 9(4).                                
014900         05 DAGENS-VECKA-D       PIC 9(1).                                
015000                                                                          
015100     03  WS-TIAARP-UTRAEKNING    PIC  9(4)   VALUE ZERO.                  
015200     03  FILLER REDEFINES WS-TIAARP-UTRAEKNING.                           
015300         05 WS-TIAA              PIC  9(2).                               
015400         05 WS-TIRP              PIC  9(2).                               
015500                                                                          
015600     03  WS-ATTA-VECKOR          PIC  9(5)   VALUE ZERO.                  
015700     03  WS-ATTA-VECKOR-AAVVD REDEFINES WS-ATTA-VECKOR.                   
015800         05 WS-ATTA-VECKOR-AAVV  PIC  9(4).                               
015900         05 WS-ATTA-VECKOR-D     PIC  9(1).                               
016000                                                                          
016100     03  SPAR-TIAARP             PIC  9(4)   VALUE ZERO.                  
016200                                                                          
016300     03  WS-YEAR.                                                         
016400         05 WS-YEAR-CC           PIC  9(2).                               
016500         05 WS-YEAR-AA           PIC  9(2).                               
016600                                                                          
016700     03  WS-KVOT-TOT             PIC S9(11)  VALUE ZERO COMP-3.           
016800     03  WS-KVOT-AKT             PIC S9(11)  VALUE ZERO COMP-3.           
016900     EJECT                                                                
017000*                                                                         
017100 01  DYNAMISKA-SUBPROGRAM.                                                
017200*                                                                         
017300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
017700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
017900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
018000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL ABEND                                            
018300                                                                          
018400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018600     SKIP2                                                                
018700*                                                                         
018800 01  UT-AREA-START              PIC X(24)   VALUE                         
018900                                 'UT-AREA-START  '.                       
019000     SKIP2                                                                
019100                                                                          
019200*01  AREA -COPY W27240     -PRE UT-                                       
019300     EJECT                                                                
019400*                                                                         
019500*    --- PARAMETRAR TILL POSTSUM                                          
019600*                                                                         
019700*01  -COPY W0005   -PRE  POSTSUM-                                         
019800     EJECT                                                                
019900*    --- PARAMETRAR TILL WDATKONV                                         
020000*                                                                         
020100*01  -COPY WDATAREA                                                       
020200*                                                                         
020300*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
020400*01  -COPY WZ20DAYS                                                       
020500*                                                                         
020600*    --- PARAMETRAR TILL VECKOADD                                         
020700                                                                          
020800 01  W009VADD-AREA.                                                       
020900     03 VADD-DATUM-AAVV          PIC S9(5)   VALUE ZERO COMP-3.           
021000     03 VADD-ANTAL               PIC S9(3)   VALUE ZERO COMP-3.           
021100                                                                          
021200     EJECT                                                                
021300*    --- PARAMETRAR TILL DATKORT                                          
021400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27240'.              
021500     SKIP2                                                                
021600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
021700     SKIP2                                                                
021800*01  -COPY WDATKORT                                                       
021900     EJECT                                                                
022000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
022100 01  FILLER REDEFINES DAGENS-DATUM.                                       
022200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
022300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
022400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
022500     EJECT                                                                
022600*---                                                                      
022700*--- IMS AREA                                                             
022800*---                                                                      
022900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023000     SKIP3                                                                
023100 01  NYCKLAR-TILL-DLI.                                                    
023200     03  W-IDARTNR-X.                                                     
023300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023400     03  W-KDSEGKEY-X.                                                    
023500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
023600     03  W-IDDC-X.                                                        
023700         05  W-IDDC              PIC X(2)    VALUE '11'.                  
023800     03  W-TIAAAA-X.                                                      
023900         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
024000*                                                                         
024100*    --- STATUS-KOD FROM IMS                                              
024200 01  STATUS-WS                   PIC XX.                                  
024300     88  SEGMENT-FINNS                       VALUE '  '.                  
024400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024700     88  IMS-EJ-OK                           VALUE 'XD'.                  
024800     SKIP2                                                                
024900 01  GODK-STATUSKODER.                                                    
025000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025100     SKIP3                                                                
025200 01  SSA1                        PIC X(128).                              
025300 01  SSA2                        PIC X(64).                               
025400     EJECT                                                                
025500*    --- IMS FUNKTIONSKODER                                               
025600*01  -COPY W0003                                                          
025700     EJECT                                                                
025800*    ---  DLI INPUT-OUTPUT AREA                                           
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
026000 01  DLI-IO-WDK601.                                                       
026100*    03  -COPY WDK601    -PRE WDK6-                                       
026200*                                                                         
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
026400 01  DLI-IO-WDK611.                                                       
026500*    03  -COPY WDK611                                                     
026600*                                                                         
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
026800 01  DLI-IO-WDK629.                                                       
026900*    03  -COPY WDK629                                                     
027000*                                                                         
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
027200 01  DLI-IO-WDL801.                                                       
027300*    03  -COPY WDL801                                                     
027400*                                                                         
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
027600 01  DLI-IO-WDL811.                                                       
027700*    03  -COPY WDL811                                                     
027800*                                                                         
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
028000 01  DLI-IO-WDB601.                                                       
028100*    03  -COPY WDB601                                                     
028200*                                                                         
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB614'.                      
028400 01  DLI-IO-WDB614.                                                       
028500*    03  -COPY WDB614   -PRE WDB614-                                      
028600*                                                                         
028700 LINKAGE SECTION.                                                         
028800                                                                          
028900*01  -COPY W0008  -PRE WDK6-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008  -PRE WDL8-                                              
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE WDB6-                                              
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800 PROCEDURE DIVISION  USING WDK6-PCB                                       
029900                           WDL8-PCB WDB6-PCB.                             
030000     ENTRY 'DLITCBL' USING WDK6-PCB                                       
030100                           WDL8-PCB WDB6-PCB.                             
030200                                                                          
030300     PERFORM A-INIT                                                       
030400                                                                          
030500     PERFORM IMS-GN-WDK629                                                
030600                                                                          
030700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
030800       IF  CREF-KDREFSTA = 'P'                                            
030900       AND CREF-FLREFILL = JA                                             
031000         PERFORM IMS-GNP-WDK611                                           
031100         PERFORM IMS-GNP-WDK601                                           
031200         MOVE WDK6-ART-IDARTNR        TO W-IDARTNR                        
031300                                                                          
031400         IF SEGMENT-FINNS                                                 
031500           IF CLAG-KDERS < 07                                             
031600*---                                                                      
031700*---       FOR ACTIVATING PARTS WITH KDERS LESS THAN 7                    
031800*---                                                                      
031900             PERFORM B-GET-ORDERHIT-WDL811                                
032000*                                                                         
032100             PERFORM C-PROCESS-ACTIVATION                                 
032200*                                                                         
032300             IF AKTIVERA-ART-OK                                           
032400                PERFORM E-SKRIV-UPDFIL                                    
032500             END-IF                                                       
032600           END-IF                                                         
032700         END-IF                                                           
032800                                                                          
032900       END-IF                                                             
033000*                                                                         
033100       PERFORM IMS-GN-WDK629                                              
033200     END-PERFORM                                                          
033300                                                                          
033400     PERFORM Z-FINIT                                                      
033500                                                                          
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     OPEN OUTPUT W27240                                                   
034200                                                                          
034300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034400                                                                          
034500     ACCEPT TODAYS-DATE FROM DATE                                         
034600                                                                          
034700     CALL DATKORT   USING PROGRAM-NAMN  DATUMKORT-ID DATUMKORT            
034800     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
034900     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
035000     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
035100     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
035200                                                                          
035300     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
035400     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
035500                                                                          
035600     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
035700                          DAT-O-TIDATUM DAT-KDSVAR                        
035800                                                                          
035900     IF DAT-KDSVAR-OK                                                     
036000       MOVE DAT-TISEKEL TO WS-YEAR-CC                                     
036100       MOVE DAT-TIAARP  TO DAGENS-TIAARP                                  
036200       MOVE DAT-TIVV    TO DAGENS-TIVV                                    
036300       MOVE DAT-TIAAVVD TO DAGENS-VECKA                                   
036400     ELSE                                                                 
036500       MOVE 'RC FROM WDATKONV 1 CALL NOT OK' TO FELTEXT-STR               
036600       DISPLAY FELTEXT                                                    
036700       PERFORM S99-ABEND                                                  
036800     END-IF                                                               
036900                                                                          
037000     INITIALIZE WS-TABLE-ARRAY                                            
037100     PERFORM AA-LOAD-WDB6-TABLE                                           
037200                                                                          
037300     PERFORM AB-INIT-PERIODTABLE                                          
037400     .                                                                    
037500     EJECT                                                                
037600 AA-LOAD-WDB6-TABLE SECTION.                                              
037700                                                                          
037800     PERFORM IMS-GU-WDB601-KY                                             
037900*                                                                         
038000     PERFORM IMS-GNP-WDB614                                               
038100     MOVE +1   TO IX1                                                     
038200     IF SEGMENT-FINNS                                                     
038300       PERFORM UNTIL SEGMENT-SAKNAS OR IX1 > MAX-IX1                      
038400         MOVE WDB614-AKT-KVAKT   TO WS-TAB-KVAKT(IX1)                     
038500         MOVE WDB614-AKT-KVVECKOR-PUBV                                    
038600                                 TO WS-TAB-KVVECKOR-PUBV(IX1)             
038700         MOVE WDB614-AKT-PRARTSTD                                         
038800                                 TO WS-TAB-PRARTSTD(IX1)                  
038900         MOVE WDB614-AKT-VLARTNTO                                         
039000                                 TO WS-TAB-VLARTNTO(IX1)                  
039100         MOVE WDB614-AKT-KVPB-SEP-REF                                     
039200                                 TO WS-TAB-KVPB-SEP-REF(IX1)              
039300*                                                                         
039400         MOVE +1                 TO IX2                                   
039500         PERFORM UNTIL      IX2   > MAX-IX2                               
039600           MOVE WDB614-AKT-KDPRODSL(IX2)                                  
039700                                 TO WS-TAB-KDPRODSL(IX1 IX2)              
039800           ADD +1                TO IX2                                   
039900         END-PERFORM                                                      
040000*                                                                         
040100         PERFORM IMS-GNP-WDB614                                           
040200         ADD +1   TO IX1                                                  
040300       END-PERFORM                                                        
040400     END-IF                                                               
040500*                                                                         
040600     .                                                                    
040700     EJECT                                                                
040800 AB-INIT-PERIODTABLE  SECTION.                                            
040900                                                                          
041000*--------------------------------------------------------------*          
041100* HERE INITIALIZING PERIOD TABLE. INDX 1 CORRESPONDS TO CURRENT*          
041200* PERIOD. TABLE INTIALIZED WITH PERIOD + START AND END WEEK    *          
041300* TO PERFORM SUMMATION                                         *          
041400*--------------------------------------------------------------*          
041500                                                                          
041600     MOVE +1                      TO RP-INDX                              
041700                                                                          
041800     PERFORM UNTIL RP-INDX         >  MAX-RP-INDX                         
041900       IF RP-INDX                  =  1                                   
042000*------- COMPUTE CURRENT PERIOD - 1                                       
042100         MOVE DAGENS-TIAARP       TO WS-TIAARP-UTRAEKNING                 
042200         SUBTRACT 1             FROM WS-TIRP                              
042300         IF WS-TIRP                = ZERO                                 
042400           MOVE 12                TO WS-TIRP                              
042500           IF WS-TIAA              = ZERO                                 
042600             MOVE 99              TO WS-TIAA                              
042700           ELSE                                                           
042800             SUBTRACT 1         FROM WS-TIAA                              
042900           END-IF                                                         
043000         END-IF                                                           
043100       ELSE                                                               
043200*------- COMPUTE PREVIOUS PERIOD                                          
043300         SUBTRACT 1              FROM WS-TIRP                             
043400         IF WS-TIRP                 = ZERO                                
043500           MOVE 12                 TO WS-TIRP                             
043600           IF WS-TIAA               = ZERO                                
043700             MOVE 99               TO WS-TIAA                             
043800           ELSE                                                           
043900             SUBTRACT  1         FROM WS-TIAA                             
044000           END-IF                                                         
044100         END-IF                                                           
044200       END-IF                                                             
044300                                                                          
044400*----- COMPUTE THE START WEEK OF THE PERIOD                               
044500       MOVE 'AARP  '               TO DAT-KDDATFORM                       
044600       MOVE WS-TIAARP-UTRAEKNING   TO DAT-I-TIDATUM                       
044700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
044800                           DAT-O-TIDATUM DAT-KDSVAR                       
044900       IF DAT-KDSVAR-OK                                                   
045000         MOVE WS-TIAARP-UTRAEKNING TO WS-TIAARP      (RP-INDX)            
045100         MOVE DAT-TIVV             TO WS-FORSTA-TIVV (RP-INDX)            
045200       ELSE                                                               
045300         MOVE 'RC FROM WDATKONV 2 CALL NOT OK'                            
045400                                   TO FELTEXT-STR                         
045500         DISPLAY FELTEXT                                                  
045600         PERFORM S99-ABEND                                                
045700       END-IF                                                             
045800                                                                          
045900       IF RP-INDX                   = 1                                   
046000*------- COMPUTE LAST WEEK OF CURRENT PERIOD                              
046100*------- BY USING FIRST WEEK OF NEXT PERIOD                               
046200         MOVE WS-TIAARP-UTRAEKNING TO SPAR-TIAARP                         
046300         ADD 1                     TO WS-TIRP                             
046400         IF WS-TIRP                 > 12                                  
046500           ADD  1                  TO WS-TIAA                             
046600           MOVE 1                  TO WS-TIRP                             
046700         END-IF                                                           
046800         MOVE WS-TIAARP-UTRAEKNING TO DAT-I-TIDATUM                       
046900         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
047000                             DAT-O-TIDATUM DAT-KDSVAR                     
047100         IF DAT-KDSVAR-OK                                                 
047200           MOVE DAT-TIVV           TO WS-SISTA-TIVV (1)                   
047300           SUBTRACT 1            FROM WS-SISTA-TIVV (1)                   
047400           IF WS-SISTA-TIVV (1)     = ZERO                                
047500             MOVE 53               TO WS-SISTA-TIVV (1)                   
047600           END-IF                                                         
047700         ELSE                                                             
047800           MOVE 'RC FROM WDATKONV 3 CALL NOT OK'                          
047900                                   TO FELTEXT-STR                         
048000           DISPLAY FELTEXT                                                
048100           PERFORM S99-ABEND                                              
048200         END-IF                                                           
048300                                                                          
048400         MOVE SPAR-TIAARP           TO WS-TIAARP-UTRAEKNING               
048500                                                                          
048600       END-IF                                                             
048700                                                                          
048800       IF RP-INDX         < 12                                            
048900*------- FIRST WEEK OF A PERIOD IS USED                                   
049000*------- TO CALCULATE THE LAST WEEK OF PREVIOUS PERIOD                    
049100         MOVE WS-FORSTA-TIVV (RP-INDX) TO                                 
049200                                 WS-SISTA-TIVV (RP-INDX + 1)              
049300         SUBTRACT 1              FROM WS-SISTA-TIVV (RP-INDX + 1)         
049400         IF WS-SISTA-TIVV (RP-INDX + 1) = ZERO                            
049500           MOVE 53               TO WS-SISTA-TIVV (RP-INDX + 1)           
049600         END-IF                                                           
049700       END-IF                                                             
049800                                                                          
049900*----- ENSURE THAT THE FIRST WEEK IS ALWAYS 01                            
050000*----- FOR THE INITIAL PERIOD EVERY YEAR                                  
050100       IF WS-TIAARP-RP (RP-INDX) = 01                                     
050200         MOVE 1                     TO WS-FORSTA-TIVV (RP-INDX)           
050300       END-IF                                                             
050400                                                                          
050500       ADD +1                       TO RP-INDX                            
050600     END-PERFORM                                                          
050700                                                                          
050800*--- FOR WEEKS IN THE FIRST PERIOD AND THE LAST PERIOD                    
050900*--- IF THERE IS ANY OVERLAP, ADJUST TO ELIMINATE COMMON                  
051000*--- WEEKS.                                                               
051100                                                                          
051200*--- EX. FIRST  PERIOD   = 1610 WITH WEEKS      40 - 44                   
051300*---     LAST   PERIOD   = 1511 WITH WEEKS      44 - 48                   
051400*---     REDUCE THE WEEKS OF THE LAST PERIOD TO 45 - 48                   
051500                                                                          
051600*--- ADJUSTMENTS MADE BY CHECK FOR MAX 2 OVERLAPPED WEEKS                 
051700                                                                          
051800     IF WS-SISTA-TIVV(1)          = WS-FORSTA-TIVV(12)  OR                
051900        WS-SISTA-TIVV(1)          = WS-FORSTA-TIVV(12) + 1                
052000       COMPUTE WS-FORSTA-TIVV(12) = WS-SISTA-TIVV(1) + 1                  
052100     END-IF                                                               
052200                                                                          
052300     .                                                                    
052400     EJECT                                                                
052500 B-GET-ORDERHIT-WDL811  SECTION.                                          
052600                                                                          
052700     MOVE  1                     TO INDX                                  
052800                                    VV-INDX                               
052900     INITIALIZE                     WS-PER-TAB-AR-0                       
053000                                    WS-PER-TAB-AR-1                       
053100*                                                                         
053200*    MOVE WS-TIAARP(INDX) (1:2)  TO WS-YEAR-AA                            
053300     MOVE WS-TIAARP-AA(INDX)     TO WS-YEAR-AA                            
053400     MOVE WS-YEAR                TO W-TIAAAA                              
053500     PERFORM IMS-GU-WDL811                                                
053600     IF SEGMENT-FINNS                                                     
053700        PERFORM UNTIL VV-INDX     > MAX-VV-INDX                           
053800          ADD AAR-KVOT-PROG   (VV-INDX)                                   
053900                                 TO WS-KVOT-SUM-0(VV-INDX)                
054000          ADD AAR-KVOT-REFILL (VV-INDX)                                   
054100                                 TO WS-KVOT-SUM-0(VV-INDX)                
054200          ADD 1                  TO VV-INDX                               
054300        END-PERFORM                                                       
054400        ADD 1                    TO INDX                                  
054500     ELSE                                                                 
054600        ADD 1                    TO INDX                                  
054700     END-IF                                                               
054800*                                                                         
054900     PERFORM UNTIL  INDX          > MAX-RP-INDX                           
055000*      IF (INDX                   > 1                                     
055100*      AND WS-TIAARP(INDX)    NOT = WS-TIAARP(INDX   1))                  
055200*      IF  WS-TIAARP(INDX)    NOT = WS-TIAARP(INDX - 1)                   
055300       IF  WS-TIAARP-AA(INDX) NOT = WS-TIAARP-AA(INDX - 1)                
055400*                                                                         
055500          MOVE  1                TO VV-INDX                               
055600*         MOVE WS-TIAARP(INDX) (1:2)                                      
055700          MOVE WS-TIAARP-AA(INDX)                                         
055800                                 TO WS-YEAR-AA                            
055900          MOVE WS-YEAR           TO W-TIAAAA                              
056000          PERFORM IMS-GU-WDL811                                           
056100          IF SEGMENT-FINNS                                                
056200             PERFORM UNTIL VV-INDX > MAX-VV-INDX                          
056300               ADD AAR-KVOT-PROG   (VV-INDX)                              
056400                                 TO WS-KVOT-SUM-1(VV-INDX)                
056500               ADD AAR-KVOT-REFILL (VV-INDX)                              
056600                                 TO WS-KVOT-SUM-1(VV-INDX)                
056700               ADD 1             TO VV-INDX                               
056800             END-PERFORM                                                  
056900          END-IF                                                          
057000          MOVE MAX-RP-INDX       TO INDX                                  
057100       END-IF                                                             
057200       ADD 1                     TO INDX                                  
057300     END-PERFORM                                                          
057400     .                                                                    
057500     EJECT                                                                
057600 C-PROCESS-ACTIVATION  SECTION.                                           
057700                                                                          
057800     MOVE NEJ                    TO AKTIVERING-OK-SW                      
057900     MOVE JA                     TO FLKTRL-AKT-SW                         
058000                                                                          
058100     IF CLAG-KDERS > ZERO                                                 
058200        PERFORM CA-CTRL-ACTIVATION                                        
058300     END-IF                                                               
058400*                                                                         
058500     IF FLKTRL-AKT-JA                                                     
058600        PERFORM CB-CHECK-ACTIVATION-RULES                                 
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 CA-CTRL-ACTIVATION    SECTION.                                           
059100                                                                          
059200     IF CLAG-KDERS = 04                                                   
059300        MOVE NEJ                 TO FLKTRL-AKT-SW                         
059400     ELSE                                                                 
059500       IF CLAG-KDERS = 01 OR 02 OR 03 OR 05 OR 06                         
059600          MOVE CLAG-TISTOREF     TO WS-TISTOREF                           
059700*---                                                                      
059800*--- CHECK IF STOP DATE WITHIN 8 WEEKS                                    
059900*---                                                                      
060000          MOVE DAGENS-VECKA-D    TO WS-ATTA-VECKOR-D                      
060100                                                                          
060200          MOVE DAGENS-VECKA-AAVV TO VADD-DATUM-AAVV                       
060300          MOVE +8                TO VADD-ANTAL                            
060400                                                                          
060500          CALL W009VADD       USING VADD-DATUM-AAVV                       
060600                                    VADD-ANTAL                            
060700          MOVE VADD-DATUM-AAVV   TO WS-ATTA-VECKOR-AAVV                   
060800*---                                                                      
060900*--- IF STOP DATE WITH 8 WEEKS, PART CANNOT BE ACTIVATED                  
061000*---                                                                      
061100          IF WS-TISTOREF          > WS-ATTA-VECKOR                        
061200             CONTINUE                                                     
061300          ELSE                                                            
061400             MOVE NEJ            TO FLKTRL-AKT-SW                         
061500          END-IF                                                          
061600       END-IF                                                             
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 CB-CHECK-ACTIVATION-RULES  SECTION.                                      
062100                                                                          
062200     PERFORM CBA-CHECK-ORDERHIT                                           
062300                                                                          
062400     PERFORM CBB-GET-PUBV-INFO                                            
062500                                                                          
062600     MOVE NEJ                    TO AKTIVERING-OK-SW                      
062700     MOVE +1                     TO IX1                                   
062800                                                                          
062900     IF WS-TAB-KVAKT (IX1)        > ZERO                                  
063000        PERFORM UNTIL IX1         > MAX-IX1                               
063100                OR (AKTIVERING-OK-SW  = JA)                               
063200                OR (WS-TAB-KVAKT(IX1) = ZERO)                             
063300                                                                          
063400          IF  WS-KVOT-TOT        >= WS-TAB-KVAKT(IX1)                     
063500          AND WS-KVOT-TOT        >  ZERO                                  
063600             MOVE JA             TO AKTIVERING-OK-SW                      
063700                                                                          
063800             IF WS-TAB-KVVECKOR-PUBV(IX1) > ZERO                          
063900                PERFORM CBC-CHECK-PUBV                                    
064000             END-IF                                                       
064100                                                                          
064200             IF WS-TAB-PRARTSTD(IX1)      > ZERO                          
064300                PERFORM CBD-CHECK-STD-PRIS                                
064400             END-IF                                                       
064500                                                                          
064600             IF WS-TAB-VLARTNTO(IX1)      > ZERO                          
064700                PERFORM CBE-CHECK-VOLYM                                   
064800             END-IF                                                       
064900                                                                          
065000             IF WS-TAB-KDPRODSL(IX1 1)    > ZERO                          
065100                PERFORM CBF-CHECK-PG                                      
065200             END-IF                                                       
065300                                                                          
065400          END-IF                                                          
065500          ADD +1                 TO IX1                                   
065600        END-PERFORM                                                       
065700     ELSE                                                                 
065800        MOVE NEJ                 TO AKTIVERING-OK-SW                      
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 CBA-CHECK-ORDERHIT  SECTION.                                             
066300                                                                          
066400*--- CHECK FROM LAST DATE OF ACTICLE PASSIVATION.                         
066500*--- ORDERHITS ONLY AFTER THE THIS DATE IS CONSIDERED.                    
066600                                                                          
066700     IF CREF-TIREFSTA             > ZERO                                  
066800        MOVE CREF-TIREFSTA       TO WS-TIREFSTA-6                         
066900        MOVE WS-TIREFSTA-6       TO DAT-I-TIDATUM                         
067000        MOVE 'AAMMDD'            TO DAT-KDDATFORM                         
067100        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
067200                            DAT-O-TIDATUM DAT-KDSVAR                      
067300       IF DAT-KDSVAR-OK                                                   
067400          MOVE DAT-TIAA-VECKA    TO WS-TIREFSTA-JFR-AA                    
067500          MOVE DAT-TIVV          TO WS-TIREFSTA-JFR-VV                    
067600       ELSE                                                               
067700         MOVE ZERO               TO WS-TIREFSTA-JFR                       
067800       END-IF                                                             
067900     ELSE                                                                 
068000       MOVE ZERO                 TO WS-TIREFSTA-JFR                       
068100     END-IF                                                               
068200                                                                          
068300*--- SUM ALL ORDER HITS FOR LAST 12 PERIODS                               
068400     MOVE +1                     TO INDX                                  
068500     MOVE ZERO                   TO WS-KVOT-TOT                           
068600     PERFORM UNTIL INDX           > 12                                    
068700       MOVE WS-FORSTA-TIVV(INDX) TO VV-INDX                               
068800       PERFORM UNTIL VV-INDX      > WS-SISTA-TIVV(INDX)                   
068900                                                                          
069000*        MOVE WS-TIAARP(INDX) (1:2)                                       
069100         MOVE WS-TIAARP-AA(INDX)                                          
069200                                 TO WS-TIKVOT-TIAA                        
069300         MOVE VV-INDX            TO WS-TIKVOT-TIVV                        
069400         MOVE WS-TIKVOT-JFR      TO TMP1-YYWW                             
069500         MOVE WS-TIREFSTA-JFR    TO TMP2-YYWW                             
069600         PERFORM WY2000P3                                                 
069700         IF TMP1-YYWW >= TMP2-YYWW                                        
069800*           IF WS-TIAARP(1) (1:2) = WS-TIKVOT-TIAA                        
069900*           IF WS-TIAARP-AA(INDX) = WS-TIKVOT-TIAA                        
070000            IF WS-TIAARP-AA(INDX) = WS-TIAARP-AA(1)                       
070100               ADD WS-KVOT-SUM-0 (VV-INDX)                                
070200                                 TO WS-KVOT-TOT                           
070300            ELSE                                                          
070400               ADD WS-KVOT-SUM-1 (VV-INDX)                                
070500                                 TO WS-KVOT-TOT                           
070600            END-IF                                                        
070700         END-IF                                                           
070800         ADD +1                  TO VV-INDX                               
070900       END-PERFORM                                                        
071000       ADD +1                    TO INDX                                  
071100     END-PERFORM                                                          
071200                                                                          
071300                                                                          
071400*--- GET FIRST WEEK FOR THE CURRENT PERIOD                                
071500     MOVE 'AARP  '               TO DAT-KDDATFORM                         
071600     MOVE DAGENS-TIAARP          TO DAT-I-TIDATUM                         
071700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
071800                         DAT-O-TIDATUM DAT-KDSVAR                         
071900     IF DAT-KDSVAR-OK                                                     
072000       MOVE DAT-TIVV             TO WS-INNEVARANDE-TIVV                   
072100     ELSE                                                                 
072200       MOVE 'RC FROM WDATKONV 5 CALL NOT OK' TO FELTEXT-STR               
072300       DISPLAY FELTEXT                                                    
072400       PERFORM S99-ABEND                                                  
072500     END-IF                                                               
072600                                                                          
072700                                                                          
072800*--- ADD ORDERHITS FOR THE CURRECT PERIOD - TILL CURRENT WEEK             
072900*---                                                                      
073000     MOVE WS-INNEVARANDE-TIVV    TO VV-INDX                               
073100                                                                          
073200     PERFORM UNTIL VV-INDX        > DAGENS-TIVV                           
073300       MOVE DAGENS-TIAA          TO WS-TIKVOT-TIAA                        
073400       MOVE WS-INNEVARANDE-TIVV  TO WS-TIKVOT-TIVV                        
073500       MOVE WS-TIKVOT-JFR        TO TMP1-YYWW                             
073600       MOVE WS-TIREFSTA-JFR      TO TMP2-YYWW                             
073700       PERFORM WY2000P3                                                   
073800       IF TMP1-YYWW >= TMP2-YYWW                                          
073900          ADD WS-KVOT-SUM-0 (VV-INDX)                                     
074000                                 TO WS-KVOT-TOT                           
074100       END-IF                                                             
074200       ADD +1                    TO VV-INDX                               
074300                                    WS-INNEVARANDE-TIVV                   
074400     END-PERFORM                                                          
074500     .                                                                    
074600     EJECT                                                                
074700 CBB-GET-PUBV-INFO   SECTION.                                             
074800                                                                          
074900     MOVE WDK6-ART-TIFINLV       TO WS-TIFINLV-AAVVD                      
075000     MOVE WS-TIFINLV-AAVVD       TO DAYS-TIDATE1                          
075100     MOVE 'YYWWD'                TO DAYS-KDDATFMT1                        
075200*                                                                         
075300     MOVE ZERO                   TO DAYS-KVDAYS                           
075400     MOVE 'YYMMDD'               TO DAYS-KDDATFMT2                        
075500     MOVE DAGENS-DATUM           TO DAYS-TIDATE2                          
075600                                                                          
075700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
075800*                                                                         
075900     IF DAYS-KDRC = +0                                                    
076000       COMPUTE WS-KVVECKOR-PUBV   = DAYS-KVDAYS / 7                       
076100     ELSE                                                                 
076200       MOVE 'ERROR FROM WZ20DAYS MODULE' TO FELTEXT-STR                   
076300       DISPLAY FELTEXT                                                    
076400       DISPLAY DAYS-KDRC                                                  
076500       PERFORM S99-ABEND                                                  
076600     END-IF                                                               
076700     .                                                                    
076800     EJECT                                                                
076900 CBC-CHECK-PUBV      SECTION.                                             
077000                                                                          
077100     IF WS-KVVECKOR-PUBV          < WS-TAB-KVVECKOR-PUBV(IX1)             
077200       CONTINUE                                                           
077300     ELSE                                                                 
077400       MOVE NEJ                  TO AKTIVERING-OK-SW                      
077500     END-IF                                                               
077600     .                                                                    
077700     EJECT                                                                
077800 CBD-CHECK-STD-PRIS  SECTION.                                             
077900                                                                          
078000     MOVE CLAG-PRARTSTD          TO WS-PRARTSTD-JFR                       
078100                                                                          
078200     IF WS-PRARTSTD-HELTAL        < WS-TAB-PRARTSTD(IX1)                  
078300       CONTINUE                                                           
078400     ELSE                                                                 
078500       MOVE NEJ                  TO AKTIVERING-OK-SW                      
078600     END-IF                                                               
078700     .                                                                    
078800     EJECT                                                                
078900 CBE-CHECK-VOLYM     SECTION.                                             
079000                                                                          
079100     MOVE CLAG-VLARTNTO          TO WS-VLARTNTO-JFR                       
079200                                                                          
079300     IF WS-VLARTNTO-HELTAL        < WS-TAB-VLARTNTO(IX1)                  
079400        CONTINUE                                                          
079500     ELSE                                                                 
079600        MOVE NEJ                 TO AKTIVERING-OK-SW                      
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 CBF-CHECK-PG        SECTION.                                             
080100                                                                          
080200     MOVE NEJ                    TO PG-SW                                 
080300                                                                          
080400     MOVE +1                     TO IX2                                   
080500     PERFORM UNTIL IX2 > MAX-IX2 OR PG-FINNS                              
080600       IF WDK6-ART-KDPRODSL       = WS-TAB-KDPRODSL(IX1 IX2 )             
080700          MOVE JA                TO PG-SW                                 
080800       END-IF                                                             
080900       ADD +1                    TO IX2                                   
081000     END-PERFORM                                                          
081100                                                                          
081200     IF PG-SW = NEJ                                                       
081300       MOVE NEJ                  TO AKTIVERING-OK-SW                      
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800 E-SKRIV-UPDFIL  SECTION.                                                 
081900*    UPD FILE TO ACTIVATE ARTICLES ON WDK6                                
082000                                                                          
082100     MOVE WDK6-ART-IDARTNR       TO UT-IDARTNR                            
082200     MOVE CREF-IDDC-REF          TO UT-IDDC-REF                           
082300     MOVE TODAYS-DATE            TO UT-TIREFSTA                           
082400                                                                          
082500     PERFORM S01-SKRIV-W27240                                             
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 Z-FINIT SECTION.                                                         
083000                                                                          
083100     CLOSE W27240                                                         
083200                                                                          
083300     MOVE 'S' TO POSTSUM-OPKOD                                            
083400     CALL POSTSUM USING POSTSUM-PARM                                      
083500     .                                                                    
083600     EJECT                                                                
083700                                                                          
083800 S01-SKRIV-W27240 SECTION.                                                
083900                                                                          
084000     WRITE UT-POST FROM UT-AREA                                           
084100                                                                          
084200     MOVE 'W27240'   TO POSTSUM-FDNAMN                                    
084300     MOVE 'W27240D1' TO POSTSUM-DDNAMN2                                   
084400     CALL POSTSUM USING POSTSUM-PARM                                      
084500     .                                                                    
084600     SKIP3                                                                
084700                                                                          
084800 S99-ABEND SECTION.                                                       
084900     MOVE 'S' TO POSTSUM-OPKOD                                            
085000     CALL POSTSUM USING POSTSUM-PARM                                      
085100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
085200     .                                                                    
085300* --- IMS SEKTIONER ---                                                   
085400 IMS-GN-WDK629 SECTION.                                                   
085500     MOVE 'WDK629 '        TO SSA1                                        
085600     MOVE '  GEGB'           TO GODK-STATUSKODER                          
085700     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
085800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
085900     PERFORM IMS-STATUSKONTROLL                                           
086000     SKIP3                                                                
086100     .                                                                    
086200     EJECT                                                                
086300 IMS-GNP-WDK611 SECTION.                                                  
086400     MOVE 'WDK611 '        TO SSA1                                        
086500     MOVE '  '             TO GODK-STATUSKODER                            
086600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
086700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
086800     PERFORM IMS-STATUSKONTROLL                                           
086900     SKIP3                                                                
087000     .                                                                    
087100     EJECT                                                                
087200 IMS-GNP-WDK601 SECTION.                                                  
087300     MOVE 'WDK601 '        TO SSA1                                        
087400     MOVE '  '             TO GODK-STATUSKODER                            
087500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
087600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     SKIP3                                                                
087900     .                                                                    
088000     EJECT                                                                
088100******************************************************************        
088200 IMS-GU-WDL811      SECTION.                                              
088300     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
088400          DELIMITED BY SIZE INTO SSA1                                     
088500     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
088600          DELIMITED BY SIZE INTO SSA2                                     
088700     MOVE '  GE' TO GODK-STATUSKODER                                      
088800     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
088900     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     .                                                                    
089200     SKIP3                                                                
089300 IMS-GU-WDB601-KY SECTION.                                                
089400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
089500          DELIMITED BY SIZE INTO SSA1                                     
089600     MOVE '  '                TO GODK-STATUSKODER                         
089700     CALL CBLTDLI          USING GU WDB6-PCB DLI-IO-WDB601 SSA1           
089800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
089900     PERFORM IMS-STATUSKONTROLL                                           
090000     .                                                                    
090100 IMS-GNP-WDB614 SECTION.                                                  
090200     MOVE 'WDB614  '          TO SSA1                                     
090300     MOVE '  GE'              TO GODK-STATUSKODER                         
090400     CALL CBLTDLI          USING GNP WDB6-PCB DLI-IO-WDB614 SSA1          
090500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-STATUSKONTROLL SECTION.                                              
091000     SET STATUS-IX TO 1                                                   
091100     SEARCH GODK-STATUS                                                   
091200       AT END                                                             
091300         MOVE 'FELAKTIG IMS-RETURKOD' TO FELTEXT-STR                      
091400         DISPLAY FELTEXT                                                  
091500         CALL FELLOG                                                      
091600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
091700         CONTINUE                                                         
091800     END-SEARCH                                                           
091900     .                                                                    
092000*    -COPY WY2000P3                                                       
