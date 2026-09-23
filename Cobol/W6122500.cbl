000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122500.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/01/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER FIL W61221 LEV.ANM CDC-NDC. SAMMANSLAGNING AV              
001000*        AVVIKELSER OCH SKAPAR FIL W61222 TILL LEV.ANM. CDC.              
001100*                                                                         
001200*        TILLAGT 2011-11-15 FÖR KINA:                                     
001300*        FILEN INNEHÅLLER ÄVEN LEV.ANM. CDC-NDC/CDC-LDC KINA              
001400*        MEN DESSA SKALL INTE HA NÅGON SAMMANSLAGNING AV                  
001500*        AVVIKELSER.                                                      
001600*                                                                         
001610*        EXPORTFLÖDEN REFILL-EXP-NDC SKALL HA BÅDE LEV.ANM OCH            
001620*        RAPPORT W41841-001.                                              
001700*                                                                         
001800*    2011-11-15 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1              
001900*                                                                         
001910*    2016-04-26 E-TRACKER 10243132 CHINA EXPORT PROJECT REFILL            
001911*                                  FRÅN DC71 TILL CDC                     
001920*                                                                         
001930*    2018 - JIRA PULS-977 GLOBAL EXPORT                                   
001940*                         REFILL FRÅN US-TO-CN, CN-TO-US,US-TO-CDC        
001950*                                                                         
001960*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- LEV.ANM CDC                                                
002800     SELECT W61221                     ASSIGN TO W61225D1.                
002900     SKIP2                                                                
003000*          --- SAMMANSLAGNING LEV.ANM CDC                                 
003100     SELECT W61222                     ASSIGN TO W61225D2.                
003110     SKIP2                                                                
003120*          --- DISCREPANCY DC71 TO DC11, DC4X-CDC, CN-US, US-CN           
003130     SELECT W61228                     ASSIGN TO W61225D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W61221                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W418REFC     -L.                                               
004200     SKIP3                                                                
004300 FD  W61222                                                               
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W418REF  -PRE  UT-  -L.                                   
004710     SKIP3                                                                
004810 FD  W61228                                                               
004820     RECORDING       F                                                    
004830     BLOCK CONTAINS  0.                                                   
004840                                                                          
004850*01  POST -COPY W414100A -PRE  UT2- -L.                                   
004860     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W6122500'.            
005110 77  FILLER                      PIC X(8)    VALUE 'WORKSTO:'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005310 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005311 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005330                                                                          
005400 77  SW-SUM                      PIC X       VALUE 'N'.                   
005500 77  WS-SUMMA-00                 PIC S9(7)   VALUE ZERO COMP-3.           
005600 77  WS-SUMMA-11                 PIC S9(7)   VALUE ZERO COMP-3.           
005700 77  WS-SUMMA-21                 PIC S9(7)   VALUE ZERO COMP-3.           
005800 77  WS-SUMMA-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
005900 77  WS-SUMMA-RED                PIC 9(7)    VALUE ZERO COMP-3.           
006000 77  WS-SPAR-IDFAKT              PIC 9(7)    VALUE ZERO.                  
006100 77  WS-SPAR-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.           
006200 77  WS-IDRADNR                  PIC S9(5)   VALUE ZERO COMP-3.           
006300                                                                          
006400 77  W61221-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W61221                       VALUE 'J'.                   
006600                                                                          
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007200     EJECT                                                                
007300                                                                          
007400 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
007500*01  FILLER  -COPY WWDIST35 -RED TEST-IDDISTR.                            
007600                                                                          
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     SKIP2                                                                
008500*    --- PARAMETRAR TILL ABEND                                            
008600                                                                          
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900 01  IN-AREA-START               PIC X(24)   VALUE                        
010000                                 'IN-AREA-START  '.                       
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W418REFC    -PRE IN-                                      
010400     EJECT                                                                
010500 01  UT-AREA-START               PIC X(24)   VALUE                        
010600                                 'UT-AREA-START  '.                       
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W418REF     -PRE UT-                                      
011000     SKIP2                                                                
011100                                                                          
011200 01  UT2-AREA-START              PIC X(24)   VALUE                        
011300                                 'UT2-AREA-START  '.                      
011400                                                                          
011500*01  AREA -COPY W414100A    -PRE UT2-                                     
011600     EJECT                                                                
011700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000                                                                          
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-IDARTNR-X.                                                     
012300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012400                                                                          
012500     03  W-IDSKYLT-X.                                                     
012600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
012700                                                                          
012800     03  W-IDDC-X.                                                        
012900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013000                                                                          
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                       VALUE '  '.                  
013400     88  BASEN-SLUT                          VALUE 'GB'.                  
013500                                                                          
013600 01  GODK-STATUSKODER.                                                    
013700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013800     SKIP3                                                                
013900 01  SSA1                        PIC X(160).                              
014000 01  SSA2                        PIC X(64).                               
014100     EJECT                                                                
014200*    --- IMS FUNKTIONSKODER                                               
014300*01  -COPY W0003                                                          
014400     EJECT                                                                
014500*    ---  DLI INPUT-OUTPUT AREA                                           
014600                                                                          
014700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
014800 01  DLI-IO-WDD301.                                                       
014900*    03  -COPY WDD301                                                     
015000     EJECT                                                                
015100                                                                          
015200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
015300 01  DLI-IO-WDD311.                                                       
015400*    03  -COPY WDD311                                                     
015500     EJECT                                                                
015600                                                                          
015700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
015800 01  DLI-IO-AREA-WDK711.                                                  
015900*    03  -COPY WDK711                                                     
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200                                                                          
016600*01  -COPY W0008  -PRE WDD3-                                              
016700     05  FILLER                  PIC X.                                   
016800                                                                          
016810*01  -COPY W0008  -PRE WDK7-                                              
016820     05  FILLER                  PIC X.                                   
016830                                                                          
016900     EJECT                                                                
017000 PROCEDURE DIVISION USING WDD3-PCB WDK7-PCB.                              
017100 MAIN SECTION.                                                            
017200     ENTRY 'DLITCBL' USING WDD3-PCB WDK7-PCB.                             
017300                                                                          
017400     PERFORM A-INIT                                                       
017500                                                                          
017600     PERFORM S01-LAES-W61221                                              
017700     IF END-OF-W61221                                                     
017800        CONTINUE                                                          
017900     ELSE                                                                 
018000        MOVE IN-IDFAKT  TO WS-SPAR-IDFAKT                                 
018100        MOVE IN-IDARTNR TO WS-SPAR-IDARTNR                                
018200     END-IF                                                               
018300                                                                          
018400     PERFORM UNTIL END-OF-W61221                                          
018500        MOVE IN-IDDISTR  TO TEST-IDDISTR                                  
018510        IF DIST35-NONVCC-CDC-REFILL OR                                    
018511           DIST35-NONVCC-VCC-REFILL OR                                    
018511           DIST35-NONVCC-VCC-TRANSFER                                     
018512          PERFORM D-CREATE-W61228                                         
018520        ELSE                                                              
018530          IF DIST35-NONVCC-NONVCC-REFILL OR                               
018530             DIST35-NONVCC-NONVCC-TRANSFER OR                             
                   DIST35-VCC-NONVCC-REFILL OR                                  
                   DIST35-VCC-NONVCC-TRANSFER                                   
018540            PERFORM D-CREATE-W61228                                       
018550          END-IF                                                          
018560                                                                          
018600          IF IN-IDFAKT = WS-SPAR-IDFAKT                                   
018700          AND IN-IDARTNR = WS-SPAR-IDARTNR                                
018800             IF (IN-KDANMORS = '00' OR '11' OR '21') AND                  
018900                (DIST35-REFILL-NA OR DIST35-REFILL-NS OR                  
                       DIST35-NDCUS-NDC52-REFILL OR                             
                       DIST35-NDCUS-NDC53-REFILL OR                             
                       DIST35-NDCCN-NDCUS-REFILL)                               
019000                                                                          
019100                MOVE IN-AREA TO UT-AREA                                   
019200                PERFORM B-SUMMERA                                         
019300             ELSE                                                         
019400                MOVE IN-AREA    TO UT-AREA                                
019500                ADD +1          TO WS-IDRADNR                             
019600                MOVE WS-IDRADNR TO UT-IDRADNR                             
019700                PERFORM S02-SKRIV-W61222                                  
019800             END-IF                                                       
019900          ELSE                                                            
020000             IF SW-SUM = JA                                               
020100                PERFORM C-SKAPA-SKRIV-UTPOST                              
020200             END-IF                                                       
020300             MOVE IN-IDFAKT  TO WS-SPAR-IDFAKT                            
020400             MOVE IN-IDARTNR TO WS-SPAR-IDARTNR                           
020500             MOVE ZERO TO WS-SUMMA-00                                     
020600                          WS-SUMMA-11                                     
020700                          WS-SUMMA-21                                     
020800                          WS-SUMMA-TOT                                    
020900                          WS-SUMMA-RED                                    
021000             MOVE NEJ TO SW-SUM                                           
021100             IF (IN-KDANMORS = '00' OR '11' OR '21') AND                  
018900                (DIST35-REFILL-NA OR DIST35-REFILL-NS OR                  
                       DIST35-NDCUS-NDC52-REFILL OR                             
                       DIST35-NDCUS-NDC53-REFILL OR                             
                       DIST35-NDCCN-NDCUS-REFILL)                               
021300                                                                          
021400                MOVE IN-AREA TO UT-AREA                                   
021500                PERFORM B-SUMMERA                                         
021600             ELSE                                                         
021700                MOVE IN-AREA    TO UT-AREA                                
021800                ADD +1          TO WS-IDRADNR                             
021900                MOVE WS-IDRADNR TO UT-IDRADNR                             
022000                PERFORM S02-SKRIV-W61222                                  
022100             END-IF                                                       
022200          END-IF                                                          
022210        END-IF                                                            
022500                                                                          
022600        PERFORM S01-LAES-W61221                                           
022700     END-PERFORM                                                          
022800                                                                          
022900     IF SW-SUM = JA                                                       
023000        PERFORM C-SKAPA-SKRIV-UTPOST                                      
023100     END-IF                                                               
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900                                                                          
024000     OPEN INPUT  W61221                                                   
024100     OPEN OUTPUT W61222                                                   
024110     OPEN OUTPUT W61228                                                   
024200                                                                          
024300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024400     PERFORM S11-NOLLSTALL-UT                                             
024500     MOVE ZERO TO WS-SUMMA-00                                             
024600                  WS-SUMMA-11                                             
024700                  WS-SUMMA-21                                             
024800                  WS-SUMMA-TOT                                            
024900                  WS-SUMMA-RED                                            
025000                  WS-IDRADNR                                              
025100     MOVE NEJ TO SW-SUM                                                   
025200     .                                                                    
025300     EJECT                                                                
025400 B-SUMMERA SECTION.                                                       
025410     MOVE 'B-SUMMA '   TO CURRENT-SECTION                                 
025500                                                                          
025600     IF IN-KDANMORS = '11'                                                
025700        ADD IN-KVLEVANM TO WS-SUMMA-11                                    
025800     ELSE                                                                 
025900        IF IN-KDANMORS = '00'                                             
026000           ADD IN-KVLEVANM TO WS-SUMMA-00                                 
026100        ELSE                                                              
026200           IF IN-KDANMORS = '21'                                          
026300              ADD IN-KVLEVANM TO WS-SUMMA-21                              
026400           END-IF                                                         
026500        END-IF                                                            
026600     END-IF                                                               
026700     MOVE JA TO SW-SUM                                                    
026800     .                                                                    
026900     EJECT                                                                
027000 C-SKAPA-SKRIV-UTPOST SECTION.                                            
027010     MOVE 'C-SKAPA-SKRIV-UTPOST '  TO CURRENT-SECTION                     
027100                                                                          
027200     COMPUTE WS-SUMMA-TOT = (WS-SUMMA-11 + WS-SUMMA-21)                   
027300                               - WS-SUMMA-00                              
027400     IF WS-SUMMA-TOT = 0                                                  
027500        CONTINUE                                                          
027600     ELSE                                                                 
027700        IF WS-SUMMA-TOT < 0                                               
027800           MOVE '00'         TO UT-KDANMORS                               
027900        ELSE                                                              
028000           IF WS-SUMMA-11 > WS-SUMMA-21                                   
028100              MOVE '11'      TO UT-KDANMORS                               
028200           ELSE                                                           
028300              MOVE '21'      TO UT-KDANMORS                               
028400           END-IF                                                         
028500        END-IF                                                            
028600        MOVE WS-SUMMA-TOT TO WS-SUMMA-RED                                 
028700        MOVE WS-SUMMA-RED TO UT-KVLEVANM                                  
028800        ADD +1               TO WS-IDRADNR                                
028900        MOVE WS-IDRADNR      TO UT-IDRADNR                                
029000        PERFORM S02-SKRIV-W61222                                          
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 D-CREATE-W61228   SECTION.                                               
029410     MOVE 'D-CREATE-W61228 '  TO CURRENT-SECTION                          
029500*----------------------------------------------------------               
029600* LIST W41841 WEB FOLLOW UP AVVIKELSER REFILL R34-LIST-DAY.               
029610*----------------------------------------------------------               
029700                                                                          
030200     MOVE IN-IDARTNR                TO UT2-100-IDARTNR                    
030300                                                                          
030310     IF DIST35-NONVCC-NONVCC-REFILL OR                                    
030310        DIST35-NONVCC-NONVCC-TRANSFER                                     
030311       MOVE IN-IDDC-LEV             TO UT2-100-IDDC-REC                   
030312                                       W-IDDC                             
030330     ELSE                                                                 
030400       MOVE IN-IDDC-SEND            TO UT2-100-IDDC-REC                   
030410                                       W-IDDC                             
030420     END-IF                                                               
030500                                                                          
030600     MOVE IN-IDDC-REC               TO UT2-100-IDDC-SEND                  
030800                                                                          
031000     MOVE IN-KVLEVANM               TO UT2-100-KVANTAL                    
031100                                                                          
031200     MOVE SPACE                     TO UT2-100-AVVIKELSETYP               
031300     IF IN-KDANMORS = '00'                                                
031400       MOVE 'SHORTAGE '             TO UT2-100-AVVIKELSETYP               
031500     END-IF                                                               
031510     IF IN-KDANMORS = '11'                                                
031600       MOVE 'OVERAGE '              TO UT2-100-AVVIKELSETYP               
031700     END-IF                                                               
031710     IF IN-KDANMORS = '21'                                                
031720       MOVE 'EXTRA PART'            TO UT2-100-AVVIKELSETYP               
031730     END-IF                                                               
031740     IF IN-KDANMORS = '43'                                                
031750       MOVE 'DAMAGE QTY'            TO UT2-100-AVVIKELSETYP               
031760     END-IF                                                               
031770     IF IN-KDANMORS = '60'                                                
031780       MOVE 'LOST CASE '            TO UT2-100-AVVIKELSETYP               
031790     END-IF                                                               
031791     IF IN-KDANMORS = '61'                                                
031792       MOVE 'FOUND CASE'            TO UT2-100-AVVIKELSETYP               
031793     END-IF                                                               
031794     IF IN-KDANMORS = '63'                                                
031795       MOVE 'DAM CASE  '            TO UT2-100-AVVIKELSETYP               
031796     END-IF                                                               
031800                                                                          
031810     MOVE IN-IDARTNR                TO W-IDARTNR                          
031900     PERFORM IMS-GU-WDD301-BSEQ                                           
032000     IF SEGMENT-FINNS                                                     
032100       MOVE 'GB' TO W-IDSKYLT                                             
032200       PERFORM IMS-GNP-WDD311                                             
032300       IF SEGMENT-FINNS                                                   
032400         MOVE TEXT-BEART      TO UT2-100-BEART                            
032500       ELSE                                                               
032600         MOVE SPACE           TO UT2-100-BEART                            
032700       END-IF                                                             
032800     END-IF                                                               
032900                                                                          
033000     PERFORM IMS-GU-WDK711                                                
033100     IF SEGMENT-FINNS                                                     
033200       MOVE SLAG-ADLAGOMR           TO UT2-100-ADLAGOMR                   
033300       MOVE SLAG-ADGANG             TO UT2-100-ADGANG                     
033400       MOVE SLAG-ADPLATS            TO UT2-100-ADPLATS                    
033500     ELSE                                                                 
033600       MOVE ZERO                    TO UT2-100-ADLAGOMR                   
033700       MOVE ZERO                    TO UT2-100-ADGANG                     
033800       MOVE ZERO                    TO UT2-100-ADPLATS                    
033900     END-IF                                                               
034000                                                                          
034100     PERFORM S03-WRITE-W61228                                             
034300     .                                                                    
034400     SKIP2                                                                
034500 Z-FINIT SECTION.                                                         
034600                                                                          
034700     CLOSE W61221                                                         
034800           W61222                                                         
034810           W61228                                                         
034900     SKIP2                                                                
035000     MOVE 'S' TO POSTSUM-OPKOD                                            
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
035300     EJECT                                                                
035400 S01-LAES-W61221  SECTION.                                                
035500                                                                          
035600     READ W61221 INTO IN-AREA                                             
035700     AT END                                                               
035800        SET END-OF-W61221 TO TRUE                                         
035900     NOT AT END                                                           
036000        MOVE 'W61221'   TO POSTSUM-FDNAMN                                 
036100        MOVE 'W61225D1' TO POSTSUM-DDNAMN2                                
036200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
036300        CALL POSTSUM USING POSTSUM-PARM                                   
036400     END-READ                                                             
036500     .                                                                    
036600     EJECT                                                                
036700 S02-SKRIV-W61222 SECTION.                                                
036800                                                                          
036900     WRITE UT-POST FROM UT-AREA                                           
037000                                                                          
037100     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
037200     MOVE 'W61222' TO POSTSUM-FDNAMN                                      
037300     MOVE 'W61225D2' TO POSTSUM-DDNAMN2                                   
037400     CALL POSTSUM USING POSTSUM-PARM                                      
037500     .                                                                    
037600     EJECT                                                                
037700 S03-WRITE-W61228 SECTION.                                                
037800                                                                          
037900     WRITE UT2-POST FROM UT2-AREA                                         
038000                                                                          
038200     MOVE 'W61228' TO POSTSUM-FDNAMN                                      
038300     MOVE 'W61225D3' TO POSTSUM-DDNAMN2                                   
038400     CALL POSTSUM USING POSTSUM-PARM                                      
038500     .                                                                    
038600     EJECT                                                                
038700 S11-NOLLSTALL-UT SECTION.                                                
038800                                                                          
038900     MOVE SPACE     TO UT-AREA                                            
039000     MOVE ZERO      TO UT-IDDISTR                                         
039100                       UT-IDKUNDNR                                        
039200                       UT-IDRAPPNR                                        
039300                       UT-IDARTNR                                         
039400                       UT-IDRADNR                                         
039500                       UT-IDLOPNRM                                        
039600                       UT-IDFTG                                           
039700                       UT-IDFAKT                                          
039800                       UT-IDKOLLI                                         
039900                       UT-KDEMBLEV                                        
040000                       UT-KDFRAKT                                         
040100                       UT-KVLEVANM                                        
040200                       UT-PRARTBTO                                        
040300                       UT-TIFAKT                                          
040400                       UT-TILEVANM                                        
040500                       UT-PRARTBTO-LOC                                    
040600                       UT-KDVALISO                                        
040700     .                                                                    
040800     EJECT                                                                
040900* --- IMS SEKTIONER ---                                                   
041000                                                                          
041100 IMS-GU-WDK711 SECTION.                                                   
041110     MOVE 'IMS-GU-WDK711 '  TO DBS-SECTION                                
041200                                                                          
041300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
041400          DELIMITED BY SIZE INTO SSA1                                     
041500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
041600          DELIMITED BY SIZE INTO SSA2                                     
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
041900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     SKIP2                                                                
042300 IMS-GU-WDD301-BSEQ SECTION.                                              
042310     MOVE 'IMS-GU-WDD301-BSEQ '  TO DBS-SECTION                           
042400                                                                          
042500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
042600          DELIMITED BY SIZE INTO SSA1                                     
042700     MOVE '  GE' TO GODK-STATUSKODER                                      
042800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
042900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
043000     PERFORM IMS-STATUSKONTROLL                                           
043100     .                                                                    
043200     SKIP3                                                                
043300 IMS-GNP-WDD311 SECTION.                                                  
043310     MOVE 'IMS-GNP-WDD311 '  TO DBS-SECTION                               
043400                                                                          
043500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '  GE' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
043900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     EJECT                                                                
044300                                                                          
044400 IMS-STATUSKONTROLL SECTION.                                              
044500     SET STATUS-IX TO 1                                                   
044600     SEARCH GODK-STATUS                                                   
044700       AT END                                                             
044800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044900           DELIMITED BY SIZE INTO FELTEXT                                 
045000         DISPLAY FELTEXT                                                  
045100         CALL FELLOG                                                      
045200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
045300         CONTINUE                                                         
045400     END-SEARCH                                                           
045500     .                                                                    
045600     SKIP2                                                                
