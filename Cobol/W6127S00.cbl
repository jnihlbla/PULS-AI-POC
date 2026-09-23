000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W6127S00.                                                 
000300 AUTHOR.        CAMELIA OLGRENER.                                         
000400 DATE-WRITTEN.  AUGUSTI 2012.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKAPAR FIL MED ARTIKLAR SOM HAR BLIVIT                
000900*        SKROTADE I OLIKA PROCESSER PÅ DC-N OCH SOM SKALL                 
001000*        LÄGGAS UPP I FOLLOW-UP PER DC-N SOM KÖR I WEBB-PULS.             
001100*                                                                         
001200*        INFILEN SKAPAS I RUTIN W414D1, W414J003.                         
001300*                                                                         
001400*        PGM:ET LÄSER WDD3                                                
001500*                     WDB6                                                
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*- - - - - - - - - - - - INFIL:                                           
002500     SELECT W4140S                       ASSIGN TO W6127SD1.              
002600*- - - - - - - - - - - - UTFIL:                                           
002700*          --- SKROTFIL TILL FOLLOW-UP                                    
002800     SELECT W6127S                       ASSIGN TO W6127SD2.              
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300 FD  W4140S                                                               
003400     RECORDING      F                                                     
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  INPOST.                                                              
003800*    03  -COPY W41403S  -L.                                               
003900*                                                                         
004000 FD  W6127S                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  UT-DAP-POST.                                                         
004500     03  FILLER         PIC X(21).                                        
004600                                                                          
004700 01  UT-DATA-POST.                                                        
004800*    03  -COPY W6127S01  -L.                                              
004900                                                                          
005000     SKIP2                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'W6127S00'.            
005500 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
005600 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
005700 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
005800 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) VALUE +0 COMP-3.          
005900 77  WS-KVANTAL1-REF             PIC 9(6)       VALUE ZERO.               
006000 77  WS-KVANTAL2-INH             PIC 9(6)       VALUE ZERO.               
006100 77  WS-KVANTAL3-RET             PIC 9(6)       VALUE ZERO.               
006200 77  WS-KVANTAL4-QAL             PIC 9(6)       VALUE ZERO.               
006300 77  WS-KVANTAL5-ECO             PIC 9(6)       VALUE ZERO.               
006400 77  WS-IDSKYLT-GB               PIC X(3)    VALUE 'GB '.                 
006500 77  WS-IDSKYLT-RCN              PIC X(3)    VALUE 'RCN'.                 
006600 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006700 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006800                                                                          
006900 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
007000     88 SKRIV-POST                           VALUE 'J'.                   
007100                                                                          
007200 77  FIRST-TIME-002-SW           PIC X       VALUE 'N'.                   
007300     88 FIRST-TIME-002                       VALUE 'J'.                   
007400                                                                          
007500 77  W4140S-EOF-SW               PIC X    VALUE 'N'.                      
007600     88  W4140S-EOF                       VALUE 'J'.                      
007700                                                                          
007800 77  SEG-WDB601-SW               PIC X.                                   
007900     88  SEG-WDB601-FINNS                    VALUE 'J'.                   
008000     88  SEG-WDB601-FINNS-EJ                 VALUE 'N'.                   
008100                                                                          
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600 01  TEST-IDDISTR     PIC S9(5)              VALUE ZERO  COMP-3.          
008700                                                                          
008800 01  ARBETSFALT.                                                          
008900     03  FILLER                  PIC X(8)    VALUE 'ARBFALT'.             
009000                                                                          
009100 01  WS-PUNKT                    PIC X       VALUE '.'.                   
009200                                                                          
009300                                                                          
009400******************************************************************        
009500*       CONSTANTS                                                *        
009600******************************************************************        
009700     SKIP2                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
009900 01  KONSTANTER.                                                          
010000     03  JA                      PIC X(1)    VALUE 'J'.                   
010100     03  NEJ                     PIC X(1)    VALUE 'N'.                   
010200     SKIP2                                                                
010300                                                                          
010400 01  SPAR-IDDC                   PIC X(2)   VALUE SPACE.                  
010500 01  SPAR-IDARTNR                PIC S9(9)  VALUE ZERO COMP-3.            
010600                                                                          
010700******************************************************************        
010800*       VARIABLES                                                *        
010900******************************************************************        
011000     SKIP2                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
011200 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
011300                                                                          
011400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011500 01  FILLER REDEFINES DAGENS-DATUM.                                       
011600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011900                                                                          
012000 01  DAGENS-VECKA                PIC 9(4)    VALUE ZERO.                  
012100     EJECT                                                                
012200 01  WS-CURRENT-DATE-TIME.                                                
012300     03 WS-CURRENT-DATE          PIC 9(8).                                
012400     03 WS-CURRENT-TIME          PIC 9(4).                                
012500     SKIP3                                                                
012600     EJECT                                                                
012700******************************************************************        
012800*       WORK AREA                                                *        
012900******************************************************************        
013000     SKIP2                                                                
013100     EJECT                                                                
013200******************************************************************        
013300*       OUTPUT AREA                                                       
013400******************************************************************        
013500     SKIP2                                                                
013600 01  DYNAMISKA-SUBPROGRAM.                                                
013700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013800   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013900   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
014000   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
014100   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
014200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
014300   03  WTRAUTF8                  PIC X(8)    VALUE 'WTRAUTF8'.            
014400     SKIP2                                                                
014500 01  RETURKODER.                                                          
014600     03  RKOD-ABEND              PIC S9(4) VALUE +0    COMP SYNC.         
014700     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
014800     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
014900     SKIP2                                                                
015000*    --- PARAMETRAR TILL DATKORT                                          
015100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6127P'.              
015200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015300     SKIP2                                                                
015400*01  -COPY WDATKORT                                                       
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL WDATKONV                                         
015700*01  -COPY WDATAREA                                                       
015800     EJECT                                                                
015900*    --- PARAMETRAR TILL POSTSUM                                          
016000*                                                                         
016100*01   -COPY W0005       -PRE POSTSUM-.                                    
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL WTRAUTF8                                         
016400*                                                                         
016500*01   -COPY WTRAUTF8                                                      
016600     EJECT                                                                
016700 01  IN-AREA-START               PIC X(16) VALUE 'IN-AREA  '.             
016800           SKIP2                                                          
016900 01  IN-AREA.                                                             
017000                                                                          
017100*    03  POST      -COPY W41403S  -PRE IN-.                               
017200                                                                          
017300     EJECT                                                                
017400 01  DAP-AREA-TYPE.                                                       
017500     03  FILLER                  PIC X(17)   VALUE                        
017600     '¤DAPW6127S-001  '.                                                  
017700 01  DAP-AREA-SUBTYPE.                                                    
017800     03  FILLER                  PIC X(04)   VALUE                        
017900     '¤DAP'.                                                              
018000     03 DAP-IDDC                 PIC X(2).                                
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE                        
018300                                            'UT-AREA-START '.             
018400                                                                          
018500 01  FILLER                      PIC X(24) VALUE 'STA-AREA'.              
018600 01  AREA  -COPY W6127S01  -PRE UT-                                       
018700                                                                          
018800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018900     SKIP3                                                                
019000 01  NYCKLAR-TILL-DLI.                                                    
019100     03   W-IDARTNR-X.                                                    
019200          05 W-IDARTNR           PIC S9(9)   COMP-3  VALUE ZERO.          
019300                                                                          
019400     03   W-IDSKYLT-X.                                                    
019500        05  W-IDSKYLT            PIC X(3)    VALUE SPACE.                 
019600                                                                          
019700     03  W-IDDC-WDB6-X.                                                   
019800        05  W-IDDC-WDB6          PIC X(2)    VALUE SPACE.                 
019900                                                                          
020000     SKIP2                                                                
020100*    --- STATUS-KOD FRÅN IMS                                              
020200 01  STATUS-WS                   PIC XX.                                  
020300     88  SEGMENT-FINNS                       VALUE '  '.                  
020400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020700     SKIP2                                                                
020800 01  GODK-STATUSKODER.                                                    
020900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021000     SKIP3                                                                
021100 01  SSA1                        PIC X(64).                               
021200 01  SSA2                        PIC X(64).                               
021300     EJECT                                                                
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD311'.         
021900 01  DLI-IO-WDD311.                                                       
022000*    03  -COPY WDD311                                                     
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB611'.         
022300 01  DLI-IO-WDB601.                                                       
022400*    03  -COPY WDB601                                                     
022500     EJECT                                                                
022600 LINKAGE SECTION.                                                         
022700*01  -COPY W0008  -PRE WDD3-                                              
022800     05  FILLER                  PIC X.                                   
022900*01  -COPY W0008  -PRE WDB6-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING WDD3-PCB WDB6-PCB.                             
023300 MAIN SECTION.                                                            
023400     ENTRY 'DLITCBL' USING WDD3-PCB WDB6-PCB.                             
023500                                                                          
023600     PERFORM A-INIT                                                       
023700     PERFORM S01-LAES-W4140S                                              
023800                                                                          
023900     PERFORM UNTIL W4140S-EOF                                             
024000       MOVE IN-IDDC        TO SPAR-IDDC                                   
024100       MOVE IN-IDARTNR     TO SPAR-IDARTNR                                
024200       PERFORM B-TA-FRAM-IDCITY                                           
024300                                                                          
024400       PERFORM UNTIL IN-IDDC NOT = SPAR-IDDC OR W4140S-EOF                
024500         IF IN-IDARTNR NOT = SPAR-IDARTNR                                 
024600           PERFORM D-SKRIVA-RAD                                           
024700           PERFORM C-RAKNA-TOTALER-PER-ART                                
024800         ELSE                                                             
024900           PERFORM C-RAKNA-TOTALER-PER-ART                                
025000         END-IF                                                           
025100         PERFORM S01-LAES-W4140S                                          
025200       END-PERFORM                                                        
025300                                                                          
025400       PERFORM D-SKRIVA-RAD                                               
025500     END-PERFORM                                                          
025600                                                                          
025700     PERFORM Z-FINIT                                                      
025800                                                                          
025900     MOVE ZERO TO RETURN-CODE                                             
026000     GOBACK                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 A-INIT SECTION.                                                          
026400                                                                          
026500     OPEN OUTPUT W6127S                                                   
026600     OPEN INPUT  W4140S                                                   
026700                                                                          
026800     MOVE SPACE       TO SPAR-IDDC                                        
026900     MOVE ZERO        TO SPAR-IDARTNR                                     
027000                         WS-KVANTAL1-REF                                  
027100                         WS-KVANTAL2-INH                                  
027200                         WS-KVANTAL3-RET                                  
027300                         WS-KVANTAL4-QAL                                  
027400                         WS-KVANTAL5-ECO                                  
027500                                                                          
027600     ACCEPT DAGENS-DATUM  FROM DATE                                       
027700     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
027800                                                                          
027900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028000     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
028100     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
028200     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
028300                                                                          
028400     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
028500     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
028600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
028700                         DAT-O-TIDATUM DAT-KDSVAR                         
028800                                                                          
028900     IF DAT-KDSVAR-OK                                                     
029000       MOVE DAT-TIAAVV-GRP   TO DAGENS-VECKA                              
029100                                                                          
029200     ELSE                                                                 
029300       MOVE 'FEL I WDATKONV' TO FELTEXT-STR                               
029400       DISPLAY FELTEXT                                                    
029500       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
029600       PERFORM S99-ABEND                                                  
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 B-TA-FRAM-IDCITY SECTION.                                                
030100                                                                          
030200     MOVE IN-IDDC   TO W-IDDC-WDB6                                        
030300     PERFORM IMS-GU-WDB601                                                
030400     IF SEGMENT-FINNS                                                     
030500       MOVE JA      TO SEG-WDB601-SW                                      
030600     ELSE                                                                 
030700       MOVE NEJ     TO SEG-WDB601-SW                                      
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 C-RAKNA-TOTALER-PER-ART SECTION.                                         
031200                                                                          
031300     IF IN-KDSORT1 = 1                                                    
031400       COMPUTE WS-KVANTAL1-REF = WS-KVANTAL1-REF +                        
031500                                 IN-KVBEART                               
031600     END-IF                                                               
031700                                                                          
031800     IF IN-KDSORT1 = 2                                                    
031900       COMPUTE WS-KVANTAL2-INH = WS-KVANTAL2-INH +                        
032000                                 IN-KVBEART                               
032100     END-IF                                                               
032200                                                                          
032300     IF IN-KDSORT1 = 3                                                    
032400       COMPUTE WS-KVANTAL3-RET = WS-KVANTAL3-RET +                        
032500                                 IN-KVBEART                               
032600     END-IF                                                               
032700                                                                          
032800     IF IN-KDSORT1 = 4                                                    
032900       COMPUTE WS-KVANTAL4-QAL = WS-KVANTAL4-QAL +                        
033000                                 IN-KVBEART                               
033100     END-IF                                                               
033200                                                                          
033300     IF IN-KDSORT1 = 5                                                    
033400       COMPUTE WS-KVANTAL5-ECO = WS-KVANTAL5-ECO +                        
033500                                 IN-KVBEART                               
033600     END-IF                                                               
033700                                                                          
033800     IF IN-KDSORT1 = 6                                                    
033900       CONTINUE                                                           
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 D-SKRIVA-RAD SECTION.                                                    
034400                                                                          
034500     IF WS-KVANTAL1-REF > 0 OR                                            
034600        WS-KVANTAL2-INH > 0 OR                                            
034700        WS-KVANTAL3-RET > 0 OR                                            
034800        WS-KVANTAL4-QAL > 0 OR                                            
034900        WS-KVANTAL5-ECO > 0                                               
035000       PERFORM S10-SKRIV-START-DAP                                        
035100                                                                          
035200       PERFORM DA-MOVE-TO-OUTPUT                                          
035300                                                                          
035400       PERFORM S11-SKRIV-UTPOST                                           
035500                                                                          
035600     END-IF                                                               
035700     IF NOT W4140S-EOF                                                    
035800       MOVE IN-IDARTNR            TO SPAR-IDARTNR                         
035900       MOVE ZERO                  TO WS-KVANTAL1-REF                      
036000                                     WS-KVANTAL2-INH                      
036100                                     WS-KVANTAL3-RET                      
036200                                     WS-KVANTAL4-QAL                      
036300                                     WS-KVANTAL5-ECO                      
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800 DA-MOVE-TO-OUTPUT SECTION.                                               
036900                                                                          
037000     MOVE SPAR-IDDC               TO UT-IDDC                              
037100     IF SEG-WDB601-FINNS                                                  
037200       MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                               
037300                                  TO UT-ADCITY                            
037400     ELSE                                                                 
037500       MOVE SPACE                 TO UT-ADCITY                            
037600     END-IF                                                               
037700                                                                          
037800     MOVE DAGENS-VECKA            TO UT-TIAAVV                            
037900     MOVE SPAR-IDARTNR            TO UT-IDARTNR                           
038000                                                                          
038100*    HÄR HÄMTAS BENÄMNINGEN FÖR IDARTNR SOM SKALL SKRIVAS UT.             
038200     IF SEG-WDB601-FINNS                                                  
             MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                            
             IF DCS-UNICODE-IDSKYLT                                             
               MOVE 'UTF8'             TO TRAUTF8-KDCP                          
             ELSE                                                               
               MOVE '278 '             TO TRAUTF8-KDCP                          
             END-IF                                                             
           ELSE                                                                 
             MOVE WS-IDSKYLT-GB         TO W-IDSKYLT-X                          
             MOVE '278'                 TO TRAUTF8-KDCP                         
           END-IF                                                               
039400     MOVE SPAR-IDARTNR            TO W-IDARTNR                            
039500     PERFORM IMS-GU-WDD311-TEXT                                           
039600                                                                          
039700     IF SEGMENT-FINNS                                                     
039800       MOVE TEXT-BEART            TO TRAUTF8-TECONV-FROM                  
039900     ELSE                                                                 
040000       MOVE SPACE                 TO TRAUTF8-TECONV-FROM                  
040100       MOVE '278'                 TO TRAUTF8-KDCP                         
040200     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311-TEXT                                          
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
040300     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
040400     MOVE TRAUTF8-TECONV-TO       TO UT-BEART                             
040500                                                                          
040600     MOVE WS-KVANTAL1-REF         TO UT-KVANTAL-REF                       
040700     MOVE WS-KVANTAL2-INH         TO UT-KVANTAL-INH                       
040800     MOVE WS-KVANTAL3-RET         TO UT-KVANTAL-RET                       
040900     MOVE WS-KVANTAL4-QAL         TO UT-KVANTAL-QAL                       
041000     MOVE WS-KVANTAL5-ECO         TO UT-KVANTAL-ECO                       
041100     .                                                                    
041200     EJECT                                                                
041300                                                                          
041400 S01-LAES-W4140S  SECTION.                                                
041500                                                                          
041600     READ W4140S INTO IN-AREA                                             
041700     AT END                                                               
041800        MOVE HIGH-VALUE TO IN-AREA                                        
041900        SET W4140S-EOF TO TRUE                                            
042000                                                                          
042100     NOT AT END                                                           
042200        MOVE 'W6127S' TO POSTSUM-FDNAMN                                   
042300        MOVE 'W6127SD1' TO POSTSUM-DDNAMN2                                
042400        MOVE SPACE     TO POSTSUM-TRANSTYP                                
042500        CALL POSTSUM USING POSTSUM-PARM                                   
042600     END-READ                                                             
042700     .                                                                    
042800     EJECT                                                                
042900 S10-SKRIV-START-DAP SECTION.                                             
043000                                                                          
043100     IF DAP-IDDC = SPAR-IDDC                                              
043200       CONTINUE                                                           
043300     ELSE                                                                 
043400       WRITE UT-DAP-POST FROM DAP-AREA-TYPE                               
043500       PERFORM S20-POSTSUM-UTPOST                                         
043600                                                                          
043700       MOVE SPAR-IDDC      TO DAP-IDDC                                    
043800       WRITE UT-DAP-POST FROM DAP-AREA-SUBTYPE                            
043900       PERFORM S20-POSTSUM-UTPOST                                         
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 S11-SKRIV-UTPOST SECTION.                                                
044400                                                                          
044500     WRITE UT-DATA-POST FROM UT-AREA                                      
044600     PERFORM S20-POSTSUM-UTPOST                                           
044700     .                                                                    
044800     EJECT                                                                
044900 S20-POSTSUM-UTPOST SECTION.                                              
045000                                                                          
045100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
045200     MOVE 'W6127S'   TO POSTSUM-FDNAMN                                    
045300     MOVE 'W6127SD2' TO POSTSUM-DDNAMN2                                   
045400     CALL POSTSUM USING POSTSUM-PARM                                      
045500     .                                                                    
045600     EJECT                                                                
045700 Z-FINIT SECTION.                                                         
045800                                                                          
045900     CLOSE W4140S                                                         
046000           W6127S                                                         
046100     SKIP2                                                                
046200     MOVE 'S' TO POSTSUM-OPKOD                                            
046300     CALL POSTSUM USING POSTSUM-PARM                                      
046400     .                                                                    
046500     EJECT                                                                
046600 IMS-GU-WDD311-TEXT SECTION.                                              
046700                                                                          
046800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
046900            DELIMITED BY SIZE INTO SSA1                                   
047000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
047100            DELIMITED BY SIZE INTO SSA2                                   
047200     MOVE '  GE' TO GODK-STATUSKODER                                      
047300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
047400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUSKONTROLL                                           
047600     .                                                                    
047700     EJECT                                                                
047800 IMS-GU-WDB601 SECTION.                                                   
047900                                                                          
048000     STRING 'WDB601  (IDDC     =' W-IDDC-WDB6-X ')'                       
048100          DELIMITED BY SIZE INTO SSA1                                     
048200     MOVE '  GE'              TO GODK-STATUSKODER                         
048300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
048400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700     EJECT                                                                
048800 S99-ABEND SECTION.                                                       
048900                                                                          
049000     SKIP2                                                                
049100     MOVE 'S' TO POSTSUM-OPKOD                                            
049200     CALL POSTSUM USING POSTSUM-PARM                                      
049300     CALL ABEND USING RKOD-ABEND                                          
049400     .                                                                    
049500     EJECT                                                                
049600 IMS-STATUSKONTROLL SECTION.                                              
049700     SET STATUS-IX TO 1                                                   
049800     SEARCH GODK-STATUS                                                   
049900       AT END                                                             
050000         CALL FELLOG                                                      
050100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050200         CONTINUE                                                         
050300     END-SEARCH                                                           
050400     .                                                                    
