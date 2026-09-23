000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W6127P00.                                                 
000300 AUTHOR.        CAMELIA OLGRENER.                                         
000400 DATE-WRITTEN.  AUGUSTI 2012.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKAPAR FIL MED TOTAL ARTIKLAR SOM HAR BLIVIT          
000900*        SKROTADE I OLIKA PROCESSER PÅ DC-N PER TYP AV SKROT              
001000*        OCH SOM KOMMER ATT LÄGGAS UPP I MANAGEMENT 'PERIODICALLY         
001100*        FOLLOW-UP' PER DC-N SOM KÖR I WEBB-PULS.                         
001200*                                                                         
001300*        INFILEN SKAPAS I RUTIN W414D1, W414J003.                         
001400*                                                                         
001500*        PGM:ET LÄSER WDB6, WDK6, WDK7.                                   
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*- - - - - - - - - - - - INFIL:                                           
002500     SELECT W4140SV                      ASSIGN TO W6127PD1.              
002600*- - - - - - - - - - - - UTFIL:                                           
002700*          --- SKROTFIL TILL MANAGEMENT FOLLOW-UP                         
002800     SELECT W6127P                       ASSIGN TO W6127PD2.              
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300 FD  W4140SV                                                              
003400     RECORDING      F                                                     
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  INPOST.                                                              
003800*    03  -COPY W41403S  -L.                                               
003900*                                                                         
004000 FD  W6127P                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  UT-DAP-POST.                                                         
004500     03  FILLER         PIC X(21).                                        
004600                                                                          
004700 01  UT-DATA-POST.                                                        
004800*    03  -COPY W6127P01 -L.                                               
004900                                                                          
005000     SKIP2                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8) VALUE 'W6127P00'.               
005500                                                                          
005600 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
005700 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
005800 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
005900 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) VALUE +0 COMP-3.          
006000 77  WS-KVANTAL1-REF             PIC 9(6)       VALUE ZERO.               
006100 77  WS-KVANTAL2-INH             PIC 9(6)       VALUE ZERO.               
006200 77  WS-KVANTAL3-RET             PIC 9(6)       VALUE ZERO.               
006300 77  WS-KVANTAL4-QAL             PIC 9(6)       VALUE ZERO.               
006400 77  WS-KVANTAL5-ECO             PIC 9(6)       VALUE ZERO.               
006500 77  WS-KVANTAL6-MIX             PIC 9(6)       VALUE ZERO.               
006600 77  WS-KVANTAL7-R34             PIC 9(6)       VALUE ZERO.               
006700 77  WS-KVANTAL-DC               PIC 9(6)       VALUE ZERO.               
006800 77  WS-SUARTSTD1-REF            PIC 9(8)V9(2)  VALUE ZERO.               
006900 77  WS-SUARTSTD2-INH            PIC 9(8)V9(2)  VALUE ZERO.               
007000 77  WS-SUARTSTD3-RET            PIC 9(8)V9(2)  VALUE ZERO.               
007100 77  WS-SUARTSTD4-QAL            PIC 9(8)V9(2)  VALUE ZERO.               
007200 77  WS-SUARTSTD5-ECO            PIC 9(8)V9(2)  VALUE ZERO.               
007300 77  WS-SUARTSTD6-MIX            PIC 9(8)V9(2)  VALUE ZERO.               
007400 77  WS-SUARTSTD7-R34            PIC 9(8)V9(2)  VALUE ZERO.               
007500 77  WS-SUARTSTD-DC              PIC 9(8)V9(2)  VALUE ZERO.               
007600 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
007700 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
007800                                                                          
007900 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
008000     88 SKRIV-POST                           VALUE 'J'.                   
008100                                                                          
008200 77  W4140SV-EOF-SW              PIC X    VALUE 'N'.                      
008300     88  W4140SV-EOF                      VALUE 'J'.                      
008400                                                                          
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900 01  TEST-IDDISTR     PIC S9(5)              VALUE ZERO  COMP-3.          
009000                                                                          
009100 01  ARBETSFALT.                                                          
009200     03  FILLER                  PIC X(8)    VALUE 'ARBFALT'.             
009300                                                                          
009400 01  WS-PUNKT                    PIC X       VALUE '.'.                   
009500                                                                          
009600                                                                          
009700******************************************************************        
009800*       CONSTANTS                                                *        
009900******************************************************************        
010000     SKIP2                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
010200 01  KONSTANTER.                                                          
010300     03  JA                      PIC X(1)    VALUE 'J'.                   
010400     03  NEJ                     PIC X(1)    VALUE 'N'.                   
010500     SKIP2                                                                
010600                                                                          
010700 01  SPAR-KDMFUP                 PIC X(2)    VALUE SPACE.                 
010800 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
010900 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO  COMP-3.          
011000                                                                          
011100******************************************************************        
011200*       VARIABLES                                                *        
011300******************************************************************        
011400     SKIP2                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
011600                                                                          
011700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011800 01  FILLER REDEFINES DAGENS-DATUM.                                       
011900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012200                                                                          
012300 01  DAGENS-PERIOD               PIC 9(4)    VALUE ZERO.                  
012400     EJECT                                                                
012500 01  WS-CURRENT-DATE-TIME.                                                
012600     03 WS-CURRENT-DATE          PIC 9(8).                                
012700     03 WS-CURRENT-TIME          PIC 9(4).                                
012800     SKIP3                                                                
012900     EJECT                                                                
013000******************************************************************        
013100*       WORK AREA                                                *        
013200******************************************************************        
013300     SKIP2                                                                
013400     EJECT                                                                
013500*01 -COPY WWDC99                                                          
013600     EJECT                                                                
013700 01  DYNAMISKA-SUBPROGRAM.                                                
013800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
014000   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
014100   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
014200   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
014300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
014310   03  WL10WBDC                  PIC X(8)    VALUE 'WL10WBDC'.            
014400     SKIP2                                                                
014500 01  RETURKODER.                                                          
014600   03  RKOD-ABEND                PIC S9(4) VALUE +0    COMP SYNC.         
014700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) VALUE +16   COMP SYNC.         
014800   03  RKOD-ABEND-MED-DUMP       PIC S9(4) VALUE +1000 COMP SYNC.         
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
015810*    --- PARAMETRAR TILL WL10WBDC                                         
015820*01  -COPY WL10WBDC                                                       
015830     EJECT                                                                
015900*    --- PARAMETRAR TILL POSTSUM                                          
016000*                                                                         
016100*01   -COPY W0005       -PRE POSTSUM-.                                    
016200     EJECT                                                                
016300 01  IN-AREA-START               PIC X(16) VALUE 'IN-AREA  '.             
016400           SKIP2                                                          
016500 01  IN-AREA.                                                             
016600                                                                          
016700*    03  POST      -COPY W41403S  -PRE IN-.                               
016800                                                                          
016900******************************************************************        
017000*       OUTPUT AREA                                                       
017100******************************************************************        
017200     SKIP2                                                                
017300     EJECT                                                                
017400 01  DAP-AREA-TYPE.                                                       
017500     03  FILLER                  PIC X(17)   VALUE                        
017600     '¤DAPW6127P-001  '.                                                  
017700 01  DAP-AREA-SUBTYPE.                                                    
017800     03  FILLER                  PIC X(04)   VALUE                        
017900     '¤DAP'.                                                              
018100     03 DAP-KDMFUP               PIC X(2).                                
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE                        
018400                                            'UT-AREA-START '.             
018500                                                                          
018600 01  FILLER                      PIC X(24) VALUE 'STA-AREA'.              
018700 01  UT-AREA.                                                             
018800*    03  POST      -COPY W6127P01  -PRE UT-.                              
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019100     SKIP3                                                                
019200 01  NYCKLAR-TILL-DLI.                                                    
019300                                                                          
019400     03  W-IDDC-WDB6-X.                                                   
019500        05  W-IDDC-WDB6          PIC X(2)    VALUE SPACE.                 
019600                                                                          
019700     03   W-IDARTNR-X.                                                    
019800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019900                                                                          
020000     03   W-IDDC-X.                                                       
020100        05  W-IDDC               PIC X(2)    VALUE SPACE.                 
020200     SKIP2                                                                
020300*    --- STATUS-KOD FRÅN IMS                                              
020400 01  STATUS-WS                   PIC XX.                                  
020500     88  SEGMENT-FINNS                       VALUE '  '.                  
020600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020900     SKIP2                                                                
021000 01  GODK-STATUSKODER.                                                    
021100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200     SKIP3                                                                
021300 01  SSA1                        PIC X(64).                               
021400 01  SSA2                        PIC X(64).                               
021500     EJECT                                                                
021600*    --- IMS FUNKTIONSKODER                                               
021700*01  -COPY W0003                                                          
021800     EJECT                                                                
021900*    ---  DLI INPUT-OUTPUT AREA                                           
022000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB611'.         
022100 01  DLI-IO-WDB601.                                                       
022200*    03  -COPY WDB601                                                     
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
022500 01  DLI-IO-WDK601.                                                       
022600*    03  -COPY WDK601                                                     
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
022900 01  DLI-IO-WDK611.                                                       
023000*    03  -COPY WDK611                                                     
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
023300 01  DLI-IO-WDK711.                                                       
023400*    03  -COPY WDK711                                                     
023500     EJECT                                                                
023600                                                                          
023700 LINKAGE SECTION.                                                         
023800*01  -COPY W0008  -PRE WDB6-                                              
023900     05  FILLER                  PIC X.                                   
024000*01  -COPY W0008  -PRE WDK6-                                              
024100     05  FILLER                  PIC X.                                   
024200*01  -COPY W0008  -PRE WDK7-                                              
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500 PROCEDURE DIVISION  USING WDB6-PCB WDK6-PCB WDK7-PCB.                    
024600 MAIN SECTION.                                                            
024700     ENTRY 'DLITCBL' USING WDB6-PCB WDK6-PCB WDK7-PCB.                    
024800                                                                          
024900     PERFORM A-INIT                                                       
025000     PERFORM S01-LAES-W4140SV                                             
025100                                                                          
025200     IF NOT W4140SV-EOF                                                   
025300       PERFORM S10-SKRIV-START-DAP                                        
025400       MOVE IN-IDDC    TO SPAR-IDDC                                       
025410                          WS-IDDC                                         
025500       MOVE IN-KDMFUP  TO SPAR-KDMFUP                                     
025700                                                                          
025800       PERFORM UNTIL W4140SV-EOF                                          
025900         PERFORM UNTIL IN-KDMFUP NOT = SPAR-KDMFUP OR W4140SV-EOF         
026000           IF IN-IDDC NOT = SPAR-IDDC                                     
026100             PERFORM B-TA-FRAM-IDCITY                                     
026200             PERFORM D-SKRIV-RAD                                          
026300             PERFORM C-RAKNA-TOTALER-PER-DC                               
026400           ELSE                                                           
026500             PERFORM C-RAKNA-TOTALER-PER-DC                               
026600           END-IF                                                         
026700           PERFORM S01-LAES-W4140SV                                       
026800         END-PERFORM                                                      
026900                                                                          
027000         PERFORM B-TA-FRAM-IDCITY                                         
027100         PERFORM D-SKRIV-RAD                                              
027200                                                                          
027300         IF NOT W4140SV-EOF                                               
027400           PERFORM S10-SKRIV-START-DAP                                    
027500                                                                          
027600           MOVE IN-IDDC   TO SPAR-IDDC                                    
027610                             WS-IDDC                                      
027700           MOVE IN-KDMFUP TO SPAR-KDMFUP                                  
027800         END-IF                                                           
027900       END-PERFORM                                                        
028000     END-IF                                                               
028100                                                                          
028200     PERFORM Z-FINIT                                                      
028300                                                                          
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     OPEN OUTPUT W6127P                                                   
029100     OPEN INPUT  W4140SV                                                  
029200                                                                          
029300     MOVE SPACE       TO SPAR-IDDC                                        
029400     MOVE ZERO        TO SPAR-KDMFUP                                      
029410                         SPAR-IDARTNR                                     
029500                         WS-SUARTSTD1-REF                                 
029600                         WS-SUARTSTD2-INH                                 
029700                         WS-SUARTSTD3-RET                                 
029800                         WS-SUARTSTD4-QAL                                 
029900                         WS-SUARTSTD5-ECO                                 
030000                         WS-SUARTSTD6-MIX                                 
030100                         WS-SUARTSTD7-R34                                 
030200                         WS-SUARTSTD-DC                                   
030300                         WS-KVANTAL1-REF                                  
030400                         WS-KVANTAL2-INH                                  
030500                         WS-KVANTAL3-RET                                  
030600                         WS-KVANTAL4-QAL                                  
030700                         WS-KVANTAL5-ECO                                  
030800                         WS-KVANTAL6-MIX                                  
030900                         WS-KVANTAL7-R34                                  
031000                         WS-KVANTAL-DC                                    
031100                                                                          
031200     ACCEPT DAGENS-DATUM  FROM DATE                                       
031300     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
031400                                                                          
031500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
031600     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
031700     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
031800     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
031900                                                                          
032000     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
032100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
032200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
032300                         DAT-O-TIDATUM DAT-KDSVAR                         
032400                                                                          
032500     IF DAT-KDSVAR-OK                                                     
032600       MOVE DAT-TIAAPP  TO DAGENS-PERIOD                                  
032700                                                                          
032800     ELSE                                                                 
032900       MOVE 'FEL I WDATKONV' TO FELTEXT-STR                               
033000       DISPLAY FELTEXT                                                    
033100       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
033200       PERFORM S99-ABEND                                                  
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 B-TA-FRAM-IDCITY SECTION.                                                
033700                                                                          
033800     MOVE SPAR-IDDC TO W-IDDC-WDB6                                        
033900     PERFORM IMS-GU-WDB601                                                
034000     .                                                                    
034100     EJECT                                                                
034200 C-RAKNA-TOTALER-PER-DC SECTION.                                          
034300                                                                          
034400     IF SPAR-IDARTNR NOT = IN-IDARTNR                                     
034500       MOVE IN-IDARTNR TO W-IDARTNR                                       
034501                          SPAR-IDARTNR                                    
034520       IF LDC-CN OR NDC-CN                                                
034530         MOVE IN-IDDC  TO W-IDDC                                          
034600         PERFORM IMS-GU-WDK711                                            
034700         IF SEGMENT-SAKNAS                                                
034720           MOVE ZERO   TO SLAG-PRAVCOST                                   
034721         END-IF                                                           
034730         PERFORM CA-RAKNA-TOTALER-CN                                      
034740       ELSE                                                               
034760         PERFORM IMS-GU-WDK611                                            
034761         IF SEGMENT-SAKNAS                                                
034762           MOVE ZERO   TO CLAG-PRARTSTD                                   
034763         END-IF                                                           
034764         PERFORM CB-RAKNA-TOTALER                                         
034771       END-IF                                                             
034772                                                                          
034780     ELSE                                                                 
034790       IF LDC-CN OR NDC-CN                                                
034800         PERFORM CA-RAKNA-TOTALER-CN                                      
034900       ELSE                                                               
035000         PERFORM CB-RAKNA-TOTALER                                         
035100       END-IF                                                             
035200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044410 CA-RAKNA-TOTALER-CN SECTION.                                             
044411                                                                          
044420     IF IN-KDSORT1 = 1                                                    
044440       COMPUTE WS-KVANTAL1-REF  = WS-KVANTAL1-REF + IN-KVBEART            
044460       COMPUTE WS-SUARTSTD1-REF = WS-SUARTSTD1-REF +                      
044461                                  (IN-KVBEART * SLAG-PRAVCOST)            
044470       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044480       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044481                                  (IN-KVBEART * SLAG-PRAVCOST)            
044490     END-IF                                                               
044491                                                                          
044492     IF IN-KDSORT1 = 2                                                    
044493       COMPUTE WS-KVANTAL2-INH  = WS-KVANTAL2-INH + IN-KVBEART            
044494       COMPUTE WS-SUARTSTD2-INH = WS-SUARTSTD2-INH +                      
044495                                  (IN-KVBEART * SLAG-PRAVCOST)            
044496       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044497       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044498                                  (IN-KVBEART * SLAG-PRAVCOST)            
044500     END-IF                                                               
044501                                                                          
044502     IF IN-KDSORT1 = 3                                                    
044503       COMPUTE WS-KVANTAL3-RET  = WS-KVANTAL3-RET + IN-KVBEART            
044504       COMPUTE WS-SUARTSTD3-RET = WS-SUARTSTD3-RET +                      
044505                                  (IN-KVBEART * SLAG-PRAVCOST)            
044506       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044507       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044508                                  (IN-KVBEART * SLAG-PRAVCOST)            
044510     END-IF                                                               
044511                                                                          
044512     IF IN-KDSORT1 = 4                                                    
044513       COMPUTE WS-KVANTAL4-QAL  = WS-KVANTAL4-QAL + IN-KVBEART            
044514       COMPUTE WS-SUARTSTD4-QAL = WS-SUARTSTD4-QAL +                      
044515                                  (IN-KVBEART * SLAG-PRAVCOST)            
044516       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044517       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044518                                  (IN-KVBEART * SLAG-PRAVCOST)            
044520     END-IF                                                               
044521                                                                          
044522     IF IN-KDSORT1 = 5                                                    
044523       COMPUTE WS-KVANTAL5-ECO  = WS-KVANTAL5-ECO + IN-KVBEART            
044524       COMPUTE WS-SUARTSTD5-ECO = WS-SUARTSTD5-ECO +                      
044525                                  (IN-KVBEART * SLAG-PRAVCOST)            
044526       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044527       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044528                                  (IN-KVBEART * SLAG-PRAVCOST)            
044530     END-IF                                                               
044531                                                                          
044532     IF IN-KDSORT1 = 6                                                    
044533       COMPUTE WS-KVANTAL6-MIX  = WS-KVANTAL6-MIX + IN-KVBEART            
044534       COMPUTE WS-SUARTSTD6-MIX = WS-SUARTSTD6-MIX +                      
044535                                  (IN-KVBEART * SLAG-PRAVCOST)            
044536     END-IF                                                               
044537                                                                          
044538     IF IN-KDSORT1 = 7                                                    
044539       COMPUTE WS-KVANTAL7-R34  = WS-KVANTAL7-R34 + IN-KVBEART            
044540       COMPUTE WS-SUARTSTD7-R34 = WS-SUARTSTD7-R34 +                      
044541                                  (IN-KVBEART * SLAG-PRAVCOST)            
044542     END-IF                                                               
044543     .                                                                    
044544     EJECT                                                                
044545 CB-RAKNA-TOTALER SECTION.                                                
044546                                                                          
044551     IF IN-KDSORT1 = 1                                                    
044552       COMPUTE WS-KVANTAL1-REF  = WS-KVANTAL1-REF + IN-KVBEART            
044553       COMPUTE WS-SUARTSTD1-REF = WS-SUARTSTD1-REF +                      
044554                                  (IN-KVBEART * CLAG-PRARTSTD)            
044555       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044556       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044557                                  (IN-KVBEART * CLAG-PRARTSTD)            
044562     END-IF                                                               
044563                                                                          
044564     IF IN-KDSORT1 = 2                                                    
044565       COMPUTE WS-KVANTAL2-INH  = WS-KVANTAL2-INH + IN-KVBEART            
044566       COMPUTE WS-SUARTSTD2-INH = WS-SUARTSTD2-INH +                      
044567                                  (IN-KVBEART * CLAG-PRARTSTD)            
044568       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044569       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044570                                  (IN-KVBEART * CLAG-PRARTSTD)            
044572     END-IF                                                               
044573                                                                          
044574     IF IN-KDSORT1 = 3                                                    
044575       COMPUTE WS-KVANTAL3-RET  = WS-KVANTAL3-RET + IN-KVBEART            
044576       COMPUTE WS-SUARTSTD3-RET = WS-SUARTSTD3-RET +                      
044577                                  (IN-KVBEART * CLAG-PRARTSTD)            
044578       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044579       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044580                                  (IN-KVBEART * CLAG-PRARTSTD)            
044582     END-IF                                                               
044583                                                                          
044584     IF IN-KDSORT1 = 4                                                    
044585       COMPUTE WS-KVANTAL4-QAL  = WS-KVANTAL4-QAL + IN-KVBEART            
044586       COMPUTE WS-SUARTSTD4-QAL = WS-SUARTSTD4-QAL +                      
044587                                  (IN-KVBEART * CLAG-PRARTSTD)            
044588       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044589       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044590                                  (IN-KVBEART * CLAG-PRARTSTD)            
044592     END-IF                                                               
044593                                                                          
044600     IF IN-KDSORT1 = 5                                                    
044601       COMPUTE WS-KVANTAL5-ECO  = WS-KVANTAL5-ECO + IN-KVBEART            
044602       COMPUTE WS-SUARTSTD5-ECO = WS-SUARTSTD5-ECO +                      
044603                                  (IN-KVBEART * CLAG-PRARTSTD)            
044604       COMPUTE WS-KVANTAL-DC    = WS-KVANTAL-DC  + IN-KVBEART             
044605       COMPUTE WS-SUARTSTD-DC   = WS-SUARTSTD-DC +                        
044606                                  (IN-KVBEART * CLAG-PRARTSTD)            
044608     END-IF                                                               
044609                                                                          
044610     IF IN-KDSORT1 = 6                                                    
044611       COMPUTE WS-KVANTAL6-MIX  = WS-KVANTAL6-MIX + IN-KVBEART            
044612       COMPUTE WS-SUARTSTD6-MIX = WS-SUARTSTD6-MIX +                      
044613                                  (IN-KVBEART * CLAG-PRARTSTD)            
044616     END-IF                                                               
044617                                                                          
044618     IF IN-KDSORT1 = 7                                                    
044619       COMPUTE WS-KVANTAL7-R34  = WS-KVANTAL7-R34 + IN-KVBEART            
044620       COMPUTE WS-SUARTSTD7-R34 = WS-SUARTSTD7-R34 +                      
044621                                  (IN-KVBEART * CLAG-PRARTSTD)            
044622     END-IF                                                               
044623     .                                                                    
044624     EJECT                                                                
044628 D-SKRIV-RAD SECTION.                                                     
044630                                                                          
044700     MOVE DAGENS-PERIOD           TO UT-TIAAPP                            
044800     MOVE SPAR-IDDC               TO UT-IDDC                              
044900     IF SEGMENT-FINNS                                                     
045000       MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                               
045100                                  TO UT-ADCITY                            
045200     ELSE                                                                 
045300       MOVE SPACE                 TO UT-ADCITY                            
045400     END-IF                                                               
045500                                                                          
045600     MOVE WS-KVANTAL1-REF         TO UT-KVANTAL-REF                       
045700     MOVE WS-SUARTSTD1-REF        TO UT-SUARTSTD-REF                      
045800     MOVE WS-KVANTAL2-INH         TO UT-KVANTAL-INH                       
045900     MOVE WS-SUARTSTD2-INH        TO UT-SUARTSTD-INH                      
046000     MOVE WS-KVANTAL3-RET         TO UT-KVANTAL-RET                       
046100     MOVE WS-SUARTSTD3-RET        TO UT-SUARTSTD-RET                      
046200     MOVE WS-KVANTAL4-QAL         TO UT-KVANTAL-QAL                       
046300     MOVE WS-SUARTSTD4-QAL        TO UT-SUARTSTD-QAL                      
046400     MOVE WS-KVANTAL5-ECO         TO UT-KVANTAL-ECO                       
046500     MOVE WS-SUARTSTD5-ECO        TO UT-SUARTSTD-ECO                      
046600     MOVE WS-KVANTAL-DC           TO UT-KVANTAL-DC                        
046700     MOVE WS-SUARTSTD-DC          TO UT-SUARTSTD-DC                       
046800                                                                          
046900     MOVE WS-KVANTAL6-MIX         TO UT-KVANTAL-MIX                       
047000     COMPUTE UT-SUARTSTD-MIX = - WS-SUARTSTD6-MIX                         
047100     MOVE WS-KVANTAL7-R34         TO UT-KVANTAL-R34                       
047200     MOVE WS-SUARTSTD7-R34        TO UT-SUARTSTD-R34                      
047300                                                                          
047400     PERFORM S11-SKRIV-UTPOST                                             
047500                                                                          
047600     IF NOT W4140SV-EOF                                                   
047700       MOVE IN-IDDC               TO SPAR-IDDC                            
047800       MOVE ZERO                  TO WS-KVANTAL1-REF                      
047900                                     WS-SUARTSTD1-REF                     
048000                                     WS-KVANTAL2-INH                      
048100                                     WS-SUARTSTD2-INH                     
048200                                     WS-KVANTAL3-RET                      
048300                                     WS-SUARTSTD3-RET                     
048400                                     WS-KVANTAL4-QAL                      
048500                                     WS-SUARTSTD4-QAL                     
048600                                     WS-KVANTAL5-ECO                      
048700                                     WS-SUARTSTD5-ECO                     
048800                                     WS-KVANTAL6-MIX                      
048900                                     WS-SUARTSTD6-MIX                     
049000                                     WS-KVANTAL7-R34                      
049100                                     WS-SUARTSTD7-R34                     
049200                                     WS-KVANTAL-DC                        
049300                                     WS-SUARTSTD-DC                       
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049800 S01-LAES-W4140SV SECTION.                                                
049900                                                                          
050000     READ W4140SV INTO IN-AREA                                            
050100     AT END                                                               
050200        MOVE HIGH-VALUE TO IN-AREA                                        
050300        SET W4140SV-EOF TO TRUE                                           
050400                                                                          
050500     NOT AT END                                                           
050600        MOVE 'W6127P'   TO POSTSUM-FDNAMN                                 
050700        MOVE 'W6127PD1' TO POSTSUM-DDNAMN2                                
050800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
050900        CALL POSTSUM USING POSTSUM-PARM                                   
051000     END-READ                                                             
051100     .                                                                    
051200     EJECT                                                                
051300 S10-SKRIV-START-DAP SECTION.                                             
051400                                                                          
051500     WRITE UT-DAP-POST FROM DAP-AREA-TYPE                                 
051600     PERFORM S20-POSTSUM-UTPOST                                           
051800                                                                          
052500     MOVE IN-KDMFUP TO DAP-KDMFUP                                         
052600                                                                          
053100     WRITE UT-DAP-POST FROM DAP-AREA-SUBTYPE                              
053200     PERFORM S20-POSTSUM-UTPOST                                           
053300     .                                                                    
053400     EJECT                                                                
053500 S11-SKRIV-UTPOST SECTION.                                                
053600                                                                          
053700     WRITE UT-DATA-POST FROM UT-AREA                                      
053800     PERFORM S20-POSTSUM-UTPOST                                           
053900     .                                                                    
054000     EJECT                                                                
054100 S20-POSTSUM-UTPOST SECTION.                                              
054200                                                                          
054300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
054400     MOVE 'W6127P'   TO POSTSUM-FDNAMN                                    
054500     MOVE 'W6127PD2' TO POSTSUM-DDNAMN2                                   
054600     CALL POSTSUM USING POSTSUM-PARM                                      
054700     .                                                                    
054800 Z-FINIT SECTION.                                                         
054900                                                                          
055000     CLOSE W4140SV                                                        
055100           W6127P                                                         
055200     SKIP2                                                                
055300     MOVE 'S' TO POSTSUM-OPKOD                                            
055400     CALL POSTSUM USING POSTSUM-PARM                                      
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-GU-WDB601 SECTION.                                                   
055800                                                                          
055900     STRING 'WDB601  (IDDC     =' W-IDDC-WDB6-X ')'                       
056000          DELIMITED BY SIZE INTO SSA1                                     
056100     MOVE '  GE'              TO GODK-STATUSKODER                         
056200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
056300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     EJECT                                                                
058510 IMS-GU-WDK611 SECTION.                                                   
058530                                                                          
058540     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
058550          DELIMITED BY SIZE INTO SSA1                                     
058560     MOVE 'WDK611 '             TO SSA2                                   
058570     MOVE '  GE'                TO GODK-STATUSKODER                       
058580     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
058590     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
058591     PERFORM IMS-STATUSKONTROLL                                           
058592     .                                                                    
058593     SKIP3                                                                
058600 IMS-GU-WDK711 SECTION.                                                   
058700                                                                          
058800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
059100          DELIMITED BY SIZE INTO SSA2                                     
059200     MOVE '  GE'                 TO GODK-STATUSKODER                      
059300     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
059400     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     SKIP3                                                                
059800 S99-ABEND SECTION.                                                       
059900                                                                          
060000     SKIP2                                                                
060100     MOVE 'S' TO POSTSUM-OPKOD                                            
060200     CALL POSTSUM USING POSTSUM-PARM                                      
060300     CALL ABEND USING RKOD-ABEND                                          
060400     .                                                                    
060500     EJECT                                                                
060600 IMS-STATUSKONTROLL SECTION.                                              
060700     SET STATUS-IX TO 1                                                   
060800     SEARCH GODK-STATUS                                                   
060900       AT END                                                             
061000         CALL FELLOG                                                      
061100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061200         CONTINUE                                                         
061300     END-SEARCH                                                           
061400     .                                                                    
