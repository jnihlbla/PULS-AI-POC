000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222700.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   99/12/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRADE SÄSONGSINDEX PÅ WDK6                       
001400*        SAMT HÄNDELSEREGISTER                                            
001410*                                                                         
001500*        PROGRAMMET UPPDATERAR WDK6                                       
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
002800     SKIP2                                                                
002900*          --- SASONGSINDEX                                               
003000     SELECT W22227IN                   ASSIGN TO W22227D1.                
003010*                            *** UTFIL                         ***        
003020     SELECT W22227UT                   ASSIGN TO W22227D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
004070                                                                          
004080 FD  W22227IN                                                             
004090     RECORDING       F                                                    
004091     BLOCK CONTAINS  0.                                                   
004092                                                                          
004093*01  POST -COPY W22226  -PRE IN-    -L.                                   
004094                                                                          
004095 FD  W22227UT                                                             
004096     RECORDING F                                                          
004097     BLOCK 0                                                              
004098     LABEL RECORD STANDARD.                                               
004099                                                                          
004100 01  W22227UT-POST.                                                       
004101*    03  -COPY W2212204   -L.                                             
004110     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2222700'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004610 77  WS-NOLL                     PIC S9(9)   VALUE ZERO COMP-3.           
004620 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004621 77  WS-ANTAL-WDK711             PIC S9(9)   VALUE ZERO COMP-3.           
004622 77  WS-ANTAL-TOT                PIC S9(9)   VALUE ZERO COMP-3.           
004623 77  WS-ANTAL-0                  PIC S9(9)   VALUE ZERO COMP-3.           
004624 77  WS-ANTAL-0-5                PIC S9(9)   VALUE ZERO COMP-3.           
004625 77  WS-ANTAL-5-10               PIC S9(9)   VALUE ZERO COMP-3.           
004626 77  WS-ANTAL-10-15              PIC S9(9)   VALUE ZERO COMP-3.           
004627 77  WS-ANTAL-15-100             PIC S9(9)   VALUE ZERO COMP-3.           
004700     SKIP2                                                                
004701 01  CHKP-VAR.                                                            
004702 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004703 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004704 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004705 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004706 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004710 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
004720                                                                          
004802                                                                          
004803 01  RESULTAT-RAKNARE.                                                    
004804     03  WS-ANTAL-REPL           PIC S9(9)  VALUE ZERO COMP-3.            
004805     03  WS-ANTAL-ISRT           PIC S9(9)  VALUE ZERO COMP-3.            
004816                                                                          
004817                                                                          
004820 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W22227-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W22227                       VALUE 'J'.                   
005301                                                                          
005400     EJECT                                                                
005500                                                                          
005600 01      WS-CURRENT-DATE.                                                 
005700         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
005800         05  FILLER              PIC 9(4)   VALUE ZERO.                   
005900         05  FILLER              PIC 9(6)   VALUE ZERO.                   
005910                                                                          
005920 01      FILLER REDEFINES WS-CURRENT-DATE.                                
005930*-----   INKLUSIVE SEKEL                                                  
005940         05  WS-DAGENS-DATUM     PIC 9(8).                                
005950         05  WS-DAGENS-TID.                                               
005960             07 WS-DAGENS-TIMME  PIC 9(2).                                
005970             07 WS-DAGENS-MINUT  PIC 9(2).                                
005980             07 WS-DAGENS-SEKUND PIC 9(2).                                
006000     EJECT                                                                
006010*      --- VALID IDDC CODES                                               
006020*                                                                         
006030*01    -COPY WWDC99                                                       
006040       EJECT                                                              
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                             'IN-AREA-START'.             
007300     SKIP2                                                                
007400                                                                          
007500*01  AREA -COPY W22226     -PRE IN-                                       
007600*                                                                         
007610     EJECT                                                                
007651 01  UT-AREA-START               PIC X(24)   VALUE                        
007652                                             'UT-AREA-START'.             
007660*01  AREA  -COPY W2212204    -PRE 2204-.                                  
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-IDARTNR-X.                                                     
008200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     03  W-IDDC-X.                                                        
008400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009810 01  SSA3                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010310     EJECT                                                                
010320 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
010330     SKIP3                                                                
010340 01  DLI-IO-WDK611.                                                       
010350*    03  -COPY WDK611                                                     
010360     EJECT                                                                
010370 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK626'.             
010380     SKIP3                                                                
010390 01  DLI-IO-WDK626.                                                       
010391*    03  -COPY WDK626                                                     
011491     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE WDK6-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012300     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W22227                                              
012710     PERFORM UNTIL END-OF-W22227                                          
012770                                                                          
012793       PERFORM B-BEHANDLA-POSTER                                          
012796       PERFORM S01-LAES-W22227                                            
012797                                                                          
012798     END-PERFORM                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014001     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
014010                                                                          
014100     OPEN INPUT  W22227IN                                                 
014300     OPEN OUTPUT W22227UT                                                 
014310                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014410     PERFORM IMS-RESTART                                                  
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014900     MOVE IN-IDARTNR TO W-IDARTNR                                         
014910                                                                          
014920     PERFORM IMS-GHU-K626                                                 
015128                                                                          
015130     IF  SEGMENT-FINNS                                                    
015131     AND JUST-DASPSEA < WS-DAGENS-DATUM                                   
015136        IF  JUST-RESEASON (1) = IN-RESEASON (1)                           
015137        AND JUST-RESEASON (2) = IN-RESEASON (2)                           
015138        AND JUST-RESEASON (3) = IN-RESEASON (3)                           
015139        AND JUST-RESEASON (4) = IN-RESEASON (4)                           
015140        AND JUST-RESEASON (5) = IN-RESEASON (5)                           
015141        AND JUST-RESEASON (6) = IN-RESEASON (6)                           
015142        AND JUST-RESEASON (7) = IN-RESEASON (7)                           
015143        AND JUST-RESEASON (8) = IN-RESEASON (8)                           
015144        AND JUST-RESEASON (9) = IN-RESEASON (9)                           
015145        AND JUST-RESEASON (10) = IN-RESEASON (10)                         
015146        AND JUST-RESEASON (11) = IN-RESEASON (11)                         
015147        AND JUST-RESEASON (12) = IN-RESEASON (12)                         
015149           IF JUST-DAMANSEA > ZERO                                        
015150           OR JUST-DASPSEA > ZERO                                         
015151              MOVE ZERO      TO JUST-DAMANSEA                             
015152                                JUST-DASPSEA                              
015153              PERFORM IMS-REPL-K626                                       
015154              ADD +1 TO CHKP-ANT                                          
015155              IF CHKP-ANT > CHKP-MAX                                      
015156                PERFORM IMS-CHECKPOINT                                    
015157                MOVE +0 TO CHKP-ANT                                       
015158              END-IF                                                      
015159              ADD +1         TO WS-ANTAL-REPL                             
015160           END-IF                                                         
015161        ELSE                                                              
015162           IF     (JUST-RESEASON (1) = 1.00                               
015163           AND     JUST-RESEASON (2) = 1.00                               
015164           AND     JUST-RESEASON (3) = 1.00                               
015165           AND     JUST-RESEASON (4) = 1.00                               
015166           AND     JUST-RESEASON (5) = 1.00                               
015167           AND     JUST-RESEASON (6) = 1.00                               
015168           AND     JUST-RESEASON (7) = 1.00                               
015169           AND     JUST-RESEASON (8) = 1.00                               
015170           AND     JUST-RESEASON (9) = 1.00                               
015171           AND     JUST-RESEASON (10) = 1.00                              
015172           AND     JUST-RESEASON (11) = 1.00                              
015173           AND     JUST-RESEASON (12) = 1.00)                             
015174           AND NOT (IN-RESEASON (1) = 1.00                                
015175           AND      IN-RESEASON (2) = 1.00                                
015176           AND      IN-RESEASON (3) = 1.00                                
015177           AND      IN-RESEASON (4) = 1.00                                
015178           AND      IN-RESEASON (5) = 1.00                                
015179           AND      IN-RESEASON (6) = 1.00                                
015180           AND      IN-RESEASON (7) = 1.00                                
015181           AND      IN-RESEASON (8) = 1.00                                
015182           AND      IN-RESEASON (9) = 1.00                                
015183           AND      IN-RESEASON (10) = 1.00                               
015184           AND      IN-RESEASON (11) = 1.00                               
015185           AND      IN-RESEASON (12) = 1.00)                              
015186*                                                                         
015187*     ARTIKELN ÄR PÅ VÄG ATT BLI SÄSONG                                   
015188*     AUTOMATPLANER SKA STOPPAS                                           
015189*                                                                         
015190             PERFORM S02-UPPDAT-HAENDELSEREG                              
015191           END-IF                                                         
015192           MOVE IN-RESEASON (1)                                           
015193                             TO JUST-RESEASON (1)                         
015194           MOVE IN-RESEASON (2)                                           
015195                             TO JUST-RESEASON (2)                         
015196           MOVE IN-RESEASON (3)                                           
015197                             TO JUST-RESEASON (3)                         
015198           MOVE IN-RESEASON (4)                                           
015199                             TO JUST-RESEASON (4)                         
015200           MOVE IN-RESEASON (5)                                           
015201                             TO JUST-RESEASON (5)                         
015202           MOVE IN-RESEASON (6)                                           
015203                             TO JUST-RESEASON (6)                         
015204           MOVE IN-RESEASON (7)                                           
015205                             TO JUST-RESEASON (7)                         
015206           MOVE IN-RESEASON (8)                                           
015207                             TO JUST-RESEASON (8)                         
015208           MOVE IN-RESEASON (9)                                           
015209                             TO JUST-RESEASON (9)                         
015210           MOVE IN-RESEASON (10)                                          
015211                             TO JUST-RESEASON (10)                        
015212           MOVE IN-RESEASON (11)                                          
015213                             TO JUST-RESEASON (11)                        
015214           MOVE IN-RESEASON (12)                                          
015215                             TO JUST-RESEASON (12)                        
015216           MOVE ZERO         TO JUST-DAMANSEA                             
015217                                JUST-DASPSEA                              
015218           PERFORM IMS-REPL-K626                                          
015219           ADD +1 TO CHKP-ANT                                             
015220           IF CHKP-ANT > CHKP-MAX                                         
015221             PERFORM IMS-CHECKPOINT                                       
015222             MOVE +0 TO CHKP-ANT                                          
015223           END-IF                                                         
015224           ADD +1            TO WS-ANTAL-REPL                             
015225        END-IF                                                            
015226     ELSE                                                                 
015227       IF SEGMENT-SAKNAS                                                  
015228       AND NOT (IN-RESEASON (1) = 1.00                                    
015229       AND     IN-RESEASON (2) = 1.00                                     
015230       AND     IN-RESEASON (3) = 1.00                                     
015231       AND     IN-RESEASON (4) = 1.00                                     
015232       AND     IN-RESEASON (5) = 1.00                                     
015233       AND     IN-RESEASON (6) = 1.00                                     
015234       AND     IN-RESEASON (7) = 1.00                                     
015235       AND     IN-RESEASON (8) = 1.00                                     
015236       AND     IN-RESEASON (9) = 1.00                                     
015237       AND     IN-RESEASON (10) = 1.00                                    
015238       AND     IN-RESEASON (11) = 1.00                                    
015239       AND     IN-RESEASON (12) = 1.00)                                   
015240          MOVE ZERO          TO JUST-REPBJUST                             
015241                                JUST-TIPBJUST-CENTR                       
015242                                JUST-DAMANSEA                             
015243                                JUST-DASPSEA                              
015244                                JUST-KVPB-JUST (1)                        
015245                                JUST-TIPBJUST (1)                         
015246                                JUST-KVPB-JUST (2)                        
015247                                JUST-TIPBJUST (2)                         
015248          MOVE IN-RESEASON (1)                                            
015249                             TO JUST-RESEASON (1)                         
015250          MOVE IN-RESEASON (2)                                            
015251                             TO JUST-RESEASON (2)                         
015252          MOVE IN-RESEASON (3)                                            
015253                             TO JUST-RESEASON (3)                         
015254          MOVE IN-RESEASON (4)                                            
015255                             TO JUST-RESEASON (4)                         
015256          MOVE IN-RESEASON (5)                                            
015257                             TO JUST-RESEASON (5)                         
015258          MOVE IN-RESEASON (6)                                            
015259                             TO JUST-RESEASON (6)                         
015260          MOVE IN-RESEASON (7)                                            
015261                             TO JUST-RESEASON (7)                         
015262          MOVE IN-RESEASON (8)                                            
015263                             TO JUST-RESEASON (8)                         
015264          MOVE IN-RESEASON (9)                                            
015265                             TO JUST-RESEASON (9)                         
015266          MOVE IN-RESEASON (10)                                           
015267                             TO JUST-RESEASON (10)                        
015268          MOVE IN-RESEASON (11)                                           
015269                             TO JUST-RESEASON (11)                        
015270          MOVE IN-RESEASON (12)                                           
015271                             TO JUST-RESEASON (12)                        
015272          PERFORM IMS-ISRT-K626                                           
015273          ADD +1 TO CHKP-ANT                                              
015274          IF CHKP-ANT > CHKP-MAX                                          
015275            PERFORM IMS-CHECKPOINT                                        
015276            MOVE +0 TO CHKP-ANT                                           
015277          END-IF                                                          
015278          ADD +1             TO WS-ANTAL-ISRT                             
015279*                                                                         
015280*     ARTIKELN ÄR PÅ VÄG ATT BLI SÄSONG                                   
015281*     AUTOMATPLANER SKA STOPPAS                                           
015282*                                                                         
015283          PERFORM S02-UPPDAT-HAENDELSEREG                                 
015284       END-IF                                                             
015290     END-IF                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W22227IN                                                       
015810           W22227UT                                                       
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016101                                                                          
016110     DISPLAY 'ANTAL SASONGSANDR '                                         
016120     DISPLAY 'ANTAL REPL : ' WS-ANTAL-REPL                                
016130     DISPLAY 'ANTAL ISRT : ' WS-ANTAL-ISRT                                
016170                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W22227  SECTION.                                                
016500     SKIP2                                                                
016600     READ W22227IN INTO IN-AREA                                           
016700     AT END                                                               
016900        SET END-OF-W22227 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W22227' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W22227D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017810 S02-UPPDAT-HAENDELSEREG SECTION.                                         
017820******************************************************************        
017830*                                                                *        
017841*    2204-TRANS SKAPAS FÖR ARTIKEL SOM BLIR SÄSONG               *        
017842*    LEVERANSPLANEKONCEPTET KOMMER ATT STOPPAS                   *        
017850*                                                                *        
017860******************************************************************        
017870                                                                          
017880     MOVE W-IDARTNR        TO 2204-IDARTNR                                
017890     MOVE 14               TO 2204-KDLPORS                                
017891     MOVE '2204'           TO 2204-IDHTYP                                 
017892     PERFORM S03-SKRIV-W22227UT                                           
017893     .                                                                    
017894     EJECT                                                                
017895 S03-SKRIV-W22227UT SECTION.                                              
017896                                                                          
017897*****************************************************************         
017898*                                                               *         
017899*****************************************************************         
017900                                                                          
017901     WRITE W22227UT-POST         FROM  2204-AREA                          
017902     MOVE 'W22227'             TO POSTSUM-FDNAMN                          
017903     MOVE 'W22227D2'           TO POSTSUM-DDNAMN2                         
017904     MOVE '2204'               TO POSTSUM-TRANSTYP                        
017905     CALL POSTSUM USING POSTSUM-PARM                                      
017906     .                                                                    
017907     EJECT                                                                
017908                                                                          
017910* --- IMS SEKTIONER ---                                                   
018000     SKIP3                                                                
018100     EJECT                                                                
021008 IMS-GHU-K626 SECTION.                                                    
021009     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021010          DELIMITED BY SIZE INTO SSA1                                     
021011     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
021012     MOVE 'WDK626   ' TO SSA3                                             
021013     MOVE '  GE' TO GODK-STATUSKODER                                      
021014     CALL CBLTDLI USING GHU                                               
021015                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
021016     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021017     PERFORM IMS-STATUSKONTROLL                                           
021018     .                                                                    
021019     EJECT                                                                
021020 IMS-ISRT-K626 SECTION.                                                   
021021                                                                          
021022     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021023          DELIMITED BY SIZE INTO SSA1                                     
021024     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
021025     MOVE 'WDK626   ' TO SSA3                                             
021026     MOVE '  ' TO GODK-STATUSKODER                                        
021027     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK626                       
021028                             SSA1 SSA2 SSA3                               
021029     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021030     PERFORM IMS-STATUSKONTROLL                                           
021031     .                                                                    
021032     EJECT                                                                
021033 IMS-REPL-K626 SECTION.                                                   
021034                                                                          
021035     MOVE '  ' TO GODK-STATUSKODER                                        
021036     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
021037     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021038     PERFORM IMS-STATUSKONTROLL                                           
021039     .                                                                    
021041     EJECT                                                                
021042 IMS-RESTART SECTION.                                                     
021043     SKIP2                                                                
021044     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021045     MOVE '  ' TO GODK-STATUSKODER                                        
021046     CALL CBLTDLI USING XRST MSG-PCB                                      
021047                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021048                        CHKP-AREA-LENGTH CHKP-AREA                        
021049     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021050     PERFORM IMS-STATUSKONTROLL                                           
021051     .                                                                    
021052     EJECT                                                                
021053 IMS-CHECKPOINT SECTION.                                                  
021054     SKIP2                                                                
021055     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021056     MOVE '  XD' TO GODK-STATUSKODER                                      
021057     CALL CBLTDLI USING CHKP MSG-PCB                                      
021058                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021059                        CHKP-AREA-LENGTH CHKP-AREA                        
021060     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021061     PERFORM IMS-STATUSKONTROLL                                           
021062                                                                          
021063     IF IMS-EJ-OK                                                         
021064       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021065       DISPLAY FELTEXT                                                    
021066       CALL FELLOG                                                        
021067     END-IF                                                               
021068     .                                                                    
021069     EJECT                                                                
021070 IMS-STATUSKONTROLL SECTION.                                              
021100     SKIP2                                                                
021200     SET STATUS-IX TO 1                                                   
021300     SEARCH GODK-STATUS                                                   
021400       AT END                                                             
021500         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
021600         DISPLAY FELTEXT                                                  
021700         CALL FELLOG                                                      
021800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021900         CONTINUE                                                         
022000     END-SEARCH                                                           
022100     .                                                                    
