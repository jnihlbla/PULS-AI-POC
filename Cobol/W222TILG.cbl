000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W222TILG.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/10/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CALCULATION OF ASSETS IN CDC.                                    
000900*        OUTPUT FILE TO UPDATE ASSETS ON WDK611.                          
001000*                                                                         
001100*        THE PROGRAM READS     WDL2                                       
001200*                              WDD9                                       
001300*                              WDK7                                       
001400*                              WDB6                                       
001410*                              WDK6                                       
001420*                              WDK9                                       
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W222TILG'.            
003500 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
003600 77  CURRENT-IMS-SECTION         PIC X(80)   VALUE SPACE.                 
003700 77  YES                         PIC X       VALUE 'J'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900 77  W-CALC-KVAVIS-KVRAPP        PIC X       VALUE SPACE.                 
004000 77  W-KVOKS                     PIC S9(7)   VALUE +0 COMP-3.             
004010 77  W-KVRAPP-SUM                PIC S9(07)  VALUE +0 COMP-3.             
004020 77  W-TILLG-SDC                 PIC S9(07)  VALUE +0 COMP-3.             
004100 77  W-KVAVROP-TOT               PIC S9(07)  VALUE +0 COMP-3.             
004110 77  W-KVREFOVL-TOT              PIC S9(07)  VALUE +0 COMP-3.             
004200 77  W-KVRETUR-TOT               PIC S9(07)  VALUE +0 COMP-3.             
004510 77  W-KVTILLG-TOT               PIC S9(07)  VALUE +0 COMP-3.             
005400                                                                          
006000*01  -COPY WWDCKONS                                                       
006100     EJECT                                                                
006110*    --- COPYTEXT FÖR ATT KUNNA UR DISTR FÅ MOTTAGANDE IDDC               
006120*01 -COPY WWDIST35                                                        
006130     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
007400                                                                          
007500*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
007600 01  TABENTRY-PARM.                                                       
007700     03  STEGLANGD               PIC S9(9) COMP.                          
007800     03  ANTAL                   PIC S9(9) COMP.                          
007900     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
008000                                                                          
008100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008200                                                                          
008300 77  RKOD                        PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700     SKIP2                                                                
008800 01  ERROR-TEXT.                                                          
008900     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
009000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- AREAS FOR IMS-SECTIONS                                           
009300*                                                                         
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  KEYS-FOR-DLI.                                                        
009800     03  W-WDD901KY-X.                                                    
009900         05  W-IDARTNR-D9         PIC S9(9)  VALUE ZERO COMP-3.           
010000         05  W-IDDC-D9            PIC X(2)   VALUE SPACE.                 
010100                                                                          
010110     03  W-IDDC-B6-X.                                                     
010120         05  W-IDDC-B6            PIC X(2)    VALUE SPACE.                
010130                                                                          
010200     03  W-IDDC-REF-X.                                                    
010300         05  W-IDDC-REF           PIC X(2)   VALUE SPACE.                 
010400                                                                          
010500     03  W-IDARTNR-X.                                                     
010600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010700                                                                          
010800     03  W-IDPTYP-X.                                                      
010900         05  W-IDPTYP            PIC X(3)   VALUE '310'.                  
011000                                                                          
011100     03 W-KDAVROP-X.                                                      
011200         05  W-KDAVROP           PIC S9(1)   COMP-3 VALUE +2.             
011300                                                                          
011400     03 W-DAINLEV-X.                                                      
011500         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
011600                                                                          
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FOUND                       VALUE '  '.                  
012000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012200     88  SEGMENT-END                         VALUE 'GB'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900 01  SSA3                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
013600 01  DLI-IO-WDL201.                                                       
013700*    03  -COPY WDL201 -PRE L201-                                          
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
014000 01  DLI-IO-WDL211.                                                       
014100*    03  -COPY WDL211                                                     
014200                                                                          
014610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
014620 01  DLI-IO-WDL221.                                                       
014630*    03  -COPY WDL221                                                     
014640                                                                          
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL231'.                      
014800 01  DLI-IO-WDL231.                                                       
014900*    03  -COPY WDL231                                                     
015000     EJECT                                                                
015012 01  FILLER         PIC X(16)  VALUE 'DLI-IO-AREA-WDB6'.                  
015013 01   DLI-IO-AREA-B601.                                                   
015014*     03  -COPY WDB601                                                    
015015                                                                          
015016 01  FILLER         PIC X(16)   VALUE 'B601-TABELL'.                      
015017 01  B601-IX        PIC S9(4)   COMP SYNC VALUE ZERO.                     
015018 01  MAX-B601-IX    PIC S9(4)   COMP SYNC VALUE +80.                      
015019 01  B601-TABELL.                                                         
015020     03  FILLER OCCURS 80.                                                
015021*      05  -COPY WDB601  -PRE TAB-                                        
015022                                                                          
015400     EJECT                                                                
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
015600 01  DLI-IO-WDD901.                                                       
015700*    03  -COPY WDD901 -PRE WDD901-                                        
015800     EJECT                                                                
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
016000 01  DLI-IO-WDD905.                                                       
016100*    03  -COPY WDD905 -PRE WDD905-                                        
016200     EJECT                                                                
016300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
016400 01  DLI-IO-WDK701.                                                       
016500*    03  -COPY WDK701                                                     
016600     EJECT                                                                
016700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
016800 01  DLI-IO-WDK711.                                                       
016900*    03  -COPY WDK711                                                     
017000     EJECT                                                                
017010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017020 01  DLI-IO-WDK611.                                                       
017030*    03  -COPY WDK611                                                     
017040     EJECT                                                                
017050 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
017060 01  DLI-IO-WDK901.                                                       
017070*    03  -COPY WDK901                                                     
017080     EJECT                                                                
017100 LINKAGE SECTION.                                                         
017200*01  -COPY W222TILG                                                       
017300                                                                          
017400*01  -COPY W0008  -PRE WDK7-                                              
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700*01  -COPY W0008  -PRE WDL2-                                              
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000*01  -COPY W0008  -PRE WDB6-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE WDD9-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018510*01  -COPY W0008  -PRE WDK6-                                              
018520     05  FILLER                  PIC X.                                   
018530     EJECT                                                                
018540*01  -COPY W0008  -PRE WDK9-                                              
018550     05  FILLER                  PIC X.                                   
018560     EJECT                                                                
018600 PROCEDURE DIVISION  USING TILG-W222TILG                                  
018700                           WDK7-PCB WDL2-PCB WDB6-PCB WDD9-PCB            
018710                           WDK6-PCB WDK9-PCB.                             
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING TILG-W222TILG                                  
019000                           WDK7-PCB WDL2-PCB WDB6-PCB WDD9-PCB            
019010                           WDK6-PCB WDK9-PCB.                             
019100                                                                          
019200     PERFORM A-INIT                                                       
019300                                                                          
019400     PERFORM B-CALCULATION-ASSETS-WDK611                                  
019500                                                                          
019600     PERFORM Z-FINIT                                                      
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 A-INIT SECTION.                                                          
020300     MOVE 'A-INIT              ' TO CURRENT-SECTION                       
020400                                                                          
020500     ACCEPT TODAYS-DATE  FROM DATE                                        
020600     .                                                                    
020700     EJECT                                                                
020800 B-CALCULATION-ASSETS-WDK611 SECTION.                                     
020900     MOVE 'B-CALCULATION-ASSETS-WDK611' TO CURRENT-SECTION                
021000                                                                          
021100     PERFORM BA-KVRETUR-TOT                                               
021200                                                                          
021300     PERFORM BB-KVAVROP-TOT                                               
021400                                                                          
021500     PERFORM BC-KVREFOVL-TOT                                              
021600                                                                          
021700     PERFORM BD-KVTILLG-TOT                                               
021800                                                                          
021900     MOVE W-KVAVROP-TOT            TO TILG-KVAVROP-TOT                    
022000     MOVE W-KVREFOVL-TOT           TO TILG-KVREFOVL-TOT                   
022100     MOVE W-KVRETUR-TOT            TO TILG-KVRETUR-TOT                    
022300     MOVE W-KVTILLG-TOT            TO TILG-KVTILLG-TOT                    
022700     .                                                                    
022800     EJECT                                                                
022900 BA-KVRETUR-TOT    SECTION.                                               
023000     MOVE 'BA-KVRETUR-TOT      '  TO CURRENT-SECTION                      
023100                                                                          
023300     MOVE +0                         TO W-KVRAPP-SUM                      
023400                                        W-KVRETUR-TOT                     
023530                                                                          
023600     MOVE TILG-IDARTNR               TO W-IDARTNR                         
023700     PERFORM IMS-GU-WDL201                                                
023800     IF SEGMENT-FOUND                                                     
023900       PERFORM IMS-GNP-WDL211                                             
024000       PERFORM UNTIL SEGMENT-MISSING                                      
024010         MOVE INL-DAINLEV            TO W-DAINLEV                         
024120         PERFORM IMS-GNP-WDL221                                           
024200         IF SEGMENT-FOUND                                                 
024230                                                                          
024310           IF MOT-IDDC = WC-CDC-SE                                        
024320          AND MOT-KDRT = 7                                                
024510              MOVE ZERO              TO TALLY                             
024520              INSPECT MOT-IDLEVNR TALLYING TALLY                          
024530                      FOR CHARACTERS BEFORE INITIAL SPACE                 
024540              IF TALLY = ZERO                                             
024550                 MOVE ZERO           TO DIST35-IDDISTR                    
024560              ELSE                                                        
024570                 MOVE MOT-IDLEVNR(1:TALLY) TO DIST35-IDDISTR              
024580              END-IF                                                      
024590*- KVALITETSRETURER EXKLUDERAS, BB RETURER INKLUDERAS I W-TILLG           
024610              IF DIST35-NA-CDC-BB-RETURN                                  
024620              OR DIST35-CN-CDC-RETUR                                      
024621              OR DIST35-IN-CDC-RETUR                                      
024622              OR DIST35-KR-CDC-RETUR                                      
024623              OR DIST35-AE-CDC-RETUR                                      
024624              OR DIST35-TH-CDC-RETUR                                      
024625              OR DIST35-TW-CDC-RETUR                                      
024626              OR DIST35-MY-CDC-RETUR                                      
024627              OR DIST35-RU-CDC-RETUR                                      
024630                 CONTINUE                                                 
024640              ELSE                                                        
024700                 MOVE +0             TO W-KVRAPP-SUM                      
024900                 PERFORM IMS-GNP-WDL231                                   
025000                 PERFORM UNTIL SEGMENT-MISSING                            
025100                    ADD DEL-KVRAPP   TO W-KVRAPP-SUM                      
025200                    PERFORM IMS-GNP-WDL231                                
025300                 END-PERFORM                                              
025400                                                                          
025500                 COMPUTE W-KVRETUR-TOT =                                  
025600                         W-KVRETUR-TOT +                                  
025700                        (MOT-KVAVIS - W-KVRAPP-SUM)                       
025810              END-IF                                                      
025820           END-IF                                                         
025900         END-IF                                                           
026000         PERFORM IMS-GNP-WDL211                                           
026100       END-PERFORM                                                        
026110                                                                          
026200       IF W-KVRETUR-TOT < +0                                              
026300          MOVE +0                    TO W-KVRETUR-TOT                     
026400       END-IF                                                             
026410                                                                          
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 BB-KVAVROP-TOT    SECTION.                                               
026900     MOVE 'BB-KVAVROP-TOT      ' TO CURRENT-SECTION                       
027000                                                                          
027100     MOVE +0                             TO W-KVAVROP-TOT                 
027200                                                                          
027300     MOVE TILG-IDARTNR                   TO W-IDARTNR-D9                  
027400     MOVE WC-CDC-SE                      TO W-IDDC-D9                     
027500     PERFORM IMS-GU-WDD901                                                
027600     IF SEGMENT-FOUND                                                     
027700        PERFORM IMS-GNP-WDD905                                            
027800        PERFORM UNTIL SEGMENT-MISSING                                     
027900          COMPUTE W-KVAVROP-TOT = W-KVAVROP-TOT + WDD905-KVAVROP          
028000                                                                          
028100          PERFORM IMS-GNP-WDD905                                          
028200        END-PERFORM                                                       
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028510 BC-KVREFOVL-TOT   SECTION.                                               
028520     MOVE 'BC-KVREFOVL-TOT     ' TO CURRENT-SECTION                       
028530                                                                          
028540     MOVE +0                          TO W-KVREFOVL-TOT                   
028550                                                                          
028560     MOVE TILG-IDARTNR                TO W-IDARTNR                        
028570     PERFORM IMS-GU-WDK701                                                
028580     IF SEGMENT-FOUND                                                     
028591                                                                          
028592        MOVE WC-CDC-SE                TO W-IDDC-REF                       
028593        PERFORM IMS-GNP-WDK711-REF                                        
028594        PERFORM UNTIL SEGMENT-MISSING                                     
028595                                                                          
028596           MOVE +0                    TO W-TILLG-SDC                      
028602           COMPUTE W-TILLG-SDC = SLAG-KVLS                                
028603                               + SLAG-KVBEART                             
028604                               + SLAG-KVAKS-SDC                           
028605                               + SLAG-KVAKS-PAV                           
028613           COMPUTE W-KVOKS = SLAG-KVOKS-BULK + SLAG-KVOKS-DAG             
028614           IF W-KVOKS > +0                                                
028615              SUBTRACT W-KVOKS   FROM W-TILLG-SDC                         
028616           END-IF                                                         
028618                                                                          
028619           PERFORM BCA-READ-OR-TAB-B601                                   
028628                                                                          
028630*          - KOLLA OM SDC-LAGERKODENS FÖRSTA POS ÄR 'S' (SDC)             
028632           IF NOT DCS-CHINA                                               
028633             IF DCS-KDDC(1:1) = 'S'                                       
028635*              - KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS             
028636               IF DCS-FLOVRLAGBER = NOO                                   
028637                 CONTINUE                                                 
028638               ELSE                                                       
028639                 IF SLAG-KVREFOVL < W-TILLG-SDC                           
028642                   COMPUTE W-KVREFOVL-TOT = W-KVREFOVL-TOT                
028643                                          + W-TILLG-SDC                   
028644                                          - SLAG-KVREFOVL                 
028647                 END-IF                                                   
028649               END-IF                                                     
028650             END-IF                                                       
028651           END-IF                                                         
028652                                                                          
028653         PERFORM IMS-GNP-WDK711-REF                                       
028654       END-PERFORM                                                        
028655     END-IF                                                               
028657     .                                                                    
028660     EJECT                                                                
028670 BCA-READ-OR-TAB-B601 SECTION.                                            
028680                                                                          
028690     IF TAB-DCS-IDDC(1) = LOW-VALUE                                       
028700*--TAB IS EMPTY (FIRST CALL)                                              
028800       MOVE SLAG-IDDC         TO W-IDDC-B6                                
029000       PERFORM IMS-GU-WDB601                                              
029100       MOVE DCS-WDB601        TO TAB-DCS-WDB601 (1)                       
029200     ELSE                                                                 
029300       MOVE +1                TO B601-IX                                  
029400       PERFORM UNTIL B601-IX > MAX-B601-IX                                
029500         IF TAB-DCS-IDDC(B601-IX) = SLAG-IDDC                             
029600*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB601                               
029700           MOVE TAB-DCS-WDB601 (B601-IX) TO DCS-WDB601                    
029800           MOVE MAX-B601-IX   TO B601-IX                                  
029900         ELSE                                                             
030000           IF TAB-DCS-IDDC(B601-IX) = LOW-VALUE                           
030100*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
030200             MOVE SLAG-IDDC   TO W-IDDC-B6                                
030400             PERFORM IMS-GU-WDB601                                        
030500             MOVE DCS-WDB601  TO TAB-DCS-WDB601 (B601-IX)                 
030600             MOVE MAX-B601-IX TO B601-IX                                  
030700           END-IF                                                         
030800         END-IF                                                           
030900         ADD +1 TO B601-IX                                                
031000       END-PERFORM                                                        
031100       IF DCS-IDDC NOT = SLAG-IDDC                                        
031200*--NO MATCH. INDICATES THAT THE TABEL TO SMALL.                           
031300*--THERE ARE MORE THEN 80 XDC:S IN WDB601!!                               
031400         MOVE 'NO MATCH = TOO SMALL TABLE(80)' TO ERROR-TEXT-STR          
031510         DISPLAY ERROR-TEXT                                               
031600         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
031700       END-IF                                                             
031800     END-IF                                                               
031900     .                                                                    
031910     EJECT                                                                
032000                                                                          
039110 BD-KVTILLG-TOT    SECTION.                                               
039120     MOVE 'BD-KVTILLG-TOT      ' TO CURRENT-SECTION                       
039130                                                                          
039140     MOVE +0                          TO W-KVTILLG-TOT                    
039150                                                                          
039160     MOVE TILG-IDARTNR                TO W-IDARTNR                        
039170     PERFORM IMS-GU-WDK611                                                
039194     IF SEGMENT-FOUND                                                     
039195                                                                          
039248        COMPUTE W-KVTILLG-TOT = CLAG-KVLS                                 
039249                              + CLAG-KVAKS-PAV                            
039250                              + CLAG-KVAKS-CDC                            
039251                              + CLAG-KVAKS-T                              
039252                              + CLAG-KVBEART                              
039253                              - CLAG-KVRESS                               
039254                              - CLAG-KVSPARR-KVAL                         
039255                              - CLAG-KVROS                                
039256                              - W-KVRETUR-TOT                             
039257                              + W-KVAVROP-TOT                             
039258                              + W-KVREFOVL-TOT                            
039259                                                                          
039261        MOVE TILG-IDARTNR          TO W-IDARTNR                           
039262        PERFORM IMS-GU-WDK901                                             
039263        IF SEGMENT-FOUND                                                  
039264           COMPUTE W-KVOKS = ART-KVOKS-BULK                               
039265                           + ART-KVOKS-DAG                                
039266                           + ART-KVOKS-VOR                                
039270           IF W-KVOKS > +0                                                
039271              SUBTRACT W-KVOKS   FROM W-KVTILLG-TOT                       
039272           END-IF                                                         
039274        END-IF                                                            
039275     END-IF                                                               
039276     .                                                                    
039277     EJECT                                                                
039280 Z-FINIT SECTION.                                                         
039300     MOVE 'Z-FINIT             ' TO CURRENT-SECTION                       
039400     .                                                                    
039500     EJECT                                                                
039600 S99-ABEND SECTION.                                                       
039700                                                                          
039800     CALL ABEND USING RKOD-ABEND                                          
039900     .                                                                    
040000     EJECT                                                                
040100* --- IMS SECTIONS  ---                                                   
040200                                                                          
040300     EJECT                                                                
040400 IMS-GU-WDL201 SECTION.                                                   
040500     MOVE 'IMS-GU-WDL201 '    TO CURRENT-IMS-SECTION                      
040600                                                                          
040700     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
040800          DELIMITED BY SIZE INTO SSA1                                     
040900     MOVE '  GE'              TO GOOD-STATUSCODES                         
041000     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
041100     MOVE WDL2-STATUS-CODE    TO STATUS-WS                                
041200     PERFORM IMS-STATUSCHECK                                              
041300     .                                                                    
041400                                                                          
041500 IMS-GNP-WDL211 SECTION.                                                  
041600     MOVE 'IMS-GNP-WDL211'    TO CURRENT-IMS-SECTION                      
041700                                                                          
041800     MOVE 'WDL211'            TO SSA1                                     
041900     MOVE '  GE'              TO GOOD-STATUSCODES                         
042000     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
042100     MOVE WDL2-STATUS-CODE    TO STATUS-WS                                
042200     PERFORM IMS-STATUSCHECK                                              
042300     .                                                                    
042400                                                                          
042500 IMS-GNP-WDL221 SECTION.                                                  
042600     MOVE 'IMS-GNP-WDL221'       TO CURRENT-IMS-SECTION                   
042700                                                                          
042710     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
042720             DELIMITED BY SIZE INTO SSA1                                  
042800     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
042900             DELIMITED BY SIZE INTO SSA2                                  
043000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
043100     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
043200     MOVE WDL2-STATUS-CODE       TO STATUS-WS                             
043300     PERFORM IMS-STATUSCHECK                                              
043400     .                                                                    
043500                                                                          
043600 IMS-GNP-WDL231 SECTION.                                                  
043700     MOVE 'IMS-GNP-WDL231'       TO CURRENT-IMS-SECTION                   
043800                                                                          
043900     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
044000             DELIMITED BY SIZE INTO SSA1                                  
044100     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
044200             DELIMITED BY SIZE INTO SSA2                                  
044300     MOVE 'WDL231   '            TO SSA3                                  
044400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
044500     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL231 SSA1 SSA2 SSA3         
044600     MOVE WDL2-STATUS-CODE       TO STATUS-WS                             
044700     PERFORM IMS-STATUSCHECK                                              
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 IMS-GU-WDD901 SECTION.                                                   
045200     MOVE 'IMS-GU-WDD901 '   TO CURRENT-IMS-SECTION                       
045300                                                                          
045400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
045500          DELIMITED BY SIZE INTO SSA1                                     
045600     MOVE '  GE'              TO GOOD-STATUSCODES                         
045700     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
045800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
045900     PERFORM IMS-STATUSCHECK                                              
046000     .                                                                    
046100                                                                          
046200 IMS-GNP-WDD905 SECTION.                                                  
046300     MOVE 'IMS-GNP-WDD905 '   TO CURRENT-IMS-SECTION                      
046400                                                                          
046500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
046600          DELIMITED BY SIZE   INTO SSA1                                   
046700     MOVE '  GE'                TO GOOD-STATUSCODES                       
046800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
046900     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
047000     PERFORM IMS-STATUSCHECK                                              
047100     .                                                                    
047200                                                                          
047300 IMS-GU-WDK701 SECTION.                                                   
047400     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
047500                                                                          
047600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
047700          DELIMITED BY SIZE INTO SSA1                                     
047800     MOVE '  GE'              TO GOOD-STATUSCODES                         
047900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
048000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
048100     PERFORM IMS-STATUSCHECK                                              
048200     .                                                                    
048300                                                                          
048400 IMS-GNP-WDK711-REF SECTION.                                              
048500     MOVE 'IMS-GNP-WDK701-REF' TO CURRENT-IMS-SECTION                     
048600                                                                          
048700     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
048800          DELIMITED BY SIZE INTO SSA1                                     
048900     MOVE '  GE'              TO GOOD-STATUSCODES                         
049000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
049100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
049200     PERFORM IMS-STATUSCHECK                                              
049300     .                                                                    
049400                                                                          
049410 IMS-GU-WDB601    SECTION.                                                
049411     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
049412                                                                          
049430     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
049440          DELIMITED BY SIZE INTO SSA1                                     
049450     MOVE '  '                TO GOOD-STATUSCODES                         
049460     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
049470     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
049471     PERFORM IMS-STATUSCHECK                                              
049490     .                                                                    
049491                                                                          
050401 IMS-GU-WDK611     SECTION.                                               
050402                                                                          
050403     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
050404       DELIMITED BY SIZE INTO SSA1                                        
050407     MOVE 'WDK611   '      TO SSA2                                        
050408     MOVE '  GE'           TO GOOD-STATUSCODES                            
050410     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
050411     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050412     PERFORM IMS-STATUSCHECK                                              
050413     .                                                                    
050414     SKIP3                                                                
050415 IMS-GU-WDK901 SECTION.                                                   
050420                                                                          
050430     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
050440       DELIMITED BY SIZE INTO SSA1                                        
050450     MOVE '  GE'           TO GOOD-STATUSCODES                            
050460     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
050470     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
050471     PERFORM IMS-STATUSCHECK                                              
050490     .                                                                    
050491     EJECT                                                                
050500 IMS-STATUSCHECK SECTION.                                                 
050600                                                                          
050700     SET STATUS-IX TO 1                                                   
050800     SEARCH GOOD-STATUS                                                   
050900       AT END                                                             
051000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051100           DELIMITED BY SIZE INTO ERROR-TEXT                              
051200         DISPLAY ERROR-TEXT                                               
051300         CALL FELLOG                                                      
051400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
051500         CONTINUE                                                         
051600     END-SEARCH                                                           
051700     .                                                                    
