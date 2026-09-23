000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1146200.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   05/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR IDINK     FRÅN SI+  KOD=4,0,2,3                       
001000*                   TIMOTSI             KOD=0,2,3,4,8,20                  
001100*                   KDANSKQ             KOD=3,8,20                        
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK6                                       
001400*                              WDD2                                       
001410*                                                                         
001411**----------------------------------------------------------------        
001420*    ÄNDRINGAR:                                                           
001430*    2014-12-05  SCR 10247191  RÄTTA SCR 10179830, DÄR RETURKOD           
001440*                              "04" TOGS BORT FELAKTIGT.                  
001450*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- RETUR FRÅN SI+   FILERNA 6A,6B,6C,6D,6G                    
002400     SELECT W1146C                     ASSIGN TO W11462D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W1146C                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY T335R309      -L.                                              
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W1146200'.            
003810 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003820 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 77  W1146C-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W1146C                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100                                                                          
006200 01  ARB-AREOR.                                                           
006300                                                                          
006400     03  WS-IDPITEM              PIC X(20).                               
006500     03  FILLER  REDEFINES WS-IDPITEM.                                    
006600         05  FILLER              PIC X(11).                               
006700         05  WS-IDARTNR          PIC X(9).                                
006800     03  FILLER  REDEFINES WS-IDPITEM.                                    
006900         05  FILLER              PIC X(11).                               
007000         05  WS-IDARTNR-NUM      PIC 9(9).                                
007100                                                                          
007200     03  WS-IDHANDLR             PIC 9(4)    VALUE ZERO.                  
007300     03  WS-IDINK                PIC X(4)    VALUE SPACE.                 
007400     EJECT                                                                
007410*01    -COPY WWDCKONS                                                     
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000                                                                          
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  IN-AREA-START               PIC X(24)   VALUE                        
008700                                             'IN-AREA-START'.             
008800     SKIP2                                                                
008900                                                                          
009000*01  AREA -COPY T335R309     -PRE IN-                                     
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800     03  W-KDSEGKEY-X.                                                    
009900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009910     03  W-WDGX2263-X.                                                    
009920         05  W-IDHTYP-2263       PIC X(4)    VALUE '2263'.                
009930         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
009940     03  W-TISOP-X.                                                       
009950         05  W-TISOP-2264        PIC S9(5)           COMP-3.              
010000     SKIP2                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010700     88  IMS-EJ-OK                           VALUE 'XD'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011310 01  SSA3                        PIC X(128).                              
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900                                                                          
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012100 01  DLI-IO-WDK601.                                                       
012200*    03  -COPY WDK601 -PRE 601-                                           
012300     EJECT                                                                
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012500 01  DLI-IO-WDK611.                                                       
012600*    03  -COPY WDK611                                                     
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
012900 01  DLI-IO-WDD201.                                                       
013000*    03  -COPY WDD201                                                     
013001     EJECT                                                                
013090 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2264'.                    
013091 01  DLI-IO-WDGX2264.                                                     
013092*    03  -COPY WDGX2264                                                   
013094                                                                          
013095 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2266'.                    
013096 01  DLI-IO-WDGX2266.                                                     
013097*    03  -COPY WDGX2266                                                   
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013600                                                                          
013700*01  -COPY W0008  -PRE WDK6-                                              
013800     05  FILLER                  PIC X.                                   
013900                                                                          
014000*01  -COPY W0008  -PRE WDD2-                                              
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014210*01  -COPY W0008  -PRE WDR2-                                              
014220     05  FILLER                  PIC X.                                   
014230     EJECT                                                                
014300 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDD2-PCB WDR2-PCB.            
014400 MAIN SECTION.                                                            
014500     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDD2-PCB WDR2-PCB.            
014600                                                                          
014700     SKIP2                                                                
014800     PERFORM A-INIT                                                       
014900     PERFORM S01-LAES-W1146C                                              
015000     PERFORM UNTIL END-OF-W1146C                                          
015100       IF CHKP-ANT > CHKP-MAX                                             
015200         PERFORM X-TAG-CHECKPOINT                                         
015300       END-IF                                                             
015400                                                                          
015500*      FIXA IDARTNR                                                       
015600       MOVE IN-IDPITEM TO WS-IDPITEM                                      
015700       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
015800       MOVE WS-IDARTNR-NUM TO W-IDARTNR                                   
016000                                                                          
016100       IF IN-RETURN-CODE = '  ' OR '00' OR '02' OR '03' OR '04'           
016200**        FIXA IDINK / KONTROLLERA                                        
016300          IF IN-RETURN-IDHANDLR > SPACE                                   
016400             MOVE IN-RETURN-IDHANDLR     TO WS-IDHANDLR                   
016500          ELSE                                                            
016600             MOVE IN-IDHANDLR            TO WS-IDHANDLR                   
016700          END-IF                                                          
016800          IF WS-IDHANDLR (2:1) > ZERO                                     
016900             MOVE WS-IDHANDLR (2:3)      TO WS-IDINK                      
017000          ELSE                                                            
017100             IF WS-IDHANDLR (3:1) > ZERO                                  
017200                MOVE WS-IDHANDLR (3:2)   TO WS-IDINK                      
017300             ELSE                                                         
017400                IF WS-IDHANDLR (4:1) > ZERO                               
017500                   MOVE WS-IDHANDLR (4:1) TO WS-IDINK                     
017600                ELSE                                                      
017700                   MOVE SPACE            TO WS-IDINK                      
017800                END-IF                                                    
017900             END-IF                                                       
018000          END-IF                                                          
018100                                                                          
018200          PERFORM IMS-GET-WDK611                                          
018300          IF SEGMENT-FINNS AND CLAG-IDINK NOT = WS-IDINK                  
018400                           AND WS-IDINK > SPACE                           
018500             MOVE WS-IDINK    TO CLAG-IDINK                               
018600             PERFORM IMS-REPL-WDK611                                      
018700          END-IF                                                          
018800                                                                          
018900          PERFORM IMS-GET-WDD201                                          
019000          IF SEGMENT-FINNS AND ART-IDINK NOT = WS-IDINK                   
019100                           AND WS-IDINK > SPACE                           
019200             MOVE WS-IDINK    TO ART-IDINK                                
019300             PERFORM IMS-REPL-WDD201                                      
019400          END-IF                                                          
019500       END-IF                                                             
019600                                                                          
019710       IF IN-RETURN-CODE = '  ' OR '00' OR '02' OR '03'                   
019720                        OR '04' OR '08' OR '20'                           
019800          PERFORM IMS-GET-WDD201                                          
019900          IF SEGMENT-FINNS                                                
020000             MOVE DAGENS-DATUM  TO ART-TIMOTSI                            
020110             IF (IN-RETURN-CODE = '03' OR '08' OR '20') AND               
020200                ART-KDANSKQ = '2'                                         
020300                MOVE '1' TO ART-KDANSKQ                                   
020400             END-IF                                                       
020500             PERFORM IMS-REPL-WDD201                                      
020510                                                                          
020520             PERFORM H-ISRT-WDGX2264-2266                                 
020600          END-IF                                                          
020700       END-IF                                                             
020800                                                                          
020900       PERFORM S01-LAES-W1146C                                            
021000     END-PERFORM                                                          
021100                                                                          
021200                                                                          
021300     PERFORM Z-FINIT                                                      
021400                                                                          
021500     MOVE ZERO TO RETURN-CODE                                             
021600     GOBACK                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 A-INIT SECTION.                                                          
022000     SKIP2                                                                
022100                                                                          
022200     PERFORM IMS-RESTART                                                  
022300                                                                          
022400     OPEN INPUT W1146C                                                    
022500                                                                          
022600     ACCEPT DAGENS-DATUM  FROM DATE                                       
022700                                                                          
022800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022900     .                                                                    
023000     EJECT                                                                
023010 H-ISRT-WDGX2264-2266 SECTION.                                            
023030                                                                          
023040     PERFORM IMS-GU-WDK601                                                
023050     IF SEGMENT-FINNS                                                     
023060        MOVE 601-ART-TISOP      TO 2264-TISOP                             
023070                                   W-TISOP-2264                           
023080        PERFORM IMS-ISRT-WDGX2264                                         
023090                                                                          
023091        MOVE ART-IDARTNR    TO 2266-IDARTNR                               
023092        MOVE WC-CDC-SE      TO 2266-IDDC                                  
023093        MOVE ART-TIMOTSI    TO 2266-TIMOTSI                               
023094        PERFORM IMS-ISRT-WDGX2266                                         
023095     END-IF                                                               
023096     .                                                                    
023097                                                                          
023100 Z-FINIT SECTION.                                                         
023200                                                                          
023300                                                                          
023400     CLOSE W1146C                                                         
023500     SKIP2                                                                
023600     MOVE 'S' TO POSTSUM-OPKOD                                            
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023800     .                                                                    
023900     EJECT                                                                
024000 S01-LAES-W1146C  SECTION.                                                
024100     SKIP2                                                                
024200     READ W1146C INTO IN-AREA                                             
024300     AT END                                                               
024400        SET END-OF-W1146C TO TRUE                                         
024500                                                                          
024600     NOT AT END                                                           
024700        MOVE 'W1146C'   TO POSTSUM-FDNAMN                                 
024800        MOVE 'W11462D1' TO POSTSUM-DDNAMN2                                
024900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
025000        CALL POSTSUM USING POSTSUM-PARM                                   
025100                                                                          
025200     END-READ                                                             
025300     .                                                                    
025400     EJECT                                                                
025500 X-TAG-CHECKPOINT   SECTION.                                              
025600                                                                          
025700     PERFORM IMS-CHECKPOINT                                               
025800     MOVE ZERO TO CHKP-ANT                                                
025900     .                                                                    
026000     EJECT                                                                
026100* --- IMS SEKTIONER ---                                                   
026200                                                                          
026300 IMS-GU-WDK601 SECTION.                                                   
026310     MOVE 'IMS-GU-WDK601       ' TO CURRENT-IMS-SECTION                   
026400                                                                          
026500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026600          DELIMITED BY SIZE INTO SSA1                                     
026700     MOVE '  GE' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
026900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     EJECT                                                                
027300 IMS-GET-WDK611 SECTION.                                                  
027310     MOVE 'IMS-GET-WDK611      ' TO CURRENT-IMS-SECTION                   
027400                                                                          
027500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
027600          DELIMITED BY SIZE INTO SSA1                                     
027700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
027800          DELIMITED BY SIZE INTO SSA2                                     
027900     MOVE '  GE' TO GODK-STATUSKODER                                      
028000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
028100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028200     PERFORM IMS-STATUSKONTROLL                                           
028300     .                                                                    
028400     SKIP3                                                                
028500 IMS-REPL-WDK611 SECTION.                                                 
028510     MOVE 'IMS-REPL-WDK611     ' TO CURRENT-IMS-SECTION                   
028600                                                                          
028700     MOVE '  ' TO GODK-STATUSKODER                                        
028800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
028900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     ADD +1 TO CHKP-ANT                                                   
029200     .                                                                    
029300     EJECT                                                                
029400 IMS-GET-WDD201 SECTION.                                                  
029410     MOVE 'IMS-GET-WDD201      ' TO CURRENT-IMS-SECTION                   
029500                                                                          
029600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
029700          DELIMITED BY SIZE INTO SSA1                                     
029800     MOVE '  GE' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
030000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-REPL-WDD201 SECTION.                                                 
030410     MOVE 'IMS-REPL-WDD201     ' TO CURRENT-IMS-SECTION                   
030500                                                                          
030600     MOVE '  ' TO GODK-STATUSKODER                                        
030700     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
030800     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
030900     PERFORM IMS-STATUSKONTROLL                                           
031000     ADD +1 TO CHKP-ANT                                                   
031100     .                                                                    
031200     EJECT                                                                
031210 IMS-ISRT-WDGX2264 SECTION.                                               
031220     MOVE 'IMS-ISRT-WDGX2264  '  TO CURRENT-IMS-SECTION                   
031230                                                                          
031250     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
031260          DELIMITED BY SIZE INTO SSA1                                     
031270     MOVE   'WDGX2264'        TO SSA2                                     
031280     MOVE '  II'              TO GODK-STATUSKODER                         
031290     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX2264 SSA1 SSA2           
031291     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
031292     PERFORM IMS-STATUSKONTROLL                                           
031293     .                                                                    
031294                                                                          
031295 IMS-ISRT-WDGX2266 SECTION.                                               
031296     MOVE 'IMS-ISRT-WDGX2266  '  TO CURRENT-IMS-SECTION                   
031297                                                                          
031299     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
031300          DELIMITED BY SIZE INTO SSA1                                     
031301     STRING 'WDGX2264(TISOP    =' W-TISOP-X ')'                           
031302          DELIMITED BY SIZE INTO SSA2                                     
031303     MOVE   'WDGX2266'        TO SSA3                                     
031304     MOVE '  II'              TO GODK-STATUSKODER                         
031305     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX2266 SSA1                
031306                                                        SSA2              
031307                                                        SSA3              
031308     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
031309     PERFORM IMS-STATUSKONTROLL                                           
031310     .                                                                    
031311                                                                          
031330 IMS-RESTART SECTION.                                                     
031400     SKIP2                                                                
031500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031600     MOVE '  ' TO GODK-STATUSKODER                                        
031700     CALL CBLTDLI USING XRST MSG-PCB                                      
031800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031900                        CHKP-AREA-LENGTH CHKP-AREA                        
032000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032300     SKIP3                                                                
032400 IMS-CHECKPOINT SECTION.                                                  
032500     SKIP2                                                                
032600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
032700     MOVE '  XD' TO GODK-STATUSKODER                                      
032800     CALL CBLTDLI USING CHKP MSG-PCB                                      
032900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
033000                        CHKP-AREA-LENGTH CHKP-AREA                        
033100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033200     PERFORM IMS-STATUSKONTROLL                                           
033300                                                                          
033400     IF IMS-EJ-OK                                                         
033500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
033600       DISPLAY FELTEXT                                                    
033700       CALL FELLOG                                                        
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 IMS-STATUSKONTROLL SECTION.                                              
034200     SKIP2                                                                
034300     SET STATUS-IX TO 1                                                   
034400     SEARCH GODK-STATUS                                                   
034500       AT END                                                             
034600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034700           DELIMITED BY SIZE INTO FELTEXT                                 
034800         DISPLAY FELTEXT                                                  
034900         CALL FELLOG                                                      
035000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035100         CONTINUE                                                         
035200     END-SEARCH                                                           
035300     .                                                                    
