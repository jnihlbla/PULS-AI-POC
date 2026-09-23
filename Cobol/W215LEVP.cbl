000100*COMPOPT STDSUB=YES                                                       
000200     SKIP2                                                                
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W215LEVP.                                                
000500 AUTHOR.         LARS THELL.                                              
000600 DATE-WRITTEN.   91/04/03.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        UPPDATERAR LEVERANSPLAN MED HEL-/DELANNULLERING                  
001200*        AV SATSORDER                                                     
001300*                                                                         
001400*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001500*        PROGRAMMET UPPATERAR WLINLB (WDD9)                               
001600*        PROGRAMMET UPPATERAR WLXXBM (WDG3)                               
001700*        PROGRAMMET LÄSER     WLXXBK (WDG3)                               
001800*        PROGRAMMET UPPATERAR WLXXBL (WDG3)                               
001900*        PROGRAMMET UPPATERAR WLXXBW (WDR5)                               
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200     SKIP2                                                                
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500 77  IDPGM                       PIC X(8)    VALUE 'W215LEVP'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  W-KVAVROP                   PIC S9(7)   VALUE ZERO  COMP-3.          
003900 77  W-TIAVRDAT-DISP             PIC S9(6)   VALUE ZERO  COMP-3.          
004000 77  W-TIAVROP                   PIC S9(5)   VALUE ZERO  COMP-3.          
004100                                                                          
004200*01  -COPY WWDCKONS                                                       
004300                                                                          
004400     EJECT                                                                
004500 01  DAGENS-DATUM.                                                        
004600     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
004700     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
004800     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
004900                                                                          
005000 01  WS-DAAVROP-AVS              PIC 9(6).                                
005100 01  FILLER  REDEFINES WS-DAAVROP-AVS.                                    
005200     03  WS-DAAVROP-SS           PIC 9(2).                                
005300     03  WS-DAAVROP-AAVV         PIC 9(4).                                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006000     SKIP2                                                                
006100***  SWITCHAR.                                                            
006200*                                                                         
006300 77  ALLT-SW                     PIC X(1)    VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500     SKIP2                                                                
006600*    --- PARAMETRAR TILL ABEND                                            
006700                                                                          
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  FELTEXT.                                                             
007200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500*01  -COPY WDATAREA                                                       
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-IDARTNR-X.                                                     
008300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008400     03  W-KDSEGKEY-X.                                                    
008500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008600     03  W-KDCLAGER-X.                                                    
008700         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
008710     03  W-IDLEVNR-X.                                                     
008720         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
008800     03  W-WDD901KY-X.                                                    
008900         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
009200         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
009300     03  W-WDD905KY-X.                                                    
009400         05  W-DAAVROP-AVS-X.                                             
009500             07  W-DAAVROP-AVS   PIC  9(6)    VALUE ZERO.                 
009600         05  W-TILEVDAG-X.                                                
009700             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
009800     03  W-KDAVROP-X.                                                     
009900         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
010000     03  W-IDLOPNRM-X.                                                    
010100         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
010200     03  W-IDORDNSB-X.                                                    
010300         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
010400     03  W-WDG3KEY-X.                                                     
010500         05  W-WDG3-KEY          PIC X(4)     VALUE SPACE.                
010600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
010700     03 W-2227KEY-X.                                                      
010800         05 W-IDHTYP             PIC X(4)    VALUE '2227'.                
010900         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
011000                                                                          
011100     EJECT                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400 01  SSA3                        PIC X(64).                               
012500 01  SSA4                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013200     SKIP3                                                                
013300 01  DLI-IO-AREA1.                                                        
013400     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
013500     SKIP3                                                                
013600     03  WLINLB23 REDEFINES IO-AREA1.                                     
013700*        05  -COPY WDD905     -PRE INLB23-                                
013800     SKIP3                                                                
013900*        05  -COPY WDD907     -PRE INLB32-                                
014000     EJECT                                                                
014100 01  DLI-IO-AREA2.                                                        
014200     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
014300     SKIP3                                                                
014400     03  WLINLB31 REDEFINES IO-AREA2.                                     
014500*        05  -COPY WDD906     -PRE INLB31-                                
014600     EJECT                                                                
014700 01  DLI-IO-AREA3.                                                        
014800     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
014900     SKIP3                                                                
015000     03  WLXXBM11 REDEFINES IO-AREA3.                                     
015100*        05  -COPY WDG32201   -PRE 2201-                                  
015200     EJECT                                                                
015300     03  WLXXBW11 REDEFINES IO-AREA3.                                     
015400*        05  -COPY WDGX2228                                               
015500 01  DLI-IO-AREA4.                                                        
015600     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
015700     SKIP3                                                                
015800     03  WLARTC11 REDEFINES IO-AREA4.                                     
015900*        05  -COPY WDK611                                                 
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200                                                                          
016300*01  -COPY W215LEVPC0                                                     
016400     EJECT                                                                
016500                                                                          
016600*01  -COPY W0008      -PRE ARTC-                                          
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008      -PRE INLB1-                                         
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200*01  -COPY W0008      -PRE INLB2-                                         
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008      -PRE XXBM-                                          
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008      -PRE XXBW-                                          
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING  LEVP-W215LEVP  ARTC-PCB                       
018200                                           INLB1-PCB INLB2-PCB            
018300                                           XXBM-PCB XXBW-PCB.             
018400                                                                          
018500*------------------------                                                 
018600     SKIP2                                                                
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     IF ALLT-OK                                                           
019000         PERFORM B-UPPDATERA-LEVPLAN                                      
019100     END-IF                                                               
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     MOVE JA                    TO ALLT-SW                                
020000                                                                          
020100     MOVE LEVP-TIREGDAT   TO TMP1-YYMMDD                                  
020200     MOVE 961216          TO TMP2-YYMMDD                                  
020300     PERFORM WY2000P1                                                     
020400     IF  LEVP-TIBEGPAC = 071231                                           
020500****    (LEVP-TIBEGPAC = 961230 OR                                        
020600****     LEVP-TIBEGPAC = 961231)                                          
020700        MOVE 0801               TO W-TIAVROP                              
020800     ELSE                                                                 
020900        MOVE 'AAMMDD'              TO DAT-KDDATFORM                       
021000        MOVE LEVP-TIBEGPAC         TO DAT-I-TIDATUM                       
021100                                                                          
021200        CALL WDATKONV USING DAT-KDDATFORM                                 
021300                            DAT-I-TIDATUM                                 
021400                            DAT-O-TIDATUM                                 
021500                            DAT-KDSVAR                                    
021600                                                                          
021700        IF DAT-KDSVAR-OK                                                  
021800            MOVE DAT-TIAAVVD (1:4) TO W-TIAVROP                           
021900        ELSE                                                              
022000            MOVE '1'               TO LEVP-KDSVAR                         
022100            MOVE NEJ               TO ALLT-SW                             
022200        END-IF                                                            
022300     END-IF                                                               
022400     MOVE ZERO                  TO W-KVAVROP                              
022500                                                                          
022600     .                                                                    
022700     EJECT                                                                
022800 B-UPPDATERA-LEVPLAN       SECTION.                                       
022900                                                                          
023000     MOVE LEVP-IDORDNSB        TO  W-IDORDNSB                             
023100     MOVE LEVP-IDARTNR         TO  W-IDARTNR                              
023200     MOVE LEVP-IDLEVNR         TO  W-IDLEVNR                              
023300     MOVE W-TIAVROP            TO  WS-DAAVROP-AAVV                        
023400     IF WS-DAAVROP-AAVV > 5000                                            
023500        MOVE 19                TO  WS-DAAVROP-SS                          
023600     ELSE                                                                 
023700        MOVE 20                TO  WS-DAAVROP-SS                          
023800     END-IF                                                               
023900     MOVE WS-DAAVROP-AVS       TO  W-DAAVROP-AVS                          
024000                                                                          
024100     MOVE W-IDARTNR            TO W-IDARTNR-D9                            
024110     MOVE WC-CDC-SE            TO W-IDDC-D9                               
024200     PERFORM IMS-GHU-INLB1-INLB23-INLB32                                  
024300                                                                          
024400******   * FÖR FIX AV ÄNDRAD LEVPLAN GODK STATUS GE OCKSÅ                 
024500*****IF SEGMENT-FINNS                                                     
024600     IF INLB23-KDAVROP = +9                                               
024700         CONTINUE                                                         
024800      ELSE                                                                
024900         IF LEVP-KVANNANT      = LEVP-KVBEART AND                         
025000            LEVP-KVBEART       = INLB23-KVAVROP                           
025100             PERFORM BA-ANNULLERA-HELA-AVROPET                            
025200          ELSE                                                            
025300             PERFORM BB-ANNULLERA-DEL-AV-AVROPET                          
025400         END-IF                                                           
025500     END-IF                                                               
025600*****END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 BA-ANNULLERA-HELA-AVROPET   SECTION.                                     
026000                                                                          
026100     MOVE +2                   TO  W-KDAVROP                              
026110     MOVE W-IDARTNR            TO  W-IDARTNR-D9                           
026120     MOVE WC-CDC-SE            TO  W-IDDC-D9                              
026200     PERFORM IMS-GU-INLB2-INLB31                                          
026300     IF SEGMENT-FINNS                                                     
026400         COMPUTE INLB23-KVAVROP = INLB23-KVAVROP - LEVP-KVANNANT          
026500         MOVE +9               TO INLB23-KDAVROP                          
026600         PERFORM IMS-REPL-INLB1-INLB23                                    
026700      ELSE                                                                
026800         PERFORM IMS-DLET-INLB1-INLB23                                    
026900     END-IF                                                               
027000     PERFORM S01-SKAPA-2201-TRANS                                         
027100     PERFORM S02-SKAPA-EV-2228-TRANS                                      
027200     MOVE '0'                  TO LEVP-KDSVAR                             
027300     .                                                                    
027400     EJECT                                                                
027500 BB-ANNULLERA-DEL-AV-AVROPET   SECTION.                                   
027600                                                                          
027700     COMPUTE INLB23-KVAVROP    = INLB23-KVAVROP - LEVP-KVANNANT           
027800     IF INLB23-KVAVROP         = ZERO                                     
027900         MOVE +9               TO INLB23-KDAVROP                          
028000     END-IF                                                               
028100     PERFORM IMS-REPL-INLB1-INLB23                                        
028200                                                                          
028300     PERFORM S01-SKAPA-2201-TRANS                                         
028400     PERFORM S02-SKAPA-EV-2228-TRANS                                      
028500     MOVE '0'                  TO LEVP-KDSVAR                             
028600                                                                          
028700     .                                                                    
028800     EJECT                                                                
028900 S01-SKAPA-2201-TRANS   SECTION.                                          
029000                                                                          
029100     MOVE '2201'               TO  W-WDG3-KEY                             
029200                                                                          
029300     MOVE LEVP-IDARTNR         TO  2201-IDARTNR-SATS                      
029400     MOVE JA                   TO  2201-FLAGGA-LPKNTL-ING                 
029500     PERFORM IMS-ISRT-XXBM11                                              
029600     .                                                                    
029700     EJECT                                                                
029800 S02-SKAPA-EV-2228-TRANS   SECTION.                                       
029900                                                                          
030000     MOVE LEVP-IDARTNR         TO  W-IDARTNR                              
030100     MOVE LEVP-KDCLAGER        TO  W-KDCLAGER                             
030200                                                                          
030300     MOVE INLB23-TIAVRDAT-DISP TO W-TIAVRDAT-DISP                         
030400     PERFORM IMS-GU-ARTC11                                                
030500     MOVE W-TIAVRDAT-DISP      TO TMP1-YYMMDD                             
030600     MOVE CLAG-TIDISPIN        TO TMP2-YYMMDD                             
030700     PERFORM WY2000P1                                                     
030800     IF TMP1-YYMMDD <= TMP2-YYMMDD                                        
030900        PERFORM S02A-SKAPA-2228-TRANS                                     
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 S02A-SKAPA-2228-TRANS   SECTION.                                         
031400                                                                          
031500     MOVE LEVP-IDARTNR         TO  2228-IDARTNR                           
031600     MOVE LOW-VALUE            TO  2228-LOW-VALUE                         
031700     MOVE SPACE                TO  2228-FILLER                            
031800                                                                          
031900     PERFORM IMS-ISRT-XXBW11                                              
032000     .                                                                    
032100     EJECT                                                                
032200* --- IMS SEKTIONER ---                                                   
032300     SKIP3                                                                
032400 IMS-GU-ARTC11        SECTION.                                            
032500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
032600          DELIMITED BY SIZE INTO SSA1                                     
032700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
032800          DELIMITED BY SIZE INTO SSA2                                     
032900     MOVE '  GE' TO GODK-STATUSKODER                                      
033000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
033100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     EJECT                                                                
033500 IMS-GHU-INLB1-INLB23-INLB32  SECTION.                                    
033600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
033700          DELIMITED BY SIZE INTO SSA1                                     
033800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
033900          DELIMITED BY SIZE INTO SSA2                                     
034000     STRING 'WLINLB23*D(DAAVROP  =' W-DAAVROP-AVS-X ')'                   
034100          DELIMITED BY SIZE INTO SSA3                                     
034200     STRING 'WLINLB32(IDORDNSB =' W-IDORDNSB-X ')'                        
034300          DELIMITED BY SIZE INTO SSA4                                     
034400     MOVE '  ' TO GODK-STATUSKODER                                        
034500*****MOVE '  GE' TO GODK-STATUSKODER                                      
034600     CALL CBLTDLI USING GHU INLB1-PCB DLI-IO-AREA1                        
034700                        SSA1 SSA2 SSA3 SSA4                               
034800     MOVE INLB1-STATUS-CODE TO STATUS-WS                                  
034900     PERFORM IMS-STATUSKONTROLL                                           
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-REPL-INLB1-INLB23 SECTION.                                           
035300                                                                          
035400     MOVE 'WLINLB32*N' TO SSA1                                            
035500     MOVE '  ' TO GODK-STATUSKODER                                        
035600     CALL CBLTDLI USING REPL INLB1-PCB DLI-IO-AREA1 SSA1                  
035700     MOVE INLB1-STATUS-CODE TO STATUS-WS                                  
035800     PERFORM IMS-STATUSKONTROLL                                           
035900     .                                                                    
036000     SKIP3                                                                
036100 IMS-DLET-INLB1-INLB23 SECTION.                                           
036200                                                                          
036300     MOVE '  ' TO GODK-STATUSKODER                                        
036400     CALL CBLTDLI USING DLET INLB1-PCB DLI-IO-AREA1                       
036500     MOVE INLB1-STATUS-CODE TO STATUS-WS                                  
036600     PERFORM IMS-STATUSKONTROLL                                           
036700     .                                                                    
036800     EJECT                                                                
036900 IMS-GU-INLB2-INLB31     SECTION.                                         
037000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
037100          DELIMITED BY SIZE INTO SSA1                                     
037200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
037300          DELIMITED BY SIZE INTO SSA2                                     
037400     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-AVS-X                         
037500                    '&KDAVROP  =' W-KDAVROP-X ')'                         
037600          DELIMITED BY SIZE INTO SSA3                                     
037700     MOVE   'WLINLB31'        TO SSA4                                     
037800     MOVE '  GE' TO GODK-STATUSKODER                                      
037900     CALL CBLTDLI USING GU INLB2-PCB DLI-IO-AREA2                         
038000                        SSA1 SSA2 SSA3 SSA4                               
038100     MOVE INLB2-STATUS-CODE TO STATUS-WS                                  
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-ISRT-XXBM11 SECTION.                                                 
038600                                                                          
038700     STRING 'WLXXBM01(WDG3KEY  =' W-WDG3KEY-X ')'                         
038800          DELIMITED BY SIZE INTO SSA1                                     
038900     MOVE 'WLXXBM11 ' TO SSA2                                             
039000     MOVE '  II' TO GODK-STATUSKODER                                      
039100     CALL CBLTDLI USING ISRT XXBM-PCB DLI-IO-AREA3 SSA1 SSA2              
039200     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
039300     PERFORM IMS-STATUSKONTROLL                                           
039400     .                                                                    
039500     EJECT                                                                
039600 IMS-ISRT-XXBW11 SECTION.                                                 
039700                                                                          
039800     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
039900          DELIMITED BY SIZE INTO SSA1                                     
040000     MOVE 'WLXXBW11 ' TO SSA2                                             
040100     MOVE '  II' TO GODK-STATUSKODER                                      
040200     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA3 SSA1 SSA2              
040300     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
040400     PERFORM IMS-STATUSKONTROLL                                           
040500     .                                                                    
040600     EJECT                                                                
040700 IMS-STATUSKONTROLL SECTION.                                              
040800     SKIP2                                                                
040900     SET STATUS-IX TO 1                                                   
041000     SEARCH GODK-STATUS                                                   
041100       AT END                                                             
041200         CALL FELLOG                                                      
041300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
041400     END-SEARCH                                                           
041500     .                                                                    
041600     EJECT                                                                
041700*    -COPY WY2000P1                                                       
