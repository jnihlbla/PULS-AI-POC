000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W411ORDN.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   MARS -90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*       -PROGRAMMET KONTROLLERAR ATT EVENTUELLT GIVET                     
001100*        ORDERNUMMER (IDORDNR) INTE LIGGER I DET AUTOMATISKA              
001200*        ORDERNUMMERINTERVALLET. OM ORDERNR ÄR GODKÄNT                    
001300*        SÄTTS UT-IDORDNR = IN-IDORDNR.                                   
001400*        OM ORDERNUMMER EJ ANGIVITS BERÄKNAS AUTOMATISKT                  
001500*        ORDERNUMMER OCH IDORDNR-LEDIG UPPDATERAS.                        
001600*                                                                         
001700*       -NÄR ANROP EJ SKER FRÅN W411OHLK                                  
001800*        TAS OCKSÅ UT PARTS ORDERNUMMER (IDORDER)                         
001900*                                                                         
002000*       -NÄR ANROP SKER FRÅN WOPS TAS ENDAST PRODUKTIONSNR UT             
002100*                                                                         
002200*        PROGRAMMET LÄSER      WLXXKP (WDR1)  AUTOMATISK-ORDERNR          
002300*                              WLORQL (WDQ2C) ORDERHUVUD-SEKINDEX         
002400*                              WLORQI (WDQ2)  ORDERHUVUD                  
002410*                              WLPROC (WDE8)  PROFORMA-HUVUD              
002411*                              WDQ3D          ORDERDELSREGISTER KÖ        
002500*                                                                         
002600*    LÄNKAREA: W411ORDN                                                   
002601*                                                                         
002610*  SEPT 2005 LINDA NILSSON                                                
002620*  ETRACKER 1334295                                                       
002630*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100     SKIP3                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W411ORDN'.            
003500 77  FELTEXT                     PIC X(48).                               
003700 77  RAEKNARE                    PIC 9(2).                                
003710 77  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
003800                                                                          
003900 01  GENERELLA-SUBPROGRAM.                                                
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004300     EJECT                                                                
004500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
004600*                                                                         
004700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
004800     SKIP3                                                                
004900*    --- STATUS-KOD FRÅN IMS                                              
005000 01  STATUS-WS-Q2                PIC XX.                                  
005300     88  SEGMENT-SAKNAS-Q2                   VALUE 'GE'.                  
005400     SKIP2                                                                
005401 01  STATUS-WS-E8                PIC XX.                                  
005402     88  SEGMENT-SAKNAS-E8                   VALUE 'GE'.                  
005403     SKIP2                                                                
005410 01  STATUS-WS                   PIC XX.                                  
005420     88  SEGMENT-FINNS                       VALUE '  '.                  
005430     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005440     SKIP2                                                                
005500 01  GODK-STATUSKODER.                                                    
005600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005700     SKIP3                                                                
005800 01  SSA1                        PIC X(128).                              
005900 01  SSA2                        PIC X(64).                               
006000     EJECT                                                                
006100*    --- IMS FUNKTIONSKODER                                               
006200*01  -COPY W0003                                                          
006400     EJECT                                                                
006500 01  NYCKLAR-TILL-DLI.                                                    
006600     03  W-WDGXKEY-4525-X.                                                
006700         05  W-IDHTYP            PIC X(4)    VALUE '4525'.                
006800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
006900                                                                          
007000     03  W-WDGXKEY-4526-X.                                                
007100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
007200         05  FILLER              PIC X(4)    VALUE LOW-VALUE.             
007300                                                                          
007400     03  W-IDGMTREF-X.                                                    
007900         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
008100         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
008200         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
008300                                                                          
008400     03  W-IDORDER-X.                                                     
008500         05  W-IDORDER           PIC S9(7)   VALUE +0 COMP-3.             
008501                                                                          
008502     03  W-WDQ3D1KY-MIN-X.                                                
008503         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE +0 COMP-3.             
008504         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
008505                                                                          
008506     03  W-WDQ3D1KY-MAX-X.                                                
008507         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE +0 COMP-3.             
008508         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
008510                                                                          
008600     EJECT                                                                
008800*    ---  DLI INPUT-OUTPUT AREA                                           
008900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q201'.         
008910 01  DLI-IO-Q201.                                                         
008920*    03   -COPY WDQ201                                                    
009000     EJECT                                                                
009001 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q2C1'.         
009002 01  DLI-IO-Q2C1.                                                         
009003*    03   -COPY WDQ2C1                                                    
009004     EJECT                                                                
009010 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E801'.         
009011 01  DLI-IO-E801.                                                         
009012*    03   -COPY WDE801                                                    
009013     EJECT                                                                
009020 01  FILLER                      PIC X(16)   VALUE 'WLXXKP-AREA'.         
009100 01  4526-IO-AREA.                                                        
009200*    03  WLXXKP11 -COPY WDGX4526                                          
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-WDQ3'.         
009600 01  DLI-IO-WDQ3.                                                         
009700*    03  -COPY WDQ3D1                                                     
009800     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500*                                                                         
010600*   -COPY W411ORDN                                                        
010900     EJECT                                                                
011000*01  -COPY W0008      -PRE XXKP-                                          
011200     05  FILLER                  PIC X.                                   
011300     SKIP2                                                                
011400*01  -COPY W0008      -PRE ORQL-                                          
011600     05  FILLER                  PIC X.                                   
011610     EJECT                                                                
011620*01  -COPY W0008      -PRE PROC-                                          
012000     05  FILLER                  PIC X.                                   
012100     SKIP2                                                                
012101*01  -COPY W0008      -PRE ORQI-                                          
012130     05  FILLER                  PIC X.                                   
012131     SKIP2                                                                
012132*01  -COPY W0008      -PRE WDQ3-                                          
012133     05  FILLER                  PIC X.                                   
012134     EJECT                                                                
012200 PROCEDURE DIVISION  USING ORDN-W411ORDN XXKP-PCB ORQL-PCB                
012210                                PROC-PCB ORQI-PCB WDQ3-PCB.               
012300                                                                          
012500     MOVE SPACE TO FELTEXT                                                
012510     MOVE ZERO  TO RAEKNARE                                               
012600                                                                          
012700     PERFORM IMS-GHU-WLXXKP11-4525                                        
012800                                                                          
012900     IF ORDN-IDSYSTEM = 'WOPS' OR 'UTSK'                                  
013100        PERFORM A-TA-UT-PRODUKTIONSNR                                     
013200     ELSE                                                                 
013300        IF ORDN-IDSYSTEM = 'SATS'                                         
013500           PERFORM B-TA-UT-SATSORDERNR                                    
013600        ELSE                                                              
013601           IF ORDN-IDSYSTEM = 'OHLK'                                      
013603              PERFORM C-KOLLA-MANUELLT-ORDNR                              
013604           ELSE                                                           
014300              IF ORDN-IDORDNR-IN = +0                                     
014310                 PERFORM D-TA-UT-AUTOMATISKT-SERIENR                      
014330              ELSE                                                        
014340                 MOVE ORDN-IDORDNR-IN TO ORDN-IDORDNR-UT                  
014350              END-IF                                                      
014360                                                                          
014370              IF ORDN-IDSYSTEM NOT = 'W203' AND NOT = 'API '              
014400                 PERFORM E-TA-UT-PARTS-IDORDERNR                          
014401              END-IF                                                      
014410              PERFORM IMS-REPL-WLXXKP11                                   
014800           END-IF                                                         
014810        END-IF                                                            
014900     END-IF                                                               
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-TA-UT-PRODUKTIONSNR SECTION.                                           
015400                                                                          
015500     MOVE 4526-IDPRODNR-LEDIG TO ORDN-IDPRODNR-UT                         
015510                                 W-IDPRODNR-MIN                           
015520                                 W-IDPRODNR-MAX                           
015600                                                                          
015700     IF 4526-IDPRODNR-LEDIG = +9999999                                    
015800       MOVE +1 TO 4526-IDPRODNR-LEDIG                                     
015900     ELSE                                                                 
016000       ADD  +1 TO 4526-IDPRODNR-LEDIG                                     
016100     END-IF                                                               
016200                                                                          
016210     PERFORM IMS-GU-WDQ3D1                                                
016220     PERFORM UNTIL SEGMENT-SAKNAS                                         
016230                                                                          
016240        MOVE 4526-IDPRODNR-LEDIG TO ORDN-IDPRODNR-UT                      
016241                                    W-IDPRODNR-MIN                        
016242                                    W-IDPRODNR-MAX                        
016260        PERFORM IMS-GU-WDQ3D1                                             
016270                                                                          
016280        IF 4526-IDPRODNR-LEDIG = +9999999                                 
016290          MOVE +1 TO 4526-IDPRODNR-LEDIG                                  
016291        ELSE                                                              
016292          ADD  +1 TO 4526-IDPRODNR-LEDIG                                  
016293        END-IF                                                            
016294                                                                          
016295        ADD    1    TO RAEKNARE                                           
016296        IF RAEKNARE > 25                                                  
016297*   --- AKUT ÖKAD TILL 25 PGA PRODPROBLEM (FMI)                           
016298*   --- KOLLA VARFÖR MAN HAR KROCKAT >10 GÅNGER                           
016299           MOVE 'ÄNDRA MED FMIED IDPRODNR-LEDIG PÅ WDR1-BASEN!'           
016300                  TO FELTEXT                                              
016301           CALL ABEND USING RKOD-ABEND                                    
016302        END-IF                                                            
016303     END-PERFORM                                                          
016310     PERFORM IMS-REPL-WLXXKP11                                            
016400     .                                                                    
016500     EJECT                                                                
016510 B-TA-UT-SATSORDERNR SECTION.                                             
016520                                                                          
016530     MOVE 4526-IDORDNSB-LEDIG TO ORDN-IDORDNSB-UT                         
016540                                                                          
016550     IF 4526-IDORDNSB-LEDIG = +9999                                       
016560       MOVE +1000 TO 4526-IDORDNSB-LEDIG                                  
016570     ELSE                                                                 
016580       ADD  +1    TO 4526-IDORDNSB-LEDIG                                  
016590     END-IF                                                               
016591                                                                          
016592     PERFORM IMS-REPL-WLXXKP11                                            
016593     .                                                                    
016594     EJECT                                                                
016600 C-KOLLA-MANUELLT-ORDNR SECTION.                                          
016700                                                                          
016800     IF ORDN-IDORDNR-IN < 4526-IDORDNR7-MIN                               
016900*LO      OR ORDN-IDORDNR-IN > 4526-IDORDNR7-MAX                           
017000       MOVE ORDN-IDORDNR-IN TO ORDN-IDORDNR-UT                            
017100     ELSE                                                                 
017200       MOVE ZERO TO ORDN-IDORDNR-UT                                       
017300     END-IF                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 D-TA-UT-AUTOMATISKT-SERIENR SECTION.                                     
017700                                                                          
017800     MOVE ORDN-IDDISTR        TO W-IDDISTR                                
017900     MOVE ORDN-IDKUNDNR       TO W-IDKUNDNR                               
018000     MOVE 4526-IDORDNR7-LEDIG TO W-IDKUNDRF                               
018100                                 ORDN-IDORDNR-UT                          
018400     ADD  +1 TO 4526-IDORDNR7-LEDIG                                       
018500     IF 4526-IDORDNR7-LEDIG > 4526-IDORDNR7-MAX                           
018600       MOVE 4526-IDORDNR7-MIN TO 4526-IDORDNR7-LEDIG                      
018700     END-IF                                                               
018701                                                                          
018710     PERFORM IMS-GU-WLORQL-WDQ2C                                          
018720     PERFORM IMS-GU-WLPROC-WDE801                                         
018800                                                                          
018900     PERFORM UNTIL SEGMENT-SAKNAS-Q2 AND SEGMENT-SAKNAS-E8                
019000                                                                          
019100        MOVE 4526-IDORDNR7-LEDIG TO W-IDKUNDRF                            
019200                                    ORDN-IDORDNR-UT                       
019300        PERFORM IMS-GU-WLORQL-WDQ2C                                       
019310        PERFORM IMS-GU-WLPROC-WDE801                                      
019400                                                                          
019500        ADD  +1 TO 4526-IDORDNR7-LEDIG                                    
019600        IF 4526-IDORDNR7-LEDIG > 4526-IDORDNR7-MAX                        
019700          MOVE 4526-IDORDNR7-MIN TO 4526-IDORDNR7-LEDIG                   
019800        END-IF                                                            
019900     END-PERFORM                                                          
020000     .                                                                    
020100     EJECT                                                                
020200 E-TA-UT-PARTS-IDORDERNR SECTION.                                         
020300                                                                          
020400     MOVE 4526-IDORDER-LEDIG TO ORDN-IDORDER-UT                           
020500                                W-IDORDER                                 
020700                                                                          
020800     IF 4526-IDORDER-LEDIG = +9999999                                     
020900       MOVE +1 TO 4526-IDORDER-LEDIG                                      
021000     ELSE                                                                 
021100       ADD  +1 TO 4526-IDORDER-LEDIG                                      
021200     END-IF                                                               
021300                                                                          
021301     IF ORDN-IDSYSTEM NOT = 'PROF'                                        
021302                                                                          
021310       PERFORM IMS-GU-WLORQI01-WDQ201                                     
021400       PERFORM UNTIL SEGMENT-SAKNAS                                       
021500                                                                          
021600          MOVE 4526-IDORDER-LEDIG TO ORDN-IDORDER-UT                      
021700                                     W-IDORDER                            
021800          PERFORM IMS-GU-WLORQI01-WDQ201                                  
021900                                                                          
022000          IF 4526-IDORDER-LEDIG = +9999999                                
022100            MOVE +1 TO 4526-IDORDER-LEDIG                                 
022200          ELSE                                                            
022300            ADD  +1 TO 4526-IDORDER-LEDIG                                 
022400          END-IF                                                          
022500                                                                          
022600          ADD  1    TO RAEKNARE                                           
022700          IF RAEKNARE > 10                                                
022710*   --- KOLLA VARFÖR MAN HAR KROCKAT >10 GÅNGER                           
022800             MOVE 'ÄNDRA MED FMIED IDORDER-LEDIG PÅ WDR1-BASEN!'          
022900                    TO FELTEXT                                            
023000             CALL ABEND USING RKOD-ABEND                                  
023100          END-IF                                                          
023200       END-PERFORM                                                        
023210     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500******IMS-SEKTIONER*******************************                        
023600     SKIP2                                                                
023700 IMS-GHU-WLXXKP11-4525 SECTION.                                           
023800                                                                          
023900     STRING 'WLXXKP01(WDGXKEY  =' W-WDGXKEY-4525-X ')'                    
024000            DELIMITED BY SIZE INTO SSA1                                   
024100     STRING 'WLXXKP11(WDGXKEY  =' W-WDGXKEY-4526-X ')'                    
024200            DELIMITED BY SIZE INTO SSA2                                   
024300     MOVE '  ' TO GODK-STATUSKODER                                        
024400     CALL CBLTDLI USING GHU XXKP-PCB 4526-IO-AREA SSA1 SSA2               
024500     MOVE XXKP-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     .                                                                    
024710     SKIP2                                                                
024900 IMS-REPL-WLXXKP11 SECTION.                                               
025000                                                                          
025100     MOVE '  ' TO GODK-STATUSKODER                                        
025200     CALL CBLTDLI USING REPL XXKP-PCB 4526-IO-AREA                        
025300     MOVE XXKP-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     SKIP2                                                                
025700 IMS-GU-WLORQL-WDQ2C SECTION.                                             
025800                                                                          
025900     STRING 'WLORQL01(WDQ2C1KY =' W-IDGMTREF-X ')'                        
026100            DELIMITED BY SIZE INTO SSA1                                   
026200     MOVE 'GE  ' TO GODK-STATUSKODER                                      
026300     CALL CBLTDLI USING GU  ORQL-PCB DLI-IO-Q2C1 SSA1                     
026400     MOVE ORQL-STATUS-CODE TO STATUS-WS  STATUS-WS-Q2                     
026500     PERFORM IMS-STATUSKONTROLL                                           
026600     .                                                                    
026700     EJECT                                                                
027710 IMS-GU-WLPROC-WDE801 SECTION.                                            
027720                                                                          
027730     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
027740            DELIMITED BY SIZE INTO SSA1                                   
027750     MOVE 'GE  ' TO GODK-STATUSKODER                                      
027760     CALL CBLTDLI USING GU  PROC-PCB DLI-IO-E801 SSA1                     
027770     MOVE PROC-STATUS-CODE TO STATUS-WS  STATUS-WS-E8                     
027780     PERFORM IMS-STATUSKONTROLL                                           
027790     .                                                                    
027791     SKIP2                                                                
027792 IMS-GU-WLORQI01-WDQ201 SECTION.                                          
027793                                                                          
027794     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
027795            DELIMITED BY SIZE INTO SSA1                                   
027796     MOVE 'GE  ' TO GODK-STATUSKODER                                      
027797     CALL CBLTDLI USING GU  ORQI-PCB DLI-IO-Q201 SSA1                     
027798     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
027799     PERFORM IMS-STATUSKONTROLL                                           
027800     .                                                                    
027801     SKIP2                                                                
027802 IMS-GU-WDQ3D1 SECTION.                                                   
027803                                                                          
027804     STRING 'WDQ3D1  (WDQ3D1KY>=' W-WDQ3D1KY-MIN-X                        
027805                    '&WDQ3D1KY<=' W-WDQ3D1KY-MAX-X ')'                    
027806            DELIMITED BY SIZE INTO SSA1                                   
027807     MOVE '  GE' TO GODK-STATUSKODER                                      
027808     CALL CBLTDLI USING GU  WDQ3-PCB DLI-IO-WDQ3 SSA1                     
027809     MOVE WDQ3-STATUS-CODE TO STATUS-WS  STATUS-WS                        
027810     PERFORM IMS-STATUSKONTROLL                                           
027811     .                                                                    
027812     EJECT                                                                
027820 IMS-STATUSKONTROLL SECTION.                                              
027900     SKIP2                                                                
028000     SET STATUS-IX TO 1                                                   
028100     SEARCH GODK-STATUS AT END CALL FELLOG                                
028200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
028300     END-SEARCH                                                           
028400     .                                                                    
