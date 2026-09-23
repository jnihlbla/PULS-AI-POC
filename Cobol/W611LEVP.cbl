000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611LEVP.                                                
000500*AUTHOR.         LARS THELL.                                              
000600*DATE-WRITTEN.   92/02/10.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        SUB PROGRAM SOM UPPDATERAR LEVERANSBESKEDET PÅ                   
001200*        LEVERANSPLANEN.                                                  
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100     SKIP3                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300     SKIP2                                                                
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(8)    VALUE 'W611LEVP'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000                                                                          
003100 01  WS-DATE-TIME.                                                        
003200     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
003300     03  FILLER    REDEFINES DAGENS-TID.                                  
003400         05  DAGENS-HHMMSS       PIC 9(6).                                
003500         05  FILLER              PIC 9(2).                                
003600     03  DAGENS-DATUM            PIC 9(6) VALUE ZERO.                     
003700                                                                          
003800 01  W-DATUM-AADDD.                                                       
003900   03  W-DATUM-AA                PIC 9(2).                                
004000   03  W-DATUM-DDD               PIC 9(3).                                
004100                                                                          
004200 01  W-JMF-AADDD.                                                         
004300   03  W-JMF-AA                  PIC 9(2).                                
004400   03  W-JMF-DDD                 PIC 9(3).                                
004500                                                                          
004600 01  WS-DALEVBSK-AVS             PIC 9(8).                                
004700 01  WS-DALEVBSK REDEFINES WS-DALEVBSK-AVS.                               
004800   03  WS-DA                     PIC 9(2).                                
004900   03  WS-TILEVBSK               PIC 9(6).                                
005000                                                                          
005100 01  W-NY-AADDD                  PIC 9(6).                                
005200 01  FILLER REDEFINES W-NY-AADDD.                                         
005300   03  FILLER                    PIC 9.                                   
005400   03  W-NY-AA                   PIC 9(2).                                
005500   03  W-NY-DDD                  PIC 9(3).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006400     SKIP2                                                                
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006800     EJECT                                                                
006900*01  -COPY WDATAREA                                                       
007000     EJECT                                                                
007100 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
007200*01   -COPY WORKAREA.                                                     
007300     EJECT                                                                
007400*01     W-WLINLB24  -COPY WDD924                                          
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007900     03  W-WDD901KY-X.                                                    
008000         05 W-IDARTNR-X.                                                  
008100             07 W-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
008200         05 W-IDDC-X.                                                     
008300             07 W-IDDC           PIC  X(2)   VALUE SPACE.                 
008400     03  W-IDLEVNR-X.                                                     
008500         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
008600     03  W-DALEVBSK-X.                                                    
008700         05  W-DALEVBSK          PIC  9(8)   VALUE ZERO.                  
008800     SKIP2                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FINNS                       VALUE '  '.                  
009200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100 01  SSA3                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNKTIONSKODER                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010800     SKIP3                                                                
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011100     SKIP3                                                                
011200     03  WLINLB24 REDEFINES IO-AREA.                                      
011300*        05  -COPY WDD924  -PRE INLB24-                                   
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W611LEVP                                                       
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE INLB-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING LEVP-W611LEVP INLB-PCB.                        
012300                                                                          
012400     PERFORM A-INIT                                                       
012500                                                                          
012600     EVALUATE LEVP-KDBEH                                                  
012700        WHEN 'T'                                                          
012800         PERFORM B-TILLAEGG-LEVERANSBESKED                                
012900                                                                          
013000        WHEN 'R'                                                          
013100         PERFORM C-RAETTA-LEVERANSBESKED                                  
013200                                                                          
013300        WHEN 'B'                                                          
013400         PERFORM D-TA-BORT-LEVERANSBESKED                                 
013500                                                                          
013600     END-EVALUATE                                                         
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200                                                                          
014300     MOVE LEVP-IDARTNR         TO W-IDARTNR                               
014400     MOVE LEVP-IDDC            TO W-IDDC                                  
014500     MOVE LEVP-IDLEVNR         TO W-IDLEVNR                               
014600     MOVE LEVP-TILEVBSK-AVS    TO WS-TILEVBSK                             
014700     IF WS-TILEVBSK (1:2) < 50                                            
014800       MOVE 20 TO WS-DA                                                   
014900     ELSE                                                                 
015000       MOVE 19 TO WS-DA                                                   
015100     END-IF                                                               
015200     MOVE WS-DALEVBSK-AVS      TO W-DALEVBSK                              
015300                                                                          
015400     ACCEPT DAGENS-DATUM     FROM DATE                                    
015500     ACCEPT DAGENS-TID       FROM TIME                                    
015600                                                                          
015700     .                                                                    
015800     EJECT                                                                
015900 B-TILLAEGG-LEVERANSBESKED SECTION.                                       
016000                                                                          
016100     PERFORM IMS-GHU-INLB24                                               
016200     IF SEGMENT-FINNS                                                     
016300       IF INLB24-LEV-FLFORAVI = 'J'                                       
016400         PERFORM BB-UPPDATERA-INLB24                                      
016500       ELSE                                                               
016600         MOVE LEVP-KVAVIS TO INLB24-LEV-KVAVIS-BSKURS                     
016700                             INLB24-LEV-KVAVIS-BSKKVAR                    
016800         MOVE JA          TO INLB24-LEV-FLFORAVI                          
016900         MOVE DAGENS-DATUM   TO INLB24-LEV-TIREGDAT                       
017000         MOVE DAGENS-HHMMSS  TO INLB24-LEV-TIREGTID                       
017100         PERFORM IMS-REPL-INLB24                                          
017200       END-IF                                                             
017300     ELSE                                                                 
017400       PERFORM BC-SKAPA-INLB24                                            
017500     END-IF                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 BB-UPPDATERA-INLB24     SECTION.                                         
017900                                                                          
018000     ADD LEVP-KVAVIS  TO INLB24-LEV-KVAVIS-BSKURS                         
018100                         INLB24-LEV-KVAVIS-BSKKVAR                        
018200                                                                          
018300     MOVE DAGENS-DATUM       TO INLB24-LEV-TIREGDAT                       
018400     MOVE DAGENS-HHMMSS      TO INLB24-LEV-TIREGTID                       
018500                                                                          
018600     PERFORM IMS-REPL-INLB24                                              
018700     .                                                                    
018800     EJECT                                                                
018900 BC-SKAPA-INLB24     SECTION.                                             
019000                                                                          
019100     MOVE WS-DALEVBSK-AVS  TO INLB24-LEV-DALEVBSK-AVS                     
019200     PERFORM BCA-BERAEKNA-DISP                                            
019300     IF DAT-KDSVAR-OK                                                     
019400       MOVE DAT-TIAAMMDD     TO INLB24-LEV-TILEVBSK-DISP                  
019500     ELSE                                                                 
019600       CALL FELLOG                                                        
019700     END-IF                                                               
019800                                                                          
019900     IF INLB24-LEV-TILEVBSK-DISP > ZERO                                   
020000       MOVE 002                      TO WORK-KDCALL                       
020100       MOVE 1                        TO WORK-KVWORKD                      
020200       MOVE W-IDDC                   TO WORK-IDDC                         
020300       MOVE INLB24-LEV-TILEVBSK-DISP TO WORK-TIAAMMDD-FOM                 
020400       CALL WORKDAY               USING WORK-KDCALL,                      
020500                                        WORK-DATE-AREA,                   
020600                                        WORK-KDSVAR                       
020700       IF WORK-KDSVAR-OK                                                  
020800         MOVE WORK-TIAAMMDD-TOM TO INLB24-LEV-TILEVBSK-DISP               
020900       END-IF                                                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE LEVP-TILEVBSK-INL  TO INLB24-LEV-TILEVBSK-INL                   
021300     MOVE LEVP-KVAVIS        TO INLB24-LEV-KVAVIS-BSKURS                  
021400                                INLB24-LEV-KVAVIS-BSKKVAR                 
021500     MOVE JA                 TO INLB24-LEV-FLFORAVI                       
021600     MOVE NEJ                TO INLB24-LEV-FLSENLEV                       
021700     MOVE DAGENS-DATUM       TO INLB24-LEV-TIREGDAT                       
021800     MOVE DAGENS-HHMMSS      TO INLB24-LEV-TIREGTID                       
021900     PERFORM IMS-ISRT-INLB24                                              
022000     .                                                                    
022100     EJECT                                                                
022200 BCA-BERAEKNA-DISP SECTION.                                               
022300                                                                          
022400     MOVE ZERO                 TO W-NY-AADDD                              
022500     MOVE LEVP-TILEVBSK-INL    TO DAT-I-TIDATUM                           
022600     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
022700                                                                          
022800     CALL WDATKONV USING DAT-KDDATFORM,                                   
022900                         DAT-I-TIDATUM,                                   
023000                         DAT-O-TIDATUM,                                   
023100                         DAT-KDSVAR                                       
023200     IF DAT-KDSVAR-OK                                                     
023300       MOVE DAT-TIAADDD        TO W-DATUM-AADDD                           
023400     ELSE                                                                 
023500       MOVE ZERO               TO W-DATUM-AADDD                           
023600     END-IF                                                               
023700     COMPUTE W-DATUM-DDD =                                                
023800             W-DATUM-DDD +                                                
023900             LEVP-KVDAGAR-INLEV                                           
024000     MOVE W-DATUM-AA           TO W-NY-AA                                 
024100     IF W-DATUM-DDD            > 365                                      
024200       MOVE W-DATUM-AA         TO W-JMF-AA                                
024300       MOVE 366 TO W-JMF-DDD                                              
024400       MOVE W-JMF-AADDD        TO DAT-I-TIDATUM                           
024500       MOVE 'AADDD '           TO DAT-KDDATFORM                           
024600                                                                          
024700       CALL WDATKONV USING DAT-KDDATFORM,                                 
024800                           DAT-I-TIDATUM,                                 
024900                           DAT-O-TIDATUM,                                 
025000                           DAT-KDSVAR                                     
025100       IF DAT-KDSVAR-OK                                                   
025200         COMPUTE W-NY-DDD      = W-DATUM-DDD                              
025300                               - 366                                      
025400       ELSE                                                               
025500         COMPUTE W-NY-DDD      = W-DATUM-DDD                              
025600                               - 365                                      
025700       END-IF                                                             
025800       ADD 1                   TO W-NY-AA                                 
025900     ELSE                                                                 
026000       MOVE W-DATUM-DDD        TO W-NY-DDD                                
026100     END-IF                                                               
026200     MOVE W-NY-AADDD           TO DAT-I-TIDATUM                           
026300     MOVE 'AADDD '             TO DAT-KDDATFORM                           
026400                                                                          
026500     CALL WDATKONV USING DAT-KDDATFORM,                                   
026600                         DAT-I-TIDATUM,                                   
026700                         DAT-O-TIDATUM,                                   
026800                         DAT-KDSVAR                                       
026900     .                                                                    
027000     EJECT                                                                
027100 C-RAETTA-LEVERANSBESKED SECTION.                                         
027200                                                                          
027300     PERFORM IMS-GHU-INLB24                                               
027400     IF SEGMENT-FINNS                                                     
027500         MOVE LEVP-KVAVIS    TO INLB24-LEV-KVAVIS-BSKURS                  
027600                                INLB24-LEV-KVAVIS-BSKKVAR                 
027700         MOVE DAGENS-DATUM   TO INLB24-LEV-TIREGDAT                       
027800         MOVE DAGENS-HHMMSS  TO INLB24-LEV-TIREGTID                       
027900         PERFORM IMS-REPL-INLB24                                          
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 D-TA-BORT-LEVERANSBESKED SECTION.                                        
028400                                                                          
028500     PERFORM IMS-GHU-INLB24                                               
028600     IF SEGMENT-FINNS                                                     
028700         PERFORM IMS-DLET-INLB24                                          
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100* --- IMS SEKTIONER ---                                                   
029200     SKIP3                                                                
029300 IMS-GU-INLB11 SECTION.                                                   
029400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
029500          DELIMITED BY SIZE INTO SSA1                                     
029600     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
029700          DELIMITED BY SIZE INTO SSA2                                     
029800     MOVE '  GE' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
030000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-GHU-INLB24 SECTION.                                                  
030500                                                                          
030600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
030700          DELIMITED BY SIZE INTO SSA1                                     
030800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
030900          DELIMITED BY SIZE INTO SSA2                                     
031000     STRING 'WLINLB24(DALEVBSK =' W-DALEVBSK-X ')'                        
031100          DELIMITED BY SIZE INTO SSA3                                     
031200     MOVE '  GE' TO GODK-STATUSKODER                                      
031300     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
031400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031700     SKIP3                                                                
031800 IMS-GNP-INLB24 SECTION.                                                  
031900                                                                          
032000     MOVE   'WLINLB24'  TO SSA1                                           
032100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032200     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
032300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600     SKIP3                                                                
032700 IMS-ISRT-INLB24 SECTION.                                                 
032800                                                                          
032900     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
033000          DELIMITED BY SIZE INTO SSA1                                     
033100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
033200          DELIMITED BY SIZE INTO SSA2                                     
033300     MOVE 'WLINLB24 ' TO SSA3                                             
033400     MOVE '  GE' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
033600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     SKIP3                                                                
034000 IMS-REPL-INLB24 SECTION.                                                 
034100                                                                          
034200     MOVE '  ' TO GODK-STATUSKODER                                        
034300     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
034400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
034500     PERFORM IMS-STATUSKONTROLL                                           
034600     .                                                                    
034700     SKIP3                                                                
034800 IMS-DLET-INLB24 SECTION.                                                 
034900                                                                          
035000     MOVE '  ' TO GODK-STATUSKODER                                        
035100     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
035200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     EJECT                                                                
035600 IMS-STATUSKONTROLL SECTION.                                              
035700     SKIP2                                                                
035800     SET STATUS-IX TO 1                                                   
035900     SEARCH GODK-STATUS                                                   
036000       AT END                                                             
036100         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
036200         CALL FELLOG                                                      
036300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036400         CONTINUE                                                         
036500     END-SEARCH                                                           
036600     .                                                                    
