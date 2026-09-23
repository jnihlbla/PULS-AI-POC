000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2711300.                                                
000400*AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500*DATE-WRITTEN.   94/12/06.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSNING AV ORDRARNA SOM BLIVIT RELEASADE FRÅN REFILL-           
001100*        DATABASEN SAMT FÖRSLAG SOM INTE BEHANDLATS                       
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLORDL (WDE3)                              
001400*        PROGRAMMET UPPDATERAR WDK711 (WDK7)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
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
002700     SKIP2                                                                
002800*          --- INFIL MED RENS-POSTER                                      
002900     SELECT W27113                     ASSIGN TO W27113D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W27113                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W27113      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2711300'.            
004600 01  CHKP-VAR.                                                            
004700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005200 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900                                                                          
006000 77  W27113-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W27113                       VALUE 'J'.                   
006200     EJECT                                                                
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400                                                                          
006410 01  W-FLREDUCE-KVBEART          PIC X(1)    VALUE 'N'.                   
006500     EJECT                                                                
006600                                                                          
006700                                                                          
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008010 01  PARM-SYSIN.                                                          
008020     03  PARM-REDUCE-KVBEART  PIC X(14)  VALUE SPACE.                     
008050     03  FILLER               PIC X(66).                                  
008060     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W27113     -PRE IN-                                       
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03 W-WDE301KY-X.                                                     
009200         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
009300         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
009400         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
009500         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
009600         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
009700                                                                          
009800                                                                          
009900     03  W-IDARTNR-X.                                                     
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100     03  W-IDDC-X.                                                        
010200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FINNS                       VALUE '  '.                  
010700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011000     88  IMS-EJ-OK                           VALUE 'XD'.                  
011100     SKIP2                                                                
011200 01  GODK-STATUSKODER.                                                    
011300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(128).                              
011600 01  SSA2                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
012300     SKIP3                                                                
012400 01  DLI-IO-AREA1.                                                        
012500     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
012600     SKIP3                                                                
012700     03  WLORDL01 REDEFINES IO-AREA1.                                     
012800*        05  -COPY WDE301  -PRE ORDL-                                     
012900     SKIP3                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
013100 01  DLI-IO-WDK711.                                                       
013200*    03  -COPY WDK711                                                     
013300     SKIP3                                                                
013400                                                                          
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009   -PRE MSG-                                              
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE ORDL-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008  -PRE ARTS-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE WDK7-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING MSG-PCB ORDL-PCB WDK7-PCB.                     
014900     ENTRY 'DLITCBL' USING MSG-PCB ORDL-PCB WDK7-PCB.                     
015000                                                                          
015100     PERFORM A-INIT                                                       
015200                                                                          
015300     PERFORM S01-LAES-W27113                                              
015400                                                                          
015500     PERFORM UNTIL END-OF-W27113                                          
015600                                                                          
015700       IF CHKP-ANT > CHKP-MAX                                             
015800         PERFORM X-TAG-CHECKPOINT                                         
015900       END-IF                                                             
016000                                                                          
016100       MOVE IN-IDDC          TO W-IDDC-301                                
016200                                W-IDDC                                    
016300       MOVE IN-IDPERSON-BUY  TO W-IDPERSON-BUY                            
016400       MOVE IN-KDREFTYP      TO W-KDREFTYP                                
016500       MOVE IN-IDARTNR       TO W-IDARTNR-301                             
016600                                W-IDARTNR                                 
016700       MOVE IN-IDDISTR       TO W-IDDISTR                                 
016800                                                                          
016900       PERFORM IMS-GHU-ORDL01                                             
017000                                                                          
017100       IF SEGMENT-FINNS                                                   
017200                                                                          
017300          ADD +1 TO CHKP-ANT                                              
017400          PERFORM IMS-DLET-ORDL                                           
017500          IF IN-FLREFNYO = JA                                             
017510          OR W-FLREDUCE-KVBEART = JA                                      
017600             PERFORM IMS-GHU-WDK711                                       
017700             IF SEGMENT-FINNS                                             
017710                IF IN-FLREFNYO = JA                                       
017800                   MOVE IN-FLREFNYO TO SLAG-FLREFNYO                      
017810                END-IF                                                    
017820                IF W-FLREDUCE-KVBEART = JA                                
017822                   COMPUTE SLAG-KVBEART = SLAG-KVBEART                    
017823                                        - ORDL-REF-KVBEART                
017840                END-IF                                                    
017900                PERFORM IMS-REPL-WDK711                                   
018000                ADD +1 TO CHKP-ANT                                        
018100             END-IF                                                       
018200          END-IF                                                          
018300       ELSE                                                               
018400*                                                                         
018500*------  ÅTERSTART HAR SKETT EFTERSOM POST SAKNAS PÅ WDE3                 
018600*                                                                         
018700         CONTINUE                                                         
018800       END-IF                                                             
018900                                                                          
019000       PERFORM S01-LAES-W27113                                            
019100     END-PERFORM                                                          
019200                                                                          
019300                                                                          
019400     PERFORM Z-FINIT                                                      
019500                                                                          
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020100     SKIP2                                                                
020200                                                                          
020300     PERFORM IMS-RESTART                                                  
020400                                                                          
020500     OPEN INPUT W27113                                                    
020600                                                                          
020700     ACCEPT DAGENS-DATUM FROM DATE                                        
020800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020810                                                                          
020820     ACCEPT PARM-SYSIN FROM SYSIN                                         
020830     UNSTRING PARM-SYSIN DELIMITED BY ','                                 
020840       INTO PARM-REDUCE-KVBEART                                           
020870                                                                          
020880     DISPLAY 'PARM-REDUCE-KVBEART : ' PARM-REDUCE-KVBEART                 
020890     IF       PARM-REDUCE-KVBEART   =     'REDUCE-KVBEART'                
020891       MOVE JA                      TO W-FLREDUCE-KVBEART                 
020892     ELSE                                                                 
020893       MOVE NEJ                     TO W-FLREDUCE-KVBEART                 
020894     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 Z-FINIT SECTION.                                                         
021200                                                                          
021300     CLOSE W27113                                                         
021400     SKIP2                                                                
021500     MOVE 'S' TO POSTSUM-OPKOD                                            
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
021800     EJECT                                                                
021900 S01-LAES-W27113  SECTION.                                                
022000     SKIP2                                                                
022100     READ W27113 INTO IN-AREA                                             
022200     AT END                                                               
022300        SET END-OF-W27113 TO TRUE                                         
022400                                                                          
022500     NOT AT END                                                           
022600        MOVE 'W27113' TO POSTSUM-FDNAMN                                   
022700        MOVE 'W27113D1' TO POSTSUM-DDNAMN2                                
022800        CALL POSTSUM USING POSTSUM-PARM                                   
022900                                                                          
023000     END-READ                                                             
023100     .                                                                    
023200     EJECT                                                                
023300 X-TAG-CHECKPOINT   SECTION.                                              
023400                                                                          
023500     PERFORM IMS-CHECKPOINT                                               
023600     MOVE ZERO TO CHKP-ANT                                                
023700     .                                                                    
023800     EJECT                                                                
023900* --- IMS SEKTIONER ---                                                   
024000     SKIP3                                                                
024100     EJECT                                                                
024200 IMS-GHU-ORDL01 SECTION.                                                  
024300                                                                          
024400     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
024500          DELIMITED BY SIZE INTO SSA1                                     
024600     MOVE '  GE' TO GODK-STATUSKODER                                      
024700     CALL CBLTDLI USING GHN ORDL-PCB DLI-IO-AREA1 SSA1                    
024800     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
024900     PERFORM IMS-STATUSKONTROLL                                           
025000     .                                                                    
025100     SKIP3                                                                
025200                                                                          
025300 IMS-DLET-ORDL SECTION.                                                   
025400                                                                          
025500     MOVE '  ' TO GODK-STATUSKODER                                        
025600     CALL CBLTDLI USING DLET ORDL-PCB DLI-IO-AREA1                        
025700     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000     EJECT                                                                
026100                                                                          
026200 IMS-GHU-WDK711 SECTION.                                                  
026300                                                                          
026400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
026500          DELIMITED BY SIZE INTO SSA1                                     
026600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
026700          DELIMITED BY SIZE INTO SSA2                                     
026800     MOVE '  GE' TO GODK-STATUSKODER                                      
026900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
027000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
027100     PERFORM IMS-STATUSKONTROLL                                           
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 IMS-REPL-WDK711 SECTION.                                                 
027600                                                                          
027700     MOVE '  ' TO GODK-STATUSKODER                                        
027800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
027900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
028000     PERFORM IMS-STATUSKONTROLL                                           
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 IMS-RESTART SECTION.                                                     
028500     SKIP2                                                                
028600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028700     MOVE '  ' TO GODK-STATUSKODER                                        
028800     CALL CBLTDLI USING XRST MSG-PCB                                      
028900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029000                        CHKP-AREA-LENGTH CHKP-AREA                        
029100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400     EJECT                                                                
029500 IMS-CHECKPOINT SECTION.                                                  
029600     SKIP2                                                                
029700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029800     MOVE '  XD' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING CHKP MSG-PCB                                      
030000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030100                        CHKP-AREA-LENGTH CHKP-AREA                        
030200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400                                                                          
030500     IF IMS-EJ-OK                                                         
030600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
030700       DISPLAY FELTEXT                                                    
030800       CALL FELLOG                                                        
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 IMS-STATUSKONTROLL SECTION.                                              
031300     SKIP2                                                                
031400     SET STATUS-IX TO 1                                                   
031500     SEARCH GODK-STATUS                                                   
031600       AT END                                                             
031700         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
031800         DISPLAY FELTEXT                                                  
031900         CALL FELLOG                                                      
032000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
