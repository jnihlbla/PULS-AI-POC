000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4408900.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   05/05/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        TOTAL BACKORDER LINES OLDER THAN TWO WEEKS                       
001000*        FILE FROM EPLUS PROGRAM W4408600                                 
001100*                                                                         
001200*        THE PROGRAM READS     WDD9 TO CHECK FOR DELIVERY INFO            
001300*                                                                         
001400* ETRACKER: 1545209                                                       
001500                                                                          
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000     EJECT                                                                
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- LAST WEEK BACKORDER                                        
002400     SELECT W44086                     ASSIGN TO W44089D1.                
002500     SKIP3                                                                
002600*          --- FILE TO W44090-PGM                                         
002700     SELECT W44089                     ASSIGN TO W44089D2.                
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W44086                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W440083      -L.                                               
003800     EJECT                                                                
003900 FD  W44089                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W440087  -PRE  UT-  -L.                                   
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'W4408900'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004900 01  ERRTEXT.                                                             
005000     03 FILLER                   PIC X(8)  VALUE 'ERRTEXT'.               
005100     03 ERRTEXT-STR              PIC X(72) VALUE SPACE.                   
005200                                                                          
005300 77  KDRC-DISPLAY                PIC Z(5).                                
005400                                                                          
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005700                                                                          
005800                                                                          
005900 77  W44086-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W44086                       VALUE 'Y'.                   
006100                                                                          
006200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007300                                                                          
007400 01  WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.                  
007500 01  WS-TILEVBSK                 PIC 9(6)    VALUE ZERO.                  
007600 01  WS-DARODAT                  PIC 9(6)    VALUE ZERO.                  
007700                                                                          
007800 77  IX                          PIC S9(3)   VALUE +0 COMP-3.             
007900                                                                          
008000*                                                                         
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400*01  AREA -COPY W440083     -PRE IN-                                      
008500                                                                          
008600     EJECT                                                                
008700*                                                                         
008800 01  UT-AREA-START               PIC X(24)   VALUE                        
008900                                             'UT-AREA-START'.             
009000     SKIP2                                                                
009100*01  AREA -COPY W440087     -PRE UT-                                      
009200                                                                          
009300     EJECT                                                                
009400                                                                          
009500*    --- PARAMETERS TO POSTSUM                                            
009600     EJECT                                                                
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900*01  -COPY WDATAREA                                                       
010000*                                                                         
010100                                                                          
010200*    --- PARAMETERS TO ABEND                                              
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010700     SKIP3                                                                
010800 01  MESSAGE-CODES.                                                       
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS  '.            
011200     SKIP3                                                                
011300 01  KEYS-TO-DLI.                                                         
011400     03  W-WDD901KY-X.                                                    
011500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011600         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
011700     03  W-IDLEVNR-X.                                                     
011800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011900     03  W-DALEVBSK-X.                                                    
012000         05  W-DALEVBSK          PIC 9(8)    VALUE ZERO.                  
012100                                                                          
012200*    --- STATUS-CODE FROM IMS                                             
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FOUND                       VALUE '  '.                  
012500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
012700     88  IMS-NOT-OK                          VALUE 'XD'.                  
012800     SKIP2                                                                
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
014000 01  DLI-IO-WDD901.                                                       
014100*    03  -COPY WDD901                                                     
014200     EJECT                                                                
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
014400 01  DLI-IO-WDD902.                                                       
014500*    03  -COPY WDD902                                                     
014600     EJECT                                                                
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
014800 01  DLI-IO-WDD924.                                                       
014900*    03  -COPY WDD924                                                     
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200                                                                          
015300*01  -COPY W0008  -PRE WDD9-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING WDD9-PCB.                                      
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
015900                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-READ-W44086                                              
016200     PERFORM UNTIL END-OF-W44086                                          
016300         PERFORM B-CHECK-KEYS                                             
016400         IF KEYS-OK                                                       
016500           PERFORM C-CREATE-FILE                                          
016600           PERFORM S11-SKRIV-W44089                                       
016700         END-IF                                                           
016800       PERFORM S01-READ-W44086                                            
016900     END-PERFORM                                                          
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN INPUT W44086                                                    
017900     OPEN OUTPUT W44089                                                   
018000                                                                          
018100     INITIALIZE IN-W440083                                                
018200                                                                          
018300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018400     MOVE 'S' TO POSTSUM-OPKOD                                            
018500     CALL POSTSUM USING POSTSUM-PARM                                      
018600     .                                                                    
018700     EJECT                                                                
018800 B-CHECK-KEYS SECTION.                                                    
018900                                                                          
019000     MOVE YES TO KEYS-SW                                                  
019100     IF IN-IDARTNR NUMERIC                                                
019200       MOVE IN-IDARTNR   TO W-IDARTNR                                     
019300       MOVE IN-IDDC-RO   TO W-IDDC                                        
019400       MOVE IN-IDLEVNR   TO W-IDLEVNR                                     
019500     ELSE                                                                 
019600       MOVE NOO          TO KEYS-SW                                       
019700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 C-CREATE-FILE SECTION.                                                   
020100                                                                          
020200     MOVE +1 TO IX                                                        
020300     PERFORM IMS-GU-WDD902                                                
020400     IF SEGMENT-FOUND                                                     
020500       PERFORM UNTIL IX > 3                                               
020600         PERFORM IMS-GNP-WDD924                                           
020700         IF SEGMENT-FOUND                                                 
020800           MOVE LEV-TILEVBSK-INL TO WS-TILEVBSK                           
020900                                                                          
021000           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
021100           MOVE WS-TILEVBSK TO DAT-I-TIDATUM                              
021200                                                                          
021300           CALL WDATKONV USING DAT-KDDATFORM                              
021400                               DAT-I-TIDATUM                              
021500                               DAT-O-TIDATUM                              
021600                               DAT-KDSVAR                                 
021700                                                                          
021800           IF DAT-KDSVAR-OK                                               
021900             MOVE DAT-TIAAVVD TO WS-TIAAVVD                               
022000             MOVE WS-TIAAVVD  TO UT-TIAAVVD (IX)                          
022100           ELSE                                                           
022200             MOVE 'FEL SVAR FRÅN WDATKONV' TO ERRTEXT                     
022300             MOVE RKOD-ABEND-WITH-DUMP TO RKOD-ABEND                      
022400             PERFORM S99-ABEND                                            
022500           END-IF                                                         
022600         ELSE                                                             
022700           MOVE ZERO TO UT-TIAAVVD (IX)                                   
022800         END-IF                                                           
022900         ADD +1 TO IX                                                     
023000       END-PERFORM                                                        
023100       PERFORM IMS-GNP-WDD924                                             
023200     ELSE                                                                 
023300       MOVE ZERO TO UT-TIAAVVD (1)                                        
023400                    UT-TIAAVVD (2)                                        
023500                    UT-TIAAVVD (3)                                        
023600     END-IF                                                               
023700                                                                          
023800     MOVE IN-IDARTNR                 TO UT-IDARTNR                        
023900     MOVE IN-BEART                   TO UT-BEART                          
024000     MOVE IN-IDLEVNR                 TO UT-IDLEVNR                        
024100     MOVE IN-KDPRODSL                TO UT-KDPRODSL                       
024200     MOVE IN-IDFKNGRP                TO UT-IDFKNGRP                       
024300     MOVE IN-KVRADER                 TO UT-KVRADER                        
024400     MOVE IN-KVBEART-Q               TO UT-KVBEART-Q                      
024500     IF IN-PRARTNTO-LOCPREL > ZERO                                        
024600       MOVE '*'                      TO UT-TEASTRIX                       
024700     ELSE                                                                 
024800       MOVE SPACE                    TO UT-TEASTRIX                       
024900     END-IF                                                               
025000     MOVE IN-IDLANDX3                TO UT-IDLANDX3                       
025100     MOVE IN-FLSPARR                 TO UT-FLSPARR                        
025200     MOVE IN-FLSALDCH                TO UT-FLSALDCH                       
025300     MOVE IN-SUORDV                  TO UT-SUORDV                         
025310     MOVE IN-IDDISTR                 TO UT-IDDISTR                        
025320     MOVE IN-IDKUNDNR                TO UT-IDKUNDNR                       
025330     MOVE IN-IDORDNR5                TO UT-IDORDNR5                       
025340     MOVE IN-KVRO                    TO UT-KVRO                           
025350     MOVE IN-TIREGDAT                TO UT-TIREGDAT                       
025400           MOVE IN-DARODAT TO WS-DARODAT                                  
025500                                                                          
025600           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
025700           MOVE WS-DARODAT TO DAT-I-TIDATUM                               
025800                                                                          
025900           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
026000                               DAT-O-TIDATUM DAT-KDSVAR                   
026100                                                                          
026200           IF DAT-KDSVAR-OK                                               
026300             MOVE DAT-TIAAVVD TO UT-DARODAT                               
026400           ELSE                                                           
026500             MOVE 'FEL SVAR FRÅN WDATKONV' TO ERRTEXT                     
026600             MOVE RKOD-ABEND-WITH-DUMP TO RKOD-ABEND                      
026700             PERFORM S99-ABEND                                            
026800           END-IF                                                         
026900     .                                                                    
027000     EJECT                                                                
027100 Z-FINIT SECTION.                                                         
027200                                                                          
027300     CLOSE W44086                                                         
027400           W44089                                                         
027500     .                                                                    
027600     EJECT                                                                
027700 S01-READ-W44086  SECTION.                                                
027800     SKIP2                                                                
027900     READ W44086 INTO IN-AREA                                             
028000     AT END                                                               
028100        MOVE HIGH-VALUE TO IN-AREA                                        
028200        SET END-OF-W44086 TO TRUE                                         
028300                                                                          
028400     NOT AT END                                                           
028500        MOVE 'W44089' TO POSTSUM-FDNAMN                                   
028600        MOVE 'W44089D1' TO POSTSUM-DDNAMN2                                
028700        MOVE SPACE TO POSTSUM-TRANSTYP                                    
028800        CALL POSTSUM USING POSTSUM-PARM                                   
028900     END-READ                                                             
029000     .                                                                    
029100     EJECT                                                                
029200 S11-SKRIV-W44089 SECTION.                                                
029300                                                                          
029400     WRITE UT-POST FROM UT-AREA                                           
029500                                                                          
029600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029700     MOVE 'W44089'   TO POSTSUM-FDNAMN                                    
029800     MOVE 'W44089D2' TO POSTSUM-DDNAMN2                                   
029900     CALL POSTSUM USING POSTSUM-PARM                                      
030000     .                                                                    
030100     EJECT                                                                
030200 S99-ABEND SECTION.                                                       
030300                                                                          
030400     CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800* --- IMS SECTIONS  ---                                                   
030900 IMS-GU-WDD902 SECTION.                                                   
031000                                                                          
031100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
031200          DELIMITED BY SIZE INTO SSA1                                     
031300     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
031400          DELIMITED BY SIZE INTO SSA2                                     
031500     MOVE '  GE' TO GOOD-STATUSCODES                                      
031600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
031700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
031800     PERFORM IMS-STATUSCHECK                                              
031900     .                                                                    
032000     EJECT                                                                
032100 IMS-GNP-WDD924 SECTION.                                                  
032200     MOVE 'WDD924' TO SSA1                                                
032300     MOVE '  GE' TO GOOD-STATUSCODES                                      
032400     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
032500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
032600     PERFORM IMS-STATUSCHECK                                              
032700     .                                                                    
032800     EJECT                                                                
032900 IMS-STATUSCHECK SECTION.                                                 
033000     SKIP2                                                                
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GOOD-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500           DELIMITED BY SIZE INTO ERRTEXT                                 
033600         DISPLAY ERRTEXT                                                  
033700         CALL FELLOG                                                      
033800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033900         CONTINUE                                                         
034000     END-SEARCH                                                           
034100     .                                                                    
034200     EJECT                                                                
