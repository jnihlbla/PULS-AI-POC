000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4408700.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   05/05/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        LATEST WEEKS BACKORDER LINES TOP 30 PARTS                        
001000*        FILE FROM EPLUS PROGRAM W4408400                                 
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
002400     SELECT W44084                     ASSIGN TO W44087D1.                
002500     SKIP3                                                                
002600*          --- FILE TO W44090-PGM                                         
002700     SELECT W44087                     ASSIGN TO W44087D2.                
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W44084                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W440083      -L.                                               
003800     EJECT                                                                
003900 FD  W44087                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W440087  -PRE  UT-  -L.                                   
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'W4408700'.            
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
005900 77  W44084-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W44084                       VALUE 'Y'.                   
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
007600                                                                          
007700 77  IX                          PIC S9(3)   VALUE +0 COMP-3.             
007800                                                                          
007900*                                                                         
008000 01  IN-AREA-START               PIC X(24)   VALUE                        
008100                                             'IN-AREA-START'.             
008200     SKIP2                                                                
008300*01  AREA -COPY W440083     -PRE IN-                                      
008400                                                                          
008500     EJECT                                                                
008600*                                                                         
008700 01  UT-AREA-START               PIC X(24)   VALUE                        
008800                                             'UT-AREA-START'.             
008900     SKIP2                                                                
009000*01  AREA -COPY W440087     -PRE UT-                                      
009100                                                                          
009200     EJECT                                                                
009300                                                                          
009400*    --- PARAMETERS TO POSTSUM                                            
009500     EJECT                                                                
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800*01  -COPY WDATAREA                                                       
009900*                                                                         
010000                                                                          
010100*    --- PARAMETERS TO ABEND                                              
010200                                                                          
010300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010600     SKIP3                                                                
010700 01  MESSAGE-CODES.                                                       
010800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS  '.            
011100     SKIP3                                                                
011200 01  KEYS-TO-DLI.                                                         
011300     03  W-WDD901KY-X.                                                    
011400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011500         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
011600     03  W-IDLEVNR-X.                                                     
011700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011800     03  W-DALEVBSK-X.                                                    
011900         05  W-DALEVBSK          PIC 9(8)    VALUE ZERO.                  
012000                                                                          
012100*    --- STATUS-CODE FROM IMS                                             
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FOUND                       VALUE '  '.                  
012400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012500     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
012600     88  IMS-NOT-OK                          VALUE 'XD'.                  
012700     SKIP2                                                                
012800 01  GOOD-STATUSCODES.                                                    
012900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNCTION CODES                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700                                                                          
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
013900 01  DLI-IO-WDD901.                                                       
014000*    03  -COPY WDD901                                                     
014100     EJECT                                                                
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
014300 01  DLI-IO-WDD902.                                                       
014400*    03  -COPY WDD902                                                     
014500     EJECT                                                                
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
014700 01  DLI-IO-WDD924.                                                       
014800*    03  -COPY WDD924                                                     
014900     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100                                                                          
015200*01  -COPY W0008  -PRE WDD9-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING WDD9-PCB.                                      
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
015800                                                                          
015900     PERFORM A-INIT                                                       
016000     PERFORM S01-READ-W44084                                              
016100     PERFORM UNTIL END-OF-W44084                                          
016200         PERFORM B-CHECK-KEYS                                             
016300         IF KEYS-OK                                                       
016400           PERFORM C-CREATE-FILE                                          
016500           PERFORM S11-SKRIV-W44087                                       
016600         END-IF                                                           
016700       PERFORM S01-READ-W44084                                            
016800     END-PERFORM                                                          
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     OPEN INPUT W44084                                                    
017800     OPEN OUTPUT W44087                                                   
017900                                                                          
018000     INITIALIZE IN-W440083                                                
018100                                                                          
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     MOVE 'S' TO POSTSUM-OPKOD                                            
018400     CALL POSTSUM USING POSTSUM-PARM                                      
018500     .                                                                    
018600     EJECT                                                                
018700 B-CHECK-KEYS SECTION.                                                    
018800                                                                          
018900     MOVE YES TO KEYS-SW                                                  
019000     IF IN-IDARTNR NUMERIC                                                
019100       MOVE IN-IDARTNR   TO W-IDARTNR                                     
019110       MOVE IN-IDDC-RO   TO W-IDDC                                        
019200       MOVE IN-IDLEVNR   TO W-IDLEVNR                                     
019300     ELSE                                                                 
019400       MOVE NOO          TO KEYS-SW                                       
019500     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 C-CREATE-FILE SECTION.                                                   
019900                                                                          
020000     MOVE +1 TO IX                                                        
020100     PERFORM IMS-GU-WDD902                                                
020200     IF SEGMENT-FOUND                                                     
020300       PERFORM UNTIL IX > 3                                               
020400         PERFORM IMS-GNP-WDD924                                           
020500         IF SEGMENT-FOUND                                                 
020600           MOVE LEV-TILEVBSK-INL TO WS-TILEVBSK                           
020700                                                                          
020800           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
020900           MOVE WS-TILEVBSK TO DAT-I-TIDATUM                              
021000                                                                          
021100           CALL WDATKONV USING DAT-KDDATFORM                              
021200                               DAT-I-TIDATUM                              
021300                               DAT-O-TIDATUM                              
021400                               DAT-KDSVAR                                 
021500                                                                          
021600           IF DAT-KDSVAR-OK                                               
021700             MOVE DAT-TIAAVVD TO WS-TIAAVVD                               
021800             MOVE WS-TIAAVVD  TO UT-TIAAVVD (IX)                          
021900           ELSE                                                           
022000             MOVE 'FEL SVAR FRÅN WDATKONV' TO ERRTEXT                     
022100             MOVE RKOD-ABEND-WITH-DUMP TO RKOD-ABEND                      
022200             PERFORM S99-ABEND                                            
022300           END-IF                                                         
022400         ELSE                                                             
022500           MOVE ZERO TO UT-TIAAVVD (IX)                                   
022600         END-IF                                                           
022700         ADD +1 TO IX                                                     
022800       END-PERFORM                                                        
022900       PERFORM IMS-GNP-WDD924                                             
023000     ELSE                                                                 
023100       MOVE ZERO TO UT-TIAAVVD (1)                                        
023200                    UT-TIAAVVD (2)                                        
023300                    UT-TIAAVVD (3)                                        
023400     END-IF                                                               
023500                                                                          
023600     MOVE IN-IDARTNR                 TO UT-IDARTNR                        
023700     MOVE IN-BEART                   TO UT-BEART                          
023800     MOVE IN-IDLEVNR                 TO UT-IDLEVNR                        
023900     MOVE IN-KDPRODSL                TO UT-KDPRODSL                       
024000     MOVE IN-IDFKNGRP                TO UT-IDFKNGRP                       
024100     MOVE IN-KVRADER                 TO UT-KVRADER                        
024200     MOVE IN-KVBEART-Q               TO UT-KVBEART-Q                      
024300     IF IN-PRARTNTO-LOCPREL > ZERO                                        
024400       MOVE '*'                      TO UT-TEASTRIX                       
024500     ELSE                                                                 
024600       MOVE SPACE                    TO UT-TEASTRIX                       
024700     END-IF                                                               
024800     MOVE IN-IDLANDX3                TO UT-IDLANDX3                       
024900     MOVE IN-FLSPARR                 TO UT-FLSPARR                        
025000     MOVE IN-FLSALDCH                TO UT-FLSALDCH                       
025100     MOVE IN-SUORDV                  TO UT-SUORDV                         
025200     MOVE ZERO                       TO UT-DARODAT                        
025300     .                                                                    
025400     EJECT                                                                
025500 Z-FINIT SECTION.                                                         
025600                                                                          
025700     CLOSE W44084                                                         
025800           W44087                                                         
025900     .                                                                    
026000     EJECT                                                                
026100 S01-READ-W44084  SECTION.                                                
026200     SKIP2                                                                
026300     READ W44084 INTO IN-AREA                                             
026400     AT END                                                               
026500        MOVE HIGH-VALUE TO IN-AREA                                        
026600        SET END-OF-W44084 TO TRUE                                         
026700                                                                          
026800     NOT AT END                                                           
026900        MOVE 'W44087' TO POSTSUM-FDNAMN                                   
027000        MOVE 'W44087D1' TO POSTSUM-DDNAMN2                                
027100        MOVE SPACE TO POSTSUM-TRANSTYP                                    
027200        CALL POSTSUM USING POSTSUM-PARM                                   
027300     END-READ                                                             
027400     .                                                                    
027500     EJECT                                                                
027600 S11-SKRIV-W44087 SECTION.                                                
027700                                                                          
027800     WRITE UT-POST FROM UT-AREA                                           
027900                                                                          
028000     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
028100     MOVE 'W44087'   TO POSTSUM-FDNAMN                                    
028200     MOVE 'W44087D2' TO POSTSUM-DDNAMN2                                   
028300     CALL POSTSUM USING POSTSUM-PARM                                      
028400     .                                                                    
028500     EJECT                                                                
028600 S99-ABEND SECTION.                                                       
028700                                                                          
028800     CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
029200* --- IMS SECTIONS  ---                                                   
029300 IMS-GU-WDD902 SECTION.                                                   
029400                                                                          
029500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
029600          DELIMITED BY SIZE INTO SSA1                                     
029700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
029800          DELIMITED BY SIZE INTO SSA2                                     
029900     MOVE '  GE' TO GOOD-STATUSCODES                                      
030000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
030100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSCHECK                                              
030300     .                                                                    
030400     EJECT                                                                
030500 IMS-GNP-WDD924 SECTION.                                                  
030600     MOVE 'WDD924' TO SSA1                                                
030700     MOVE '  GE' TO GOOD-STATUSCODES                                      
030800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
030900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
031000     PERFORM IMS-STATUSCHECK                                              
031100     .                                                                    
031200     EJECT                                                                
031300 IMS-STATUSCHECK SECTION.                                                 
031400     SKIP2                                                                
031500     SET STATUS-IX TO 1                                                   
031600     SEARCH GOOD-STATUS                                                   
031700       AT END                                                             
031800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031900           DELIMITED BY SIZE INTO ERRTEXT                                 
032000         DISPLAY ERRTEXT                                                  
032100         CALL FELLOG                                                      
032200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
032600     EJECT                                                                
