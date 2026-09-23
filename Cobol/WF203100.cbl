000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF203100.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/04/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PROGRAM UPDATES TABLE T01PROC                                
001000*                                                                         
001100*        PROGRAM CHANGE STATUS FROM COMING VERSION TO CURRENT             
001200*        VERSION WHEN DAUPPDAT IS LESS/EQUAL TO CURRENT DATE.             
001300*        CHANGES WILL BE DONE IN FOLLOWING TABLES:                        
001400*        - T01LSEL (DELETE, INSERT)                                       
001500*        - T01FCUS (DELETE, INSERT)                                       
001600*        - T01CUGR (DELETE, INSERT)                                       
001700*        - T01DOTY (DELETE, INSERT)                                       
001800*        - T01SECO (DELETE, INSERT)                                       
001900*        - T01RECO (DELETE, INSERT)                                       
002000*        - T01INRE (DELETE, INSERT)                                       
002100*        - T01BURE (DELETE, INSERT)                                       
002200*        - T01PAIN (DELETE, INSERT)                                       
002300*        - T01CURR (DELETE)                                               
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600 INPUT-OUTPUT SECTION.                                                    
002700 FILE-CONTROL.                                                            
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(8)   VALUE 'WF203100'.             
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600     EJECT                                                                
003700                                                                          
003800 01  ERROR-TEXT.                                                          
003900     03  FILLER                  PIC X(10)  VALUE 'ERROR-TEXT'.           
004000     03  ERROR-TEXT-STR          PIC X(72)  VALUE SPACE.                  
004100                                                                          
004200 01  WS-CURRENT-VERSION          PIC S9(3)  VALUE +001 COMP-3.            
004300 01  WS-COMING-VERSION           PIC S9(3)  VALUE +002 COMP-3.            
004400 01  WS-CURRENT-DATE             PIC X(8)   VALUE SPACE.                  
004500 01  WS-CURRENT-DATE-NUM         PIC 9(8).                                
004600 01  WS-DASTADAT-NUM             PIC 9(8).                                
004700 01  WS-DATE                     PIC 9(8).                                
004800                                                                          
004900 01  CODE-OF-TREATMENT-SW        PIC X      VALUE SPACE.                  
005000     88 CODE-OF-TREATMENT-VALID      VALUE 'D', 'P', 'W'.                 
005100                                                                          
005200*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
005500     EJECT                                                                
005600                                                                          
005700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005800 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
006000 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006200     SKIP2                                                                
006300                                                                          
006400 01  MESSAGE-CODES.                                                       
006500     03  ERR-WRONG-KEY           PIC X(3)   VALUE '022'.                  
006600     EJECT                                                                
006700                                                                          
006800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
006900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
007000                                                                          
007100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
007200 01  DB2-WS.                                                              
007300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
007400         88  CURSOR-OK                      VALUE 000.                    
007500         88  LINES-FOUND                    VALUE 000.                    
007600         88  LINES-MISSING                  VALUE 100.                    
007700         88  RESOURCE-WRONG                 VALUE 904.                    
007800                                                                          
007900     03  T01PROC-WS              PIC 9(3)   VALUE ZERO.                   
008000        88 T01PROC-OK                       VALUE 000.                    
008100        88 T01PROC-MISSING                  VALUE 100.                    
008200        88 T01PROC-ERROR                    VALUE 904.                    
008300                                                                          
008400     03  GOOD-SQLCODECODES.                                               
008500         05  GOOD-SQLCODE OCCURS 5                                        
008600             INDEXED BY SQLCODE-IX PIC 9(3).                              
008700                                                                          
008800 01  FILLER                      PIC X(16)  VALUE 'WS-AREA'.              
008900 01  WS-AREA.                                                             
009000     03 WS-IDLEGSEL              PIC X(4)   VALUE SPACE.                  
009100     03 WS-DAEXDAT               PIC X(8)   VALUE SPACE.                  
009200     03 WS-TIEXTID               PIC S9(7)  COMP-3 VALUE ZERO.            
009300                                                                          
009400     03 PROC-KDBEH               PIC X(1)   VALUE SPACE.                  
009500     EJECT                                                                
009600                                                                          
009700 01  FILLER                      PIC X(16)  VALUE 'T01PROC-AREA'.         
009800*01  -COPY T01PROC -PRE T01PROC-                                          
009900                                                                          
010000 01  FILLER                      PIC X(16)  VALUE 'T01LSEL-AREA'.         
010100*01  -COPY T01LSEL -PRE LSEL-                                             
010200                                                                          
010300 01  FILLER                      PIC X(16)  VALUE 'T01FCUS-AREA'.         
010400*01  -COPY T01FCUS -PRE FCUS-                                             
010500                                                                          
010600 01  FILLER                      PIC X(16)  VALUE 'T01CUGR-AREA'.         
010700*01  -COPY T01CUGR -PRE CUGR-                                             
010800                                                                          
010900 01  FILLER                      PIC X(16)  VALUE 'T01DOTY-AREA'.         
011000*01  -COPY T01DOTY -PRE DOTY-                                             
011100                                                                          
011200 01  FILLER                      PIC X(16)  VALUE 'T01SECO-AREA'.         
011300*01  -COPY T01SECO -PRE SECO-                                             
011400                                                                          
011500 01  FILLER                      PIC X(16)  VALUE 'T01RECO-AREA'.         
011600*01  -COPY T01RECO -PRE RECO-                                             
011700                                                                          
011800 01  FILLER                      PIC X(16)  VALUE 'T01INRE-AREA'.         
011900*01  -COPY T01INRE -PRE INRE-                                             
012000                                                                          
012100 01  FILLER                      PIC X(16)  VALUE 'T01BURE-AREA'.         
012200*01  -COPY T01BURE -PRE BURE-                                             
012300                                                                          
012400 01  FILLER                      PIC X(16)  VALUE 'T01PAIN-AREA'.         
012500*01  -COPY T01PAIN -PRE PAIN-                                             
012600                                                                          
012700 01  FILLER                      PIC X(16)  VALUE 'T01CURR-AREA'.         
012800*01  -COPY T01CURR -PRE CURR-                                             
012900     EJECT                                                                
013000                                                                          
013100     EXEC SQL INCLUDE T01PROC END-EXEC.                                   
013200                                                                          
013300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
013400                                                                          
013500     EXEC SQL INCLUDE T01FCUS END-EXEC.                                   
013600                                                                          
013700     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
013800                                                                          
013900     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
014000                                                                          
014100     EXEC SQL INCLUDE T01SECO END-EXEC.                                   
014200                                                                          
014300     EXEC SQL INCLUDE T01RECO END-EXEC.                                   
014400                                                                          
014500     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
014600                                                                          
014700     EXEC SQL INCLUDE T01BURE END-EXEC.                                   
014800                                                                          
014900     EXEC SQL INCLUDE T01PAIN END-EXEC.                                   
015000                                                                          
015100     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
015200     EJECT                                                                
015300                                                                          
015400 PROCEDURE DIVISION.                                                      
015500 MAIN SECTION.                                                            
015600     PERFORM A-INIT                                                       
015700                                                                          
015800     PERFORM DB2-OPEN-T01PROC-CRS                                         
015900     PERFORM DB2-FETCH-T01PROC-CRS                                        
016000     PERFORM UNTIL T01PROC-MISSING                                        
016100       MOVE T01PROC-KDBEH TO CODE-OF-TREATMENT-SW                         
016200       IF CODE-OF-TREATMENT-VALID                                         
016300         PERFORM B-UPD-COMING-CURRENT-VERSION                             
016400       END-IF                                                             
016500       PERFORM DB2-FETCH-T01PROC-CRS                                      
016600     END-PERFORM                                                          
016700     PERFORM DB2-CLOSE-T01PROC-CRS                                        
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200                                                                          
017300 A-INIT SECTION.                                                          
017400     INITIALIZE GOOD-SQLCODECODES                                         
017500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
017600     MOVE WS-CURRENT-DATE             TO WS-CURRENT-DATE-NUM              
017700     .                                                                    
017800                                                                          
017900 B-UPD-COMING-CURRENT-VERSION SECTION.                                    
018000     PERFORM BA-T01LSEL                                                   
018100     PERFORM BB-T01FCUS                                                   
018200     PERFORM BC-T01CUGR                                                   
018300     PERFORM BD-T01DOTY                                                   
018400     PERFORM BE-T01SECO                                                   
018500     PERFORM BF-T01RECO                                                   
018600     PERFORM BG-T01INRE                                                   
018700     PERFORM BH-T01BURE                                                   
018800     PERFORM BI-T01CURR                                                   
018900     PERFORM BJ-T01PAIN                                                   
019000     .                                                                    
019100                                                                          
019200 BA-T01LSEL SECTION.                                                      
019300     PERFORM DB2-DCL-OPN-T01LSEL-CRS                                      
019400     PERFORM DB2-FETCH-T01LSEL-CRS                                        
019500                                                                          
019600     PERFORM UNTIL LINES-MISSING                                          
019700       PERFORM DB2-DELETE-T01LSEL-CURRENT                                 
019800       PERFORM DB2-INSERT-T01LSEL-CURRENT                                 
019900       PERFORM DB2-DELETE-T01LSEL-COMING                                  
020000       PERFORM DB2-FETCH-T01LSEL-CRS                                      
020100     END-PERFORM                                                          
020200                                                                          
020300     PERFORM DB2-CLOSE-T01LSEL-CRS                                        
020400     .                                                                    
020500                                                                          
020600 BB-T01FCUS SECTION.                                                      
020700     PERFORM DB2-DCL-OPN-T01FCUS-CRS                                      
020800     PERFORM DB2-FETCH-T01FCUS-CRS                                        
020900                                                                          
021000     PERFORM UNTIL LINES-MISSING                                          
021100       PERFORM DB2-DELETE-T01FCUS-CURRENT                                 
021200       PERFORM DB2-INSERT-T01FCUS-CURRENT                                 
021300       PERFORM DB2-DELETE-T01FCUS-COMING                                  
021400       PERFORM DB2-FETCH-T01FCUS-CRS                                      
021500     END-PERFORM                                                          
021600                                                                          
021700     PERFORM DB2-CLOSE-T01FCUS-CRS                                        
021800     .                                                                    
021900                                                                          
022000 BC-T01CUGR SECTION.                                                      
022100     PERFORM DB2-DCL-OPN-T01CUGR-CRS                                      
022200     PERFORM DB2-FETCH-T01CUGR-CRS                                        
022300                                                                          
022400     PERFORM UNTIL LINES-MISSING                                          
022500       PERFORM DB2-DELETE-T01CUGR-CURRENT                                 
022600       PERFORM DB2-INSERT-T01CUGR-CURRENT                                 
022700       PERFORM DB2-DELETE-T01CUGR-COMING                                  
022800       PERFORM DB2-FETCH-T01CUGR-CRS                                      
022900     END-PERFORM                                                          
023000                                                                          
023100     PERFORM DB2-CLOSE-T01CUGR-CRS                                        
023200     .                                                                    
023300                                                                          
023400 BD-T01DOTY SECTION.                                                      
023500     PERFORM DB2-DCL-OPN-T01DOTY-CRS                                      
023600     PERFORM DB2-FETCH-T01DOTY-CRS                                        
023700                                                                          
023800     PERFORM UNTIL LINES-MISSING                                          
023900       PERFORM DB2-DELETE-T01DOTY-CURRENT                                 
024000       PERFORM DB2-INSERT-T01DOTY-CURRENT                                 
024100       PERFORM DB2-DELETE-T01DOTY-COMING                                  
024200       PERFORM DB2-FETCH-T01DOTY-CRS                                      
024300     END-PERFORM                                                          
024400                                                                          
024500     PERFORM DB2-CLOSE-T01DOTY-CRS                                        
024600     .                                                                    
024700                                                                          
024800 BE-T01SECO SECTION.                                                      
024900     PERFORM DB2-DCL-OPN-T01SECO-CRS                                      
025000     PERFORM DB2-FETCH-T01SECO-CRS                                        
025100                                                                          
025200     PERFORM UNTIL LINES-MISSING                                          
025300       PERFORM DB2-DELETE-T01SECO-CURRENT                                 
025400       PERFORM DB2-INSERT-T01SECO-CURRENT                                 
025500       PERFORM DB2-DELETE-T01SECO-COMING                                  
025600       PERFORM DB2-FETCH-T01SECO-CRS                                      
025700     END-PERFORM                                                          
025800                                                                          
025900     PERFORM DB2-CLOSE-T01SECO-CRS                                        
026000     .                                                                    
026100                                                                          
026200 BF-T01RECO SECTION.                                                      
026300     PERFORM DB2-DCL-OPN-T01RECO-CRS                                      
026400     PERFORM DB2-FETCH-T01RECO-CRS                                        
026500                                                                          
026600     PERFORM UNTIL LINES-MISSING                                          
026700       PERFORM DB2-DELETE-T01RECO-CURRENT                                 
026800       PERFORM DB2-INSERT-T01RECO-CURRENT                                 
026900       PERFORM DB2-DELETE-T01RECO-COMING                                  
027000       PERFORM DB2-FETCH-T01RECO-CRS                                      
027100     END-PERFORM                                                          
027200                                                                          
027300     PERFORM DB2-CLOSE-T01RECO-CRS                                        
027400     .                                                                    
027500                                                                          
027600 BG-T01INRE SECTION.                                                      
027700     PERFORM DB2-DCL-OPN-T01INRE-CRS                                      
027800     PERFORM DB2-FETCH-T01INRE-CRS                                        
027900                                                                          
028000     PERFORM UNTIL LINES-MISSING                                          
028100       PERFORM DB2-DELETE-T01INRE-CURRENT                                 
028200       PERFORM DB2-INSERT-T01INRE-CURRENT                                 
028300       PERFORM DB2-DELETE-T01INRE-COMING                                  
028400       PERFORM DB2-FETCH-T01INRE-CRS                                      
028500     END-PERFORM                                                          
028600                                                                          
028700     PERFORM DB2-CLOSE-T01INRE-CRS                                        
028800     .                                                                    
028900                                                                          
029000 BH-T01BURE SECTION.                                                      
029100     PERFORM DB2-DCL-OPN-T01BURE-CRS                                      
029200     PERFORM DB2-FETCH-T01BURE-CRS                                        
029300                                                                          
029400     PERFORM UNTIL LINES-MISSING                                          
029500       PERFORM DB2-DELETE-T01BURE-CURRENT                                 
029600       PERFORM DB2-INSERT-T01BURE-CURRENT                                 
029700       PERFORM DB2-DELETE-T01BURE-COMING                                  
029800       PERFORM DB2-FETCH-T01BURE-CRS                                      
029900     END-PERFORM                                                          
030000                                                                          
030100     PERFORM DB2-CLOSE-T01BURE-CRS                                        
030200     .                                                                    
030300                                                                          
030400 BI-T01CURR SECTION.                                                      
030500     PERFORM DB2-DCL-OPN-T01CURR-CRS                                      
030600     PERFORM DB2-FETCH-T01CURR-CRS                                        
030700                                                                          
030800     PERFORM UNTIL LINES-MISSING                                          
030900       MOVE CURR-DASTADAT TO WS-DASTADAT-NUM                              
031000       COMPUTE WS-DATE = WS-CURRENT-DATE-NUM -                            
031100                         WS-DASTADAT-NUM                                  
031200       END-COMPUTE                                                        
031300       IF WS-DATE > 10000                                                 
031400         IF LSEL-FLCURRCL = 'J'                                           
031500           PERFORM DB2-DELETE-T01CURR                                     
031600         END-IF                                                           
031700       END-IF                                                             
031800       PERFORM DB2-FETCH-T01CURR-CRS                                      
031900     END-PERFORM                                                          
032000                                                                          
032100     PERFORM DB2-CLOSE-T01CURR-CRS                                        
032200     .                                                                    
032300                                                                          
032400 BJ-T01PAIN SECTION.                                                      
032500     PERFORM DB2-DCL-OPN-T01PAIN-CRS                                      
032600     PERFORM DB2-FETCH-T01PAIN-CRS                                        
032700                                                                          
032800     PERFORM UNTIL LINES-MISSING                                          
032900       PERFORM DB2-DELETE-T01PAIN-CURRENT                                 
033000       PERFORM DB2-INSERT-T01PAIN-CURRENT                                 
033100       PERFORM DB2-DELETE-T01PAIN-COMING                                  
033200       PERFORM DB2-FETCH-T01PAIN-CRS                                      
033300     END-PERFORM                                                          
033400                                                                          
033500     PERFORM DB2-CLOSE-T01PAIN-CRS                                        
033600     .                                                                    
033700                                                                          
033800*   --- DB2 SECTIONS                                                      
033900*                                                                         
034000 DB2-OPEN-T01PROC-CRS SECTION.                                            
034100     EXEC SQL DECLARE T01PROC-CRS CURSOR FOR                              
034200     SELECT   IDSYSTEM,                                                   
034300              IDLEGSEL,                                                   
034400              KDBEH                                                       
034500                                                                          
034600     FROM     T01PROC                                                     
034700                                                                          
034800     END-EXEC                                                             
034900                                                                          
035000     EXEC SQL OPEN T01PROC-CRS                                            
035100     END-EXEC                                                             
035200                                                                          
035300     MOVE 000            TO GOOD-SQLCODECODES                             
035400     MOVE SQLCODE        TO SQLCODE-WS                                    
035500     PERFORM DB2-STATUS-CHECK                                             
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900 DB2-FETCH-T01PROC-CRS SECTION.                                           
036000     EXEC SQL FETCH T01PROC-CRS INTO                                      
036100            :T01PROC-IDSYSTEM,                                            
036200            :T01PROC-IDLEGSEL,                                            
036300            :T01PROC-KDBEH                                                
036400     END-EXEC                                                             
036500                                                                          
036600     MOVE 000100         TO GOOD-SQLCODECODES                             
036700     MOVE SQLCODE        TO SQLCODE-WS                                    
036800                            T01PROC-WS                                    
036900     PERFORM DB2-STATUS-CHECK                                             
037000     .                                                                    
037100     EJECT                                                                
037200                                                                          
037300 DB2-CLOSE-T01PROC-CRS SECTION.                                           
037400     EXEC SQL CLOSE T01PROC-CRS                                           
037500     END-EXEC                                                             
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 DB2-DCL-OPN-T01LSEL-CRS SECTION.                                         
038000     MOVE 000100 TO GOOD-SQLCODECODES                                     
038100                                                                          
038200     EXEC SQL                                                             
038300         DECLARE LSEL-CRS CURSOR WITH HOLD FOR                            
038400            SELECT  IDLEGSEL                                              
038500                  , KDSTATUS                                              
038600                  , BELEGRAD_1                                            
038700                  , BELEGRAD_2                                            
038800                  , ADLEG_STREET                                          
038900                  , ADLEG_BOX                                             
039000                  , ADLEG_PCODE                                           
039100                  , ADLEG_CITY                                            
039200                  , IDLANDX3                                              
039300                  , IDTFN                                                 
039400                  , IDTFX                                                 
039500                  , IDMAIL                                                
039600                  , BECONT                                                
039700                  , IDVAT                                                 
039800                  , IDBG                                                  
039900                  , IDPG                                                  
040000                  , KDINVFRQ                                              
040100                  , FLSLUT                                                
040200                  , DAREGDAT                                              
040300                  , DAUPPDAT                                              
040400                  , IDUSER                                                
040500                  , FLCURRCL                                              
040510                  , KDVALISO                                              
040520                  , KDTRADP                                               
040600                                                                          
040700           FROM     T01LSEL                                               
040800                                                                          
040900           WHERE    KDSTATUS = :WS-COMING-VERSION                         
041000           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
041100                                                                          
041200     END-EXEC                                                             
041300                                                                          
041400     MOVE 000100 TO GOOD-SQLCODECODES                                     
041500                                                                          
041600     EXEC SQL                                                             
041700        OPEN LSEL-CRS                                                     
041800     END-EXEC                                                             
041900                                                                          
042000     MOVE SQLCODE TO SQLCODE-WS                                           
042100     PERFORM DB2-STATUS-CHECK                                             
042200     .                                                                    
042300                                                                          
042400 DB2-FETCH-T01LSEL-CRS SECTION.                                           
042500     MOVE 000100  TO GOOD-SQLCODECODES                                    
042600                                                                          
042700     EXEC SQL                                                             
042800         FETCH LSEL-CRS                                                   
042900                                                                          
043000         INTO :LSEL-IDLEGSEL                                              
043100            , :LSEL-KDSTATUS                                              
043200            , :LSEL-BELEGRAD-1                                            
043300            , :LSEL-BELEGRAD-2                                            
043400            , :LSEL-ADLEG-STREET                                          
043500            , :LSEL-ADLEG-BOX                                             
043600            , :LSEL-ADLEG-PCODE                                           
043700            , :LSEL-ADLEG-CITY                                            
043800            , :LSEL-IDLANDX3                                              
043900            , :LSEL-IDTFN                                                 
044000            , :LSEL-IDTFX                                                 
044100            , :LSEL-IDMAIL                                                
044200            , :LSEL-BECONT                                                
044300            , :LSEL-IDVAT                                                 
044400            , :LSEL-IDBG                                                  
044500            , :LSEL-IDPG                                                  
044600            , :LSEL-KDINVFRQ                                              
044700            , :LSEL-FLSLUT                                                
044800            , :LSEL-DAREGDAT                                              
044900            , :LSEL-DAUPPDAT                                              
045000            , :LSEL-IDUSER                                                
045100            , :LSEL-FLCURRCL                                              
045110            , :LSEL-KDVALISO                                              
045120            , :LSEL-KDTRADP                                               
045200     END-EXEC                                                             
045300                                                                          
045400     MOVE SQLCODE TO SQLCODE-WS                                           
045500     PERFORM DB2-STATUS-CHECK                                             
045600     .                                                                    
045700                                                                          
045800 DB2-DELETE-T01LSEL-CURRENT SECTION.                                      
045900     MOVE 000   TO GOOD-SQLCODECODES                                      
046000                                                                          
046100     EXEC SQL                                                             
046200         DELETE FROM T01LSEL                                              
046300                                                                          
046400         WHERE  IDLEGSEL = :LSEL-IDLEGSEL                                 
046500         AND    KDSTATUS = :WS-CURRENT-VERSION                            
046600     END-EXEC                                                             
046700                                                                          
046800     MOVE SQLCODE TO SQLCODE-WS                                           
046900     PERFORM DB2-STATUS-CHECK                                             
047000     .                                                                    
047100                                                                          
047200 DB2-INSERT-T01LSEL-CURRENT SECTION.                                      
047300     MOVE 000   TO GOOD-SQLCODECODES                                      
047400                                                                          
047500     EXEC SQL                                                             
047600       INSERT INTO T01LSEL                                                
047700          (IDLEGSEL                                                       
047800          ,KDSTATUS                                                       
047900          ,BELEGRAD_1                                                     
048000          ,BELEGRAD_2                                                     
048100          ,ADLEG_STREET                                                   
048200          ,ADLEG_BOX                                                      
048300          ,ADLEG_PCODE                                                    
048400          ,ADLEG_CITY                                                     
048500          ,IDLANDX3                                                       
048600          ,IDTFN                                                          
048700          ,IDTFX                                                          
048800          ,IDMAIL                                                         
048900          ,BECONT                                                         
049000          ,IDVAT                                                          
049100          ,IDBG                                                           
049200          ,IDPG                                                           
049300          ,KDINVFRQ                                                       
049400          ,FLSLUT                                                         
049500          ,DAREGDAT                                                       
049600          ,DAUPPDAT                                                       
049700          ,IDUSER                                                         
049800          ,FLVATUPD                                                       
049900          ,FLCUSUPD                                                       
050000          ,FLCURRCL                                                       
050100          ,FLINTODO                                                       
050200          ,KDVALISO                                                       
050210          ,KDTRADP)                                                       
050300       VALUES                                                             
050400          (:LSEL-IDLEGSEL                                                 
050500          ,:WS-CURRENT-VERSION                                            
050600          ,:LSEL-BELEGRAD-1                                               
050700          ,:LSEL-BELEGRAD-2                                               
050800          ,:LSEL-ADLEG-STREET                                             
050900          ,:LSEL-ADLEG-BOX                                                
051000          ,:LSEL-ADLEG-PCODE                                              
051100          ,:LSEL-ADLEG-CITY                                               
051200          ,:LSEL-IDLANDX3                                                 
051300          ,:LSEL-IDTFN                                                    
051400          ,:LSEL-IDTFX                                                    
051500          ,:LSEL-IDMAIL                                                   
051600          ,:LSEL-BECONT                                                   
051700          ,:LSEL-IDVAT                                                    
051800          ,:LSEL-IDBG                                                     
051900          ,:LSEL-IDPG                                                     
052000          ,:LSEL-KDINVFRQ                                                 
052100          ,:LSEL-FLSLUT                                                   
052200          ,:LSEL-DAREGDAT                                                 
052300          ,:LSEL-DAUPPDAT                                                 
052400          ,:LSEL-IDUSER                                                   
052500          ,:LSEL-FLVATUPD                                                 
052600          ,:LSEL-FLCUSUPD                                                 
052700          ,:LSEL-FLCURRCL                                                 
052800          ,:LSEL-FLINTODO                                                 
052900          ,:LSEL-KDVALISO                                                 
052910          ,:LSEL-KDTRADP)                                                 
053000     END-EXEC                                                             
053100                                                                          
053200     MOVE SQLCODE TO SQLCODE-WS                                           
053300     PERFORM DB2-STATUS-CHECK                                             
053400     .                                                                    
053500                                                                          
053600 DB2-DELETE-T01LSEL-COMING SECTION.                                       
053700     MOVE 000   TO GOOD-SQLCODECODES                                      
053800                                                                          
053900     EXEC SQL                                                             
054000         DELETE FROM T01LSEL                                              
054100                                                                          
054200         WHERE  IDLEGSEL  = :LSEL-IDLEGSEL                                
054300         AND    KDSTATUS  = :WS-COMING-VERSION                            
054400     END-EXEC                                                             
054500                                                                          
054600     MOVE SQLCODE TO SQLCODE-WS                                           
054700     PERFORM DB2-STATUS-CHECK                                             
054800     .                                                                    
054900                                                                          
055000 DB2-CLOSE-T01LSEL-CRS SECTION.                                           
055100                                                                          
055200     EXEC SQL                                                             
055300        CLOSE LSEL-CRS                                                    
055400     END-EXEC                                                             
055500     .                                                                    
055600                                                                          
055700 DB2-DCL-OPN-T01FCUS-CRS SECTION.                                         
055800     MOVE 000100 TO GOOD-SQLCODECODES                                     
055900                                                                          
056000     EXEC SQL                                                             
056100         DECLARE FCUS-CRS CURSOR WITH HOLD FOR                            
056200            SELECT  IDLEGSEL                                              
056300                  , IDPARTNR                                              
056400                  , KDSTATUS                                              
056500                  , IDALPHA                                               
056600                  , BEBET_NAME1                                           
056700                  , BEBET_NAME2                                           
056800                  , BEBET_NAME3                                           
056900                  , BEBET_NAME4                                           
057000                  , ADBET_STREET                                          
057100                  , ADBET_BOX                                             
057200                  , ADBET_CITY                                            
057300                  , ADBET_PCODE                                           
057400                  , IDLANDX3                                              
057500                  , IDSPRAK                                               
057600                  , IDTFN                                                 
057700                  , IDTFX                                                 
057800                  , IDMAIL                                                
057900                  , IDLEVNR_AP                                            
058000                  , IDVAT                                                 
058100                  , KDVALISO                                              
058200                  , KDTRADP                                               
058300                  , KDBETALV                                              
058400                  , KDKREDSP                                              
058500                  , KDPARTTY                                              
058600                  , KDPARTGR                                              
058700                  , DAREGDAT                                              
058800                  , DAUPPDAT                                              
058900                  , DADELDAT                                              
059000                  , IDUSER                                                
059100                  , FLRATE                                                
059200                  , FLLOCCUR                                              
059210                  , FLFINFIL                                              
059220                  , FLSAPBLK                                              
059230                  , FLDECIMAL                                             
059240                  , FLCURINF                                              
059250                  , FLCURRND                                              
059260                  , KDVALTYP                                              
059270                  , FLDIRVAT                                              
059300                                                                          
059400           FROM     T01FCUS                                               
059500                                                                          
059600           WHERE    KDSTATUS = :WS-COMING-VERSION                         
059700           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
059800                                                                          
059900     END-EXEC                                                             
060000                                                                          
060100     MOVE 000100 TO GOOD-SQLCODECODES                                     
060200                                                                          
060300     EXEC SQL                                                             
060400        OPEN FCUS-CRS                                                     
060500     END-EXEC                                                             
060600                                                                          
060700     MOVE SQLCODE TO SQLCODE-WS                                           
060800     PERFORM DB2-STATUS-CHECK                                             
060900     .                                                                    
061000                                                                          
061100 DB2-FETCH-T01FCUS-CRS SECTION.                                           
061200     MOVE 000100  TO GOOD-SQLCODECODES                                    
061300                                                                          
061400     EXEC SQL                                                             
061500         FETCH FCUS-CRS                                                   
061600                                                                          
061700         INTO :FCUS-IDLEGSEL                                              
061800            , :FCUS-IDPARTNR                                              
061900            , :FCUS-KDSTATUS                                              
062000            , :FCUS-IDALPHA                                               
062100            , :FCUS-BEBET-NAME1                                           
062200            , :FCUS-BEBET-NAME2                                           
062300            , :FCUS-BEBET-NAME3                                           
062400            , :FCUS-BEBET-NAME4                                           
062500            , :FCUS-ADBET-STREET                                          
062600            , :FCUS-ADBET-BOX                                             
062700            , :FCUS-ADBET-CITY                                            
062800            , :FCUS-ADBET-PCODE                                           
062900            , :FCUS-IDLANDX3                                              
063000            , :FCUS-IDSPRAK                                               
063100            , :FCUS-IDTFN                                                 
063200            , :FCUS-IDTFX                                                 
063300            , :FCUS-IDMAIL                                                
063400            , :FCUS-IDLEVNR-AP                                            
063500            , :FCUS-IDVAT                                                 
063600            , :FCUS-KDVALISO                                              
063700            , :FCUS-KDTRADP                                               
063800            , :FCUS-KDBETALV                                              
063900            , :FCUS-KDKREDSP                                              
064000            , :FCUS-KDPARTTY                                              
064100            , :FCUS-KDPARTGR                                              
064200            , :FCUS-DAREGDAT                                              
064300            , :FCUS-DAUPPDAT                                              
064400            , :FCUS-DADELDAT                                              
064500            , :FCUS-IDUSER                                                
064600            , :FCUS-FLRATE                                                
064610            , :FCUS-FLLOCCUR                                              
064620            , :FCUS-FLFINFIL                                              
064630            , :FCUS-FLSAPBLK                                              
064640            , :FCUS-FLDECIMAL                                             
064650            , :FCUS-FLCURINF                                              
064660            , :FCUS-FLCURRND                                              
064670            , :FCUS-KDVALTYP                                              
064680            , :FCUS-FLDIRVAT                                              
064700     END-EXEC                                                             
064800                                                                          
064900     MOVE SQLCODE TO SQLCODE-WS                                           
065000     PERFORM DB2-STATUS-CHECK                                             
065100     .                                                                    
065200                                                                          
065300 DB2-DELETE-T01FCUS-CURRENT SECTION.                                      
065400     MOVE 000   TO GOOD-SQLCODECODES                                      
065500                                                                          
065600     EXEC SQL                                                             
065700         DELETE FROM T01FCUS                                              
065800                                                                          
065900         WHERE  IDLEGSEL = :FCUS-IDLEGSEL                                 
066000         AND    IDPARTNR = :FCUS-IDPARTNR                                 
066100         AND    KDSTATUS = :WS-CURRENT-VERSION                            
066200     END-EXEC                                                             
066300                                                                          
066400     MOVE SQLCODE TO SQLCODE-WS                                           
066500     PERFORM DB2-STATUS-CHECK                                             
066600     .                                                                    
066700                                                                          
066800 DB2-INSERT-T01FCUS-CURRENT SECTION.                                      
066900     MOVE 000   TO GOOD-SQLCODECODES                                      
067000                                                                          
067100     EXEC SQL                                                             
067200       INSERT INTO T01FCUS                                                
067300          (IDLEGSEL                                                       
067400          ,IDPARTNR                                                       
067500          ,KDSTATUS                                                       
067600          ,IDALPHA                                                        
067700          ,BEBET_NAME1                                                    
067800          ,BEBET_NAME2                                                    
067900          ,BEBET_NAME3                                                    
068000          ,BEBET_NAME4                                                    
068100          ,ADBET_STREET                                                   
068200          ,ADBET_BOX                                                      
068300          ,ADBET_PCODE                                                    
068400          ,ADBET_CITY                                                     
068500          ,IDLANDX3                                                       
068600          ,IDSPRAK                                                        
068700          ,IDTFN                                                          
068800          ,IDTFX                                                          
068900          ,IDMAIL                                                         
069000          ,IDLEVNR_AP                                                     
069100          ,IDVAT                                                          
069200          ,KDVALISO                                                       
069300          ,KDTRADP                                                        
069400          ,KDBETALV                                                       
069500          ,KDKREDSP                                                       
069600          ,KDPARTTY                                                       
069700          ,KDPARTGR                                                       
069800          ,DAREGDAT                                                       
069900          ,DAUPPDAT                                                       
070000          ,DADELDAT                                                       
070100          ,IDUSER                                                         
070200          ,FLRATE                                                         
070300          ,FLLOCCUR                                                       
070400          ,FLFINFIL                                                       
070410          ,FLSAPBLK                                                       
070420          ,FLDECIMAL                                                      
070430          ,FLCURINF                                                       
070440          ,FLCURRND                                                       
070450          ,KDVALTYP                                                       
070460          ,FLDIRVAT)                                                      
070500       VALUES                                                             
070600          (:FCUS-IDLEGSEL                                                 
070700          ,:FCUS-IDPARTNR                                                 
070800          ,:WS-CURRENT-VERSION                                            
070900          ,:FCUS-IDALPHA                                                  
071000          ,:FCUS-BEBET-NAME1                                              
071100          ,:FCUS-BEBET-NAME2                                              
071200          ,:FCUS-BEBET-NAME3                                              
071300          ,:FCUS-BEBET-NAME4                                              
071400          ,:FCUS-ADBET-STREET                                             
071500          ,:FCUS-ADBET-BOX                                                
071600          ,:FCUS-ADBET-PCODE                                              
071700          ,:FCUS-ADBET-CITY                                               
071800          ,:FCUS-IDLANDX3                                                 
071900          ,:FCUS-IDSPRAK                                                  
072000          ,:FCUS-IDTFN                                                    
072100          ,:FCUS-IDTFX                                                    
072200          ,:FCUS-IDMAIL                                                   
072300          ,:FCUS-IDLEVNR-AP                                               
072400          ,:FCUS-IDVAT                                                    
072500          ,:FCUS-KDVALISO                                                 
072600          ,:FCUS-KDTRADP                                                  
072700          ,:FCUS-KDBETALV                                                 
072800          ,:FCUS-KDKREDSP                                                 
072900          ,:FCUS-KDPARTTY                                                 
073000          ,:FCUS-KDPARTGR                                                 
073100          ,:FCUS-DAREGDAT                                                 
073200          ,:FCUS-DAUPPDAT                                                 
073300          ,:FCUS-DADELDAT                                                 
073400          ,:FCUS-IDUSER                                                   
073500          ,:FCUS-FLRATE                                                   
073600          ,:FCUS-FLLOCCUR                                                 
073700          ,:FCUS-FLFINFIL                                                 
073710          ,:FCUS-FLSAPBLK                                                 
073720          ,:FCUS-FLDECIMAL                                                
073730          ,:FCUS-FLCURINF                                                 
073740          ,:FCUS-FLCURRND                                                 
073750          ,:FCUS-KDVALTYP                                                 
073760          ,:FCUS-FLDIRVAT)                                                
073800     END-EXEC                                                             
073900                                                                          
074000     MOVE SQLCODE TO SQLCODE-WS                                           
074100     PERFORM DB2-STATUS-CHECK                                             
074200     .                                                                    
074300                                                                          
074400 DB2-DELETE-T01FCUS-COMING SECTION.                                       
074500     MOVE 000   TO GOOD-SQLCODECODES                                      
074600                                                                          
074700     EXEC SQL                                                             
074800         DELETE FROM T01FCUS                                              
074900                                                                          
075000         WHERE  IDLEGSEL  = :FCUS-IDLEGSEL                                
075100         AND    IDPARTNR  = :FCUS-IDPARTNR                                
075200         AND    KDSTATUS  = :WS-COMING-VERSION                            
075300     END-EXEC                                                             
075400                                                                          
075500     MOVE SQLCODE TO SQLCODE-WS                                           
075600     PERFORM DB2-STATUS-CHECK                                             
075700     .                                                                    
075800                                                                          
075900 DB2-CLOSE-T01FCUS-CRS SECTION.                                           
076000     EXEC SQL                                                             
076100        CLOSE FCUS-CRS                                                    
076200     END-EXEC                                                             
076300     .                                                                    
076400                                                                          
076500 DB2-DCL-OPN-T01CUGR-CRS SECTION.                                         
076600     MOVE 000100 TO GOOD-SQLCODECODES                                     
076700                                                                          
076800     EXEC SQL                                                             
076900         DECLARE CUGR-CRS CURSOR WITH HOLD FOR                            
077000            SELECT  IDLEGSEL                                              
077100                  , KDPARTTY                                              
077200                  , KDPARTGR                                              
077300                  , KDSTATUS                                              
077400                  , KDINVFRQ                                              
077500                  , DAREGDAT                                              
077600                  , DAUPPDAT                                              
077700                  , DADELDAT                                              
077800                  , IDUSER                                                
077900                                                                          
078000           FROM     T01CUGR                                               
078100                                                                          
078200           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
078300           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
078400     END-EXEC                                                             
078500                                                                          
078600     MOVE 000100 TO GOOD-SQLCODECODES                                     
078700                                                                          
078800     EXEC SQL                                                             
078900        OPEN CUGR-CRS                                                     
079000     END-EXEC                                                             
079100                                                                          
079200     MOVE SQLCODE TO SQLCODE-WS                                           
079300     PERFORM DB2-STATUS-CHECK                                             
079400     .                                                                    
079500                                                                          
079600 DB2-FETCH-T01CUGR-CRS SECTION.                                           
079700     MOVE 000100  TO GOOD-SQLCODECODES                                    
079800                                                                          
079900     EXEC SQL                                                             
080000         FETCH CUGR-CRS                                                   
080100                                                                          
080200         INTO :CUGR-IDLEGSEL                                              
080300            , :CUGR-KDPARTTY                                              
080400            , :CUGR-KDPARTGR                                              
080500            , :CUGR-KDSTATUS                                              
080600            , :CUGR-KDINVFRQ                                              
080700            , :CUGR-DAREGDAT                                              
080800            , :CUGR-DAUPPDAT                                              
080900            , :CUGR-DADELDAT                                              
081000            , :CUGR-IDUSER                                                
081100     END-EXEC                                                             
081200                                                                          
081300     MOVE SQLCODE TO SQLCODE-WS                                           
081400     PERFORM DB2-STATUS-CHECK                                             
081500     .                                                                    
081600                                                                          
081700 DB2-DELETE-T01CUGR-CURRENT SECTION.                                      
081800     MOVE 000   TO GOOD-SQLCODECODES                                      
081900                                                                          
082000     EXEC SQL                                                             
082100         DELETE FROM T01CUGR                                              
082200                                                                          
082300         WHERE  IDLEGSEL = :CUGR-IDLEGSEL                                 
082400         AND    KDPARTTY = :CUGR-KDPARTTY                                 
082500         AND    KDPARTGR = :CUGR-KDPARTGR                                 
082600         AND    KDSTATUS = :WS-CURRENT-VERSION                            
082700     END-EXEC                                                             
082800                                                                          
082900     MOVE SQLCODE TO SQLCODE-WS                                           
083000     PERFORM DB2-STATUS-CHECK                                             
083100     .                                                                    
083200                                                                          
083300 DB2-INSERT-T01CUGR-CURRENT SECTION.                                      
083400     MOVE 000   TO GOOD-SQLCODECODES                                      
083500                                                                          
083600     EXEC SQL                                                             
083700       INSERT INTO T01CUGR                                                
083800          (IDLEGSEL                                                       
083900          ,KDPARTTY                                                       
084000          ,KDPARTGR                                                       
084100          ,KDSTATUS                                                       
084200          ,KDINVFRQ                                                       
084300          ,DAREGDAT                                                       
084400          ,DAUPPDAT                                                       
084500          ,DADELDAT                                                       
084600          ,IDUSER                                                         
084700          ,FLVAT)                                                         
084800       VALUES                                                             
084900          (:CUGR-IDLEGSEL                                                 
085000          ,:CUGR-KDPARTTY                                                 
085100          ,:CUGR-KDPARTGR                                                 
085200          ,:WS-CURRENT-VERSION                                            
085300          ,:CUGR-KDINVFRQ                                                 
085400          ,:CUGR-DAREGDAT                                                 
085500          ,:CUGR-DAUPPDAT                                                 
085600          ,:CUGR-DADELDAT                                                 
085700          ,:CUGR-IDUSER                                                   
085800          ,:CUGR-FLVAT)                                                   
085900     END-EXEC                                                             
086000                                                                          
086100     MOVE SQLCODE TO SQLCODE-WS                                           
086200     PERFORM DB2-STATUS-CHECK                                             
086300     .                                                                    
086400                                                                          
086500 DB2-DELETE-T01CUGR-COMING SECTION.                                       
086600     MOVE 000   TO GOOD-SQLCODECODES                                      
086700                                                                          
086800     EXEC SQL                                                             
086900         DELETE FROM T01CUGR                                              
087000                                                                          
087100         WHERE  IDLEGSEL  = :CUGR-IDLEGSEL                                
087200         AND    KDPARTTY  = :CUGR-KDPARTTY                                
087300         AND    KDPARTGR  = :CUGR-KDPARTGR                                
087400         AND    KDSTATUS  = :WS-COMING-VERSION                            
087500     END-EXEC                                                             
087600                                                                          
087700     MOVE SQLCODE TO SQLCODE-WS                                           
087800     PERFORM DB2-STATUS-CHECK                                             
087900     .                                                                    
088000                                                                          
088100 DB2-CLOSE-T01CUGR-CRS SECTION.                                           
088200     EXEC SQL                                                             
088300        CLOSE CUGR-CRS                                                    
088400     END-EXEC                                                             
088500     .                                                                    
088600                                                                          
088700 DB2-DCL-OPN-T01DOTY-CRS SECTION.                                         
088800     MOVE 000100 TO GOOD-SQLCODECODES                                     
088900                                                                          
089000     EXEC SQL                                                             
089100         DECLARE DOTY-CRS CURSOR WITH HOLD FOR                            
089200            SELECT  IDLEGSEL                                              
089300                  , KDFINDOC                                              
089400                  , KDSTATUS                                              
089500                  , BEFINDOC                                              
089600                  , DAREGDAT                                              
089700                  , DAUPPDAT                                              
089800                  , DADELDAT                                              
089900                  , IDUSER                                                
090000                  , FLAP                                                  
090100                  , FLAR                                                  
090200                  , FLGL                                                  
090300                  , FLINTREP                                              
090400                  , FLVATREP                                              
090500                  , FLCUSREP                                              
090600                                                                          
090700           FROM     T01DOTY                                               
090800                                                                          
090900           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
091000           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
091100     END-EXEC                                                             
091200                                                                          
091300     MOVE 000100 TO GOOD-SQLCODECODES                                     
091400                                                                          
091500     EXEC SQL                                                             
091600        OPEN DOTY-CRS                                                     
091700     END-EXEC                                                             
091800                                                                          
091900     MOVE SQLCODE TO SQLCODE-WS                                           
092000     PERFORM DB2-STATUS-CHECK                                             
092100     .                                                                    
092200                                                                          
092300 DB2-FETCH-T01DOTY-CRS SECTION.                                           
092400     MOVE 000100  TO GOOD-SQLCODECODES                                    
092500                                                                          
092600     EXEC SQL                                                             
092700         FETCH DOTY-CRS                                                   
092800                                                                          
092900         INTO :DOTY-IDLEGSEL                                              
093000            , :DOTY-KDFINDOC                                              
093100            , :DOTY-KDSTATUS                                              
093200            , :DOTY-BEFINDOC                                              
093300            , :DOTY-DAREGDAT                                              
093400            , :DOTY-DAUPPDAT                                              
093500            , :DOTY-DADELDAT                                              
093600            , :DOTY-IDUSER                                                
093700            , :DOTY-FLAP                                                  
093800            , :DOTY-FLAR                                                  
093900            , :DOTY-FLGL                                                  
094000            , :DOTY-FLINTREP                                              
094100            , :DOTY-FLVATREP                                              
094200            , :DOTY-FLCUSREP                                              
094300     END-EXEC                                                             
094400                                                                          
094500     MOVE SQLCODE TO SQLCODE-WS                                           
094600     PERFORM DB2-STATUS-CHECK                                             
094700     .                                                                    
094800                                                                          
094900 DB2-DELETE-T01DOTY-CURRENT SECTION.                                      
095000     MOVE 000   TO GOOD-SQLCODECODES                                      
095100                                                                          
095200     EXEC SQL                                                             
095300         DELETE FROM T01DOTY                                              
095400                                                                          
095500         WHERE  IDLEGSEL = :DOTY-IDLEGSEL                                 
095600         AND    KDFINDOC = :DOTY-KDFINDOC                                 
095700         AND    KDSTATUS = :WS-CURRENT-VERSION                            
095800     END-EXEC                                                             
095900                                                                          
096000     MOVE SQLCODE TO SQLCODE-WS                                           
096100     PERFORM DB2-STATUS-CHECK                                             
096200     .                                                                    
096300                                                                          
096400 DB2-INSERT-T01DOTY-CURRENT SECTION.                                      
096500     MOVE 000   TO GOOD-SQLCODECODES                                      
096600                                                                          
096700     EXEC SQL                                                             
096800       INSERT INTO T01DOTY                                                
096900          (IDLEGSEL                                                       
097000          ,KDFINDOC                                                       
097100          ,KDSTATUS                                                       
097200          ,BEFINDOC                                                       
097300          ,DAREGDAT                                                       
097400          ,DAUPPDAT                                                       
097500          ,DADELDAT                                                       
097600          ,IDUSER                                                         
097700          ,FLAP                                                           
097800          ,FLAR                                                           
097900          ,FLGL                                                           
098000          ,FLINTREP                                                       
098100          ,FLVATREP                                                       
098200          ,FLCUSREP)                                                      
098300       VALUES                                                             
098400          (:DOTY-IDLEGSEL                                                 
098500          ,:DOTY-KDFINDOC                                                 
098600          ,:WS-CURRENT-VERSION                                            
098700          ,:DOTY-BEFINDOC                                                 
098800          ,:DOTY-DAREGDAT                                                 
098900          ,:DOTY-DAUPPDAT                                                 
099000          ,:DOTY-DADELDAT                                                 
099100          ,:DOTY-IDUSER                                                   
099200          ,:DOTY-FLAP                                                     
099300          ,:DOTY-FLAR                                                     
099400          ,:DOTY-FLGL                                                     
099500          ,:DOTY-FLINTREP                                                 
099600          ,:DOTY-FLVATREP                                                 
099700          ,:DOTY-FLCUSREP)                                                
099800     END-EXEC                                                             
099900                                                                          
100000     MOVE SQLCODE TO SQLCODE-WS                                           
100100     PERFORM DB2-STATUS-CHECK                                             
100200     .                                                                    
100300                                                                          
100400 DB2-DELETE-T01DOTY-COMING SECTION.                                       
100500     MOVE 000   TO GOOD-SQLCODECODES                                      
100600                                                                          
100700     EXEC SQL                                                             
100800         DELETE FROM T01DOTY                                              
100900                                                                          
101000         WHERE  IDLEGSEL  = :DOTY-IDLEGSEL                                
101100         AND    KDFINDOC  = :DOTY-KDFINDOC                                
101200         AND    KDSTATUS  = :WS-COMING-VERSION                            
101300     END-EXEC                                                             
101400                                                                          
101500     MOVE SQLCODE TO SQLCODE-WS                                           
101600     PERFORM DB2-STATUS-CHECK                                             
101700     .                                                                    
101800                                                                          
101900 DB2-CLOSE-T01DOTY-CRS SECTION.                                           
102000     EXEC SQL                                                             
102100        CLOSE DOTY-CRS                                                    
102200     END-EXEC                                                             
102300     .                                                                    
102400                                                                          
102500 DB2-DCL-OPN-T01SECO-CRS SECTION.                                         
102600     MOVE 000100 TO GOOD-SQLCODECODES                                     
102700                                                                          
102800     EXEC SQL                                                             
102900         DECLARE SECO-CRS CURSOR WITH HOLD FOR                            
103000            SELECT  IDLEGSEL                                              
103100                  , IDLANDX3                                              
103200                  , KDSTATUS                                              
103300                  , IDVAT                                                 
103400                  , KDVALISO                                              
103500                  , DAREGDAT                                              
103600                  , DAUPPDAT                                              
103700                  , DADELDAT                                              
103800                  , IDUSER                                                
103900                  , BETEXT_1                                              
104000                  , BETEXT_2                                              
104100                  , BETEXT_3                                              
104200                  , BETEXT_4                                              
104300                                                                          
104400           FROM     T01SECO                                               
104500                                                                          
104600           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
104700           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
104800     END-EXEC                                                             
104900                                                                          
105000     MOVE 000100 TO GOOD-SQLCODECODES                                     
105100                                                                          
105200     EXEC SQL                                                             
105300        OPEN SECO-CRS                                                     
105400     END-EXEC                                                             
105500                                                                          
105600     MOVE SQLCODE TO SQLCODE-WS                                           
105700     PERFORM DB2-STATUS-CHECK                                             
105800     .                                                                    
105900                                                                          
106000 DB2-FETCH-T01SECO-CRS SECTION.                                           
106100     MOVE 000100  TO GOOD-SQLCODECODES                                    
106200                                                                          
106300     EXEC SQL                                                             
106400         FETCH SECO-CRS                                                   
106500                                                                          
106600         INTO :SECO-IDLEGSEL                                              
106700            , :SECO-IDLANDX3                                              
106800            , :SECO-KDSTATUS                                              
106900            , :SECO-IDVAT                                                 
107000            , :SECO-KDVALISO                                              
107100            , :SECO-DAREGDAT                                              
107200            , :SECO-DAUPPDAT                                              
107300            , :SECO-DADELDAT                                              
107400            , :SECO-IDUSER                                                
107500            , :SECO-BETEXT-1                                              
107600            , :SECO-BETEXT-2                                              
107700            , :SECO-BETEXT-3                                              
107800            , :SECO-BETEXT-4                                              
107900     END-EXEC                                                             
108000                                                                          
108100     MOVE SQLCODE TO SQLCODE-WS                                           
108200     PERFORM DB2-STATUS-CHECK                                             
108300     .                                                                    
108400                                                                          
108500 DB2-DELETE-T01SECO-CURRENT SECTION.                                      
108600     MOVE 000   TO GOOD-SQLCODECODES                                      
108700                                                                          
108800     EXEC SQL                                                             
108900         DELETE FROM T01SECO                                              
109000                                                                          
109100         WHERE  IDLEGSEL = :SECO-IDLEGSEL                                 
109200         AND    IDLANDX3 = :SECO-IDLANDX3                                 
109300         AND    KDSTATUS = :WS-CURRENT-VERSION                            
109400     END-EXEC                                                             
109500                                                                          
109600     MOVE SQLCODE TO SQLCODE-WS                                           
109700     PERFORM DB2-STATUS-CHECK                                             
109800     .                                                                    
109900                                                                          
110000 DB2-INSERT-T01SECO-CURRENT SECTION.                                      
110100     MOVE 000   TO GOOD-SQLCODECODES                                      
110200                                                                          
110300     EXEC SQL                                                             
110400       INSERT INTO T01SECO                                                
110500          (IDLEGSEL                                                       
110600          ,IDLANDX3                                                       
110700          ,KDSTATUS                                                       
110800          ,IDVAT                                                          
110900          ,KDVALISO                                                       
111000          ,DAREGDAT                                                       
111100          ,DAUPPDAT                                                       
111200          ,DADELDAT                                                       
111300          ,IDUSER                                                         
111400          ,BETEXT_1                                                       
111500          ,BETEXT_2                                                       
111600          ,BETEXT_3                                                       
111700          ,BETEXT_4)                                                      
111800       VALUES                                                             
111900          (:SECO-IDLEGSEL                                                 
112000          ,:SECO-IDLANDX3                                                 
112100          ,:WS-CURRENT-VERSION                                            
112200          ,:SECO-IDVAT                                                    
112300          ,:SECO-KDVALISO                                                 
112400          ,:SECO-DAREGDAT                                                 
112500          ,:SECO-DAUPPDAT                                                 
112600          ,:SECO-DADELDAT                                                 
112700          ,:SECO-IDUSER                                                   
112800          ,:SECO-BETEXT-1                                                 
112900          ,:SECO-BETEXT-2                                                 
113000          ,:SECO-BETEXT-3                                                 
113100          ,:SECO-BETEXT-4)                                                
113200     END-EXEC                                                             
113300                                                                          
113400     MOVE SQLCODE TO SQLCODE-WS                                           
113500     PERFORM DB2-STATUS-CHECK                                             
113600     .                                                                    
113700                                                                          
113800 DB2-DELETE-T01SECO-COMING SECTION.                                       
113900     MOVE 000   TO GOOD-SQLCODECODES                                      
114000                                                                          
114100     EXEC SQL                                                             
114200         DELETE FROM T01SECO                                              
114300                                                                          
114400         WHERE  IDLEGSEL  = :SECO-IDLEGSEL                                
114500         AND    IDLANDX3  = :SECO-IDLANDX3                                
114600         AND    KDSTATUS  = :WS-COMING-VERSION                            
114700     END-EXEC                                                             
114800                                                                          
114900     MOVE SQLCODE TO SQLCODE-WS                                           
115000     PERFORM DB2-STATUS-CHECK                                             
115100     .                                                                    
115200                                                                          
115300 DB2-CLOSE-T01SECO-CRS SECTION.                                           
115400                                                                          
115500     EXEC SQL                                                             
115600        CLOSE SECO-CRS                                                    
115700     END-EXEC                                                             
115800     .                                                                    
115900                                                                          
116000 DB2-DCL-OPN-T01RECO-CRS SECTION.                                         
116100     MOVE 000100 TO GOOD-SQLCODECODES                                     
116200                                                                          
116300     EXEC SQL                                                             
116400         DECLARE RECO-CRS CURSOR WITH HOLD FOR                            
116500            SELECT  IDLEGSEL                                              
116600                  , IDLANDX3                                              
116700                  , KDSTATUS                                              
116800                  , IDSPRAK                                               
116900                  , BERESPRA_1                                            
117000                  , BERESPRA_2                                            
117100                  , ADRESP_STREET                                         
117200                  , ADRESP_BOX                                            
117300                  , ADRESP_CITY                                           
117400                  , ADRESP_PCODE                                          
117500                  , IDTFN                                                 
117600                  , IDTFX                                                 
117700                  , IDMAIL                                                
117800                  , BECONT                                                
117900                  , IDVAT                                                 
118000                  , IDBG                                                  
118100                  , IDPG                                                  
118200                  , DAREGDAT                                              
118300                  , DAUPPDAT                                              
118400                  , DADELDAT                                              
118500                  , IDUSER                                                
118600                  , IDVAT_AGENT                                           
118700                                                                          
118800           FROM     T01RECO                                               
118900                                                                          
119000           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
119100           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
119200     END-EXEC                                                             
119300                                                                          
119400     MOVE 000100 TO GOOD-SQLCODECODES                                     
119500                                                                          
119600     EXEC SQL                                                             
119700        OPEN RECO-CRS                                                     
119800     END-EXEC                                                             
119900                                                                          
120000     MOVE SQLCODE TO SQLCODE-WS                                           
120100     PERFORM DB2-STATUS-CHECK                                             
120200     .                                                                    
120300                                                                          
120400 DB2-FETCH-T01RECO-CRS SECTION.                                           
120500     MOVE 000100  TO GOOD-SQLCODECODES                                    
120600                                                                          
120700     EXEC SQL                                                             
120800         FETCH RECO-CRS                                                   
120900                                                                          
121000         INTO :RECO-IDLEGSEL                                              
121100            , :RECO-IDLANDX3                                              
121200            , :RECO-KDSTATUS                                              
121300            , :RECO-IDSPRAK                                               
121400            , :RECO-BERESPRA-1                                            
121500            , :RECO-BERESPRA-2                                            
121600            , :RECO-ADRESP-STREET                                         
121700            , :RECO-ADRESP-BOX                                            
121800            , :RECO-ADRESP-CITY                                           
121900            , :RECO-ADRESP-PCODE                                          
122000            , :RECO-IDTFN                                                 
122100            , :RECO-IDTFX                                                 
122200            , :RECO-IDMAIL                                                
122300            , :RECO-BECONT                                                
122400            , :RECO-IDVAT                                                 
122500            , :RECO-IDBG                                                  
122600            , :RECO-IDPG                                                  
122700            , :RECO-DAREGDAT                                              
122800            , :RECO-DAUPPDAT                                              
122900            , :RECO-DADELDAT                                              
123000            , :RECO-IDUSER                                                
123100            , :RECO-IDVAT-AGENT                                           
123200     END-EXEC                                                             
123300                                                                          
123400     MOVE SQLCODE TO SQLCODE-WS                                           
123500     PERFORM DB2-STATUS-CHECK                                             
123600     .                                                                    
123700                                                                          
123800 DB2-DELETE-T01RECO-CURRENT SECTION.                                      
123900     MOVE 000   TO GOOD-SQLCODECODES                                      
124000                                                                          
124100     EXEC SQL                                                             
124200         DELETE FROM T01RECO                                              
124300                                                                          
124400         WHERE  IDLEGSEL = :RECO-IDLEGSEL                                 
124500         AND    IDLANDX3 = :RECO-IDLANDX3                                 
124600         AND    KDSTATUS = :WS-CURRENT-VERSION                            
124700     END-EXEC                                                             
124800                                                                          
124900     MOVE SQLCODE TO SQLCODE-WS                                           
125000     PERFORM DB2-STATUS-CHECK                                             
125100     .                                                                    
125200                                                                          
125300 DB2-INSERT-T01RECO-CURRENT SECTION.                                      
125400     MOVE 000   TO GOOD-SQLCODECODES                                      
125500                                                                          
125600     EXEC SQL                                                             
125700       INSERT INTO T01RECO                                                
125800          (IDLEGSEL                                                       
125900          ,IDLANDX3                                                       
126000          ,KDSTATUS                                                       
126100          ,IDSPRAK                                                        
126200          ,BERESPRA_1                                                     
126300          ,BERESPRA_2                                                     
126400          ,ADRESP_STREET                                                  
126500          ,ADRESP_BOX                                                     
126600          ,ADRESP_CITY                                                    
126700          ,ADRESP_PCODE                                                   
126800          ,IDTFN                                                          
126900          ,IDTFX                                                          
127000          ,IDMAIL                                                         
127100          ,BECONT                                                         
127200          ,IDVAT                                                          
127300          ,IDBG                                                           
127400          ,IDPG                                                           
127500          ,DAREGDAT                                                       
127600          ,DAUPPDAT                                                       
127700          ,DADELDAT                                                       
127800          ,IDUSER                                                         
127900          ,IDVAT_AGENT)                                                   
128000       VALUES                                                             
128100          (:RECO-IDLEGSEL                                                 
128200          ,:RECO-IDLANDX3                                                 
128300          ,:WS-CURRENT-VERSION                                            
128400          ,:RECO-IDSPRAK                                                  
128500          ,:RECO-BERESPRA-1                                               
128600          ,:RECO-BERESPRA-2                                               
128700          ,:RECO-ADRESP-STREET                                            
128800          ,:RECO-ADRESP-BOX                                               
128900          ,:RECO-ADRESP-CITY                                              
129000          ,:RECO-ADRESP-PCODE                                             
129100          ,:RECO-IDTFN                                                    
129200          ,:RECO-IDTFX                                                    
129300          ,:RECO-IDMAIL                                                   
129400          ,:RECO-BECONT                                                   
129500          ,:RECO-IDVAT                                                    
129600          ,:RECO-IDBG                                                     
129700          ,:RECO-IDPG                                                     
129800          ,:RECO-DAREGDAT                                                 
129900          ,:RECO-DAUPPDAT                                                 
130000          ,:RECO-DADELDAT                                                 
130100          ,:RECO-IDUSER                                                   
130200          ,:RECO-IDVAT-AGENT)                                             
130300     END-EXEC                                                             
130400                                                                          
130500     MOVE SQLCODE TO SQLCODE-WS                                           
130600     PERFORM DB2-STATUS-CHECK                                             
130700     .                                                                    
130800                                                                          
130900 DB2-DELETE-T01RECO-COMING SECTION.                                       
131000     MOVE 000   TO GOOD-SQLCODECODES                                      
131100                                                                          
131200     EXEC SQL                                                             
131300         DELETE FROM T01RECO                                              
131400                                                                          
131500         WHERE  IDLEGSEL  = :RECO-IDLEGSEL                                
131600         AND    IDLANDX3  = :RECO-IDLANDX3                                
131700         AND    KDSTATUS  = :WS-COMING-VERSION                            
131800     END-EXEC                                                             
131900                                                                          
132000     MOVE SQLCODE TO SQLCODE-WS                                           
132100     PERFORM DB2-STATUS-CHECK                                             
132200     .                                                                    
132300                                                                          
132400 DB2-CLOSE-T01RECO-CRS SECTION.                                           
132500                                                                          
132600     EXEC SQL                                                             
132700        CLOSE RECO-CRS                                                    
132800     END-EXEC                                                             
132900     .                                                                    
133000                                                                          
133100 DB2-DCL-OPN-T01INRE-CRS SECTION.                                         
133200     MOVE 000100 TO GOOD-SQLCODECODES                                     
133300                                                                          
133400     EXEC SQL                                                             
133500         DECLARE INRE-CRS CURSOR WITH HOLD FOR                            
133600            SELECT  IDLEGSEL                                              
133700                  , IDLANDX3_SEND                                         
133800                  , IDLANDX3_REC                                          
133900                  , KDSTATUS                                              
134000                  , FLEXPORT                                              
134100                  , FLVAT                                                 
134200                  , FLVAT_PRIV                                            
134300                  , FLVATREP                                              
134400                  , FLINTREP                                              
134500                  , KDVAT                                                 
134600                  , KDVAT_SERV                                            
134700                  , DAREGDAT                                              
134800                  , DAUPPDAT                                              
134900                  , DADELDAT                                              
135000                  , IDUSER                                                
135100                  , FLCUSREP                                              
135200                  , BETEXT_1                                              
135300                  , BETEXT_2                                              
135400                  , BETEXT_3                                              
135500                  , BETEXT_4                                              
135600                  , BETEXT_5                                              
135700                  , BETEXT_6                                              
135800                  , BETEXT_7                                              
135900                  , BETEXT_8                                              
136000                  , BETEXT_9                                              
136100                  , BETEXT_10                                             
136200                  , BETEXT_11                                             
136300                  , BETEXT_12                                             
136400                  , BETEXT_13                                             
136500                  , BETEXT_14                                             
136600                  , BETEXT_15                                             
136700                  , BETEXT_16                                             
136800                  , BETEXT_17                                             
136900                  , BETEXT_18                                             
137000                  , BETEXT_19                                             
137100                  , BETEXT_20                                             
137100                  , BETEXT_21                                             
137100                  , BETEXT_22                                             
137100                  , BETEXT_23                                             
137100                  , BETEXT_24                                             
137100                  , BETEXT_25                                             
137100                  , BETEXT_26                                             
137100                  , BETEXT_27                                             
137100                  , BETEXT_28                                             
137100                  , BETEXT_29                                             
137100                  , BETEXT_30                                             
137100                  , BETEXT_31                                             
137100                  , BETEXT_32                                             
137100                  , BETEXT_33                                             
137100                  , BETEXT_34                                             
137100                  , BETEXT_35                                             
137100                  , BETEXT_36                                             
137100                  , BETEXT_37                                             
137100                  , BETEXT_38                                             
137100                  , BETEXT_39                                             
137100                  , BETEXT_40                                             
137200                                                                          
137300           FROM     T01INRE                                               
137400                                                                          
137500           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
137600           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
137700     END-EXEC                                                             
137800                                                                          
137900     MOVE 000100 TO GOOD-SQLCODECODES                                     
138000                                                                          
138100     EXEC SQL                                                             
138200        OPEN INRE-CRS                                                     
138300     END-EXEC                                                             
138400                                                                          
138500     MOVE SQLCODE TO SQLCODE-WS                                           
138600     PERFORM DB2-STATUS-CHECK                                             
138700     .                                                                    
138800                                                                          
138900 DB2-FETCH-T01INRE-CRS SECTION.                                           
139000     MOVE 000100  TO GOOD-SQLCODECODES                                    
139100                                                                          
139200     EXEC SQL                                                             
139300         FETCH INRE-CRS                                                   
139400                                                                          
139500         INTO :INRE-IDLEGSEL                                              
139600            , :INRE-IDLANDX3-SEND                                         
139700            , :INRE-IDLANDX3-REC                                          
139800            , :INRE-KDSTATUS                                              
139900            , :INRE-FLEXPORT                                              
140000            , :INRE-FLVAT                                                 
140100            , :INRE-FLVAT-PRIV                                            
140200            , :INRE-FLVATREP                                              
140300            , :INRE-FLINTREP                                              
140400            , :INRE-KDVAT                                                 
140500            , :INRE-KDVAT-SERV                                            
140600            , :INRE-DAREGDAT                                              
140700            , :INRE-DAUPPDAT                                              
140800            , :INRE-DADELDAT                                              
140900            , :INRE-IDUSER                                                
141000            , :INRE-FLCUSREP                                              
141100            , :INRE-BETEXT-1                                              
141200            , :INRE-BETEXT-2                                              
141300            , :INRE-BETEXT-3                                              
141400            , :INRE-BETEXT-4                                              
141500            , :INRE-BETEXT-5                                              
141600            , :INRE-BETEXT-6                                              
141700            , :INRE-BETEXT-7                                              
141800            , :INRE-BETEXT-8                                              
141900            , :INRE-BETEXT-9                                              
142000            , :INRE-BETEXT-10                                             
142100            , :INRE-BETEXT-11                                             
142200            , :INRE-BETEXT-12                                             
142300            , :INRE-BETEXT-13                                             
142400            , :INRE-BETEXT-14                                             
142500            , :INRE-BETEXT-15                                             
142600            , :INRE-BETEXT-16                                             
142700            , :INRE-BETEXT-17                                             
142800            , :INRE-BETEXT-18                                             
142900            , :INRE-BETEXT-19                                             
143000            , :INRE-BETEXT-20                                             
143000            , :INRE-BETEXT-21                                             
143000            , :INRE-BETEXT-22                                             
143000            , :INRE-BETEXT-23                                             
143000            , :INRE-BETEXT-24                                             
143000            , :INRE-BETEXT-25                                             
143000            , :INRE-BETEXT-26                                             
143000            , :INRE-BETEXT-27                                             
143000            , :INRE-BETEXT-28                                             
143000            , :INRE-BETEXT-29                                             
143000            , :INRE-BETEXT-30                                             
143000            , :INRE-BETEXT-31                                             
143000            , :INRE-BETEXT-32                                             
143000            , :INRE-BETEXT-33                                             
143000            , :INRE-BETEXT-34                                             
143000            , :INRE-BETEXT-35                                             
143000            , :INRE-BETEXT-36                                             
143000            , :INRE-BETEXT-37                                             
143000            , :INRE-BETEXT-38                                             
143000            , :INRE-BETEXT-39                                             
143000            , :INRE-BETEXT-40                                             
143100     END-EXEC                                                             
143200                                                                          
143300     MOVE SQLCODE TO SQLCODE-WS                                           
143400     PERFORM DB2-STATUS-CHECK                                             
143500     .                                                                    
143600                                                                          
143700 DB2-DELETE-T01INRE-CURRENT SECTION.                                      
143800     MOVE 000   TO GOOD-SQLCODECODES                                      
143900                                                                          
144000     EXEC SQL                                                             
144100         DELETE FROM T01INRE                                              
144200                                                                          
144300         WHERE  IDLEGSEL      = :INRE-IDLEGSEL                            
144400         AND    IDLANDX3_SEND = :INRE-IDLANDX3-SEND                       
144500         AND    IDLANDX3_REC  = :INRE-IDLANDX3-REC                        
144600         AND    KDSTATUS      = :WS-CURRENT-VERSION                       
144700     END-EXEC                                                             
144800                                                                          
144900     MOVE SQLCODE TO SQLCODE-WS                                           
145000     PERFORM DB2-STATUS-CHECK                                             
145100     .                                                                    
145200                                                                          
145300 DB2-INSERT-T01INRE-CURRENT SECTION.                                      
145400     MOVE 000   TO GOOD-SQLCODECODES                                      
145500                                                                          
145600     EXEC SQL                                                             
145700       INSERT INTO T01INRE                                                
145800          (IDLEGSEL                                                       
145900          ,IDLANDX3_SEND                                                  
146000          ,IDLANDX3_REC                                                   
146100          ,KDSTATUS                                                       
146200          ,FLEXPORT                                                       
146300          ,FLVAT                                                          
146400          ,FLVAT_PRIV                                                     
146500          ,FLVATREP                                                       
146600          ,FLINTREP                                                       
146700          ,KDVAT                                                          
146800          ,KDVAT_SERV                                                     
146900          ,DAREGDAT                                                       
147000          ,DAUPPDAT                                                       
147100          ,DADELDAT                                                       
147200          ,IDUSER                                                         
147300          ,FLCUSREP                                                       
147400          ,BETEXT_1                                                       
147500          ,BETEXT_2                                                       
147600          ,BETEXT_3                                                       
147700          ,BETEXT_4                                                       
147800          ,BETEXT_5                                                       
147900          ,BETEXT_6                                                       
148000          ,BETEXT_7                                                       
148100          ,BETEXT_8                                                       
148200          ,BETEXT_9                                                       
148300          ,BETEXT_10                                                      
148400          ,BETEXT_11                                                      
148500          ,BETEXT_12                                                      
148600          ,BETEXT_13                                                      
148700          ,BETEXT_14                                                      
148800          ,BETEXT_15                                                      
148900          ,BETEXT_16                                                      
149000          ,BETEXT_17                                                      
149100          ,BETEXT_18                                                      
149200          ,BETEXT_19                                                      
149300          ,BETEXT_20                                                      
149300          ,BETEXT_21                                                      
149300          ,BETEXT_22                                                      
149300          ,BETEXT_23                                                      
149300          ,BETEXT_24                                                      
149300          ,BETEXT_25                                                      
149300          ,BETEXT_26                                                      
149300          ,BETEXT_27                                                      
149300          ,BETEXT_28                                                      
149300          ,BETEXT_29                                                      
149300          ,BETEXT_30                                                      
149300          ,BETEXT_31                                                      
149300          ,BETEXT_32                                                      
149300          ,BETEXT_33                                                      
149300          ,BETEXT_34                                                      
149300          ,BETEXT_35                                                      
149300          ,BETEXT_36                                                      
149300          ,BETEXT_37                                                      
149300          ,BETEXT_38                                                      
149300          ,BETEXT_39                                                      
149300          ,BETEXT_40)                                                     
149400       VALUES                                                             
149500          (:INRE-IDLEGSEL                                                 
149600          ,:INRE-IDLANDX3-SEND                                            
149700          ,:INRE-IDLANDX3-REC                                             
149800          ,:WS-CURRENT-VERSION                                            
149900          ,:INRE-FLEXPORT                                                 
150000          ,:INRE-FLVAT                                                    
150100          ,:INRE-FLVAT-PRIV                                               
150200          ,:INRE-FLVATREP                                                 
150300          ,:INRE-FLINTREP                                                 
150400          ,:INRE-KDVAT                                                    
150500          ,:INRE-KDVAT-SERV                                               
150600          ,:INRE-DAREGDAT                                                 
150700          ,:INRE-DAUPPDAT                                                 
150800          ,:INRE-DADELDAT                                                 
150900          ,:INRE-IDUSER                                                   
151000          ,:INRE-FLCUSREP                                                 
151100          ,:INRE-BETEXT-1                                                 
151200          ,:INRE-BETEXT-2                                                 
151300          ,:INRE-BETEXT-3                                                 
151400          ,:INRE-BETEXT-4                                                 
151500          ,:INRE-BETEXT-5                                                 
151600          ,:INRE-BETEXT-6                                                 
151700          ,:INRE-BETEXT-7                                                 
151800          ,:INRE-BETEXT-8                                                 
151900          ,:INRE-BETEXT-9                                                 
152000          ,:INRE-BETEXT-10                                                
152100          ,:INRE-BETEXT-11                                                
152200          ,:INRE-BETEXT-12                                                
152300          ,:INRE-BETEXT-13                                                
152400          ,:INRE-BETEXT-14                                                
152500          ,:INRE-BETEXT-15                                                
152600          ,:INRE-BETEXT-16                                                
152700          ,:INRE-BETEXT-17                                                
152800          ,:INRE-BETEXT-18                                                
152900          ,:INRE-BETEXT-19                                                
153000          ,:INRE-BETEXT-20                                                
153000          ,:INRE-BETEXT-21                                                
153000          ,:INRE-BETEXT-22                                                
153000          ,:INRE-BETEXT-23                                                
153000          ,:INRE-BETEXT-24                                                
153000          ,:INRE-BETEXT-25                                                
153000          ,:INRE-BETEXT-26                                                
153000          ,:INRE-BETEXT-27                                                
153000          ,:INRE-BETEXT-28                                                
153000          ,:INRE-BETEXT-29                                                
153000          ,:INRE-BETEXT-30                                                
153000          ,:INRE-BETEXT-31                                                
153000          ,:INRE-BETEXT-32                                                
153000          ,:INRE-BETEXT-33                                                
153000          ,:INRE-BETEXT-34                                                
153000          ,:INRE-BETEXT-35                                                
153000          ,:INRE-BETEXT-36                                                
153000          ,:INRE-BETEXT-37                                                
153000          ,:INRE-BETEXT-38                                                
153000          ,:INRE-BETEXT-39                                                
153000          ,:INRE-BETEXT-40)                                               
153100     END-EXEC                                                             
153200                                                                          
153300     MOVE SQLCODE TO SQLCODE-WS                                           
153400     PERFORM DB2-STATUS-CHECK                                             
153500     .                                                                    
153600                                                                          
153700 DB2-DELETE-T01INRE-COMING SECTION.                                       
153800     MOVE 000   TO GOOD-SQLCODECODES                                      
153900                                                                          
154000     EXEC SQL                                                             
154100         DELETE FROM T01INRE                                              
154200                                                                          
154300         WHERE  IDLEGSEL      = :INRE-IDLEGSEL                            
154400         AND    IDLANDX3_SEND = :INRE-IDLANDX3-SEND                       
154500         AND    IDLANDX3_REC  = :INRE-IDLANDX3-REC                        
154600         AND    KDSTATUS      = :WS-COMING-VERSION                        
154700     END-EXEC                                                             
154800                                                                          
154900     MOVE SQLCODE TO SQLCODE-WS                                           
155000     PERFORM DB2-STATUS-CHECK                                             
155100     .                                                                    
155200                                                                          
155300 DB2-CLOSE-T01INRE-CRS SECTION.                                           
155400                                                                          
155500     EXEC SQL                                                             
155600        CLOSE INRE-CRS                                                    
155700     END-EXEC                                                             
155800     .                                                                    
155900                                                                          
156000 DB2-DCL-OPN-T01BURE-CRS SECTION.                                         
156100     MOVE 000100 TO GOOD-SQLCODECODES                                     
156200                                                                          
156300     EXEC SQL                                                             
156400         DECLARE BURE-CRS CURSOR WITH HOLD FOR                            
156500            SELECT  IDLEGSEL                                              
156600                  , KDFINDOC                                              
156700                  , KDPARTTY                                              
156800                  , KDPARTGR                                              
156900                  , KDSTATUS                                              
157000                  , BEFORMS                                               
157100                  , FLGL                                                  
157200                  , FLAR                                                  
157300                  , FLAP                                                  
157400                  , FLVATCHK                                              
157500                  , DAREGDAT                                              
157600                  , DAUPPDAT                                              
157700                  , DADELDAT                                              
157800                  , IDUSER                                                
157900                                                                          
158000           FROM     T01BURE                                               
158100                                                                          
158200           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
158300           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
158400     END-EXEC                                                             
158500                                                                          
158600     MOVE 000100 TO GOOD-SQLCODECODES                                     
158700                                                                          
158800     EXEC SQL                                                             
158900        OPEN BURE-CRS                                                     
159000     END-EXEC                                                             
159100                                                                          
159200     MOVE SQLCODE TO SQLCODE-WS                                           
159300     PERFORM DB2-STATUS-CHECK                                             
159400     .                                                                    
159500                                                                          
159600 DB2-FETCH-T01BURE-CRS SECTION.                                           
159700     MOVE 000100  TO GOOD-SQLCODECODES                                    
159800                                                                          
159900     EXEC SQL                                                             
160000         FETCH BURE-CRS                                                   
160100                                                                          
160200         INTO :BURE-IDLEGSEL                                              
160300            , :BURE-KDFINDOC                                              
160400            , :BURE-KDPARTTY                                              
160500            , :BURE-KDPARTGR                                              
160600            , :BURE-KDSTATUS                                              
160700            , :BURE-BEFORMS                                               
160800            , :BURE-FLGL                                                  
160900            , :BURE-FLAR                                                  
161000            , :BURE-FLAP                                                  
161100            , :BURE-FLVATCHK                                              
161200            , :BURE-DAREGDAT                                              
161300            , :BURE-DAUPPDAT                                              
161400            , :BURE-DADELDAT                                              
161500            , :BURE-IDUSER                                                
161600     END-EXEC                                                             
161700                                                                          
161800     MOVE SQLCODE TO SQLCODE-WS                                           
161900     PERFORM DB2-STATUS-CHECK                                             
162000     .                                                                    
162100                                                                          
162200 DB2-DELETE-T01BURE-CURRENT SECTION.                                      
162300     MOVE 000   TO GOOD-SQLCODECODES                                      
162400                                                                          
162500     EXEC SQL                                                             
162600         DELETE FROM T01BURE                                              
162700                                                                          
162800         WHERE  IDLEGSEL = :BURE-IDLEGSEL                                 
162900         AND    KDFINDOC = :BURE-KDFINDOC                                 
163000         AND    KDPARTTY = :BURE-KDPARTTY                                 
163100         AND    KDPARTGR = :BURE-KDPARTGR                                 
163200         AND    KDSTATUS = :WS-CURRENT-VERSION                            
163300     END-EXEC                                                             
163400                                                                          
163500     MOVE SQLCODE TO SQLCODE-WS                                           
163600     PERFORM DB2-STATUS-CHECK                                             
163700     .                                                                    
163800                                                                          
163900 DB2-INSERT-T01BURE-CURRENT SECTION.                                      
164000     MOVE 000   TO GOOD-SQLCODECODES                                      
164100                                                                          
164200     EXEC SQL                                                             
164300       INSERT INTO T01BURE                                                
164400          (IDLEGSEL                                                       
164500          ,KDFINDOC                                                       
164600          ,KDPARTTY                                                       
164700          ,KDPARTGR                                                       
164800          ,KDSTATUS                                                       
164900          ,BEFORMS                                                        
165000          ,FLGL                                                           
165100          ,FLAR                                                           
165200          ,FLAP                                                           
165300          ,DAREGDAT                                                       
165400          ,DAUPPDAT                                                       
165500          ,DADELDAT                                                       
165600          ,IDUSER)                                                        
165700       VALUES                                                             
165800          (:BURE-IDLEGSEL                                                 
165900          ,:BURE-KDFINDOC                                                 
166000          ,:BURE-KDPARTTY                                                 
166100          ,:BURE-KDPARTGR                                                 
166200          ,:WS-CURRENT-VERSION                                            
166300          ,:BURE-BEFORMS                                                  
166400          ,:BURE-FLGL                                                     
166500          ,:BURE-FLAR                                                     
166600          ,:BURE-FLAP                                                     
166700          ,:BURE-DAREGDAT                                                 
166800          ,:BURE-DAUPPDAT                                                 
166900          ,:BURE-DADELDAT                                                 
167000          ,:BURE-IDUSER)                                                  
167100     END-EXEC                                                             
167200                                                                          
167300     MOVE SQLCODE TO SQLCODE-WS                                           
167400     PERFORM DB2-STATUS-CHECK                                             
167500     .                                                                    
167600                                                                          
167700 DB2-DELETE-T01BURE-COMING SECTION.                                       
167800     MOVE 000   TO GOOD-SQLCODECODES                                      
167900                                                                          
168000     EXEC SQL                                                             
168100         DELETE FROM T01BURE                                              
168200                                                                          
168300         WHERE  IDLEGSEL = :BURE-IDLEGSEL                                 
168400         AND    KDFINDOC = :BURE-KDFINDOC                                 
168500         AND    KDPARTTY = :BURE-KDPARTTY                                 
168600         AND    KDPARTGR = :BURE-KDPARTGR                                 
168700         AND    KDSTATUS = :WS-COMING-VERSION                             
168800     END-EXEC                                                             
168900                                                                          
169000     MOVE SQLCODE TO SQLCODE-WS                                           
169100     PERFORM DB2-STATUS-CHECK                                             
169200     .                                                                    
169300                                                                          
169400 DB2-CLOSE-T01BURE-CRS SECTION.                                           
169500     EXEC SQL                                                             
169600        CLOSE BURE-CRS                                                    
169700     END-EXEC                                                             
169800     .                                                                    
169900                                                                          
170000 DB2-DCL-OPN-T01CURR-CRS SECTION.                                         
170100     MOVE 000100 TO GOOD-SQLCODECODES                                     
170200                                                                          
170300     EXEC SQL                                                             
170400         DECLARE CURR-CRS CURSOR WITH HOLD FOR                            
170500            SELECT  IDLEGSEL                                              
170600                  , KDVALISO                                              
170700                  , DASTADAT                                              
170800                                                                          
170900           FROM     T01CURR                                               
171000     END-EXEC                                                             
171100                                                                          
171200     MOVE 000100 TO GOOD-SQLCODECODES                                     
171300                                                                          
171400     EXEC SQL                                                             
171500        OPEN CURR-CRS                                                     
171600     END-EXEC                                                             
171700                                                                          
171800     MOVE SQLCODE TO SQLCODE-WS                                           
171900     PERFORM DB2-STATUS-CHECK                                             
172000     .                                                                    
172100                                                                          
172200 DB2-FETCH-T01CURR-CRS SECTION.                                           
172300     MOVE 000100  TO GOOD-SQLCODECODES                                    
172400                                                                          
172500     EXEC SQL                                                             
172600         FETCH CURR-CRS                                                   
172700                                                                          
172800         INTO :CURR-IDLEGSEL                                              
172900            , :CURR-KDVALISO                                              
173000            , :CURR-DASTADAT                                              
173100     END-EXEC                                                             
173200                                                                          
173300     MOVE SQLCODE TO SQLCODE-WS                                           
173400     PERFORM DB2-STATUS-CHECK                                             
173500     .                                                                    
173600                                                                          
173700 DB2-DELETE-T01CURR SECTION.                                              
173800     MOVE 000   TO GOOD-SQLCODECODES                                      
173900                                                                          
174000     EXEC SQL                                                             
174100         DELETE FROM T01CURR                                              
174200                                                                          
174300         WHERE  IDLEGSEL = :CURR-IDLEGSEL                                 
174400         AND    KDVALISO = :CURR-KDVALISO                                 
174500         AND    DASTADAT = :CURR-DASTADAT                                 
174600     END-EXEC                                                             
174700                                                                          
174800     MOVE SQLCODE TO SQLCODE-WS                                           
174900     PERFORM DB2-STATUS-CHECK                                             
175000     .                                                                    
175100                                                                          
175200 DB2-CLOSE-T01CURR-CRS SECTION.                                           
175300     EXEC SQL                                                             
175400        CLOSE CURR-CRS                                                    
175500     END-EXEC                                                             
175600     .                                                                    
175700                                                                          
175800 DB2-DCL-OPN-T01PAIN-CRS SECTION.                                         
175900     MOVE 000100 TO GOOD-SQLCODECODES                                     
176000                                                                          
176100     EXEC SQL                                                             
176200         DECLARE PAIN-CRS CURSOR WITH HOLD FOR                            
176300            SELECT  IDLEGSEL                                              
176400                  , KDFINDOC                                              
176500                  , KDPARTTY                                              
176600                  , KDPARTGR                                              
176700                  , KDVALISO                                              
176800                  , KDSTATUS                                              
176900                  , BETEXT_1                                              
177000                  , BETEXT_2                                              
177100                  , BETEXT_3                                              
177200                  , BETEXT_4                                              
177300                  , DAREGDAT                                              
177400                  , DAUPPDAT                                              
177500                  , IDUSER                                                
177600                                                                          
177700           FROM     T01PAIN                                               
177800                                                                          
177900           WHERE    KDSTATUS  = :WS-COMING-VERSION                        
178000           AND      DAUPPDAT <= :WS-CURRENT-DATE                          
178100     END-EXEC                                                             
178200                                                                          
178300     MOVE 000100 TO GOOD-SQLCODECODES                                     
178400                                                                          
178500     EXEC SQL                                                             
178600        OPEN PAIN-CRS                                                     
178700     END-EXEC                                                             
178800                                                                          
178900     MOVE SQLCODE TO SQLCODE-WS                                           
179000     PERFORM DB2-STATUS-CHECK                                             
179100     .                                                                    
179200                                                                          
179300 DB2-FETCH-T01PAIN-CRS SECTION.                                           
179400     MOVE 000100  TO GOOD-SQLCODECODES                                    
179500                                                                          
179600     EXEC SQL                                                             
179700         FETCH PAIN-CRS                                                   
179800                                                                          
179900         INTO :PAIN-IDLEGSEL                                              
180000            , :PAIN-KDFINDOC                                              
180100            , :PAIN-KDPARTTY                                              
180200            , :PAIN-KDPARTGR                                              
180300            , :PAIN-KDVALISO                                              
180400            , :PAIN-KDSTATUS                                              
180500            , :PAIN-BETEXT-1                                              
180600            , :PAIN-BETEXT-2                                              
180700            , :PAIN-BETEXT-3                                              
180800            , :PAIN-BETEXT-4                                              
180900            , :PAIN-DAREGDAT                                              
181000            , :PAIN-DAUPPDAT                                              
181100            , :PAIN-IDUSER                                                
181200     END-EXEC                                                             
181300                                                                          
181400     MOVE SQLCODE TO SQLCODE-WS                                           
181500     PERFORM DB2-STATUS-CHECK                                             
181600     .                                                                    
181700                                                                          
181800 DB2-DELETE-T01PAIN-CURRENT SECTION.                                      
181900     MOVE 000   TO GOOD-SQLCODECODES                                      
182000                                                                          
182100     EXEC SQL                                                             
182200         DELETE FROM T01PAIN                                              
182300                                                                          
182400         WHERE  IDLEGSEL = :PAIN-IDLEGSEL                                 
182500         AND    KDFINDOC = :PAIN-KDFINDOC                                 
182600         AND    KDPARTTY = :PAIN-KDPARTTY                                 
182700         AND    KDPARTGR = :PAIN-KDPARTGR                                 
182800         AND    KDVALISO = :PAIN-KDVALISO                                 
182900         AND    KDSTATUS = :WS-CURRENT-VERSION                            
183000     END-EXEC                                                             
183100                                                                          
183200     MOVE SQLCODE TO SQLCODE-WS                                           
183300     PERFORM DB2-STATUS-CHECK                                             
183400     .                                                                    
183500                                                                          
183600 DB2-INSERT-T01PAIN-CURRENT SECTION.                                      
183700     MOVE 000   TO GOOD-SQLCODECODES                                      
183800                                                                          
183900     EXEC SQL                                                             
184000       INSERT INTO T01PAIN                                                
184100          (IDLEGSEL                                                       
184200          ,KDFINDOC                                                       
184300          ,KDPARTTY                                                       
184400          ,KDPARTGR                                                       
184500          ,KDVALISO                                                       
184600          ,KDSTATUS                                                       
184700          ,BETEXT_1                                                       
184800          ,BETEXT_2                                                       
184900          ,BETEXT_3                                                       
185000          ,BETEXT_4                                                       
185100          ,DAREGDAT                                                       
185200          ,DAUPPDAT                                                       
185300          ,IDUSER)                                                        
185400       VALUES                                                             
185500          (:PAIN-IDLEGSEL                                                 
185600          ,:PAIN-KDFINDOC                                                 
185700          ,:PAIN-KDPARTTY                                                 
185800          ,:PAIN-KDPARTGR                                                 
185900          ,:PAIN-KDVALISO                                                 
186000          ,:WS-CURRENT-VERSION                                            
186100          ,:PAIN-BETEXT-1                                                 
186200          ,:PAIN-BETEXT-2                                                 
186300          ,:PAIN-BETEXT-3                                                 
186400          ,:PAIN-BETEXT-4                                                 
186500          ,:PAIN-DAREGDAT                                                 
186600          ,:PAIN-DAUPPDAT                                                 
186700          ,:PAIN-IDUSER)                                                  
186800     END-EXEC                                                             
186900                                                                          
187000     MOVE SQLCODE TO SQLCODE-WS                                           
187100     PERFORM DB2-STATUS-CHECK                                             
187200     .                                                                    
187300                                                                          
187400 DB2-DELETE-T01PAIN-COMING SECTION.                                       
187500     MOVE 000   TO GOOD-SQLCODECODES                                      
187600                                                                          
187700     EXEC SQL                                                             
187800         DELETE FROM T01PAIN                                              
187900                                                                          
188000         WHERE  IDLEGSEL = :PAIN-IDLEGSEL                                 
188100         AND    KDFINDOC = :PAIN-KDFINDOC                                 
188200         AND    KDPARTTY = :PAIN-KDPARTTY                                 
188300         AND    KDPARTGR = :PAIN-KDPARTGR                                 
188400         AND    KDVALISO = :PAIN-KDVALISO                                 
188500         AND    KDSTATUS = :WS-COMING-VERSION                             
188600     END-EXEC                                                             
188700                                                                          
188800     MOVE SQLCODE TO SQLCODE-WS                                           
188900     PERFORM DB2-STATUS-CHECK                                             
189000     .                                                                    
189100                                                                          
189200 DB2-CLOSE-T01PAIN-CRS SECTION.                                           
189300     EXEC SQL                                                             
189400        CLOSE PAIN-CRS                                                    
189500     END-EXEC                                                             
189600     .                                                                    
189700                                                                          
189800 DB2-STATUS-CHECK     SECTION.                                            
189900     SET SQLCODE-IX TO 1                                                  
190000     SEARCH GOOD-SQLCODE                                                  
190100       AT END                                                             
190200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
190300          DELIMITED BY SIZE INTO ERROR-TEXT                               
190400          CALL ABEND USING RKOD-ABEND-DB2                                 
190500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
190600     END-SEARCH                                                           
190700     .                                                                    
