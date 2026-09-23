000100*PROCESS DYNAM                                                            
000300*                                                                         
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF108000.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/04/25.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*                                                                         
001900*    FUNCTION:                                                            
002710*        PGM READS   ROWS IN TABLE  T01SYST                               
002711*        PGM READS   ROWS IN TABLE  T01TBUN                               
002712*        PGM READS   ROWS IN TABLE  T01ALIN                               
002713*        PGM DELETES ROWS IN TABLE  T01TRAW                               
002730*        PGM DELETES ROWS IN TABLE  T01ABUN                               
002800*                                                                         
002811*        PGM TAKES CURRENT DATE MINUS NUMBER OF DAYS THE                  
002812*        DATA WILL BE KEPT (KVDAGAR FROM T01SYST) AND DELETES             
002813*        ALL ROWS OLDER THAN THAT IN                                      
002815*        - TABLE T01TRAW                                                  
002819*        - TABLE T01ABUN                                                  
002820*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003500 FILE-CONTROL.                                                            
003900 DATA DIVISION.                                                           
004100 FILE SECTION.                                                            
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'WF108000'.            
004610                                                                          
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004812     EJECT                                                                
004813                                                                          
004814 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004815     88  KEYS-OK                            VALUE 'J'.                    
004816     88  KEYS-ERROR                         VALUE 'N'.                    
004817                                                                          
004819 01  WS-COMMIT.                                                           
004820     03  WS-COMMIT-FREQUENCY      PIC S9(9)V9(2)  COMP-3.                 
004830     03  WS-COMMIT-COUNT          PIC S9(9)V9(2)  COMP-3.                 
004840                                                                          
004850 01  WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.                  
004860                                                                          
005000 01  ERRORTEXT.                                                           
005100     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
005200     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
005700     EJECT                                                                
005710                                                                          
005720*    --- SUBPROGRAMS OCH PARAMETER AREAS.                                 
005730 01  GENERAL-SUBPROGRAMS.                                                 
005740     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
005770     EJECT                                                                
005771                                                                          
005790*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005792 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
005793 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
005794 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
005795 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
005796     SKIP2                                                                
005797                                                                          
005802 01  MESSAGE-CODES.                                                       
005803     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005804     EJECT                                                                
005805                                                                          
006400 01  DYNAMIC-SUBPROGRAMS.                                                 
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  W980SOP                 PIC X(8)    VALUE 'WW980SOP'.            
007000     EJECT                                                                
008100                                                                          
008200 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
008300     SKIP3                                                                
008400*    -COPY WZ20DAYS                                                       
008500     EJECT                                                                
008600                                                                          
010503 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010504       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010505                                                                          
010506 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010507 01  DB2-WS.                                                              
010508     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
010509         88  CURSOR-OK                       VALUE 000.                   
010510         88  LINES-FOUND                     VALUE 000.                   
010511         88  LINES-MISSING                   VALUE 100.                   
010512         88  RESOURCE-WRONG                  VALUE 904.                   
010513                                                                          
010534     03  T01SYST-WS              PIC 9(3)   VALUE ZERO.                   
010535        88 T01SYST-OK                       VALUE 000.                    
010536        88 T01SYST-MISSING                  VALUE 100.                    
010537        88 T01SYST-ERROR                    VALUE 904.                    
010538                                                                          
010574     03  GOOD-SQLCODECODES.                                               
010575         05  GOOD-SQLCODE OCCURS 5                                        
010576             INDEXED BY SQLCODE-IX PIC 9(3).                              
010580                                                                          
010590 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010591 01  WS-AREA.                                                             
010592     03 WS-CURRENT-DATE        PIC X(8)  VALUE SPACE.                     
010593     03 WS-DELDATUM            PIC X(8)  VALUE SPACE.                     
010594     03 WS-IDLEGSEL            PIC X(4)  VALUE SPACE.                     
010595     03 WS-DAEXDAT             PIC X(8)  VALUE SPACE.                     
010597     03 WS-TIEXTID             PIC S9(7) COMP-3 VALUE ZERO.               
010599                                                                          
010712     03 SYST-KVDAGAR           PIC S9(3) VALUE ZERO COMP-3.               
010713                                                                          
010714     03 ABUN-IDLEGSEL          PIC X(4)  VALUE SPACE.                     
010715     03 ABUN-IDBUNDLE          PIC X(15) VALUE SPACE.                     
010716     03 ABUN-DAREGDAT          PIC X(8)  VALUE SPACE.                     
010717     03 ABUN-TIREGTID          PIC S9(10) COMP-3 VALUE ZERO.              
010718     03 TBUN-IDLEGSEL          PIC X(4)  VALUE SPACE.                     
010719     03 TBUN-IDBUNDLE          PIC X(15) VALUE SPACE.                     
010720     03 TBUN-DAREGDAT          PIC X(8)  VALUE SPACE.                     
010721     03 TBUN-TIREGTID          PIC S9(10) COMP-3 VALUE ZERO.              
010722                                                                          
010730     EJECT                                                                
010800                                                                          
010937 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
010939*01  -COPY T01SYST -PRE T01SYST-                                          
010940     EJECT                                                                
011101                                                                          
011102 01  FILLER                    PIC X(16)    VALUE 'T01TRAW-AREA'.         
011103*01  -COPY T01TRAW -PRE T01TRAW-                                          
011104     EJECT                                                                
011105                                                                          
011106 01  FILLER                    PIC X(16)    VALUE 'T01TBUN-AREA'.         
011107*01  -COPY T01TBUN -PRE T01TBUN-                                          
011108     EJECT                                                                
011109                                                                          
011110 01  FILLER                    PIC X(16)    VALUE 'T01ALIN-AREA'.         
011111*01  -COPY T01ALIN -PRE T01ALIN-                                          
011112     EJECT                                                                
011113                                                                          
011114 01  FILLER                    PIC X(16)    VALUE 'T01ABUN-AREA'.         
011115*01  -COPY T01ABUN -PRE T01ABUN-                                          
011116     EJECT                                                                
011117                                                                          
011196     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
011197     EJECT                                                                
011205     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
011206     EJECT                                                                
011207     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
011208     EJECT                                                                
011209     EXEC SQL INCLUDE T01ALIN END-EXEC.                                   
011210     EJECT                                                                
011211     EXEC SQL INCLUDE T01ABUN END-EXEC.                                   
011212     EJECT                                                                
011221                                                                          
011230 LINKAGE SECTION.                                                         
011901 PROCEDURE DIVISION.                                                      
011902 MAIN SECTION.                                                            
012210     PERFORM A-INIT                                                       
012220                                                                          
014230     PERFORM B-DELETE-T01TRAW-T01TBUN                                     
014240     PERFORM C-DELETE-T01ALIN-T01ABUN                                     
014290                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014601                                                                          
014602 A-INIT SECTION.                                                          
014603     MOVE +100                        TO WS-COMMIT-FREQUENCY              
014604     MOVE +0                          TO WS-COMMIT-COUNT                  
014605                                                                          
014606     PERFORM DB2-SELECT-T01SYST                                           
014607                                                                          
014608     MOVE YES TO KEYS-SW                                                  
014609     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
014610     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
014611     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
014612     MOVE SYST-KVDAGAR                TO DAYS-KVDAYS                      
014613     MOVE 'WEEKDAYS'                  TO DAYS-IDCALEND                    
014614     MOVE SPACE                       TO DAYS-TIDATE1                     
014615     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
014616     CALL WZ20DAYS USING                                                  
014617          DAYS-WZ20DAYS                                                   
014618     IF DAYS-KDRC = ZERO                                                  
014619       MOVE DAYS-TIDATE1              TO WS-DELDATUM                      
014620     ELSE                                                                 
014621       MOVE NOO                       TO KEYS-SW                          
014622     END-IF                                                               
014628     .                                                                    
014629     EJECT                                                                
014630                                                                          
014631 B-DELETE-T01TRAW-T01TBUN SECTION.                                        
014632     PERFORM DB2-OPEN-T01TRAW-CRS                                         
014633     PERFORM DB2-FETCH-T01TRAW-CRS                                        
014634     PERFORM UNTIL LINES-MISSING                                          
014635       PERFORM DB2-SELECT-T01TBUN-VIA-TRAW                                
014636       IF LINES-MISSING                                                   
014637         PERFORM DB2-DELETE-T01TRAW                                       
014638         IF WS-COMMIT-COUNT = WS-COMMIT-FREQUENCY                         
014639           PERFORM DB2-COMMIT-WORK                                        
014640           MOVE ZERO TO WS-COMMIT-COUNT                                   
014641           PERFORM DB2-CLOSE-T01TRAW-CRS                                  
014642           PERFORM DB2-OPEN-T01TRAW-CRS                                   
014643         END-IF                                                           
014644       END-IF                                                             
014645       PERFORM DB2-FETCH-T01TRAW-CRS                                      
014646     END-PERFORM                                                          
014647     PERFORM DB2-CLOSE-T01TRAW-CRS                                        
014659     .                                                                    
014660     EJECT                                                                
014661                                                                          
014662 C-DELETE-T01ALIN-T01ABUN SECTION.                                        
014677     PERFORM DB2-OPEN-T01ABUN-CRS                                         
014678     PERFORM DB2-FETCH-T01ABUN-CRS                                        
014679     PERFORM UNTIL LINES-MISSING                                          
014680       PERFORM DB2-SELECT-T01ALIN-VIA-ABUN                                
014681       IF LINES-MISSING                                                   
014682         PERFORM DB2-DELETE-T01ABUN                                       
014683         IF WS-COMMIT-COUNT = WS-COMMIT-FREQUENCY                         
014684           PERFORM DB2-COMMIT-WORK                                        
014685           MOVE ZERO TO WS-COMMIT-COUNT                                   
014686           PERFORM DB2-CLOSE-T01ABUN-CRS                                  
014687           PERFORM DB2-OPEN-T01ABUN-CRS                                   
014688         END-IF                                                           
014689       END-IF                                                             
014690       PERFORM DB2-FETCH-T01ABUN-CRS                                      
014691     END-PERFORM                                                          
014692     PERFORM DB2-CLOSE-T01ABUN-CRS                                        
014693     .                                                                    
014694     EJECT                                                                
014695                                                                          
021125 DB2-SELECT-T01SYST SECTION.                                              
021126     MOVE 000     TO GOOD-SQLCODECODES                                    
021127     EXEC SQL                                                             
021128         SELECT KVDAGAR                                                   
021129                                                                          
021138         INTO :SYST-KVDAGAR                                               
021139                                                                          
021140         FROM    T01SYST                                                  
021141                                                                          
021142         WHERE   IDSYSTEM = 'WF02'                                        
021144     END-EXEC                                                             
021146     MOVE SQLCODE TO SQLCODE-WS                                           
021147                     T01SYST-WS                                           
021148     PERFORM DB2-STATUS-CHECK                                             
021149     .                                                                    
021150     EJECT                                                                
021151                                                                          
021331 DB2-OPEN-T01TRAW-CRS SECTION.                                            
021332     MOVE 000100  TO GOOD-SQLCODECODES                                    
021333     EXEC SQL DECLARE T01TRAW-CRS CURSOR WITH HOLD FOR                    
021334       SELECT                                                             
021335              T01TRAW.IDLEGSEL                                            
021336             ,T01TRAW.IDBUNDLE                                            
021337             ,T01TRAW.DAREGDAT                                            
021338             ,T01TRAW.TIREGTID                                            
021339             ,T01TRAW.IDREF                                               
021340             ,T01TRAW.DAREFDAT                                            
021341             ,T01TRAW.IDREFRAD                                            
021342                                                                          
021343       FROM   T01TRAW                                                     
021345                                                                          
021347       WHERE  DAREGDAT < :WS-DELDATUM                                     
021348                                                                          
021352     END-EXEC                                                             
021353                                                                          
021354     MOVE 000100         TO GOOD-SQLCODECODES                             
021355     EXEC SQL OPEN T01TRAW-CRS END-EXEC                                   
021356     MOVE SQLCODE        TO SQLCODE-WS                                    
021357     PERFORM DB2-STATUS-CHECK                                             
021358     .                                                                    
021359     EJECT                                                                
021360                                                                          
021361 DB2-FETCH-T01TRAW-CRS SECTION.                                           
021362     MOVE 000100       TO GOOD-SQLCODECODES                               
021363     EXEC SQL FETCH T01TRAW-CRS INTO                                      
021364            :T01TRAW-IDLEGSEL                                             
021365           ,:T01TRAW-IDBUNDLE                                             
021366           ,:T01TRAW-DAREGDAT                                             
021367           ,:T01TRAW-TIREGTID                                             
021368           ,:T01TRAW-IDREF                                                
021369           ,:T01TRAW-DAREFDAT                                             
021370           ,:WS-IDREFRAD                                                  
021371     END-EXEC                                                             
021372                                                                          
021373     MOVE SQLCODE        TO SQLCODE-WS                                    
021374     PERFORM DB2-STATUS-CHECK                                             
021375     .                                                                    
021376     EJECT                                                                
021377                                                                          
021378 DB2-CLOSE-T01TRAW-CRS SECTION.                                           
021379     EXEC SQL CLOSE T01TRAW-CRS                                           
021380     END-EXEC                                                             
021381     .                                                                    
021382     EJECT                                                                
021383                                                                          
021384 DB2-SELECT-T01TBUN-VIA-TRAW SECTION.                                     
021385     EXEC SQL                                                             
021387       SELECT DISTINCT                                                    
021388              IDLEGSEL                                                    
021389             ,IDBUNDLE                                                    
021390             ,DAREGDAT                                                    
021391             ,TIREGTID                                                    
021392                                                                          
021393       INTO  :T01TBUN-IDLEGSEL                                            
021394            ,:T01TBUN-IDBUNDLE                                            
021395            ,:T01TBUN-DAREGDAT                                            
021396            ,:T01TBUN-TIREGTID                                            
021397                                                                          
021398       FROM   T01TBUN                                                     
021399                                                                          
021405       WHERE  IDLEGSEL = :T01TRAW-IDLEGSEL                                
021406       AND    IDBUNDLE = :T01TRAW-IDBUNDLE                                
021407       AND    DAREGDAT = :T01TRAW-DAREGDAT                                
021408       AND    TIREGTID = :T01TRAW-TIREGTID                                
021409     END-EXEC                                                             
021410                                                                          
021411     MOVE 000100  TO GOOD-SQLCODECODES                                    
021412     MOVE SQLCODE        TO SQLCODE-WS                                    
021413     PERFORM DB2-STATUS-CHECK                                             
021414     .                                                                    
021415     EJECT                                                                
021416                                                                          
021417 DB2-DELETE-T01TRAW SECTION.                                              
021418     MOVE 000     TO GOOD-SQLCODECODES                                    
021419     EXEC SQL                                                             
021420        DELETE FROM T01TRAW                                               
021421                                                                          
021422        WHERE  T01TRAW.IDLEGSEL = :T01TRAW-IDLEGSEL                       
021423        AND    T01TRAW.IDBUNDLE = :T01TRAW-IDBUNDLE                       
021424        AND    T01TRAW.DAREGDAT = :T01TRAW-DAREGDAT                       
021425        AND    T01TRAW.TIREGTID = :T01TRAW-TIREGTID                       
021426        AND    T01TRAW.IDREF    = :T01TRAW-IDREF                          
021427        AND    T01TRAW.DAREFDAT = :T01TRAW-DAREFDAT                       
021428        AND    T01TRAW.IDREFRAD = :WS-IDREFRAD                            
021429     END-EXEC                                                             
021430                                                                          
021431     MOVE SQLCODE TO SQLCODE-WS                                           
021432     ADD +1       TO WS-COMMIT-COUNT                                      
021433     PERFORM DB2-STATUS-CHECK                                             
021434     .                                                                    
021435     EJECT                                                                
021436                                                                          
021437 DB2-OPEN-T01ABUN-CRS SECTION.                                            
021438     MOVE 000100  TO GOOD-SQLCODECODES                                    
021439     EXEC SQL DECLARE T01ABUN-CRS CURSOR WITH HOLD FOR                    
021440       SELECT                                                             
021441              IDLEGSEL                                                    
021442             ,IDBUNDLE                                                    
021443             ,DAREGDAT                                                    
021444             ,TIREGTID                                                    
021445                                                                          
021446       FROM   T01ABUN                                                     
021447                                                                          
021448       WHERE  DAREGDAT < :WS-DELDATUM                                     
021449     END-EXEC                                                             
021450                                                                          
021451     MOVE 000100         TO GOOD-SQLCODECODES                             
021452     EXEC SQL OPEN T01ABUN-CRS END-EXEC                                   
021453     MOVE SQLCODE        TO SQLCODE-WS                                    
021454     PERFORM DB2-STATUS-CHECK                                             
021455     .                                                                    
021456     EJECT                                                                
021457                                                                          
021458 DB2-FETCH-T01ABUN-CRS SECTION.                                           
021459     MOVE 000100       TO GOOD-SQLCODECODES                               
021460     EXEC SQL FETCH T01ABUN-CRS INTO                                      
021461            :T01ABUN-IDLEGSEL                                             
021462           ,:T01ABUN-IDBUNDLE                                             
021463           ,:T01ABUN-DAREGDAT                                             
021464           ,:T01ABUN-TIREGTID                                             
021465     END-EXEC                                                             
021466                                                                          
021467     MOVE SQLCODE        TO SQLCODE-WS                                    
021468     PERFORM DB2-STATUS-CHECK                                             
021469     .                                                                    
021470     EJECT                                                                
021471                                                                          
021472 DB2-CLOSE-T01ABUN-CRS SECTION.                                           
021473     SKIP2                                                                
021474     EXEC SQL CLOSE T01ABUN-CRS                                           
021475     END-EXEC                                                             
021476     .                                                                    
021477     EJECT                                                                
021478                                                                          
021479 DB2-SELECT-T01ALIN-VIA-ABUN SECTION.                                     
021480     EXEC SQL                                                             
021481       SELECT DISTINCT                                                    
021482              IDLEGSEL                                                    
021483             ,IDBUNDLE                                                    
021484             ,DAREGDAT                                                    
021485             ,TIREGTID                                                    
021486                                                                          
021487       INTO  :T01ALIN-IDLEGSEL                                            
021488            ,:T01ALIN-IDBUNDLE                                            
021489            ,:T01ALIN-DAREGDAT                                            
021490            ,:T01ALIN-TIREGTID                                            
021491                                                                          
021492       FROM   T01ALIN                                                     
021493                                                                          
021494       WHERE  IDLEGSEL = :T01ABUN-IDLEGSEL                                
021495       AND    IDBUNDLE = :T01ABUN-IDBUNDLE                                
021496       AND    DAREGDAT = :T01ABUN-DAREGDAT                                
021497       AND    TIREGTID = :T01ABUN-TIREGTID                                
021500     END-EXEC                                                             
021501                                                                          
021502     MOVE 000100  TO GOOD-SQLCODECODES                                    
021503     MOVE SQLCODE        TO SQLCODE-WS                                    
021504     PERFORM DB2-STATUS-CHECK                                             
021505     .                                                                    
021506     EJECT                                                                
021507                                                                          
021508 DB2-DELETE-T01ABUN SECTION.                                              
021509     MOVE 000    TO GOOD-SQLCODECODES                                     
021510     EXEC SQL                                                             
021511        DELETE FROM T01ABUN                                               
021512                                                                          
021513        WHERE CURRENT OF T01ABUN-CRS                                      
021514     END-EXEC                                                             
021515                                                                          
021516     MOVE SQLCODE TO SQLCODE-WS                                           
021517     PERFORM DB2-STATUS-CHECK                                             
021518     .                                                                    
021519     EJECT                                                                
021520                                                                          
021848 DB2-COMMIT-WORK SECTION.                                                 
021849     EXEC SQL COMMIT WORK                                                 
021850     END-EXEC                                                             
021851     .                                                                    
021852     EJECT                                                                
021853                                                                          
021854 DB2-STATUS-CHECK     SECTION.                                            
021855     SET SQLCODE-IX TO 1                                                  
021856     SEARCH GOOD-SQLCODE                                                  
021857       AT END                                                             
021858          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
021859          DELIMITED BY SIZE INTO ERRORTEXT                                
021860          CALL ABEND USING RKOD-ABEND-DB2                                 
021861       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
021870     END-SEARCH                                                           
021900     .                                                                    
