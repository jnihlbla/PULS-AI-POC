000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF201600.                                                
001300 AUTHOR.         ANDERS HENRIKSSON.                                       
001400 DATE-WRITTEN.   02-04-19.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   CREATES FILE (A/R DATA) FROM DOCUMENT-TYPE AND                        
002000*   BUSINESS RELATION TABLES.                                             
002410*                                                                         
002430*   PGM READS                                                             
002440*   - ROWS IN TABLE T01PROC                                               
002441*   - ROWS IN TABLE T01DHEA                                               
002450*   - ROWS IN TABLE T01DOTY                                               
002460*   - ROWS IN TABLE T01BURE                                               
002470*   - ROWS IN TABLE T01FCUS                                               
002480*   - ROWS IN TABLE T01LSEL                                               
002500                                                                          
002600**** VI HAR KVAR ATT LÄGGA TILL REVALUTA                                  
002700 ENVIRONMENT DIVISION.                                                    
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003012*    ---- UTFIL: GENERAL LEDGER DATA                                      
003013     SELECT  WF2016        ASSIGN TO WF2016D1.                            
003014                                                                          
003015     SELECT  WF2026        ASSIGN TO WF2016D2.                            
003016     EJECT                                                                
003017                                                                          
003018 DATA DIVISION.                                                           
003019 FILE SECTION.                                                            
003020 FD  WF2016                                                               
003021     RECORDING  F                                                         
003022     BLOCK CONTAINS 0.                                                    
003030                                                                          
003040*01  POST-WF2016  -COPY WF2016  -L.                                       
003041                                                                          
003042 FD  WF2026                                                               
003043     RECORDING  F                                                         
003044     BLOCK CONTAINS 0.                                                    
003045                                                                          
003046*01  POST-WF2026  -COPY WF2016  -L.                                       
003050     EJECT                                                                
003060                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110                                                                          
004200 77  IDPGM                        PIC X(8)   VALUE 'WF201600'.            
004430     EJECT                                                                
004440                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006320*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
006330 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006340 77  KDRC-DISPLAY                PIC Z(5).                                
006341 77  WS-IDLEGSEL-CRS             PIC X(4).                                
006350                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006503                                                                          
006504 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
006505                                 'OUTPUT-TAB   '.                         
006506*01  -COPY WF2016T                                                        
006507                                                                          
006508 01  UTFIL-AREA-START            PIC X(24)   VALUE                        
006509                                             'UTFIL-AREA-START'.          
006510     SKIP2                                                                
006511*01  -COPY WF2016 -PRE WS-                                                
006520     EJECT                                                                
006550                                                                          
006560 01  WS-AREA.                                                             
006570     03 WS-IDLEGSEL          OCCURS 100  PIC X(4).                        
006580     03 WS-KDVALISO          OCCURS 100  PIC X(3).                        
006581     03 WS-PRKURS            OCCURS 100  PIC S9(6)V9(5) COMP-3.           
006582     03 WS-IDLANDX3-SEND     OCCURS 100  PIC X(3).                        
006583     03 WS-IDPARTNR          OCCURS 100  PIC X(9).                        
006584     03 WS-KDFINDOC          OCCURS 100  PIC X(4).                        
006585     03 WS-FLFREE            OCCURS 100  PIC X(1).                        
006586     03 WS-DAFINDOC          OCCURS 100  PIC X(4).                        
006587     03 WS-IDFINDOC          OCCURS 100  PIC S9(9) COMP-3.                
006588     03 WS-IDLANDX3-BET      OCCURS 100  PIC X(3).                        
006589     03 WS-SUNTO-SERV        OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006590     03 WS-SUBTO-SERV        OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006591     03 WS-SUNTO-PART        OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006592     03 WS-SUBTO-PART        OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006593     03 WS-SUNTO-TOT         OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006594     03 WS-SUBTO-TOT         OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006596     03 WS-SUVAT-BILLIT-TOT  OCCURS 100  PIC S9(11)V9(2) COMP-3.          
006597     03 WS-KDTRADP           OCCURS 100  PIC X(4).                        
006598                                                                          
006780     03 DOTY-FLAR            OCCURS 100  PIC X(1).                        
006790     03 BURE-FLAR            OCCURS 100  PIC X(1).                        
006791                                                                          
006793     03 PROC-IDLEGSEL           PIC X(4)     VALUE SPACE.                 
006794     03 PROC-DAEXDAT            PIC X(8)     VALUE SPACE.                 
006795     03 PROC-TIEXTID            PIC S9(7)    VALUE ZERO COMP-3.           
006796     03 PROC-KDBEH              PIC X(1)     VALUE SPACE.                 
006800                                                                          
006900 01  WS-DIVERSE-MULTIFETCH.                                               
007000     03 WS-MX                    PIC S9(3)  COMP-3.                       
007100     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
007200                                                                          
010602*******  WORK-AREAS FOR DB2-SECTIONS                                      
010603                                                                          
010604 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA  '.         
010605*01  -COPY T01PROC    -PRE T01PROC-                                       
010606                                                                          
010607 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA  '.         
010608*01  -COPY T01DHEA    -PRE T01DHEA-                                       
010609                                                                          
010614 01  FILLER                       PIC X(16)  VALUE 'DOTY-AREA  '.         
010615*01  -COPY T01DOTY    -PRE T01DOTY-                                       
010616                                                                          
010617 01  FILLER                       PIC X(16)  VALUE 'BURE-AREA  '.         
010618*01  -COPY T01BURE    -PRE T01BURE-                                       
010619                                                                          
010620 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA  '.         
010621*01  -COPY T01FCUS    -PRE T01FCUS-                                       
010622                                                                          
010623 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA  '.         
010624*01  -COPY T01LSEL    -PRE T01LSEL-                                       
010625     EJECT                                                                
010626                                                                          
010627     EXEC SQL INCLUDE T01PROC  END-EXEC.                                  
010628     EJECT                                                                
010630     EXEC SQL INCLUDE T01DHEA  END-EXEC.                                  
010631     EJECT                                                                
010636     EXEC SQL INCLUDE T01DOTY  END-EXEC.                                  
010637     EJECT                                                                
010639     EXEC SQL INCLUDE T01BURE  END-EXEC.                                  
010643     EJECT                                                                
010644     EXEC SQL INCLUDE T01FCUS  END-EXEC.                                  
010645     EJECT                                                                
010646     EXEC SQL INCLUDE T01LSEL  END-EXEC.                                  
010647     EJECT                                                                
010648                                                                          
010649 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
010650       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010651                                                                          
010652***** STATUS-CODE FROM DB2                                                
010653 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
010654                                                                          
010655 01  DB2-WS.                                                              
010656   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
010657     88  CURSOR-OK                           VALUE +000.                  
010658     88  LINES-FOUND                         VALUE +000.                  
010659     88  LINES-MISSING                       VALUE +100.                  
010660     88  RESOURCE-WRONG                      VALUE 904.                   
010661                                                                          
010662   03  GOOD-SQLCODES.                                                     
010663     05  GOOD-SQLCODE OCCURS 5                                            
010664         INDEXED BY SQLCODE-IX    PIC 999.                                
010665     EJECT                                                                
010670                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
012500                                                                          
012510     PERFORM Z-FINISH                                                     
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012910                                                                          
013000 A-INIT SECTION.                                                          
013110     OPEN OUTPUT WF2016                                                   
013120                 WF2026                                                   
014100     .                                                                    
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014987     PERFORM DB2-OPEN-CRS-LSEL                                            
014988     PERFORM DB2-FETCH-CRS-LSEL                                           
014989     PERFORM UNTIL LINES-MISSING                                          
014990       PERFORM DB2-SELECT-T01PROC                                         
014991                                                                          
014992       PERFORM DB2-DCL-OPN-CRS1-ALL                                       
014994       PERFORM DB2-FETCH-CRS1-ALL                                         
014995       IF SQLERRD(3) > 0                                                  
014996         MOVE 000     TO SQLCODE-WS                                       
014997       END-IF                                                             
014998       PERFORM UNTIL LINES-MISSING                                        
014999         MOVE SQLERRD(3) TO WS-MULTIFETCH                                 
015000         MOVE ZERO       TO WS-MX                                         
015001         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
015002           ADD +1        TO WS-MX                                         
015003**** INT2 SHOULD NOT BE IN FILE                                           
015004           IF WS-KDFINDOC(WS-MX) = 'INT2'                                 
015005             CONTINUE                                                     
015006           ELSE                                                           
015007             PERFORM F-CREATE-SEQUENCEFILE                                
015008             PERFORM S11-WRITE-WF20X6                                     
015009           END-IF                                                         
015010         END-PERFORM                                                      
015011         IF WS-MULTIFETCH = 100                                           
015012           PERFORM DB2-FETCH-CRS1-ALL                                     
015013           IF SQLERRD(3) > 0                                              
015014             MOVE 000     TO SQLCODE-WS                                   
015015           END-IF                                                         
015016         ELSE                                                             
015017           MOVE 100     TO SQLCODE-WS                                     
015018         END-IF                                                           
015019       END-PERFORM                                                        
015020       PERFORM DB2-CLOSE-CRS1-ALL                                         
015030                                                                          
015040       PERFORM DB2-FETCH-CRS-LSEL                                         
015050     END-PERFORM                                                          
015060     PERFORM DB2-CLOSE-CRS-LSEL                                           
015103     .                                                                    
015105                                                                          
015106 F-CREATE-SEQUENCEFILE SECTION.                                           
015109     MOVE  WS-IDLEGSEL(WS-MX)         TO WS-AR-IDLEGSEL                   
015110     MOVE  WS-KDVALISO(WS-MX)         TO WS-AR-KDVALISO                   
015111     MOVE  WS-PRKURS(WS-MX)           TO WS-AR-PRKURS                     
015112     MOVE  WS-IDLANDX3-SEND(WS-MX)    TO WS-AR-IDLANDX3-SEND              
015113     MOVE  WS-IDPARTNR(WS-MX)         TO WS-AR-IDPARTNR                   
015114     MOVE  WS-KDFINDOC(WS-MX)         TO WS-AR-KDFINDOC                   
015116     MOVE  WS-DAFINDOC(WS-MX)         TO WS-AR-DAFINDOC                   
015117     MOVE  WS-IDFINDOC(WS-MX)         TO WS-AR-IDFINDOC                   
015118     MOVE  WS-IDLANDX3-BET(WS-MX)     TO WS-AR-IDLANDX3-BET               
015119     MOVE  WS-SUNTO-SERV(WS-MX)       TO WS-AR-SUNTO-SERV                 
015120     MOVE  WS-SUBTO-SERV(WS-MX)       TO WS-AR-SUBTO-SERV                 
015121     MOVE  WS-SUNTO-PART(WS-MX)       TO WS-AR-SUNTO-PART                 
015122     MOVE  WS-SUBTO-PART(WS-MX)       TO WS-AR-SUBTO-PART                 
015123     MOVE  WS-SUNTO-TOT(WS-MX)        TO WS-AR-SUNTO-TOT                  
015124     MOVE  WS-SUBTO-TOT(WS-MX)        TO WS-AR-SUBTO-TOT                  
015125     MOVE  WS-SUVAT-BILLIT-TOT(WS-MX) TO WS-AR-SUVAT-BILLIT-TOT           
015155     .                                                                    
015157                                                                          
015158 S11-WRITE-WF20X6 SECTION.                                                
015160     IF WS-AR-IDLEGSEL        = 'VCCS'                                    
015161       WRITE POST-WF2016     FROM WS-AR-WF2016                            
015162     ELSE                                                                 
015163       IF WS-AR-IDLEGSEL(1:2) = 'SC'                                      
015164         WRITE POST-WF2026   FROM WS-AR-WF2016                            
015165       END-IF                                                             
015166     END-IF                                                               
015167     .                                                                    
015170                                                                          
015197 Z-FINISH SECTION.                                                        
015199     CLOSE WF2016                                                         
015200     CLOSE WF2026                                                         
015202     .                                                                    
015204                                                                          
015215* --- DB2 SECTIONS  ---                                                   
015216 DB2-SELECT-T01PROC SECTION.                                              
015217     MOVE 000100  TO GOOD-SQLCODES                                        
015218     EXEC SQL                                                             
015220         SELECT IDLEGSEL                                                  
015221              , DAEXDAT                                                   
015222              , TIEXTID                                                   
015223              , KDBEH                                                     
015224                                                                          
015231         INTO :PROC-IDLEGSEL                                              
015232            , :PROC-DAEXDAT                                               
015233            , :PROC-TIEXTID                                               
015234            , :PROC-KDBEH                                                 
015240                                                                          
015250         FROM    T01PROC                                                  
015260                                                                          
015270         WHERE   IDSYSTEM = 'WF02'           AND                          
015271                 IDLEGSEL = :WS-IDLEGSEL-CRS                              
015280     END-EXEC                                                             
015290     MOVE SQLCODE TO SQLCODE-WS                                           
015400     PERFORM DB2-STATUS-CHECK                                             
015500     .                                                                    
015700                                                                          
017220 DB2-DCL-OPN-CRS1-ALL SECTION.                                            
017231     MOVE 000100 TO GOOD-SQLCODES                                         
017240     EXEC SQL                                                             
017241        DECLARE CRS1 CURSOR WITH ROWSET POSITIONING FOR                   
017250        SELECT A.IDLEGSEL                                                 
017251             , A.KDVALISO                                                 
017260             , A.PRKURS                                                   
017261             , A.IDLANDX3_SEND                                            
017262             , A.IDPARTNR                                                 
017263             , A.KDFINDOC                                                 
017265             , A.DAFINDOC                                                 
017266             , A.IDFINDOC                                                 
017270             , A.IDLANDX3_BET                                             
017271             , A.SUNTO_SERV                                               
017272             , A.SUBTO_SERV                                               
017273             , A.SUNTO_PART                                               
017274             , A.SUBTO_PART                                               
017275             , A.SUNTO_TOT                                                
017276             , A.SUBTO_TOT                                                
017278             , A.SUVAT_BILLIT_TOT                                         
017497             , C.FLAR                                                     
017498             , D.FLAR                                                     
017499                                                                          
017500        FROM   T01DHEA A                                                  
017502             , T01DOTY C                                                  
017503             , T01BURE D                                                  
017504             , T01FCUS E                                                  
017505                                                                          
017507        WHERE  A.IDLEGSEL = :PROC-IDLEGSEL                                
017508        AND    A.DAEXDAT  = :PROC-DAEXDAT                                 
017509        AND    A.TIEXTID  = :PROC-TIEXTID                                 
017510        AND    A.FLFREE   = 'N'                                           
017511        AND    C.IDLEGSEL = A.IDLEGSEL                                    
017512        AND    C.KDFINDOC = A.KDFINDOC                                    
017513        AND    C.KDSTATUS = 001                                           
017514        AND    C.DADELDAT = '00000000'                                    
017515        AND    C.FLAR     = 'J'                                           
017516        AND    E.IDLEGSEL = A.IDLEGSEL                                    
017517        AND    E.IDPARTNR = A.IDPARTNR                                    
017518        AND    E.KDSTATUS  = 001                                          
017519        AND    E.DADELDAT = '00000000'                                    
017520        AND    D.IDLEGSEL = A.IDLEGSEL                                    
017521        AND    D.KDFINDOC = A.KDFINDOC                                    
017522        AND    D.KDPARTTY = E.KDPARTTY                                    
017523        AND    D.KDPARTGR = E.KDPARTGR                                    
017524        AND    D.FLAR     = 'J'                                           
017525        AND    D.KDSTATUS  = 001                                          
017526        AND    D.DADELDAT = '00000000'                                    
017527     END-EXEC                                                             
017528                                                                          
017529     EXEC SQL                                                             
017530        OPEN CRS1                                                         
017531     END-EXEC                                                             
017532                                                                          
017533     MOVE SQLCODE        TO SQLCODE-WS                                    
017534     PERFORM DB2-STATUS-CHECK                                             
017535     .                                                                    
017537                                                                          
017538 DB2-FETCH-CRS1-ALL SECTION.                                              
017539     MOVE 000100         TO GOOD-SQLCODES                                 
017540                                                                          
017541     EXEC SQL                                                             
017542            FETCH NEXT ROWSET FROM CRS1 FOR 100 ROWS                      
017543            INTO    :WS-IDLEGSEL                                          
017544                  , :WS-KDVALISO                                          
017550                  , :WS-PRKURS                                            
017560                  , :WS-IDLANDX3-SEND                                     
017570                  , :WS-IDPARTNR                                          
017580                  , :WS-KDFINDOC                                          
017591                  , :WS-DAFINDOC                                          
017592                  , :WS-IDFINDOC                                          
017593                  , :WS-IDLANDX3-BET                                      
017594                  , :WS-SUNTO-SERV                                        
017595                  , :WS-SUBTO-SERV                                        
017596                  , :WS-SUNTO-PART                                        
017597                  , :WS-SUBTO-PART                                        
017598                  , :WS-SUNTO-TOT                                         
017599                  , :WS-SUBTO-TOT                                         
017601                  , :WS-SUVAT-BILLIT-TOT                                  
017892                  , :DOTY-FLAR                                            
017893                  , :BURE-FLAR                                            
017900     END-EXEC                                                             
017910                                                                          
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
019011                                                                          
019306 DB2-CLOSE-CRS1-ALL SECTION.                                              
019308     EXEC SQL                                                             
019309        CLOSE CRS1                                                        
019310     END-EXEC                                                             
019311     .                                                                    
019313                                                                          
019314 DB2-OPEN-CRS-LSEL SECTION.                                               
019315     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
019316     SELECT   T01LSEL.IDLEGSEL                                            
019317                                                                          
019318     FROM     T01LSEL                                                     
019319                                                                          
019320     WHERE    KDSTATUS = 1                                                
019330     END-EXEC                                                             
019340                                                                          
019350     EXEC SQL OPEN T01LSEL-CRS                                            
019360     END-EXEC                                                             
019370                                                                          
019380     MOVE 000            TO GOOD-SQLCODES                                 
019381     MOVE SQLCODE        TO SQLCODE-WS                                    
019382     PERFORM DB2-STATUS-CHECK                                             
019383     .                                                                    
019384                                                                          
019385 DB2-FETCH-CRS-LSEL SECTION.                                              
019386     EXEC SQL FETCH T01LSEL-CRS INTO                                      
019387            :WS-IDLEGSEL-CRS                                              
019388     END-EXEC                                                             
019389                                                                          
019390     MOVE 000100         TO GOOD-SQLCODES                                 
019391     MOVE SQLCODE        TO SQLCODE-WS                                    
019392     PERFORM DB2-STATUS-CHECK                                             
019393     .                                                                    
019394                                                                          
019395 DB2-CLOSE-CRS-LSEL SECTION.                                              
019396     EXEC SQL CLOSE T01LSEL-CRS                                           
019397     END-EXEC                                                             
019398     .                                                                    
019399                                                                          
019400 DB2-STATUS-CHECK  SECTION.                                               
019401     SET SQLCODE-IX TO 1                                                  
019402     SEARCH GOOD-SQLCODE                                                  
019403       AT END                                                             
019404          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
019405          DELIMITED BY SIZE INTO ERROR-TEXT                               
019406          CALL ABEND USING RKOD-ABEND-DB2                                 
019407       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
019408          CONTINUE                                                        
019410     END-SEARCH                                                           
019500     .                                                                    
