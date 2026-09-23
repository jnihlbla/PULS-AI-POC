000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF108900.                                                
001400 AUTHOR.         P-A FORSBERG.                                            
001500 DATE-WRITTEN.   07/01/03.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*                                                                         
001900*    FUNCTION:                                                            
002710*        PGM READS   ROWS IN TABLE  T01SYST                               
002800*                                                                         
002811*        PGM TAKES CURRENT DATE MINUS NUMBER OF DAYS THE                  
002812*        DATA WILL BE KEPT (KVDAGAR FROM T01SYST) AND CREATES             
002813*        AN OUTPUT FILE FOR DELETE OF ROWS IN                             
002814*        - TABLE T01TBUN                                                  
002815*        - TABLE T01TRAW                                                  
002816*        - TABLE T01SLIN                                                  
002817*        - TABLE T01DHEA                                                  
002818*        - TABLE T01DLIN                                                  
002819*        - TABLE T01DAPP                                                  
002820*                                                                         
002850*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003500 FILE-CONTROL.                                                            
003600     SELECT WF10FIL                    ASSIGN TO WF1089D1.                
003900 DATA DIVISION.                                                           
004100 FILE SECTION.                                                            
004200                                                                          
004300 FD  WF10FIL                                                              
004310     RECORDING       F                                                    
004320     BLOCK CONTAINS  0.                                                   
004330                                                                          
004340 01  WF10POST                    PIC X(80).                               
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'WF108900'.            
004610                                                                          
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004812     EJECT                                                                
004813                                                                          
004814 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004815     88  KEYS-OK                            VALUE 'J'.                    
004816     88  KEYS-ERROR                         VALUE 'N'.                    
004817                                                                          
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
010592     03 WS-FILLER1             PIC X(8)  VALUE SPACE.                     
010593     03 WS-DAEXDAT             PIC X(4) VALUE " < '".                     
010595     03 WS-DELDATUM            PIC X(8)  VALUE SPACE.                     
010597     03 WS-FILLER2             PIC X(60) VALUE "')".                      
010599                                                                          
010600 01  WS-CURRENT-DATE           PIC X(8).                                  
010712 01  SYST-KVDAGAR              PIC S9(3) VALUE ZERO COMP-3.               
010713                                                                          
010730     EJECT                                                                
010800                                                                          
010937 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
010939*01  -COPY T01SYST -PRE T01SYST-                                          
010940     EJECT                                                                
011101                                                                          
011174     EJECT                                                                
011180                                                                          
011196     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
011197     EJECT                                                                
011221                                                                          
011901 PROCEDURE DIVISION.                                                      
011902 MAIN SECTION.                                                            
012210     PERFORM A-INIT                                                       
012220                                                                          
014290     WRITE WF10POST FROM WS-AREA                                          
014291     CLOSE WF10FIL                                                        
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014601                                                                          
014602 A-INIT SECTION.                                                          
014605     OPEN OUTPUT WF10FIL                                                  
014606                                                                          
014607     PERFORM DB2-SELECT-T01SYST                                           
014608                                                                          
014609     MOVE YES TO KEYS-SW                                                  
014610     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
014611     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
014612     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
014613     MOVE SYST-KVDAGAR                TO DAYS-KVDAYS                      
014614     MOVE 'WEEKDAYS'                  TO DAYS-IDCALEND                    
014615     MOVE SPACE                       TO DAYS-TIDATE1                     
014616     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
014617     CALL WZ20DAYS USING                                                  
014618          DAYS-WZ20DAYS                                                   
014619     IF DAYS-KDRC = ZERO                                                  
014620       MOVE DAYS-TIDATE1              TO WS-DELDATUM                      
014621       ELSE                                                               
014622       DISPLAY ' ERROR IN WZ20DAYS ' DAYS-KDRC                            
014623     END-IF                                                               
014628     .                                                                    
014629     EJECT                                                                
014630                                                                          
016300     EJECT                                                                
016400                                                                          
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
