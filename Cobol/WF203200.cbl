000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF203200.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   07/06/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PROGRAM UPDATES TABLE T01PROC                                
001000*                                                                         
002500 ENVIRONMENT DIVISION.                                                    
002600 INPUT-OUTPUT SECTION.                                                    
002700 FILE-CONTROL.                                                            
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(8)   VALUE 'WF203200'.             
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600     EJECT                                                                
003700                                                                          
003800 01  ERROR-TEXT.                                                          
003900     03  FILLER                  PIC X(10)  VALUE 'ERROR-TEXT'.           
004000     03  ERROR-TEXT-STR          PIC X(72)  VALUE SPACE.                  
004100                                                                          
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
013100     EXEC SQL INCLUDE T01PROC END-EXEC.                                   
015200     EJECT                                                                
015300                                                                          
015400 PROCEDURE DIVISION.                                                      
015500 MAIN SECTION.                                                            
015600     PERFORM A-INIT                                                       
015700                                                                          
016900     PERFORM DB2-OPEN-T01PROC-CRS2                                        
017000     PERFORM DB2-FETCH-T01PROC-CRS2                                       
017100     PERFORM UNTIL T01PROC-MISSING                                        
017200       PERFORM DB2-UPDATE-T01PROC                                         
017300       PERFORM DB2-FETCH-T01PROC-CRS2                                     
017400     END-PERFORM                                                          
017500     PERFORM DB2-CLOSE-T01PROC-CRS2                                       
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000                                                                          
018100 A-INIT SECTION.                                                          
018200     INITIALIZE GOOD-SQLCODECODES                                         
018500     .                                                                    
018600                                                                          
034600*   --- DB2 SECTIONS                                                      
034700*                                                                         
036700 DB2-OPEN-T01PROC-CRS2 SECTION.                                           
036800     EXEC SQL DECLARE T01PROC-CRS2 CURSOR FOR                             
036900     SELECT   KDBEH,                                                      
037000              DAEXDAT,                                                    
037100              TIEXTID                                                     
037200                                                                          
037300     FROM     T01PROC                                                     
037400                                                                          
037500     FOR UPDATE OF                                                        
037600              KDBEH,                                                      
037700              DAEXDAT,                                                    
037800              TIEXTID                                                     
037900     END-EXEC                                                             
038000                                                                          
038100     EXEC SQL OPEN T01PROC-CRS2                                           
038200     END-EXEC                                                             
038300                                                                          
038400     MOVE 000            TO GOOD-SQLCODECODES                             
038500     MOVE SQLCODE        TO SQLCODE-WS                                    
038600     PERFORM DB2-STATUS-CHECK                                             
038700     .                                                                    
038800     EJECT                                                                
038900                                                                          
040400 DB2-FETCH-T01PROC-CRS2 SECTION.                                          
040500     EXEC SQL FETCH T01PROC-CRS2 INTO                                     
040600            :T01PROC-KDBEH,                                               
040700            :T01PROC-DAEXDAT,                                             
040800            :T01PROC-TIEXTID                                              
040900     END-EXEC                                                             
041000                                                                          
041100     MOVE 000100         TO GOOD-SQLCODECODES                             
041200     MOVE SQLCODE        TO SQLCODE-WS                                    
041300                            T01PROC-WS                                    
041400     PERFORM DB2-STATUS-CHECK                                             
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 DB2-UPDATE-T01PROC SECTION.                                              
041900     EXEC SQL                                                             
042000         UPDATE T01PROC                                                   
042100         SET KDBEH   = ' '                                                
042200         ,   DAEXDAT = '00000000'                                         
042300         ,   TIEXTID = 0                                                  
042400         WHERE CURRENT OF T01PROC-CRS2                                    
042500     END-EXEC                                                             
042600                                                                          
042700     MOVE 000     TO GOOD-SQLCODECODES                                    
042800     MOVE SQLCODE TO SQLCODE-WS                                           
042900                     T01PROC-WS                                           
043000     PERFORM DB2-STATUS-CHECK                                             
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
044000 DB2-CLOSE-T01PROC-CRS2    SECTION.                                       
044100     EXEC SQL CLOSE T01PROC-CRS2                                          
044200     END-EXEC                                                             
044300     .                                                                    
044400     EJECT                                                                
044500                                                                          
196400 DB2-STATUS-CHECK     SECTION.                                            
196500     SET SQLCODE-IX TO 1                                                  
196600     SEARCH GOOD-SQLCODE                                                  
196700       AT END                                                             
196800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
196900          DELIMITED BY SIZE INTO ERROR-TEXT                               
197000          CALL ABEND USING RKOD-ABEND-DB2                                 
197100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
197200     END-SEARCH                                                           
197300     .                                                                    
