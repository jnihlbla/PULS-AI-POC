000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF107500.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   22/09/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        DELETE 10 YEARS OLD DATA FROM T01DL_INARC AND T01DHEA_ARC        
001100*        DB2 TABLE                                                        
001200*                                                                         
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(8)    VALUE 'WF107500'.            
002800 77  YES                         PIC X       VALUE 'J'.                   
002900 77  NOO                         PIC X       VALUE 'N'.                   
003000 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
003100 01  WS-RUNDATUM-10YROLD         PIC X(8)  VALUE SPACE.                   
003200 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
003300     EJECT                                                                
003400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003500 01  FILLER REDEFINES TODAYS-DATE.                                        
003600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
003700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
003800     03  TODAYS-DATE-DAY         PIC 9(2).                                
003900     EJECT                                                                
004000 01  GENERAL-SUBPROGRAMS.                                                 
004100*                                                                         
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004300     SKIP2                                                                
004400*    --- PARAMETERS TO ABEND                                              
004500                                                                          
004600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004900     SKIP2                                                                
005000 01  ERROR-TEXT.                                                          
005100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005300     EJECT                                                                
005400 01  MESSAGE-CODES.                                                       
005500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005600     EJECT                                                                
005700 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
005800     SKIP3                                                                
005900*    -COPY WZ20DAYS                                                       
006000     EJECT                                                                
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300*                                                                         
006400 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
006500 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  FILLER                    PIC X(16) VALUE 'T01DHEAARC-AREA '.        
007200*01  -COPY T01DHEA        -PRE DLIN-                                      
007300     EXEC SQL INCLUDE T01DHEA   END-EXEC.                                 
007400     EJECT                                                                
007500 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA '.        
007600*01  -COPY T01DLIN        -PRE DLIN-                                      
007700     EXEC SQL INCLUDE T01DLIN   END-EXEC.                                 
007800     EJECT                                                                
007900 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
008000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008100*                        **** STATUS-CODE FROM DB2                        
008200                                                                          
008300 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
008400 01  DB2-WS.                                                              
008500   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
008600     88  ROW-FOUND                         VALUE +000.                    
008700     88  ROW-MISSING                       VALUE +100.                    
008800   03  GOOD-SQLCODES.                                                     
008900     05  GOOD-SQLCODE OCCURS 5                                            
009000         INDEXED BY SQLCODE-IX PIC 999.                                   
009100     EJECT                                                                
009200 LINKAGE SECTION.                                                         
009300                                                                          
009400*01  -COPY W0009   -PRE MSG-                                              
009500     EJECT                                                                
009600 PROCEDURE DIVISION  USING MSG-PCB.                                       
009700 MAIN SECTION.                                                            
009800     ENTRY 'DLITCBL' USING MSG-PCB.                                       
009900                                                                          
010000     PERFORM A-INIT                                                       
010100                                                                          
010200     PERFORM B-EXEC-PARA                                                  
010300                                                                          
010400     MOVE ZERO TO RETURN-CODE                                             
010500     GOBACK                                                               
010600     .                                                                    
010700     EJECT                                                                
010800 A-INIT SECTION.                                                          
010900     SKIP2                                                                
011000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
011100     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
011200     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
011300     MOVE 3652                        TO DAYS-KVDAYS                      
011400     MOVE ' '                         TO DAYS-IDCALEND                    
011500     MOVE SPACE                       TO DAYS-TIDATE1                     
011600     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
011700     CALL WZ20DAYS USING                                                  
011800          DAYS-WZ20DAYS                                                   
011900     IF DAYS-KDRC = ZERO                                                  
012000       MOVE DAYS-TIDATE2              TO WS-RUNDATUM-TO                   
012100       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-10YROLD              
012400     END-IF                                                               
012500     INITIALIZE GOOD-SQLCODES                                             
012600     .                                                                    
012700     EJECT                                                                
012800 B-EXEC-PARA SECTION.                                                     
012900     PERFORM  DB2-DELETE-T01DHEA_ARC                                      
013000     PERFORM  DB2-DELETE-T01DLIN_ARC                                      
013100     .                                                                    
013200     EJECT                                                                
013300 DB2-DELETE-T01DHEA_ARC SECTION.                                          
013400     MOVE 000 TO GOOD-SQLCODES                                            
013500                                                                          
013600     EXEC SQL                                                             
013700         DELETE FROM T01DHEA_ARC                                          
013800                                                                          
013900         WHERE DAEXDAT    <  :WS-RUNDATUM-10YROLD                         
014300     END-EXEC                                                             
014400                                                                          
014500     MOVE 000100         TO GOOD-SQLCODES                                 
014600     MOVE SQLCODE TO SQLCODE-WS                                           
014700     PERFORM DB2-STATUS-CHECK                                             
014800     .                                                                    
014900     EJECT                                                                
015000  DB2-DELETE-T01DLIN_ARC SECTION.                                         
015100     MOVE 000 TO GOOD-SQLCODES                                            
015200                                                                          
015300     EXEC SQL                                                             
015400         DELETE FROM T01DLIN_ARC                                          
015500                                                                          
015600         WHERE DAEXDAT    <  :WS-RUNDATUM-10YROLD                         
016000     END-EXEC                                                             
016100                                                                          
016200     MOVE 000100         TO GOOD-SQLCODES                                 
016300     MOVE SQLCODE TO SQLCODE-WS                                           
016400     PERFORM DB2-STATUS-CHECK                                             
016500     .                                                                    
016600     EJECT                                                                
016700 DB2-STATUS-CHECK  SECTION.                                               
016800                                                                          
016900     SET SQLCODE-IX TO 1                                                  
017000     SEARCH GOOD-SQLCODE                                                  
017100       AT END                                                             
017200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
017300          DELIMITED BY SIZE INTO ERROR-TEXT                               
017400          CALL ABEND USING RKOD-ABEND-DB2                                 
017500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
017600     END-SEARCH                                                           
017700     .                                                                    
