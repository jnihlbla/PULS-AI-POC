000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183M00.                                                
000300 AUTHOR.         SURESH GUDIVADA.                                         
000400 DATE-WRITTEN.   24/07/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        To create a generic MQ file with the header & detail             
001000*        records for the respective country codes.                        
001100*                                                                         
001200*         INPUT FILE: W418.W418D2.W418MQA                                 
001300*        OUTPUT FILE: W418.W418D2.W418MQ                                  
001400*                                                                         
001930*                                                                         
001940                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INDATA                                                     
002800     SELECT W418MQA                    ASSIGN TO W4183MD1.                
002900     SKIP2                                                                
003000*          --- UTDATA                                                     
003100     SELECT W418MQ                     ASSIGN TO W4183MD2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W418MQA                                                              
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W418003B -PRE  IN-  -L.                                   
004200     SKIP3                                                                
004300 FD  W418MQ                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W418003A -PRE  UT-  -L.                                   
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W4183M00'.            
005500 77  W418MQA-EOF-SW              PIC X       VALUE 'N'.                   
005600     88  END-OF-W418MQA                      VALUE 'J'.                   
005700     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200    EJECT                                                                 
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                 'IN-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W418003B     -PRE IN-                                     
008800     EJECT                                                                
008900 01  UT-AREA-START               PIC X(24)   VALUE                        
009000                                 'UT-AREA-START  '.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W418003A     -PRE UT-                                     
009400     EJECT                                                                
009401     SKIP2                                                                
009402                                                                          
009430**   --- MQ HEADER WITH THE COUNTRY CODE INFO ---  **                     
009431 01  WS-IDLANDX2                 PIC X(2)    VALUE LOW-VALUE.             
009432 01  WS-HEADER                   PIC X(16)   VALUE                        
009433                                 '¤MQMPROP Market='.                      
009491 01  UT-HEADER                   PIC X(18)   VALUE SPACE.                 
009492     EJECT                                                                
009493                                                                          
009500 PROCEDURE DIVISION.                                                      
009600 MAIN SECTION.                                                            
009700     SKIP2                                                                
009800                                                                          
009900     PERFORM A-INIT                                                       
010000     PERFORM S01-READ-W418MQA                                             
010100     PERFORM UNTIL END-OF-W418MQA                                         
015030          IF WS-IDLANDX2 NOT = IN-IDLANDX2                                
010120             PERFORM S02-WRITE-HEADER                                     
010130          END-IF                                                          
010121          PERFORM S03-WRITE-W418MQ                                        
010800          PERFORM S01-READ-W418MQA                                        
010900     END-PERFORM                                                          
011100                                                                          
011200     PERFORM Z-FINIT                                                      
011300                                                                          
011400     MOVE ZERO TO RETURN-CODE                                             
011500     GOBACK                                                               
011600     .                                                                    
011700     EJECT                                                                
011710                                                                          
011800 A-INIT SECTION.                                                          
012000     OPEN INPUT  W418MQA                                                  
012100                                                                          
012200     OPEN OUTPUT W418MQ                                                   
012300     SKIP2                                                                
012500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     INITIALIZE IN-AREA                                                   
012610     .                                                                    
012700     EJECT                                                                
012710                                                                          
013600 S01-READ-W418MQA SECTION.                                                
013700     READ W418MQA INTO IN-AREA                                            
013800     AT END                                                               
013900        MOVE HIGH-VALUE    TO IN-AREA                                     
014000        SET END-OF-W418MQA TO TRUE                                        
014100                                                                          
014200     NOT AT END                                                           
014300        MOVE 'W418MQA'     TO POSTSUM-FDNAMN                              
014400        MOVE 'W4183MD1'    TO POSTSUM-DDNAMN2                             
014600        MOVE IN-IDPTYP     TO POSTSUM-TRANSTYP                            
014700        CALL POSTSUM    USING POSTSUM-PARM                                
014800     END-READ                                                             
014900     .                                                                    
015000     EJECT                                                                
015001                                                                          
015010 S02-WRITE-HEADER SECTION.                                                
015012     INITIALIZE UT-HEADER                                                 
015020                                                                          
007900*    --- Validate Market Headers in the File!                             
015030     IF IN-IDLANDX2  = SPACE                                              
015096        MOVE 'XX'   TO IN-IDLANDX2                                        
010130     END-IF                                                               
015020                                                                          
015040     STRING WS-HEADER      DELIMITED BY SIZE                              
015050            IN-IDLANDX2    DELIMITED BY SIZE                              
015060       INTO UT-HEADER                                                     
015070     END-STRING                                                           
015090     WRITE UT-POST    FROM UT-HEADER                                      
015096     MOVE IN-IDLANDX2   TO WS-IDLANDX2                                    
015098     .                                                                    
015099     EJECT                                                                
015001                                                                          
015100 S03-WRITE-W418MQ SECTION.                                                
015110     INITIALIZE UT-AREA                                                   
015200                                                                          
015210     MOVE IN-IDPTYP           TO UT-IDPTYP                                
015213     MOVE IN-IDDISTR          TO UT-IDDISTR                               
015215     MOVE IN-IDKUNDNR         TO UT-IDKUNDNR                              
015217     MOVE IN-IDRAPPNR         TO UT-IDRAPPNR                              
015230     MOVE IN-IDRADNR          TO UT-IDRADNR                               
015250     MOVE IN-TEANMNOT-DLR (1) TO UT-TEANMNOT-DLR (1)                      
015251     MOVE IN-TEANMNOT-DLR (2) TO UT-TEANMNOT-DLR (2)                      
015260     MOVE IN-TEANMNOT-DLR (3) TO UT-TEANMNOT-DLR (3)                      
015261                                                                          
015300     WRITE UT-POST    FROM UT-AREA                                        
015500     MOVE UT-IDPTYP     TO POSTSUM-TRANSTYP                               
015600     MOVE 'W418MQ'      TO POSTSUM-FDNAMN                                 
015700     MOVE 'W4183MD2'    TO POSTSUM-DDNAMN2                                
015800     CALL POSTSUM    USING POSTSUM-PARM                                   
015900     .                                                                    
016000     EJECT                                                                
015001                                                                          
012800 Z-FINIT SECTION.                                                         
012900     CLOSE W418MQA                                                        
013000           W418MQ                                                         
013100     SKIP2                                                                
013200     MOVE 'S' TO POSTSUM-OPKOD                                            
013300     CALL POSTSUM USING POSTSUM-PARM                                      
013400     .                                                                    
013500     EJECT                                                                
