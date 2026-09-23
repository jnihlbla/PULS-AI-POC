000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5126600.                                                
000300 AUTHOR.         RAGUR SATHEESH.                                          
000400 DATE-WRITTEN.   18/12/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATES OUT FILE WITH INTERNAL GIT INFO.                         
000900*        USED AS INPUT TO CREATE INTERNAL GIT REPORT IN                   
001000*        IN SUBSEQUENT PROGRAMS                                           
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- BASE FILE FROM W5161 PROGRAM                               
002600     SELECT W51261                     ASSIGN TO W51266D1.                
002700     SKIP2                                                                
002800*          --- BASE FILE FROM W5162 PROGRAM                               
002900     SELECT W51262                     ASSIGN TO W51266D2.                
003000     SKIP2                                                                
003100*          --- OUTPUT FILE WITH GIT FOR SEPV                              
003200     SELECT W51266                     ASSIGN TO W51266D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W51261                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W51261      -L.                                                
004300     SKIP3                                                                
004400 FD  W51262                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W51262      -L.                                                
004900     SKIP3                                                                
005000     SKIP3                                                                
005100 FD  W51266                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400*01  POST   -COPY W51266 -PRE  UT-  -L.                                   
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700*                                                                         
005800 77  IDPGM                       PIC X(8)    VALUE 'W5126600'.            
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006100                                                                          
006400 01  TOT-PRARTNTO                PIC S9(11)V9(2) VALUE +0 COMP-3.         
006500 01  TOT-PRARTSTD                PIC S9(11)V9(2) VALUE +0 COMP-3.         
006600                                                                          
006700 01  WS-61-ARTNR-IDDC.                                                    
006800     05  WS-61-ARTNR             PIC 9(9)    VALUE ZERO.                  
006900     05  WS-61-IDDC              PIC X(2)    VALUE SPACES.                
007000                                                                          
007100 01  WS-62-ARTNR-IDDC.                                                    
007200     05  WS-62-ARTNR             PIC 9(9)    VALUE ZERO.                  
007300     05  WS-62-IDDC              PIC X(2)    VALUE SPACES.                
007400                                                                          
007500 77  W51262-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-W51262                       VALUE 'J'.                   
007700                                                                          
007800 77  W51261-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W51261                       VALUE 'J'.                   
008000     EJECT                                                                
008100*                                                                         
009400 01  GENERAL-SUBPROGRAMS.                                                 
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010000     SKIP2                                                                
010100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
010200                                                                          
010300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010600     SKIP2                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
011000*01  -COPY W0005   -PRE  POSTSUM-                                         
011100*01  -COPY WWDIST35                                                       
011200*01  -COPY WWDC99                                                         
011300     EJECT                                                                
011400 01  IN1-AREA-START              PIC X(24)   VALUE                        
011500                                 'IN1-AREA-START  '.                      
011600     SKIP2                                                                
011800*01  AREA -COPY W51261     -PRE IN1-                                      
012000 01  IN2-AREA-START              PIC X(24)   VALUE                        
012100                                 'IN2-AREA-START  '.                      
012200     SKIP2                                                                
012300*01  AREA -COPY W51262     -PRE IN2-                                      
012400     EJECT                                                                
012500 01  UT-AREA-START               PIC X(24)   VALUE                        
012600                                 'UT-AREA-START  '.                       
012700*01  AREA -COPY W51266     -PRE UT-                                       
012800     SKIP2                                                                
018200     EJECT                                                                
018310 PROCEDURE DIVISION .                                                     
018400 MAIN SECTION.                                                            
018700                                                                          
018800     PERFORM A-INIT                                                       
018900                                                                          
019000     PERFORM S01-READ-W51261                                              
019100     PERFORM S02-READ-W51262                                              
019200                                                                          
019300     PERFORM UNTIL  END-OF-W51261                                         
019400       PERFORM UNTIL END-OF-W51262                                        
019500                 OR WS-62-ARTNR-IDDC > WS-61-ARTNR-IDDC                   
019600                                                                          
019700         IF  IN2-IDARTNR = IN1-IDARTNR                                    
019800         AND IN2-IDDC    = IN1-IDDC                                       
019900             PERFORM B-PROCESS                                            
020000         END-IF                                                           
020100         PERFORM S02-READ-W51262                                          
020200       END-PERFORM                                                        
020300       PERFORM S01-READ-W51261                                            
020400     END-PERFORM                                                          
020500                                                                          
020600     PERFORM Z-FINIT                                                      
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300                                                                          
021400     OPEN INPUT  W51261                                                   
021500                 W51262                                                   
021600                                                                          
021700     OPEN OUTPUT W51266                                                   
021800                                                                          
022000     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
022100     .                                                                    
022200     EJECT                                                                
022300 B-PROCESS SECTION.                                                       
022400     MOVE IN2-IDDISTR TO DIST35-IDDISTR                                   
022500     MOVE IN2-IDDC    TO WS-IDDC                                          
022600                                                                          
022710     IF (NDC-NA OR XDC-NON-VCC-OWNED)                                     
022720       CONTINUE                                                           
022730     ELSE                                                                 
022800        IF DIST35-NONVCC-CDC-REFILL                                       
022810        OR DIST35-NONVCC-VCC-REFILL                                       
022820        OR DIST35-NONVCC-VCC-TRANSFER                                     
022900          CONTINUE                                                        
022910        ELSE                                                              
023000          COMPUTE TOT-PRARTNTO ROUNDED =                                  
023100                  (IN2-PRARTNTO * IN2-KVAVIS)                             
023110          COMPUTE TOT-PRARTSTD ROUNDED =                                  
023120                  (IN1-PRARTSTD * IN2-KVAVIS)                             
023200          PERFORM BB-CREATE-OUTPUT                                        
023700        END-IF                                                            
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100                                                                          
026900 BB-CREATE-OUTPUT SECTION.                                                
027000                                                                          
027100     MOVE IN2-IDARTNR            TO UT-IDARTNR                            
027200     MOVE IN2-IDDC               TO UT-IDDC                               
027300     MOVE IN2-DAINLEV            TO UT-DAINLEV                            
027400     MOVE IN2-KVAVIS             TO UT-KVAVIS                             
027500     MOVE IN2-PRARTNTO           TO UT-PRARTNTO                           
027510     MOVE TOT-PRARTNTO           TO UT-SUNTO-TOT                          
027600     MOVE 'SEK'                  TO UT-KDVALISO                           
027700     MOVE IN1-KDPRODSL           TO UT-KDPRODSL                           
027800     MOVE IN1-PRARTSTD           TO UT-PRARTSTD                           
027810     MOVE TOT-PRARTSTD           TO UT-SUSTDTOT                           
027900     PERFORM S11-WRITE-W51266                                             
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300 Z-FINIT SECTION.                                                         
028400     CLOSE W51261                                                         
028500           W51262                                                         
028600           W51266                                                         
028700     SKIP2                                                                
028800     MOVE 'S'        TO POSTSUM-OPKOD                                     
028900     CALL POSTSUM USING POSTSUM-PARM                                      
029000     .                                                                    
029100     EJECT                                                                
029200 S01-READ-W51261  SECTION.                                                
029300     READ W51261        INTO IN1-AREA                                     
029400     AT END                                                               
029500        MOVE HIGH-VALUE   TO IN1-AREA                                     
029600        SET END-OF-W51261 TO TRUE                                         
029700                                                                          
029800     NOT AT END                                                           
029900        MOVE IN1-IDARTNR TO WS-61-ARTNR                                   
030000        MOVE IN1-IDDC    TO WS-61-IDDC                                    
030100        MOVE 'W51261'    TO POSTSUM-FDNAMN                                
030200        MOVE 'W51266D1'  TO POSTSUM-DDNAMN2                               
030300        CALL POSTSUM  USING POSTSUM-PARM                                  
030400     END-READ                                                             
030500     .                                                                    
030600     EJECT                                                                
030700 S02-READ-W51262  SECTION.                                                
030800     READ W51262        INTO IN2-AREA                                     
030900     AT END                                                               
031000        MOVE HIGH-VALUE   TO IN2-AREA                                     
031100        SET END-OF-W51262 TO TRUE                                         
031200                                                                          
031300     NOT AT END                                                           
031400        MOVE IN2-IDARTNR TO WS-62-ARTNR                                   
031500        MOVE IN2-IDDC    TO WS-62-IDDC                                    
031600        MOVE 'W51262'    TO POSTSUM-FDNAMN                                
031700        MOVE 'W51266D2'  TO POSTSUM-DDNAMN2                               
031800        CALL POSTSUM  USING POSTSUM-PARM                                  
031900     END-READ                                                             
032000     .                                                                    
032100     EJECT                                                                
032200 S11-WRITE-W51266 SECTION.                                                
032300                                                                          
032400     WRITE UT-POST  FROM UT-AREA                                          
032500                                                                          
032600     MOVE 'W51266'   TO POSTSUM-FDNAMN                                    
032700     MOVE 'W51266D3' TO POSTSUM-DDNAMN2                                   
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     .                                                                    
033000     EJECT                                                                
033100 S99-ABEND SECTION.                                                       
033200                                                                          
033300     SKIP2                                                                
033400     MOVE 'S'        TO POSTSUM-OPKOD                                     
033500     CALL POSTSUM USING POSTSUM-PARM                                      
033600     CALL ABEND   USING RKOD-ABEND                                        
033700     .                                                                    
033800     EJECT                                                                
