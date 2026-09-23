000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9804500.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   07-07-24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SOP PASSWORD CHANGE ROUTINE:                                     
000900*        - CREATE A NEW RANDOM NUMBER AS SOURCE FOR THE SOP               
000910*          PASSWORD, AND GENERATE A PASSWORD FROM THIS NUMBER.            
001000*        - ORDER A JOB (NAME VIA PARM) THAT PERFORMS THE PASSWORD         
001100*          CHANGE AND UPDATES THE SOP DATABASE WITH NEW NUMBER.           
001101*                                                                         
001110*        THE PROGRAM CAN HANDLE EXISTING PASSWORDS IN BOTH                
001120*        ENCRYPTED (NUMBER) AND UNENCRYPTED (CLEAR TEXT)                  
001121*        FORMAT.  THE SUBSEQUENT JOB WILL STORE THE PASSWORD              
001130*        AS A NUMBER.                                                     
001140*                                                                         
001150*    ABEND CODES:                                                         
001160*       U0016 - IF AN INVALID JOB NAME IS GIVEN IN EXEC PARM              
001200                                                                          
001300     EJECT                                                                
001400 DATA DIVISION.                                                           
001500                                                                          
001600 WORKING-STORAGE SECTION.                                                 
001700                                                                          
001800 77  IDPGM                       PIC X(8)    VALUE 'W9804500'.            
001900 77  YES                         PIC X       VALUE 'J'.                   
002000 77  NOO                         PIC X       VALUE 'N'.                   
002100                                                                          
002200 01  GENERAL-SUBPROGRAMS.                                                 
002300*                                                                         
002400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
002500     03  W980PASS                PIC X(8)    VALUE 'W980PASS'.            
002600     03  W980SOP                 PIC X(8)    VALUE 'W980SOP'.             
002700     03  W980WSPC                PIC X(8)    VALUE 'W980WSPC'.            
002800                                                                          
002900*    --- PARAMETERS TO ABEND                                              
003000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
003100                                                                          
003200 01  ERRTEXT.                                                             
003300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
003400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
003500                                                                          
003600 77  ERR-WSFUNC                  PIC X(4)    VALUE SPACE.                 
003700 77  ERR-WSRKOD                  PIC X       VALUE SPACE.                 
003800                                                                          
003900 01  JOBNAME-STATUS              PIC X       VALUE 'N'.                   
004000     88 JOBNAME-OK                           VALUE 'J'.                   
004100     88 JOBNAME-INVALID                      VALUE 'N'.                   
004200                                                                          
004300     EJECT                                                                
004400 77  W-RANDOM                    PIC V9(9)   PACKED-DECIMAL.              
004410                                                                          
004500 77  OLD-NUMBER                  PIC 9(9).                                
004600 77  NEW-NUMBER                  PIC 9(9).                                
004700 77  OLD-PASSWORD                PIC X(8).                                
004800 77  NEW-PASSWORD                PIC X(8).                                
004900                                                                          
004910*------- PARAMETERS TO W980PASS                                           
005000 01  -COPY W980PASS                                                       
005010                                                                          
005100     EJECT                                                                
005110*------- PARAMETERS TO W980SOP                                            
005200 01  -COPY WSOPAREA                                                       
005300                                                                          
005400     EJECT                                                                
005500*------- PARAMETERS TO W980WSPC                                           
005510                                                                          
005600 01  DD-PARM.                                                             
005700     03   DD-LENGTH          PIC S9(4)   COMP  VALUE +10.                 
005800     03   DD-NAME            PIC X(8)    VALUE 'SOPDD1  '.                
005900                                                                          
006000 01  SOP-PARM.                                                            
006100     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
006200     03   FILLER             PIC X(5)    VALUE '*SOP*'.                   
006300                                                                          
006400 01  DROWP-PARM.                                                          
006500     03   FILLER             PIC S9(4)   COMP VALUE +8.                   
006600     03   FILLER             PIC X(6)    VALUE '&DROWP'.                  
006700                                                                          
006800 01  DATA-PARM.                                                           
006900     03   DATA-LENGTH        PIC S9(4)   COMP.                            
007000     03   DATA-VALUE         PIC X(240).                                  
007100                                                                          
007200 01  FUNCTION-CODES.                                                      
007300     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
007400     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
007500     03  FSAVE               PIC X(4)    VALUE 'SAVE'.                    
007600     03  FQUIT               PIC X(4)    VALUE 'QUIT'.                    
007800     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
007900     03  FGETN               PIC X(4)    VALUE 'GETN'.                    
008500                                                                          
008600 01  WSFUNC                  PIC X(4).                                    
008700 01  WSRKOD                  PIC X.                                       
008800                                                                          
008900     EJECT                                                                
009000 LINKAGE SECTION.                                                         
009100                                                                          
009200 01  EXEC-PARM.                                                           
009300     03  PARM-LENGTH             PIC S9(4)   BINARY.                      
009400     03  PARM-JOBNAME            PIC X(8).                                
009500                                                                          
009600     EJECT                                                                
009700 PROCEDURE DIVISION USING EXEC-PARM.                                      
009800 MAIN SECTION.                                                            
009900                                                                          
010000     PERFORM A-CHECK-JOBNAME                                              
010100     IF JOBNAME-OK                                                        
010300       PERFORM B-FETCH-OLD-PWD                                            
010310       PERFORM C-COMPUTE-NEW-PWD                                          
010400       PERFORM D-ORDER-JOB                                                
010500     ELSE                                                                 
010600       PERFORM S99-ABEND                                                  
010700     END-IF                                                               
010800                                                                          
010900     MOVE ZERO TO RETURN-CODE                                             
011000     GOBACK                                                               
011100     .                                                                    
011200     EJECT                                                                
011300 A-CHECK-JOBNAME   SECTION.                                               
011400                                                                          
011500     IF  PARM-JOBNAME(1:1) IS ALPHABETIC-UPPER                            
011600     AND PARM-JOBNAME(2:) NOT = SPACE                                     
011700       SET JOBNAME-OK TO TRUE                                             
011800     ELSE                                                                 
011900       MOVE 'INVALID JOB NAME IN PARM' TO ERRTEXT-STR                     
012000       SET JOBNAME-INVALID TO TRUE                                        
012100     END-IF                                                               
012200     .                                                                    
012300     EJECT                                                                
012400 B-FETCH-OLD-PWD   SECTION.                                               
012500                                                                          
012600     MOVE SPACE TO ERR-WSFUNC                                             
012700                                                                          
012800     MOVE FOPEN TO WSFUNC                                                 
012900     CALL W980WSPC USING WSFUNC WSRKOD DD-PARM                            
013000                                                                          
013100     IF WSRKOD = SPACE                                                    
013200       MOVE FGETF TO WSFUNC                                               
013300       MOVE ALL '?' TO DATA-VALUE                                         
013400       CALL W980WSPC USING WSFUNC WSRKOD SOP-PARM                         
013500                           DROWP-PARM DATA-PARM                           
013600                                                                          
013700       IF WSRKOD NOT = SPACE                                              
013800         MOVE FGETF  TO ERR-WSFUNC                                        
013900         MOVE WSRKOD TO ERR-WSRKOD                                        
014000       END-IF                                                             
014100                                                                          
014200       MOVE FQUIT TO WSFUNC                                               
014300       CALL W980WSPC USING FCLSE WSRKOD                                   
014400                                                                          
014500       MOVE FCLSE TO WSFUNC                                               
014600       CALL W980WSPC USING FCLSE WSRKOD                                   
014610                                                                          
014700     ELSE                                                                 
014800       MOVE FOPEN  TO ERR-WSFUNC                                          
014900       MOVE WSRKOD TO ERR-WSRKOD                                          
015000     END-IF                                                               
015100                                                                          
015200     IF ERR-WSRKOD NOT = SPACE                                            
015300       STRING 'ERROR FROM W980WSPC '                                      
015400               ERR-WSFUNC ' '                                             
015500               ERR-WSRKOD                                                 
015600            DELIMITED BY SIZE INTO ERRTEXT-STR                            
015700       PERFORM S99-ABEND                                                  
015800     END-IF                                                               
015900                                                                          
016000*    -- CHECK FORMAT OF EXISTING PASSWORD:                                
016010*    -- ENCHRYPTED (NUMBER) OR CLEAR TEXT (NON-NUMERIC)                   
016020                                                                          
016100     IF DATA-VALUE(1:DATA-LENGTH - 2) NOT NUMERIC                         
016200       DISPLAY 'USING UNENCRYPTED PWD'                                    
016310                                                                          
016400*      -- FETCH A SEED NUMBER FROM CURRENT DATE AND TIME INSTEAD          
016401*      -- (DHHMMSSHH FROM YYMMDDHHMMSSHH)                                 
016410       MOVE FUNCTION CURRENT-DATE(8:9)    TO OLD-NUMBER                   
016411       MOVE DATA-VALUE(1:DATA-LENGTH - 2) TO OLD-PASSWORD                 
016412                                                                          
016420     ELSE                                                                 
016430       MOVE DATA-VALUE(1:DATA-LENGTH - 2) TO PASS-NUMBER                  
016431       DISPLAY 'USING ENCRYPTED PWD'                                      
016435                                                                          
016440*      -- GENERATE PASSWORD FROM NUMBER                                   
016450       CALL W980PASS USING PASS-AREA                                      
016460                                                                          
016470       MOVE PASS-NUMBER TO OLD-NUMBER                                     
016480       MOVE PASS-WORD   TO OLD-PASSWORD                                   
016600     END-IF                                                               
016900                                                                          
017400     .                                                                    
017410                                                                          
017500     EJECT                                                                
017600 C-COMPUTE-NEW-PWD SECTION.                                               
017700                                                                          
017800     COMPUTE W-RANDOM = FUNCTION RANDOM (OLD-NUMBER)                      
017900     COMPUTE PASS-NUMBER = W-RANDOM * 1000000000                          
018000                                                                          
018100     CALL W980PASS USING PASS-AREA                                        
018200                                                                          
018300     MOVE PASS-NUMBER TO NEW-NUMBER                                       
018400     MOVE PASS-WORD   TO NEW-PASSWORD                                     
018500     .                                                                    
018510                                                                          
018600     EJECT                                                                
018700 D-ORDER-JOB    SECTION.                                                  
018800                                                                          
018900     MOVE SPACE        TO SOP-DDPREFIX                                    
019000     MOVE 'O'          TO SOP-SOPFUNC                                     
019100     MOVE PARM-JOBNAME TO SOP-PROC-NAME                                   
019200     MOVE ZERO         TO SOP-ACTPASS-DATE                                
019300     MOVE SPACE        TO SOP-SYMBOLIC-VARIABLES                          
019610     STRING                                                               
019700            'OLDNBR('     DELIMITED BY SIZE                               
019800            OLD-NUMBER    DELIMITED BY SIZE                               
019900            ') OLDPWD('   DELIMITED BY SIZE                               
019920            OLD-PASSWORD  DELIMITED BY SPACE                              
019930            ') NEWNBR('   DELIMITED BY SIZE                               
020100            NEW-NUMBER    DELIMITED BY SIZE                               
020200            ') NEWPWD('   DELIMITED BY SIZE                               
020210            NEW-PASSWORD  DELIMITED BY SPACE                              
020220            ') '          DELIMITED BY SIZE                               
020300       INTO SOP-SYMBOLIC-VARIABLES                                        
020400                                                                          
020500     CALL W980SOP USING SOP-PARM-AREA                                     
020600                                                                          
020700     IF SOP-RETCODE > 4                                                   
020800       MOVE 'ERROR FROM SOP ORDER' TO ERRTEXT-STR                         
020900       PERFORM S99-ABEND                                                  
021000     END-IF                                                               
021100     .                                                                    
021110                                                                          
021200     EJECT                                                                
021300 S99-ABEND SECTION.                                                       
021400                                                                          
021500     DISPLAY ERRTEXT-STR                                                  
021600     CALL ABEND USING RKOD-ABEND-NO-DUMP                                  
021700     .                                                                    
