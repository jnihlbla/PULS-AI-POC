000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0302000.                                                
000300*AUTHOR.         KJELL ANDRE                                              
000400*DATE-WRITTEN.   01-04-09.                                                
000500                                                                          
000600*                                                                         
000700*    FUNCTION:                                                            
000800*        SETS SYMBOLIC PARAMETERS IN SOP CORRESPONDING TO                 
000900*        CURRENT WEEK, PROGRAM AND ACCOUNTING PERIODS.                    
001000*        THIS PROGRAM RUNS WITH VARIOUS FREQUENCIES AND                   
001100*        THE NAME OF THE SYMBOLIC PARAMETER TO BE COMPUTED                
001110*        AND SET IS FETCHED FROM EXEC PARM.                               
001200*                                                                         
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100                                                                          
002200 FILE SECTION.                                                            
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002501                                                                          
002600 77  IDPGM                       PIC X(8)    VALUE 'W0302000'.            
002810                                                                          
002811*    -- SYMBOL NAME FROM EXEC PARM                                        
002812 01  W-SYMBOL                    PIC X(10).                               
002813                                                                          
002814*    -- RETURN CODE IN DISPLAY FORMAT                                     
002815 01  W-RKOD                      PIC 9(4).                                
002816                                                                          
002817*    -- CURRENT DATE AND TIME                                             
002819 01  WDATE.                                                               
002820     03 WDATE-NUM                PIC 9(8).                                
002821                                                                          
002830                                                                          
002900     EJECT                                                                
003600 01  GENERAL-SUBPROGRAM.                                                  
003700*                                                                         
003800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004100     03  W980SOP                 PIC X(8)    VALUE 'W980SOP'.             
004200                                                                          
004300*    --- PARAMETERS TO ABEND                                              
004400                                                                          
004500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004700                                                                          
004800 01  ERRTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005310                                                                          
005400     EJECT                                                                
006300*01  -COPY WDATAREA                                                       
006400     EJECT                                                                
006500*01  -COPY WSOPAREA                                                       
006600     EJECT                                                                
006700 LINKAGE SECTION.                                                         
006701                                                                          
006702 01  EXEC-PARM.                                                           
006703     03  PARM-LENGTH             PIC S9(4)  BINARY.                       
006704     03  PARM-DATA               PIC X(10).                               
006705                                                                          
006710     EJECT                                                                
006720 PROCEDURE DIVISION  USING EXEC-PARM.                                     
006800 MAIN SECTION.                                                            
006900                                                                          
007300     PERFORM A-INIT                                                       
007400                                                                          
007401     MOVE SPACE TO SOP-SYMBOLIC-VARIABLES                                 
007410     IF W-SYMBOL = 'YY'                                                   
007430       STRING 'YY('  DAT-TIAA  ')'                                        
007431       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007432       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007500     ELSE                                                                 
007600     IF W-SYMBOL = 'OLDYY'                                                
007601       STRING 'OLDYY('  DAT-TIAA  ')'                                     
007602       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007603       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007604     ELSE                                                                 
007610     IF W-SYMBOL = 'YYWW'                                                 
007630       STRING 'YYWW(' DAT-TIAAVV-GRP ')'                                  
007631       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007632       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007660     ELSE                                                                 
007670     IF W-SYMBOL = 'OLDYYWW'                                              
007671       STRING 'OLDYYWW(' DAT-TIAAVV-GRP ')'                               
007672       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007673       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007674     ELSE                                                                 
007680     IF W-SYMBOL = 'YYP'                                                  
007690       STRING 'YYP(' DAT-TIAAP-GRP ')'                                    
007691       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007692       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007694     ELSE                                                                 
007695     IF W-SYMBOL = 'OLDYYP'                                               
007696       STRING 'OLDYYP(' DAT-TIAAP-GRP ')'                                 
007697       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007698       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007699     ELSE                                                                 
007700     IF W-SYMBOL = 'YYAA'                                                 
007701       STRING 'YYAA(' DAT-TIAARP-GRP ')'                                  
007702       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007703       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007704     ELSE                                                                 
007705     IF W-SYMBOL = 'OLDYYAA'                                              
007706       STRING 'OLDYYAA(' DAT-TIAARP-GRP ')'                               
007707       DELIMITED BY SIZE INTO SOP-SYMBOLIC-VARIABLES                      
007708       DISPLAY 'VARIABLES SET: ' SOP-SYMBOLIC-VARIABLES(1:40)             
007709                                                                          
007710     ELSE                                                                 
007711       STRING 'INVALID SYMBOL NAME IN EXEC PARAMETER: ' W-SYMBOL          
007712            DELIMITED BY SIZE INTO ERRTEXT-STR                            
007713       DISPLAY ERRTEXT-STR                                                
007714       PERFORM S99-ABEND                                                  
007715     END-IF                                                               
007716     END-IF                                                               
007717     END-IF                                                               
007718     END-IF                                                               
007719     END-IF                                                               
007720     END-IF                                                               
007721     END-IF                                                               
007722     END-IF                                                               
007723                                                                          
007724     PERFORM B-SET-SYMBOLS-IN-SOP                                         
007725                                                                          
007730     MOVE ZERO TO RETURN-CODE                                             
007800     GOBACK                                                               
007900     .                                                                    
008000     EJECT                                                                
008100 A-INIT SECTION.                                                          
008200                                                                          
008201     MOVE SPACE TO W-SYMBOL                                               
008210     UNSTRING PARM-DATA DELIMITED BY '/' INTO W-SYMBOL                    
008230                                                                          
008231     MOVE FUNCTION CURRENT-DATE(1:8) TO WDATE                             
008240     IF W-SYMBOL(1:3) = 'OLD'                                             
008243       COMPUTE WDATE-NUM = FUNCTION DATE-OF-INTEGER(                      
008244           FUNCTION INTEGER-OF-DATE(WDATE-NUM) - 5 )                      
008510     END-IF                                                               
008511     MOVE WDATE(3:6)     TO DAT-I-TIDATUM                                 
008520     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
008620     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
008630                     DAT-O-TIDATUM DAT-KDSVAR                             
008700     .                                                                    
008800                                                                          
010400     EJECT                                                                
010500 B-SET-SYMBOLS-IN-SOP SECTION.                                            
010600                                                                          
010700     MOVE 'W' TO SOP-DDPREFIX                                             
010800     MOVE 'V' TO SOP-SOPFUNC                                              
010900     MOVE '*SOP*' TO SOP-PROC-NAME                                        
011500                                                                          
011600     CALL W980SOP USING SOP-PARM-AREA                                     
011700                                                                          
011800     IF SOP-RETCODE NOT = ZERO                                            
011810       MOVE SOP-RETCODE TO W-RKOD                                         
011900       STRING 'ERROR FROM W980SOP '  W-RKOD                               
011910           DELIMITED BY SIZE INTO ERRTEXT-STR                             
011920       DISPLAY ERRTEXT-STR                                                
012000       PERFORM S99-ABEND                                                  
012100     END-IF                                                               
015800     .                                                                    
015810                                                                          
015900     EJECT                                                                
016000 S99-ABEND SECTION.                                                       
016100                                                                          
016200     CALL ABEND USING RKOD-ABEND-NO-DUMP                                  
016300     .                                                                    
