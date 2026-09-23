000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W980PASS.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   07/06/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        COMPUTE A PASSWORD FROM A RANDOM NUMBER                          
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 DATA DIVISION.                                                           
001400     SKIP3                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600                                                                          
001700 77  IDPGM                       PIC X(8)      VALUE 'W980PASS'.          
001800                                                                          
001900 77  W-RANDOM                    PIC V9(9)     PACKED-DECIMAL.            
002000 77  RIX                         PIC S9(4)     BINARY.                    
002100 77  PIX                         PIC S9(4)     BINARY.                    
002200                                                                          
002210 01  DIGIT-STATUS                PIC X         VALUE '0'.                 
002220     88 NO-DIGIT-YET                           VALUE '0'.                 
002221     88 AT-LEAST-ONE-DIGIT                     VALUE '1'.                 
002240                                                                          
002300 01  LETTERS                     PIC X(26)     VALUE                      
002400     'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                                        
002500 01  FILLER REDEFINES LETTERS.                                            
002600     03 LETTER-ARR OCCURS 26     PIC X.                                   
002700                                                                          
002800 01  NUMBERS                     PIC X(10)     VALUE                      
002900     '0123456789'.                                                        
003000 01  FILLER REDEFINES NUMBERS.                                            
003100     03 NUMBER-ARR OCCURS 10     PIC X.                                   
003200                                                                          
003300 01  ALPHANUM                    PIC X(36)     VALUE                      
003400     'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.                              
003500 01  FILLER REDEFINES ALPHANUM.                                           
003600     03 ALPHANUM-ARR OCCURS 36     PIC X.                                 
003700                                                                          
003800     SKIP3                                                                
003900 LINKAGE SECTION.                                                         
004000                                                                          
004100 01  -COPY W980PASS                                                       
004200                                                                          
004300     EJECT                                                                
004400 PROCEDURE DIVISION USING PASS-AREA.                                      
004500 MAIN SECTION.                                                            
004600     SKIP2                                                                
004700     MOVE SPACE TO PASS-WORD                                              
004800     MOVE 1 TO PIX                                                        
004900                                                                          
005000     COMPUTE W-RANDOM = FUNCTION RANDOM (PASS-NUMBER)                     
005100                                                                          
005200     COMPUTE RIX = FUNCTION INTEGER-PART(W-RANDOM * 26) + 1               
005300     MOVE LETTER-ARR (RIX) TO PASS-WORD(PIX:1)                            
005400     ADD 1 TO PIX                                                         
005510     SET NO-DIGIT-YET TO TRUE                                             
005520                                                                          
005600     PERFORM UNTIL PIX > 8                                                
005700                                                                          
005701       COMPUTE W-RANDOM = FUNCTION RANDOM                                 
005710       IF PIX = 8 AND NO-DIGIT-YET                                        
005711         COMPUTE RIX = FUNCTION INTEGER-PART(W-RANDOM * 10) + 1           
005712         MOVE NUMBER-ARR (RIX) TO PASS-WORD(PIX:1)                        
005720       ELSE                                                               
005900         COMPUTE RIX = FUNCTION INTEGER-PART(W-RANDOM * 36) + 1           
006000         MOVE ALPHANUM-ARR (RIX) TO PASS-WORD(PIX:1)                      
006010         IF ALPHANUM-ARR (RIX) NUMERIC                                    
006020           SET AT-LEAST-ONE-DIGIT TO TRUE                                 
006030         END-IF                                                           
006040       END-IF                                                             
006100                                                                          
006200       ADD 1 TO PIX                                                       
006300     END-PERFORM                                                          
006400                                                                          
006500     MOVE ZERO TO RETURN-CODE                                             
006600     GOBACK                                                               
006700     .                                                                    
