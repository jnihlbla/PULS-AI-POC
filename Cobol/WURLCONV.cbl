000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WURLCONV.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   24/09/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        This program handles URL encoding (percent encoding).            
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700 CONFIGURATION SECTION.                                                   
001800*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77  OK-SWITCH                   PIC X     VALUE 'Y'.                     
002500     88  ALL-OK                            VALUE 'Y'.                     
002600     88  SOME-ERROR                        VALUE 'N'.                     
002700                                                                          
002800 01  WS-WORK-AREA.                                                        
002900     03  IX                      PIC S9(9) COMP   VALUE ZERO.             
003000     03  WS-IN-POS               PIC S9(9) COMP   VALUE ZERO.             
003100     03  WS-OUT-POS              PIC S9(9) COMP   VALUE ZERO.             
003200     03  WS-CHAR-UTF8            PIC U.                                   
003300     03  WS-CHAR                 PIC X     VALUE ' '.                     
003400         88 UNRESERVED-CHAR                VALUE 'A' 'B' 'C' 'D'          
003500                                                 'E' 'F' 'G' 'H'          
003600                                                 'I' 'J' 'K' 'L'          
003700                                                 'M' 'N' 'O' 'P'          
003800                                                 'Q' 'R' 'S' 'T'          
003900                                                 'U' 'V' 'W' 'X'          
004000                                                 'Y' 'Z'                  
004100                                                 'a' 'b' 'c' 'd'          
004200                                                 'e' 'f' 'g' 'h'          
004300                                                 'i' 'j' 'k' 'l'          
004400                                                 'm' 'n' 'o' 'p'          
004500                                                 'q' 'r' 's' 't'          
004600                                                 'u' 'v' 'w' 'x'          
004700                                                 'y' 'z'                  
004800                                                 '0' THRU '9',            
004900                                                 '-', '.',                
005000                                                 '_', '~'.                
005100                                                                          
005200     03  WS-HEX-CHAR             PIC X(8) VALUE SPACES.                   
005300                                                                          
005400 01  WS-MESSAGES.                                                         
005500     03  WS-MESSAGE-SUCCESS.                                              
005600         05  WS-KVDLEN-IN        PIC ZZ,ZZZ,ZZ9.                          
005700         05  FILLER              PIC X(27)   VALUE                        
005800               ' chars converted to '.                                    
005900         05  WS-KVDLEN-OUT       PIC ZZ,ZZZ,ZZ9.                          
006000         05  FILLER              PIC X(13)   VALUE                        
006100               ' chars'.                                                  
006200                                                                          
006300                                                                          
006400 LINKAGE SECTION.                                                         
006500*01 -COPY WURLCON1                                                        
006600 01  URL-KVDLEN-IN              PIC S9(9) COMP.                           
006700 01  URL-DATA-IN                PIC X(3000).                              
006800 01  URL-KVDLEN-OUT             PIC S9(9) COMP.                           
006900 01  URL-DATA-OUT               PIC X(3000).                              
007000                                                                          
007100 PROCEDURE DIVISION                                                       
007200              USING URL-CONTROL-AREA                                      
007300                    URL-KVDLEN-IN                                         
007400                    URL-DATA-IN                                           
007500                    URL-KVDLEN-OUT                                        
007600                    URL-DATA-OUT.                                         
007700                                                                          
007800 MAIN SECTION.                                                            
007900                                                                          
008000     PERFORM A-INIT                                                       
008100     PERFORM B-URL-ENCODE                                                 
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     GOBACK                                                               
008500     .                                                                    
008600                                                                          
008700 A-INIT SECTION.                                                          
008800                                                                          
008900     MOVE ZERO                   TO URL-KDRC                              
009000     MOVE 1                      TO WS-IN-POS                             
009100                                    WS-OUT-POS                            
009200     EVALUATE URL-KDCALL                                                  
009300       WHEN 001                                                           
009400         CONTINUE                                                         
009500       WHEN OTHER                                                         
009600         MOVE 10                 TO URL-KDRC                              
009700         MOVE 'Invalid call type'                                         
009800                                 TO URL-MESSAGE                           
009900         SET SOME-ERROR          TO TRUE                                  
010000     END-EVALUATE                                                         
010100     .                                                                    
010200                                                                          
010300 B-URL-ENCODE SECTION.                                                    
010400                                                                          
010500     PERFORM                                                              
010600     VARYING WS-IN-POS FROM 1 BY 1                                        
010700       UNTIL WS-IN-POS > URL-KVDLEN-IN                                    
010800       MOVE URL-DATA-IN(WS-IN-POS:1)                                      
010900                                 TO WS-CHAR                               
011000       IF UNRESERVED-CHAR                                                 
011100         STRING WS-CHAR DELIMITED BY SIZE                                 
011200                               INTO URL-DATA-OUT                          
011300           WITH POINTER WS-OUT-POS                                        
011400       ELSE                                                               
011500         MOVE WS-CHAR            TO WS-CHAR-UTF8                          
011600         MOVE SPACES             TO WS-HEX-CHAR                           
011700         MOVE FUNCTION HEX-OF (WS-CHAR-UTF8)                              
011800                                 TO WS-HEX-CHAR                           
011900         PERFORM                                                          
012000         VARYING IX FROM 1 BY 2                                           
012100           UNTIL IX > 8                                                   
012200              OR WS-HEX-CHAR (IX:2) = SPACES                              
012300           STRING                                                         
012400             '%'                            DELIMITED BY SIZE             
012500             WS-HEX-CHAR (IX : 2)           DELIMITED BY SIZE             
012600                               INTO URL-DATA-OUT                          
012700             WITH POINTER WS-OUT-POS                                      
012800           END-STRING                                                     
012900         END-PERFORM                                                      
013000       END-IF                                                             
013100     END-PERFORM                                                          
013200     .                                                                    
013300                                                                          
013400 Z-FINIT SECTION.                                                         
013500     SUBTRACT 1 FROM WS-OUT-POS                                           
013600                             GIVING URL-KVDLEN-OUT                        
013700                                                                          
013800     IF URL-KDRC = ZERO                                                   
013900       MOVE URL-KVDLEN-IN        TO WS-KVDLEN-IN                          
014000       MOVE URL-KVDLEN-OUT       TO WS-KVDLEN-OUT                         
014100       MOVE WS-MESSAGE-SUCCESS   TO URL-MESSAGE                           
014200     END-IF                                                               
014300D    DISPLAY URL-MESSAGE                                                  
014400D    DISPLAY 'URL-DATA-IN =' URL-DATA-IN  (1:URL-KVDLEN-IN )              
014500D    DISPLAY 'URL-DATA-OUT=' URL-DATA-OUT (1:URL-KVDLEN-OUT)              
014600     .                                                                    
