000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WB64CONV.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   21/12/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        This program handles decodes base64 string.                      
001000*        Output is in UTF-8 format.                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 CONFIGURATION SECTION.                                                   
001900*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500 77  OK-SWITCH                   PIC X     VALUE 'Y'.                     
002600     88  ALL-OK                            VALUE 'Y'.                     
002700     88  SOME-ERROR                        VALUE 'N'.                     
002800                                                                          
002900 01  WS-BINARY-AREA.                                                      
003000     03  WS-FULL-WORD.                                                    
003100         05  WS-FW-BINARY        PIC 9(8)  BINARY VALUE ZERO.             
003200     03  FILLER REDEFINES WS-FULL-WORD.                                   
003300         05  WS-FW-1             PIC X.                                   
003400         05  WS-FW-2             PIC X.                                   
003500         05  WS-FW-3             PIC X.                                   
003600         05  WS-FW-4             PIC X.                                   
003700     03  FILLER REDEFINES WS-FULL-WORD.                                   
003800         05  FILLER              PIC X.                                   
003900         05  WS-FW-234           PIC X(3).                                
004000     03  WS-HALF-WORD.                                                    
004100         05  WS-HW-BINARY        PIC 9(4)  BINARY VALUE ZERO.             
004200     03  FILLER REDEFINES WS-HALF-WORD.                                   
004300         05  WS-HW-1             PIC X.                                   
004400         05  WS-HW-2             PIC X.                                   
004500                                                                          
004600 01  WS-WORK-AREA.                                                        
004700     03  WS-IN-POS               PIC 9(8)  BINARY VALUE ZERO.             
004800     03  WS-OUT-POS              PIC 9(8)  BINARY VALUE ZERO.             
004900     03  WS-PAD-COUNT            PIC 9(4)  BINARY VALUE ZERO.             
005000     03  WS-CHARACTER            PIC X     VALUE ' '.                     
005100     03  WS-CHARACTER-NUM REDEFINES WS-CHARACTER                          
005200                                 PIC 9.                                   
005300                                                                          
005400 01  WS-BASE64-AREA.                                                      
005500     03  WS-BASE64-CHARS.                                                 
005600         05  FILLER              PIC X(26) VALUE                          
005700               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                              
005800         05  FILLER              PIC X(26) VALUE                          
005900               'abcdefghijklmnopqrstuvwxyz'.                              
006000         05  FILLER              PIC X(12) VALUE                          
006100               '0123456789+/'.                                            
006200     03  FILLER REDEFINES WS-BASE64-CHARS.                                
006300         05  WS-BASE64-CHAR      PIC X     OCCURS 64.                     
006400                                                                          
006500 01  WS-MESSAGES.                                                         
006600     03  WS-MESSAGE-SUCCESS.                                              
006700         05  WS-KVDLEN-IN        PIC ZZ,ZZZ,ZZ9.                          
006800         05  FILLER              PIC X(27)   VALUE                        
006900               ' Base64 chars converted to '.                             
007000         05  WS-KVDLEN-OUT       PIC ZZ,ZZZ,ZZ9.                          
007100         05  FILLER              PIC X(13)   VALUE                        
007200               ' chars'.                                                  
007300     03  WS-MESSAGE-20.                                                   
007400         05  FILLER              PIC X(40)   VALUE                        
007500               'Non Base64 characters in input string'.                   
007600     03  WS-MESSAGE-21.                                                   
007700         05  FILLER              PIC X(40)   VALUE                        
007800               'Output length is too small'.                              
007900                                                                          
008000 LINKAGE SECTION.                                                         
008100*01 -COPY WB64CON1                                                        
008200 01  B64-KVDLEN-IN              PIC S9(9) COMP.                           
008300 01  B64-DATA-IN                PIC X(1000000).                           
008400 01  B64-KVDLEN-OUT             PIC S9(9) COMP.                           
008500 01  B64-DATA-OUT               PIC X(1000000).                           
008600                                                                          
008700 PROCEDURE DIVISION                                                       
008800              USING B64-CONTROL-AREA                                      
008900                    B64-KVDLEN-IN                                         
009000                    B64-DATA-IN                                           
009100                    B64-KVDLEN-OUT                                        
009200                    B64-DATA-OUT.                                         
009300                                                                          
009400 MAIN SECTION.                                                            
009500                                                                          
009600     PERFORM A-INIT                                                       
009700     PERFORM                                                              
009800       UNTIL WS-IN-POS > B64-KVDLEN-IN                                    
009900          OR SOME-ERROR                                                   
010000       PERFORM B-BASE64-TO-BIN                                            
010100     END-PERFORM                                                          
010200     PERFORM Z-FINIT                                                      
010300                                                                          
010400     GOBACK                                                               
010500     .                                                                    
010600                                                                          
010700 A-INIT SECTION.                                                          
010800                                                                          
010900     MOVE ZERO                   TO B64-KDRC                              
011000     MOVE 1                      TO WS-IN-POS                             
011100                                    WS-OUT-POS                            
011200     EVALUATE B64-KDCALL                                                  
011300       WHEN 001                                                           
011400         CONTINUE                                                         
011500       WHEN OTHER                                                         
011600         MOVE 10                 TO B64-KDRC                              
011700         MOVE 'Invalid call type'                                         
011800                                 TO B64-MESSAGE                           
011900         SET SOME-ERROR          TO TRUE                                  
012000     END-EVALUATE                                                         
012100     .                                                                    
012200                                                                          
012300 B-BASE64-TO-BIN SECTION.                                                 
012400     MOVE ZERO                   TO WS-FW-BINARY                          
012500                                    WS-PAD-COUNT                          
012600                                                                          
012700     PERFORM BA-EVALUATE-CHARACTER 4 TIMES                                
012800                                                                          
012900     PERFORM BB-MOVE-TO-OUTPUT                                            
013000     .                                                                    
013100                                                                          
013200 BA-EVALUATE-CHARACTER SECTION.                                           
013300     MULTIPLY 64                 BY WS-FW-BINARY.                         
013400                                                                          
013500     IF WS-IN-POS > B64-KVDLEN-IN                                         
013600       MOVE '='                  TO WS-CHARACTER                          
013700     ELSE                                                                 
013800       MOVE B64-DATA-IN (WS-IN-POS:1)                                     
013900                                 TO WS-CHARACTER                          
014000     END-IF                                                               
014100                                                                          
014200     EVALUATE TRUE ALSO TRUE                                              
014300       WHEN WS-CHARACTER NOT < 'A' ALSO WS-CHARACTER NOT > 'Z'            
014400         PERFORM BAA-CONVERT-UPPERCASE                                    
014500       WHEN WS-CHARACTER NOT < 'a' ALSO WS-CHARACTER NOT > 'z'            
014600         PERFORM BAB-CONVERT-LOWERCASE                                    
014700       WHEN WS-CHARACTER NOT < '0' ALSO WS-CHARACTER NOT > '9'            
014800         ADD WS-CHARACTER-NUM    TO WS-FW-BINARY                          
014900         ADD 52                  TO WS-FW-BINARY                          
015000       WHEN WS-CHARACTER = '+' ALSO ANY                                   
015100         ADD 62                  TO WS-FW-BINARY                          
015200       WHEN WS-CHARACTER = '/' ALSO ANY                                   
015300         ADD 63                  TO WS-FW-BINARY                          
015400       WHEN WS-CHARACTER = '=' ALSO ANY                                   
015500         ADD 1                   TO WS-PAD-COUNT                          
015600       WHEN OTHER                                                         
015700         ADD 1                   TO WS-PAD-COUNT                          
015800         MOVE 20                 TO B64-KDRC                              
015900         MOVE WS-MESSAGE-20      TO B64-MESSAGE                           
016000     END-EVALUATE                                                         
016100                                                                          
016200     ADD 1                       TO WS-IN-POS                             
016300     .                                                                    
016400                                                                          
016500  BAA-CONVERT-UPPERCASE SECTION.                                          
016600     IF WS-CHARACTER < 'N'                                                
016700       IF WS-CHARACTER < 'H'                                              
016800         IF WS-CHARACTER < 'E'                                            
016900           EVALUATE WS-CHARACTER                                          
017000             WHEN 'B'                                                     
017100               ADD 1             TO WS-FW-BINARY                          
017200             WHEN 'C'                                                     
017300               ADD 2             TO WS-FW-BINARY                          
017400             WHEN 'D'                                                     
017500               ADD 3             TO WS-FW-BINARY                          
017600           END-EVALUATE                                                   
017700         ELSE                                                             
017800           EVALUATE WS-CHARACTER                                          
017900             WHEN 'E'                                                     
018000               ADD 4             TO WS-FW-BINARY                          
018100             WHEN 'F'                                                     
018200               ADD 5             TO WS-FW-BINARY                          
018300             WHEN 'G'                                                     
018400               ADD 6             TO WS-FW-BINARY                          
018500           END-EVALUATE                                                   
018600         END-IF                                                           
018700       ELSE                                                               
018800         IF WS-CHARACTER < 'K'                                            
018900           EVALUATE WS-CHARACTER                                          
019000             WHEN 'H'                                                     
019100               ADD 7             TO WS-FW-BINARY                          
019200             WHEN 'I'                                                     
019300               ADD 8             TO WS-FW-BINARY                          
019400             WHEN 'J'                                                     
019500               ADD 9             TO WS-FW-BINARY                          
019600           END-EVALUATE                                                   
019700         ELSE                                                             
019800           EVALUATE WS-CHARACTER                                          
019900             WHEN 'K'                                                     
020000               ADD 10            TO WS-FW-BINARY                          
020100             WHEN 'L'                                                     
020200               ADD 11            TO WS-FW-BINARY                          
020300             WHEN 'M'                                                     
020400               ADD 12            TO WS-FW-BINARY                          
020500           END-EVALUATE                                                   
020600         END-IF                                                           
020700       END-IF                                                             
020800     ELSE                                                                 
020900       IF WS-CHARACTER < 'U'                                              
021000         IF WS-CHARACTER < 'R'                                            
021100           EVALUATE WS-CHARACTER                                          
021200             WHEN 'N'                                                     
021300               ADD 13            TO WS-FW-BINARY                          
021400             WHEN 'O'                                                     
021500               ADD 14            TO WS-FW-BINARY                          
021600             WHEN 'P'                                                     
021700               ADD 15            TO WS-FW-BINARY                          
021800             WHEN 'Q'                                                     
021900               ADD 16            TO WS-FW-BINARY                          
022000           END-EVALUATE                                                   
022100         ELSE                                                             
022200           EVALUATE WS-CHARACTER                                          
022300             WHEN 'R'                                                     
022400               ADD 17            TO WS-FW-BINARY                          
022500             WHEN 'S'                                                     
022600               ADD 18            TO WS-FW-BINARY                          
022700             WHEN 'T'                                                     
022800               ADD 19            TO WS-FW-BINARY                          
022900           END-EVALUATE                                                   
023000         END-IF                                                           
023100       ELSE                                                               
023200         IF WS-CHARACTER < 'X'                                            
023300           EVALUATE WS-CHARACTER                                          
023400             WHEN 'U'                                                     
023500               ADD 20            TO WS-FW-BINARY                          
023600             WHEN 'V'                                                     
023700               ADD 21            TO WS-FW-BINARY                          
023800             WHEN 'W'                                                     
023900               ADD 22            TO WS-FW-BINARY                          
024000           END-EVALUATE                                                   
024100         ELSE                                                             
024200           EVALUATE WS-CHARACTER                                          
024300             WHEN 'X'                                                     
024400               ADD 23            TO WS-FW-BINARY                          
024500             WHEN 'Y'                                                     
024600               ADD 24            TO WS-FW-BINARY                          
024700             WHEN 'Z'                                                     
024800               ADD 25            TO WS-FW-BINARY                          
024900           END-EVALUATE                                                   
025000         END-IF                                                           
025100       END-IF                                                             
025200     END-IF                                                               
025300     .                                                                    
025400                                                                          
025500 BAB-CONVERT-LOWERCASE SECTION.                                           
025600                                                                          
025700     IF WS-CHARACTER < 'n'                                                
025800       IF WS-CHARACTER < 'h'                                              
025900         IF WS-CHARACTER < 'e'                                            
026000           EVALUATE WS-CHARACTER                                          
026100             WHEN 'a'                                                     
026200               ADD 26            TO WS-FW-BINARY                          
026300             WHEN 'b'                                                     
026400               ADD 27            TO WS-FW-BINARY                          
026500             WHEN 'c'                                                     
026600               ADD 28            TO WS-FW-BINARY                          
026700             WHEN 'd'                                                     
026800               ADD 29            TO WS-FW-BINARY                          
026900           END-EVALUATE                                                   
027000         ELSE                                                             
027100           EVALUATE WS-CHARACTER                                          
027200             WHEN 'e'                                                     
027300               ADD 30            TO WS-FW-BINARY                          
027400             WHEN 'f'                                                     
027500               ADD 31            TO WS-FW-BINARY                          
027600             WHEN 'g'                                                     
027700               ADD 32            TO WS-FW-BINARY                          
027800           END-EVALUATE                                                   
027900         END-IF                                                           
028000       ELSE                                                               
028100         IF WS-CHARACTER < 'k'                                            
028200           EVALUATE WS-CHARACTER                                          
028300             WHEN 'h'                                                     
028400               ADD 33            TO WS-FW-BINARY                          
028500             WHEN 'i'                                                     
028600               ADD 34            TO WS-FW-BINARY                          
028700             WHEN 'j'                                                     
028800               ADD 35            TO WS-FW-BINARY                          
028900           END-EVALUATE                                                   
029000         ELSE                                                             
029100           EVALUATE WS-CHARACTER                                          
029200             WHEN 'k'                                                     
029300               ADD 36            TO WS-FW-BINARY                          
029400             WHEN 'l'                                                     
029500               ADD 37            TO WS-FW-BINARY                          
029600             WHEN 'm'                                                     
029700               ADD 38            TO WS-FW-BINARY                          
029800           END-EVALUATE                                                   
029900         END-IF                                                           
030000       END-IF                                                             
030100     ELSE                                                                 
030200       IF WS-CHARACTER < 'u'                                              
030300         IF WS-CHARACTER < 'r'                                            
030400           EVALUATE WS-CHARACTER                                          
030500             WHEN 'n'                                                     
030600               ADD 39            TO WS-FW-BINARY                          
030700             WHEN 'o'                                                     
030800               ADD 40            TO WS-FW-BINARY                          
030900             WHEN 'p'                                                     
031000               ADD 41            TO WS-FW-BINARY                          
031100             WHEN 'q'                                                     
031200               ADD 42            TO WS-FW-BINARY                          
031300           END-EVALUATE                                                   
031400         ELSE                                                             
031500           EVALUATE WS-CHARACTER                                          
031600             WHEN 'r'                                                     
031700               ADD 43            TO WS-FW-BINARY                          
031800             WHEN 's'                                                     
031900               ADD 44            TO WS-FW-BINARY                          
032000             WHEN 't'                                                     
032100               ADD 45            TO WS-FW-BINARY                          
032200           END-EVALUATE                                                   
032300         END-IF                                                           
032400       ELSE                                                               
032500         IF WS-CHARACTER < 'x'                                            
032600           EVALUATE WS-CHARACTER                                          
032700             WHEN 'u'                                                     
032800               ADD 46            TO WS-FW-BINARY                          
032900             WHEN 'v'                                                     
033000               ADD 47            TO WS-FW-BINARY                          
033100             WHEN 'w'                                                     
033200               ADD 48            TO WS-FW-BINARY                          
033300           END-EVALUATE                                                   
033400         ELSE                                                             
033500           EVALUATE WS-CHARACTER                                          
033600             WHEN 'x'                                                     
033700               ADD 49            TO WS-FW-BINARY                          
033800             WHEN 'y'                                                     
033900               ADD 50            TO WS-FW-BINARY                          
034000             WHEN 'z'                                                     
034100               ADD 51            TO WS-FW-BINARY                          
034200           END-EVALUATE                                                   
034300         END-IF                                                           
034400       END-IF                                                             
034500     END-IF                                                               
034600     .                                                                    
034700                                                                          
034800 BB-MOVE-TO-OUTPUT SECTION.                                               
034900                                                                          
035000     IF WS-OUT-POS + 2 - WS-PAD-COUNT > B64-KVDLEN-OUT                    
035100       MOVE 21                   TO B64-KDRC                              
035200       MOVE WS-MESSAGE-21        TO B64-MESSAGE                           
035300       SET SOME-ERROR            TO TRUE                                  
035400     END-IF                                                               
035500                                                                          
035600     IF WS-IN-POS > B64-KVDLEN-IN AND                                     
035700        WS-PAD-COUNT > ZERO                                               
035800       IF WS-PAD-COUNT = 1                                                
035900         MOVE WS-FW-2            TO B64-DATA-OUT                          
036000                                    (WS-OUT-POS:1)                        
036100         ADD 1                   TO WS-OUT-POS                            
036200         MOVE WS-FW-3            TO B64-DATA-OUT                          
036300                                    (WS-OUT-POS:1)                        
036400         ADD 1                   TO WS-OUT-POS                            
036500       ELSE                                                               
036600         MOVE WS-FW-2            TO B64-DATA-OUT                          
036700                                    (WS-OUT-POS:1)                        
036800         ADD 1                   TO WS-OUT-POS                            
036900       END-IF                                                             
037000     ELSE                                                                 
037100       MOVE WS-FW-234            TO B64-DATA-OUT                          
037200                                    (WS-OUT-POS:3)                        
037300       ADD 3                     TO WS-OUT-POS                            
037400     END-IF.                                                              
037500                                                                          
037600                                                                          
037700 Z-FINIT SECTION.                                                         
037800     SUBTRACT 1 FROM WS-OUT-POS                                           
037900                             GIVING B64-KVDLEN-OUT                        
038000                                                                          
038100     IF B64-KDRC = ZERO                                                   
038200       MOVE B64-KVDLEN-IN        TO WS-KVDLEN-IN                          
038300       MOVE B64-KVDLEN-OUT       TO WS-KVDLEN-OUT                         
038400       MOVE WS-MESSAGE-SUCCESS   TO B64-MESSAGE                           
038500     END-IF                                                               
038600D    DISPLAY B64-MESSAGE                                                  
038700D    display 'B64-DATA-OUT='                                              
038800D     FUNCTION DISPLAY-OF(                                                
038900D             FUNCTION NATIONAL-OF(                                       
039000D                    B64-DATA-OUT (1:B64-KVDLEN-OUT),                     
039100D                                  1208),                                 
039200D             278)                                                        
039300     .                                                                    
