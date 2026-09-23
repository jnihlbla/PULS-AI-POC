000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WMSGCONV.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   21/12/07.                                                
000500*                                                                         
000600*    FUNCTION:                                                            
000700*      Translate web message numbers to corresponding text and            
000800*      code for use in API communications.                                
000900*                                                                         
001000*    CALLED BY:                                                           
001100*      CALL WMSGCONV USING MSG-CONV-AREA                                  
001200*                                                                         
001300*    INPUT ARGUMENTS:                                                     
001400*      MSG-CONV-IDSPRAK      ISO language code for the output             
001500*                            message text.                                
001600*                            Only EN supported currently.                 
001700*                                                                         
001800*      MSG-CONV-IDSYSTEM-WEB Optional data item.                          
001900*                            PULS Web reuses some message numbers         
002000*                            across different projects. If left           
002100*                            blank, "wl01" table is checked.              
002200*                                                                         
002300*      MSG-CONV-IDMSG-IN     Input message code.                          
002400*                                                                         
002500*      MSG-CONV-IDLEMT       Optional data item.                          
002600*                            Some messages have a variable portion        
002700*                            that contains a field name.                  
002800*                                                                         
002900*    OUTPUT ARGUMENTS:                                                    
003000*                                                                         
003100*      MSG-CONV-IDMSG-OUT    Output message code.                         
003200*                                                                         
003300*      MSG-CONV-MESSAGE      Output message.                              
003400*                                                                         
003500*                                                                         
003600                                                                          
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  PROGRAM-NAMN            PIC X(8)  VALUE 'WMSGCONV'.                  
004100                                                                          
004200 77  WS-MESSAGE              PIC X(100).                                  
004300 77  WS-NAME                 PIC X(30).                                   
004400                                                                          
004500 77  WS-PART1                PIC X(100).                                  
004600 77  WS-PART2                PIC X(100).                                  
004700 77  WS-IDELMT-PART1         PIC X(16).                                   
004800 77  WS-IDELMT-PART2         PIC X(16).                                   
004900                                                                          
005000 COPY WMSGWL01.                                                           
005100 COPY WMSGELMT.                                                           
005200                                                                          
005300 LINKAGE SECTION.                                                         
005400 COPY WMSGCONV.                                                           
005500                                                                          
005600 PROCEDURE DIVISION  USING  MSG-CONV-AREA.                                
005700                                                                          
005800     MOVE SPACE                  TO MSG-CONV-IDMSG-OUT                    
005900                                    MSG-CONV-MESSAGE                      
006000                                    WS-MESSAGE                            
006100                                    WS-NAME                               
006200                                    WS-PART1                              
006300                                    WS-PART2                              
006400                                    WS-IDELMT-PART1                       
006500                                    WS-IDELMT-PART2                       
006600                                                                          
006700     IF MSG-CONV-IDMSG-IN NOT = SPACE                                     
006800       SEARCH ALL TAB1-MSG                                                
006900         AT END                                                           
007000           STRING 'Unknown error '       DELIMITED BY SIZE                
007100                  MSG-CONV-IDMSG-IN      DELIMITED BY SIZE                
007200                  '. Contact support'    DELIMITED BY SIZE                
007300                               INTO WS-MESSAGE                            
007400           MOVE 'PULS-XXX'       TO MSG-CONV-IDMSG-OUT                    
007500         WHEN TAB1-IDMSG-WEB (TAB1-IX) = MSG-CONV-IDMSG-IN                
007600           MOVE TAB1-IDMSG (TAB1-IX)                                      
007700                                 TO MSG-CONV-IDMSG-OUT                    
007800           MOVE TAB1-MESSAGE (TAB1-IX)                                    
007900                                 TO WS-MESSAGE                            
008000       END-SEARCH                                                         
008100     END-IF                                                               
008200                                                                          
008300     IF MSG-CONV-IDELMT NOT = SPACE                                       
008400       MOVE ZERO                 TO TALLY                                 
008500       INSPECT WS-MESSAGE TALLYING TALLY FOR ALL '¤'                      
008600       IF TALLY > 0                                                       
008700         INSPECT MSG-CONV-IDELMT TALLYING TALLY FOR ALL '*'               
008800         IF TALLY > 0                                                     
008900           UNSTRING MSG-CONV-IDELMT DELIMITED BY '*'                      
009000                               INTO WS-IDELMT-PART1                       
009100                                    WS-IDELMT-PART2                       
009200         ELSE                                                             
009300           MOVE MSG-CONV-IDELMT  TO WS-IDELMT-PART1                       
009400         END-IF                                                           
009500                                                                          
009600         SEARCH ALL TAB-ELMT                                              
009700           AT END                                                         
009800             MOVE WS-IDELMT-PART1                                         
009900                                 TO WS-NAME                               
010000           WHEN TAB-IDELMT (TAB-IX) = WS-IDELMT-PART1                     
010100             MOVE TAB-NAME (TAB-IX)                                       
010200                                 TO WS-NAME                               
010300         END-SEARCH                                                       
010400         UNSTRING WS-MESSAGE DELIMITED BY '¤'                             
010500                               INTO WS-PART1                              
010600                                    WS-PART2                              
010700         STRING FUNCTION TRIM (WS-PART1) DELIMITED BY SIZE                
010800                  ' '                    DELIMITED BY SIZE                
010900                FUNCTION TRIM (WS-NAME)  DELIMITED BY SIZE                
011000                  ' '                    DELIMITED BY SIZE                
011100                FUNCTION TRIM (WS-IDELMT-PART2)                           
011200                                         DELIMITED BY SIZE                
011300                  ' '                    DELIMITED BY SIZE                
011400                FUNCTION TRIM (WS-PART2) DELIMITED BY SIZE                
011500                               INTO WS-MESSAGE                            
011600       ELSE                                                               
011700         STRING FUNCTION TRIM (WS-MESSAGE) DELIMITED BY SIZE              
011800                  ' '                      DELIMITED BY SIZE              
011900                FUNCTION TRIM (MSG-CONV-IDELMT)                           
012000                                           DELIMITED BY SIZE              
012100                               INTO WS-MESSAGE                            
012200       END-IF                                                             
012300     END-IF                                                               
012400                                                                          
012500     MOVE WS-MESSAGE             TO MSG-CONV-MESSAGE                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
