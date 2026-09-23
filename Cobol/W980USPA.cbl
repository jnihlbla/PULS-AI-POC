000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W980USPA.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   07/08/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        FETCH SOP ID AND PASSWORD                                        
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 DATA DIVISION.                                                           
001400     SKIP3                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600                                                                          
001700 77  IDPGM                       PIC X(8)    VALUE 'W980USPA'.            
001800                                                                          
001900 01  SOP-KDRET                   PIC S9(4)   BINARY VALUE ZERO.           
002000 01  SOP-DDPREFIX                PIC X       VALUE 'W'.                   
002100                                                                          
002200 01  DYNAMISKA-SUBPROGRAM.                                                
002300     03  W009WTOP                PIC X(8)    VALUE 'W009WTOP'.            
002400     03  W980WSPC                PIC X(8)    VALUE 'W980WSPC'.            
002500     03  W980PASS                PIC X(8)    VALUE 'W980PASS'.            
002600                                                                          
002700*------- PARAMETERAR TILL W009PASS (BERÄKNA SOP-LÖSENORD)                 
002800*                                                                         
002900 01  -COPY W980PASS                                                       
003000                                                                          
003100*------- PARAMETERAR TILL W009WTOP (SKRIV PÅ JES-LOGGEN)                  
003200*                                                                         
003300 01  WTOP-PARM.                                                           
003400     03  WTOP-MSG-LENGD        PIC S9(4)   COMP VALUE +68.                
003500     03  WTOP-MSG.                                                        
003600         05  WTOP-MSG-ID.                                                 
003700             07  FILLER        PIC X(3)    VALUE 'SOP'.                   
003800             07  WTOP-MSG-NR   PIC 9(3).                                  
003900         05  WTOP-MSG-TEXT     PIC X(62).                                 
004000                                                                          
004100                                                                          
004200 01  MSG-IX                     PIC S9(4)   COMP.                         
004300 01  M.                                                                   
004400     03  M5.                                                              
004500       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
004600       05 FILLER                PIC 999     VALUE 005.                    
004700       05 FILLER                PIC X(23)   VALUE                         
004800          'S  CAN NOT OPEN DDNAME '.                                      
004900       05 M5-DDNAMN             PIC X(8).                                 
005000       05 FILLER                PIC X(29)   VALUE SPACE.                  
005100       05 FILLER                PIC X(60)   VALUE SPACE.                  
005200                                                                          
005300     03  M28.                                                             
005400       05 FILLER        COMP    PIC S9(4)   VALUE +16.                    
005500       05 FILLER                PIC 999     VALUE 028.                    
005600       05 FILLER                PIC X(24)   VALUE                         
005700          'S  CAN NOT CLOSE DDNAME '.                                     
005800       05 M28-DDNAMN            PIC X(8).                                 
005900       05 FILLER                PIC X(28)   VALUE SPACE.                  
006000       05 FILLER                PIC X(60)   VALUE SPACE.                  
006100                                                                          
006200 01  M-RED  REDEFINES  M.                                                 
006300     03  FILLER OCCURS 2 .                                                
006400       05  MSG-RETCODE         PIC S9(4)    COMP.                         
006500       05  MSG-NR              PIC 999.                                   
006600       05  MSG1                PIC X(60).                                 
006700       05  MSG2                PIC X(60).                                 
006800                                                                          
006900     EJECT                                                                
007000*------- PARAMETERAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)           
007100 01  WSPACE-DD-PARM.                                                      
007200     03   WSPACE-DD-LENGD    PIC S9(4)   COMP  VALUE +10.                 
007300     03   WSPACE-DD-NAMN     PIC X(8)    VALUE 'SOPDD1  '.                
007400                                                                          
007500 01  FUNKTIONSKODER.                                                      
007600     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
007700     03  FQUIT               PIC X(4)    VALUE 'QUIT'.                    
007800     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
007900     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
008000                                                                          
008100 01  WSRKOD                  PIC X.                                       
008200                                                                          
008300*----------------- GENERELLA ARBETS-PARAMETRAR                            
008400 01  NAMN-PARM.                                                           
008500     03   NAMN-LENGD         PIC S9(4)   COMP.                            
008600     03   NAMN-VAERDE        PIC X(20).                                   
008700                                                                          
008800 01  ATTR-PARM.                                                           
008900     03   ATTR-LENGD         PIC S9(4)   COMP.                            
009000     03   ATTR-VAERDE        PIC X(20).                                   
009100                                                                          
009200 01  DATA-PARM.                                                           
009300     03   DATA-LENGD         PIC S9(4)   COMP.                            
009400     03   DATA-VAERDE        PIC X(240).                                  
009500                                                                          
009600 01  SOP-PARM.                                                            
009700     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
009800     03   FILLER             PIC X(5)    VALUE '*SOP*'.                   
009900     SKIP3                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200 01  -COPY W980USPA                                                       
010300                                                                          
010400     EJECT                                                                
010500 PROCEDURE DIVISION USING USPA-AREA.                                      
010600 MAIN SECTION.                                                            
010700                                                                          
010800     PERFORM X-OPEN-SOPREG                                                
010900     IF SOP-KDRET = 0                                                     
011000       MOVE '&USER'  TO ATTR-VAERDE                                       
011100       MOVE +7       TO ATTR-LENGD                                        
011200       CALL W980WSPC USING FGETF WSRKOD SOP-PARM                          
011300                     ATTR-PARM DATA-PARM                                  
011400       IF WSRKOD NOT = SPACE                                              
011500         MOVE SPACE        TO USPA-IDUSER                                 
011600       ELSE                                                               
011700         MOVE SPACE TO USPA-IDUSER                                        
011800         MOVE DATA-VAERDE(1:DATA-LENGD - 2) TO USPA-IDUSER                
011900                                                                          
012000         MOVE '&DROWP' TO ATTR-VAERDE                                     
012100         MOVE +8       TO ATTR-LENGD                                      
012200         CALL W980WSPC USING FGETF WSRKOD SOP-PARM                        
012300                       ATTR-PARM DATA-PARM                                
012400         IF WSRKOD NOT = SPACE                                            
012500           MOVE SPACE        TO USPA-IDUSER                               
012600         ELSE                                                             
012700           IF DATA-VAERDE(1:DATA-LENGD - 2) NOT NUMERIC                   
012800             MOVE SPACE TO USPA-IDPW                                      
012900             MOVE DATA-VAERDE(1:DATA-LENGD - 2) TO USPA-IDPW              
013000*            -- DATA-VAERDE ANVÄNDS SOM PASSWORD                          
013100           ELSE                                                           
013200             MOVE DATA-VAERDE(1:DATA-LENGD - 2) TO PASS-NUMBER            
013300                                                                          
013400*            -- GENERERA LÖSENORDET FRÅN TALET                            
013500             CALL W980PASS USING PASS-AREA                                
013600                                                                          
013700             MOVE PASS-WORD   TO USPA-IDPW                                
013800           END-IF                                                         
013900         END-IF                                                           
014000       END-IF                                                             
014100                                                                          
014200       PERFORM Y-CLOSE-SOPREG                                             
014300                                                                          
014400       MOVE ZERO TO RETURN-CODE                                           
014500     ELSE                                                                 
014600       MOVE 16    TO RETURN-CODE                                          
014700       MOVE SPACE TO USPA-IDUSER, USPA-IDPW                               
014800     END-IF                                                               
014900                                                                          
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015300 X-OPEN-SOPREG   SECTION.                                                 
015400                                                                          
015500     MOVE SPACE TO WSPACE-DD-NAMN                                         
015600     STRING SOP-DDPREFIX 'SOPDD1' DELIMITED BY SPACE                      
015700            INTO WSPACE-DD-NAMN                                           
015800                                                                          
015900     CALL W980WSPC USING FOPEN WSRKOD WSPACE-DD-PARM                      
016000     IF WSRKOD NOT = SPACE                                                
016100       MOVE WSPACE-DD-NAMN TO M5-DDNAMN                                   
016200       MOVE 1 TO MSG-IX                                                   
016300       PERFORM S90-DISPLAY-MSG                                            
016400     END-IF                                                               
016500     .                                                                    
016600                                                                          
016700                                                                          
016800 Y-CLOSE-SOPREG   SECTION.                                                
016900     SKIP2                                                                
017000     CALL W980WSPC USING FQUIT WSRKOD                                     
017100     CALL W980WSPC USING FCLSE WSRKOD                                     
017200     IF WSRKOD NOT = SPACE                                                
017300       MOVE 2  TO MSG-IX                                                  
017400       MOVE WSPACE-DD-NAMN TO M28-DDNAMN                                  
017500       PERFORM S90-DISPLAY-MSG                                            
017600     END-IF                                                               
017700     .                                                                    
017800                                                                          
017900                                                                          
018000 S90-DISPLAY-MSG  SECTION.                                                
018100     SKIP2                                                                
018200     MOVE MSG-RETCODE (MSG-IX) TO SOP-KDRET                               
018300     MOVE 'SOPXXX'             TO WTOP-MSG-ID                             
018400     MOVE MSG-NR (MSG-IX)      TO WTOP-MSG-NR                             
018500     MOVE MSG1 (MSG-IX)        TO WTOP-MSG-TEXT                           
018600     CALL W009WTOP USING WTOP-PARM                                        
018700                                                                          
018800     IF MSG2 (MSG-IX) NOT = SPACE                                         
018900       MOVE SPACE              TO WTOP-MSG-ID                             
019000       MOVE MSG2 (MSG-IX)      TO WTOP-MSG-TEXT                           
019100       CALL W009WTOP USING WTOP-PARM                                      
019200     END-IF                                                               
019300     .                                                                    
