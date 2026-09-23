000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVUNZH.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM UNICODE (UCS2 OR UTF8) TO TRADITIONAL            
001100*        CHINESE EBCDIC CODEPAGES (CP37/CS697 AND CP835/CS935)            
001110*        TRADITIONAL CHINESE CHARACTERS ARE USED IN TAIWAN.               
001200*        NOTE, THAT FULL CP37/CS697 IS USED FOR SINGLE BYTE               
001202*        CODES RATHER THAN THE SUBSET CP37/CS1175 WHICH IS                
001203*        NORMALLY USED TOGETHER WITH CP835 IN CHINESE EQUIPMENT.          
001204*                                                                         
001205*    TEMPORÄR FIX FÖR ATT KLARA KINESISKT TECKEN X'5185'                  
001206*    SOM INTE KLARAS AV IBM.S KONVERTERINGSTABELL WCNVU835                
001207*                                                                         
001208*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001801                                                                          
001810*    -- CHECKED BY WY2000                                                 
001900 77  IDPGM                       PIC X(8)    VALUE 'WCNVUNZH'.            
002000 77  YES                         PIC X       VALUE 'Y'.                   
002100 77  JA                          PIC X       VALUE 'J'.                   
002200 77  NOO                         PIC X       VALUE 'N'.                   
002300 77  SHIFTOUT                    PIC X       VALUE X'0E'.                 
002400 77  SHIFTIN                     PIC X       VALUE X'0F'.                 
002410 77  EEEE                        PIC XX      VALUE X'EEEE'.               
002500*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
002600 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
002700 77  FLENG                       PIC S9(4)   COMP.                        
002800                                                                          
002900*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
003000 77  IXF                         PIC S9(4)   COMP.                        
003100 77  IXT                         PIC S9(4)   COMP.                        
003190                                                                          
003191*    -- HELP-FIELD TO HANDLE DIFFERNT BYTE ORDER SCHEMES                  
003192*    -- 0 = NORMAL/MSB ORDER, 1 = REVERSE/LSB ORDER                       
003193 77  BO                          PIC S9(4)   COMP.                        
003200                                                                          
003300*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003400 01  WNUM-GRP.                                                            
003500     03 FILLER                   PIC X       VALUE LOW-VALUE.             
003600     03 WBYTE                    PIC X.                                   
003700 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
003800                                                                          
003900*    -- INDICES TO CONVERSION TABLES                                      
004000 77  IX0                         PIC S9(4)   COMP.                        
004100 77  IX1                         PIC S9(4)   COMP.                        
004200 77  IX2                         PIC S9(4)   COMP.                        
004300                                                                          
004400*    -- FLAG INDICATING STATUS OF MIXED-MODE DBCS TEXT                    
004500 77  MODE-SW                     PIC X.                                   
004600 88  SB-MODE                     VALUE 'S'.                               
004700 88  DB-MODE                     VALUE 'D'.                               
004800                                                                          
004900*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
005000 77  TEMP-1BYTE                  PIC X.                                   
005100 77  TEMP-2BYTE                  PIC XX.                                  
005200     EJECT                                                                
005300*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
005400 77  WKDSVAR                     PIC X.                                   
005500                                                                          
005810*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
005820 77  EPTR                        PIC S9(4)  COMP.                         
005830*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
005840 77  IXF-DISP                    PIC ZZ9.                                 
005850     EJECT                                                                
005860*    -- WORK FIELDS WHEN CONVERTING VALUE TO HEX-CHAR                     
005870 01  TEMP-4BYTE-X.                                                        
005880     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005890     03 TEMP-2BYTE-X.                                                     
005891         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
005892 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
005893                                 PIC S9(9)   COMP.                        
005894                                                                          
005895*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
005896 01  HEX-CHAR                    PIC X(16)  VALUE                         
005897                                 '0123456789ABCDEF'.                      
005898 77  IXH                         PIC S9(4)   COMP.                        
005899 77  IXU                         PIC S9(4)   COMP.                        
005900                                                                          
005901*    -- WORK FIELDS FOR CREATING HEX-CHAR VALUES                          
005902 01  TEMP-HEX-ENT.                                                        
005903     03  FILLER                  PIC X(1)   VALUE 'X'.                    
005904     03  TEMP-HEXNR              PIC X(4).                                
005905                                                                          
005906*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
005907 01  TECONV-TEMP                 PIC X(3000).                             
005908                                                                          
005909*    -- USED WHEN CONVERTING FROM UTF8 TO UCS2                            
005910 01  UTF8-LENG                   PIC S9(9)   COMP.                        
005911 01  UCS2-LENG                   PIC S9(9)   COMP.                        
005912                                                                          
005913*    -- SUBROUTINE FOR CONVERTING FROM UTF8 TO UCS2                       
005914 01  WCNVUTFU                    PIC X(8)    VALUE 'WCNVUTFU'.            
005915                                                                          
005920     EJECT                                                                
006000*CONTROL NOSOURCE                                                         
006100*01  -COPY WCNVU037                                                       
006200     EJECT                                                                
006300*01  -COPY WCNVU835                                                       
006400*CONTROL SOURCE                                                           
006500     EJECT                                                                
006600 LINKAGE SECTION.                                                         
006700                                                                          
006800 01  -COPY WCNVAREA                                                       
006900     EJECT                                                                
007000 PROCEDURE DIVISION USING WCNVAREA.                                       
007100 MAIN SECTION.                                                            
007200     SKIP2                                                                
007300     PERFORM A-INIT                                                       
007400                                                                          
007500     MOVE 1 TO IXF                                                        
007600     PERFORM UNTIL IXF > FLENG                                            
007700                                                                          
007800*      -- TRY CONVERTING TO SINGLEBYTE CODE (CP037)                       
007900       MOVE TECONV-TEMP (IXF + BO:1)     TO WBYTE                         
008000       ADD  WNUM  1                  GIVING IX0                           
008100       MOVE TAB2-IND-U37  (IX0)          TO IX1                           
008200       MOVE TECONV-TEMP (IXF + 1 - BO:1) TO WBYTE                         
008300       ADD  WNUM  1                  GIVING IX2                           
008400       MOVE UCS-TO-CP37  (IX1, IX2)      TO TEMP-1BYTE                    
008500       IF TEMP-1BYTE = X'3F'                                              
008600*        -- TRY CONVERTING TO DOUBLEBYTE CODE (CP835)                     
008700         MOVE TECONV-TEMP (IXF + BO:1)     TO WBYTE                       
008800         ADD  WNUM  1                  GIVING IX0                         
008900         MOVE TAB2-IND-U835 (IX0)          TO IX1                         
009000         MOVE TECONV-TEMP (IXF + 1 - BO:1) TO WBYTE                       
009100         ADD  WNUM  1                  GIVING IX2                         
009200         MOVE UCS-TO-CP835-X (IX1, IX2)    TO TEMP-2BYTE                  
009300         IF TEMP-2BYTE = X'FEFE'                                          
009310         AND TECONV-TEMP (IXF:2) NOT = X'FFFD'                            
009320*        -- FFFD ÄR DUBBELBYTE "SUB" OCH SKA BLI FEFE                     
009330*        -- DENNA ÄR ALLTSÅ OK MEDAN ANDRA SKA GE FELMED                  
009400*        -- TEXT COULD NOT BE CONVERTED                                   
009500           MOVE 'F' TO KDSVAR                                             
009510           MOVE IXF TO IXF-DISP                                           
009540           PERFORM S01-HEX-TO-CHAR                                        
009550           STRING 'POS ' IXF-DISP ' ' TEMP-HEX-ENT '. '                   
009560                DELIMITED BY SIZE                                         
009570           INTO BEFEL WITH POINTER EPTR                                   
009571                                                                          
009580           IF TECONV-TEMP (IXF + BO:1) = X'51'                            
009581*                                          MSB                            
009590           AND TECONV-TEMP (IXF + 1 - BO:1) = X'85'                       
009591*                                               LSB                       
009592*            --- FIX FÖR ATT KLARA TECKNET '5185'                         
009593*            --- SOM BETYDER "INRE" LAGRAS TILLF. SOM 'EEEE'              
009594*            --- OBS ATT X'EEEE' EJ HELLER ÄR ETT GODKÄNT TECKEN          
009595*            --- MEN DET BORDE VARA UNIKT I BASEN                         
009596             MOVE EEEE TO TEMP-2BYTE                                      
009600           END-IF                                                         
009610         END-IF                                                           
009700       END-IF                                                             
009800       PERFORM B-MOVE-TO-OUTPUT-FIELD                                     
009900                                                                          
010000       ADD 2 TO IXF                                                       
010100     END-PERFORM                                                          
010200                                                                          
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500                                                                          
010600     MOVE ZERO TO RETURN-CODE                                             
010700     GOBACK                                                               
010800     .                                                                    
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100     SKIP2                                                                
011200*    -- SO FAR ALL IS OK                                                  
011310     MOVE SPACE TO KDSVAR, BEFEL                                          
011320     MOVE 1 TO EPTR                                                       
011910                                                                          
011920*    -- PREPARE FOR DIFFERENT BYTE ORDERS                                 
011930     IF KDBYTEORD = 'M' OR FLUTF8 = JA OR YES                             
011940       MOVE 0   TO BO                                                     
011950     END-IF                                                               
011960     IF KDBYTEORD = 'L'                                                   
011970       MOVE 1   TO BO                                                     
011980     END-IF                                                               
012000                                                                          
012100*    -- SET FLAG INDICATING SINGLE BYTE MODE IN OUTPUT TEXT               
012200     SET SB-MODE TO TRUE                                                  
012300                                                                          
012400*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
012500     MOVE ZERO TO IXT                                                     
012600     MOVE SPACE TO TECONV-TO                                              
012601                                                                          
012610     IF FLUTF8 = JA OR YES                                                
012620*      -- UTF8 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
012630*      -- (IGNORE TRAILING NULL VALUES)                                   
012640       MOVE ZERO TO UTF8-LENG                                             
012650       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
012660         TALLYING UTF8-LENG FOR LEADING LOW-VALUE                         
012661       IF UTF8-LENG = ZERO                                                
012662*      -- TRY TRAILING SPACE INSTEAD                                      
012663         INSPECT FUNCTION REVERSE (TECONV-FROM)                           
012664           TALLYING UTF8-LENG FOR LEADING SPACE                           
012665       END-IF                                                             
012670       COMPUTE UTF8-LENG = TECONV-LENG - UTF8-LENG                        
012680*      -- CONVERT UTF8 INPUT TO UCS2 BEFORE CONVERTING TO EBCDIC          
012691       MOVE LENGTH OF TECONV-TEMP TO UCS2-LENG                            
012692       CALL WCNVUTFU USING TECONV-FROM UTF8-LENG                          
012693                           TECONV-TEMP UCS2-LENG                          
012694       MOVE UCS2-LENG TO FLENG                                            
012695     ELSE                                                                 
012696*      -- UCS2 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
012697*      -- (IGNORE TRAILING DOUBLE-BYTE SPACES)                            
012698       MOVE ZERO TO FLENG                                                 
012699       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
012700         TALLYING FLENG FOR LEADING SPACE                                 
012701       COMPUTE FLENG = TECONV-LENG - FLENG                                
012702       MOVE TECONV-FROM          TO TECONV-TEMP                           
012703     END-IF                                                               
012710     .                                                                    
012800     EJECT                                                                
012900 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
013000     SKIP2                                                                
013100*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO OUTPUT FIELD                
013200*    -- SHIFT TO THE RIGHT MIXED-MODE FIRST, IF NECCESSARY                
013300*    -- SIGNAL OVERFLOW OF OUTPUT FIELD WITH A T IN KDSVAR                
013400                                                                          
013500*    -- SO FAR ALL IS OK                                                  
013600     MOVE SPACE TO WKDSVAR                                                
013700                                                                          
013800     IF TEMP-1BYTE NOT = X'3F'                                            
013900*      -- CONVERSION TO SINGLE-BYTE OK                                    
014000       IF DB-MODE                                                         
014100         IF IXT < KVMAXTL                                                 
014200           ADD 1 TO IXT                                                   
014300           MOVE SHIFTIN TO TECONV-TO (IXT:1)                              
014400           SET SB-MODE  TO TRUE                                           
014500         ELSE                                                             
014600           MOVE 'T'  TO WKDSVAR                                           
014700         END-IF                                                           
014800       END-IF                                                             
014900       IF IXT < KVMAXTL                                                   
015000         ADD 1 TO IXT                                                     
015100         MOVE TEMP-1BYTE TO TECONV-TO (IXT:1)                             
015200       ELSE                                                               
015300         MOVE 'T'  TO WKDSVAR                                             
015400       END-IF                                                             
015500     ELSE                                                                 
015600*      -- CONVERSION TO DOUBLE-BYTE                                       
015700*      -- THERE MUST BE ROOM FOR A FINAL SHIFTIN                          
015800       IF SB-MODE                                                         
015900         IF IXT < KVMAXTL - 3                                             
016000           ADD 1 TO IXT                                                   
016100           MOVE SHIFTOUT TO TECONV-TO (IXT:1)                             
016200           SET DB-MODE   TO TRUE                                          
016300         ELSE                                                             
016400           MOVE 'T'  TO WKDSVAR                                           
016500         END-IF                                                           
016600       END-IF                                                             
016700       IF IXT < KVMAXTL - 2 AND DB-MODE                                   
016800         ADD 1 TO IXT                                                     
016900         MOVE TEMP-2BYTE TO TECONV-TO (IXT:2)                             
017000         ADD 1 TO IXT                                                     
017100       ELSE                                                               
017200         MOVE 'T'  TO WKDSVAR                                             
017300       END-IF                                                             
017400     END-IF                                                               
017500                                                                          
017600*    -- DON'T OVERLAY ANY F VALUE                                         
017700     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
017800       MOVE WKDSVAR TO KDSVAR                                             
017810       MOVE IXF TO IXF-DISP                                               
017820       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
017830       INTO BEFEL WITH POINTER EPTR                                       
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 Z-FINIT SECTION.                                                         
018300     SKIP2                                                                
018400*    -- INSERT A FINAL SHIFTIN CHARACTER IF IN DOUBLE-BYTE MODE           
018500     IF DB-MODE AND IXT < KVMAXTL                                         
018600       ADD 1 TO IXT                                                       
018700       MOVE SHIFTIN   TO TECONV-TO (IXT:1)                                
018800     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 S01-HEX-TO-CHAR  SECTION.                                                
019200     SKIP2                                                                
019300     MOVE TECONV-TEMP (IXF + BO:1)     TO TEMP-2BYTE-X (1:1)              
019400     MOVE TECONV-TEMP (IXF + 1 - BO:1) TO TEMP-2BYTE-X (2:1)              
019500     MOVE 4 TO IXU                                                        
019600     MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                                  
019700     PERFORM 4 TIMES                                                      
019800       DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                      
019900              REMAINDER IXH                                               
020000       MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)                    
020100       SUBTRACT 1 FROM IXU                                                
020200     END-PERFORM                                                          
020300     .                                                                    
