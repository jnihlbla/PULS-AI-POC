000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVUNSV.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM UNICODE (UCS2 OR UTF8) TO SWEDISH                
001100*        EBCDIC CODEPAGE (CP278/CS697 = CCSID 278)                        
001200                                                                          
001300     SKIP3                                                                
001400 DATA DIVISION.                                                           
001500     SKIP3                                                                
001600 WORKING-STORAGE SECTION.                                                 
001700                                                                          
001800                                                                          
001900*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                       PIC X(8)    VALUE 'WCNVUNSV'.            
002100 77  YES                         PIC X       VALUE 'Y'.                   
002200 77  JA                          PIC X       VALUE 'J'.                   
002300 77  NOO                         PIC X       VALUE 'N'.                   
002400*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
002500 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
002600 77  FLENG                       PIC S9(4)   COMP.                        
002700                                                                          
002800*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
002900 77  IXF                         PIC S9(4)   COMP.                        
003000 77  IXT                         PIC S9(4)   COMP.                        
003100                                                                          
003200*    -- HELP-FIELD TO HANDLE DIFFERNT BYTE ORDER SCHEMES                  
003300*    -- 0 = NORMAL/MSB ORDER, 1 = REVERSE/LSB ORDER                       
003400 77  BO                          PIC S9(4)   COMP.                        
003500                                                                          
003600*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003700 01  WNUM-GRP.                                                            
003800     03 FILLER                   PIC X       VALUE LOW-VALUE.             
003900     03 WBYTE                    PIC X.                                   
004000 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
004100                                                                          
004200*    -- INDICES TO CONVERSION TABLES                                      
004300 77  IX0                         PIC S9(4)   COMP.                        
004400 77  IX1                         PIC S9(4)   COMP.                        
004500 77  IX2                         PIC S9(4)   COMP.                        
004600                                                                          
004700*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
004800 77  TEMP-1BYTE                  PIC X.                                   
004900     EJECT                                                                
005000*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
005100 77  EPTR                        PIC S9(4)  COMP.                         
005200*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
005300 77  IXF-DISP                    PIC ZZ9.                                 
005400     EJECT                                                                
005500*    -- WORK FIELDS WHEN CONVERTING VALUE TO HEX-CHAR                     
005600 01  TEMP-4BYTE-X.                                                        
005700     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005800     03 TEMP-2BYTE-X.                                                     
005900         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
006000 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
006100                                 PIC S9(9)   COMP.                        
006200                                                                          
006300*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
006400 01  HEX-CHAR                    PIC X(16)  VALUE                         
006500                                 '0123456789ABCDEF'.                      
006600 77  IXH                         PIC S9(4)   COMP.                        
006700 77  IXU                         PIC S9(4)   COMP.                        
006800                                                                          
006900*    -- WORK FIELDS FOR CREATING HEX-CHAR VALUES                          
007000 01  TEMP-HEX-ENT.                                                        
007100     03  FILLER                  PIC X(1)   VALUE 'X'.                    
007200     03  TEMP-HEXNR              PIC X(4).                                
007300                                                                          
007400*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
007500 01  TECONV-TEMP                 PIC X(3000).                             
007600                                                                          
007610*    -- USED WHEN CONVERTING FROM UTF8 TO UCS2                            
007700 01  UTF8-LENG                   PIC S9(9)   COMP.                        
007710 01  UCS2-LENG                   PIC S9(9)   COMP.                        
007711                                                                          
007712*    -- SUBROUTINE FOR CONVERTING FROM UTF8 TO UCS2                       
007720 01  WCNVUTFU                    PIC X(8)    VALUE 'WCNVUTFU'.            
007800                                                                          
007900     EJECT                                                                
008000*CONTROL NOSOURCE                                                         
008100*01  -COPY WCNVU278                                                       
008200*CONTROL SOURCE                                                           
008300     EJECT                                                                
008400 LINKAGE SECTION.                                                         
008500                                                                          
008600 01  -COPY WCNVAREA                                                       
008700     EJECT                                                                
008800 PROCEDURE DIVISION USING WCNVAREA.                                       
008900 MAIN SECTION.                                                            
009000     SKIP2                                                                
009100     PERFORM A-INIT                                                       
009200                                                                          
009300     MOVE 1 TO IXF                                                        
009400     PERFORM UNTIL IXF > FLENG                                            
009500                                                                          
009600*      -- TRY CONVERTING TO SINGLEBYTE CODE (CP278)                       
009700       MOVE TECONV-TEMP (IXF + BO:1)     TO WBYTE                         
009800       ADD  WNUM  1                  GIVING IX0                           
009900       MOVE TAB2-IND-U278 (IX0)          TO IX1                           
010000       MOVE TECONV-TEMP (IXF + 1 - BO:1) TO WBYTE                         
010100       ADD  WNUM  1                  GIVING IX2                           
010200       MOVE UCS-TO-CP278 (IX1, IX2)      TO TEMP-1BYTE                    
010300       IF TEMP-1BYTE = X'3F'                                              
010400*      -- TEXT COULD NOT BE CONVERTED                                     
010500         MOVE 'F' TO KDSVAR                                               
010600         MOVE IXF TO IXF-DISP                                             
010700         PERFORM S01-HEX-TO-CHAR                                          
010800         STRING 'POS ' IXF-DISP ' ' TEMP-HEX-ENT '. '                     
010900              DELIMITED BY SIZE                                           
011000         INTO BEFEL WITH POINTER EPTR                                     
011100       END-IF                                                             
011200       PERFORM B-MOVE-TO-OUTPUT-FIELD                                     
011300                                                                          
011400       ADD 2 TO IXF                                                       
011500     END-PERFORM                                                          
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012200     SKIP2                                                                
012300*    -- SO FAR ALL IS OK                                                  
012400     MOVE SPACE TO KDSVAR, BEFEL                                          
012500     MOVE 1 TO EPTR                                                       
013200                                                                          
013300*    -- PREPARE FOR DIFFERENT BYTE ORDERS                                 
013400     IF KDBYTEORD = 'M' OR FLUTF8 = JA OR YES                             
013500       MOVE 0   TO BO                                                     
013600     END-IF                                                               
013700     IF KDBYTEORD = 'L'                                                   
013800       MOVE 1   TO BO                                                     
013900     END-IF                                                               
014000                                                                          
014100*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
014200     MOVE ZERO  TO IXT                                                    
014300     MOVE SPACE TO TECONV-TO                                              
014310                                                                          
014320     IF FLUTF8 = JA OR YES                                                
014330*      -- UTF8 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
014340*      -- (IGNORE TRAILING NULL VALUES)                                   
014350       MOVE ZERO TO UTF8-LENG                                             
014360       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
014370         TALLYING UTF8-LENG FOR LEADING LOW-VALUE                         
014371       IF UTF8-LENG = ZERO                                                
014372*      -- TRY TRAILING SPACE INSTEAD                                      
014373         INSPECT FUNCTION REVERSE (TECONV-FROM)                           
014374           TALLYING UTF8-LENG FOR LEADING SPACE                           
014375       END-IF                                                             
014380       COMPUTE UTF8-LENG = TECONV-LENG - UTF8-LENG                        
014390*      -- CONVERT UTF8 INPUT TO UCS2 BEFORE CONVERTING TO EBCDIC          
014392       MOVE LENGTH OF TECONV-TEMP TO UCS2-LENG                            
014393       CALL WCNVUTFU USING TECONV-FROM UTF8-LENG                          
014394                           TECONV-TEMP UCS2-LENG                          
014395       MOVE UCS2-LENG TO FLENG                                            
014396     ELSE                                                                 
014397*      -- UCS2 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
014398*      -- (IGNORE TRAILING DOUBLE-BYTE SPACES)                            
014399       MOVE ZERO TO FLENG                                                 
014400       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
014401         TALLYING FLENG FOR LEADING SPACE                                 
014402       COMPUTE FLENG = TECONV-LENG - FLENG                                
014404       MOVE TECONV-FROM          TO TECONV-TEMP                           
014405     END-IF                                                               
014410     .                                                                    
014500     EJECT                                                                
014600 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
014700     SKIP2                                                                
014800*    -- MOVE THE CONVERTED SINGLE BYTE VALUE TO THE OUTPUT                
014900*    -- FIELD IF THERE IS ROOM FOR IT.                                    
015000*    -- SIGNAL OVERFLOW OF OUTPUT FIELD WITH A T IN KDSVAR                
015100                                                                          
015200     IF IXT < KVMAXTL                                                     
015300       ADD 1 TO IXT                                                       
015400       MOVE TEMP-1BYTE TO TECONV-TO (IXT:1)                               
015500     ELSE                                                                 
015600*      -- DON'T OVERLAY ANY F VALUE                                       
015700       IF KDSVAR = SPACE                                                  
015800         MOVE 'T'  TO KDSVAR                                              
015900         MOVE IXF TO IXF-DISP                                             
016000         STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                    
016100         INTO BEFEL WITH POINTER EPTR                                     
016200       END-IF                                                             
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 S01-HEX-TO-CHAR  SECTION.                                                
016700     SKIP2                                                                
016800     MOVE TECONV-TEMP (IXF + BO:1)     TO TEMP-2BYTE-X (1:1)              
016900     MOVE TECONV-TEMP (IXF + 1 - BO:1) TO TEMP-2BYTE-X (2:1)              
017000     MOVE 4 TO IXU                                                        
017100     MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                                  
017200     PERFORM 4 TIMES                                                      
017300       DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                      
017400              REMAINDER IXH                                               
017500       MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)                    
017600       SUBTRACT 1 FROM IXU                                                
017700     END-PERFORM                                                          
017800     .                                                                    
