000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVUNCE.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   02/05/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM UNICODE (UCS2 OR UTF8) TO EBCDIC                 
001100*        WITH CHARACTER ENTITIES FOR ALL NON ASCII CHARACTERS             
001200*        (HAVING VALUE > X7F). ASCII CHARACTERS ARE TRANSLATED            
001300*        TO EBCDIC USING THE CP037 (= AMERICAN EBCDIC)                    
001400*        TRANSLATION TABLE.                                               
001410*                                                                         
001420*    CHANGE REQUESTS:                                                     
001430*    2005-11-01  This module will now be able to produce decimal          
001440*     Installed  character entities, when this result in a                
001450*    2005-12-04  REPRESENTATION WITH FEWER BYTES.                         
001460*                I.e. this new release will produce mixed both            
001470*                hex and dec entities accordingly to this                 
001480*                desirement.  /Conny                                      
001500*                                                                         
001600     SKIP3                                                                
001700 DATA DIVISION.                                                           
001800     SKIP3                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100                                                                          
002200*    -- CHECKED BY WY2000                                                 
002300 77  IDPGM                       PIC X(8)    VALUE 'WCNVUNCE'.            
002400 77  YES                         PIC X       VALUE 'Y'.                   
002500 77  JA                          PIC X       VALUE 'J'.                   
002600 77  NOO                         PIC X       VALUE 'N'.                   
002700*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
002800 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
002900 77  FLENG                       PIC S9(4)   COMP.                        
003000                                                                          
003100*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
003200 77  IXF                         PIC S9(4)   COMP.                        
003300 77  IXT                         PIC S9(4)   COMP.                        
003400                                                                          
003500*    -- HELP-FIELD TO HANDLE DIFFERNT BYTE ORDER SCHEMES                  
003600*    -- 0 = NORMAL/MSB ORDER, 1 = REVERSE/LSB ORDER                       
003700 77  BO                          PIC S9(4)   COMP.                        
003800                                                                          
003900*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
004000 01  WNUM-GRP.                                                            
004100     03 FILLER                   PIC X       VALUE LOW-VALUE.             
004200     03 WBYTE                    PIC X.                                   
004300 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
004400                                                                          
004500*    -- INDICES TO CONVERSION TABLES                                      
004600 77  IX0                         PIC S9(4)   COMP.                        
004700 77  IX1                         PIC S9(4)   COMP.                        
004800 77  IX2                         PIC S9(4)   COMP.                        
004810                                                                          
004820 77  SW-ENTNR-LENGTH             PIC XX.                                  
004830     88  HEX      VALUE 'X4'.                                             
004840     88  DEC-4    VALUE 'D4'.                                             
004850     88  DEC-3    VALUE 'D3'.                                             
004900                                                                          
005000*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
005100 77  TEMP-1BYTE                  PIC X.                                   
005110 77  NUM1                        PIC 9.                                   
005200     EJECT                                                                
005300*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
005400 77  EPTR                        PIC S9(4)  COMP.                         
005500*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
005600 77  IXF-DISP                    PIC ZZ9.                                 
005700     EJECT                                                                
005800*    -- WORK FIELDS WHEN CONVERTING VALUE TO HEX-CHAR                     
005900 01  TEMP-4BYTE-X.                                                        
006000     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
006100     03 TEMP-2BYTE-X.                                                     
006200         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
006300 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
006400                                 PIC S9(9)   COMP.                        
006500                                                                          
006600*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
006700 01  HEX-CHAR                    PIC X(16)  VALUE                         
006800                                 '0123456789ABCDEF'.                      
006900 77  IXH                         PIC S9(4)   COMP.                        
007000 77  IXU                         PIC S9(4)   COMP.                        
007100                                                                          
007200*    -- WORK FIELDS FOR CREATING HEX & DEC CHAR VALUES                    
007300 01  TEMP-HEX-ENT-8.                                                      
007400     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
007500     03  TEMP-HEXNR-4            PIC X(4).                                
007600     03  FILLER                  PIC X(1)   VALUE ';'.                    
007601                                                                          
007610 01  TEMP-DEC-ENT-7.                                                      
007620     03  FILLER                  PIC X(2)   VALUE '&#'.                   
007630     03  TEMP-DECNR-4            PIC 9(4).                                
007640     03  FILLER                  PIC X(1)   VALUE ';'.                    
007700                                                                          
007710 01  TEMP-DEC-ENT-6.                                                      
007720     03  FILLER                  PIC X(2)   VALUE '&#'.                   
007730     03  TEMP-DECNR-3            PIC 9(3).                                
007740     03  FILLER                  PIC X(1)   VALUE ';'.                    
007750                                                                          
007751 01  TEMP-DECNR                  PIC 9(8)   VALUE ZERO.                   
007752 01  DEC-1                       PIC 9(2)   VALUE ZERO.                   
007753 01  DEC-16                      PIC 9(3)   VALUE ZERO.                   
007754 01  DEC-256                     PIC 9(4)   VALUE ZERO.                   
007755 01  DEC-4096                    PIC 9(5)   VALUE ZERO.                   
007760                                                                          
007800*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
007900 01  TECONV-TEMP                 PIC X(3000).                             
008000                                                                          
008100*    -- USED WHEN CONVERTING FROM UTF8 TO UCS2                            
008200 01  UTF8-LENG                   PIC S9(9)   COMP.                        
008300 01  UCS2-LENG                   PIC S9(9)   COMP.                        
008400                                                                          
008500*    -- SUBROUTINE FOR CONVERTING FROM UTF8 TO UCS2                       
008600 01  WCNVUTFU                    PIC X(8)    VALUE 'WCNVUTFU'.            
008700                                                                          
008800     EJECT                                                                
008900*CONTROL NOSOURCE                                                         
009000*01  -COPY WCNVU037                                                       
009100*CONTROL SOURCE                                                           
009200     EJECT                                                                
009300 LINKAGE SECTION.                                                         
009400                                                                          
009500 01  -COPY WCNVAREA                                                       
009600     EJECT                                                                
009700 PROCEDURE DIVISION USING WCNVAREA.                                       
009800 MAIN SECTION.                                                            
009900     SKIP2                                                                
010000     PERFORM A-INIT                                                       
010100                                                                          
010200     MOVE 1 TO IXF                                                        
010300     PERFORM UNTIL IXF > FLENG                                            
010400                                                                          
010500       IF  TECONV-TEMP(IXF + BO:1) = LOW-VALUE                            
010600       AND TECONV-TEMP(IXF + 1 - BO:1) <= X'7F'                           
010700                                                                          
010800*        -- PLAIN ASCII CHARACTER - CONVERT IT TO EBCDIC (CP037)          
010900         MOVE TECONV-TEMP (IXF + BO:1)     TO WBYTE                       
011000         ADD  WNUM  1                  GIVING IX0                         
011100         MOVE TAB2-IND-U37 (IX0)          TO IX1                          
011200         MOVE TECONV-TEMP (IXF + 1 - BO:1) TO WBYTE                       
011300         ADD  WNUM  1                  GIVING IX2                         
011400         MOVE UCS-TO-CP37 (IX1, IX2)      TO TEMP-1BYTE                   
011500         PERFORM B-SIMPLE-BYTE                                            
011600                                                                          
011700       ELSE                                                               
011800         PERFORM C-MOVE-CHARACTER-ENTITY                                  
011900       END-IF                                                             
012000                                                                          
012100       ADD 2 TO IXF                                                       
012200     END-PERFORM                                                          
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 A-INIT SECTION.                                                          
012900     SKIP2                                                                
013000*    -- SO FAR ALL IS OK                                                  
013100     MOVE SPACE TO KDSVAR, BEFEL                                          
013200     MOVE 1 TO EPTR                                                       
013300                                                                          
014100*    -- PREPARE FOR DIFFERENT BYTE ORDERS                                 
014200     IF KDBYTEORD = 'M' OR FLUTF8 = JA OR YES                             
014300       MOVE 0   TO BO                                                     
014400     END-IF                                                               
014500     IF KDBYTEORD = 'L'                                                   
014600       MOVE 1   TO BO                                                     
014700     END-IF                                                               
014800                                                                          
014900*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
015000     MOVE ZERO  TO IXT                                                    
015100     MOVE SPACE TO TECONV-TO                                              
015200                                                                          
015300     IF FLUTF8 = JA OR YES                                                
015400*      -- CONVERT UTF8 INPUT TO UCS2 BEFORE CONVERTING TO EBCDIC          
015510*      -- COMPUTE LENGTH OF TEXT IN FROM FIELD                            
015520*      -- (IGNORE TRAILING NULL VALUES)                                   
015530       MOVE ZERO TO UTF8-LENG                                             
015540       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
015550         TALLYING UTF8-LENG FOR LEADING LOW-VALUE                         
015551       IF UTF8-LENG = ZERO                                                
015552*      -- TRY TRAILING SPACE INSTEAD                                      
015553         INSPECT FUNCTION REVERSE (TECONV-FROM)                           
015554           TALLYING UTF8-LENG FOR LEADING SPACE                           
015555       END-IF                                                             
015560       COMPUTE UTF8-LENG = TECONV-LENG - UTF8-LENG                        
015800       MOVE LENGTH OF TECONV-TEMP TO UCS2-LENG                            
015900       CALL WCNVUTFU USING TECONV-FROM UTF8-LENG                          
016000                           TECONV-TEMP UCS2-LENG                          
016100       MOVE UCS2-LENG TO FLENG                                            
016200     ELSE                                                                 
016210*      -- UCS2 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
016220*      -- (IGNORE TRAILING DOUBLE-BYTE SPACES)                            
016230       MOVE ZERO TO FLENG                                                 
016240       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
016250         TALLYING FLENG FOR LEADING SPACE                                 
016260       COMPUTE FLENG = TECONV-LENG - FLENG                                
016280       MOVE TECONV-FROM          TO TECONV-TEMP                           
016290     END-IF                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 B-SIMPLE-BYTE  SECTION.                                                  
016600     SKIP2                                                                
016700*    -- MOVE THE CONVERTED SINGLE BYTE VALUE TO THE OUTPUT                
016800*    -- FIELD IF THERE IS ROOM FOR IT.                                    
016900*    -- SIGNAL OVERFLOW OF OUTPUT FIELD WITH A T IN KDSVAR                
017000                                                                          
017100     IF IXT < KVMAXTL                                                     
017200       ADD 1 TO IXT                                                       
017300       MOVE TEMP-1BYTE TO TECONV-TO (IXT:1)                               
017400     ELSE                                                                 
017500*      -- DON'T OVERLAY ANY F VALUE                                       
017600       IF KDSVAR = SPACE                                                  
017700         MOVE 'T'  TO KDSVAR                                              
017800         MOVE IXF TO IXF-DISP                                             
017900         STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                    
018000         INTO BEFEL WITH POINTER EPTR                                     
018100       END-IF                                                             
018200     END-IF                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 C-MOVE-CHARACTER-ENTITY  SECTION.                                        
018600     SKIP2                                                                
018700*    -- COMPOSE AND MOVE THE CHARACTER ENTITY CORRESPONDING               
018800*    -- TO THE INPUT UNICODE CHARACTER ("&#xNNNN;")                       
018810*    -- OR IF POSSIBLE THE SHORTER ("&#NNNN;") OR ("&#NNN;")              
018900*    -- TO THE OUTPUT FIELD IF THERE IS ROOM FOR IT.                      
019000*    -- SIGNAL OVERFLOW OF OUTPUT FIELD WITH A T IN KDSVAR                
019100                                                                          
019200     IF IXT <= KVMAXTL - 8                                                
019300       PERFORM S01-HEX-TO-CHAR                                            
019310       IF HEX                                                             
019400         ADD 1 TO IXT                                                     
019500         MOVE TEMP-HEX-ENT-8 TO TECONV-TO (IXT:8)                         
019600         ADD 7 TO IXT                                                     
019610       ELSE                                                               
019611         IF DEC-4                                                         
019612           ADD 1 TO IXT                                                   
019613           MOVE TEMP-DEC-ENT-7 TO TECONV-TO (IXT:7)                       
019614           ADD 6 TO IXT                                                   
019620         END-IF                                                           
019621         IF DEC-3                                                         
019622           ADD 1 TO IXT                                                   
019623           MOVE TEMP-DEC-ENT-6 TO TECONV-TO (IXT:6)                       
019624           ADD 5 TO IXT                                                   
019625         END-IF                                                           
019630       END-IF                                                             
019700     ELSE                                                                 
019800       MOVE 'T'  TO KDSVAR                                                
019900     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 S01-HEX-TO-CHAR  SECTION.                                                
020300     SKIP2                                                                
020310*    -- FIRST, BUILD A 4 BYTE HEX REPRESENTATION OF THE CHARACTER         
020400     MOVE TECONV-TEMP (IXF + BO:1)     TO TEMP-2BYTE-X (1:1)              
020500     MOVE TECONV-TEMP (IXF + 1 - BO:1) TO TEMP-2BYTE-X (2:1)              
020600     MOVE 4 TO IXU                                                        
020700     MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                                  
020800     PERFORM 4 TIMES                                                      
020900       DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                      
021000              REMAINDER IXH                                               
021100       MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR-4 (IXU:1)                  
021200       SUBTRACT 1 FROM IXU                                                
021300     END-PERFORM                                                          
021302*    -- NOW, TRY IF DECIMAL VALUE IS SHORTER THAN THE HEX                 
021303*    -- IF THE DEC IS 4 CHAR, THE ENTITY BECOMES 1 BYTE SHORTER           
021304*    -- IF THE DEC IS 3 CHAR, THE ENTITY BECOMES 2 BYTE SHORTER           
021305     INITIALIZE   TEMP-DECNR DEC-1 DEC-16 DEC-256 DEC-4096                
021307                                                                          
021308     IF TEMP-HEXNR-4(4:1) NUMERIC                                         
021309       MOVE TEMP-HEXNR-4(4:1) TO NUM1                                     
021310       MULTIPLY NUM1 BY 1 GIVING DEC-1                                    
021311     ELSE                                                                 
021312       EVALUATE TEMP-HEXNR-4 (4:1)                                        
021313         WHEN 'A' ADD 10 TO DEC-1                                         
021314         WHEN 'B' ADD 11 TO DEC-1                                         
021315         WHEN 'C' ADD 12 TO DEC-1                                         
021316         WHEN 'D' ADD 13 TO DEC-1                                         
021317         WHEN 'E' ADD 14 TO DEC-1                                         
021318         WHEN 'F' ADD 15 TO DEC-1                                         
021319       END-EVALUATE                                                       
021320     END-IF                                                               
021321     IF TEMP-HEXNR-4(3:1) NUMERIC                                         
021322       MOVE TEMP-HEXNR-4(3:1) TO NUM1                                     
021323       MULTIPLY NUM1 BY 16 GIVING DEC-16                                  
021324     ELSE                                                                 
021325       EVALUATE TEMP-HEXNR-4 (3:1)                                        
021326         WHEN 'A' ADD 160 TO DEC-16                                       
021327         WHEN 'B' ADD 176 TO DEC-16                                       
021328         WHEN 'C' ADD 192 TO DEC-16                                       
021329         WHEN 'D' ADD 208 TO DEC-16                                       
021330         WHEN 'E' ADD 224 TO DEC-16                                       
021331         WHEN 'F' ADD 240 TO DEC-16                                       
021332       END-EVALUATE                                                       
021333     END-IF                                                               
021334     IF TEMP-HEXNR-4(2:1) NUMERIC                                         
021335       MOVE TEMP-HEXNR-4(2:1) TO NUM1                                     
021336       MULTIPLY NUM1 BY 256 GIVING DEC-256                                
021338     ELSE                                                                 
021339       EVALUATE TEMP-HEXNR-4 (2:1)                                        
021340         WHEN 'A' ADD 2560 TO DEC-256                                     
021341         WHEN 'B' ADD 2816 TO DEC-256                                     
021342         WHEN 'C' ADD 3072 TO DEC-256                                     
021343         WHEN 'D' ADD 3328 TO DEC-256                                     
021344         WHEN 'E' ADD 3584 TO DEC-256                                     
021345         WHEN 'F' ADD 3840 TO DEC-256                                     
021346       END-EVALUATE                                                       
021347     END-IF                                                               
021348     IF TEMP-HEXNR-4(1:1) NUMERIC                                         
021349       MOVE TEMP-HEXNR-4(1:1) TO NUM1                                     
021350       MULTIPLY NUM1 BY 4096 GIVING DEC-4096                              
021352     ELSE                                                                 
021353       EVALUATE TEMP-HEXNR-4 (1:1)                                        
021354         WHEN 'A' ADD 40960 TO DEC-4096                                   
021355         WHEN 'B' ADD 45056 TO DEC-4096                                   
021356         WHEN 'C' ADD 49152 TO DEC-4096                                   
021357         WHEN 'D' ADD 53248 TO DEC-4096                                   
021358         WHEN 'E' ADD 57344 TO DEC-4096                                   
021359         WHEN 'F' ADD 61440 TO DEC-4096                                   
021360       END-EVALUATE                                                       
021361     END-IF                                                               
021362     ADD DEC-1 DEC-16 DEC-256 DEC-4096 TO TEMP-DECNR                      
021370     IF TEMP-DECNR > 9999                                                 
021380       SET HEX TO TRUE                                                    
021390     ELSE                                                                 
021391       IF TEMP-DECNR > 999                                                
021392         SET DEC-4  TO TRUE                                               
021393         MOVE TEMP-DECNR  TO TEMP-DECNR-4                                 
021394       ELSE                                                               
021395         IF TEMP-DECNR > 99                                               
021396           SET DEC-3 TO TRUE                                              
021397           MOVE TEMP-DECNR TO TEMP-DECNR-3                                
021398         END-IF                                                           
021399       END-IF                                                             
021400     END-IF                                                               
021500     .                                                                    
