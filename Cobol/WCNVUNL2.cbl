000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVUNL2.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM UNICODE (UCS2 OR UTF8) TO EASTERN                
001100*        EUROPEAN EBCDIC CODEPAGE (CP0870/CS0959 = CCSID 0870)            
001200*        THIS IS A MULTI LINGUAL CODEPAGE USED CORRESPONDING              
001210*        TO "LATIN-2", AND USED BY ALBANIA, CZECHIA, SLOVAKIA             
001220*        HUNGARY, POLAND, ROMANIA, SLOVENIA, CROATIA, BOSNIA              
001230*        SERBIA, MONTENEGRO AND MACEDONIA (SOME OF THESE                  
001240*        COUNTRIES ALSO USE CYRILLIC)                                     
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001801                                                                          
001810*    -- CHECKED BY WY2000                                                 
001900 77  IDPGM                       PIC X(8)    VALUE 'WCNVUNL2'.            
002000 77  YES                         PIC X       VALUE 'Y'.                   
002010 77  JA                          PIC X       VALUE 'J'.                   
002100 77  NOO                         PIC X       VALUE 'N'.                   
002400*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
002500 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
002600 77  FLENG                       PIC S9(4)   COMP.                        
002700                                                                          
002800*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
002900 77  IXF                         PIC S9(4)   COMP.                        
003000 77  IXT                         PIC S9(4)   COMP.                        
003090                                                                          
003091*    -- HELP-FIELD TO HANDLE DIFFERNT BYTE ORDER SCHEMES                  
003092*    -- 0 = NORMAL/MSB ORDER, 1 = REVERSE/LSB ORDER                       
003093 77  BO                          PIC S9(4)   COMP.                        
003100                                                                          
003200*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003300 01  WNUM-GRP.                                                            
003400     03 FILLER                   PIC X       VALUE LOW-VALUE.             
003500     03 WBYTE                    PIC X.                                   
003600 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
003700                                                                          
003800*    -- INDICES TO CONVERSION TABLES                                      
003900 77  IX0                         PIC S9(4)   COMP.                        
003910 77  IX1                         PIC S9(4)   COMP.                        
004000 77  IX2                         PIC S9(4)   COMP.                        
004100                                                                          
004700*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
004800 77  TEMP-1BYTE                  PIC X.                                   
005000     EJECT                                                                
005310*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
005320 77  EPTR                        PIC S9(4)  COMP.                         
005330*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
005340 77  IXF-DISP                    PIC ZZ9.                                 
005350     EJECT                                                                
005360*    -- WORK FIELDS WHEN CONVERTING VALUE TO HEX-CHAR                     
005370 01  TEMP-4BYTE-X.                                                        
005380     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005390     03 TEMP-2BYTE-X.                                                     
005391         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
005392 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
005393                                 PIC S9(9)   COMP.                        
005394                                                                          
005395*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
005396 01  HEX-CHAR                    PIC X(16)  VALUE                         
005397                                 '0123456789ABCDEF'.                      
005398 77  IXH                         PIC S9(4)   COMP.                        
005399 77  IXU                         PIC S9(4)   COMP.                        
005400                                                                          
005401*    -- WORK FIELDS FOR CREATING HEX-CHAR VALUES                          
005402 01  TEMP-HEX-ENT.                                                        
005403     03  FILLER                  PIC X(1)   VALUE 'X'.                    
005404     03  TEMP-HEXNR              PIC X(4).                                
005405                                                                          
005406*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
005407 01  TECONV-TEMP                 PIC X(3000).                             
005408                                                                          
005409*    -- USED WHEN CONVERTING FROM UTF8 TO UCS2                            
005410 01  UTF8-LENG                   PIC S9(9)   COMP.                        
005411 01  UCS2-LENG                   PIC S9(9)   COMP.                        
005412                                                                          
005413*    -- SUBROUTINE FOR CONVERTING FROM UTF8 TO UCS2                       
005414 01  WCNVUTFU                    PIC X(8)    VALUE 'WCNVUTFU'.            
005415                                                                          
005420     EJECT                                                                
005500*CONTROL NOSOURCE                                                         
005600*01  -COPY WCNVU870                                                       
005900*CONTROL SOURCE                                                           
006000     EJECT                                                                
006100 LINKAGE SECTION.                                                         
006200                                                                          
006300 01  -COPY WCNVAREA                                                       
006400     EJECT                                                                
006500 PROCEDURE DIVISION USING WCNVAREA.                                       
006600 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800     PERFORM A-INIT                                                       
006900                                                                          
007000     MOVE 1 TO IXF                                                        
007100     PERFORM UNTIL IXF > FLENG                                            
007200                                                                          
007300*      -- TRY CONVERTING TO SINGLEBYTE CODE (CP 870)                      
007400       MOVE TECONV-TEMP (IXF + BO:1)     TO WBYTE                         
007420       ADD  WNUM  1                  GIVING IX0                           
007500       MOVE TAB2-IND-U870 (IX0)         TO IX1                            
007600       MOVE TECONV-TEMP (IXF + 1 - BO:1) TO WBYTE                         
007700       ADD  WNUM  1                  GIVING IX2                           
007800       MOVE UCS-TO-CP870 (IX1, IX2)     TO TEMP-1BYTE                     
007900       IF TEMP-1BYTE = X'3F'                                              
008520*      -- TEXT COULD NOT BE CONVERTED                                     
008530         MOVE 'F' TO KDSVAR                                               
008540         MOVE IXF TO IXF-DISP                                             
008570         PERFORM S01-HEX-TO-CHAR                                          
008580         STRING 'POS ' IXF-DISP ' ' TEMP-HEX-ENT '. '                     
008590              DELIMITED BY SIZE                                           
008591         INTO BEFEL WITH POINTER EPTR                                     
008600       END-IF                                                             
009200       PERFORM B-MOVE-TO-OUTPUT-FIELD                                     
009400                                                                          
009500       ADD 2 TO IXF                                                       
009600     END-PERFORM                                                          
009700                                                                          
010100     MOVE ZERO TO RETURN-CODE                                             
010200     GOBACK                                                               
010300     .                                                                    
010400     EJECT                                                                
010500 A-INIT SECTION.                                                          
010600     SKIP2                                                                
010700*    -- SO FAR ALL IS OK                                                  
010900     MOVE SPACE TO KDSVAR, BEFEL                                          
010910     MOVE 1 TO EPTR                                                       
011410                                                                          
011420*    -- PREPARE FOR DIFFERENT BYTE ORDERS                                 
011430     IF KDBYTEORD = 'M' OR FLUTF8 = JA OR YES                             
011440       MOVE 0   TO BO                                                     
011450     END-IF                                                               
011460     IF KDBYTEORD = 'L'                                                   
011470       MOVE 1   TO BO                                                     
011480     END-IF                                                               
011500                                                                          
011900*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
012000     MOVE ZERO  TO IXT                                                    
012100     MOVE SPACE TO TECONV-TO                                              
012110                                                                          
012120     IF FLUTF8 = JA OR YES                                                
012130*      -- UTF8 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
012140*      -- (IGNORE TRAILING NULL VALUES)                                   
012150       MOVE ZERO TO UTF8-LENG                                             
012160       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
012170         TALLYING UTF8-LENG FOR LEADING LOW-VALUE                         
012171       IF UTF8-LENG = ZERO                                                
012172*      -- TRY TRAILING SPACE INSTEAD                                      
012173         INSPECT FUNCTION REVERSE (TECONV-FROM)                           
012174           TALLYING UTF8-LENG FOR LEADING SPACE                           
012175       END-IF                                                             
012180       COMPUTE UTF8-LENG = TECONV-LENG - UTF8-LENG                        
012190*      -- CONVERT UTF8 INPUT TO UCS2 BEFORE CONVERTING TO EBCDIC          
012192       MOVE LENGTH OF TECONV-TEMP TO UCS2-LENG                            
012193       CALL WCNVUTFU USING TECONV-FROM UTF8-LENG                          
012194                           TECONV-TEMP UCS2-LENG                          
012195       MOVE UCS2-LENG TO FLENG                                            
012196     ELSE                                                                 
012197*      -- UCS2 - COMPUTE LENGTH OF TEXT IN FROM FIELD                     
012198*      -- (IGNORE TRAILING DOUBLE-BYTE SPACES)                            
012199       MOVE ZERO TO FLENG                                                 
012200       INSPECT FUNCTION REVERSE (TECONV-FROM)                             
012201         TALLYING FLENG FOR LEADING SPACE                                 
012202       COMPUTE FLENG = TECONV-LENG - FLENG                                
012203       MOVE TECONV-FROM          TO TECONV-TEMP                           
012204     END-IF                                                               
012210     .                                                                    
012300     EJECT                                                                
012400 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
012500     SKIP2                                                                
012600*    -- MOVE THE CONVERTED SINGLE BYTE VALUE TO THE OUTPUT                
012700*    -- FIELD IF THERE IS ROOM FOR IT.                                    
012800*    -- SIGNAL OVERFLOW OF OUTPUT FIELD WITH A T IN KDSVAR                
012900                                                                          
014100     IF IXT < KVMAXTL                                                     
014200       ADD 1 TO IXT                                                       
014300       MOVE TEMP-1BYTE TO TECONV-TO (IXT:1)                               
014400     ELSE                                                                 
014401*      -- DON'T OVERLAY ANY F VALUE                                       
014410       IF KDSVAR = SPACE                                                  
014500         MOVE 'T'  TO KDSVAR                                              
014510         MOVE IXF TO IXF-DISP                                             
014520         STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                    
014530         INTO BEFEL WITH POINTER EPTR                                     
014600       END-IF                                                             
014700     END-IF                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 S01-HEX-TO-CHAR  SECTION.                                                
017000     SKIP2                                                                
017100     MOVE TECONV-TEMP (IXF + BO:1)     TO TEMP-2BYTE-X (1:1)              
017200     MOVE TECONV-TEMP (IXF + 1 - BO:1) TO TEMP-2BYTE-X (2:1)              
017300     MOVE 4 TO IXU                                                        
017400     MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                                  
017500     PERFORM 4 TIMES                                                      
017600       DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                      
017700              REMAINDER IXH                                               
017800       MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)                    
017900       SUBTRACT 1 FROM IXU                                                
018000     END-PERFORM                                                          
018100     .                                                                    
