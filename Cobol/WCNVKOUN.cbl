000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVKOUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/06/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000         CONVERSION FROM KOREAN EBCDIC SBCS/DBCS CODEPAGES                
001100         (CP833/CS1278 AND CP834/CS1050 = CCSID 9125) TO                  
001200         UNICODE. UCS2 OR UTF8 FORMAT CAN BE GENERATED.                   
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                       PIC X(8)    VALUE 'WCNVKOUN'.            
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
003200*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003300 01  WNUM-GRP.                                                            
003400     03 FILLER                   PIC X       VALUE LOW-VALUE.             
003500     03 WBYTE                    PIC X.                                   
003600 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
003700                                                                          
003800*    -- INDICES TO CONVERSION TABLES                                      
003900 77  IX0                         PIC S9(4)   COMP.                        
004000 77  IX1                         PIC S9(4)   COMP.                        
004100 77  IX2                         PIC S9(4)   COMP.                        
004200                                                                          
004300*    -- FLAG INDICATING STATUS OF MIXED-MODE DBCS TEXT                    
004400 77  MODE-SW                     PIC X.                                   
004500 88  SB-MODE                     VALUE 'S'.                               
004600 88  DB-MODE                     VALUE 'D'.                               
004700                                                                          
004800*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
004900 01  TEMP-4BYTE-X.                                                        
005000     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005100     03 TEMP-2BYTE-X.                                                     
005200         05 TEMP-1BYTE-FIRST     PIC X.                                   
005300         05 TEMP-1BYTE-X         PIC X.                                   
005400     03 TEMP-2BYTE-N  REDEFINES TEMP-2BYTE-X                              
005500                                 PIC S9(4)   COMP.                        
005600 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
005700                                 PIC S9(9)   COMP.                        
005800                                                                          
005900*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
006000 01  HEX-CHAR                    PIC X(16)  VALUE                         
006100                                 '0123456789ABCDEF'.                      
006200 77  IXH                         PIC S9(4)   COMP.                        
006300 77  IXU                         PIC S9(4)   COMP.                        
006400                                                                          
006500*    -- WORK FIELDS FOR CREATING SGML HEX CHAR ENTITIES                   
006600*    -- MAY BE USED FOR CHARACTERS > FF                                   
006700 01  TEMP-HEX-ENT.                                                        
006800     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
006900     03  TEMP-HEXNR              PIC X(4).                                
007000     03  FILLER                  PIC X(1)   VALUE ';'.                    
007100                                                                          
007200*    -- WORK FIELDS FOR CREATING SGML DECIMAL CHAR ENTITIES               
007300*    -- MAY BE USED FOR LATIN 1 SUPLEMENT CHARACTERS (0A-FF)              
007400 01  TEMP-LAT-ENT.                                                        
007500     03  FILLER                  PIC X(2)   VALUE '&#'.                   
007600     03  TEMP-LATNR              PIC 9(3).                                
007700     03  FILLER                  PIC X(1)   VALUE ';'.                    
007800     EJECT                                                                
007900*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
008000 77  WKDSVAR                     PIC X.                                   
008100*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
008200 77  EPTR                        PIC S9(4)  COMP.                         
008300*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
008400 77  IXF-DISP                    PIC ZZ9.                                 
008500                                                                          
008600*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
008700 01  TECONV-TEMP                 PIC X(3000).                             
008800                                                                          
008900*    -- CONVERSION TABLE FOR ASCII-TO-EBCDIC                              
009000 01  ASCII-CHARACTER-GROUP.                                               
009100     03  PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.             
009200     03  PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.             
009300     03  PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.             
009400     03  PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.             
009500     03  PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.             
009600     03  PIC X(16) VALUE X'707172737475767778797A7B7C7D7E7F'.             
009700 01  ASCII-CHARACTERS REDEFINES ASCII-CHARACTER-GROUP                     
009800         PIC X(96).                                                       
009900                                                                          
010000 01  EBCDIC-CHARACTER-GROUP.                                              
010100     03  PIC X(16) VALUE ' !"#$%& ()*+,-./'.                              
010200     03  PIC X(16) VALUE '0123456789:;<=>?'.                              
010300     03  PIC X(16) VALUE '@ABCDEFGHIJKLMNO'.                              
010400     03  PIC X(16) VALUE 'PQRSTUVWXYZ[\]^_'.                              
010500     03  PIC X(16) VALUE '`abcdefghijklmno'.                              
010600     03  PIC X(16) VALUE 'pqrstuvwxyz{|}~ '.                              
010700 01  EBCDIC-CHARACTERS REDEFINES EBCDIC-CHARACTER-GROUP                   
010800         PIC X(96).                                                       
010900                                                                          
011000*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
011100 01  UTF8-LENG                   PIC S9(9)   COMP.                        
011200 01  UCS2-LENG                   PIC S9(9)   COMP.                        
011300                                                                          
011400*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
011500 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
011600     EJECT                                                                
011700*CONTROL NOSOURCE                                                         
011800*01  -COPY WCNV833U                                                       
011900     EJECT                                                                
012000*01  -COPY WCNV834U                                                       
012100*CONTROL SOURCE                                                           
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500 01  -COPY WCNVAREA                                                       
012600     EJECT                                                                
012700 PROCEDURE DIVISION USING WCNVAREA.                                       
012800 MAIN SECTION.                                                            
012900     SKIP2                                                                
013000     PERFORM A-INIT                                                       
013100                                                                          
013200     MOVE 1 TO IXF                                                        
013300     PERFORM UNTIL IXF > FLENG                                            
013400                                                                          
013500       IF SB-MODE                                                         
013600         IF TECONV-FROM (IXF:1) = SHIFT-OUT                               
013700*          -- SKIP THE CHARACTER AND SHIFT TO DB-MODE                     
013800           SET DB-MODE TO TRUE                                            
013900           ADD 1 TO IXF                                                   
014000         ELSE                                                             
014100*          -- CONVERT FROM SINGLEBYTE CODE (CP833)                        
014200           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
014300           ADD  WNUM  1             GIVING IX0                            
014400           MOVE CP833-TO-UCS-X (IX0)    TO TEMP-2BYTE-X                   
014500           IF TEMP-2BYTE-X = X'001A'                                      
014600             MOVE 'F' TO KDSVAR                                           
014700             MOVE IXF TO IXF-DISP                                         
014800             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
014900             INTO BEFEL WITH POINTER EPTR                                 
015000           END-IF                                                         
015100           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
015200           ADD 1 TO IXF                                                   
015300         END-IF                                                           
015400       ELSE                                                               
015500         IF TECONV-FROM (IXF:1) = SHIFT-IN                                
015600*          -- SKIP THE CHARACTER AND SHIFT TO SB-MODE                     
015700           SET SB-MODE TO TRUE                                            
015800           ADD 1 TO IXF                                                   
015900         ELSE                                                             
016000*        -- CONVERT FROM DOUBLEBYTE CODE (CP834)                          
016100           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
016200           ADD  WNUM  1             GIVING IX0                            
016300           MOVE TAB2-IND-834U (IX0)     TO IX1                            
016400           MOVE TECONV-FROM (IXF + 1:1) TO WBYTE                          
016500           ADD  WNUM  1             GIVING IX2                            
016600           MOVE CP834-TO-UCS-X (IX1, IX2) TO TEMP-2BYTE-X                 
016700           IF TEMP-2BYTE-X = X'FFFD'                                      
016800             MOVE 'F' TO KDSVAR                                           
016900             MOVE IXF TO IXF-DISP                                         
017000             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
017100             INTO BEFEL WITH POINTER EPTR                                 
017200           END-IF                                                         
017300           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
017400           ADD 2 TO IXF                                                   
017500         END-IF                                                           
017600       END-IF                                                             
017700                                                                          
017800     END-PERFORM                                                          
017900                                                                          
018000     IF FLUTF8 = JA OR YES                                                
018100       MOVE IXT      TO UCS2-LENG                                         
018200       MOVE KVMAXTL  TO UTF8-LENG                                         
018300*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
018400*      -- ANY TRUNCATION IS NOT DETECTED                                  
018500       MOVE SPACE     TO TECONV-TO                                        
018600       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
018700                           TECONV-TO   UTF8-LENG                          
018800     ELSE                                                                 
018900*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
019000*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
019100       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
019200     END-IF                                                               
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 A-INIT SECTION.                                                          
019900     SKIP2                                                                
020000*    -- SO FAR ALL IS OK                                                  
020100     MOVE SPACE TO KDSVAR, BEFEL                                          
020200     MOVE 1 TO EPTR                                                       
020300                                                                          
020400*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
020500     MOVE ZERO TO FLENG                                                   
020600     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
020700       TALLYING FLENG FOR LEADING SPACE                                   
020800     COMPUTE FLENG = TECONV-LENG - FLENG                                  
020900                                                                          
021000*    -- SET FLAG INDICATING SINGLE BYTE MODE IN INPUT TEXT                
021100     SET SB-MODE TO TRUE                                                  
021200                                                                          
021300*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
021400     MOVE ZERO  TO IXT                                                    
021500     MOVE SPACE TO TECONV-TEMP                                            
021600     .                                                                    
021700     EJECT                                                                
021800 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
021900     SKIP2                                                                
022000*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
022100*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
022200*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
022300*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
022400                                                                          
022500*    -- SO FAR ALL IS OK                                                  
022600     MOVE SPACE TO WKDSVAR                                                
022700                                                                          
022800     IF FLTXTENT = YES OR JA                                              
022900*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
023000*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
023100*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
023200       IF TEMP-2BYTE-X <= X'007F'                                         
023300*        -- IT IS A SIMPLE ASCII CHARACTER.                               
023400*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
023500         ADD 1 TO IXT                                                     
023600*        MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
023700         MOVE TEMP-1BYTE-X TO TECONV-TEMP (IXT:1)                         
023800         INSPECT TECONV-TEMP (IXT:1)                                      
023900         CONVERTING ASCII-CHARACTERS                                      
024000                 TO EBCDIC-CHARACTERS                                     
024100       ELSE                                                               
024200         IF IXT <= KVMAXTL - 8                                            
024300           MOVE 4 TO IXU                                                  
024400           MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                            
024500           PERFORM 4 TIMES                                                
024600             DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                
024700                    REMAINDER IXH                                         
024800             MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)              
024900             SUBTRACT 1 FROM IXU                                          
025000           END-PERFORM                                                    
025100           ADD 1 TO IXT                                                   
025200           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
025300           ADD 7 TO IXT                                                   
025400         ELSE                                                             
025500           MOVE 'T'  TO WKDSVAR                                           
025600         END-IF                                                           
025700       END-IF                                                             
025800                                                                          
025900     ELSE                                                                 
026000       IF FLUTF8 = JA OR YES  OR IXT <= KVMAXTL - 2                       
026100         ADD 1 TO IXT                                                     
026200         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
026300         ADD 1 TO IXT                                                     
026400       ELSE                                                               
026500         MOVE 'T'  TO WKDSVAR                                             
026600       END-IF                                                             
026700     END-IF                                                               
026800                                                                          
026900*    -- DON'T OVERLAY ANY F VALUE                                         
027000     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
027100       MOVE WKDSVAR TO KDSVAR                                             
027200       MOVE IXF TO IXF-DISP                                               
027300       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
027400       INTO BEFEL WITH POINTER EPTR                                       
027500     END-IF                                                               
027600     .                                                                    
