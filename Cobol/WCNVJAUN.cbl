000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVJAUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000         CONVERSION FROM JAPANSESE EBCDIC CODEPAGES                       
001100         (CP290/CS1172 AND CP300/CS1001 = CCSID 930) TO                   
001200         UNICODE. UCS2 OR UTF8 FORMAT CAN BE GENERATED                    
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                       PIC X(8)    VALUE 'WCNVJAUN'.            
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
008801*    -- CONVERSION TABLE FOR ASCII-TO-EBCDIC                              
008810 01  ASCII-CHARACTER-GROUP.                                               
008811     03  PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.             
008812     03  PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.             
008813     03  PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.             
008814     03  PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.             
008815     03  PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.             
008816     03  PIC X(16) VALUE X'707172737475767778797A7B7C7D7E7F'.             
008817 01  ASCII-CHARACTERS REDEFINES ASCII-CHARACTER-GROUP                     
008818         PIC X(96).                                                       
008830                                                                          
008831 01  EBCDIC-CHARACTER-GROUP.                                              
008832     03  PIC X(16) VALUE ' !"#$%& ()*+,-./'.                              
008833     03  PIC X(16) VALUE '0123456789:;<=>?'.                              
008834     03  PIC X(16) VALUE '@ABCDEFGHIJKLMNO'.                              
008835     03  PIC X(16) VALUE 'PQRSTUVWXYZ[\]^_'.                              
008836     03  PIC X(16) VALUE '`abcdefghijklmno'.                              
008837     03  PIC X(16) VALUE 'pqrstuvwxyz{|}~ '.                              
008838 01  EBCDIC-CHARACTERS REDEFINES EBCDIC-CHARACTER-GROUP                   
008839         PIC X(96).                                                       
008840                                                                          
008850                                                                          
008900*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
009000 01  UTF8-LENG                   PIC S9(9)   COMP.                        
009100 01  UCS2-LENG                   PIC S9(9)   COMP.                        
009200                                                                          
009300*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
009400 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
009500                                                                          
009600     EJECT                                                                
009700*CONTROL NOSOURCE                                                         
009800*01  -COPY WCNV290U                                                       
009900     EJECT                                                                
010000*01  -COPY WCNV300U                                                       
010100*CONTROL SOURCE                                                           
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010500 01  -COPY WCNVAREA                                                       
010600     EJECT                                                                
010700 PROCEDURE DIVISION USING WCNVAREA.                                       
010800 MAIN SECTION.                                                            
010900     SKIP2                                                                
011000     PERFORM A-INIT                                                       
011100                                                                          
011200     MOVE 1 TO IXF                                                        
011300     PERFORM UNTIL IXF > FLENG                                            
011400                                                                          
011500       IF SB-MODE                                                         
011600         IF TECONV-FROM (IXF:1) = SHIFT-OUT                               
011700*          -- SKIP THE CHARACTER AND SHIFT TO DB-MODE                     
011800           SET DB-MODE TO TRUE                                            
011900           ADD 1 TO IXF                                                   
012000         ELSE                                                             
012100*          -- CONVERT FROM SINGLEBYTE CODE (CP290)                        
012200           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
012300           ADD  WNUM  1             GIVING IX0                            
012400           MOVE CP290-TO-UCS-X (IX0)   TO TEMP-2BYTE-X                    
012500           IF TEMP-2BYTE-X = X'001A'                                      
012600             MOVE 'F' TO KDSVAR                                           
012700             MOVE IXF TO IXF-DISP                                         
012800             MOVE LOW-VALUE TO TEMP-1BYTE-FIRST                           
012900             MOVE TECONV-FROM (IXF:1) TO TEMP-1BYTE-X                     
013000             PERFORM S01-HEX-TO-CHAR                                      
013100             STRING 'POS ' IXF-DISP ' X'                                  
013200                    TEMP-HEXNR (3:2) '. ' DELIMITED BY SIZE               
013300             INTO BEFEL WITH POINTER EPTR                                 
013400             MOVE X'001A' TO TEMP-2BYTE-X                                 
013500           END-IF                                                         
013600           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
013700           ADD 1 TO IXF                                                   
013800         END-IF                                                           
013900       ELSE                                                               
014000         IF TECONV-FROM (IXF:1) = SHIFT-IN                                
014100*          -- SKIP THE CHARACTER AND SHIFT TO SB-MODE                     
014200           SET SB-MODE TO TRUE                                            
014300           ADD 1 TO IXF                                                   
014400         ELSE                                                             
014500*        -- CONVERT FROM DOUBLEBYTE CODE (CP300)                          
014600           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
014700           ADD  WNUM  1             GIVING IX0                            
014800           MOVE TAB2-IND-300U (IX0)     TO IX1                            
014900           MOVE TECONV-FROM (IXF + 1:1) TO WBYTE                          
015000           ADD  WNUM  1             GIVING IX2                            
015100           MOVE CP300-TO-UCS-X (IX1, IX2) TO TEMP-2BYTE-X                 
015200           IF TEMP-2BYTE-X = X'FFFD'                                      
015300           AND TECONV-FROM (IXF:2) NOT = X'FEFE'                          
015400*            -- FEFE ÄR DUBBELBYTE "SUB" OCH SKA BLI FFFD                 
015500*            -- DENNA ÄR ALLTSÅ OK MEDAN ANDRA SKA GE FELMED              
015600             MOVE 'F' TO KDSVAR                                           
015700             MOVE IXF TO IXF-DISP                                         
015800             MOVE TECONV-FROM (IXF:2) TO TEMP-2BYTE-X                     
015900             PERFORM S01-HEX-TO-CHAR                                      
016000             STRING 'POS ' IXF-DISP ' X'                                  
016100                    TEMP-HEXNR '. '  DELIMITED BY SIZE                    
016200             INTO BEFEL WITH POINTER EPTR                                 
016300             MOVE X'FFFD' TO TEMP-2BYTE-X                                 
016400           END-IF                                                         
016500           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
016600           ADD 2 TO IXF                                                   
016700         END-IF                                                           
016800       END-IF                                                             
016900                                                                          
017000     END-PERFORM                                                          
017100                                                                          
017200     IF FLUTF8 = JA OR YES                                                
017300       MOVE IXT      TO UCS2-LENG                                         
017400       MOVE KVMAXTL  TO UTF8-LENG                                         
017500*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
017600*      -- ANY TRUNCATION IS NOT DETECTED                                  
017700*      MOVE LOW-VALUE TO TECONV-TO                                        
017710       MOVE SPACE     TO TECONV-TO                                        
017800       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
017900                           TECONV-TO   UTF8-LENG                          
018200     ELSE                                                                 
018300*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
018400*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
018500       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
018600     END-IF                                                               
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019300     SKIP2                                                                
019400*    -- SO FAR ALL IS OK                                                  
019500     MOVE SPACE TO KDSVAR, BEFEL                                          
019600     MOVE 1 TO EPTR                                                       
019700                                                                          
019800*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
019900     MOVE ZERO TO FLENG                                                   
020000     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
020100       TALLYING FLENG FOR LEADING SPACE                                   
020200     COMPUTE FLENG = TECONV-LENG - FLENG                                  
020300                                                                          
020400*    -- SET FLAG INDICATING SINGLE BYTE MODE IN INPUT TEXT                
020500     SET SB-MODE TO TRUE                                                  
020600                                                                          
020700*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
020800     MOVE ZERO  TO IXT                                                    
020900     MOVE SPACE TO TECONV-TEMP                                            
021000     .                                                                    
021100     EJECT                                                                
021200 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
021300     SKIP2                                                                
021400*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
021500*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
021600*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
021700*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
021800                                                                          
021900*    -- SO FAR ALL IS OK                                                  
022000     MOVE SPACE TO WKDSVAR                                                
022100                                                                          
022200     IF FLTXTENT = YES OR JA                                              
022300*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
022400*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
022500*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
022600       IF TEMP-2BYTE-X <= X'007F'                                         
022700*        -- IT IS A SIMPLE ASCII CHARACTER.                               
022800*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
022900         ADD 1 TO IXT                                                     
023000*        MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
023010         MOVE TEMP-1BYTE-X TO TECONV-TEMP (IXT:1)                         
023020         INSPECT TECONV-TEMP (IXT:1)                                      
023030         CONVERTING ASCII-CHARACTERS                                      
023040                 TO EBCDIC-CHARACTERS                                     
023100       ELSE                                                               
023200         IF IXT <= KVMAXTL - 8                                            
023300           PERFORM S01-HEX-TO-CHAR                                        
023400           ADD 1 TO IXT                                                   
023500           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
023600           ADD 7 TO IXT                                                   
023700         ELSE                                                             
023800           MOVE 'T'  TO WKDSVAR                                           
023900         END-IF                                                           
024000       END-IF                                                             
024100                                                                          
024200     ELSE                                                                 
024300       IF FLUTF8 = JA OR YES  OR IXT <= KVMAXTL - 2                       
024400         ADD 1 TO IXT                                                     
024500         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
024600         ADD 1 TO IXT                                                     
024700       ELSE                                                               
024800         MOVE 'T'  TO WKDSVAR                                             
024900       END-IF                                                             
025000     END-IF                                                               
025100                                                                          
025200*    -- DON'T OVERLAY ANY F VALUE                                         
025300     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
025400       MOVE WKDSVAR TO KDSVAR                                             
025500       MOVE IXF TO IXF-DISP                                               
025600       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
025700       INTO BEFEL WITH POINTER EPTR                                       
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 S01-HEX-TO-CHAR  SECTION.                                                
026200     SKIP2                                                                
026300     MOVE 4 TO IXU                                                        
026400     MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                                  
026500     PERFORM 4 TIMES                                                      
026600       DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                      
026700              REMAINDER IXH                                               
026800       MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)                    
026900       SUBTRACT 1 FROM IXU                                                
027000     END-PERFORM                                                          
027100     .                                                                    
