000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVTHUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/06/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM THAI EBCDIC CODEPAGES (CP838/CS1279              
001100*        = CCSID 9030 AND CP839/CS929 = CCSID 839) TO                     
001200*        UNICODE. UCS2 OR UTF8 FORMAT CAN BE GENERATED.                   
001300*     NOTE: NO CONVERSION TABLE FOR CP839 EXISTS.                         
001400*        ACCORDING TO IBM CP839 "IS USED FOR PRESENTATION                 
001500*        PURPOSE ONLY" AND IS NOT SUPPORTED BY ANY IBM                    
001600*        PRODUCT AT THE MOMENT. THE CODE TO HANDLE CP839                  
001700*        IS THEREFOR COMMENTED IN THE PROGRAM.                            
001800*                                                                         
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100     SKIP3                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(8)    VALUE 'WCNVTHUN'.            
002600 77  YES                         PIC X       VALUE 'Y'.                   
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  NOO                         PIC X       VALUE 'N'.                   
002900*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
003000 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
003100 77  FLENG                       PIC S9(4)   COMP.                        
003200                                                                          
003300*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
003400 77  IXF                         PIC S9(4)   COMP.                        
003500 77  IXT                         PIC S9(4)   COMP.                        
003600                                                                          
003700*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003800 01  WNUM-GRP.                                                            
003900     03 FILLER                   PIC X       VALUE LOW-VALUE.             
004000     03 WBYTE                    PIC X.                                   
004100 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
004200                                                                          
004300*    -- INDICES TO CONVERSION TABLES                                      
004400 77  IX0                         PIC S9(4)   COMP.                        
004500 77  IX1                         PIC S9(4)   COMP.                        
004600 77  IX2                         PIC S9(4)   COMP.                        
004700                                                                          
004800*    -- FLAG INDICATING STATUS OF MIXED-MODE DBCS TEXT                    
004900 77  MODE-SW                     PIC X.                                   
005000 88  SB-MODE                     VALUE 'S'.                               
005100 88  DB-MODE                     VALUE 'D'.                               
005200                                                                          
005300*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
005400 01  TEMP-4BYTE-X.                                                        
005500     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005600     03 TEMP-2BYTE-X.                                                     
005700         05 TEMP-1BYTE-FIRST     PIC X.                                   
005800         05 TEMP-1BYTE-X         PIC X.                                   
005900     03 TEMP-2BYTE-N  REDEFINES TEMP-2BYTE-X                              
006000                                 PIC S9(4)   COMP.                        
006100 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
006200                                 PIC S9(9)   COMP.                        
006300                                                                          
006400*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
006500 01  HEX-CHAR                    PIC X(16)  VALUE                         
006600                                 '0123456789ABCDEF'.                      
006700 77  IXH                         PIC S9(4)   COMP.                        
006800 77  IXU                         PIC S9(4)   COMP.                        
006900                                                                          
007000*    -- WORK FIELDS FOR CREATING SGML HEX CHAR ENTITIES                   
007100*    -- MAY BE USED FOR CHARACTERS > FF                                   
007200 01  TEMP-HEX-ENT.                                                        
007300     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
007400     03  TEMP-HEXNR              PIC X(4).                                
007500     03  FILLER                  PIC X(1)   VALUE ';'.                    
007600                                                                          
007700*    -- WORK FIELDS FOR CREATING SGML DECIMAL CHAR ENTITIES               
007800*    -- MAY BE USED FOR LATIN 1 SUPLEMENT CHARACTERS (0A-FF)              
007900 01  TEMP-LAT-ENT.                                                        
008000     03  FILLER                  PIC X(2)   VALUE '&#'.                   
008100     03  TEMP-LATNR              PIC 9(3).                                
008200     03  FILLER                  PIC X(1)   VALUE ';'.                    
008300     EJECT                                                                
008400*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
008500 77  WKDSVAR                     PIC X.                                   
008600*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
008700 77  EPTR                        PIC S9(4)  COMP.                         
008800*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
008900 77  IXF-DISP                    PIC ZZ9.                                 
009000                                                                          
009100*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
009200 01  TECONV-TEMP                 PIC X(3000).                             
009300                                                                          
009400*    -- CONVERSION TABLE FOR ASCII-TO-EBCDIC                              
009500 01  ASCII-CHARACTER-GROUP.                                               
009600     03  PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.             
009700     03  PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.             
009800     03  PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.             
009900     03  PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.             
010000     03  PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.             
010100     03  PIC X(16) VALUE X'707172737475767778797A7B7C7D7E7F'.             
010200 01  ASCII-CHARACTERS REDEFINES ASCII-CHARACTER-GROUP                     
010300         PIC X(96).                                                       
010400                                                                          
010500 01  EBCDIC-CHARACTER-GROUP.                                              
010600     03  PIC X(16) VALUE ' !"#$%& ()*+,-./'.                              
010700     03  PIC X(16) VALUE '0123456789:;<=>?'.                              
010800     03  PIC X(16) VALUE '@ABCDEFGHIJKLMNO'.                              
010900     03  PIC X(16) VALUE 'PQRSTUVWXYZ[\]^_'.                              
011000     03  PIC X(16) VALUE '`abcdefghijklmno'.                              
011100     03  PIC X(16) VALUE 'pqrstuvwxyz{|}~ '.                              
011200 01  EBCDIC-CHARACTERS REDEFINES EBCDIC-CHARACTER-GROUP                   
011300         PIC X(96).                                                       
011400                                                                          
011500*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
011600 01  UTF8-LENG                   PIC S9(9)   COMP.                        
011700 01  UCS2-LENG                   PIC S9(9)   COMP.                        
011800                                                                          
011900*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
012000 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
012100                                                                          
012200     EJECT                                                                
012300*CONTROL NOSOURCE                                                         
012400*01  -COPY WCNV838U                                                       
012500     EJECT                                                                
012600* FIX -------------                                                       
012700*01  -KOPY WCNV839U                                                       
012800* END-FIX ---------                                                       
012900*CONTROL SOURCE                                                           
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300 01  -COPY WCNVAREA                                                       
013400     EJECT                                                                
013500 PROCEDURE DIVISION USING WCNVAREA.                                       
013600 MAIN SECTION.                                                            
013700     SKIP2                                                                
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     MOVE 1 TO IXF                                                        
014100     PERFORM UNTIL IXF > FLENG                                            
014200                                                                          
014300       IF SB-MODE                                                         
014400         IF TECONV-FROM (IXF:1) = SHIFT-OUT                               
014500*          -- SKIP THE CHARACTER AND SHIFT TO DB-MODE                     
014600           SET DB-MODE TO TRUE                                            
014700           ADD 1 TO IXF                                                   
014800         ELSE                                                             
014900*          -- CONVERT FROM SINGLEBYTE CODE (CP838)                        
015000           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
015100           ADD  WNUM  1             GIVING IX0                            
015200           MOVE CP838-TO-UCS-X (IX0)   TO TEMP-2BYTE-X                    
015300           IF TEMP-2BYTE-X = X'001A'                                      
015400             MOVE 'F' TO KDSVAR                                           
015500             MOVE IXF TO IXF-DISP                                         
015600             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
015700             INTO BEFEL WITH POINTER EPTR                                 
015800           END-IF                                                         
015900           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
016000           ADD 1 TO IXF                                                   
016100         END-IF                                                           
016200       ELSE                                                               
016300         IF TECONV-FROM (IXF:1) = SHIFT-IN                                
016400*          -- SKIP THE CHARACTER AND SHIFT TO SB-MODE                     
016500           SET SB-MODE TO TRUE                                            
016600           ADD 1 TO IXF                                                   
016700         ELSE                                                             
016800*        -- CONVERT FROM DOUBLEBYTE CODE (CP839)                          
016900* FIX ------                                                              
017000*          MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
017100*          ADD  WNUM  1             GIVING IX0                            
017200*          MOVE TAB2-IND-839U (IX0)     TO IX1                            
017300*          MOVE TECONV-FROM (IXF + 1:1) TO WBYTE                          
017400*          ADD  WNUM  1             GIVING IX2                            
017500*          MOVE CP839-TO-UCS-X (IX1, IX2) TO TEMP-2BYTE-X                 
017600*          IF TEMP-2BYTE-X = X'FFFD'                                      
017700             MOVE 'F' TO KDSVAR                                           
017800             MOVE IXF TO IXF-DISP                                         
017900             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
018000             INTO BEFEL WITH POINTER EPTR                                 
018100*          END-IF                                                         
018200           MOVE X'FFFD' TO TEMP-2BYTE-X                                   
018300* END-FIX -----                                                           
018400           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
018500           ADD 2 TO IXF                                                   
018600         END-IF                                                           
018700       END-IF                                                             
018800                                                                          
018900     END-PERFORM                                                          
019000                                                                          
019100     IF FLUTF8 = JA OR YES                                                
019200       MOVE IXT      TO UCS2-LENG                                         
019300       MOVE KVMAXTL  TO UTF8-LENG                                         
019400*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
019500*      -- ANY TRUNCATION IS NOT DETECTED                                  
019600       MOVE SPACE     TO TECONV-TO                                        
019700       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
019800                           TECONV-TO   UTF8-LENG                          
019900     ELSE                                                                 
020000*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
020100*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
020200       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
020300     END-IF                                                               
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000     SKIP2                                                                
021100*    -- SO FAR ALL IS OK                                                  
021200     MOVE SPACE TO KDSVAR, BEFEL                                          
021300     MOVE 1 TO EPTR                                                       
021400                                                                          
021500*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
021600     MOVE ZERO TO FLENG                                                   
021700     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
021800       TALLYING FLENG FOR LEADING SPACE                                   
021900     COMPUTE FLENG = TECONV-LENG - FLENG                                  
022000                                                                          
022100*    -- SET FLAG INDICATING SINGLE BYTE MODE IN INPUT TEXT                
022200     SET SB-MODE TO TRUE                                                  
022300                                                                          
022400*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
022500     MOVE ZERO  TO IXT                                                    
022600     MOVE SPACE TO TECONV-TEMP                                            
022700     .                                                                    
022800     EJECT                                                                
022900 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
023000     SKIP2                                                                
023100*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
023200*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
023300*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
023400*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
023500                                                                          
023600*    -- SO FAR ALL IS OK                                                  
023700     MOVE SPACE TO WKDSVAR                                                
023800                                                                          
023900     IF FLTXTENT = YES OR JA                                              
024000*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
024100*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
024200*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
024300       IF TEMP-2BYTE-X <= X'007F'                                         
024400*        -- IT IS A SIMPLE ASCII CHARACTER.                               
024500*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
024600         ADD 1 TO IXT                                                     
024700*        MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
024800         MOVE TEMP-1BYTE-X TO TECONV-TEMP (IXT:1)                         
024900         INSPECT TECONV-TEMP (IXT:1)                                      
025000         CONVERTING ASCII-CHARACTERS                                      
025100                 TO EBCDIC-CHARACTERS                                     
025200       ELSE                                                               
025300         IF IXT <= KVMAXTL - 8                                            
025400           MOVE 4 TO IXU                                                  
025500           MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                            
025600           PERFORM 4 TIMES                                                
025700             DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                
025800                    REMAINDER IXH                                         
025900             MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)              
026000             SUBTRACT 1 FROM IXU                                          
026100           END-PERFORM                                                    
026200           ADD 1 TO IXT                                                   
026300           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
026400           ADD 7 TO IXT                                                   
026500         ELSE                                                             
026600           MOVE 'T'  TO WKDSVAR                                           
026700         END-IF                                                           
026800       END-IF                                                             
026900                                                                          
027000     ELSE                                                                 
027100       IF FLUTF8 = JA OR YES OR IXT <= KVMAXTL - 2                        
027200         ADD 1 TO IXT                                                     
027300         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
027400         ADD 1 TO IXT                                                     
027500       ELSE                                                               
027600         MOVE 'T'  TO WKDSVAR                                             
027700       END-IF                                                             
027800     END-IF                                                               
027900                                                                          
028000*    -- DON'T OVERLAY ANY F VALUE                                         
028100     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
028200       MOVE WKDSVAR TO KDSVAR                                             
028300       MOVE IXF TO IXF-DISP                                               
028400       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
028500       INTO BEFEL WITH POINTER EPTR                                       
028600     END-IF                                                               
028700     .                                                                    
