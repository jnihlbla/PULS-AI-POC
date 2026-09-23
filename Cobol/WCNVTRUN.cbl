000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVTRUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/06/13.  (FREDAGEN 13:E!)                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000         CONVERSION FROM TURKISH EBCDIC CODEPAGE                          
001100         (CP1026/CS1152) TO UNICODE.                                      
001200         UCS2 OR UTF8 FORMAT CAN BE GENERATED.                            
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                       PIC X(8)    VALUE 'WCNVTRUN'.            
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
004300*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
004400 01  TEMP-4BYTE-X.                                                        
004500     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
004600     03 TEMP-2BYTE-X.                                                     
004700         05 TEMP-1BYTE-FIRST     PIC X.                                   
004800         05 TEMP-1BYTE-X         PIC X.                                   
004900     03 TEMP-2BYTE-N  REDEFINES TEMP-2BYTE-X                              
005000                                 PIC S9(4)   COMP.                        
005100 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
005200                                 PIC S9(9)   COMP.                        
005300                                                                          
005400*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
005500 01  HEX-CHAR                    PIC X(16)  VALUE                         
005600                                 '0123456789ABCDEF'.                      
005700 77  IXH                         PIC S9(4)   COMP.                        
005800 77  IXU                         PIC S9(4)   COMP.                        
005900                                                                          
006000*    -- WORK FIELDS FOR CREATING SGML HEX CHAR ENTITIES                   
006100*    -- MAY BE USED FOR CHARACTERS > FF                                   
006200 01  TEMP-HEX-ENT.                                                        
006300     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
006400     03  TEMP-HEXNR              PIC X(4).                                
006500     03  FILLER                  PIC X(1)   VALUE ';'.                    
006600                                                                          
006700*    -- WORK FIELDS FOR CREATING SGML DECIMAL CHAR ENTITIES               
006800*    -- MAY BE USED FOR LATIN 1 SUPLEMENT CHARACTERS (0A-FF)              
006900 01  TEMP-LAT-ENT.                                                        
007000     03  FILLER                  PIC X(2)   VALUE '&#'.                   
007100     03  TEMP-LATNR              PIC 9(3).                                
007200     03  FILLER                  PIC X(1)   VALUE ';'.                    
007300     EJECT                                                                
007400*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
007500 77  WKDSVAR                     PIC X.                                   
007600                                                                          
007700*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
007800 77  EPTR                        PIC S9(4)  COMP.                         
007900*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
008000 77  IXF-DISP                    PIC ZZ9.                                 
008100                                                                          
008200*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
008300 01  TECONV-TEMP                 PIC X(3000).                             
008400                                                                          
008500*    -- CONVERSION TABLE FOR ASCII-TO-EBCDIC                              
008600 01  ASCII-CHARACTER-GROUP.                                               
008700     03  PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.             
008800     03  PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.             
008900     03  PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.             
009000     03  PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.             
009100     03  PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.             
009200     03  PIC X(16) VALUE X'707172737475767778797A7B7C7D7E7F'.             
009300 01  ASCII-CHARACTERS REDEFINES ASCII-CHARACTER-GROUP                     
009400         PIC X(96).                                                       
009500                                                                          
009600 01  EBCDIC-CHARACTER-GROUP.                                              
009700     03  PIC X(16) VALUE ' !"#$%& ()*+,-./'.                              
009800     03  PIC X(16) VALUE '0123456789:;<=>?'.                              
009900     03  PIC X(16) VALUE '@ABCDEFGHIJKLMNO'.                              
010000     03  PIC X(16) VALUE 'PQRSTUVWXYZ[\]^_'.                              
010100     03  PIC X(16) VALUE '`abcdefghijklmno'.                              
010200     03  PIC X(16) VALUE 'pqrstuvwxyz{|}~ '.                              
010300 01  EBCDIC-CHARACTERS REDEFINES EBCDIC-CHARACTER-GROUP                   
010400         PIC X(96).                                                       
010500                                                                          
010600*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
010700 01  UTF8-LENG                   PIC S9(9)   COMP.                        
010800 01  UCS2-LENG                   PIC S9(9)   COMP.                        
010900                                                                          
011000*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
011100 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
011200                                                                          
011300     EJECT                                                                
011400*CONTROL NOSOURCE                                                         
011500*01  -COPY WCNVA26U                                                       
011600*CONTROL SOURCE                                                           
011700     EJECT                                                                
011800 LINKAGE SECTION.                                                         
011900                                                                          
012000 01  -COPY WCNVAREA                                                       
012100     EJECT                                                                
012200 PROCEDURE DIVISION USING WCNVAREA.                                       
012300 MAIN SECTION.                                                            
012400     SKIP2                                                                
012500     PERFORM A-INIT                                                       
012600                                                                          
012700     MOVE 1 TO IXF                                                        
012800     PERFORM UNTIL IXF > FLENG                                            
012900                                                                          
013000*      -- CONVERT FROM TURKISH EBCDIC CODE (CP1026)                       
013100       MOVE TECONV-FROM (IXF:1)     TO WBYTE                              
013200       ADD  WNUM  1             GIVING IX0                                
013300       MOVE CP1026-TO-UCS-X (IX0)    TO TEMP-2BYTE-X                      
013400       IF TEMP-2BYTE-X = X'001A'                                          
013500         MOVE 'F' TO KDSVAR                                               
013600         MOVE IXF TO IXF-DISP                                             
013700         STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                    
013800         INTO BEFEL WITH POINTER EPTR                                     
013900       END-IF                                                             
014000       PERFORM B-MOVE-TO-OUTPUT-FIELD                                     
014100       ADD 1 TO IXF                                                       
014200                                                                          
014300     END-PERFORM                                                          
014400                                                                          
014500     IF FLUTF8 = JA OR YES                                                
014600       MOVE IXT      TO UCS2-LENG                                         
014700       MOVE KVMAXTL  TO UTF8-LENG                                         
014800*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
014900*      -- ANY TRUNCATION IS NOT DETECTED                                  
015000       MOVE SPACE     TO TECONV-TO                                        
015100       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
015200                           TECONV-TO   UTF8-LENG                          
015300     ELSE                                                                 
015400*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
015500*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
015600       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
015700     END-IF                                                               
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     SKIP2                                                                
016500*    -- SO FAR ALL IS OK                                                  
016600     MOVE SPACE TO KDSVAR, BEFEL                                          
016700     MOVE 1 TO EPTR                                                       
016800                                                                          
016900*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
017000     MOVE ZERO TO FLENG                                                   
017100     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
017200       TALLYING FLENG FOR LEADING SPACE                                   
017300     COMPUTE FLENG = TECONV-LENG - FLENG                                  
017400                                                                          
017500*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
017600     MOVE ZERO  TO IXT                                                    
017700     MOVE SPACE TO TECONV-TEMP                                            
017800     .                                                                    
017900     EJECT                                                                
018000 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
018100     SKIP2                                                                
018200*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
018300*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
018400*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
018500*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
018600                                                                          
018700*    -- SO FAR ALL IS OK                                                  
018800     MOVE SPACE TO WKDSVAR                                                
018900                                                                          
019000     IF FLTXTENT = YES OR JA                                              
019100*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
019200*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
019300*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
019400       IF TEMP-2BYTE-X <= X'007F'                                         
019500*        -- IT IS A SIMPLE ASCII CHARACTER.                               
019600*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
019700         ADD 1 TO IXT                                                     
019800*        MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
019900         MOVE TEMP-1BYTE-X TO TECONV-TEMP (IXT:1)                         
020000         INSPECT TECONV-TEMP (IXT:1)                                      
020100         CONVERTING ASCII-CHARACTERS                                      
020200                 TO EBCDIC-CHARACTERS                                     
020300       ELSE                                                               
020400         IF IXT <= KVMAXTL - 8                                            
020500           MOVE 4 TO IXU                                                  
020600           MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                            
020700           PERFORM 4 TIMES                                                
020800             DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                
020900                    REMAINDER IXH                                         
021000             MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)              
021100             SUBTRACT 1 FROM IXU                                          
021200           END-PERFORM                                                    
021300           ADD 1 TO IXT                                                   
021400           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
021500           ADD 7 TO IXT                                                   
021600         ELSE                                                             
021700           MOVE 'T'  TO WKDSVAR                                           
021800         END-IF                                                           
021900       END-IF                                                             
022000                                                                          
022100     ELSE                                                                 
022200       IF FLUTF8 = JA OR YES OR IXT <= KVMAXTL - 2                        
022300         ADD 1 TO IXT                                                     
022400         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
022500         ADD 1 TO IXT                                                     
022600       ELSE                                                               
022700         MOVE 'T'  TO WKDSVAR                                             
022800       END-IF                                                             
022900     END-IF                                                               
023000                                                                          
023100*    -- DON'T OVERLAY ANY F VALUE                                         
023200     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
023300       MOVE WKDSVAR TO KDSVAR                                             
023400       MOVE IXF TO IXF-DISP                                               
023500       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
023600       INTO BEFEL WITH POINTER EPTR                                       
023700     END-IF                                                               
023800     .                                                                    
