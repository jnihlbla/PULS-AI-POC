000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVSVUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   99/09/09.  (FARLIGT DATUM!)                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000         CONVERSION FROM SWEDISH EBCDIC CODEPAGE                          
001100         (CP278/CS697) TO UNICODE.                                        
001200         UCS2 OR UTF8 FORMAT CAN BE GENERATED.                            
001300                                                                          
001400     SKIP3                                                                
001500 DATA DIVISION.                                                           
001600     SKIP3                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001810*    -- CHECKED BY WY2000                                                 
001900 77  IDPGM                       PIC X(8)    VALUE 'WCNVSVUN'.            
002000 77  YES                         PIC X       VALUE 'Y'.                   
002100 77  JA                          PIC X       VALUE 'J'.                   
002200 77  NOO                         PIC X       VALUE 'N'.                   
002500*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
002600 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
002700 77  FLENG                       PIC S9(4)   COMP.                        
002800                                                                          
002900*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
003000 77  IXF                         PIC S9(4)   COMP.                        
003100 77  IXT                         PIC S9(4)   COMP.                        
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
004900*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
004910 01  TEMP-4BYTE-X.                                                        
004920     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005000     03 TEMP-2BYTE-X.                                                     
005100         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
005101 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
005102                                 PIC S9(9)   COMP.                        
005110                                                                          
005120*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
005130 01  HEX-CHAR                    PIC X(16)  VALUE                         
005140                                 '0123456789ABCDEF'.                      
005150 77  IXH                         PIC S9(4)   COMP.                        
005160 77  IXU                         PIC S9(4)   COMP.                        
005200                                                                          
005210*    -- WORK FIELDS FOR CREATING SGML HEX CHAR ENTITIES                   
005220*    -- MAY BE USED FOR CHARACTERS > FF                                   
005300 01  TEMP-HEX-ENT.                                                        
005400     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
005500     03  TEMP-HEXNR              PIC X(4).                                
005600     03  FILLER                  PIC X(1)   VALUE ';'.                    
005700                                                                          
005710*    -- WORK FIELDS FOR CREATING SGML DECIMAL CHAR ENTITIES               
005720*    -- MAY BE USED FOR LATIN 1 SUPLEMENT CHARACTERS (0A-FF)              
005800 01  TEMP-LAT-ENT.                                                        
005900     03  FILLER                  PIC X(2)   VALUE '&#'.                   
006000     03  TEMP-LATNR              PIC 9(3).                                
006100     03  FILLER                  PIC X(1)   VALUE ';'.                    
006200     EJECT                                                                
006300*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
006400 77  WKDSVAR                     PIC X.                                   
006500                                                                          
006600*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
006700 77  EPTR                        PIC S9(4)  COMP.                         
006800*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
006810 77  IXF-DISP                    PIC ZZ9.                                 
006820                                                                          
006830*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
006840 01  TECONV-TEMP                 PIC X(3000).                             
006850                                                                          
006860*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
006870 01  UTF8-LENG                   PIC S9(9)   COMP.                        
006880 01  UCS2-LENG                   PIC S9(9)   COMP.                        
006890                                                                          
006891*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
006892 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
006893                                                                          
006900     EJECT                                                                
007000*CONTROL NOSOURCE                                                         
007100*01  -COPY WCNV278U                                                       
007400*CONTROL SOURCE                                                           
007500     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800 01  -COPY WCNVAREA                                                       
007900     EJECT                                                                
008000 PROCEDURE DIVISION USING WCNVAREA.                                       
008100 MAIN SECTION.                                                            
008200     SKIP2                                                                
008300     PERFORM A-INIT                                                       
008400                                                                          
008500     MOVE 1 TO IXF                                                        
008600     PERFORM UNTIL IXF > FLENG                                            
008700                                                                          
009400*      -- CONVERT FROM SWEDISH EBCDIC CODE (CP278)                        
009500       MOVE TECONV-FROM (IXF:1)     TO WBYTE                              
009600       ADD  WNUM  1             GIVING IX0                                
009700       MOVE CP278-TO-UCS-X (IX0)    TO TEMP-2BYTE-X                       
009900       IF TEMP-2BYTE-X = X'001A'                                          
010000         MOVE 'F' TO KDSVAR                                               
010010         MOVE IXF TO IXF-DISP                                             
010020         STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                    
010030         INTO BEFEL WITH POINTER EPTR                                     
010100       END-IF                                                             
010200       PERFORM B-MOVE-TO-OUTPUT-FIELD                                     
010210       ADD 1 TO IXF                                                       
012400                                                                          
012500     END-PERFORM                                                          
012600                                                                          
012700     IF FLUTF8 = JA OR YES                                                
012800       MOVE IXT      TO UCS2-LENG                                         
012810       MOVE KVMAXTL  TO UTF8-LENG                                         
012820*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
012830*      -- ANY TRUNCATION IS NOT DETECTED                                  
012840       MOVE SPACE     TO TECONV-TO                                        
012861       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
012862                           TECONV-TO   UTF8-LENG                          
012870     ELSE                                                                 
012880*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
012890*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
012891       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
012892     END-IF                                                               
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500     SKIP2                                                                
013600*    -- SO FAR ALL IS OK                                                  
013710     MOVE SPACE TO KDSVAR, BEFEL                                          
013720     MOVE 1 TO EPTR                                                       
013800                                                                          
013900*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
014000     MOVE ZERO TO FLENG                                                   
014100     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
014200       TALLYING FLENG FOR LEADING SPACE                                   
014300     COMPUTE FLENG = TECONV-LENG - FLENG                                  
014400                                                                          
014800*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
014900     MOVE ZERO  TO IXT                                                    
015000     MOVE SPACE TO TECONV-TEMP                                            
015100     .                                                                    
015200     EJECT                                                                
015300 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
015400     SKIP2                                                                
015500*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
015600*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
015610*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
015620*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
015700                                                                          
015800*    -- SO FAR ALL IS OK                                                  
015900     MOVE SPACE TO WKDSVAR                                                
016000                                                                          
016100     IF FLTXTENT = YES OR JA                                              
016110*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
016120*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
016130*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
016200       IF TEMP-2BYTE-X <= X'007F'                                         
016300*        -- IT IS A SIMPLE ASCII CHARACTER.                               
016400*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
016410         ADD 1 TO IXT                                                     
016500         MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
016600       ELSE                                                               
016700         IF IXT <= KVMAXTL - 8                                            
016701           MOVE 4 TO IXU                                                  
016702           MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                            
016710           PERFORM 4 TIMES                                                
016720             DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                
016721                    REMAINDER IXH                                         
016722             MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)              
016723             SUBTRACT 1 FROM IXU                                          
016730           END-PERFORM                                                    
016740           ADD 1 TO IXT                                                   
016900           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
017000           ADD 7 TO IXT                                                   
017100         ELSE                                                             
017200           MOVE 'T'  TO WKDSVAR                                           
017300         END-IF                                                           
017400       END-IF                                                             
017410                                                                          
017500     ELSE                                                                 
017600       IF FLUTF8 = JA OR YES OR IXT <= KVMAXTL - 2                        
017700         ADD 1 TO IXT                                                     
017800         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
017810         ADD 1 TO IXT                                                     
017900       ELSE                                                               
018000         MOVE 'T'  TO WKDSVAR                                             
018100       END-IF                                                             
018200     END-IF                                                               
018300                                                                          
018900*    -- DON'T OVERLAY ANY F VALUE                                         
019000     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
019100       MOVE WKDSVAR TO KDSVAR                                             
019110       MOVE IXF TO IXF-DISP                                               
019120       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
019130       INTO BEFEL WITH POINTER EPTR                                       
019200     END-IF                                                               
019300     .                                                                    
