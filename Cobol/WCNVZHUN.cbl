000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVZHUN.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/06/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CONVERSION FROM TRADITIONAL CHINESE EBCDIC CODEPAGES             
001100*        (CP37/CS697 AND CP835/CS935 = CCSID 937 (ALMOST)) TO             
001200*        UNICODE. ucs2 or utf8 format can be generated.                   
001210*        NOTE THAT THE FULL CP37/CS697 IS USED RATHER                     
001300*        THAN THE SUBSET CP37/CS1175 NORMALLY USED TOGETHER               
001400*        WITH THE CHINESE DBCS CHARACTERS.                                
001500*        TRADITIONAL CHINESE IS USED IN TAIWAN.                           
001600*                                                                         
001700*    TEMPORÄR FIX FÖR ATT KLARA KINESISKT TECKEN X'5185'                  
001800*    SOM INTE KLARAS AV IBM.S KONVERTERINGSTABELL WCNVU835                
001801*                                                                         
001810*    DÄRFÖR HAR I PROGRAM INLAGTS KOD SOM BYTER UT X'5185'                
001820*    TILL X'EEEE', VILKET ALLTSÅ MÅSTE BYTAS TILLBAKA TILL                
001830*    X'5185' I DETTA PROGRAM.  X'EEEE' FINNS EJ HELLER SOM GODK           
001840*    TECKEN I WCNV835U, VILKET GER X'FFFD' SOM RETUR TECKEN.              
001850*    EFTERSOM DETTA FEL RÄTTAS TILL, BACKAS FELTEXTAREAN                  
001860*    I MOTSVARANDE GRAD.                                                  
001900*                                                                         
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(8)    VALUE 'WCNVZHUN'.            
002700 77  YES                         PIC X       VALUE 'Y'.                   
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NOO                         PIC X       VALUE 'N'.                   
002910 77  X5185                       PIC XX      VALUE X'5185'.               
003000*    -- LENGTH OF TECONV FIELDS IN WCNVAREA                               
003100 77  TECONV-LENG                 PIC S9(4)   COMP VALUE 700.              
003200 77  FLENG                       PIC S9(4)   COMP.                        
003300                                                                          
003400*    -- POINTERS TO CHARACTERS IN TECONV FIELDS                           
003500 77  IXF                         PIC S9(4)   COMP.                        
003600 77  IXT                         PIC S9(4)   COMP.                        
003700                                                                          
003800*    -- WORK FIELDS TO CONVERT A BYTE TO A NUMBER                         
003900 01  WNUM-GRP.                                                            
004000     03 FILLER                   PIC X       VALUE LOW-VALUE.             
004100     03 WBYTE                    PIC X.                                   
004200 01  WNUM  REDEFINES WNUM-GRP    PIC S9(4)   COMP.                        
004300                                                                          
004400*    -- INDICES TO CONVERSION TABLES                                      
004500 77  IX0                         PIC S9(4)   COMP.                        
004600 77  IX1                         PIC S9(4)   COMP.                        
004700 77  IX2                         PIC S9(4)   COMP.                        
004800                                                                          
004900*    -- FLAG INDICATING STATUS OF MIXED-MODE DBCS TEXT                    
005000 77  MODE-SW                     PIC X.                                   
005100 88  SB-MODE                     VALUE 'S'.                               
005200 88  DB-MODE                     VALUE 'D'.                               
005300                                                                          
005400*    -- WORK FIELDS FOR TEMPORARY STORING CONVERTED BYTES                 
005500 01  TEMP-4BYTE-X.                                                        
005600     03 TEMP-4BYTE-FIRST2        PIC XX      VALUE LOW-VALUE.             
005700     03 TEMP-2BYTE-X.                                                     
005800         05 TEMP-2BYTE-N         PIC S9(4)   COMP.                        
005900 01  TEMP-4BYTE-N REDEFINES TEMP-4BYTE-X                                  
006000                                 PIC S9(9)   COMP.                        
006100                                                                          
006200*    -- TABLE USED WHEN CONVERTING A NUMERIC VALUE TO HEX                 
006300 01  HEX-CHAR                    PIC X(16)  VALUE                         
006400                                 '0123456789ABCDEF'.                      
006500 77  IXH                         PIC S9(4)   COMP.                        
006600 77  IXU                         PIC S9(4)   COMP.                        
006700                                                                          
006800*    -- WORK FIELDS FOR CREATING SGML HEX CHAR ENTITIES                   
006900*    -- MAY BE USED FOR CHARACTERS > FF                                   
007000 01  TEMP-HEX-ENT.                                                        
007100     03  FILLER                  PIC X(3)   VALUE '&#x'.                  
007200     03  TEMP-HEXNR              PIC X(4).                                
007300     03  FILLER                  PIC X(1)   VALUE ';'.                    
007400                                                                          
007500*    -- WORK FIELDS FOR CREATING SGML DECIMAL CHAR ENTITIES               
007600*    -- MAY BE USED FOR LATIN 1 SUPLEMENT CHARACTERS (0A-FF)              
007700 01  TEMP-LAT-ENT.                                                        
007800     03  FILLER                  PIC X(2)   VALUE '&#'.                   
007900     03  TEMP-LATNR              PIC 9(3).                                
008000     03  FILLER                  PIC X(1)   VALUE ';'.                    
008100     EJECT                                                                
008200*    -- TEMPORARY STORAGE OF KDSVAR VALUE.                                
008300 77  WKDSVAR                     PIC X.                                   
008400*    -- POINTER TO CURRENT POS IN ERROR TEXT FIELD (BEFEL)                
008500 77  EPTR                        PIC S9(4)  COMP.                         
008600*    -- POSITION IN INPUT TEXT CAUSING AN ERROR.                          
008700 77  IXF-DISP                    PIC ZZ9.                                 
008710                                                                          
008720*    -- TEMPORARY FROM-AREA WHEN CONVERTING                               
008730 01  TECONV-TEMP                 PIC X(3000).                             
008740                                                                          
008750*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
008760 01  UTF8-LENG                   PIC S9(9)   COMP.                        
008770 01  UCS2-LENG                   PIC S9(9)   COMP.                        
008780                                                                          
008790*    -- SUBROUTINE FOR CONVERTING FROM UCS2 TO UTF8                       
008791 01  WCNVUUTF                    PIC X(8)    VALUE 'WCNVUUTF'.            
008792                                                                          
008800     EJECT                                                                
008900*CONTROL NOSOURCE                                                         
009000*01  -COPY WCNV037U                                                       
009100     EJECT                                                                
009200*01  -COPY WCNV835U                                                       
009300*CONTROL SOURCE                                                           
009400     EJECT                                                                
009500 LINKAGE SECTION.                                                         
009600                                                                          
009700 01  -COPY WCNVAREA                                                       
009800     EJECT                                                                
009900 PROCEDURE DIVISION USING WCNVAREA.                                       
010000 MAIN SECTION.                                                            
010100     SKIP2                                                                
010200     PERFORM A-INIT                                                       
010300                                                                          
010400     MOVE 1 TO IXF                                                        
010500     PERFORM UNTIL IXF > FLENG                                            
010600                                                                          
010700       IF SB-MODE                                                         
010800         IF TECONV-FROM (IXF:1) = SHIFT-OUT                               
010900*          -- SKIP THE CHARACTER AND SHIFT TO DB-MODE                     
011000           SET DB-MODE TO TRUE                                            
011100           ADD 1 TO IXF                                                   
011200         ELSE                                                             
011300*          -- CONVERT FROM SINGLEBYTE CODE (CP37)                         
011400           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
011500           ADD  WNUM  1             GIVING IX0                            
011600           MOVE CP37-TO-UCS-X (IX0)     TO TEMP-2BYTE-X                   
011700           IF TEMP-2BYTE-X = X'001A'                                      
011800             MOVE 'F' TO KDSVAR                                           
011900             MOVE IXF TO IXF-DISP                                         
012000             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
012100             INTO BEFEL WITH POINTER EPTR                                 
012200           END-IF                                                         
012300           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
012400           ADD 1 TO IXF                                                   
012500         END-IF                                                           
012600       ELSE                                                               
012700         IF TECONV-FROM (IXF:1) = SHIFT-IN                                
012800*          -- SKIP THE CHARACTER AND SHIFT TO SB-MODE                     
012900           SET SB-MODE TO TRUE                                            
013000           ADD 1 TO IXF                                                   
013100         ELSE                                                             
013200*        -- CONVERT FROM DOUBLEBYTE CODE (CP835)                          
013300           MOVE TECONV-FROM (IXF:1)     TO WBYTE                          
013400           ADD  WNUM  1             GIVING IX0                            
013500           MOVE TAB2-IND-835U (IX0)     TO IX1                            
013600           MOVE TECONV-FROM (IXF + 1:1) TO WBYTE                          
013700           ADD  WNUM  1             GIVING IX2                            
013800           MOVE CP835-TO-UCS-X (IX1, IX2) TO TEMP-2BYTE-X                 
013900           IF TEMP-2BYTE-X = X'FFFD'                                      
014000             MOVE 'F' TO KDSVAR                                           
014100             MOVE IXF TO IXF-DISP                                         
014200             STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                
014300             INTO BEFEL WITH POINTER EPTR                                 
014301                                                                          
014302             IF TECONV-FROM (IXF:1) = X'EE'                               
014303*                                       MSB                               
014304             AND TECONV-FROM (IXF + 1:1) = X'EE'                          
014305*                                            LSB                          
014306*            --- FIX FÖR ATT KLARA TECKNET '5185'                         
014307*            --- SOM BETYDER "INRE" LAGRAS TILLF. SOM 'EEEE'              
014310               MOVE X5185 TO TEMP-2BYTE-X                                 
014311*              --- TAG BORT JUST DENNA FELRAPPORT FRÅN BEFEL              
014320               MOVE ' ' TO KDSVAR                                         
014330               SUBTRACT 9 FROM EPTR                                       
014340               MOVE SPACE TO BEFEL(EPTR: )                                
014400             END-IF                                                       
014410           END-IF                                                         
014500           PERFORM B-MOVE-TO-OUTPUT-FIELD                                 
014600           ADD 2 TO IXF                                                   
014700         END-IF                                                           
014800       END-IF                                                             
014900                                                                          
015000     END-PERFORM                                                          
015010                                                                          
015020     IF FLUTF8 = JA OR YES                                                
015030       MOVE IXT      TO UCS2-LENG                                         
015040       MOVE KVMAXTL  TO UTF8-LENG                                         
015050*      -- USE HEX 40 AS PADDING WHEN UTF8                                 
015060*      -- ANY TRUNCATION IS NOT DETECTED                                  
015070       MOVE SPACE     TO TECONV-TO                                        
015091       CALL WCNVUUTF USING TECONV-TEMP UCS2-LENG                          
015092                           TECONV-TO   UTF8-LENG                          
015093     ELSE                                                                 
015094*      -- USE HEX 4040 AS PADDING WHEN UCS2                               
015095*      -- TRUNCATION HAS ALREADY BEEN DETECTED IN B SECTION               
015096       MOVE TECONV-TEMP(1:IXT) TO TECONV-TO(1:KVMAXTL)                    
015097     END-IF                                                               
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700     SKIP2                                                                
015800*    -- SO FAR ALL IS OK                                                  
015900     MOVE SPACE TO KDSVAR, BEFEL                                          
016000     MOVE 1 TO EPTR                                                       
016100                                                                          
016200*    -- COMPUTE LENGTH OF TEXT IN FROM FIELD                              
016300     MOVE ZERO TO FLENG                                                   
016400     INSPECT FUNCTION REVERSE (TECONV-FROM)                               
016500       TALLYING FLENG FOR LEADING SPACE                                   
016600     COMPUTE FLENG = TECONV-LENG - FLENG                                  
016700                                                                          
016800*    -- SET FLAG INDICATING SINGLE BYTE MODE IN INPUT TEXT                
016900     SET SB-MODE TO TRUE                                                  
017000                                                                          
017100*    -- SET LAST USED POSITION IN OUTPUT TEXT FIELD                       
017200     MOVE ZERO  TO IXT                                                    
017300     MOVE SPACE TO TECONV-Temp                                            
017400     .                                                                    
017500     EJECT                                                                
017600 B-MOVE-TO-OUTPUT-FIELD  SECTION.                                         
017700     SKIP2                                                                
017800*    -- MOVE THE CONVERTED UCS2 DOUBLEBYTE TO TEMP OUTPUT FIELD           
017900*    -- IF OUTPUT SHOULD BE IN UCS2, SIGNAL OVEFLOW OF OUTPUT             
017910*    -- FIELD WITH A T IN KDSVAR, BUT NOT IF UTF8 IS SPECIFIED            
017920*    -- SINCE THEN WE DO NOT YET KNOW THE FINAL LENGTH                    
018000                                                                          
018100*    -- SO FAR ALL IS OK                                                  
018200     MOVE SPACE TO WKDSVAR                                                
018300                                                                          
018400     IF FLTXTENT = YES OR JA                                              
018500*      -- KEEP "ASCII CHARACTERS" IN EBCDIC, BUT CHANGE                   
018600*      -- ANYTHING HIGHER TO CORRESPONDING SGML HEXADECIMAL               
018700*      -- CHARACTER ENTITY SYMBOL: &#xnnnn;                               
018800       IF TEMP-2BYTE-X <= X'007F'                                         
018900*        -- IT IS A SIMPLE ASCII CHARACTER.                               
019000*        -- DON'T CONVERT, JUST MOVE THE EBCDIC CHARACTER.                
019100         ADD 1 TO IXT                                                     
019200         MOVE TECONV-FROM (IXF:1) TO TECONV-TEMP (IXT:1)                  
019300       ELSE                                                               
019400         IF IXT <= KVMAXTL - 8                                            
019500           MOVE 4 TO IXU                                                  
019600           MOVE LOW-VALUE TO TEMP-4BYTE-FIRST2                            
019700           PERFORM 4 TIMES                                                
019800             DIVIDE TEMP-4BYTE-N BY 16 GIVING TEMP-4BYTE-N                
019900                    REMAINDER IXH                                         
020000             MOVE HEX-CHAR (IXH + 1:1) TO TEMP-HEXNR (IXU:1)              
020100             SUBTRACT 1 FROM IXU                                          
020200           END-PERFORM                                                    
020300           ADD 1 TO IXT                                                   
020400           MOVE TEMP-HEX-ENT TO TECONV-TEMP (IXT:8)                       
020500           ADD 7 TO IXT                                                   
020600         ELSE                                                             
020700           MOVE 'T'  TO WKDSVAR                                           
020800         END-IF                                                           
020900       END-IF                                                             
021000                                                                          
021100     ELSE                                                                 
021200       IF FLUTF8 = JA OR YES OR IXT <= KVMAXTL - 2                        
021300         ADD 1 TO IXT                                                     
021400         MOVE TEMP-2BYTE-X TO TECONV-TEMP (IXT:2)                         
021500         ADD 1 TO IXT                                                     
021600       ELSE                                                               
021700         MOVE 'T'  TO WKDSVAR                                             
021800       END-IF                                                             
021900     END-IF                                                               
022000                                                                          
022100*    -- DON'T OVERLAY ANY F VALUE                                         
022200     IF WKDSVAR NOT = SPACE  AND  KDSVAR = SPACE                          
022300       MOVE WKDSVAR TO KDSVAR                                             
022400       MOVE IXF TO IXF-DISP                                               
022500       STRING 'POS ' IXF-DISP '. ' DELIMITED BY SIZE                      
022600       INTO BEFEL WITH POINTER EPTR                                       
022700     END-IF                                                               
022800     .                                                                    
