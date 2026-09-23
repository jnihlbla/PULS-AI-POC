000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             WTRAUTF8.                                        
000500 AUTHOR.                  UMESH JAIN                                      
000600     DATE-WRITTEN.       OCT 2011.                                        
000700*                                                                         
000800*                                                                         
000900*    FUNCTION:                                                            
001000*      TRANSLATE TEXT TO UNICODE (UTF-8) IF IS NOT ALREADY                
001100*      IN THIS FORMAT. FOR TEXT ALREAY IN UNICODE, IT REMOVES             
001200*      ANY TRAILING EBCDIC SPACE AND CHANGES IT TO UNICODE SPACE.         
001300*                                                                         
001400*    CALLED BY:                                                           
001500*          CALL WTRAUTF8 USING TRAUTF8-AREA                               
001600*                                                                         
001700*    INPUT ARGUMENTS:                                                     
001800*          TRAUTF8-KDCP            INPUT CODE PAGE                        
001900*          TRAUTF8-TECONV-FROM     INPUT TEXT.                            
002000*                                                                         
002100*    OUTPUT ARGUMENTS:                                                    
002200*          TRAUTF8-TECONV-TO       CONVERTED TEXT                         
002300*                                                                         
002400*    EXAMPLES OF POSSIBLE INPUT CODE PAGES:                               
002500*        278 - EBCDIC CODEPAGE 278 (SWEDISH EBCDIC)                       
002600*        930 - EBCDIC CODEPAGE 930 (JAPANESE EBCDIC)                      
002700*        935 - EBCDIC CODEPAGE 930 (CHINESE EBCDIC)                       
002800*        UTF8  - UNICODE TRANSMISSION FORMAT                              
002900*                                                                         
003000     EJECT                                                                
003100                                                                          
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  PROGRAM-NAMN            PIC X(8)  VALUE 'WTRAUTF8'.                  
003600                                                                          
003700 77  CP-FROM                 PIC 9(5).                                    
003800 77  UTF8                    PIC 9(5)  VALUE 1208.                        
003900                                                                          
004000*-- THE TEXT FIELDS IN THE LINK AREA ARE 700 BYTES LONG                   
004100 77  LTEXT                   PIC S9(4) BINARY VALUE 701.                  
004200 77  UCS-TEXT                PIC N(700).                                  
004300                                                                          
004400 LINKAGE SECTION.                                                         
004500                                                                          
004600 01  LINK-AREA.                                                           
004700*    03 -COPY WTRAUTF8                                                    
004800                                                                          
004900                                                                          
005000 PROCEDURE DIVISION  USING  LINK-AREA.                                    
005100                                                                          
005200     IF TRAUTF8-KDCP = 'UTF8'                                             
005300*      -- MOST OF THE TEXT IS ALREADY IN UTF8, BUT                        
005400*      -- TRAILING EBCDIC SPACE (HEX 40) NEEDS TO BE                      
005500*      -- UNICODE SPACE (HEX 20)                                          
005600       MOVE TRAUTF8-TECONV-FROM TO TRAUTF8-TECONV-TO                      
005700                                                                          
005800       MOVE ZERO TO TALLY                                                 
005900       INSPECT FUNCTION REVERSE(TRAUTF8-TECONV-TO)                        
006000         TALLYING TALLY FOR LEADING SPACE                                 
006100       IF TALLY > ZERO                                                    
006200         INSPECT TRAUTF8-TECONV-TO (LTEXT - TALLY:)                       
006300           CONVERTING SPACE TO X'20'                                      
006400       END-IF                                                             
006500                                                                          
006600     ELSE                                                                 
006700*      -- CONVERT FROM EBCDIC TO UNICODE.                                 
006800*      -- THE GIVEN CODE-PAGE SHOULD BE A NUMERIC VALUE                   
006900*      -- CORREPSONDING TO THE EBCDIC CODEPAGE OR CCSID                   
007000       COMPUTE CP-FROM = FUNCTION NUMVAL(TRAUTF8-KDCP)                    
007100                                                                          
007200*      -- FIRST CONVERT TEXT TO UNICODE UCS2 FORMAT                       
007300       MOVE FUNCTION NATIONAL-OF(TRAUTF8-TECONV-FROM, CP-FROM)            
007400            TO UCS-TEXT                                                   
007500*      -- THEN CONVERT IT TO UTF8                                         
007600       MOVE FUNCTION DISPLAY-OF(UCS-TEXT, UTF8)                           
007700            TO TRAUTF8-TECONV-TO                                          
007800     END-IF                                                               
007900                                                                          
008000*    -- IF MAX LENGTH IS PROVIDED - REMOVE OVERFLOWING CHARTACTERS        
008100     IF TRAUTF8-KVMAXTL IS NUMERIC AND TRAUTF8-KVMAXTL > 0                
008200*      -- SPACE OUT POSITIONS AFTER MAX LENGTH                            
008300       MOVE ALL X'20' TO TRAUTF8-TECONV-TO(TRAUTF8-KVMAXTL:)              
008400*      -- CHECK FOR TRUNCATED CHARACTERS AT THE END THE TEXT              
008500*      -- AND CHANGE SUCH BYTES TO UNICODE SPACE                          
008600       IF TRAUTF8-TECONV-TO(TRAUTF8-KVMAXTL:1)       >= X'C0'             
008700*        -- ONLY FIRST BYTE OF TWO OR THREE (C0 = B'110...'               
008800         MOVE X'20' TO TRAUTF8-TECONV-TO(TRAUTF8-KVMAXTL:1)               
008900       END-IF                                                             
009000       IF TRAUTF8-TECONV-TO(TRAUTF8-KVMAXTL - 1:1)   >= X'E0'             
009100*        -- ONLY FIRST TWO BYTES OF THREE (E0 = B'1110...'                
009200         MOVE X'2020'                                                     
009300         TO TRAUTF8-TECONV-TO(TRAUTF8-KVMAXTL - 1:2)                      
009400       END-IF                                                             
009500     END-IF                                                               
009600                                                                          
009700     MOVE ZERO TO RETURN-CODE                                             
009800     GOBACK                                                               
009900     .                                                                    
