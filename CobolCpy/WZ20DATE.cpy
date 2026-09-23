000100 01  DATE-WZ20DATE.                                                       
000200*                                                                         
000300*                                 1. CHECKS IF A DATE IS VALID            
000400*                                    AND CONVERTS IT TO LILIAN            
000500*                                    FORMAT (NBR OF DAYS SINCE            
000600*                                    OCTOBER 14, 1582)                    
000700*                                 2. CONVERTS A DATE IN LILIAN            
000800*                                    FORMAT TO SOME OTHER FORMAT.         
000900*                                                                         
001000*                                 THE DATE MAY BE GIVEN/RETURNED          
001100*                                 IN MANY DIFFERENT FORMATS.              
001200*                                 COBOL LANGUAGE ENVIRONMENT              
001300*                                 STANDARD PROGRAM "CEEDATE" IS           
001400*                                 USED FOR VALIDATING AND                 
001500*                                 CONVERTING THE DATE IN MOST             
001600*                                 CASES, SO ALL FORMATS SUPPORTED         
001700*                                 BY THIS SUBROUTINE ARE ALSO             
001800*                                 SUPPORTED HERE. BESIDE THOSE            
001900*                                 FORMATS, WEEK NUMBER ("WW") AND         
002000*                                 WEEK + DAY-IN-WEEK ("WWD") ARE          
002100*                                 ALSO SUPPORTED.                         
002200*                                                                         
002300*                                 CALL FORMAT:                            
002400*                                    USING DATE-WZ20DATE                  
002500*                                 WHERE WZ20DATE CONTAINS THE             
002600*                                 FOLLOWING FIELDS:                       
002700*                                    DATE-TIDATE                          
002800*                                    DATE-KDDATFMT                        
002900*                                    DATE-TILILDAT                        
003000*                                    DATE-KDRC                            
003100*                                                                         
003200*                                 TIDATE - 1. A STRING CONTAINING         
003300*                                     THE DATE TO BE VALIDATED            
003400*                                     OR                                  
003500*                                     2. SPACE IF A DATE IS TO            
003600*                                     BE RETURNED.                        
003700*                                                                         
003800*                                 KDDATFMT - A STRING DESCRI-             
003900*                                     BING THE FORMAT OF THE              
004000*                                     INPUT DATE I.E. "YYMMDD",           
004100*                                     "YYYYDDD" ETC. SEE ITEM             
004200*                                     KDDATFMT OR SUBROUTINE              
004300*                                     CEEDATE FOR DETAILS ABOUT           
004400*                                     POSSIBLE FORMATS.                   
004500*                                                                         
004600*                                 TILILDAT - A NUMBER REPRE-              
004700*                                     SENTING THE DATE IN                 
004800*                                     "LILIAN" FORMAT (= NUMBER           
004900*                                     OF DAYS SINCE 14 OCT 1582)          
005000*                                     OUTPUT FIELD IN CASE 1,             
005100*                                     INPUT FIELD IN CASE 2.              
005200*                                                                         
005300*                                 KDRC = 0 IF THE DATE IS                 
005400*                                     VALID, 8 OTHERWISE.                 
005500*                                                                         
005600     03 DATE-TIDATE          PIC X(20).                                   
005700*                                 DATE IN VARYING FORMATS                 
005800     03 DATE-KDDATFMT        PIC X(20).                                   
005900*                                 FORMAT SPECIFICATION FOR A DATE         
006000     03 DATE-TILILDAT        PIC S9(9)           COMP.                    
006100*                                 DATE IN LILIAN FORMAT                   
006200     03 DATE-KDRC            PIC S9(9)           COMP.                    
006300*                                 RETURN CODE                             
006400*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
