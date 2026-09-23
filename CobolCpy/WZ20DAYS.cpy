000100 01  DAYS-WZ20DAYS.                                                       
000200*                                 COMPUTE NUMBER OF DAYS BETWEEN          
000300*                                 TWO DATES, OR A NEW DATE FROM           
000400*                                 ONE DATE AND A NUMBER OF DAYS.          
000500*                                 THIS SUBROUTINE MAY ALSO BE             
000600*                                 USED TO CONVERT A DATE FROM             
000700*                                 ONE FORMAT TO ANOTHER, BY               
000800*                                 SPECIFYING TIDATE AND KVDAYS=0          
000900*                                 AND DIFFERENT FORMATS IN                
001000*                                 KDDATFMT1 AND KDDATFMT2.                
001100*                                                                         
001200*                                 CALL FORMAT:                            
001300*                                    USING DAYS-WZ20DAYS                  
001400*                                 COPYTEXT WZ20DAYS CONTAINS THE          
001500*                                 FOLLOWING FIELDS:                       
001600*                                    DAYS-TIDATE1                         
001700*                                    DAYS-KDDATFMT1                       
001800*                                    DAYS-TIDATE2                         
001900*                                    DAYS-KDDATFMT2                       
002000*                                    DAYS-KVDAYS                          
002100*                                    DAYS-IDCALEND                        
002200*                                    DAYS-KDRC                            
002300*                                                                         
002400*                                 TIDATE1, TIDATE2 - DATES IN THE         
002500*                                     FORMAT SPECIFIED BY                 
002600*                                     KDDATFMT1 AND KDDATFMT2             
002700*                                                                         
002800*                                 KDDATFMT1, KDDATFMT2                    
002900*                                     STRINGS DESCRIBING THE              
003000*                                     FORMAT OF THE DATES,                
003100*                                     "YYMMDD", "YYWW" "YYYYDDD"          
003200*                                     ETC. SEE STANDARD                   
003300*                                     SUBROUTINE CEEDAYS FOR              
003400*                                     DETAILS ABOUT POSSIBLE              
003500*                                     FORMATS                             
003600*                                                                         
003700*                                 KVDAYS  - A NUMBER GIVING               
003800*                                     THE NUMBER OF DAYS BETWEEN          
003900*                                     THE TWO DATES. IF TIDATE2           
004000*                                     IS EARLIER THAN TIDATE1             
004100*                                     THIS NUMBER IS NEGATIVE.            
004200*                                     THIS FIELD IS ALWAYS                
004300*                                     EXPRESSED AS A NUMBER OF            
004400*                                     DAYS, EVEN IF THE DATE              
004500*                                     FIELDS ARE GIVEN IN A               
004600*                                     FORMAT NOT CONTAINING DAYS.         
004700*                                     TIDATE1 = TIDATE2 WILL              
004800*                                     ALWAYS GIVE KVDAYS = 0              
004900*                                                                         
005000*                                 IDCALEND - A STRING IDENTIFYING         
005100*                                     A CALENDAR CONTAINING FREE          
005200*                                     DAYS THAT SHOULD BE IGNORED         
005300*                                     IN THE COMPUTATIONS.                
005400*                                     IF ALL DAYS ARE SIGNIFICANT         
005500*                                     A BLANK VALUE SHOULD BE             
005600*                                     SPECIFIED.                          
005700*                                                                         
005800*                                 KDRC = 0 IF THE ARGUMENTS ARE           
005900*                                     VALID, 8 OTHERWISE.                 
006000*                                                                         
006100*                                 TWO OF THE THREE FIELDS TIDATE1         
006200*                                 TIDATE2 AND KVDAYS MUST BE              
006300*                                 GIVEN AND ONE MUST BE BLANK             
006400*                                 (KVDAYS SHOULD BE ZERO).                
006500*                                 THE SUBROUTINE WILL THEN                
006600*                                 CALCULATE THE "MISSING" FIELD.          
006700*                                 KDDATFMT AND KDDATFMT2 MUST             
006800*                                 ALWAYS BE SPECIFIED.                    
006900*                                                                         
007000     03 DAYS-TIDATE1         PIC X(20).                                   
007100*                                 DATE IN VARYING FORMATS                 
007200     03 DAYS-KDDATFMT1       PIC X(20).                                   
007300*                                 FORMAT SPECIFICATION FOR A DATE         
007400     03 DAYS-TIDATE2         PIC X(20).                                   
007500*                                 DATE IN VARYING FORMATS                 
007600     03 DAYS-KDDATFMT2       PIC X(20).                                   
007700*                                 FORMAT SPECIFICATION FOR A DATE         
007800     03 DAYS-KVDAYS          PIC S9(9)           COMP.                    
007900*                                 NUMBER OF DAYS                          
008000     03 DAYS-IDCALEND        PIC X(8).                                    
008100*                                 CALENDAR IDENTITY                       
008200*                                 BLANK, "WEEKDAYS" OR IDDC NBR           
008300*                                 MAY BE USED.                            
008400     03 DAYS-KDRC            PIC S9(9)           COMP.                    
008500*                                 RETURN CODE                             
008600*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
