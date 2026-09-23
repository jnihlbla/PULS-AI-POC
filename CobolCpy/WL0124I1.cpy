000100 01  REQU-WL0124I1.                                                       
000200*                                 REQUEST TO PGM WL0124                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 REQU-IDORDNR7-KEY    PIC 9(7).                                    
001300*                                 ORDERNUMMER                             
001400*                                 ORDER NUMBER                            
001500     03 REQU-IDPRODNR-KEY    PIC 9(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700*                                 PRODUCTION NUMBER                       
001800     03 REQU-IDPLKLST-UPD    PIC 9(3).                                    
001900*                                 PLOCKLISTNUMMER                         
002000*                                 PICKING LIST NUMBER                     
002100     03 REQU-TIAAMMDD-UPD    PIC 9(6).                                    
002200*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002300*                                 YEAR - MONTH - DAY  (YYMMDD)            
002400     03 REQU-TIHHMM-UPD      PIC 9(4).                                    
002500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002600*                                 TIME IN HOUR AND MINUTE                 
002700     03 REQU-FLJANEJ         PIC X.                                       
002800*                                 JA/NEJ-FLAGGA                           
002900     03 REQU-KVRADER         PIC 9(5).                                    
003000*                                 ANTAL RADER                             
003100*                                 NUMBER OF LINES                         
003200     03 REQU-RAD             OCCURS 500 TIMES.                            
003300        05 REQU-IDPLKLST-RAD PIC 9(3).                                    
003400*                                 PLOCKLISTNUMMER                         
003500*                                 PICKING LIST NUMBER                     
003600*** END OF VILMAII-COPY LENGTH= 1545 BYTES                                
