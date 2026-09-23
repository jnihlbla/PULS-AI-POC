000100 01  SEQB-WDQ2B1.                                                         
000200*                                 ORDERHUVUDS REGISTER                    
000300*                                 SEKUNDÄRT INDEX TILL WDQ201             
000400*                                 TRANSPORT-INGÅNG VIA WDQ212             
000500*                                 FYSISK NYCKEL: WDQ2B1KY                 
000600*                                 (IDDC, IDTRP, DATRPAVT,                 
000700*                                  IDORDER)                               
000800*                                 SECONDARY NYCKEL: WDQ2BSEQ              
000900*                                 (IDDC, IDTRP, DATRPAVT)                 
001000     03 SEQB-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQB-IDTRP.                                                       
001400*                                 TRANSPORTIDENTITET                      
001500*                                 TRANSPORTIDENTITY                       
001600        05 SEQB-IDTRPLOS     PIC X(3).                                    
001700*                                 TRANSPORTLÖSNING                        
001800*                                 TRANSPORTSOLUTION                       
001900        05 SEQB-IDTRPVAR     PIC X(2).                                    
002000*                                 TRANSPORTLÖSNINGSGRUPP                  
002100*                                 TRANSPORTSOLUTIONGROUP                  
002200     03 SEQB-DATRPAVT.                                                    
002300*                                 TRANSPORTAVGÅNGSTIDPUNKT                
002400*                                 TRANSPORT DEPARTURE                     
002500*                                 YYYYMMDD+HHMM                           
002600        05 SEQB-DATRPAVD     PIC 9(8).                                    
002700*                                 TRANSPORTAVGÅNGSDATUM                   
002800*                                 TRANSPORT DEPARTURE DATE                
002900        05 SEQB-TIHHMM       PIC S9(5)           COMP-3.                  
003000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003100*                                 TIME IN HOUR AND MINUTE                 
003200     03 SEQB-IDORDER         PIC S9(7)           COMP-3.                  
003300*                                 VOLVO PARTS ORDERNUMMER                 
003400*                                 VOLVO PARTS ORDER NUMBER                
003500*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
