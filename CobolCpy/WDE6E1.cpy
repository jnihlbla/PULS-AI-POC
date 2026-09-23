000100 01  SEQE-WDE6E1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE611             
000300*                                 EXIT: INDEX FINNS NÄR                   
000400*                                 IDKOLLI-SAMP > 0 & KDSTASK = C          
000500*                                 FYSISK NYCKEL: WDE6E1KY                 
000600*                                  (IDDC, IDKOLLIS,                       
000700*                                   IDPRODNR, IDKOLLI)                    
000800*                                 SECONDARY NYCKEL: WDE6ESEQ              
000900*                                  (IDDC, IDKOLLIS,                       
001000     03 SEQE-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQE-IDKOLLI-SAMP    PIC S9(5)           COMP-3.                  
001400*                                 SAMPACKNINGSKOLLINUMMER                 
001500*                                 MIXED PACKING CASE NUMBER               
001600     03 SEQE-IDPRODNR        PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800*                                 PRODUCTION NUMBER                       
001900     03 SEQE-IDKOLLI         PIC S9(5)           COMP-3.                  
002000*                                 KOLLINUMMER                             
002100*                                 CASE NUMBER                             
002200     03 SEQE-IDTRPTNR        PIC S9(3)           COMP-3.                  
002300*                                 TRANSPORTIDENTITET                      
002400*                                 TRANSPORT IDENTITY                      
002500*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
