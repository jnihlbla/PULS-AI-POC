000100 01  SEQD-WDE6D1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE601             
000300*                                 SKAPAS NÄR FRAKTSEDEL SKALL UT          
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 FLFRAKTS  = J                           
000600*                                 FYSISK NYCKEL: WDE6D1KY                 
000700*                                  (IDDISTR, IDKUNDNR, KDFRAKT,           
000800*                                   IDPRODNR-SAMP, IDPRODNR)              
000900*                                 SECONDARY KEY: WDE6DSEQ                 
001000*                                  (IDDISTR, IDKUNDNR, KDFRAKT)           
001100     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQD-KDFRAKT         PIC S9(3)           COMP-3.                  
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900*                                 FREIGHT CODE                            
002000     03 SEQD-IDPRODNR-SAMP   PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER SAMPACKNING           
002200     03 SEQD-IDPRODNR        PIC S9(7)           COMP-3.                  
002300*                                 PRODUKTIONSNUMMER                       
002400*                                 PRODUCTION NUMBER                       
002500     03 SEQD-KDORDKL         PIC S9              COMP-3.                  
002600*                                 ORDERKLASS                              
002700*                                 ORDER CLASS                             
002800     03 SEQD-IDDC            PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
