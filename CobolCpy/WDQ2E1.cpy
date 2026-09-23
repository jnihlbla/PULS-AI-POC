000100 01  SEQE-WDQ2E1.                                                         
000200*                                 ORDERHUVUD REGISTER                     
000300*                                 SEKUNDÄRT INDEX TILL WDQ201             
000400*                                 KUND/TIREGDAT/ORDKL  INGÅNG             
000500*                                 FINNS NÄR FLORDTIL=JA                   
000600*                                 FYSISK NYCKEL: WDQ2E1KY                 
000700*                                 (IDDISTR, IDKUNDNR, KDORDKL,            
000800*                                  TIREGDAT-9KOMPL, IDORDER)              
000900*                                 SECONDARY NYCKEL: WDQ2ESEQ              
001000*                                 (IDDISTR, IDKUNDNR, KDORDKL)            
001100     03 SEQE-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQE-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQE-KDORDKL         PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900*                                 ORDER CLASS                             
002000     03 SEQE-TIREGDAT-9KOMPL PIC S9(7)           COMP-3.                  
002100*                                 DATUMETS 9-KOMPLEMENT                   
002200*                                 DATES 9-COMPLEMENT                      
002300     03 SEQE-IDORDER         PIC S9(7)           COMP-3.                  
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500*                                 VOLVO PARTS ORDER NUMBER                
002600*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
