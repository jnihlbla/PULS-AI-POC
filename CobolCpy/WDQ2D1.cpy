000100 01  SEQD-WDQ2D1.                                                         
000200*                                 ORDERHUVUD REGISTER                     
000300*                                 SEKUNDÄRT INDEX TILL WDQ201             
000400*                                 KUND/TIREGDAT-9KOMPL INGÅNG             
000500*                                 FYSISK NYCKEL: WDQ2D1KY                 
000600*                                 (FLSOFT, IDDISTR, IDKUNDNR, TIR         
000700*                                 EGDA9,                                  
000800*                                  IDORDER)                               
000900*                                 SECONDARY NYCKEL: WDQ2DSEQ              
001000*                                 (FLSOFT, IDDISTR, IDKUNDNR)             
001100*                                                                         
001200     03 SEQD-FLSOFT          PIC X.                                       
001300*                                 FLAGGA SOFTVARA                         
001400*                                 SOFTWARE MARK                           
001500     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100     03 SEQD-TIREGDAT-9KOMPL PIC S9(7)           COMP-3.                  
002200*                                 DATUMETS 9-KOMPLEMENT                   
002300*                                 DATES 9-COMPLEMENT                      
002400     03 SEQD-IDORDER         PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600*                                 VOLVO PARTS ORDER NUMBER                
002700     03 SEQD-TIREGDAT        PIC S9(7)           COMP-3.                  
002800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002900*                                 REGISTRATION DATE (YYMMDD)              
003000*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
