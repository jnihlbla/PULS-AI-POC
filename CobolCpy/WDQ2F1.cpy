000100 01  SEQF-WDQ2F1.                                                         
000200*                                 ORDERHUVUD REGISTER                     
000300*                                 SEKUNDÄRT INDEX TILL WDQ201             
000400*                                 TIREGDAT-9KOMPL/KUND INGÅNG             
000500*                                 FYSISK NYCKEL: WDQ2F1KY                 
000600*                                 (FLSOFT, IDDISTR, TIREGDA9, IDK         
000700*                                 UNDNR,                                  
000800*                                  IDORDER)                               
000900*                                 SECONDARY NYCKEL: WDQ2FSEQ              
001000*                                 (FLSOFT, IDDISTR, TIREGDA9)             
001100*                                                                         
001200     03 SEQF-FLSOFT          PIC X.                                       
001300*                                 FLAGGA SOFTVARA                         
001400*                                 SOFTWARE MARK                           
001500     03 SEQF-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 SEQF-TIREGDAT-9KOMPL PIC S9(7)           COMP-3.                  
001900*                                 DATUMETS 9-KOMPLEMENT                   
002000*                                 DATES 9-COMPLEMENT                      
002100     03 SEQF-IDKUNDNR        PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400     03 SEQF-IDORDER         PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600*                                 VOLVO PARTS ORDER NUMBER                
002700     03 SEQF-TIREGDAT        PIC S9(7)           COMP-3.                  
002800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002900*                                 REGISTRATION DATE (YYMMDD)              
003000*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
