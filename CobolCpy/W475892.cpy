000100 01  W475892.                                                             
000200*                                 FAKTURA-RAD                             
000300*                                                                         
000400     03 SORT-DEL.                                                         
000500        05 IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 FILLER1           PIC S9(7)           COMP-3.                  
000800        05 IDSTATNR          PIC S9(9)           COMP-3.                  
000900*                                 STATISTISKT NUMMER                      
001000*                                 1 = NORSKT                              
001100*                                 2 = ENGELSKT                            
001200*                                 3 = BELGISKT                            
001300*                                 4 = PERUANSKT                           
001400*                                 5 = SVENSKT                             
001500*                                 6 =                                     
001600        05 KDARTURS          PIC X(2).                                    
001700*                                 ARTIKELURSPRUNGSKOD                     
001800     03 FILLER.                                                           
001900        05 IDDISTR           PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100        05 KVLEV             PIC S9(7)           COMP-3.                  
002200*                                 ANTAL LEVERANSER      KVLEV-002         
002300        05 VKLEV             PIC S9(6)V9(1)      COMP-3.                  
002400*                                 ORDER-VIKT NETTO (KG)                   
002500        05 SUFAKT            PIC S9(9)V9(2)      COMP-3.                  
002600*                                 SUMMA FAKTURERAT BELOPP                 
002700        05 SUEEC             PIC S9(9)V9(2)      COMP-3.                  
002800*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
002900        05 SUEFTA            PIC S9(9)V9(2)      COMP-3.                  
003000*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
003100        05 SUOEVR            PIC S9(9)V9(2)      COMP-3.                  
003200*                                 SUMMERING EJ EEC ELLER EFTA             
003300*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
