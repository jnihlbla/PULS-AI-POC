000100 01  W475INT.                                                             
000200*                                 DAGLIG HOPSAMLING TILL                  
000300*                                 VOLVO TRANSPORT FÖR INTRASTAT           
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLANDX2             PIC X(2).                                    
000700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000800     03 TIFAKT               PIC S9(7)           COMP-3.                  
000900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 KVLEVART             PIC S9(7)           COMP-3.                  
001300*                                 LEVERERAT ANTAL STYCK                   
001400     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
001500*                                 SUMMA FAKTURERAT BELOPP                 
001600     03 IDSTATNR             PIC S9(9)           COMP-3.                  
001700*                                 STATISTISKT NUMMER                      
001800*                                 1 = NORSKT                              
001900*                                 2 = ENGELSKT                            
002000*                                 3 = BELGISKT                            
002100*                                 4 = PERUANSKT                           
002200*                                 5 = SVENSKT                             
002300*                                 6 =                                     
002400     03 KDARTURS             PIC X(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD                     
002600     03 IDVAT                PIC X(17).                                   
002700*                                 MOMSREGISTRERINGSNUMMER                 
002800     03 KDINTTYP             PIC S9(2)           COMP-3.                  
002900*                                 AFFÄRSHÄNDELSEKOD                       
003000*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
