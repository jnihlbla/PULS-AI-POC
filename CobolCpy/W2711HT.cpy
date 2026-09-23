000100 01  W2711HT-CTX.                                                         
000200*                                 COPYTEXT FÖR FILEN W2711HT              
000300*                                 TEMP EXTRAKTFIL AV                      
000400*                                 REFILLFÖRSLAG                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
000800*                                 PERSONKOD REFILLANSVARIG                
000900     03 KDREFTYP             PIC X.                                       
001000*                                 TYP AV REFILLORDER                      
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 KDREFORS             PIC X.                                       
001400*                                 REFILL ORDER STATUSKOD                  
001500     03 KVBEART              PIC S9(7)           COMP-3.                  
001600*                                 BESTÄLLT ANTAL STYCKEN                  
001700     03 TEREFMED             PIC X(75).                                   
001800*                                 INFOMEDDELANDE REFILLARTIKEL            
001900     03 IDSTATNR             PIC S9(9)           COMP-3.                  
002000*                                 STATISTISKT NUMMER                      
002100*                                 1 = NORSKT                              
002200*                                 2 = ENGELSKT                            
002300*                                 3 = BELGISKT                            
002400*                                 4 = PERUANSKT                           
002500*                                 5 = SVENSKT                             
002600*                                 6 =                                     
002700     03 BEART                PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900     03 FLFLYG               PIC X.                                       
003000*                                 FLYGARTIKEL                             
003100     03 KDARTURS             PIC X(2).                                    
003200*                                 ARTIKELURSPRUNGSKOD                     
003300     03 VKART                PIC S9(7)           COMP-3.                  
003400*                                 ARTIKELVIKT (G)                         
003500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003600*                                 ARTIKELVOLYM (CM3)                      
003700*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
