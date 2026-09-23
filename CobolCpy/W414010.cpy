000100 01  W414010.                                                             
000200*                                 SKAPAS FÖR VORKÖRAD SOM                 
000300*                                 UPPSTOD I ORDERENTRY.                   
000400*                                 SÄNDS TILL SRS.                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 KDORDBEK             PIC 9(2).                                    
001400*                                 ORDERBEKRÄFTELSEKOD                     
001500     03 KDORDKL              PIC S9              COMP-3.                  
001600*                                 ORDERKLASS                              
001700     03 KDORDING             PIC S9              COMP-3.                  
001800*                                 UPPDATERING ORDERINGÅNG                 
001900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 FLDIRLEV             PIC X.                                       
002200*                                 DIREKTLEVERANS ?                        
002300     03 FLFORBI              PIC X.                                       
002400*                                 FÖRBIORDERFLAGGA                        
002500     03 FLORDSPE             PIC X.                                       
002600*                                 SPECIALORDERFLAGGA                      
002700     03 FLOVRLEV             PIC X.                                       
002800*                                 ÖVERLEVERANS                            
002900*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
