000100 01  LIST-WL01592.                                                        
000200*                                 HEADER RECORD FOR RETURN DOCUME         
000300*                                 NT                                      
000400     03 LIST-IDAFPRCD-LINE   PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LIST-IDARTNR         PIC Z(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 LIST-BEART           PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 LIST-IDRAPPNR        PIC Z(6)9.                                   
001100*                                 RAPPORT NUMMER                          
001200     03 LIST-KVLEVART        PIC Z(6)9.                                   
001300*                                 LEVERERAT ANTAL STYCK                   
001400     03 LIST-PRFKTUTL-RAD    PIC Z(6)9.9(2).                              
001500*                                 ARTIKELSTANDARDPRIS                     
001600     03 LIST-KDARTURS        PIC X(2).                                    
001700*                                 ARTIKELURSPRUNGSKOD                     
001800     03 LIST-IDSTATNR        PIC Z(8)9.                                   
001900*                                 STATISTISKT NUMMER                      
002000*                                 1 = NORSKT                              
002100*                                 2 = ENGELSKT                            
002200*                                 3 = BELGISKT                            
002300*                                 4 = PERUANSKT                           
002400*                                 5 = SVENSKT                             
002500*                                 6 =                                     
002600     03 LIST-VKART-TOT       PIC Z(6)9.9(2).                              
002700*                                 ARTIKELVIKT (G/OZ)                      
002800*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
