000100 01  W54020.                                                              
000200*                                 BOKFÖRINGSUNDERLAG VID                  
000300*                                 STANDARPRISÄNDRING                      
000400     03 IDLAGER              PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000900*                                 PRODUKTSLAG                             
001000     03 IDLKTO               PIC S9(7)           COMP-3.                  
001100*                                 LAGERKONTO (FFHHHUU)                    
001200     03 PRARTSTD-OLD         PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELSTANDARDPRIS                     
001400     03 PRARTSTD-NEW         PIC S9(7)V9(2)      COMP-3.                  
001500*                                 ARTIKELSTANDARDPRIS                     
001600     03 KVAKS                PIC S9(7)           COMP-3.                  
001700*                                 ANKOMSTSALDO                            
001800     03 KVEFRS               PIC S9(7)           COMP-3.                  
001900*                                 EJ FAKTURERAT ANTAL STYCK               
002000     03 KVLS                 PIC S9(7)           COMP-3.                  
002100*                                 LAGERSALDO                              
002200*** END COPY W54020      LENGTH=35                                        
