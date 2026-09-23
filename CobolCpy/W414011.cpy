000100 01  ANNVOR-W414011.                                                      
000200*                                 SKAPAS FÖR ANNULLATION                  
000300*                                 AV VORKÖRAD                             
000400*                                 SÄNDS TILL SRS.                         
000500     03 ANNVOR-IDPTYP        PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 ANNVOR-IDARTNR       PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 ANNVOR-IDDC          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 ANNVOR-IDDISTR       PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 ANNVOR-IDKUNDNR      PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 ANNVOR-KDPRODSL      PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700     03 ANNVOR-FLDIRLEV      PIC X.                                       
001800*                                 DIREKTLEVERANS ?                        
001900     03 ANNVOR-KVANNANT      PIC S9(7)           COMP-3.                  
002000*                                 ANNULLERAT ANTAL ARTIKLAR               
002100     03 ANNVOR-KVAVBART      PIC S9(7)           COMP-3.                  
002200*                                 AVBOKAT ANTAL ARTIKLAR                  
002300     03 ANNVOR-KVBEART-Q     PIC S9(7)           COMP-3.                  
002400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002500*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
