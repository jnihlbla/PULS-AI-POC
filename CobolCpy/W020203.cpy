000100 01  W020203.                                                             
000200*                                 POST FRÅN WDG6.                         
000300*                                 POSTTYP 203 = ORDER SOM GÅTT I          
000400*                                               REST FÖR FÖRSTA           
000500*                                               GÅNGEN.                   
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDORDER              PIC S9(7)           COMP-3.                  
001100*                                 VOLVO PARTS ORDERNUMMER                 
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 TIRODAT              PIC S9(7)           COMP-3.                  
001500*                                 RESTORDERDATUM         (ÅÅMMDD)         
001600     03 KVLEVART             PIC S9(7)           COMP-3.                  
001700*                                 LEVERERAT ANTAL ARTIKLAR                
001800     03 KDORDKL              PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002400*** END COPY W020203     LENGTH=30                                        
