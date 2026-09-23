000100 01  W413626.                                                             
000200*                                 ANNULLATIONER OCH FLYTTNINGAR           
000300*                                 MELLAN KOLLI PÅ PROFORMA                
000400*                                 REGISTRET I W415.                       
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 KDCLAGER             PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001500*                                 FRAKTSÄTT C1-C2 TILL KUND               
001600     03 IDORDNR              PIC S9(5)           COMP-3.                  
001700*                                 ORDERNUMMER                             
001800     03 IDKOLLI              PIC S9(7)           COMP-3.                  
001900*                                 KOLLINUMMER         IDKOLLI-002         
002000     03 IDARTNR              PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 KVBEART              PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT ANTAL ARTIKLAR                 
002400*** END COPY W413626CC0  LENGTH=29                                        
