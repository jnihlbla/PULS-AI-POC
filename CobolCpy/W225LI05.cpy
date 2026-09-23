000100 01  W225LI05.                                                            
000200*                                 LISTRECORD FÖR ÅTER STÅENDE             
000300*                                 ORDERRADER                              
000400*                                                                         
000500     03 SORTARGUMENT.                                                     
000600        05 IDLISTA           PIC S9(3)           COMP-3.                  
000700*                                 LISTNUMMER                              
000800        05 IDARTNR           PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000        05 KDCLAGER          PIC S9              COMP-3.                  
001100*                                 CENTRALLAGERKOD                         
001200        05 IDANSK            PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400     03 RESTORDERINFO.                                                    
001500        05 KVRORAD-KVAR-V1   PIC S9(7)V9(2)      COMP-3.                  
001600*                                             KVRORAD-KVAR-V1-003         
001700*                                 RESTNOTERADE RADER VECKA1               
001800        05 KVRORAD-KVAR-IV   PIC S9(7)V9(2)      COMP-3.                  
001900*                                             KVRORAD-KVAR-IV-003         
002000*                                 RESTNOTERADE RADER                      
002100*                                 INNEVARANDE VECKA                       
002200        05 KVRORAD-KVAR-P    PIC S9(7)V9(2)      COMP-3.                  
002300*                                              KVRORAD-KVAR-P-003         
002400*                                 RESTNOTERADE RADER PERIOD               
002500        05 KVINORD-KVAR-V1   PIC S9(7)           COMP-3.                  
002600*                                 ORDERINGÅNG VECKA                       
002700        05 KVINORD-KVAR-IV   PIC S9(7)           COMP-3.                  
002800*                                 ORDERINGÅNG                             
002900*                                 INNEVARANDE VECKA                       
003000        05 KVINORD-KVAR-P    PIC S9(7)           COMP-3.                  
003100*                                 ORDERINGÅNG PERIOD                      
003200*** END COPY W225LI05C0  LENGTH=37                                        
