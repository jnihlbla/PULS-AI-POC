000100* GENERATION OF COBOL HOST STRUCTURE FROM TP6PART-TAB                     
000200  01 TP6PART.                                                             
000300*              TP6PART                                                    
000400   03 IDARTNR                           PIC S9(9) COMP-3.                 
000500*              ARTIKELNUMMER                                              
000600   03 IDDC                              PIC X(2).                         
000700*              IDENTIFIERARE LAGER                                        
000800   03 ADLAGOMR                          PIC S9(3) COMP-3.                 
000900*              LAGEROMRÅDE                                                
001000   03 ADGANG                            PIC S9(3) COMP-3.                 
001100*              GÅNG                                                       
001200   03 ADPLATS                           PIC S9(5) COMP-3.                 
001300*              LAGERPLATSNUMMER                                           
001400   03 KVLS                              PIC S9(7) COMP-3.                 
001500*              LAGERSALDO                                                 
001600   03 KVPB-REF                          PIC S9(6)V9(1) COMP-3.            
001700*              PERIODBEHOV REFILLING                                      
001800   03 TIREFEFT                          PIC S9(7) COMP-3.                 
001900*              DATUM SENAST EFTERFRÅGAD                                   
002000   03 KDERS                             PIC S9(3) COMP-3.                 
002100*              ERSÄTTNINGSKOD                                             
002200   03 KDSORT                            PIC X(2).                         
002300*              SORT-KOD                                                   
002400   03 KDPRODSL                          PIC S9(3) COMP-3.                 
002500*              PRODUKTSLAG                                                
002600   03 IDSKYLT                           PIC X(3).                         
002700*              NATIONALITETSTECKEN                                        
      *              SPRÅKIDENTIFIKATION                                        
002800   03 BEART                             PIC X(25).                        
002900*              ARTIKELBENÄMNING                                           
003000*                                                                         
003100*** END OF VILMAII-COPY LENGTH= 60 OLD LENGTH=                            
