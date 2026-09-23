000100 01  W224971.                                                             
000200*                                 POSTTYP 971                             
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 LB-INFO-NEW.                                                      
001100        05 KDKG-NEW          PIC S9              COMP-3.                  
001200*                                 KURANSGRUPP                             
001300        05 IDLKTO-NEW        PIC S9(7)           COMP-3.                  
001400*                                 LAGERKONTO                              
001500     03 LB-INFO-OLD.                                                      
001600        05 KDKG-OLD          PIC S9              COMP-3.                  
001700*                                 KURANSGRUPP                             
001800        05 IDLKTO-OLD        PIC S9(7)           COMP-3.                  
001900*                                 LAGERKONTO                              
002000        05 KVLS-OLD          PIC S9(7)           COMP-3.                  
002100*                                 LAGERSALDO                              
002200        05 KVEFRS-OLD        PIC S9(7)           COMP-3.                  
002300*                                 EJ-FAKTURERADE-RADER SALDO              
002400        05 KVAKS-OLD         PIC S9(7)           COMP-3.                  
002500*                                 ANKOMSTSALDO                            
002600        05 PRARTSTD-OLD      PIC S9(7)V9(2)      COMP-3.                  
002700*                                 ARTIKELSTANDARDPRIS                     
002800*** END OF VILMAII-COPY LENGTH= 36 OLD LENGTH= LENGTH=36                  
