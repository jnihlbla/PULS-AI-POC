000100 01  LINE-WL01292.                                                        
000200*                                 COPYTEXT FOR DELIVERY NOTE LDC          
000300*                                 LINE                                    
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-REP-IDPTYP-2    PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 LINE-REP-IDORDNR-RO  PIC Z(5).                                    
000900*                                 ORDERNUMMER UTGÅR PD90                  
001000     03 LINE-REP-IDARTNR     PIC Z(7)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 LINE-REP-REKSIFFR    PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 LINE-REP-BERADREF    PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600     03 LINE-REP-IDARTNR-ERS PIC X.                                       
001700     03 LINE-REP-BEART       PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900     03 LINE-REP-KVLEVART    PIC Z(6)9.                                   
002000*                                 LEVERERAT ANTAL STYCK                   
002100     03 LINE-IDLEVART        PIC X(13).                                   
002200     03 LINE-IDSYSTEM        PIC X(4).                                    
002300*                                 VOLVO VCCS SYSTEMNUMMER                 
002400     03 LINE-FILLER          PIC X(192).                                  
002500*** END OF VILMAII-COPY LENGTH= 279 BYTES                                 
