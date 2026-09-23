000100 01  W405772.                                                             
000200*                                 COPYTEXT TILL DOCUMENT LINE BO-         
000300*                                 HUNTING                                 
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-KDPGM           PIC X.                                       
000700     03 LINE-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 LINE-IDDISTR         PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 LINE-IDKUNDNR        PIC Z(5)9.                                   
001200*                                 KUNDNUMMER                              
001300     03 LINE-IDORDNR7        PIC Z(6)9.                                   
001400*                                 ORDERNUMMER                             
001500     03 LINE-TIRFSDAT        PIC X(6).                                    
001600*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001700     03 LINE-TIRODAT         PIC X(6).                                    
001800*                                 RESTORDERDATUM         (ÅÅMMDD)         
001900     03 LINE-BERADREF        PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100     03 LINE-IDARTNR         PIC Z(7)9.                                   
002200*                                 ARTIKELNUMMER                           
002300     03 LINE-BEART-ENG       PIC X(25).                                   
002400*                                 ENGELSK ARTIKELBENÄMNING                
002500     03 LINE-KVART           PIC Z(7).                                    
002600*                                 ANTAL ARTNR PER BRYTBEGREPP             
002700     03 LINE-KVRO            PIC Z(6).                                    
002800*                                 ANTAL RESTNOTERADE ARTIKLAR             
002900     03 LINE-IDANSK          PIC Z(2)9.                                   
003000*                                 ANSKAFFARNUMMER                         
003100     03 LINE-TIREPDAT        PIC X(6).                                    
003200*                                 REPAIR DATE                             
003300*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
