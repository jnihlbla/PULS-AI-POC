000100 01  W414101.                                                             
000200*                                 COPYTEXT FÖR UPPFÖLJNING VID            
000300*                                 IDENTIFIERING AV ARTNR 100              
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDARTNR              PIC Z(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 KVANTAL              PIC -(6)9.                                   
000900*                                 ANTAL                                   
001000     03 BEART                PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 ADLAGOMR             PIC Z(2)9.                                   
001300*                                 LAGEROMRÅDE                             
001400     03 ADGANG               PIC Z(2)9.                                   
001500*                                 GÅNG                                    
001600     03 ADPLATS              PIC Z(4)9.                                   
001700*                                 LAGERPLATSNUMMER                        
001800     03 IDDC-SEND            PIC X(2).                                    
001900*                                 SÄNDANDE LAGER                          
002000     03 IDDC                 PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 AVVIKELSETYP         PIC X(10).                                   
002300     03 DATUM                PIC X(6).                                    
002400*** END OF VILMAII-COPY LENGTH= 82 BYTES                                  
