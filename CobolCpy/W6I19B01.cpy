000100 01  MID-W6I19B01.                                                        
000200*                                 MIDCOPYTEXT TILL W6019B.                
000300     03 MID-IDARTNR          PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-ADLAGOMR         PIC X(2).                                    
000800*                                 LAGEROMRÅDE                             
000900     03 MID-ADGANG           PIC X(2).                                    
001000*                                 GÅNG                                    
001100     03 MID-ADPLATS          PIC X(5).                                    
001200*                                 LAGERPLATSNUMMER                        
001300     03 MID-BEFT             PIC X(2).                                    
001400*                                 FÖRPACKNINGSTYP                         
001500     03 MID-IDARTNR-EMBQ3    PIC X(9).                                    
001600*                                 EMBALLAGEARTIKELNR FÖR Q3               
001700     03 MID-KDARTURS         PIC X(2).                                    
001800*                                 ARTIKELURSPRUNGSKOD                     
001900     03 MID-PRARTSTD         PIC X(10).                                   
002000*                                 ARTIKELSTANDARDPRIS                     
002100     03 MID-VKART            PIC X(7).                                    
002200*                                 ARTIKELVIKT (G)                         
002300     03 MID-VLARTNTO         PIC X(9).                                    
002400*                                 ARTIKELVOLYM NETTO (CM3)                
002500     03 MID-CDPLATS          OCCURS 4 TIMES.                              
002600*                                                                         
002700        05 MID-ADGANG-CD     PIC X(2).                                    
002800*                                 GÅNG                                    
002900        05 MID-ADPLATS-CD    PIC X(5).                                    
003000*                                 LAGERPLATSNUMMER                        
003100*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
