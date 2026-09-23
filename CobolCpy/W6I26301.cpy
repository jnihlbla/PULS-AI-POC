000100 01  MID-W6I26301.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-ADLAGOMR         PIC X(2).                                    
000800*                                 LAGEROMRÅDE                             
000900     03 MID-ADGANG           PIC X(2).                                    
001000*                                 GÅNG                                    
001100     03 MID-ADPLATS          PIC X(5).                                    
001200*                                 LAGERPLATSNUMMER                        
001300     03 MID-VKART            PIC X(7).                                    
001400*                                 ARTIKELVIKT (G)                         
001500     03 MID-VLARTNTO         PIC X(9).                                    
001600*                                 ARTIKELVOLYM NETTO (CM3)                
001700     03 MID-VLARTNTO-NUM REDEFINES MID-VLARTNTO                           
001800                             PIC 9(8)V9(1).                               
001900*                                 ARTIKELVOLYM NETTO (CM3)                
002000     03 MID-KDVSOP           PIC X(3).                                    
002100*                                 VSOP-KOD                                
002200     03 MID-KDSPEEMB         PIC X.                                       
002300*                                 SPECIALEMBALLAGEKOD                     
002400     03 MID-IDARTNR-EMBQ3    PIC X(9).                                    
002500*                                 EMBALLAGEARTIKELNR FÖR Q3               
002600     03 MID-IDARTNR-EMBQ4    PIC X(9).                                    
002700*                                 EMBALLAGEARTIKELNR FÖR Q4               
002800     03 MID-ADINLOMR-PRT     PIC X(4).                                    
002900*                                 PRINTERPLACERING                        
