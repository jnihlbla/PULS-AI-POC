000100 01  MID-W6I16901.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 INDATA FÖR UPPDATERING                  
000900        05 MID-VKART-NTO     PIC 9(8).                                    
001000*                                 ARTIKELNS NETTOVIKT                     
001100        05 MID-VKART-BTO     PIC 9(8).                                    
001200*                                 ART BRUTTOVIKT MASKINELLT               
001300        05 MID-VLARTNTO      PIC X(9).                                    
001400*                                 ARTIKELVOLYM (CM3)                      
001500        05 MID-VLARTNTO-NUM REDEFINES MID-VLARTNTO                        
001600                             PIC 9(8)V9(1).                               
001700*                                 ARTIKELVOLYM (CM3)                      
001800        05 MID-KDVSOP        PIC 9(3).                                    
001900*                                 VSOP-KOD                                
002000*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
