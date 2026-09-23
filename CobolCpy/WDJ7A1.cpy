000100 01  SEQA-WDJ7A1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEKUNDÄR INGÅNG PLATS                   
000400*                                 FYSISK NKL : WDJ7A1KY                   
000500*                                 (IDDC ADLAGOMR ADGANG ADPLATS           
000600*                                  IDARTNR)                               
000700*                                 SEKUNDARY NKL: WDJ7ASEQ                 
000800*                                 (IDDC ADLAGOMR ADGANG ADPLATS)          
000900     03 SEQA-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 SEQA-ADLAGOMR        PIC 9(2).                                    
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500     03 SEQA-ADGANG          PIC 9(2).                                    
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800     03 SEQA-ADPLATS         PIC 9(5).                                    
001900*                                 LAGERPLATSNUMMER                        
002000*                                 LOCATION                                
002100     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
