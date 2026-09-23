000100 01  W4885601.                                                            
000200*                                 FIL MED INFO OM VILKA ARTIKLAR          
000300*                                 SOM FINNS PÅ BUFFERTREGISTRET           
000400*                                 MED BUFFERTOMRÅDE HL                    
000500*                                 OCH DESS LAGERPLATSER                   
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000900*                                 LAGEROMRÅDE                             
001000     03 ADGANG               PIC S9(3)           COMP-3.                  
001100*                                 GÅNG                                    
001200     03 ADPLATS              PIC S9(5)           COMP-3.                  
001300*                                 LAGERPLATSNUMMER                        
001400     03 KVLS-CDC             PIC S9(7)           COMP-3.                  
001500*                                 LAGERSALDO                              
001600     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
001700*                                 PERIODBEHOV (PROGNOS)                   
001800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
001900*                                 ARTIKELVOLYM NETTO (CM3)                
002000     03 ADINLOMR-BOA         PIC X(4).                                    
002100*                                 BUFFERTOMRÅDE-ALTERNATIVT               
002200     03 BEART                PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
