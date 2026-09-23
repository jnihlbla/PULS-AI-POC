000100 01  W51371.                                                              
000200*                                 URVALSREGISTER FÖR INVENTERING          
000300*                                                                         
000400     03 IDURVAL-INV          PIC X.                                       
000500*                                 TYP AV URVAL FÖR INVENTERING            
000600*                                 1  = OMRÅDE 91, ALLA /ÅR                
000700*                                 2  = 10% AV DE BILLIGASTE /5 ÅR         
000800*                                 3  = 30% AV DE MED HÖGST PB /ÅR         
000900*                                 4  = 33% AV RESTEN /ÅR                  
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600     03 ADGANG               PIC S9(3)           COMP-3.                  
001700*                                 GÅNG                                    
001800     03 ADPLATS              PIC S9(5)           COMP-3.                  
001900*                                 LAGERPLATSNUMMER                        
002000     03 KDVVKL               PIC S9              COMP-3.                  
002100*                                 VOLYMVÄRDESKLASS                        
002200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002300*                                 PRODUKTSLAG                             
002400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002500*                                 FUNKTIONSGRUPP                          
002600*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
