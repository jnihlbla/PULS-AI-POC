000100 01  W224LI12-CTX.                                                        
000200*                                 POST FÖR OMSPEC AV LEVERANSPLAN         
000300*                                 KINA LOKAL ANSKAFFNING                  
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 KDERS                PIC S9(3)           COMP-3.                  
001000*                                 ERSÄTTNINGSKOD                          
001100     03 KDLPORS-TAB          OCCURS 3 TIMES                               
001200                             PIC S9(3)           COMP-3.                  
001300*                                 LEVERANSPLANEORSAK                      
001400     03 TIFINLV              PIC S9(5)           COMP-3.                  
001500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001600     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
001700*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001800*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
