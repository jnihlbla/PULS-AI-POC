000100 01  W63120.                                                              
000200*                                 SRS - COPYTEXT TILL FIL W61320(         
000300*                                 W02008).                                
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 KDORDKL              PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700     03 REINKORD             PIC S9              COMP-3.                  
001800*                                 INKOMMEN ORDERRAD                       
001900     03 REFYSAVV             PIC S9V9(2)         COMP-3.                  
002000*                                 FYSISK AVVIKELSE                        
002100     03 REAVBRAD             PIC S9V9(2)         COMP-3.                  
002200*                                 AVBOKAD MÄNGD (DEL AV RAD)              
002300     03 RERORAD              PIC S9V9(2)         COMP-3.                  
002400*                                 RESTNOTERAD MÄNGD (DEL AV RAD)          
002500     03 KVBEART              PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700     03 KDMARK-SERV          PIC S9(3)           COMP-3.                  
002800*                                 MARKNADSKOD SERVICEGRADSBERÄKNI         
002900*                                 NG                                      
003000     03 FILLER               PIC X(4).                                    
003100*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
