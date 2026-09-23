000100 01  W4122A.                                                              
000200*                                 EXTRACT ORDERS WITH STATUS P            
000300*                                 WITH TODAYS RFS DATE AND HAVING         
000400*                                 REPAIR DATE FOR DISTRICT 778,           
000500*                                 DC 11                                   
000600*                                                                         
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDORDNR              PIC S9(5)           COMP-3.                  
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 KVLEVART             PIC S9(7)           COMP-3.                  
001600*                                 LEVERERAT ANTAL STYCK                   
001700     03 TIREPDAT             PIC S9(7)           COMP-3.                  
001800*                                 REPAIR DATE                             
001900     03 BERADREF             PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
