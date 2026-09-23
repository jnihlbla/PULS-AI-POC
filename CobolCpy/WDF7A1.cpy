000100 01  SEQA-WDF7A1.                                                         
000200*                                 CROSS-REF FORD-VCC                      
000300*                                 SEKUNDÄRT INDEX TILL WDF701             
000400*                                 FYSISK NYCKEL: WDF7A1KY:                
000500*                                 (IDARTFMC+FLGEMFMC+TIDATIME9+)          
000600*                                  + IDARTNR                   )          
000700*                                 SEKUNDÄR NYCKEL: WDF7ASEQ               
000800*                                 (IDARTFMC)                              
000900     03 SEQA-IDARTFMC        PIC X(22).                                   
001000*                                 ARTIKELNR FORD KONSTR.                  
001100     03 SEQA-FLGEMFMC        PIC X.                                       
001200*                                 GEMENSAM FORD/MPNR ARTIKEL              
001300     03 SEQA-TIDATETIME-9KOMPL                                            
001400                             PIC X(14).                                   
001500*                                 DATUMTIDS 9-KOMPLEMENT                  
001600     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 SEQA-IDPRTNER        PIC S9(5)           COMP-3.                  
001900*                                 PARTNER-ID  P.A.G.                      
002000*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
