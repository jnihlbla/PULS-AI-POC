000100 01  W211R40.                                                             
000200*                                 POSTTYP R40                             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 KDSORT2              PIC S9(3)           COMP-3.                  
000600*                                 SORTERINGSFÄLT                          
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KVANTAL              PIC S9(7)           COMP-3.                  
001000*                                 ANTAL                                   
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 FLUPPBR              PIC S9              COMP-3.                  
001400*                                 BESTÄLLNINGSREST UPPDATERAS?            
001500*                                 (1 = JA)                                
001600     03 IDORDNR              PIC S9(7)           COMP-3.                  
001700*                                 ORDERNR             IDORDNR-002         
001800     03 FILLER               PIC X(17).                                   
001900*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
