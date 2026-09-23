000100 01  LEVPL-W2120303.                                                      
000200*                                 DATA FÖR FÖRÄNDRING AV                  
000300*                                 LEVERANSPLANER (INLB01/INLB11)          
000400*                                 NYUPPLÄGG    = PTYP NLE                 
000500*                                 UPPDATERING  = PTYP ULE                 
000600     03 LEVPL-IDPTYP         PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 LEVPL-IDARTNR        PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 LEVPL-IDLEVNR        PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 LEVPL-IDLEVNR-SHIP   PIC X(5).                                    
001300*                                 SKEPPANDE LEVERANTÖR                    
001400     03 LEVPL-KVBR           PIC S9(7)           COMP-3.                  
001500*                                 BESTÄLLNINGSREST                        
001600     03 LEVPL-TILEVPL        PIC S9(7)           COMP-3.                  
001700*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
001800*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
