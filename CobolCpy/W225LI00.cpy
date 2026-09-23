000100 01  W225LI00.                                                            
000200*                                 LISTRECORD FÖR SERVICE GRAD PER         
000300*                                 ANSKAFFARE URVALS-INFORMATION           
000400*                                                                         
000500     03 SORTARGUMENT.                                                     
000600        05 IDLISTA           PIC S9(3)           COMP-3.                  
000700*                                 LISTNUMMER                              
000800        05 IDANSK            PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000        05 IDLEVNR           PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200        05 IDARTNR           PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 URVALS-INFO.                                                      
001500        05 URVALSINFORMATION PIC X(293).                                  
001600*** END OF VILMAII-COPY LENGTH= 307 BYTES                                 
