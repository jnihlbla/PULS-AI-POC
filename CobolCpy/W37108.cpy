000100 01  AREA.                                                                
000200*                                 COPYTEXT FÖR FAKTURERING BYTES          
000300*                                                                         
000400     03 W37108.                                                           
000500        05 IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 IDDISTR           PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100        05 IDARTNR-OBJ       PIC S9(9)           COMP-3.                  
001200*                                 OBJEKTNUMMER                            
001300        05 IDORDNR           PIC S9(5)           COMP-3.                  
001400*                                 ORDERNUMMER                             
001500        05 KVANTAL           PIC S9(7)           COMP-3.                  
001600*                                 ANTAL ALLMÄNT                           
001700        05 SUPRIS            PIC S9(7)V9(2)      COMP-3.                  
001800*                                 RAD TOTAL                               
001900        05 TIFAKT            PIC S9(7)           COMP-3.                  
002000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002100*** END COPY W37108CCC0  LENGTH=31                                        
