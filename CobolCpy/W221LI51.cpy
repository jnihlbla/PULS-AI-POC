000100 01  W221LI51.                                                            
000200*                                 POST FÖR FRAMSTÄLLNING AV               
000300*                                 LEVERANSPLANEKONCEPT                    
000400*                                                                         
000500     03 IDANSK               PIC S9(3)           COMP-3.                  
000600*                                 ANSKAFFARNUMMER                         
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 TIOMSPEC-FOREG       PIC S9(5)           COMP-3.                  
001200*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
001300     03 KDLPORS-TAB          OCCURS 3 TIMES                               
001400                             PIC S9(3)           COMP-3.                  
001500*                                 LEVERANSPLANEORSAK                      
001600     03 FLLEVPLA             PIC X.                                       
001700*                                 FLAGGA MÖJLIG AUTOMAT LEVPLAN           
001800     03 TELPORS              PIC X(13).                                   
001900*                                 LEVERANSPLANEORSAK                      
002000*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
