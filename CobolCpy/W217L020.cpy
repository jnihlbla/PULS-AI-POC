000100 01  W217L020.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21702 MOT SUBPROGRAM                   
000400*                                                                         
000500     03 KDBEHAND             PIC X(4).                                    
000600     03 IDFELKODX            PIC X(3).                                    
000700*                                 FELKOD                                  
000800     03 IDANSK-FEL           PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 IDLEVNR-BAS          PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
001300*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001400     03 TIAAVV               PIC S9(5)           COMP-3.                  
001500*                                 ÅR - VECKA  (ÅÅVV)                      
001600     03 POSTAREA             PIC X(60).                                   
001700*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
