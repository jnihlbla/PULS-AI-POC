000100 01  TMS-W403TMS1.                                                        
000200*                                 LÄNKAREA TILL W403TMS1 -                
000300*                                 SEND TMS EVENT TO API                   
000400*                                                                         
000500     03 TMS-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 TMS-IDDISTR          PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 TMS-IDKUNDNR         PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 TMS-IDORDNR7         PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 TMS-INDATA           OCCURS 99 TIMES.                             
001400*                                 INDATA TILL W403TMS1                    
001500        05 TMS-IDKOLLI       PIC 9(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 TMS-IDPRODNR         PIC S9(7)           COMP-3.                  
001800*                                 PRODUKTIONSNUMMER                       
001900*** END OF VILMAII-COPY LENGTH= 515 BYTES                                 
