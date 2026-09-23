000100 01  W61263.                                                              
000200*                                 PASSERADE  LEVERANSER                   
000300*                                 FRÅN CDC                                
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 KVAVIS               PIC S9(7)           COMP-3.                  
001400*                                 AVISERAT ANTAL                          
001500     03 TIBERANK             PIC 9(6).                                    
001600*                                 BERÄKNAD ANKOMSTDATUM                   
001700*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
