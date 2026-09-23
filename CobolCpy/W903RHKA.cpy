000100 01  RHKA-W903RHK.                                                        
000200*                                 TILLÄGG ORDERRADER VDI                  
000300*                                 POSTTYP = RHK                           
000400     03 RHKA-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RHKA-KDORDKL         PIC 9.                                       
000700*                                 ORDERKLASS                              
000800     03 RHKA-BEKUNDRF        PIC X(10).                                   
000900*                                 KUNDENS REFERENS                        
001000     03 RHKA-RADER           OCCURS 100 TIMES.                            
001100*                                 ORDERRADER VDI                          
001200        05 RHKA-IDARTNR      PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 RHKA-KVBEART      PIC 9(6).                                    
001500*                                 BESTÄLLT ANTAL STYCKEN                  
001600        05 RHKA-FLSLATT      PIC X.                                       
001700*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
001800*                                 LL BERÄKNAS ELLER EJ                    
001900*                                 OM FLRESTN = J OCH FLSLATT = J,         
002000*                                  DÅ BERÄKNAS KVSLATT                    
002100        05 RHKA-KDKVBRYT     PIC X.                                       
002200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002300        05 RHKA-BERADREF     PIC X(10).                                   
002400*                                 KUNDENS RADREFERENS                     
002500        05 RHKA-KDDSP        PIC X.                                       
002600*                                 PÅVERKAN PÅ DSP                         
002700*** END OF VILMAII-COPY LENGTH= 2816 BYTES                                
