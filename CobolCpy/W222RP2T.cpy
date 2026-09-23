000100 01  W222RP2T.                                                            
000200*                                 UPPDATERING TREND                       
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC 9(8).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 KDCLAGER             PIC 9.                                       
000800*                                 CENTRALLAGERKOD                         
000900     03 KVTREND-X.                                                        
001000        05 KVTREND           PIC 9(6)V9(1).                               
001100*                                 TRENDANTAL                              
001200     03 FLNEGTR              PIC X.                                       
001300*                                 FLAGGA NEGATIV TREND                    
001400     03 TITREND-X.                                                        
001500        05 TITREND           PIC 9(4).                                    
001600*                                 STARTPERIOD TREND                       
001700     03 RVTREND-X.                                                        
001800        05 RVTREND           PIC 9(2).                                    
001900*                                 ÅTERSTÅENDE TRENDPERIODER               
002000     03 FLABORT-TREND        PIC X.                                       
002100*                                 BORTTAG AV BEFINTLIG TREND?             
002200*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
