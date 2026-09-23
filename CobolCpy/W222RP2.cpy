000100 01  W222RP2.                                                             
000200*                                 UPPDATERING TREND                       
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 KDCLAGER             PIC S9              COMP-3.                  
000800*                                 CENTRALLAGERKOD                         
000900     03 KVTREND              PIC S9(6)V9(1)      COMP-3.                  
001000*                                 TRENDANTAL                              
001100     03 FLNEGTR              PIC X.                                       
001200*                                 FLAGGA NEGATIV TREND                    
001300     03 TITREND              PIC 9(4).                                    
001400*                                 STARTPERIOD TREND                       
001500     03 RVTREND              PIC S9(3)           COMP-3.                  
001600*                                 ÅTERSTÅENDE TRENDPERIODER               
001700     03 FLABORT-TREND        PIC X.                                       
001800*                                 BORTTAG AV BEFINTLIG TREND?             
001900*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
