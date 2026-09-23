000100 01  EXCH-W411EXCH.                                                       
000200*                                 LÄNKAREA TILL W411EXCH -                
000300*                                 OMRÄKNING AV VALUTA DDI                 
000400     03 EXCH-INDATA.                                                      
000500        05 EXCH-PRKURS       PIC S9(6)V9(5)      COMP-3.                  
000600*                                 VALUTAKURS                              
000700        05 EXCH-SUORDV-IN    PIC S9(9)V9(2)      COMP-3.                  
000800*                                 SUMMA ORDERVÄRDE                        
000900        05 EXCH-PRARTNTO-IN  PIC S9(7)V9(2)      COMP-3.                  
001000*                                 ARTIKELPRIS NETTO                       
001100        05 EXCH-KDCALL       PIC S9(3)           COMP-3.                  
001200*                                 ANROPSTYP                               
001300     03 EXCH-UTDATA.                                                      
001400        05 EXCH-SUORDV-UT    PIC S9(9)V9(2)      COMP-3.                  
001500*                                 SUMMA ORDERVÄRDE                        
001600        05 EXCH-PRARTNTO-UT  PIC S9(7)V9(2)      COMP-3.                  
001700*                                 ARTIKELPRIS NETTO                       
001800*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
