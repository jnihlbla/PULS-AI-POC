000100 01  RAB-WDC213.                                                          
000200*                                 PRIS-RABATT REGISTER                    
000300*                                 RABATTINFORMATION                       
000400*                                 FYSISK NYCKEL: DASTADAT                 
000500     03 RAB-DASTADAT         PIC 9(8).                                    
000600*                                 GENERELLT STARTDATUM                    
000700*                                 GENERAL START DATE                      
000800     03 RAB-RABATT           OCCURS 99 TIMES.                             
000900        05 RAB-KDARTRAB      PIC 9(2).                                    
001000*                                 RABATTKOD (ARTIKELPRIS)                 
001100*                                 PURCHASE DISCOUNT CODE                  
001200        05 RAB-REARTRAB-DO   PIC S9(2)V9(2)      COMP-3.                  
001300*                                 ARTIKELRABATT DAGORDER                  
001400*                                 PARTS DISCOUNT DAYORDER                 
001500        05 RAB-REARTRAB-BULK PIC S9(2)V9(2)      COMP-3.                  
001600*                                 ARTIKELRABATT BULKORDER                 
001700*                                 PARTS DISCOUNT STOCKORDER               
001800*** END OF VILMAII-COPY LENGTH= 800 BYTES                                 
