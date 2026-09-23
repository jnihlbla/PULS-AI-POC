000100 01  SRP-WDC311.                                                          
000200*                                 SUGGESTED RETAIL PRICES                 
000300*                                 PER SALES COMPANY                       
000400*                                 PRICE PER COUNTRY                       
000500*                                 FYSISK NYCKEL: IDLANDX2                 
000600     03 SRP-IDLANDX2         PIC X(2).                                    
000700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000800*                                 2-LETTER CODE FOR COUNTRY               
000900     03 SRP-KDVALISO         PIC X(3).                                    
001000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001100*                                 CURRENCY CODE BY ISO-STANDARD.          
001200     03 SRP-PRARTBTO-SC      PIC S9(9)V9(2)      COMP-3.                  
001300*                                 BRUTTOPRIS PER SÄLJBOLAG                
001400*                                 SUGGESTED RETAIL PER SALE COMP          
001500     03 SRP-TIUPPDAT         PIC S9(7)           COMP-3.                  
001600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001700*                                 UPDATING DATE     (YYMMDD)              
001800*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
