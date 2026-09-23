000100 01  W61276.                                                              
000200*                                 COPYTEXT TILL REFILL FOLLOW UP          
000300*                                 WEEK FOR WEBB LIST                      
000400*                                 REFILL-AK-WEEK                          
000500     03 IDAFPRCD             PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 AK-LINES             PIC Z(5)9.                                   
001000*                                 ANTAL                                   
001100     03 BINNED-LINES         PIC Z(5)9.                                   
001200*                                 ANTAL                                   
001300     03 BINNED-PRIO          PIC Z(5)9.                                   
001400*                                 ANTAL                                   
001500     03 TIME-TOT             PIC Z(3)9.9.                                 
001600*                                 ANTAL DAGAR MED DECIMAL                 
001700     03 DAYS-PRIO            PIC Z(3)9.9.                                 
001800*                                 ANTAL DAGAR MED DECIMAL                 
001900     03 TIAAVV               PIC 9(4).                                    
002000*                                 ≈R - VECKA  (≈≈VV)                      
002100     03 KDREFTYP             PIC X.                                       
002200*                                 TYP AV REFILLORDER                      
002300*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
