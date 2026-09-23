000100 01  W612861.                                                             
000200*                                 COPYTEXT TILL REFILL FOLLOW UP          
000300*                                 PERIOD FOR WEBB LIST                    
000400*                                 REFILL-AK-PER                           
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
001900     03 TIAARP               PIC 9(4).                                    
002000*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002100*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
002200     03 KDREFTYP             PIC X.                                       
002300*                                 TYP AV REFILLORDER                      
002400*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
