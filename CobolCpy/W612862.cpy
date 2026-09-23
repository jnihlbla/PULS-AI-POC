000100 01  W612862.                                                             
000200*                                 COPYTEXT TILL MANAGEMENT REFILL         
000300*                                  FOLLOW UP PERIOD FOR WEBB LIST         
000400*                                 MAN-REF-AK-PER                          
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
002000*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
002100*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
002200     03 IDLANDX2             PIC X(2).                                    
002300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002400     03 ADCITY               PIC X(25).                                   
002500*                                 BENÄMNING PÅ STAD                       
002600     03 SUARTNTO-BINNED      PIC Z(7)9.9(2).                              
002700*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
002800     03 KDREFTYP             PIC X.                                       
002900*                                 TYP AV REFILLORDER                      
003000*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
