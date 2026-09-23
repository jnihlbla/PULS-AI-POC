000100 01  W612762.                                                             
000200*                                 COPYTEXT TILL MANAGEMENT REFILL         
000300*                                  FOLLOW UP WEEK FOR WEBB LIST           
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
002000*                                 ÅR - VECKA  (ÅÅVV)                      
002100     03 IDLANDX2             PIC X(2).                                    
002200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002300     03 ADCITY               PIC X(25).                                   
002400*                                 BENÄMNING PÅ STAD                       
002500     03 SUARTNTO-BINNED      PIC Z(7)9.9(2).                              
002600*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
002700     03 KDREFTYP             PIC X.                                       
002800*                                 TYP AV REFILLORDER                      
002900*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
