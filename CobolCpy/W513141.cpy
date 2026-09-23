000100 01  W513141.                                                             
000200*                                 COPYTEXT TILL MANAGEMNET WEEKLY         
000300*                                  REPORT PER AREA                        
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIAAVV               PIC 9(4).                                    
000900*                                 ÅR - VECKA  (ÅÅVV)                      
001000     03 ADLAGOMR             PIC X(5).                                    
001100     03 BETEXT               PIC X(5).                                    
001200     03 SUARTSTD-WEEK        PIC -Z(6)9.9(2).                             
001300*                                 SUMMA STANDARDPRIS RADVÄRDE             
001400     03 SUARTSTD-WEEKAVG     PIC -Z(6)9.9(2).                             
001500*                                 SUMMA STANDARDPRIS RADVÄRDE             
001600     03 SUARTSTD-TOT         PIC -Z(6)9.9(2).                             
001700*                                 SUMMA STANDARDPRIS RADVÄRDE             
001800     03 REDIFF               PIC -Z(3)9.9(2).                             
001900*                                 DIFFERENS UTTRYCKT I PROCENT            
002000     03 SUARTSTD-DIFF        PIC -Z(6)9.9(2).                             
002100*                                 SUMMA STANDARDPRIS RADVÄRDE             
002200     03 REDIFF-LYEAR         PIC -Z(3)9.9(2).                             
002300*                                 DIFFERENS UTTRYCKT I PROCENT            
002400     03 IDLANDX2             PIC X(2).                                    
002500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002600     03 ADCITY               PIC X(20).                                   
002700*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
