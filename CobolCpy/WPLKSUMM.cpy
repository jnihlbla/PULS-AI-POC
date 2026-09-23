000100 01  PLK-SUMM-WPLKSUMM.                                                   
000200*                                 COPYTEXT FOR PICKING LABEL SUMM         
000300*                                 ARY LINE                                
000400     03 PLK-SUMM-IDAFPRCD-TOT                                             
000500                             PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700     03 PLK-SUMM-IDLOPNR-ORD-TOT                                          
000800                             PIC 9(3).                                    
000900*                                 ORDERNS ORDNINGSNUMMER INOM             
001000*                                 EN PLOCKSATS                            
001100     03 PLK-SUMM-IDLOPNR-PL-TOT                                           
001200                             PIC 9(3).                                    
001300*                                 PLOCKSATSENS L÷PNUMMER PER PRC          
001400     03 PLK-SUMM-IDPRC-TOT.                                               
001500*                                 PRODUKTIONSKANAL                        
001600        05 PLK-SUMM-IDPRCBAS PIC X(3).                                    
001700*                                 PRC-BAS                                 
001800        05 PLK-SUMM-IDPRCVAR PIC X.                                       
001900*                                 PRC-VARIANT                             
002000     03 PLK-SUMM-KVRADER     PIC 9(5).                                    
002100*                                 ANTAL RADER                             
002200     03 PLK-SUMM-IDTRPTNR    PIC 9(3).                                    
002300*                                 TRANSPORTIDENTITET                      
002400     03 PLK-SUMM-TIRFSDAT    PIC 9(6).                                    
002500*                                 KLART F÷R TRANSPORT ≈≈MMDD              
002600     03 PLK-SUMM-TIRFSTID    PIC 9(4).                                    
002700*                                 KLART F÷R TRANSPORT (TTMM)              
002800*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
