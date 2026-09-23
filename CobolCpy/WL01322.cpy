000100 01  TOTAL-WL01322.                                                       
000200*                                 COPYTEXT FOR PICKING LABEL LDC          
000300*                                 TOTAL                                   
000400     03 TOTAL-IDAFPRCD       PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 TOTAL-IDLOPNR-ORD    PIC 9(3).                                    
000700*                                 ORDERNS ORDNINGSNUMMER INOM             
000800*                                 EN PLOCKSATS                            
000900     03 TOTAL-IDLOPNR-PL     PIC 9(3).                                    
001000*                                 PLOCKSATSENS L÷PNUMMER INOM             
001100*                                 PRC-GRUPP                               
001200     03 TOTAL-IDPRC.                                                      
001300*                                 PRODUKTIONSKANAL                        
001400        05 TOTAL-IDPRCBAS    PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600        05 TOTAL-IDPRCVAR    PIC X.                                       
001700*                                 PRC-VARIANT                             
001800     03 TOTAL-KVRADER        PIC Z(4)9.                                   
001900*                                 ANTAL RADER                             
002000     03 TOTAL-IDTRPTNR       PIC Z(2)9.                                   
002100*                                 TRANSPORTIDENTITET                      
002200     03 TOTAL-TIRFSDAT       PIC 9(6).                                    
002300*                                 KLART F÷R TRANSPORT ≈≈MMDD              
002400     03 TOTAL-TIRFSTID       PIC 9(4).                                    
002500*                                 KLART F÷R TRANSPORT (TTMM)              
002600*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
