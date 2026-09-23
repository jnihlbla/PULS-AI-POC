000100 01  TOTAL-W40376.                                                        
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
002000     03 TOTAL-TIPRTDAT       PIC 9(6).                                    
002100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002200     03 TOTAL-TIPRTTID       PIC 9(4).                                    
002300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002400     03 TOTAL-ANTAL-TEXT     PIC X(6).                                    
002500     03 TOTAL-PICKUP-PRC-PART                                             
002600                             OCCURS 8 TIMES.                              
002700        05 TOTAL-ADLAGOMR    PIC Z(3).                                    
002800*                                 LAGEROMR≈DE                             
002900        05 TOTAL-KVANTAL     PIC Z(3).                                    
003000        05 TOTAL-KOMMA-TEXT  PIC X(2).                                    
003100     03 TOTAL-FILLER         PIC X(27).                                   
003200*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
