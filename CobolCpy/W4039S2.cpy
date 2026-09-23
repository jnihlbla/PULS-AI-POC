000100 01  MID2-W4I39902.                                                       
000200*                                 KOMMENTAR (SVENSKA)                     
000300*                                 31 POS / RAD                            
000400     03 MID2-IDANSTNR        PIC X(5).                                    
000500*                                 ANSTƒLLNINGSNUMMER                      
000600     03 MID2-IDDISTR         PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID2-IDKUNDNR        PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID2-IDORDNR         PIC X(5).                                    
001100*                                 ORDERNUMMER UTG≈R PD90                  
001200     03 MID2-IDPRODNR        PIC X(7).                                    
001300*                                 PRODUKTIONSNUMMER                       
001400     03 MID2-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID2-IDFAKT-GNB      PIC X(8).                                    
001700*                                 FAKTURANUMMER GNB                       
001800     03 MID2-IDSHIPM         PIC 9(7).                                    
001900*                                 SKEPPNINGSNUMMER                        
002000     03 MID2-RAD             OCCURS 15 TIMES.                             
002100        05 MID2-IDRADNR      PIC X(4).                                    
002200*                                 RADNUMMER                               
002300        05 MID2-KVLEVART     PIC X(6).                                    
002400*                                 LEVERERAT ANTAL STYCK                   
002500        05 MID2-IDKLIENT     PIC X(10).                                   
002600*                                 VADIS KLIENT                            
002700        05 MID2-IDARBREF     PIC X(10).                                   
002800*                                 ARBETSORDER VADIS                       
002900        05 MID2-IDBIL.                                                    
003000*                                 BILIDENTITET                            
003100           07 MID2-IDBILTYP  PIC X(3).                                    
003200*                                 BILTYP                                  
003300           07 MID2-TIAAAA    PIC X(4).                                    
003400*                                 ≈RTAL (≈≈≈≈)                            
003500           07 MID2-IDCHASSI-PIE                                           
003600                             PIC X(6).                                    
003700*                                 CHASSINUMMER PIE                        
003800        05 MID2-IDVIN        PIC X(17).                                   
003900*                                 VIN ID FORDON                           
004000*** END OF VILMAII-COPY LENGTH= 944 BYTES                                 
