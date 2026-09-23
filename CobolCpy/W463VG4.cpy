000100 01  W463VG4-CTX.                                                         
000200*                                 TYP = VG4 PIE FAKTURATRANS              
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 KDSOFT               PIC X.                                       
000700*                                 0 NORMAL ORDER                          
000800*                                 1 VCEM SOFTWARE ORDER                   
000900*                                 2 VADIS SOFTWARE ORDER                  
001000*                                 3 OTHER SOFTWARE ORDER                  
001100     03 IDDISTR              PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 IDKUNDNR             PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 IDORDNR7             PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 IDPRODNR             PIC 9(7).                                    
001800*                                 PRODUKTIONSNUMMER                       
001900     03 IDARBREF             PIC X(10).                                   
002000*                                 ARBETSORDER VADIS                       
002100     03 IDRADNR              PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300     03 IDARTPRE             PIC X(3).                                    
002400*                                 IDENTIFIERARE ARTIKELSORTIMENT          
002500     03 IDARTBET             PIC X(17).                                   
002600*                                 ARTIKELBETECKNING EFTERMARKNAD          
002700     03 KVLEVART             PIC 9(6).                                    
002800*                                 LEVERERAT ANTAL STYCK                   
002900     03 IDKLIENT             PIC X(10).                                   
003000*                                 VADIS KLIENT                            
003100     03 IDBIL.                                                            
003200*                                 BILIDENTITET                            
003300        05 IDBILTYP          PIC X(3).                                    
003400*                                 BILTYP                                  
003500        05 TIAAAA-PIE        PIC X(4).                                    
003600*                                 ≈RTAL (≈≈≈≈)                            
003700        05 IDCHASSI-PIE      PIC X(6).                                    
003800*                                 CHASSINUMMER PIE                        
003900     03 IDVIN                PIC X(17).                                   
004000*                                 VIN ID FORDON                           
004100     03 IDPIERAD             PIC X(20).                                   
004200*                                 PIE ORDERAD NR/REFERENS                 
004300*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
