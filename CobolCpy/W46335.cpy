000100 01  W46335.                                                              
000200*                                 DIRECT DELIVERIES, PACKING TRAN         
000300*                                 S                                       
000400*                                 FROM PIE TO RETAILER                    
000500*                                                                         
000600*                                                                         
000700     03 IDPRODNR             PIC 9(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900*                                 PRODUCTION NUMBER                       
001000     03 IDDISTR              PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 IDKUNDNR             PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 IDORDNR7             PIC 9(7).                                    
001700*                                 ORDERNUMMER                             
001800*                                 ORDER NUMBER                            
001900     03 IDRADNR              PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100*                                 LINE NO                                 
002200     03 IDARTPRE             PIC X(3).                                    
002300*                                 IDENTIFIERARE ARTIKELSORTIMENT          
002400*                                 PARTS RANGE IDENTIFIER                  
002500     03 IDARTBET             PIC X(17).                                   
002600*                                 ARTIKELBETECKNING EFTERMARKNAD          
002700*                                 AFTERMARKET PARTNUMBER                  
002800     03 KVLEVART             PIC 9(7).                                    
002900*                                 LEVERERAT ANTAL STYCK                   
003000*                                 DELIVERED QUANTITY                      
003100     03 IDKLIENT             PIC X(10).                                   
003200*                                 VADIS KLIENT                            
003300*                                 VADIS CLIENT                            
003400     03 IDARBREF             PIC X(10).                                   
003500*                                 ARBETSORDER VADIS                       
003600*                                 WORK ORDER VADIS                        
003700     03 IDBIL.                                                            
003800*                                 BILIDENTITET                            
003900*                                 CAR IDENTITY                            
004000        05 IDBILTYP          PIC X(3).                                    
004100*                                 BILTYP                                  
004200*                                 CAR TYPE                                
004300        05 TIAAAA            PIC X(4).                                    
004400*                                 ≈RTAL (≈≈≈≈)                            
004500*                                 YEAR  (YYYY)                            
004600        05 IDCHASSI-PIE      PIC X(6).                                    
004700*                                 CHASSINUMMER PIE                        
004800*                                 CHASSI NUMBER PIE                       
004900     03 IDVIN                PIC X(17).                                   
005000*                                 VIN ID FORDON                           
005100*                                 VEHICLE VIN ID                          
005200*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
