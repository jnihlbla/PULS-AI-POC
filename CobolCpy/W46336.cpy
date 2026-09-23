000100 01  W46336.                                                              
000200*                                 PACKNINGSTRANS SOFTWARE PIE             
000300*                                                                         
000400     03 IDANSTNR             PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600*                                 IDENTIFICATION NO EMPLOYEE              
000700     03 IDDISTR              PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 IDKUNDNR             PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300     03 IDORDNR              PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500*                                 ORDER NUMBER                            
001600     03 IDPRODNR             PIC X(7).                                    
001700*                                 PRODUKTIONSNUMMER                       
001800*                                 PRODUCTION NUMBER                       
001900     03 IDDC                 PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 IDFAKT-GNB           PIC X(8).                                    
002300*                                 FAKTURANUMMER GNB                       
002400*                                 INVOICE NO. GNB                         
002500     03 IDSNDNOD             PIC X(8).                                    
002600*                                 SÄNDANDE NODE IDENTITET                 
002700*                                 IDENTITY OF SENDING NODE                
002800     03 RAD                  OCCURS 15 TIMES.                             
002900        05 IDRADNR           PIC X(4).                                    
003000*                                 RADNUMMER                               
003100*                                 LINE NO                                 
003200        05 KVLEVART          PIC X(6).                                    
003300*                                 LEVERERAT ANTAL STYCK                   
003400*                                 DELIVERED QUANTITY                      
003500        05 IDKLIENT          PIC X(10).                                   
003600*                                 VADIS KLIENT                            
003700*                                 VADIS CLIENT                            
003800        05 IDARBREF          PIC X(10).                                   
003900*                                 ARBETSORDER VADIS                       
004000*                                 WORK ORDER VADIS                        
004100        05 IDBIL.                                                         
004200*                                 BILIDENTITET                            
004300*                                 CAR IDENTITY                            
004400           07 IDBILTYP       PIC X(3).                                    
004500*                                 BILTYP                                  
004600*                                 CAR TYPE                                
004700           07 TIAAAA         PIC X(4).                                    
004800*                                 ÅRTAL (ÅÅÅÅ)                            
004900*                                 YEAR  (YYYY)                            
005000           07 IDCHASSI-PIE   PIC X(6).                                    
005100*                                 CHASSINUMMER PIE                        
005200*                                 CHASSI NUMBER PIE                       
005300        05 IDVIN             PIC X(17).                                   
005400*                                 VIN ID FORDON                           
005500*                                 VEHICLE VIN ID                          
005600*** END OF VILMAII-COPY LENGTH= 945 BYTES                                 
