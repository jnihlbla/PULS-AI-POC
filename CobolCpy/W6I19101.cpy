000100 01  MID-W6I19101.                                                        
000200*                                 MIDCOPYTEXT TILL W60191.                
000300     03 MID-IDPGM            PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 MID-KVPOST           PIC 9(7).                                    
000700*                                 RÄKNARE, ANTAL POSTER                   
000800*                                 RECORD COUNTER                          
000900     03 MID-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MID-UPPFOELJN-POST   OCCURS 24 TIMES.                             
001300        05 MID-IDLOPNRM      PIC 9(9).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600*                                 SERIAL NO RECEIVING REPORT              
001700*                                 (0WWDLLLLC)                             
001800        05 MID-IDRADNR       PIC 9(5).                                    
001900*                                 RADNUMMER                               
002000*                                 LINE NO                                 
002100        05 MID-KDINLPRIO     PIC 9(2).                                    
002200*                                 PRIORITETSGRUPP                         
002300*                                 PRIORITY GROUP                          
002400        05 MID-PRARTSTD      PIC 9(7)V9(2).                               
002500*                                 ARTIKELSTANDARDPRIS                     
002600*                                 STANDARD PRICE                          
002700        05 MID-KVKOLLI       PIC 9(4).                                    
002800*                                 ANTAL KOLLI                             
002900*                                 NBR OF CASES                            
003000        05 MID-FLINLI        PIC X.                                       
003100*                                 INLAGD RAD, PARTI ELLER KOLLI           
003200*                                 STORED  LINE                            
003300        05 MID-ADINLOMR-OLD  PIC X(4).                                    
003400*                                 INLEVERANSOMRÅDE                        
003500*                                 RECEIVING AREA                          
003600        05 MID-ADINLOMR-NXT-OLD                                           
003700                             PIC X(4).                                    
003800*                                 INLEVERANSOMRÅDE                        
003900*                                 RECEIVING AREA                          
004000        05 MID-KDINLSTA-OLD  PIC X(3).                                    
004100*                                 SYSTEMSTATUS INLEVERANS                 
004200*                                 SYSTEM STATUS RECEIVING                 
004300        05 MID-KVINLART-OLD  PIC S9(6).                                   
004400*                                 ANTAL I PARTIRAD                        
004500*                                 QTY/LINE IN A LOT                       
004600        05 MID-ADINLOMR-NEW  PIC X(4).                                    
004700*                                 INLEVERANSOMRÅDE                        
004800*                                 RECEIVING AREA                          
004900        05 MID-ADINLOMR-NXT-NEW                                           
005000                             PIC X(4).                                    
005100*                                 INLEVERANSOMRÅDE                        
005200*                                 RECEIVING AREA                          
005300        05 MID-KDINLSTA-NEW  PIC X(3).                                    
005400*                                 SYSTEMSTATUS INLEVERANS                 
005500*                                 SYSTEM STATUS RECEIVING                 
005600        05 MID-KVINLART-NEW  PIC S9(6).                                   
005700*                                 ANTAL I PARTIRAD                        
005800*                                 QTY/LINE IN A LOT                       
005900*** END OF VILMAII-COPY LENGTH= 1553 BYTES                                
