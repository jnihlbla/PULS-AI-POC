000100 01  PRC-WDB612.                                                          
000200*                                 DC STYRREGISTER                         
000300*                                 PRC STARTKOLLINUMMER                    
000400*                                 FYSISK NYCKEL: IDPRC                    
000500     03 PRC-IDPRC.                                                        
000600*                                 PRODUKTIONSKANAL                        
000700*                                 PRODUCTION CHANNEL                      
000800        05 PRC-IDPRCBAS      PIC X(3).                                    
000900*                                 PRC-BAS                                 
001000*                                 PRC-BASIC                               
001100        05 PRC-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300*                                 PRC-VARIANT                             
001400     03 PRC-IDKOLLI-PRCSTA   PIC S9(5)           COMP-3.                  
001500*                                 STARTKOLLINUMMER PER DC/PRC             
001600*                                 START CASE NO PER DC/PRC                
001700     03 PRC-KDKOLLI          PIC X(8).                                    
001800*                                 KOLLIKOD                                
001900*                                 KOLLI CODE                              
002000*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
