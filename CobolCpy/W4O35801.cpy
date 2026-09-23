000100 01  MOD-W4O35801.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 PART NUMBER                             
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 PART NUMBER                             
001000     03 MOD-LINE.                                                         
001100        05 MOD-BEART-RAD     PIC X(25).                                   
001200*                                 PART DESCRIPTION                        
001300        05 MOD-TIHH.                                                      
001400           07 MOD-TIHH-RAD-ATTR                                           
001500                             PIC X(2).                                    
001600           07 MOD-TIHH-RAD   PIC 9(2).                                    
001700        05 MOD-TIMM.                                                      
001800           07 MOD-TIMM-RAD-ATTR                                           
001900                             PIC X(2).                                    
002000           07 MOD-TIMM-RAD   PIC 9(2).                                    
002100     03 MOD-TIHH-ATTR        PIC X(2).                                    
002200     03 MOD-TIHH-UPD         PIC X(2).                                    
002300*                                 MFS DISPOSITION OF INPUT FIELD          
002400     03 MOD-TIMM-ATTR        PIC X(2).                                    
002500     03 MOD-TIMM-UPD         PIC X(2).                                    
002600*                                 MFS DISPOSITION OF INPUT FIELD          
002700     03 MOD-TEMFSINF         PIC X(55).                                   
002800*                                 INFORMATION MESSAGE                     
002900*** END OF VILMAII-COPY LENGTH= 158 BYTES                                 
