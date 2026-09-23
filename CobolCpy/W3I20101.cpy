000100 01  MID-W3I20101.                                                        
000200*                                 MID-COPYTEXT F÷R W3020100               
000300     03 MID-IDFSGURV-IN      PIC X(8).                                    
000400*                                 URVALS IDENTITET                        
000500     03 MID-IDUSER-IN        PIC X(8).                                    
000600*                                 ANVƒNDARENS SƒKERHETS ID                
000700     03 MID-IDFSGURV-UT      PIC X(8).                                    
000800*                                 URVALS IDENTITET                        
000900     03 MID-IDUSER-UT        PIC X(8).                                    
001000*                                 ANVƒNDARENS SƒKERHETS ID                
001100     03 MID-IDTRANS-LO       PIC X(4).                                    
001200*                                 BILDNUMMER                              
001300     03 MID-IDTRANS-HI       PIC X(4).                                    
001400*                                 BILDNUMMER                              
001500     03 MID-DAREGDAT-LO      PIC 9(8).                                    
001600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001700     03 MID-DAREGDAT-HI      PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001900     03 MID-TIREGTID-LO      PIC 9(6).                                    
002000*                                 REGISTRERINGSTID                        
002100     03 MID-TIREGTID-HI      PIC 9(6).                                    
002200*                                 REGISTRERINGSTID                        
002300     03 MID-IDFSGURV-LO      PIC X(8).                                    
002400*                                 URVALS IDENTITET                        
002500     03 MID-IDFSGURV-HI      PIC X(8).                                    
002600*                                 URVALS IDENTITET                        
002700     03 MID-INFO-RAD         OCCURS 28 TIMES.                             
002800*                                 RADINFORMATION                          
002900        05 MID-SELECT-URVAL  PIC X.                                       
003000        05 MID-IDFSGURV-VISA PIC X(8).                                    
003100*                                 URVALS IDENTITET                        
003200        05 MID-DAREGDAT-VISA PIC 9(8).                                    
003300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003400        05 MID-TIREGTID-VISA PIC 9(6).                                    
003500*                                 REGISTRERINGSTID                        
003600        05 MID-IDTRANS-VISA  PIC X(4).                                    
003700*                                 BILDNUMMER                              
003800*** END OF VILMAII-COPY LENGTH= 840 BYTES                                 
