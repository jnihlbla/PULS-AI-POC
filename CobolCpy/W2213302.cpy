000100 01  W2213302-CTX.                                                        
000200*                                 CALL OFF INFO TO TMS - DEMAND           
000300*                                                                         
000400     03 IDPTYP-015           PIC X(15).                                   
000500*                                 POSTTYP              IDPTYP-015         
000600     03 IDVTYP               PIC X(2).                                    
000700*                                 POSTTYPSVERSION                         
000800     03 KVAVROP              PIC 9(7).9(2).                               
000900*                                 AVROPSKVANTITET                         
001000     03 DAAVROP-STA          PIC 9(12).                                   
001100*                                 AVHƒMTNING DAT+TIDIGASTE TIDEN          
001200*                                 (≈≈≈≈MMDD0000)                          
001300     03 DAAVROP-STO          PIC 9(12).                                   
001400*                                 AVHƒMTNINGS DAT+SENASTE TIDEN           
001500*                                 (≈≈≈≈MMDD2359)                          
001600     03 DASNDTID-STA         PIC 9(12).                                   
001700*                                 AVSƒNDNINGS DAT+TIDIGASTE TIDEN         
001800*                                 (≈≈≈≈MMDD0000)                          
001900     03 DASNDTID-STO         PIC 9(12).                                   
002000*                                 AVSƒNDNINGS DAT+SENASTE TIDEN           
002100*                                 (≈≈≈≈MMDD0000)                          
002200*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
