000100 01  REQU-W60126I1-CTX.                                                   
000200*                                 INDATA F÷R PROGRAM W60126               
000300*                                 PROGRAMMET VISAR PLACERINGS-            
000400*                                 HISTORIK F÷R INLEVERANS                 
000500     03 REQU-IDLOPNRM-KEY    PIC 9(8).                                    
000600*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
000700*                                 (0VVDLLLLK)                             
000800*                                 SERIAL NO RECEIVING REPORT              
000900*                                 (0WWDLLLLC)                             
001000     03 REQU-IDRADNR-KEY     PIC 9(4).                                    
001100*                                 RADNUMMER                               
001200*                                 LINE NO                                 
001300     03 REQU-IDRADNR-START   PIC 9(4).                                    
001400*                                 RADNUMMER                               
001500*                                 LINE NO                                 
001600     03 REQU-DAREGDAT-START  PIC 9(8).                                    
001700*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001800*                                 REGISTRATION DATE (YYYYMMDD)            
001900     03 REQU-TIKLOCK-START   PIC 9(8).                                    
002000*                                 KLOCKSLAG (TTMMSSTH)                    
002100*                                 TIME OF DAY (HHMMSSTH)                  
002200     03 REQU-IDSPRAK         PIC X(2).                                    
002300*                                 2-STƒLLIG ISO SPR≈KKOD                  
002400*                                 2-LETTER ISO LANGUAGE CODE              
002500*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
