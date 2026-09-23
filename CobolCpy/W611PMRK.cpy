000100 01  PMRK-W611PMRK.                                                       
000200*                                 LÄNKAREA TILL W611PMRK -                
000300*                                 PRIOMÄRKNING AV PARTI                   
000400*                                 KDSVAR = 1 BETYDER ATT BORT-            
000500*                                 TAG AV PRIO EJ SKETT.                   
000600*                                 KDSVAR = SPACE I ANDRA FALL             
000700*                                 KODEN AVSER DIVERSEKOLLI                
000800     03 PMRK-INDATA1.                                                     
000900*                                                                         
001000        05 PMRK-IDLEVNR      PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200        05 PMRK-IDOKOLLI     PIC 9(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400     03 PMRK-INDATA2.                                                     
001500*                                                                         
001600        05 PMRK-IDLOPNRM     PIC 9(8).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900        05 PMRK-IDRADNR      PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100     03 PMRK-KDSVAR          PIC X.                                       
002200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002300*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
