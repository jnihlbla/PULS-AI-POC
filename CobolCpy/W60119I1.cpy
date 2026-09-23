000100 01  REQU-W60119I1.                                                       
000200*                                 COPYTEXT FÖR REQU TILL W60119/W         
000300*                                 6W119                                   
000400*                                 W60119I1                                
000500     03 REQU-IDLOPNRM-KEY    PIC X(8).                                    
000600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000700*                                 (0VVDLLLLK)                             
000800*                                 SERIAL NO RECEIVING REPORT              
000900*                                 (0WWDLLLLC)                             
001000     03 REQU-IDDC-KEY        PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 REQU-INPUT.                                                       
001400*                                 INDATA FÖR UPPDATERING                  
001500        05 REQU-FLMAK        PIC X.                                       
001600*                                 ALLMÄN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800        05 REQU-FLBACK       PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
