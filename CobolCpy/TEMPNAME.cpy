000100 01  002A-W016002A.                                                       
000200*                                 COPYTEXT FÖR VCOM-SÄNDNING              
000300     03 002A-IDVCOM.                                                      
000400*                                 VCOM IDENTITET                          
000500*                                 VCOM IDENTITY                           
000600        05 002A-VC-IDSYSTEM  PIC X(4).                                    
000700*                                 VOLVO VCAS SYSTEMNUMMER                 
000800*                                 VOLVO VCAS SYSTEM NUMBER                
000900        05 002A-VC-IDVCOMLOP PIC X(2).                                    
001000*                                 VCOM IDENTITET LÖPNUMMER                
001100*                                 VCOM IDENTITY LOPNUMBER                 
001200        05 002A-VC-IDLANDX2  PIC X(2).                                    
001300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 2-LETTER CODE FOR COUNTRY               
001500     03 002A-IDCPYTXT.                                                    
001600*                                 COPYTEXT IDENTITET                      
001700*                                 IDENTITY OF A COPYTEXT                  
001800        05 002A-CT-IDSYSTEM  PIC X(4).                                    
001900*                                 VOLVO VCAS SYSTEMNUMMER                 
002000*                                 VOLVO VCAS SYSTEM NUMBER                
002100        05 002A-CT-IDPTYP    PIC X(3).                                    
002200*                                 POSTTYP                                 
002300*                                 RECORD TYPE                             
002400        05 002A-CT-IDVTYP    PIC X.                                       
002500*                                 POSTTYPSVERSION                         
002600*                                 RECORD TYPE VERSION                     
002700     03 002A-FILLER          PIC X(64).                                   
002800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
