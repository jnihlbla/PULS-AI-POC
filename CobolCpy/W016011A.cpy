000100 01  011A-W016011A.                                                       
000200*                                 COPYTEXT FÖR VCOM-MOTTAGNING            
000300*                                 FIL-INFORMATION.                        
000400     03 011A-IDVCOM.                                                      
000500*                                 VCOM IDENTITET                          
000600*                                 VCOM IDENTITY                           
000700        05 011A-VC-IDSYSTEM  PIC X(4).                                    
000800*                                 VOLVO VCAS SYSTEMNUMMER                 
000900*                                 VOLVO VCAS SYSTEM NUMBER                
001000        05 011A-VC-IDVCOMLOP PIC X(2).                                    
001100*                                 VCOM IDENTITET LÖPNUMMER                
001200*                                 VCOM IDENTITY LOPNUMBER                 
001300        05 011A-VC-IDLANDX2  PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500*                                 2-LETTER CODE FOR COUNTRY               
001600     03 011A-IDPTYP          PIC X(3).                                    
001700*                                 POSTTYP                                 
001800*                                 RECORD TYPE                             
001900     03 011A-IDVTYP          PIC X.                                       
002000*                                 POSTTYPSVERSION                         
002100*                                 RECORD TYPE VERSION                     
002200     03 011A-IDFTG           PIC 9(2).                                    
002300*                                 FÖRETAGSID EKONOM REDOVISNING           
002400*                                 COMPANY IDENTITY ACCOUNTING             
002500     03 011A-IDLANDX2        PIC X(2).                                    
002600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002700*                                 2-LETTER CODE FOR COUNTRY               
002800     03 011A-IDDSN           PIC X(44).                                   
002900*                                 DATASET NAMN                            
003000*                                 DATASET NAME                            
003100     03 011A-FILLER          PIC X(20).                                   
003200*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
