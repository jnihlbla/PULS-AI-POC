000100 01  001A-W016001A.                                                       
000200*                                 COPYTEXT FÖR VCOM-MOTTAGNING            
000300*                                 STANDARD INFORMATION.                   
000400     03 001A-IDVCOM.                                                      
000500*                                 VCOM IDENTITET                          
000600*                                 VCOM IDENTITY                           
000700        05 001A-VC-IDSYSTEM  PIC X(4).                                    
000800*                                 VOLVO VCAS SYSTEMNUMMER                 
000900*                                 VOLVO VCAS SYSTEM NUMBER                
001000        05 001A-VC-IDVCOMLOP PIC X(2).                                    
001100*                                 VCOM IDENTITET LÖPNUMMER                
001200*                                 VCOM IDENTITY LOPNUMBER                 
001300        05 001A-VC-IDLANDX2  PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500*                                 2-LETTER CODE FOR COUNTRY               
001600     03 001A-IDPTYP          PIC X(3).                                    
001700*                                 POSTTYP                                 
001800*                                 RECORD TYPE                             
001900     03 001A-IDVTYP          PIC X.                                       
002000*                                 POSTTYPSVERSION                         
002100*                                 RECORD TYPE VERSION                     
002200     03 001A-IDCPYTXT.                                                    
002300*                                 COPYTEXT IDENTITET                      
002400*                                 IDENTITY OF A COPYTEXT                  
002500        05 001A-CT-IDSYSTEM  PIC X(4).                                    
002600*                                 VOLVO VCAS SYSTEMNUMMER                 
002700*                                 VOLVO VCAS SYSTEM NUMBER                
002800        05 001A-CT-IDPTYP    PIC X(3).                                    
002900*                                 POSTTYP                                 
003000*                                 RECORD TYPE                             
003100        05 001A-CT-IDVTYP    PIC X.                                       
003200*                                 POSTTYPSVERSION                         
003300*                                 RECORD TYPE VERSION                     
003400     03 001A-TIREGDAT        PIC 9(6).                                    
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600*                                 REGISTRATION DATE (YYMMDD)              
003700     03 001A-KVPOST          PIC 9(7).                                    
003800*                                 RÄKNARE, ANTAL POSTER                   
003900*                                 RECORD COUNTER                          
004000     03 001A-IDVCOMEX        PIC X(34).                                   
004100*                                 VCOM IDENTITET                          
004200*                                 VCOM IDENTITY                           
004300     03 001A-FILLER          PIC X(13).                                   
004400*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
