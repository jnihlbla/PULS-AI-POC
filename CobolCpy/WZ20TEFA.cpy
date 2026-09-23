000100 01  TEFA-WZ20TEFA.                                                       
000200*                                 SUBROUTINE FOR VALIDATING A             
000300*                                 TELEFAX OR TELEPHONE NUMBER.            
000400*                                 VALID FORMAT IS:                        
000500*                                 (+COUNTRY)(AREA)(SIMPLE NUMBER)         
000600*                                 INCOMPLETE NUMBERS WILL BE              
000700*                                 COMPLETED WITH (+46 = SWEDEN)           
000800*                                 AND (31 = GOTHENBURG)                   
000900*                                                                         
001000*                                 CALLS:                                  
001100*                                    USING TEFA-WZ20TEFA                  
001200*                                                                         
001300*                                 INPUT: IDTFN-IDTFX - NUMBER TO          
001400*                                                   BE VALIDATED.         
001500*                                 OUTPUT: IDTFN-IDTFX - COMPLETED         
001600*                                                       NUMBER.           
001700*                                      KDRC                               
001800*                                       = 0 IF COMPLETE & OK              
001900*                                       = 4 IF INCOMPLETE BUT OK          
002000*                                       = 8 IF INCORRECT                  
002100     03 TEFA-IDTFN-IDTFX     PIC X(20).                                   
002200*                                 TELEPHONE NUMBER  EXTERNAL              
002300     03 TEFA-KDRC            PIC S9(9)           COMP.                    
002400*                                 RETURN CODE                             
002500*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
