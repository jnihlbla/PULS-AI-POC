000100 01  CRUL-WZ04CRUL.                                                       
000200*                                 SUBPROGRAM WZ04CRUL: CHECK IF           
000300*                                 MATCHING D&P RULE EXISTS.               
000400*                                                                         
000500*                                 CALL FORMAT:                            
000600*                                  CALL WZ04CRUL USING                    
000700*                                                CRUL-WZ04CRUL            
000800*                                 INPUT PARAMETERS                        
000900*                                  CRUL-IDOUTTYPE  KEY                    
001000*                                  CRUL-IDOUTREC   KEY                    
001100*                                                                         
001200*                                 OUTPUT PARAMETER                        
001300*                                  CRUL-KDRC   = ZERO  = OK               
001400*                                              = 4 = MISSING RULE         
001500*                                                                         
001600     03 CRUL-IDOUTTYPE       PIC X(15).                                   
001700*                                 OUTPUT TYPE                             
001800     03 CRUL-IDOUTREC        PIC X(30).                                   
001900*                                 OUTPUT RECEIVER                         
002000     03 CRUL-KDRC            PIC S9(9)           COMP.                    
002100*                                 RETURN CODE                             
002200*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
