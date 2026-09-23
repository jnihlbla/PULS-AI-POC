000100*                                                                         
000200*                                                                         
000300*    COPYTEXT FÖR VCC INHOUSE FORMAT                                      
000400*    GENERAL RECORD FORMAT                                                
000500******************************************************************        
000600 01  W611GEN.                                                             
000700     03  PTDA.                                                            
000800         05  PT          PIC X(3).                                        
000900*                           *** RECORD TYPE                               
001000         03  DA-LENGTH   PIC 9(3).                                        
001100*                           *** DATA LENGTH                               
001200     03  DATA.                                                            
001300         05  CHAR        PIC X  OCCURS 999 DEPENDING DA-LENGTH.           
001400*                           *** RECORD DATA                               
001500*** END COPY W6110806    LENGTH=1005                                      
