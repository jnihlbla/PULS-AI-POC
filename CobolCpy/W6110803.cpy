000100*                                                                         
000200*                                                                         
000300*    COPYTEXT FÖR VCC INHOUSE FORMAT: 003                                 
000400*    FILE TRAILER RECORD                                                  
000500******************************************************************        
000600 01  W6110803.                                                            
000700     03  PT              PIC X(3).                                        
000800*        VALUE '003'        *** RECORD TYPE                               
000900     03  DA-LENGTH       PIC 9(3).                                        
001000*        VALUE 073          *** DATA LENGTH                               
001100     03  FILLER          PIC X(73).                                       
001200*                           *** FILLER                                    
001300*** END COPY W6110803    LENGTH=79                                        
