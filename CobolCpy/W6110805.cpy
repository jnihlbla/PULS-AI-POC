000100*                                                                         
000200*                                                                         
000300*    COPYTEXT FÖR VCC INHOUSE FORMAT: UNH                                 
000400*    MESSAGE HEADER                                                       
000500******************************************************************        
000600 01  W611UNH.                                                             
000700     03  PT              PIC X(3).                                        
000800*       VALUE 'UNH'         *** RECORD TYPE                               
000900     03  DA-LENGTH       PIC 9(3).                                        
001000*        VALUE 073          *** DATA LENGTH                               
001100     03  MSGTYPE         PIC X(6).                                        
001200*                           *** MESSAGE TYPE                              
001300     03  MSGVER          PIC X(3).                                        
001400*                           *** MESSAGE VERSION                           
001500     03  FILLER          PIC X(64).                                       
001600*                           *** FILLER                                    
001700*** END COPY W6110805    LENGTH=79                                        
