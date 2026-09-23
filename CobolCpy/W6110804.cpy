000100*                                                                         
000200*                                                                         
000300*    COPYTEXT FÖR VCC INHOUSE FORMAT: UNB                                 
000400*    INTERCHANGE HEADER                                                   
000500******************************************************************        
000600 01  W611UNB.                                                             
000700     03  PT              PIC X(3).                                        
000800*        VALUE 'UNB'        *** RECORD TYPE                               
000900     03  DA-LENGTH       PIC 9(3).                                        
001000*          VALUE 073        *** DATA LENGTH                               
001100*    03  SPARTID         PIC X(5) - X(14).                                
001200*                           *** SENDER - LENGTH IS UNDEFINED              
001300*    03  RPARTID         PIC X(5) - X(14).                                
001400*                           *** RECIPIENT - LENGTH IS UNDEFINED           
001500*    03  INTREF          PIC X(14).                                       
001600*                           *** INTERCHANGE CONTROL REFERENCE             
001700     03  FILLER          PIC X(73).                                       
001800*                           *** FILLER                                    
001900*** END COPY W6110804    LENGTH=79                                        
