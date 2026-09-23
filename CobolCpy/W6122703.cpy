000100*** EDIT ALLOWED                                                          
000200*** REQUEST COPYBOOK WHEN CALLING PROJECT44. ***                          
000300*** TRANSACTION WHEN PROJECT44 SHOULD        ***                          
000400*** ARCHIVE A SUBSCRIPTION ID.               ***                          
000500                                                                          
000600        06 ReqPathParameters.                                             
000700           09 subscription-id          PIC S9(18) COMP-5 SYNC.            
000800        06 ReqHeaders.                                                    
000900           09 proxy-key-length         PIC S9999  COMP-5 SYNC.            
001000           09 proxy-key                PIC X(255).                        
