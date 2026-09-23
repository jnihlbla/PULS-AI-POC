000010*** EDIT ALLOWED                                                          
000020*** REQUEST COPYBOOK WHEN CALLING PROJECT44. ***                          
000030*** CREATE SUBSCRIPTION TRANSACTION.         ***                          
000040     06 ReqHeaders.                                                       
000050       09 proxy-key-length              PIC S9999 COMP-5 SYNC.            
000060       09 proxy-key                     PIC X(255).                       
000070     06 ReqBody.                                                          
000080       09 request-carrier-code-num      PIC S9(9) COMP-5 SYNC.            
000090       09 request-carrier-code.                                           
000100         12 request-carrier-code-len    PIC S9999 COMP-5 SYNC.            
000200         12 request-carrier-code2       PIC X(15).                        
000300                                                                          
000400       09 request-type-num              PIC S9(9) COMP-5 SYNC.            
000500                                                                          
000600       09 request-type.                                                   
000700         12 request-type2-length        PIC S9999 COMP-5 SYNC.            
000800         12 request-type2               PIC X(4).                         
000900                                                                          
001000       09 request-key-num               PIC S9(9) COMP-5 SYNC.            
001100                                                                          
001200       09 request-key.                                                    
001300         12 request-key2-length         PIC S9999 COMP-5 SYNC.            
001400         12 request-key2                PIC X(15).                        
