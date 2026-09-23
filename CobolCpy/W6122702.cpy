000010*** EDIT ALLOWED                                                          
000020*** REQUEST COPYBOOK WHEN CALLING PROJECT44. ***                          
000030*** CER TRANSACTION WHEN PROJECT44 SHOULD    ***                          
000040*** STOP SENDING PUSHEVENTS WITH ETA INFO    ***                          
000050*** FOR A CONTAINER.                         ***                          
000060      06 ReqPathParameters.                                               
000070        09 shipment-id                    PIC S9(18) COMP-5 SYNC.         
000080      06 ReqHeaders.                                                      
000090        09 proxy-key-length               PIC S9999 COMP-5 SYNC.          
000100        09 proxy-key                      PIC X(255).                     
000200      06 ReqBody.                                                         
000300        09 empty-return-customer-num      PIC S9(9) COMP-5 SYNC.          
000400        09 empty-return-customer.                                         
000500          12 empty-return-customer-len    PIC S9999 COMP-5 SYNC.          
000600          12 empty-return-customer2       PIC X(10).                      
