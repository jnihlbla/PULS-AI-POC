000100 01  REQU-REQUPRE.                                                        
000200*                                 THE FIRST FIELDS IN AN IMS              
000300*                                 MESSAGE INPUT DESCRIPTION               
000400     03 REQU-KVLL            PIC S9(4)           COMP.                    
000500*                                 LRECL I ETT VARIABELT RECORD            
000600*                                 LRECL IN A VARIABLE RECORD              
000700     03 REQU-KDZ1            PIC X.                                       
000800*                                 POS 3 I LRECL I MID/MOD                 
000900*                                 POS 3 IN LRECL IN MID/MOD               
001000     03 REQU-KDZ2            PIC X.                                       
001100*                                 POS 4 I LRECL I MID/MOD                 
001200*                                 POS 4 IN LRECL IN MID/MOD               
001300     03 REQU-KDTRANS         PIC X(8).                                    
001400*                                 TRANSAKTIONSKOD                         
001500*                                 TRANSACTION CODE                        
001600*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
