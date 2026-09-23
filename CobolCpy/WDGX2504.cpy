000100 01  2504-WDGX2504.                                                       
000200*                                 REFILL ODERNUMMER REGISTER              
000300     03 2504-IDKUNDRF        PIC X(10).                                   
000400*                                 KUNDENS REFERENS (ORDERID)              
000500*                                 CUSTOMER REFERENCE (ORDER ID)           
000600     03 2504-IDORDNR7-FILLER REDEFINES 2504-IDKUNDRF.                     
000700        05 2504-IDORDNR7     PIC 9(7).                                    
000800*                                 ORDERNUMMER                             
000900*                                 ORDER NUMBER                            
001000        05 FILLER            PIC X(3).                                    
001100     03 2504-IDORDNR5-FILLER REDEFINES 2504-IDKUNDRF.                     
001200        05 2504-IDORDNR5     PIC 9(5).                                    
001300*                                 ORDERNUMMER                             
001400*                                 ORDER NUMBER                            
001500        05 FILLER            PIC X(5).                                    
001600*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
