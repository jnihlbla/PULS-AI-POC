000100 01  MID-W4I57801.                                                        
000200     03 MID-IDDISTR-IN       PIC X(4).                                    
000300*                                 DISTRICT NUMBER                         
000400     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000500*                                 CUSTOMER NO                             
000600     03 MID-IDARTNR-IN       PIC X(9).                                    
000700*                                 PART NUMBER                             
000800     03 MID-KDFARLIG-IN      PIC X.                                       
000900*                                 DANGEROUS GOODS CODE                    
001000     03 MID-TIRFSDAT-CDC-IN  PIC X(6).                                    
001100*                                 READY FOR SHIPMENT CDC YYMMDD           
001200     03 MID-INPUT            OCCURS 6 TIMES.                              
001300        05 MID-KDCMD         PIC X.                                       
001400*                                 LINE UPDATE COMMAND                     
001500        05 MID-KDORDKL       PIC 9.                                       
001600*                                 ORDER CLASS                             
001700        05 MID-IDDC-PRIM     PIC X(2).                                    
001800*                                 PRIMARY DELIVERING DC                   
001900        05 MID-BELAGINS      PIC X(60).                                   
002000*                                 PART OF WAREHOUSE INSTRUCTIONS          
002100        05 MID-BETEXT        PIC X(10).                                   
002200     03 MID-RUBVAR           PIC X(6).                                    
002300*** END OF VILMAII-COPY LENGTH= 476 BYTES                                 
