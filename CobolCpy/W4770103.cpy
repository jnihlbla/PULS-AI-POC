000100 01  W4770103.                                                            
000200*                                 EDI PC INFORMATION                      
000300*                                 RECORD TYPE 1001                        
000400     03 IDPTYP               PIC X(4).                                    
000500*                                 RECORD TYPE          IDPTYP-004         
000600     03 IDSKEPPN             PIC 9(7).                                    
000700*                                 SHIPMENT NO                             
000800     03 IDFAKT               PIC 9(7).                                    
000900*                                 INVOICE NO.                             
001000     03 IDDISTR              PIC 9(4).                                    
001100*                                 DISTRICT NUMBER                         
001200     03 IDKUNDNR             PIC 9(6).                                    
001300*                                 CUSTOMER NO                             
001400     03 IDKUNDRF             PIC X(10).                                   
001500*                                 CUSTOMER REFERENCE (ORDER ID)           
001600     03 IDKOLLI              PIC 9(5).                                    
001700*                                 CASE NUMBER                             
001800     03 VKORDBTO-KOLLI       PIC 9(6)V9(1).                               
001900*                                 ORDER WEIGHT GROSS PER CASE             
002000     03 VKORDNTO-KOLLI       PIC 9(6)V9(1).                               
002100*                                 ORDER WEIGHT NET PER CASE               
002200     03 VLORDBTO-KOLLI       PIC 9(4)V9(3).                               
002300*                                 ORDER VOL GR/CASE                       
002400     03 KDEMBTYP             PIC 9.                                       
002500*                                 PACKAGE TYPE                            
002600     03 SUORDV-KOLLI         PIC 9(9)V9(2).                               
002700*                                 ORDER VALUE PER CASE                    
002800*** END COPY W4770103    LENGTH=76                                        
