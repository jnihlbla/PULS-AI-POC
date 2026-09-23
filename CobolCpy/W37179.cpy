000100 01  W37179.                                                              
000200*                                 BYTES FLEN-MAASTRICHT                   
000300     03 IDFAKT               PIC S9(7)           COMP-3.                  
000400*                                 FAKTURANUMMER                           
000500*                                 INVOICE NO.                             
000600     03 DAANKDAG             PIC 9(8).                                    
000700*                                 ANKOMSTDAG                              
000800*                                 RECEIVING DATE                          
000900     03 DASNDDAT             PIC 9(8).                                    
001000*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
001100*                                 SHIPPING DATE   (YYYYMMDD)              
001200     03 IDDC-REC             PIC X(2).                                    
001300*                                 MOTTAGANDE LAGER                        
001400*                                 RECEIVING WAREHOUSE                     
001500     03 IDDC-SEND            PIC X(2).                                    
001600*                                 SÄNDANDE LAGER                          
001700*                                 SENDING WAREHOUSE                       
001800     03 KDTRSTAT             PIC S9              COMP-3.                  
001900*                                 TRANSAKTIONSSTATUS                      
002000*                                 TRANSACTIONS-STATUS                     
002100     03 SUFKTNTO             PIC S9(11)V9(2)     COMP-3.                  
002200*                                 FAKTURERAT VARUVÄRDE NETTO              
002300*                                 INVOICED GOODSVALUE NET                 
002400     03 VKORDBTO-FAKT        PIC S9(6)V9(1)      COMP-3.                  
002500*                                 ORDERVIKT BRUTTO PER FAKTURA            
002600*                                 ORDER WEIGHT GROSS PER INVOICE          
002700     03 VLORDBTO-FAKT        PIC S9(4)V9(3)      COMP-3.                  
002800*                                 ORDERVOLYM BRUTTO PER FAKTURA           
002900*                                 ORDER VOLUME GROSS PER INVOICE          
003000*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
