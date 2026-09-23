000100 01  RIK-W461RIK1.                                                        
000200*                                 INVOICE HEAD TO IMPORTER                
000300*                                 RECORD TYPE RIK                         
000400     03 RIK-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIK-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RIK-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RIK-KDFAKTYP         PIC X.                                       
001100*                                 INVOICE TYPE                            
001200     03 RIK-IDFAKT           PIC 9(7).                                    
001300*                                 INVOICE NO.                             
001400     03 RIK-TIFAKT           PIC 9(6).                                    
001500*                                 INVOICING DATE   (YYMMDD)               
001600     03 RIK-IDFRASED         PIC X(15).                                   
001700*                                 FREIGHT LETTER NO,                      
001800     03 RIK-SUFKTBEL         PIC 9(8)V9(2).                               
001900*                                 TOTAL INVOICED AMOUNT                   
002000     03 RIK-KDVALUTA         PIC 9(3).                                    
002100*                                 CURRENCY CODE                           
002200     03 RIK-PRKURS           PIC 9(6)V9(5).                               
002300*                                 CURRENCY EXCHANGE RATE                  
002400     03 RIK-SUFKTUTL         PIC 9(11)V9(2).                              
002500*                                 INVOICE-SUM IN FOREIGN VALUE            
002600     03 RIK-KDFAKNOT         PIC X(2).                                    
002700*                                 INVOICE NOTES                           
002800     03 RIK-KDVALISO         PIC X(3).                                    
002900*                                 CURRENCY CODE BY ISO-STANDARD.          
003000*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
