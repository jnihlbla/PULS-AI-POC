000100 01  RIA-W461RIAN-CTX.                                                    
000200*                                 ATTATCHED BO TO IMPORTER                
000300*                                 RECORD TYPE RIA                         
000400     03 RIA-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIA-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RIA-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RIA-IDKUNDNR         PIC 9(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 RIA-IDORDNR          PIC 9(7).                                    
001300*                                 ORDER NUMBER        IDORDNR-002         
001400     03 RIA-KDORDKL          PIC 9.                                       
001500*                                 ORDER CLASS                             
001600     03 RIA-IDARTNR          PIC 9(9).                                    
001700*                                 PART NUMBER                             
001800     03 RIA-REKSIFFR         PIC 9.                                       
001900*                                 PART NO CHECK DIGIT                     
002000     03 RIA-BERADREF         PIC X(10).                                   
002100*                                 CUSTOMERS ITEM REF.                     
002200     03 RIA-IDRONR           PIC 9(7).                                    
002300*                                 ORIGINAL ORDERNR     IDRONR-002         
002400     03 RIA-BEVOLREF         PIC X(10).                                   
002500*                                 VOLVO REFERENCE                         
002600     03 RIA-KVLEVART         PIC 9(6).                                    
002700*                                 DELIVERED QUANTITY                      
002800     03 RIA-KDRESTR          PIC 9(2).                                    
002900*                                 RESTRICTION CODE                        
003000     03 RIA-TIMM             PIC 9(2).                                    
003100*                                 MONTH (MM)                              
003200     03 RIA-TIDD             PIC 9(2).                                    
003300*                                 DAY OF MONTH (DD)                       
003400     03 RIA-TIKLOCK          PIC 9(8).                                    
003500*                                 TIME OF DAY (HHMMSSTH)                  
003600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
