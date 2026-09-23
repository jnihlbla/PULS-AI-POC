000100 01  RIG-W461RIGN-CTX.                                                    
000200*                                 ORDER. CONFIRM.  QUANTITY ADAP-         
000300*                                 TION  TO IMPORTER                       
000400*                                 RECORD TYPE RIG                         
000500     03 RIG-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIG-IDDC             PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RIG-IDARTNR          PIC 9(9).                                    
001000*                                 PART NUMBER                             
001100     03 RIG-REKSIFFR         PIC 9.                                       
001200*                                 PART NO CHECK DIGIT                     
001300     03 RIG-BERADREF         PIC X(10).                                   
001400*                                 CUSTOMERS ITEM REF.                     
001500     03 RIG-IDRONR           PIC 9(7).                                    
001600*                                 ORIGINAL ORDERNR     IDRONR-002         
001700     03 RIG-BEVOLREF         PIC X(10).                                   
001800*                                 VOLVO REFERENCE                         
001900     03 RIG-KDRESTR          PIC 9(2).                                    
002000*                                 RESTRICTION CODE                        
002100     03 RIG-KVBEART          PIC 9(6).                                    
002200*                                 ORDERED QUANTITY                        
002300     03 RIG-KVBEART-Q        PIC 9(6).                                    
002400*                                 ORDERED QUANTITY ADAPTED                
002500*                                  ITEMS                                  
002600     03 RIG-KVQPACK-1        PIC 9(5).                                    
002700*                                 QUANTITY IN BULK PACK Q1                
002800     03 RIG-KDDSP            PIC 9.                                       
002900*                                 AFFECT ON DSP                           
003000     03 RIG-TIMM             PIC 9(2).                                    
003100*                                 MONTH (MM)                              
003200     03 RIG-TIDD             PIC 9(2).                                    
003300*                                 DAY OF MONTH (DD)                       
003400     03 RIG-TIKLOCK          PIC 9(8).                                    
003500*                                 TIME OF DAY (HHMMSSTH)                  
003600     03 RIG-FILLERX6         PIC X(6).                                    
003700*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
