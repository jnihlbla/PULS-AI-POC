000100 01  RII-W461RIIN-CTX.                                                    
000200*                                 ORDER. CONFIRM. CANCELLED LINES         
000300*                                  TO IMPORTER    RECORD TYPE RII         
000400     03 RII-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RII-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RII-IDARTNR          PIC 9(9).                                    
000900*                                 PART NUMBER                             
001000     03 RII-REKSIFFR         PIC 9.                                       
001100*                                 PART NO CHECK DIGIT                     
001200     03 RII-BERADREF         PIC X(10).                                   
001300*                                 CUSTOMERS ITEM REF.                     
001400     03 RII-IDRONR           PIC 9(7).                                    
001500*                                 ORIGINAL ORDERNR     IDRONR-002         
001600     03 RII-BEVOLREF         PIC X(10).                                   
001700*                                 VOLVO REFERENCE                         
001800     03 RII-KDRESTR          PIC 9(2).                                    
001900*                                 RESTRICTION CODE                        
002000     03 RII-KVBEART          PIC 9(6).                                    
002100*                                 ORDERED QUANTITY                        
002200     03 RII-KDDSP            PIC 9.                                       
002300*                                 AFFECT ON DSP                           
002400     03 RII-TIMM             PIC 9(2).                                    
002500*                                 MONTH (MM)                              
002600     03 RII-TIDD             PIC 9(2).                                    
002700*                                 DAY OF MONTH (DD)                       
002800     03 RII-TIKLOCK          PIC 9(8).                                    
002900*                                 TIME OF DAY (HHMMSSTH)                  
003000     03 RII-FILLERX17        PIC X(17).                                   
003100*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
