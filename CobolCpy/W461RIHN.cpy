000100 01  RIH-W461RIHN-CTX.                                                    
000200*                                 ORDER. CONFIRM.    STATED QUAN-         
000300*                                 TITY IS BACK-ORDERED                    
000400*                                 TO IMPORTER                             
000500*                                 RECORD TYPE RIH                         
000600     03 RIH-IDPTYP           PIC X(3).                                    
000700*                                 RECORD TYPE                             
000800     03 RIH-IDDC             PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 RIH-IDARTNR          PIC 9(9).                                    
001100*                                 PART NUMBER                             
001200     03 RIH-REKSIFFR         PIC 9.                                       
001300*                                 PART NO CHECK DIGIT                     
001400     03 RIH-BERADREF         PIC X(10).                                   
001500*                                 CUSTOMERS ITEM REF.                     
001600     03 RIH-IDRONR           PIC 9(7).                                    
001700*                                 ORIGINAL ORDERNR     IDRONR-002         
001800     03 RIH-BEVOLREF         PIC X(10).                                   
001900*                                 VOLVO REFERENCE                         
002000     03 RIH-KDRESTR          PIC 9(2).                                    
002100*                                 RESTRICTION CODE                        
002200     03 RIH-KVBEART          PIC 9(6).                                    
002300*                                 ORDERED QUANTITY                        
002400     03 RIH-KVAVBART         PIC 9(6).                                    
002500*                                 ALLOCATED QUANTITY                      
002600     03 RIH-KVRO             PIC 9(6).                                    
002700*                                 BACKORDERED QTY                         
002800     03 RIH-KDDSP            PIC 9.                                       
002900*                                 AFFECT ON DSP                           
003000     03 RIH-TIDISPIN         PIC 9(6).                                    
003100*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
003200     03 RIH-TIMM             PIC 9(2).                                    
003300*                                 MONTH (MM)                              
003400     03 RIH-TIDD             PIC 9(2).                                    
003500*                                 DAY OF MONTH (DD)                       
003600     03 RIH-TIKLOCK          PIC 9(8).                                    
003700*                                 TIME OF DAY (HHMMSSTH)                  
003800*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
