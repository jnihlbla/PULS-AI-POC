000100 01  RIW-W461RIWN-CTX.                                                    
000200*                                 TRANSACTION FOR RECONCILIATION          
000300*                                 OF DSP BALANCE TO IMPORTER              
000400*                                 RECORD TYPE  RIW                        
000500     03 RIW-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIW-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RIW-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RIW-IDARTNR          PIC 9(9).                                    
001200*                                 PART NUMBER                             
001300     03 RIW-REKSIFFR         PIC 9.                                       
001400*                                 PART NO CHECK DIGIT                     
001500     03 RIW-BEVOLREF         PIC X(10).                                   
001600*                                 VOLVO REFERENCE                         
001700     03 RIW-KVBEART          PIC 9(6).                                    
001800*                                 ORDERED QUANTITY                        
001900     03 RIW-KVRO             PIC 9(6).                                    
002000*                                 BACKORDERED QTY                         
002100     03 RIW-FILLERX35        PIC X(35).                                   
002200*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
