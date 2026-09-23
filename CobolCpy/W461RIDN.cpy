000100 01  RID-W461RIDN-CTX.                                                    
000200*                                 ORD.CONF. FULLY INTERCHANGEABLE         
000300*                                  TO IMPORTER RECORD TYPE RID            
000400     03 RID-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RID-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RID-IDARTNR          PIC 9(9).                                    
000900*                                 PART NUMBER                             
001000     03 RID-REKSIFFR         PIC 9.                                       
001100*                                 PART NO CHECK DIGIT                     
001200     03 RID-IDLOPNRE         PIC 9(3).                                    
001300*                                 SEQUENCE NUMBER FOR EACH SUPER-         
001400*                                 SESSION                                 
001500     03 RID-IDKORTNR         PIC 9(2).                                    
001600*                                 SEQUENCE NUMBER FOR EACH RECORD         
001700*                                  IN A SUPERSESSION                      
001800     03 RID-BERADREF         PIC X(10).                                   
001900*                                 CUSTOMERS ITEM REF.                     
002000     03 RID-IDRONR           PIC 9(7).                                    
002100*                                 ORIGINAL ORDERNR     IDRONR-002         
002200     03 RID-BEVOLREF         PIC X(10).                                   
002300*                                 VOLVO REFERENCE                         
002400     03 RID-KDRESTR          PIC 9(2).                                    
002500*                                 RESTRICTION CODE                        
002600     03 RID-KDERS            PIC 9(2).                                    
002700*                                 SUPERSESSION CODE                       
002800     03 RID-KVBEART          PIC 9(6).                                    
002900*                                 ORDERED QUANTITY                        
003000     03 RID-IDARTNR-TILLK    PIC 9(9).                                    
003100*                                 REPLACEMENT PART NO.                    
003200     03 RID-REKSIFFR-TILLK   PIC 9.                                       
003300*                                 CHECK DIGIT REPLACING PART              
003400     03 RID-KVBEART-TILLK    PIC 9(6).                                    
003500*                                 ORD. QUANT NEW PART                     
003600     03 RID-DIERS-KVOT       PIC 9(3)V9(3).                               
003700*                                 QUOTIENT BETWEEN                        
003800*                                 DIERS-TILLK AND DIERS-ERS               
003900     03 RID-KDERSUP          PIC 9.                                       
004000*                                 UPDATE IMPORTERS PART FILE AT           
004100*                                 SUPERSESSION                            
004200     03 RID-KDDSP            PIC 9.                                       
004300*                                 AFFECT ON DSP                           
004400*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
