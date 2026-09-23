000100 01  RIE-W461RIEN-CTX.                                                    
000200*                                 ORD.CONF. NOT FULLY INTERCHANGE         
000300*                                 -ABLE TO IMPORTER                       
000400*                                  RECORD TYPE RIE                        
000500     03 RIE-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIE-IDDC             PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RIE-IDARTNR          PIC 9(9).                                    
001000*                                 PART NUMBER                             
001100     03 RIE-REKSIFFR         PIC 9.                                       
001200*                                 PART NO CHECK DIGIT                     
001300     03 RIE-IDLOPNRE         PIC 9(3).                                    
001400*                                 SEQUENCE NUMBER FOR EACH SUPER-         
001500*                                 SESSION                                 
001600     03 RIE-IDKORTNR         PIC 9(2).                                    
001700*                                 SEQUENCE NUMBER FOR EACH RECORD         
001800*                                  IN A SUPERSESSION                      
001900     03 RIE-BERADREF         PIC X(10).                                   
002000*                                 CUSTOMERS ITEM REF.                     
002100     03 RIE-IDRONR           PIC 9(7).                                    
002200*                                 ORIGINAL ORDERNR     IDRONR-002         
002300     03 RIE-BEVOLREF         PIC X(10).                                   
002400*                                 VOLVO REFERENCE                         
002500     03 RIE-KDRESTR          PIC 9(2).                                    
002600*                                 RESTRICTION CODE                        
002700     03 RIE-KDERS            PIC 9(2).                                    
002800*                                 SUPERSESSION CODE                       
002900     03 RIE-KVBEART          PIC 9(6).                                    
003000*                                 ORDERED QUANTITY                        
003100     03 RIE-IDARTNR-TILLK    PIC 9(9).                                    
003200*                                 REPLACEMENT PART NO.                    
003300     03 RIE-REKSIFFR-TILLK   PIC 9.                                       
003400*                                 CHECK DIGIT REPLACING PART              
003500     03 RIE-KVBEART-TILLK    PIC 9(6).                                    
003600*                                 ORD. QUANT NEW PART                     
003700     03 RIE-DIERS-KVOT       PIC 9(3)V9(3).                               
003800*                                 QUOTIENT BETWEEN                        
003900*                                 DIERS-TILLK AND DIERS-ERS               
004000     03 RIE-KDERSUP          PIC 9.                                       
004100*                                 UPDATE IMPORTERS PART FILE AT           
004200*                                 SUPERSESSION                            
004300     03 RIE-KDDSP            PIC 9.                                       
004400*                                 AFFECT ON DSP                           
004500*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
