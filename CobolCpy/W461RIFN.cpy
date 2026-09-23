000100 01  RIF-W461RIFN-CTX.                                                    
000200*                                 ORDER. CONF.  SUPERSESSION TEXT         
000300*                                  TO IMPORTER    RECORD TYPE RIF         
000400     03 RIF-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIF-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RIF-IDARTNR          PIC 9(9).                                    
000900*                                 PART NUMBER                             
001000     03 RIF-REKSIFFR         PIC 9.                                       
001100*                                 PART NO CHECK DIGIT                     
001200     03 RIF-IDLOPNRE         PIC 9(3).                                    
001300*                                 SEQUENCE NUMBER FOR EACH SUPER-         
001400*                                 SESSION                                 
001500     03 RIF-IDKORTNR         PIC 9(2).                                    
001600*                                 SEQUENCE NUMBER FOR EACH RECORD         
001700*                                  IN A SUPERSESSION                      
001800     03 RIF-BERADREF         PIC X(10).                                   
001900*                                 CUSTOMERS ITEM REF.                     
002000     03 RIF-IDRONR           PIC 9(7).                                    
002100*                                 ORIGINAL ORDERNR     IDRONR-002         
002200     03 RIF-BEVOLREF         PIC X(10).                                   
002300*                                 VOLVO REFERENCE                         
002400     03 RIF-KDRESTR          PIC 9(2).                                    
002500*                                 RESTRICTION CODE                        
002600     03 RIF-KDERS            PIC 9(2).                                    
002700*                                 SUPERSESSION CODE                       
002800     03 RIF-KVBEART          PIC 9(6).                                    
002900*                                 ORDERED QUANTITY                        
003000     03 RIF-BEERS            PIC X(20).                                   
003100*                                 REPLACEMENT TEXT                        
003200     03 RIF-KDERSUP          PIC 9.                                       
003300*                                 UPDATE IMPORTERS PART FILE AT           
003400*                                 SUPERSESSION                            
003500     03 RIF-KDDSP            PIC 9.                                       
003600*                                 AFFECT ON DSP                           
003700     03 RIF-FILLERX1         PIC X.                                       
003800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
