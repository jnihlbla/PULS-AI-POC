000100 01  RIO-W461RION-CTX.                                                    
000200*                                 INVOICE LINE TO IMPORTER                
000300*                                 RECORD TYPE RIO                         
000400     03 RIO-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIO-IDORDNR          PIC 9(7).                                    
000700*                                 ORDER NUMBER        IDORDNR-002         
000800     03 RIO-IDARTNR          PIC 9(9).                                    
000900*                                 PART NUMBER                             
001000     03 RIO-REKSIFFR         PIC 9.                                       
001100*                                 PART NO CHECK DIGIT                     
001200     03 RIO-BERADREF         PIC X(10).                                   
001300*                                 CUSTOMERS ITEM REF.                     
001400     03 RIO-KVBEART          PIC 9(6).                                    
001500*                                 ORDERED QUANTITY                        
001600     03 RIO-KVLEVART         PIC 9(6).                                    
001700*                                 DELIVERED QUANTITY                      
001800     03 RIO-RESERVG          PIC 9(3)V9(2).                               
001900*                                 PERCENT SERVICE DEGREE                  
002000     03 RIO-PRARTBTO-EXP     PIC 9(7)V9(2).                               
002100*                                 GROSS-PRICE EXPORT                      
002200*                                  (FOB-GROSS)                            
002300     03 RIO-PRARTNTO         PIC 9(7)V9(2).                               
002400*                                 NET PRICE EACH   (FOB NET)              
002500     03 RIO-IDFKNGRP         PIC 9(4).                                    
002600*                                 FUNCTION GROUP                          
002700     03 RIO-KDPRODSL         PIC 9(2).                                    
002800*                                 PRODUCT GROUP                           
002900     03 RIO-KDDSP            PIC 9.                                       
003000*                                 AFFECT ON DSP                           
003100     03 RIO-KDVVKL           PIC 9.                                       
003200*                                 VOLUME VALUE CLASS                      
003300     03 RIO-KDVRINFO         PIC 9.                                       
003400*                                 VR/DSP UP-DATE                          
003500     03 RIO-FLINVEST         PIC X.                                       
003600*                                 EXCHANGE INVESTMENT FLAG                
003700     03 RIO-FLPRTILL         PIC X.                                       
003800*                                 PRICE PENALTY FLAG                      
003900     03 RIO-FLDIRLEV         PIC X.                                       
004000*                                 DIRECT DELIVERY ?                       
004100     03 RIO-KDRABATT         PIC 9(3).                                    
004200*                                 PURCHASE DISCOUNT CODE                  
004300     03 RIO-IDDC             PIC X(2).                                    
004400*                                 WAREHOUSE IDENTIFIER                    
004500     03 RIO-KDPSLLOC         PIC 9(2).                                    
004600*                                 PRODUCT GROUP LOCAL                     
004700     03 RIO-PRAVCOST         PIC 9(7)V9(2).                               
004800*                                 AVERAGE COST FOREIGN CURRENCY           
004900     03 RIO-PRAVCOST-CORE    PIC 9(7)V9(2).                               
005000*                                 AV. COST PRICE OF THE CORE IN F         
005100*                                 OR. CURR.                               
005200*** END OF VILMAII-COPY LENGTH= 102 BYTES                                 
