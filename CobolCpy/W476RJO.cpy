000100 01  RJO-W476RJON.                                                        
000200*                                 DEALER INVOICE TO VIPS                  
000300*                                 LINE                                    
000400     03 RJO-SORT-GRPRJX.                                                  
000500        05 RJO-IDFAKT        PIC S9(7)           COMP-3.                  
000600*                                 INVOICE NO.                             
000700        05 RJO-IDDISTR       PIC S9(5)           COMP-3.                  
000800*                                 DISTRICT NUMBER                         
000900        05 RJO-IDKUNDNR      PIC S9(7)           COMP-3.                  
001000*                                 CUSTOMER NO                             
001100        05 RJO-IDORDER       PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDER NUMBER                
001300        05 RJO-IDPRODNR      PIC S9(7)           COMP-3.                  
001400*                                 PRODUCTION NUMBER                       
001500        05 RJO-IDPURAD       PIC S9(5)           COMP-3.                  
001600*                                 LINENO IN PACKINGDOCUMENT               
001700     03 RJO-IDPTYP           PIC X(3).                                    
001800*                                 RECORD TYPE                             
001900     03 RJO-IDORDNR          PIC 9(7).                                    
002000*                                 ORDER NUMBER        IDORDNR-002         
002100     03 RJO-IDARTNR          PIC 9(9).                                    
002200*                                 PART NUMBER                             
002300     03 RJO-REKSIFFR         PIC 9.                                       
002400*                                 PART NO CHECK DIGIT                     
002500     03 RJO-BERADREF         PIC X(10).                                   
002600*                                 CUSTOMERS ITEM REF.                     
002700     03 RJO-KVBEART          PIC 9(6).                                    
002800*                                 ORDERED QUANTITY                        
002900     03 RJO-KVLEVART         PIC 9(6).                                    
003000*                                 DELIVERED QUANTITY                      
003100     03 RJO-RESERVG          PIC 9(3)V9(2).                               
003200*                                 PERCENT SERVICE DEGREE                  
003300     03 RJO-PRARTBTO-EXP     PIC 9(7)V9(2).                               
003400*                                 GROSS-PRICE EXPORT                      
003500*                                  (FOB-GROSS)                            
003600     03 RJO-PRARTSTD         PIC 9(7)V9(2).                               
003700*                                 STANDARD PRICE                          
003800     03 RJO-PRARTSJK         PIC 9(7)V9(2).                               
003900*                                 COST OF SALES                           
004000     03 RJO-PRARTNTO-LOC     PIC 9(7)V9(2).                               
004100*                                 NET PRICE EACH LOCAL CURRENCY           
004200     03 RJO-PRARTBTO-LOC     PIC 9(7)V9(2).                               
004300*                                 LOCAL GROSS SALES PRICE                 
004400     03 RJO-SULNELOC         PIC 9(9)V9(2).                               
004500*                                 INVOICE LINE SUM VAT EXCLUDED           
004600     03 RJO-SUVAT-LINE       PIC 9(11)V9(2).                              
004700*                                 VAT VALUE PER INVOICE LINE              
004800     03 RJO-KDVAT            PIC X(2).                                    
004900*                                 VAT CODE                                
005000     03 RJO-IDFKNGRP         PIC 9(4).                                    
005100*                                 FUNCTION GROUP                          
005200     03 RJO-KDPRODSL         PIC 9(2).                                    
005300*                                 PRODUCT GROUP                           
005400     03 RJO-KDDSP            PIC 9.                                       
005500*                                 AFFECT ON DSP                           
005600     03 RJO-KDVVKL           PIC 9.                                       
005700*                                 VOLUME VALUE CLASS                      
005800     03 RJO-KDVRINFO         PIC 9.                                       
005900*                                 VR/DSP UP-DATE                          
006000     03 RJO-FLINVEST         PIC X.                                       
006100*                                 EXCHANGE INVESTMENT FLAG                
006200     03 RJO-FLPRTILL         PIC X.                                       
006300*                                 PRICE PENALTY FLAG                      
006400     03 RJO-FLDIRLEV         PIC X.                                       
006500*                                 DIRECT DELIVERY ?                       
006600     03 RJO-KDRABATT         PIC 9(3).                                    
006700*                                 PURCHASE DISCOUNT CODE                  
006800     03 RJO-KDRAB            PIC X(5).                                    
006900     03 RJO-IDDC             PIC X(2).                                    
007000*                                 WAREHOUSE IDENTIFIER                    
007100     03 RJO-KDPSLLOC         PIC 9(2).                                    
007200*                                 PRODUCT GROUP LOCAL                     
007300     03 RJO-PRAVCOST         PIC 9(7)V9(2).                               
007400*                                 AVERAGE COST FOREIGN CURRENCY           
007500     03 RJO-PRAVCOST-CORE    PIC 9(7)V9(2).                               
007600*                                 AV. COST PRICE OF THE CORE IN F         
007700*                                 OR. CURR.                               
007800     03 RJO-IDBIL.                                                        
007900*                                 CAR IDENTITY                            
008000        05 RJO-IDBILTYP      PIC X(3).                                    
008100*                                 CAR TYPE                                
008200        05 RJO-TIAAAA        PIC X(4).                                    
008300*                                 YEAR  (YYYY)                            
008400        05 RJO-IDCHASSI-PIE  PIC X(6).                                    
008500*                                 CHASSI NUMBER PIE                       
008600*** END OF VILMAII-COPY LENGTH= 195 BYTES                                 
