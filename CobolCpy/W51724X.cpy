000100 01  W51724X.                                                             
000200     03 IDDC                 PIC X(2)                                     
000300                             VALUE SPACES.                                
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 IDDISTR              PIC Z(3)9                                    
000700                             VALUE ZEROS.                                 
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 DAVVREG              PIC 9(6)                                     
001100                             VALUE ZEROS.                                 
001200*                                 GÄLLANDE VECKA (ÅÅÅÅVV)                 
001300*                                 BELONGS TO THIS WEEK (YYYYVV)           
001400     03 KDPRODSL             PIC Z9                                       
001500                             VALUE ZEROS.                                 
001600*                                 PRODUKTSLAG                             
001700*                                 PRODUCT GROUP                           
001800     03 SUARTSTD             PIC -(9)9.9(2)                               
001900                             VALUE ZEROS.                                 
002000*                                 SUMMA STANDARDPRIS RADVÄRDE             
002100*                                 SUM LINEVALUE STANDARD PRICE            
002200     03 SUARTSJK             PIC -(9)9.9(2)                               
002300                             VALUE ZEROS.                                 
002400*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
002500*                                 SUM COST OF SALES PART NO               
002600     03 SUARTFSG             PIC -(9)9.9(2)                               
002700                             VALUE ZEROS.                                 
002800*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
002900*                                                                         
003000*                                 SUM SALES AMOUNT PER PART               
003100*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
