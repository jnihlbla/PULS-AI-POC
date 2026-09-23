000100 01  W51716X1.                                                            
000200*                                 A COPY OF W51716X1 - TO CREATE          
000300*                                 WXTR FILE IN EDITABLE FORMAT            
000400     03 IDDC                 PIC X(2)                                     
000500                             VALUE SPACES.                                
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 KDPRODSL             PIC Z9                                       
000900                             VALUE ZEROS.                                 
001000*                                 PRODUKTSLAG                             
001100*                                 PRODUCT GROUP                           
001200     03 DAVVREG              PIC 9(6)                                     
001300                             VALUE ZEROS.                                 
001400*                                 GÄLLANDE VECKA (ÅÅÅÅVV)                 
001500*                                 BELONGS TO THIS WEEK (YYYYVV)           
001600     03 SUAKSV               PIC -(9)9.9(2)                               
001700                             VALUE ZEROS.                                 
001800*                                 VÄRDE AV ANKOMSTSALDO                   
001900     03 SULSV                PIC -(12)9.9(2)                              
002000                             VALUE ZEROS.                                 
002100*                                 LAGERVÄRDE                              
002200     03 SUEFRV               PIC -(9)9.9(2)                               
002300                             VALUE ZEROS.                                 
002400*                                 SUMMA EJ FAKTURERAT VÄRDE               
002500     03 SURESSV              PIC -(9)9.9(2)                               
002600                             VALUE ZEROS.                                 
002700*                                 SUMMA RESERVERAT LAGERVÄRDE             
002800*                                 ACCUM RESERVED STOCK VALUE              
002900     03 SUOKSV               PIC -(9)9.9(2)                               
003000                             VALUE ZEROS.                                 
003100*                                 SUMMA VÄRDE AV ORDERKÖSALDO             
003200*                                 ACCUM. VALUE OF ORDER Q BAL.            
003300*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
