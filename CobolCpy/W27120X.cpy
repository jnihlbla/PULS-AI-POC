000100 01  W27120X.                                                             
000200*                                 ARTIKLAR TILL UPPFÖLJNING               
000300     03 IDARTNR              PIC Z(7)9                                    
000400                             VALUE ZEROS.                                 
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 IDDC                 PIC X(2)                                     
000800                             VALUE SPACES.                                
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 KDPRODSL             PIC Z9                                       
001200                             VALUE ZEROS.                                 
001300*                                 PRODUKTSLAG                             
001400*                                 PRODUCT GROUP                           
001500     03 KDREFSTA             PIC X                                        
001600                             VALUE SPACE.                                 
001700*                                 STATUS REFILLARTIKEL                    
001800*                                 STATUS REFILLPART                       
001900     03 KLASS                PIC X(3)                                     
002000                             VALUE SPACES.                                
002100     03 PRARTBES             PIC Z(6)9.9(2)                               
002200                             VALUE ZEROS.                                 
002300*                                 BESTÄLLNINGSPRIS I KRONOR               
002400*                                 ORDER PRICE SWEDISH CURRENCY            
002500     03 PRARTSTD             PIC Z(6)9.9(2)                               
002600                             VALUE ZEROS.                                 
002700*                                 ARTIKELSTANDARDPRIS                     
002800*                                 STANDARD PRICE                          
002900     03 FLWILSON             PIC X                                        
003000                             VALUE SPACE.                                 
003100*                                 WILSONFORMEL                            
003200*                                 FLAG TO USE WILSON OR NOT               
003300     03 IDREFTAB             PIC X                                        
003400                             VALUE SPACE.                                 
003500*                                 IDENTITET REFILLTABELL                  
003600*                                 REFILLINGTABLE IDENTIFIER               
003700     03 PRMATRL              PIC Z(6)9.9(2)                               
003800                             VALUE ZEROS.                                 
003900*                                 FAST PRIS UNDER LÖPANDE ÅR              
004000*                                 MATERIAL PRICE FOR ACTUAL YEAR          
004100*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
