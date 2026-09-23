000100 01  MOD-W4O62401.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDDISTR-IN       PIC X(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 MOD-IDDISTR-UT       PIC X(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001300*                                 CUSTOMER NO                             
001400     03 MOD-IDSHIPM-IN       PIC 9(7).                                    
001500*                                 SHIPMENT NO                             
001600     03 MOD-IDSHIPM-UT       PIC Z(7).                                    
001700*                                 SHIPMENT NO                             
001800     03 MOD-IDTRPTNR-IN      PIC X(3).                                    
001900*                                 TRANSPORT IDENTITY                      
002000     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
002100*                                 TRANSPORT IDENTITY                      
002200     03 MOD-IDDC-IN          PIC X(2).                                    
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 MOD-IDDC-UT          PIC X(2).                                    
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 MOD-TOTAL.                                                        
002700        05 MOD-KVKOLLI-SKP   PIC Z(4)9.                                   
002800*                                 NO OF CASES SHIPMENT                    
002900        05 MOD-VKORDBTO-SKP  PIC Z(5)9.9.                                 
003000*                                 GR WEIGHT/SHIPMENT                      
003100        05 MOD-VLORDBTO-SKP  PIC Z(3)9.9(3).                              
003200*                                 GROSS VOL/SHIPMENT                      
003300        05 MOD-SUORDV-SKEPPN PIC Z(8)9.9(2).                              
003400*                                 ORDER VALVE SHIPPED                     
003500        05 MOD-TEASTRIX-TRP  PIC X.                                       
003600*                                 ASTERISK                                
003700        05 MOD-VKORDNTO-SKP  PIC Z(5)9.9.                                 
003800*                                 ORDER WGT NT/SHIPPING                   
003900     03 MOD-AREA             OCCURS 6 TIMES.                              
004000        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004100*                                 CUSTOMER NO                             
004200        05 MOD-IDORDNR5      PIC Z(4)9.                                   
004300*                                 ORDER NUMBER                            
004400        05 MOD-IDKOLLI       PIC Z(4)9.                                   
004500*                                 CASE NUMBER                             
004600        05 MOD-IDTULL.                                                    
004700*                                 IDENTITY CUSTOMS TRANSMISSION           
004800           07 MOD-IDTULFTG   PIC X(2).                                    
004900*                                 IDENTIFIER COMPANY TO CUSTOMS           
005000           07 MOD-IDTULLNR   PIC X(7).                                    
005100*                                 SERIAL NUMBER IN CUSTOMS ID             
005200           07 MOD-RETULKS    PIC X.                                       
005300*                                 CHECK DIGIT CUSTOMS ID                  
005400        05 MOD-DIKOLLIL      PIC Z(3)9.                                   
005500*                                 CASE LENGTH                             
005600        05 MOD-DIKOLLIB      PIC Z(2)9.                                   
005700*                                 CASE WIDTH                              
005800        05 MOD-DIKOLLIH      PIC Z(2)9.                                   
005900*                                 CASE HEIGHT                             
006000        05 MOD-VKORDBTO-KOLLI                                             
006100                             PIC Z(5)9.9.                                 
006200*                                 ORDER WEIGHT GROSS PER CASE             
006300        05 MOD-VLORDBTO-KOLLI                                             
006400                             PIC Z(3)9.9(3).                              
006500*                                 ORDER VOL GR/CASE                       
006600        05 MOD-SUORDV-KOLLI  PIC Z(8)9.9(2).                              
006700*                                 ORDER VALUE PER CASE                    
006800        05 MOD-TEASTRIX-KLI  PIC X.                                       
006900*                                 ASTERISK                                
007000        05 MOD-VKORDNTO-KOLLI                                             
007100                             PIC Z(5)9.9.                                 
007200*                                 ORDER WEIGHT NET PER CASE               
007300        05 MOD-FLFARLIG      PIC X.                                       
007400*                                 DENGEROUS GOODS FLAG                    
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATION MESSAGE                     
007700*** END OF VILMAII-COPY LENGTH= 629 BYTES                                 
