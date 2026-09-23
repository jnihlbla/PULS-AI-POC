000100 01  RESP-WL0182O1.                                                       
000200     03 RESP-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000500*                                 DISTRICT NUMBER                         
000600     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000700*                                 CUSTOMER NO                             
000800     03 RESP-IDSHIPM-KEY     PIC Z(7).                                    
000900*                                 SHIPMENT NO                             
001000     03 RESP-IDTRPTNR-KEY    PIC Z(3).                                    
001100*                                 TRANSPORT IDENTITY                      
001200     03 RESP-TOTAL.                                                       
001300        05 RESP-KVKOLLI-SKP  PIC Z(3)9.                                   
001400*                                 NO OF CASES SHIPMENT                    
001500        05 RESP-VKORDBTO-SKP PIC Z(5)9.9.                                 
001600*                                 GR WEIGHT/SHIPMENT                      
001700        05 RESP-VLORDBTO-SKP PIC Z(3)9.9(3).                              
001800*                                 GROSS VOL/SHIPMENT                      
001900        05 RESP-SUORDV-SKEPPN                                             
002000                             PIC Z(8)9.9(2).                              
002100*                                 ORDER VALVE SHIPPED                     
002200        05 RESP-TEASTRIX-TRP PIC X.                                       
002300*                                 ASTERISK                                
002400        05 RESP-VKORDNTO-SKP PIC Z(5)9.9.                                 
002500*                                 ORDER WGT NT/SHIPPING                   
002600     03 RESP-KVRADER         PIC Z(4)9.                                   
002700*                                 NUMBER OF LINES                         
002800     03 RESP-AREA1           OCCURS 500 TIMES.                            
002900        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
003000*                                 CUSTOMER NO                             
003100        05 RESP-IDORDNR5     PIC Z(4)9.                                   
003200*                                 ORDER NUMBER                            
003300        05 RESP-IDKOLLI      PIC Z(4)9.                                   
003400*                                 CASE NUMBER                             
003500        05 RESP-IDTULL.                                                   
003600*                                 IDENTITY CUSTOMS TRANSMISSION           
003700           07 RESP-IDTULFTG  PIC X(2).                                    
003800*                                 IDENTIFIER COMPANY TO CUSTOMS           
003900           07 RESP-IDTULLNR  PIC X(7).                                    
004000*                                 SERIAL NUMBER IN CUSTOMS ID             
004100           07 RESP-RETULKS   PIC X.                                       
004200*                                 CHECK DIGIT CUSTOMS ID                  
004300        05 RESP-DIKOLLIL     PIC Z(3)9.                                   
004400*                                 CASE LENGTH                             
004500        05 RESP-DIKOLLIB     PIC Z(2)9.                                   
004600*                                 CASE WIDTH                              
004700        05 RESP-DIKOLLIH     PIC Z(2)9.                                   
004800*                                 CASE HEIGHT                             
004900        05 RESP-VKORDBTO-KOLLI                                            
005000                             PIC Z(5)9.9.                                 
005100*                                 ORDER WEIGHT GROSS PER CASE             
005200        05 RESP-VLORDBTO-KOLLI                                            
005300                             PIC Z(3)9.9(3).                              
005400*                                 ORDER VOL GR/CASE                       
005500        05 RESP-SUORDV-KOLLI PIC Z(8)9.9(2).                              
005600*                                 ORDER VALUE PER CASE                    
005700        05 RESP-TEASTRIX-KLI PIC X.                                       
005800*                                 ASTERISK                                
005900        05 RESP-VKORDNTO-KOLLI                                            
006000                             PIC Z(5)9.9.                                 
006100*                                 ORDER WEIGHT NET PER CASE               
006200        05 RESP-FLFARLIG     PIC X.                                       
006300*                                 DENGEROUS GOODS FLAG                    
006400*** END OF VILMAII-COPY LENGTH= 37068 BYTES                               
