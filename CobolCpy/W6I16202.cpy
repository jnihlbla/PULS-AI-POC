000100 01  REQU-W6I16202.                                                       
000200*                                 COPYBOOK FOR BUFFERSALDO API            
000300     03 REQU-IDPTYP-SYNQ     PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 REQU-IDARTNR-SYNQ    PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 REQU-KDTECKEN-BUFF-F-SYNQ                                         
000800                             PIC X.                                       
000900*                                 PLUS ELLER MINUS (+ -)                  
001000     03 REQU-KVBUFF-F-SYNQ   PIC 9(7).                                    
001100*                                 FÖRÄDLAT BUFFERSALDO                    
001200     03 REQU-KDTECKEN-BUFF-OF-SYNQ                                        
001300                             PIC X.                                       
001400*                                 PLUS ELLER MINUS (+ -)                  
001500     03 REQU-KVBUFF-OF-SYNQ  PIC 9(7).                                    
001600*                                 BUFFERSALDO OFÖRÄDLAT GODS              
001700     03 REQU-KDTECKEN-KLI-F-SYNQ                                          
001800                             PIC X.                                       
001900*                                 PLUS ELLER MINUS (+ -)                  
002000     03 REQU-KVKOLLI-F-SYNQ  PIC 9(4).                                    
002100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002200     03 REQU-KDTECKEN-KLI-OF-SYNQ                                         
002300                             PIC X.                                       
002400*                                 PLUS ELLER MINUS (+ -)                  
002500     03 REQU-KVKOLLI-OF-SYNQ PIC 9(4).                                    
002600*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
002700     03 REQU-IDLEVNR-KOLLI-SYNQ                                           
002800                             PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER KOLLI                  
003000     03 REQU-IDOKOLLI-SYNQ   PIC 9(9).                                    
003100*                                 ODETTE KOLLINUMMER                      
003200*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
