000100 01  W4O69401-CTX.                                                        
000200*                                 MOD-COPYTEXT (KOMPLETTERING)            
000300*                                 FÖR W40694                              
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-UT           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR-UT          PIC Z(5)9.                                   
001100*                                 KUNDNUMMER                              
001200     03 KDFRAKT-UT           PIC X(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 IDDC-UT              PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 FLLANDROVER          PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800     03 BEGMT-RAD1           PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 1                  
002000     03 BEGMT-RAD2           PIC X(35).                                   
002100*                                 GODSMOTTAGARNAMN RAD 2                  
002200     03 ADGMT-RAD1           PIC X(35).                                   
002300*                                 GODSMOTTAGARADRESS RAD 1                
002400     03 ADGMT-RAD2           PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS RAD 2                
002600     03 BEGMRK-RAD1          PIC X(30).                                   
002700*                                 GODSMÄRKE  RAD1                         
002800     03 IDDISTR              PIC Z(3)9.                                   
002900*                                 DISTRIKTNUMMER                          
003000     03 IDKUNDNR             PIC Z(5)9.                                   
003100*                                 KUNDNUMMER                              
003200     03 KDFRAKT              PIC Z9.                                      
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 IDORDNR              OCCURS 13 TIMES                              
003500                             PIC Z(5).                                    
003600*                                 ORDERNUMMER UTGÅR PD90                  
003700     03 IDPRODNR             OCCURS 13 TIMES                              
003800                             PIC Z(7).                                    
003900*                                 PRODUKTIONSNUMMER                       
004000     03 IDAVD-ATTR           PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 IDAVD                PIC X(5).                                    
004300*                                 DEN ANSTÄLLDES AVDELNING/               
004400*                                 KOSTNADSSTÄLLE                          
004500     03 IDDISTR-BET-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 IDDISTR-BET          PIC X(4).                                    
004800*                                 DISTRIKTNUMMER                          
004900     03 FLMOTBET-ATTR        PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 FLMOTBET             PIC X.                                       
005200*                                 MOTTAGAREN BETALAR FRAKTEN              
005300     03 TEMFSINF             PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 468 BYTES                                 
