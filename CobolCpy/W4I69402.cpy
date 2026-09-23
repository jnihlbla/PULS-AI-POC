000100 01  W4I69402-CTX.                                                        
000200*                                 MID-COPYTEXT (FRÅN EGEN BILD)           
000300*                                 FÖR W40694                              
000400     03 IDDISTR-UT           PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR-UT          PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 KDFRAKT-UT           PIC X(2).                                    
000900*                                 FRAKTSÄTT DC TILL KUND                  
001000     03 IDDC-UT              PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 FLLANDROVER          PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400     03 BEGMT-RAD1           PIC X(35).                                   
001500*                                 GODSMOTTAGARNAMN RAD 1                  
001600     03 BEGMT-RAD2           PIC X(35).                                   
001700*                                 GODSMOTTAGARNAMN RAD 2                  
001800     03 ADGMT-RAD1           PIC X(35).                                   
001900*                                 GODSMOTTAGARADRESS RAD 1                
002000     03 ADGMT-RAD2           PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS RAD 2                
002200     03 BEGMRK-RAD1          PIC X(30).                                   
002300*                                 GODSMÄRKE  RAD1                         
002400     03 IDDISTR              PIC X(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600     03 IDKUNDNR             PIC X(6).                                    
002700*                                 KUNDNUMMER                              
002800     03 KDFRAKT              PIC X(2).                                    
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000     03 IDORDNR              OCCURS 13 TIMES                              
003100                             PIC 9(5).                                    
003200*                                 ORDERNUMMER UTGÅR PD90                  
003300     03 IDPRODNR             OCCURS 13 TIMES                              
003400                             PIC 9(7).                                    
003500*                                 PRODUKTIONSNUMMER                       
003600     03 IDAVD                PIC 9(5).                                    
003700*                                 DEN ANSTÄLLDES AVDELNING/               
003800*                                 KOSTNADSSTÄLLE                          
003900     03 IDDISTR-BET          PIC 9(4).                                    
004000*                                 DISTRIKTNUMMER                          
004100     03 FLMOTBET             PIC X.                                       
004200*                                 MOTTAGAREN BETALAR FRAKTEN              
004300*** END OF VILMAII-COPY LENGTH= 363 BYTES                                 
