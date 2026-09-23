000100 01  1-W221L801.                                                          
000200*                                 LÄNKAREA NR 1                           
000300*                                 LÄSNING AV WLXXBK                       
000400     03 1-IDLEVNR            PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 1-IDOVERFNR          PIC S9(5)           COMP-3.                  
000700*                                 ÖVERFÖRINGSNUMMER                       
000800     03 1-FILLER             PIC X(4).                                    
000900     03 1-TISEND-SEN         PIC S9(7)           COMP-3.                  
001000*                                 SENASTE ÖVERFÖRINGSDATUM                
001100     03 1-KDVECKOSL          PIC X.                                       
001200*                                 KOD    VECKOSLUTSSÄNDNING               
001300     03 1-KDEDI              PIC X.                                       
001400*                                 ÖVERFÖRINGSSTANDARD                     
001500     03 1-FLAVIS             PIC X.                                       
001600*                                 LEVERANTÖRSAVISERING                    
001700     03 1-IDOVERFNR-VV       PIC S9(5)           COMP-3.                  
001800*                                 ÖVERFÖRINGSNUMMER                       
001900     03 1-IDLEVKND           PIC X(17).                                   
002000*                                 LEVERANTÖRENS KUNDIDENTITET             
002100     03 1-FLLEVPLP           PIC X.                                       
002200*                                 FLAGGA PERIODSLUTSSÄNDNING              
002300     03 1-FLLEVVB            PIC X.                                       
002400*                                 FLAGGA VECKOSÄNDNING                    
002500     03 1-TISEND-PER         PIC S9(7)           COMP-3.                  
002600*                                 BEGÄRD ÖVERFÖRINGSDATUM                 
002700     03 1-FLODETTE           PIC X.                                       
002800*                                 FAKTURERING SKER VIA ODETTE             
002900*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
