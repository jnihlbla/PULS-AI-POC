000100 01  WDD907DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 CALLOFF KIT                             
000400*                                                                         
000500     03 IDSEGM               PIC X(6).                                    
000600*                                 SEGMENT                                 
000700     03 FILLERX2             PIC X(2).                                    
000800     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-KEY             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR-KEY          PIC X(5).                                    
001300*                                 LEVERANT÷RNUMMER                        
001400     03 DAAVROP-KEY          PIC 9(6).                                    
001500*                                 AVROPSVECKA   (≈≈≈≈VV)                  
001600     03 TILEVDAG-KEY         PIC S9              COMP-3.                  
001700*                                 AVSƒNDNINGSDAG INOM VECKA               
001800     03 IDORDNSB-KEY         PIC S9(5)           COMP-3.                  
001900*                                 SATSORDERNUMMER-BAS                     
002000     03 FILLERX16            PIC X(16).                                   
002100     03 WDD907-CTX.                                                       
002200*                                 LEVERANSPLANEREGISTER                   
002300*                                 SATSBEORDRINGSINFORMATION               
002400*                                 FYSISK NYCKEL  IDORDNSB                 
002500        05 IDORDNSB          PIC S9(5)           COMP-3.                  
002600*                                 SATSORDERNUMMER-BAS                     
002700        05 TIBEODAT-SATS     PIC S9(5)           COMP-3.                  
002800*                                 BEORDRINGSDATUM SATS (≈≈VV)             
002900*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
