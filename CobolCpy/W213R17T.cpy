000100 01  W213R17T.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDLEVNR              PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 KDBEHX               PIC X.                                       
000700*                                 BEHANDLINGSKOD-X                        
000800     03 DATA-LEVNR-ARTNR.                                                 
000900*                                                                         
001000        05 KVVECKOR-LT       PIC 9(2).                                    
001100*                                 ANTAL VECKOR LEDTID                     
001200        05 KVVECKOR-AT       PIC 9(2).                                    
001300*                                 ANTAL VECKOR ANSKAFFNINGSTID            
001400        05 KVDAGAR-TTC1      PIC 9(2).                                    
001500*                                 DAGAR TULL- OCH TRANSPORT-TID           
001600*                                 C1                                      
001700        05 KVDAGAR-TTC2      PIC 9(2).                                    
001800*                                 DAGAR TULL- & TRANSPORT-TID  C2         
001900        05 KDLEVTYP          PIC 9.                                       
002000*                                 LEVERANTÖRTYP                           
002100        05 KDGK              PIC 9.                                       
002200*                                 GODSMOTTAGAREKOD                        
002300     03 DATA-ENDAST-LEVNR.                                                
002400*                                                                         
002500        05 IDLPKOLL          PIC 9.                                       
002600*                                 KONTROLLVECKA LEVERANSPLANER            
002700        05 FLRSADR           PIC X.                                       
002800*                                 RS-UNIK LEV-ADRESS                      
002900        05 FLEMBPOL          PIC X.                                       
003000*                                 MEDLEM EMBALLAGEPOOL                    
003100        05 KDSPRAK           PIC 9.                                       
003200*                                 SPRÅKKOD                                
003300*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
