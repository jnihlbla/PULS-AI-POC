000100 01  W4O73501.                                                            
000200*                                 MODCOPYTEXT TILL W40735.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDSNDNNR-IN          PIC X(6).                                    
000800     03 IDKOLLI-IN           PIC X(5).                                    
000900*                                 KOLLINUMMER                             
001000     03 IDDISTR-IN           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR-IN          PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 IDRAPPNR-IN          PIC X(7).                                    
001500*                                 RAPPORT NUMMER                          
001600     03 IDSNDNNR-UT          PIC X(6).                                    
001700     03 IDKOLLI-UT           PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 IDDISTR-UT           PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 IDKUNDNR-UT          PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 IDRAPPNR-UT          PIC X(7).                                    
002400*                                 RAPPORT NUMMER                          
002500     03 INPUT.                                                            
002600*                                                                         
002700        05 IDPRT-ATTR        PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 IDPRT-UPD         PIC X(3).                                    
003000*                                 LOGISK PRINTERIDENTITET                 
003100        05 IDANSTNR-ATTR     PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 IDANSTNR          PIC Z(4)9.                                   
003400*                                 ANSTÄLLNINGSNUMMER                      
003500     03 RADER                OCCURS 13 TIMES.                             
003600*                                                                         
003700        05 KDCMD-ATTR        PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 KDCMD             PIC X(4).                                    
004000        05 IDKOLLI           PIC Z(4)9.                                   
004100*                                 KOLLINUMMER                             
004200        05 IDDISTR           PIC Z(3)9.                                   
004300*                                 DISTRIKTNUMMER                          
004400        05 IDKUNDNR          PIC Z(5)9.                                   
004500*                                 KUNDNUMMER                              
004600        05 IDRAPPNR          PIC Z(6)9.                                   
004700*                                 RAPPORT NUMMER                          
004800        05 KVKOLLI           PIC Z(3)9.                                   
004900*                                 ANTAL KOLLI                             
005000        05 FLFARLIG          PIC X.                                       
005100*                                 FARLIGT GODS-FLAGGA                     
005200        05 IDFRASED-AAF      PIC X(15).                                   
005300*                                 FRAKTSEDELSNUMMER                       
005400        05 TERETNOT-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 TERETNOT          PIC X(18).                                   
005700        05 KVKOLLI-DEL       PIC Z(3)9.                                   
005800*                                 ANTAL KOLLI                             
005900        05 KDTRSTAT-ATTR     PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 KDLEVANM          PIC X.                                       
006200*                                 STATUS LEVERANSANMÄRKNING               
006300     03 TEMFSINF             PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 1142 BYTES                                
