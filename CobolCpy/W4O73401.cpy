000100 01  W4O73401.                                                            
000200*                                 MODCOPYTEXT TILL W40734.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDANSV-IN            PIC X(6).                                    
000800     03 IDRT-IN              PIC X(3).                                    
000900*                                 RETURTERMINAL                           
001000     03 IDRTLOP-IN           PIC X(3).                                    
001100*                                 RETUR TERMINAL LÖPNUMMER                
001200     03 IDKOLLI-IN           PIC X(5).                                    
001300*                                 KOLLINUMMER                             
001400     03 IDDISTR-IN           PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 FLVISAAV-IN          PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800     03 IDANSV-UT            PIC X(6).                                    
001900     03 IDRT-UT              PIC X(3).                                    
002000*                                 RETURTERMINAL                           
002100     03 IDRTLOP-UT           PIC X(3).                                    
002200*                                 RETUR TERMINAL LÖPNUMMER                
002300     03 IDKOLLI-UT           PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 IDDISTR-UT           PIC X(4).                                    
002600*                                 DISTRIKTNUMMER                          
002700     03 FLVISAAV-UT          PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 IDANSTNR-ATTR        PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 IDANSTNR             PIC X(5).                                    
003200*                                 ANSTÄLLNINGSNUMMER                      
003300     03 RADER                OCCURS 13 TIMES.                             
003400*                                                                         
003500        05 KDCMD-ATTR        PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 KDCMD             PIC X(4).                                    
003800        05 IDSNDNR-ATTR      PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 IDRT              PIC X(3).                                    
004100*                                 RETURTERMINAL                           
004200        05 IDRTLOP           PIC X(3).                                    
004300*                                 RETUR TERMINAL LÖPNUMMER                
004400        05 IDKOLLI-ATTR      PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 IDKOLLI           PIC Z(4)9.                                   
004700*                                 KOLLINUMMER                             
004800        05 FLFARLIG-ATTR     PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 FLFARLIG          PIC X.                                       
005100*                                 FARLIGT GODS-FLAGGA                     
005200        05 TIRETANK-ATTR     PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 TIRETANK          PIC 9(6).                                    
005500*                                 ANKOMSTDATUM                            
005600        05 ADINLOMR-ATTR     PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 ADINLOMR          PIC X(4).                                    
005900*                                 INLEVERANSOMRÅDE                        
006000        05 BESTATUS-ATTR     PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 BESTATUS          PIC X(4).                                    
006300     03 TEMFSINF             PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 722 BYTES                                 
