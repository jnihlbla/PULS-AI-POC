000100 01  MOD-W3O31801.                                                        
000200*                                 MODCOPYTEXT TILL W3O318.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPROMR-IN.                                                   
000800*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
000900        05 MOD-IDMARKBO      PIC X.                                       
001000*                                 MARKNADSBOLAGSKOD                       
001100        05 MOD-IDPROMRN      PIC X(2).                                    
001200*                                 PRISOMRÅDE LÖPNUMMER                    
001300     03 MOD-IDPROMR-UT.                                                   
001400*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001500        05 MOD-IDMARKBO      PIC X.                                       
001600*                                 MARKNADSBOLAGSKOD                       
001700        05 MOD-IDPROMRN      PIC X(2).                                    
001800*                                 PRISOMRÅDE LÖPNUMMER                    
001900     03 MOD-TEXT1            PIC X(12).                                   
002000     03 MOD-REARTRAB-IN      PIC X(2).                                    
002100     03 MOD-TISTADAT-IN      PIC X(6).                                    
002200*                                 GENERELLT STARTDATUM                    
002300     03 MOD-REARTRAB-ATTR    PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-REARTRAB-UT      PIC X(2).                                    
002600     03 MOD-TEMFSINF         PIC X(55).                                   
002700*                                 INFORMATIONSMEDDELANDE                  
002800*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
