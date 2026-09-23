000100 01  MOD-W5O30801.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5308              
000300*                                 AUTOMATJUSTERING                        
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLISTNR-IN      PIC 9(6).                                    
000900     03 MOD-IDUSER-IN        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MOD-IDDC-UT          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDLISTNR-UT      PIC 9(6).                                    
001400     03 MOD-IDUSER-UT        PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600     03 MOD-OK-ATTR          PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-OK               PIC X.                                       
001900     03 MOD-TABELLRAD        OCCURS 10 TIMES.                             
002000*                                 GRUPP MED TABELL RADER                  
002100        05 MOD-IDARTNR       PIC Z(8)9.                                   
002200*                                 ARTIKELNUMMER                           
002300        05 MOD-BEART-SVE     PIC X(14).                                   
002400        05 MOD-KVAKS         PIC Z(6)9-.                                  
002500*                                 ANKOMSTSALDO                            
002600        05 MOD-KVLS          PIC Z(6)9-.                                  
002700*                                 LAGERSALDO                              
002800        05 MOD-KVEFRS        PIC Z(6)9-.                                  
002900*                                 EJ FAKTURERAT ANTAL STYCK               
003000        05 MOD-ANTAL-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-ANTAL         PIC Z(5)9.                                   
003300*                                 ANTAL                                   
003400        05 MOD-TECKEN-ATTR   PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-TECKEN        PIC X.                                       
003700        05 MOD-DIFFERANS-ATTR                                             
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-DIFFERANS     PIC Z(5)9.                                   
004100*                                 ANTAL                                   
004200        05 MOD-FLOMINV-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-FLOMINV-UT    PIC X.                                       
004500*                                 ALLMÄN FLAGGA                           
004600        05 MOD-FELRAD-UT     PIC X.                                       
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 832 BYTES                                 
