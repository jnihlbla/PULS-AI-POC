000100 01  W4O67301.                                                            
000200*                                 MODCOPYTEXT TILL W40673.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDTRPTNR-IN          PIC X(3).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 IDTRPTNR-UT          PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 IDLBBET-IN           PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 IDLBBET-UT           PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 FLFARLIG-IN          PIC X.                                       
001600*                                 FARLIGT GODS-FLAGGA                     
001700     03 FLFARLIG-UT          PIC X.                                       
001800*                                 FARLIGT GODS-FLAGGA                     
001900     03 IDDC-IN              PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 IDDC-UT              PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 SUMMOR.                                                           
002400*                                                                         
002500        05 KVKOLLI-VALD      PIC Z(3)9.                                   
002600*                                 ANTAL KOLLI                             
002700        05 VKORDBTO-VALD     PIC Z(5)9.9.                                 
002800*                                 ORDERVIKT BRUTTO (KG)                   
002900        05 VLORDBTO-VALD     PIC Z(3)9.9(3).                              
003000*                                 ORDERVOLYM BRUTTO (M3)                  
003100     03 UPDRAD               OCCURS 26 TIMES.                             
003200*                                                                         
003300        05 IDDISTR-UPD-ATTR  PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 IDDISTR-UPD       PIC 9(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700        05 IDKUNDNR-UPD-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 IDKUNDNR-UPD      PIC 9(6).                                    
004000*                                 KUNDNUMMER                              
004100        05 IDORDNR7-UPD-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 IDORDNR7-UPD      PIC 9(7).                                    
004400*                                 ORDERNUMMER                             
004500        05 IDKOLLI-UPD-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 IDKOLLI-UPD       PIC 9(5).                                    
004800*                                 KOLLINUMMER                             
004900     03 TEMFSINF             PIC X(55).                                   
005000*                                 INFORMATIONSMEDDELANDE                  
005100*** END OF VILMAII-COPY LENGTH= 935 BYTES                                 
