000100 01  MOD-W2O35701.                                                        
000200*                                 MOD-COPYTEXT FÖR W20357                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-UT-AREA.                                                      
001600*                                 UTDATA 2357                             
001700        05 MOD-BEART-ENG     PIC X(25).                                   
001800*                                 ENGELSK ARTIKELBENÄMNING                
001900        05 MOD-UT-GRP        OCCURS 4 TIMES.                              
002000*                                 UTDATA 2357                             
002100           07 MOD-IDDC-GRP   PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300           07 MOD-IDLEVNR-NDC                                             
002400                             PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600           07 MOD-KDARTURS-NDC                                            
002700                             PIC X(2).                                    
002800*                                 ARTIKELURSPRUNGSKOD                     
002900           07 MOD-FLORDSP-EJRO                                            
003000                             PIC X.                                       
003100*                                 ORDERSPÄRR EJ RESTNOTERING              
003200           07 MOD-DAORDSP-EJRO                                            
003300                             PIC 9(6).                                    
003400*                                 ANGER DATUM NÄR FLAGGA FÖR ORDE         
003500*                                 RSPÄRR UTAN RESTNOTERING ÄNDRAS         
003600*                                 YYYYMMDD                                
003700           07 MOD-IDUSER-ORDSP-EJRO                                       
003800                             PIC X(8).                                    
003900*                                 ANVÄNDAR-ID ORDERSPÄRR UTAN RES         
004000*                                 TNOTERING                               
004100           07 MOD-IDPSN-DC   PIC Z(2)9.                                   
004200*                                 PROPER SHIPPING NAME PER XDC            
004300           07 MOD-DAPUBL     PIC Z(5).                                    
004400*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004500           07 MOD-IDPERSON   PIC Z(3).                                    
004600*                                 PERSONKOD                               
004700        05 MOD-TIFINLV       PIC Z(5).                                    
004800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004900        05 MOD-KDARTURS-CDC  PIC X(2).                                    
005000*                                 ARTIKELURSPRUNGSKOD                     
005100        05 MOD-IDPSN         PIC Z(2)9.                                   
005200*                                 PROPER SHIPPING NAME                    
005300     03 MOD-IN-AREA.                                                      
005400*                                 INDATA 2357                             
005500        05 MOD-IN-GRP        OCCURS 4 TIMES.                              
005600*                                 INDATA 2357                             
005700           07 MOD-KDCMD-IN-ATTR                                           
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000           07 MOD-KDCMD-IN   PIC X(2).                                    
006100*                                 MFS BEHANDLING AV INPUTFÄLT             
006200           07 MOD-FLORDSP-EJRO-IN-ATTR                                    
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500           07 MOD-FLORDSP-EJRO-IN                                         
006600                             PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800           07 MOD-IDPSN-IN-ATTR                                           
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100           07 MOD-IDPSN-IN   PIC 9(3).                                    
007200*                                 PROPER SHIPPING NAME                    
007300           07 MOD-DAPUBL-IN-ATTR                                          
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600           07 MOD-DAPUBL-IN  PIC 9(5).                                    
007700*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007800     03 MOD-TEMFSINF         PIC X(55).                                   
007900*                                 INFORMATIONSMEDDELANDE                  
008000*** END OF VILMAII-COPY LENGTH= 376 BYTES                                 
