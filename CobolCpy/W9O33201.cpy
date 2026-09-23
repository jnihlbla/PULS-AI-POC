000100 01  MOD-W9O33201.                                                        
000200*                                 MODCOPYTEXT TILL W9033200               
000300*                                 ORDER INQUIRY.                          
000400*                                 FRÅGA ORDER ELLER ARTIKEL.              
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-IDMFSFEL         PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 MOD-IDORDNR7         PIC X(7).                                    
001000*                                 ORDERNUMMER                             
001100     03 MOD-IDARTNR          PIC X(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDRADNR-NEXT     PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MOD-IDPRODNR-NEXT    PIC X(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 MOD-IDPLKLST-NEXT    PIC X(3).                                    
001800*                                 PLOCKLISTNUMMER                         
001900     03 MOD-IDKOLLI-NEXT     PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MOD-IDDC-NEXT        PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-FLSVAR-NEXT      PIC X.                                       
002400*                                 ALLMÄN SVARSFLAGGA                      
002500     03 MOD-RAD              OCCURS 11 TIMES.                             
002600        05 MOD-IDARTNR-RAD   PIC 9(8).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-KVBEART       PIC 9(6).                                    
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000        05 MOD-KVAVBART      PIC 9(6).                                    
003100*                                 AVBOKAT ANTAL ARTIKLAR                  
003200        05 MOD-KVLEVART      PIC 9(6).                                    
003300*                                 LEVERERAT ANTAL STYCK                   
003400        05 MOD-IDDC          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600        05 MOD-KDORDSTA      PIC X(2).                                    
003700*                                 VOLVOORDERSTATUS                        
003800        05 MOD-IDKOLLI       PIC 9(5).                                    
003900*                                 KOLLINUMMER                             
004000        05 MOD-KDTPOTYP      PIC X.                                       
004100*                                 TYP AV TIDPLANERAD ORDER                
004200        05 MOD-TIRODAT       PIC 9(6).                                    
004300*                                 RESTORDERDATUM         (ÅÅMMDD)         
004400        05 MOD-IDORDNR7-RO   PIC 9(7).                                    
004500*                                 ORDERNUMMER                             
004600        05 MOD-IDBIL.                                                     
004700*                                 BILIDENTITET                            
004800           07 MOD-IDBILTYP   PIC X(3).                                    
004900*                                 BILTYP                                  
005000           07 MOD-TIAAAA     PIC X(4).                                    
005100*                                 ÅRTAL (ÅÅÅÅ)                            
005200           07 MOD-IDCHASSI-PIE                                            
005300                             PIC X(6).                                    
005400*                                 CHASSINUMMER PIE                        
005500*** END OF VILMAII-COPY LENGTH= 726 BYTES                                 
