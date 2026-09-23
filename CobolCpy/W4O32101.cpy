000100 01  MOD-W4O32101.                                                        
000200*                                 MODCOPYTEXT TILL W40321.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001000*                                 ANSTÄLLNINGSNUMMER                      
001100     03 MOD-IDDISTR-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDORDNR-UT       PIC X(5).                                    
002200*                                 ORDERNUMMER                             
002300     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-NYCKEL.                                                       
003600*                                                                         
003700        05 MOD-IDPRODNR-NYCKEL                                            
003800                             PIC X(7).                                    
003900*                                 PRODUKTIONSNUMMER                       
004000        05 MOD-IDANSTNR-NYCKEL                                            
004100                             PIC X(5).                                    
004200*                                 ANSTÄLLNINGSNUMMER                      
004300        05 MOD-IDRADNR-ORD-TOM-NYCKEL                                     
004400                             PIC X(4).                                    
004500*                                 RADNUMMER PÅ VOLVOORDER TOM             
004600     03 MOD-RAD              OCCURS 26 TIMES.                             
004700*                                                                         
004800        05 MOD-ADPACOMR      PIC X(2).                                    
004900*                                 PACKNINGSOMRÅDE                         
005000        05 MOD-IDRADNR-ORD-FROM                                           
005100                             PIC Z(3)9.                                   
005200*                                 RADNUMMER PÅ VOLVOORDER FROM            
005300        05 MOD-IDRADNR-ORD-TOM                                            
005400                             PIC Z(3)9.                                   
005500*                                 RADNUMMER PÅ VOLVOORDER TOM             
005600        05 MOD-KVORAPP       PIC Z(5)9.                                   
005700*                                 EJ-RAPPORTERAT-ANTAL                    
005800        05 MOD-IDANSTNR      PIC Z(4)9.                                   
005900*                                 ANSTÄLLNINGSNUMMER                      
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
