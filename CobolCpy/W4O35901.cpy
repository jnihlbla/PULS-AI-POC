000100 01  MOD-W4O35901.                                                        
000200*                                 MOD-COPYTEXT PGM W40359                 
000300*                                 ANNULLATION AV UTSKRIVEN ORDER          
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDPRODNR-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDPRODNR-UT      PIC X(7).                                    
001500*                                 PRODUKTIONSNUMMER                       
001600     03 MOD-IDPLKLST-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDPLKLST-UT      PIC X(3).                                    
001900*                                 PLOCKLISTNUMMER                         
002000     03 MOD-IDRADNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDRADNR-UT       PIC X(4).                                    
002300*                                 RADNUMMER                               
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MOD-IDDISTR          PIC Z(3)9.                                   
002900*                                 DISTRIKTNUMMER                          
003000     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
003100*                                 KUNDNUMMER                              
003200     03 MOD-IDORDNR5         PIC Z(4)9.                                   
003300*                                 ORDERNUMMER                             
003400     03 MOD-IDDC             PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-KDORDKL          PIC X.                                       
003700*                                 ORDERKLASS                              
003800     03 MOD-KVORDRAD         PIC Z(4)9.                                   
003900*                                 ANTAL ORDERRADER                        
004000     03 MOD-KVORDRAD-PACK    PIC Z(4)9.                                   
004100*                                 ANTAL PACKADE ORDERRADER                
004200     03 MOD-KVAVBART         PIC Z(5)9.                                   
004300*                                 AVBOKAT ANTAL ARTIKLAR                  
004400     03 MOD-FLSVAR-ATTR      PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-FLSVAR           PIC X.                                       
004700*                                 ALLMÄN SVARSFLAGGA                      
004800     03 MOD-IDRADNR-FOM-ATTR PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDRADNR-FOM      PIC X(4).                                    
005100*                                 RADNUMMER                               
005200     03 MOD-IDRADNR-TOM-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDRADNR-TOM      PIC X(4).                                    
005500*                                 RADNUMMER                               
005600     03 MOD-KVANNANT-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KVANNANT         PIC Z(5)9.                                   
005900*                                 ANNULLERAT ANTAL ARTIKLAR               
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
006200*** END OF VILMAII-COPY LENGTH= 186 BYTES                                 
