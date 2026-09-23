000100 01  MOD-W4O31201.                                                        
000200*                                 MODCOPYTEXT TILL W40312.                
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
003500     03 MOD-IDPRODNR-NEXT    PIC Z(6)9.                                   
003600*                                 PRODUKTIONSNUMMER                       
003700     03 MOD-IDPLKLST-NEXT    PIC Z(2)9.                                   
003800*                                 PLOCKLISTNUMMER                         
003900     03 MOD-KDPRCGRP-NEXT    PIC X(5).                                    
004000*                                 PRODUKTIONSKANALSGRUPP                  
004100     03 MOD-TIRFS-NEXT       PIC 9(10).                                   
004200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004300     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
004400*                                 PRODUKTIONSNUMMER                       
004500     03 MOD-IDPLKLST-ENTER   PIC 9(3).                                    
004600*                                 PLOCKLISTNUMMER                         
004700     03 MOD-KDPRCGRP-ENTER   PIC X(5).                                    
004800*                                 PRODUKTIONSKANALSGRUPP                  
004900     03 MOD-TIRFS-ENTER      PIC 9(10).                                   
005000*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005100     03 MOD-KDPRCGRP-4324    PIC X(5).                                    
005200*                                 PRODUKTIONSKANALSGRUPP                  
005300     03 MOD-RAD              OCCURS 14 TIMES.                             
005400*                                                                         
005500        05 MOD-IDDISTR       PIC Z(3)9.                                   
005600*                                 DISTRIKTNUMMER                          
005700        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
005800*                                 KUNDNUMMER                              
005900        05 MOD-IDORDNR       PIC Z(4)9.                                   
006000*                                 ORDERNUMMER                             
006100        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDPRODNR      PIC Z(6)9.                                   
006400*                                 PRODUKTIONSNUMMER                       
006500        05 MOD-IDPLKLST      PIC Z(2)9.                                   
006600*                                 PLOCKLISTNUMMER                         
006700        05 MOD-IDRADNR-ORD-FROM                                           
006800                             PIC Z(3)9.                                   
006900*                                 RADNUMMER PÅ VOLVOORDER FROM            
007000        05 MOD-KDASTERISK    PIC X(2).                                    
007100        05 MOD-IDRADNR-ORD-TOM-ATTR                                       
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDRADNR-ORD-TOM                                            
007500                             PIC Z(3)9.                                   
007600*                                 RADNUMMER PÅ VOLVOORDER TOM             
007700        05 MOD-IDANSTNR-ATTR PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-IDANSTNR      PIC Z(4)9.                                   
008000*                                 ANSTÄLLNINGSNUMMER                      
008100        05 MOD-IDANSTNR-NEW-ATTR                                          
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDANSTNR-NEW  PIC Z(4)9.                                   
008500*                                 ANSTÄLLNINGSNUMMER                      
008600     03 MOD-TEMFSINF         PIC X(55).                                   
008700*                                 INFORMATIONSMEDDELANDE                  
008800*** END OF VILMAII-COPY LENGTH= 944 BYTES                                 
