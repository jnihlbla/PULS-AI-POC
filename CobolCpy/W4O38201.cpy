000100 01  MOD-W4O38201.                                                        
000200*                                 COPYTEXT FOR MOD W4O38201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRCGRP-IN      PIC X(5).                                    
000800*                                 PRODUKTIONSKANALSGRUPP                  
000900     03 MOD-KDPRODKL-IN      PIC X.                                       
001000*                                 PRODUKTIONSKLASS                        
001100     03 MOD-IDPRC-IN.                                                     
001200*                                 PRODUKTIONSKANAL                        
001300        05 MOD-IDPRCBAS      PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 MOD-IDPRCVAR      PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 MOD-IDDC-IN          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-KDPRCGRP-OUT     PIC X(5).                                    
002000*                                 PRODUKTIONSKANALSGRUPP                  
002100     03 MOD-KDPRODKL-OUT     PIC X.                                       
002200*                                 PRODUKTIONSKLASS                        
002300     03 MOD-IDPRC-OUT.                                                    
002400*                                 PRODUKTIONSKANAL                        
002500        05 MOD-IDPRCBAS      PIC X(3).                                    
002600*                                 PRC-BAS                                 
002700        05 MOD-IDPRCVAR      PIC X.                                       
002800*                                 PRC-VARIANT                             
002900     03 MOD-IDDC-OUT         PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-IDORDER-ENTER    PIC X(7).                                    
003200*                                 VOLVO PARTS ORDERNUMMER                 
003300     03 MOD-TIRFS-ENTER      PIC X(10).                                   
003400*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003500     03 MOD-TILST-O-ENTER    PIC X(10).                                   
003600*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
003700     03 MOD-IDPRCVAR-ENTER   OCCURS 10 TIMES                              
003800                             PIC X.                                       
003900*                                 PRC-VARIANT                             
004000     03 MOD-IDORDER-NEXT     PIC X(7).                                    
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200     03 MOD-TIRFS-NEXT       PIC X(10).                                   
004300*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004400     03 MOD-TILST-O-NEXT     PIC X(10).                                   
004500*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
004600     03 MOD-IDPRCVAR-NEXT    OCCURS 10 TIMES                              
004700                             PIC X.                                       
004800*                                 PRC-VARIANT                             
004900     03 MOD-LINE1            OCCURS 10 TIMES.                             
005000        05 MOD-IDPRC-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-IDPRC-LINE.                                                
005300*                                 PRODUKTIONSKANAL                        
005400           07 MOD-IDPRCBAS   PIC X(3).                                    
005500*                                 PRC-BAS                                 
005600           07 MOD-IDPRCVAR   PIC X.                                       
005700*                                 PRC-VARIANT                             
005800     03 MOD-LINE2            OCCURS 14 TIMES.                             
005900*                                 LINE2                                   
006000        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-IDDISTR       PIC Z(3)9.                                   
006300*                                 DISTRIKTNUMMER                          
006400        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
006700*                                 KUNDNUMMER                              
006800        05 MOD-IDORDNR7-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDORDNR7      PIC Z(6)9.                                   
007100*                                 ORDERNUMMER                             
007200        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDPRODNR      PIC Z(6)9.                                   
007500*                                 PRODUKTIONSNUMMER                       
007600        05 MOD-KVRADER-ATTR  PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KVRADER-LINE  PIC X(50).                                   
007900*                                                                         
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
