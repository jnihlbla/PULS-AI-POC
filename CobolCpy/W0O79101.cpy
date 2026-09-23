000100 01  MOD-W0O79101.                                                        
000200*                                 MOD-COPYTEXT PGM W00791                 
000300*                                 BASIC STOCK LISTS                       
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDBASLM-ATTR     PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-KDBASLM          PIC X(6).                                    
001100*                                 BASLAGERMARKNAD                         
001200     03 MOD-KDPRODSL-ATTR    PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-KDPRODSL         PIC X(2).                                    
001500*                                 PRODUKTSLAG                             
001600     03 MOD-IDPROJ-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-IDPROJ           PIC X(4).                                    
001900*                                 PARTS PROJEKTIDENTITET                  
002000     03 MOD-IDFKNGRP-1-ATTR  PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-IDFKNGRP-1       PIC Z(3)9.                                   
002300*                                 FUNKTIONSGRUPP                          
002400     03 MOD-IDFKNGRP-2-ATTR  PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDFKNGRP-2       PIC Z(3)9.                                   
002700*                                 FUNKTIONSGRUPP                          
002800     03 MOD-IDDISTR-ATTR     PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDDISTR          PIC Z(3)9.                                   
003100*                                 DISTRIKTNUMMER                          
003200     03 MOD-KDBPSR-1-ATTR    PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KDBPSR-1         PIC 9.                                       
003500*                                 BASLAGERFÖRSLAGSNIVÅ                    
003600     03 MOD-KDBPSR-2-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDBPSR-2         PIC 9.                                       
003900*                                 BASLAGERFÖRSLAGSNIVÅ                    
004000     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDSKYLT          PIC X(3).                                    
004300*                                 NATIONALITETSTECKEN                     
004400     03 MOD-MARK-NOT-ATTR    PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-MARK-NOT         PIC X.                                       
004700     03 MOD-MARK-QTY-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-MARK-QTY         PIC X.                                       
005000     03 MOD-KDDEALER-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KDDEALER         PIC X.                                       
005300*                                 BASLAGER KUNDKOD                        
005400     03 MOD-IDFKNGRP-SORT-ATTR                                            
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-IDFKNGRP-SORT    PIC X.                                       
005800     03 MOD-TISTOMREG-SORT-ATTR                                           
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-TISTOMREG-SORT   PIC X.                                       
006200     03 MOD-IDARTNR-SORT-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-IDARTNR-SORT     PIC X.                                       
006600     03 MOD-KDPRODSL-SORT-ATTR                                            
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-KDPRODSL-SORT    PIC X.                                       
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END COPY W0O79101    LENGTH=167                                       
