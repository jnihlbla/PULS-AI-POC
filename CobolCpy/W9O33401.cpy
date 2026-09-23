000100 01  MOD-W9O33401.                                                        
000200*                                 MODCOPYTEXT TILL W9033400.              
000300*                                 BO/TPO INQUIRY.                         
000400*                                 FR≈GA RO/TPO.                           
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-IDMFSFEL         PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 MOD-IDORDNR7-NEXT    PIC 9(7).                                    
001000*                                 ORDERNUMMER                             
001100     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
001400*                                 L÷PNUMMER                               
001500     03 MOD-RAD              OCCURS 13 TIMES.                             
001600        05 MOD-IDARTNR-RAD   PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800        05 MOD-KVART         PIC 9(7).                                    
001900*                                 ANTAL ARTNR PER BRYTBEGREPP             
002000        05 MOD-IDORDNR7-RAD  PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200        05 MOD-BEVOLREF      PIC X(10).                                   
002300*                                 VOLVO REFERENS                          
002400        05 MOD-IDDC          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600        05 MOD-KDORDKL       PIC 9.                                       
002700*                                 ORDERKLASS                              
002800        05 MOD-KDTPOTYP-RAD  PIC 9.                                       
002900*                                 TYP AV TIDPLANERAD ORDER                
003000        05 MOD-TIRODAT       PIC 9(6).                                    
003100*                                 RESTORDERDATUM         (≈≈MMDD)         
003200        05 MOD-TITPO         PIC 9(6).                                    
003300*                                 PLANERAD ORDERDATUM                     
003400        05 MOD-KDSTARAD-RAD  PIC X.                                       
003500*                                 RADSTATUSKOD                            
003600        05 MOD-TIANNULL      PIC 9(6).                                    
003700*                                 ANNULLATIONSDATUM (≈≈MMDD)              
003800*** END OF VILMAII-COPY LENGTH= 754 BYTES                                 
