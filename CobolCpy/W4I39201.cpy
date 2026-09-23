000100 01  MID-W4I39201.                                                        
000200*                                 MID-COPYTEXT FÖR W40392                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDFRAKT-UT       PIC X(2).                                    
001200*                                 FRAKTSÄTT C1-C2 TILL KUND               
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-KDORDKL-UT       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-IDKOLLI-SENAST   PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MID-RAD              OCCURS 13 TIMES.                             
002200*                                 MID-COPYTEXT FÖR W40392                 
002300        05 MID-IDKOLLI       PIC 9(5).                                    
002400*                                 KOLLINUMMER                             
002500        05 MID-KDKOLLI       PIC X(8).                                    
002600*                                 KOLLIKOD                                
002700        05 MID-VKORDBTO-KOLLI                                             
002800                             PIC 9(7).                                    
002900*                                 ORDERVIKT BRUTTO PER KOLLI              
003000        05 MID-KDEMBTYP      PIC 9.                                       
003100*                                 EMBALLAGETYP                            
