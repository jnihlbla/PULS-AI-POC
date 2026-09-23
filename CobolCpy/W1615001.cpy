000100 01  W1615001.                                                            
000200*                                 ARTIKELINFO FÖR BEREDNING AV BI         
000300*                                 MA                                      
000400*                                                                         
000500     03 IDLEVNR              PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 BELEV                PIC X(30).                                   
000800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000900     03 BEART                PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 VKART                PIC S9(7).                                   
001200     03 VLARTNTO             PIC S9(7).                                   
001300     03 KDSORT               PIC X(2).                                    
001400*                                 SORT-KOD                                
001500     03 TEARTNOT             PIC S9(13).                                  
001600     03 KVPALL               PIC S9(7).                                   
001700     03 PRARTSTD             PIC S9(8)V9(4).                              
001800     03 PRARTBEL             PIC S9(8)V9(4).                              
001900     03 KDVALISO             PIC X(3).                                    
002000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002100     03 IDARTNR              PIC S9(9).                                   
002200     03 IDANSK               PIC X(3).                                    
002300     03 IDINK                PIC X(3).                                    
002400     03 IDBERED              PIC X(2).                                    
002500     03 KVQPACK-1            PIC S9(5).                                   
002600     03 IDFKNGRP             PIC X(4).                                    
002700*                                 FUNKTIONSGRUPP                          
002800     03 KDPRODSL             PIC X(2).                                    
002900     03 TIFINLV              PIC S9(4).                                   
003000     03 KDFARLIG             PIC X.                                       
003100     03 KDBPSR               PIC X.                                       
003200     03 KVPB-SEP             PIC S9(6)V9(1).                              
003300     03 KDUART               PIC X.                                       
003400*                                 UNDANTAGSARTIKEL                        
003500     03 TIPRLIST             PIC 9(6).                                    
003600     03 FILLER               PIC X(80).                                   
003700     03 FILLER               PIC X(5).                                    
003800*** END OF VILMAII-COPY LENGTH= 256 BYTES                                 
