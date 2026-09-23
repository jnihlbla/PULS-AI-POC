000100 01  RESP-W30173O1.                                                       
000200*                                 MOD-COPYTEXT FÖR W3017300               
000300     03 RESP-KDPRT-ATTR      PIC X(2).                                    
000400*                                 MFS ATTRIBUTFÄLT                        
000500     03 RESP-KDPRT           PIC X(3).                                    
000600*                                 PRINTERKOD                              
000700     03 RESP-IDPRODNR-LO     PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 RESP-IDPRODNR-HI     PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 RESP-BELEV-LO        PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300     03 RESP-BELEV-HI        PIC X(30).                                   
001400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001500     03 RESP-BEART-SVE       PIC X(25).                                   
001600*                                 SVENSK ARTIKELBENÄMNING                 
001700     03 RESP-KDPRODSL        PIC Z9.                                      
001800*                                 PRODUKTSLAG                             
001900     03 RESP-IDFKNGRP        PIC Z(3)9.                                   
002000*                                 FUNKTIONSGRUPP                          
002100     03 RESP-IDDISTR-RENOV-ATTR                                           
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 RESP-IDDISTR-RENOV   PIC Z(3)9.                                   
002500*                                 DISTRIKTNUMMER                          
002600     03 RESP-KVBYTPKO        PIC 9(3).                                    
002700*                                 ANTAL BYTESOBJEKT PER PALL              
002800     03 RESP-ADLAGOMR        PIC Z(2).                                    
002900*                                 LAGEROMRÅDE                             
003000     03 RESP-ADGANG          PIC Z(2).                                    
003100*                                 GÅNG                                    
003200     03 RESP-ADPLATS         PIC Z(5).                                    
003300*                                 LAGERPLATSNUMMER                        
003400     03 RESP-KVLS-ATTR       PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 RESP-KVLS            PIC -(7)9.                                   
003700*                                 LAGERSALDO                              
003800     03 RESP-KVLS-MAXCORE    PIC -(7)9.                                   
003900*                                 LAGERSALDO MAXCORE                      
004000     03 RESP-BETFLEV-ATTR    PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 RESP-BETFLEV         PIC X(30).                                   
004300*                                 TILLFÄLLIG LEVERANTÖR                   
004400     03 RESP-DELAR-SAKNAS-ATTR                                            
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 RESP-DELAR-SAKNAS    PIC X(30).                                   
004800     03 RESP-TEBYTKVA1       PIC X(75).                                   
004900*                                 KVALITETSNOTERING BYTESOBJEKT           
005000     03 RESP-TEBYTKVA2       PIC X(75).                                   
005100*                                 KVALITETSNOTERING BYTESOBJEKT           
005200     03 RESP-TEBYTKVA3       PIC X(75).                                   
005300*                                 KVALITETSNOTERING BYTESOBJEKT           
005400     03 RESP-TEBYTKVA4       PIC X(75).                                   
005500*                                 KVALITETSNOTERING BYTESOBJEKT           
005600     03 RESP-REG-FLAGGA-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 RESP-REG-FLAGGA      PIC X.                                       
005900*                                 ALLMÄN FLAGGA                           
006000     03 RESP-BELEV           OCCURS 4 TIMES                               
006100                             PIC X(30).                                   
006200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006300     03 RESP-KVRADER         PIC Z(4)9.                                   
006400*                                 ANTAL RADER                             
006500     03 RESP-IDARTNR         OCCURS 300 TIMES                             
006600                             PIC Z(7)9.                                   
006700*                                 ARTIKELNUMMER                           
006800*** END OF VILMAII-COPY LENGTH= 3042 BYTES                                
